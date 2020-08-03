*&---------------------------------------------------------------------*
*&  Include           ZFI_GOS_DOWNLOAD_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_ATTACH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_attach_data .
  TYPES : BEGIN OF ty_object,
            object_id TYPE toa01-object_id,
            belnr     TYPE bkpf-belnr,
            gjahr     TYPE bkpf-gjahr,
            bukrs     TYPE bkpf-bukrs,
            budat     TYPE bkpf-budat,
            bktxt     TYPE bkpf-bktxt,
            xblnr     TYPE bkpf-xblnr,
          END OF ty_object.

  DATA  : l_tab_object TYPE STANDARD TABLE OF ty_object,
          l_str_object TYPE ty_object,
          lt_attach    TYPE STANDARD TABLE OF soli.

  DATA      : l_str_final TYPE ty_final,
              l_tab_style TYPE lvc_t_styl,
              l_str_style TYPE lvc_s_styl.

  CONSTANTS : l_con_type TYPE swo_objtyp VALUE 'BKPF'.

  DATA : l_var_key TYPE swo_typeid.

*  Get data according to selection screen input
  PERFORM gui_indicator USING 'Fething data' 20.
  SELECT  bukrs,
          belnr,
          gjahr,
          blart,
          bldat,
          budat,
          bktxt,
          xblnr
           FROM bkpf INTO TABLE @DATA(l_tab_bkpf)
           WHERE bukrs IN @s_bukrs
            AND  belnr IN @s_belnr
            AND  gjahr IN @s_gjahr
            AND  budat IN @s_budat.
  IF l_tab_bkpf[] IS NOT INITIAL.


**  Create push button in ALV to download file
*  l_str_style-fieldname = 'DOWNLOAD'.
*  l_str_style-style = cl_gui_alv_grid=>mc_style_button.
*  INSERT l_str_style INTO TABLE l_tab_style.
    "Get attachement from documents.
    PERFORM gui_indicator USING 'Searching for Manually attached documents' 25.
    LOOP AT l_tab_bkpf INTO DATA(l_str_bkpf).
      l_var_key = |{ l_str_bkpf-bukrs }{ l_str_bkpf-belnr }{ l_str_bkpf-gjahr }|.
      MOVE-CORRESPONDING l_str_bkpf TO l_str_object.
      l_str_object-object_id = l_var_key.
      APPEND l_str_object TO l_tab_object.
      CLEAR l_str_object.
      "filling itab for archive link objects

      "Get attachment list from the documents

      CALL METHOD zcl_gos_attachment=>gos_get_file_list
        EXPORTING
          objtype       = l_con_type
          objkey        = l_var_key
        IMPORTING
          t_attachments = DATA(l_tab_sood).
      IF l_tab_sood[] IS NOT INITIAL.
        LOOP AT l_tab_sood INTO DATA(l_str_sood).
*       L_STR_FINAL = CORRESPONDING #( l_str_sood ).
          l_str_final = CORRESPONDING #( l_str_bkpf ).
          l_str_final-objno  = l_str_sood-objno.
          l_str_final-objtp  = l_str_sood-objtp.
          l_str_final-objyr  = l_str_sood-objyr.
          l_str_final-objdes = l_str_sood-objdes.
          l_str_final-objlen = l_str_sood-objlen.
          l_str_final-extct  = l_str_sood-extct.
          l_str_final-file_ext = l_str_sood-file_ext.
          l_str_final-attach = |{ l_str_sood-objdes }.{ l_str_sood-file_ext }|.
*        l_str_final-style[] = l_tab_style[].
          l_str_final-download = icon_system_save.
          APPEND l_str_final TO g_tab_final.
          CLEAR l_str_final.
        ENDLOOP.
      ENDIF.
      REFRESH :l_tab_sood.
    ENDLOOP.

    "Get Archive linked document
    PERFORM gui_indicator USING 'Searching for ArchiveLink documents' 50.
    IF l_tab_object[] IS NOT INITIAL.
      SELECT * FROM toa01 INTO TABLE @DATA(l_tab_toa01) FOR ALL ENTRIES IN
               @l_tab_object WHERE sap_object = 'BKPF' AND
                                   object_id = @l_tab_object-object_id.
      IF l_tab_toa01[] IS NOT INITIAL.
        SORT l_tab_object BY object_id.
        "Get object description
        SELECT * FROM toasp INTO TABLE @DATA(l_tab_toasp) WHERE language = @sy-langu.
        IF sy-subrc = 0.
          SORT l_tab_toasp BY ar_object.
        ENDIF.
        LOOP AT l_tab_toa01 INTO DATA(l_str_toa01).

          CALL FUNCTION 'SCMS_HTTP_GET'
            EXPORTING
              crep_id               = l_str_toa01-archiv_id
              doc_id                = l_str_toa01-arc_doc_id
            TABLES
              data                  = lt_attach
            EXCEPTIONS
              bad_request           = 1
              unauthorized          = 2
              not_found             = 3
              conflict              = 4
              internal_server_error = 5
              error_http            = 6
              error_url             = 7
              error_signature       = 8
              OTHERS                = 9.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.
            CLEAR l_str_object.
            READ TABLE l_tab_object INTO l_str_object WITH KEY
             object_id = l_str_toa01-object_id BINARY SEARCH.
            MOVE-CORRESPONDING l_str_object TO l_str_final.
            l_str_final-download = icon_system_save.
            l_str_final-is_archive = abap_true.
            l_str_final-arc_doc_id = l_str_toa01-arc_doc_id.
            l_str_final-archiv_id = l_str_toa01-archiv_id.
            l_str_final-file_ext = l_str_toa01-reserve.
            READ TABLE l_tab_toasp INTO DATA(l_str_toasp) WITH KEY
                                   ar_object = l_str_toa01-ar_object
                                   BINARY SEARCH.

            IF sy-subrc = 0.
              l_str_final-attach = l_str_toasp-objecttext.
            ENDIF.
            APPEND l_str_final TO g_tab_final.
            CLEAR l_str_final.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.

    IF g_tab_final[] IS INITIAL.
      MESSAGE 'No attachment found for the documents' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      SORT g_tab_final BY bukrs gjahr belnr.
    ENDIF.
  ELSE.
    MESSAGE 'No document found' TYPE 'I'.
    LEAVE  LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_fcat .
****LOCAL VARIABLE*****
  DATA : l_str_fcat TYPE lvc_s_fcat,
         l_var_col  TYPE i.

  DEFINE fcat.
    l_var_col = l_var_col + 1.
    l_str_fcat-col_pos = l_var_col.
    l_str_fcat-fieldname = &1.
    l_str_fcat-coltext   = &2.
    l_str_fcat-edit      = &3.
    IF l_str_fcat-fieldname = 'CHECK'.
    l_str_fcat-checkbox = 'X'.
    ELSEIF l_str_fcat-fieldname = 'DOWNLOAD'.
    l_str_fcat-style = cl_gui_alv_grid=>mc_style_button.
    ENDIF.
    APPEND l_str_fcat TO g_tab_fcat.
    CLEAR l_str_fcat.
  END-OF-DEFINITION.

  fcat 'CHECK' 'Check' 'X'.
  fcat 'BUKRS' 'Company Code' ' '.
  fcat 'BELNR' 'Document No.' ' '.
  fcat 'GJAHR' 'Year' ' '.
  fcat 'BUDAT' 'Posting Dt.' ' '.
  fcat 'BKTXT' 'Header Text' ' '.
  fcat 'XBLNR' 'Ref. No.' ' '.
  fcat 'ATTACH' 'Attachment' ' '.
  fcat 'IS_ARCHIVE' 'Arch.Docs' ' '.
  fcat 'DOWNLOAD' 'Download' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
**** Local Declaration *******
  DATA : l_str_layo TYPE lvc_s_layo.
  IF cb_dwnl = abap_true.
    IF p_path IS INITIAL.
      MESSAGE 'Please select the folder' TYPE 'I'.
      LEAVE TO LIST-PROCESSING.
    ELSE.
      PERFORM download_direct.
    ENDIF.
  ELSE.
    "Call ALV Screen
    CALL SCREEN 9000.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'PF_STATUS'.
  SET TITLEBAR 'DOWNLOAD_DOCUMENT'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_9000 OUTPUT.
*Check if there is no custom container in screen 9000

  IF go_cont IS INITIAL.
*Creating object of container
    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CC_ALV'.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*Creating object of alv
    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_cont.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    *alv layout
    PERFORM alv_layout.
*alv field catalogue
    PERFORM create_fcat.
*ALV toolbar event set
    PERFORM set_toolbar.

*Displaying the ALV grid
    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        is_layout       = g_str_layo
      CHANGING
        it_outtab       = g_tab_final
        it_fieldcatalog = g_tab_fcat.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
*Create object of the event class and setting handler for double click
    CREATE OBJECT go_event.
    SET HANDLER go_event->handle_button_click FOR go_alv.
    SET HANDLER go_event->handle_double_click FOR go_alv .

  ELSE.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_layout .
  g_str_layo-zebra = abap_true.
  g_str_layo-cwidth_opt = abap_true.
*  g_str_layo-no_toolbar = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
*BREAK-POINT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
*	WHEN .
*	WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ADD_BUTTON_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM event_toolbar USING e_object
                                   TYPE REF TO cl_alv_event_toolbar_set
                               e_interactive TYPE char1.    "#EC *

  DATA: ls_toolbar TYPE stb_button.

*...Seperator
  ls_toolbar-function  = 'DUMMY'.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO e_object->mt_toolbar.

*... Normal Button
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'SALL'.                            "#EC NOTEXT
  ls_toolbar-icon      = icon_select_all.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
*  ls_toolbar-text      = 'Download'.                        "#EC NOTEXT
  ls_toolbar-quickinfo = 'Select All'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.
  CLEAR ls_toolbar.
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'DALL'.                            "#EC NOTEXT
  ls_toolbar-icon      = icon_deselect_all.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
*  ls_toolbar-text      = 'Download'.                        "#EC NOTEXT
  ls_toolbar-quickinfo = 'Deselect All'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'SEL_DWLN'.                        "#EC NOTEXT
  ls_toolbar-icon      = icon_attachment.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
  ls_toolbar-text      = 'Download'.                        "#EC NOTEXT
  ls_toolbar-quickinfo = space.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.

  LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs>).
    DATA(lv_index) = sy-tabix.
    IF <fs>-function = '&MB_SUM' OR <fs>-function = '&MB_SUMTOT' OR
       <fs>-function = '&DETAIL' OR <fs>-function = '&INFO' OR
       <fs>-function = '&&SEP00' OR <fs>-function = '&CHECK' OR
       <fs>-function = '&REFRESH' OR <fs>-function = '&&SEP01' OR
       <fs>-function = '&LOCAL&CUT' OR <fs>-function = '&LOCAL&COPY' OR
       <fs>-function = '&LOCAL&UNDO' OR <fs>-function = '&LOCAL&PASTE' OR
       <fs>-function = '&&SEP02' OR <fs>-function = '&LOCAL&APPEND' OR
       <fs>-function = '&LOCAL&DELETE_ROW' OR <fs>-function = '&LOCAL&INSERT_ROW' OR
       <fs>-function = '&&SEP03' OR <fs>-function = '&LOCAL&COPY_ROW'.
      DELETE e_object->mt_toolbar INDEX lv_index.
      CONTINUE.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " d0100_sflight_event_toolbar
*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_toolbar .
  CREATE OBJECT go_action.

  SET HANDLER go_action->toolbar
              FOR go_alv.
  SET HANDLER go_action->user_command
                FOR go_alv.
ENDFORM.

FORM event_ucomm USING e_ucomm LIKE sy-ucomm.               "#EC *

  DATA: l_count TYPE i,
        l_path  TYPE string.

  CASE e_ucomm.
    WHEN 'SEL_DWLN'.                                        "#EC NOTEXT
      "Select Directory
      PERFORM browse_directory CHANGING l_path.

      "Get Selected documents count
      DATA(l_tab_count) = g_tab_final[].
      DELETE l_tab_count WHERE check IS INITIAL.
      DESCRIBE TABLE l_tab_count LINES DATA(lv_lines).
      FREE l_tab_count.


      LOOP AT g_tab_final INTO DATA(l_str_final) WHERE check = 'X'.
        l_count = l_count + 1.
        DATA(lv_gui_txt) = | Downloading Documents { l_count } / { lv_lines } |.
        PERFORM gui_indicator USING lv_gui_txt 50.
        PERFORM download_single_doc USING l_path l_str_final.
      ENDLOOP.

    WHEN 'SALL'. "Select all
      LOOP AT g_tab_final ASSIGNING FIELD-SYMBOL(<fs_final>).
        <fs_final>-check = abap_true.
      ENDLOOP.

    WHEN 'DALL'. "Deselect all
      LOOP AT g_tab_final ASSIGNING <fs_final>.
        <fs_final>-check = abap_false.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.
  IF go_alv IS BOUND.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDFORM.                    " d0100_sflight_event_ucomm
*&---------------------------------------------------------------------*
*&      Form  BROWSE_DIRECTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_VAR_PATH  text
*----------------------------------------------------------------------*
FORM browse_directory  CHANGING p_var_path.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'File Directory'
*     initial_folder  = 'C:'
    CHANGING
      selected_folder = p_var_path. "Path
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SINGLE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_single_doc USING l_file l_str_data TYPE ty_final.

  DATA : l_str_attach TYPE sood,
         lv_sep       TYPE char1 VALUE '\'.
  IF l_file IS NOT INITIAL.
    DATA(l_key) = |{ l_str_data-bukrs }{ l_str_data-belnr }{ l_str_data-gjahr }|.
    IF l_str_data-is_archive = abap_false.
      l_str_attach = CORRESPONDING #( l_str_data ).

      CALL METHOD zcl_gos_attachment=>gos_download_file
        EXPORTING
          filepath   = l_file
          attachment = l_str_attach
          key        = l_key.

    ELSE.

      data(l_path_file) = |{ l_file }{ lv_sep }{ l_key }_{ l_str_data-attach }.{ l_str_data-file_ext }|.
      CALL METHOD zcl_gos_attachment=>archivelink_download
        EXPORTING
          crep_id   = l_str_data-archiv_id
          file_path = l_path_file
          doc_id    = l_str_data-arc_doc_id.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DIRECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_direct .
  LOOP AT g_tab_final INTO DATA(l_str_final).

    PERFORM download_single_doc USING p_path l_str_final.
  ENDLOOP.
ENDFORM.

FORM handle_double_click USING i_row TYPE lvc_s_row
                                i_column TYPE lvc_s_col
                                is_row_no TYPE lvc_s_roid.

  READ TABLE g_tab_final INTO DATA(l_str_final) INDEX is_row_no-row_id .
  IF sy-subrc = 0 .
    IF i_column-fieldname = 'BELNR' OR i_column-fieldname = 'BUKRS'
       OR i_column-fieldname = 'GJAHR'.
      SET PARAMETER ID 'BLN' FIELD l_str_final-belnr.
      SET PARAMETER ID 'BUK' FIELD l_str_final-bukrs.
      SET PARAMETER ID 'GJR' FIELD l_str_final-gjahr.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.


ENDFORM . "handle_double_click
*&---------------------------------------------------------------------*
*&      Form  GUI_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0028   text
*      -->P_20     text
*----------------------------------------------------------------------*
FORM gui_indicator  USING    text
                             progress.
* ALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = progress
*      text       = text. C

  cl_progress_indicator=>progress_indicate(
    i_text = text
    i_output_immediately = abap_true ).

ENDFORM.
