class ZCL_ABAP_UTILITY definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_rows,
              row    TYPE i,
              field  TYPE char30,
              dename TYPE char30,
              value  TYPE string,
            END   OF ty_rows .
  types:
    BEGIN OF ty_fields,
        field  TYPE char30,
        dename TYPE char30,
      END   OF ty_fields .

  class-methods SEND_EMAIL
    importing
      !IM_RECLIST type SOMLRECI1_T optional
      !IM_BODY_CONTENT type BCSY_TEXT optional
      !IM_SUBJECT type SO_OBJ_DES optional
      !IM_XSTRING type XSTRING optional
      !IM_STRING type STRING optional
      !IM_ATTCH_NAME type SO_OBJ_DES optional
      !IM_SEND_ID type AD_SMTPADR optional
      !IM_BODY_TYPE type SO_OBJ_TP default 'RAW'
      !IM_ATTACH_TYP type SO_OBJ_TP default 'PDF'
      !IM_COMMIT type ABAP_BOOL default 'X'
    exporting
      !E_MESSAGE type STRING
    exceptions
      L_SEND_EXCEPTION .
  class-methods SAVE_LOG_APP_SERVER
    importing
      !I_OBJ_NUMBER type BALNREXT optional
      !I_OBJ_NAME type BALOBJ_D optional
      !I_SUBOBJ_NAME type BALSUBOBJ optional
      !I_BAPI_RETURN type BAPIRET2_T optional
      !I_MSG_LOG type BAL_T_MSG optional
      !I_LOG_EXPIRE type ALDATE_DEL optional
      !I_BDC_RETURN type TAB_BDCMSGCOLL optional
      !I_OBJ_CREATE type CHAR1 optional
    exporting
      !E_RETURN type SUBRC
      !E_MESSAGE type CHAR255 .
  class-methods MOVE_CORRESPONDING_IGNORE_INIT
    importing
      !I_STR_SOURCE type ANY optional
    changing
      !C_STR_DEST type ANY optional .
  class-methods CONVERSION_EXIT
    importing
      !INPUT_VALUE type ANY optional
      !TYPE type I optional
    exporting
      !OUTPUT_VALUE type ANY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAP_UTILITY IMPLEMENTATION.


  method CONVERSION_EXIT.
    FIELD-SYMBOLS: <fs_t> TYPE ANY.

  DATA:lfd_convexit TYPE funcname,
       wf_data      TYPE REF TO DATA.

  DATA:lfd_length  TYPE i,
       lfd_mask    TYPE STRING.

* Determine the mask from the domain
  DESCRIBE FIELD input_value OUTPUT-LENGTH lfd_length
                             EDIT MASK     lfd_mask.
  IF lfd_mask IS INITIAL.

* This statement is included because its not necessary that all field
* will be having mask associated with it, for example: ATNAM. To get the
* conversion exit of ATNAM the exporting parameter will be holding the
* respective mask

    DESCRIBE FIELD output_value OUTPUT-LENGTH lfd_length
                                EDIT MASK     lfd_mask.
    IF lfd_mask IS INITIAL.
      IF output_value IS INITIAL.
        output_value = input_value.
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.

  REPLACE '==' IN lfd_mask WITH SPACE.
  CONDENSE lfd_mask NO-GAPS.

  CREATE DATA wf_data TYPE c LENGTH lfd_length.
  IF wf_data IS BOUND.
    ASSIGN wf_data->* TO <fs_t>.
    IF  <fs_t> IS NOT ASSIGNED.
      IF output_value IS INITIAL.
        output_value = input_value.
      ENDIF.
      EXIT.
    ENDIF.
  ELSE.
    IF output_value IS INITIAL.
      output_value = input_value.
    ENDIF.
    EXIT.
  ENDIF.

  IF type = 1.
    CONCATENATE 'CONVERSION_EXIT_' lfd_mask '_INPUT' INTO lfd_convexit.
  ELSEIF type = 2.
    CONCATENATE 'CONVERSION_EXIT_' lfd_mask '_OUTPUT' INTO lfd_convexit.
  ENDIF.

  CALL FUNCTION lfd_convexit
    EXPORTING
      INPUT  = input_value
    IMPORTING
      OUTPUT = <fs_t>.
  IF sy-subrc NE 0.
    output_value = input_value.
  ELSE.
    output_value = <fs_t>.
  ENDIF.
  endmethod.


  METHOD move_corresponding_ignore_init.
    DATA:
     l_rcl_abap_structdescr TYPE REF TO cl_abap_structdescr.

    l_rcl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( i_str_source ).
    LOOP AT l_rcl_abap_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_str_component>).
      ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE c_str_dest TO FIELD-SYMBOL(<fs_dest_field>).
      IF sy-subrc = 0.
        ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE i_str_source TO FIELD-SYMBOL(<fs_source_field>).
        ASSERT sy-subrc = 0.
        IF <fs_source_field> IS NOT INITIAL.
          <fs_dest_field> = <fs_source_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.                "move_corresponding_ignore_init
ENDMETHOD.


  method SAVE_LOG_APP_SERVER.
      DATA : L_STR_LOG         TYPE BAL_S_LOG,
         LV_LOG_HANDLE     TYPE BALLOGHNDL,
         L_STR_BAPI_RETURN TYPE BAPIRET2,
         L_STR_BDC_RETURN  TYPE BDCMSGCOLL,
         L_STR_MSG         TYPE BAL_S_MSG,
         L_STR_BALOBJ      TYPE BALOBJ,
         L_STR_SUBOBJ      TYPE BALSUB.
  "Create object/sub object if does not exist in
  "Tcode SLG0
  IF I_OBJ_CREATE IS NOT INITIAL.

    "Object
    SELECT SINGLE * FROM BALOBJ INTO L_STR_BALOBJ
           WHERE OBJECT = I_OBJ_NAME.
    IF SY-SUBRC <> 0.
      L_STR_BALOBJ-OBJECT = I_OBJ_NAME.
      INSERT INTO BALOBJ VALUES L_STR_BALOBJ.
    ENDIF.

    "Sub Object
    SELECT SINGLE * FROM BALSUB INTO L_STR_SUBOBJ
          WHERE OBJECT = I_OBJ_NAME AND
                SUBOBJECT = I_SUBOBJ_NAME.
    IF SY-SUBRC <> 0.
      L_STR_SUBOBJ-OBJECT = I_OBJ_NAME.
      L_STR_SUBOBJ-SUBOBJECT = I_SUBOBJ_NAME.
      INSERT INTO BALSUB VALUES L_STR_SUBOBJ.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM BALOBJ INTO L_STR_BALOBJ
           WHERE OBJECT = I_OBJ_NAME.
    IF SY-SUBRC = 0.
    ELSE.
      E_MESSAGE = 'Entered Object/Subobject Does Not Exist'.
    ENDIF.

    SELECT SINGLE * FROM BALSUB INTO L_STR_SUBOBJ
          WHERE OBJECT = I_OBJ_NAME AND
                SUBOBJECT = I_SUBOBJ_NAME.
    IF SY-SUBRC = 0.
    ELSE.
      E_MESSAGE = 'Entered Object/Subobject Does Not Exist'.
    ENDIF.
  ENDIF.

  "Open Application Log
  L_STR_LOG-EXTNUMBER  = I_OBJ_NUMBER.
  L_STR_LOG-SUBOBJECT  = I_SUBOBJ_NAME.
  L_STR_LOG-ALPROG     = SY-REPID.
  L_STR_LOG-OBJECT     = I_OBJ_NAME.
  IF I_LOG_EXPIRE IS NOT INITIAL.
    L_STR_LOG-ALDATE_DEL = I_LOG_EXPIRE.
  ELSE.
    L_STR_LOG-ALDATE_DEL = SY-DATUM + 7.
  ENDIF.
  L_STR_LOG-ALTCODE    = SY-TCODE.
  L_STR_LOG-ALUSER     = SY-UNAME.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      I_S_LOG                 = L_STR_LOG
    IMPORTING
      E_LOG_HANDLE            = LV_LOG_HANDLE
    EXCEPTIONS
      LOG_HEADER_INCONSISTENT = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
*     Implement suitable error handling here
    E_RETURN = SY-SUBRC.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = SY-MSGID
        MSGNR               = SY-MSGNO
        MSGV1               = SY-MSGV1
        MSGV2               = SY-MSGV2
        MSGV3               = SY-MSGV3
        MSGV4               = SY-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = E_MESSAGE.
  ENDIF.

  IF I_BAPI_RETURN[] IS NOT INITIAL.
    LOOP AT I_BAPI_RETURN INTO L_STR_BAPI_RETURN.
      L_STR_MSG-MSGTY = L_STR_BAPI_RETURN-TYPE.
      L_STR_MSG-MSGID = L_STR_BAPI_RETURN-ID.
      L_STR_MSG-MSGNO = L_STR_BAPI_RETURN-NUMBER.
      L_STR_MSG-MSGV1 = L_STR_BAPI_RETURN-MESSAGE_V1.
      L_STR_MSG-MSGV2 = L_STR_BAPI_RETURN-MESSAGE_V2.
      L_STR_MSG-MSGV3 = L_STR_BAPI_RETURN-MESSAGE_V3.
      L_STR_MSG-MSGV4 = L_STR_BAPI_RETURN-MESSAGE_V4.

      "Add messages to message object
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          I_LOG_HANDLE     = LV_LOG_HANDLE
          I_S_MSG          = L_STR_MSG
        EXCEPTIONS
          LOG_NOT_FOUND    = 1
          MSG_INCONSISTENT = 2
          LOG_IS_FULL      = 3
          OTHERS           = 4.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        E_RETURN = SY-SUBRC.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = SY-MSGID
            MSGNR               = SY-MSGNO
            MSGV1               = SY-MSGV1
            MSGV2               = SY-MSGV2
            MSGV3               = SY-MSGV3
            MSGV4               = SY-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = E_MESSAGE.
      ELSE.
*        Save Logs
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            I_SAVE_ALL       = 'X'
          EXCEPTIONS
            LOG_NOT_FOUND    = 1
            SAVE_NOT_ALLOWED = 2
            NUMBERING_ERROR  = 3
            OTHERS           = 4.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
          E_RETURN = SY-SUBRC.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = E_MESSAGE.
        ELSE.
          E_RETURN = SY-SUBRC.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF I_MSG_LOG[] IS NOT INITIAL.
    LOOP AT I_MSG_LOG INTO L_STR_MSG.
      "Add messages to message object
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          I_LOG_HANDLE     = LV_LOG_HANDLE
          I_S_MSG          = L_STR_MSG
        EXCEPTIONS
          LOG_NOT_FOUND    = 1
          MSG_INCONSISTENT = 2
          LOG_IS_FULL      = 3
          OTHERS           = 4.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        E_RETURN = SY-SUBRC.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = SY-MSGID
            MSGNR               = SY-MSGNO
            MSGV1               = SY-MSGV1
            MSGV2               = SY-MSGV2
            MSGV3               = SY-MSGV3
            MSGV4               = SY-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = E_MESSAGE.
      ELSE.
*        Save Logs
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            I_SAVE_ALL       = 'X'
          EXCEPTIONS
            LOG_NOT_FOUND    = 1
            SAVE_NOT_ALLOWED = 2
            NUMBERING_ERROR  = 3
            OTHERS           = 4.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
          E_RETURN = SY-SUBRC.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = E_MESSAGE.
        ELSE.
          E_RETURN = SY-SUBRC.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF I_BDC_RETURN[] IS NOT INITIAL.
    LOOP AT I_BDC_RETURN INTO L_STR_BDC_RETURN.
      L_STR_MSG-MSGTY = L_STR_BDC_RETURN-MSGTYP.
      L_STR_MSG-MSGID = L_STR_BDC_RETURN-MSGID.
      L_STR_MSG-MSGNO = L_STR_BDC_RETURN-MSGNR.
      L_STR_MSG-MSGV1 = L_STR_BDC_RETURN-MSGV1.
      L_STR_MSG-MSGV2 = L_STR_BDC_RETURN-MSGV2.
      L_STR_MSG-MSGV3 = L_STR_BDC_RETURN-MSGV3.
      L_STR_MSG-MSGV4 = L_STR_BDC_RETURN-MSGV4.

      "Add messages to message object
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          I_LOG_HANDLE     = LV_LOG_HANDLE
          I_S_MSG          = L_STR_MSG
        EXCEPTIONS
          LOG_NOT_FOUND    = 1
          MSG_INCONSISTENT = 2
          LOG_IS_FULL      = 3
          OTHERS           = 4.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        E_RETURN = SY-SUBRC.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            MSGID               = SY-MSGID
            MSGNR               = SY-MSGNO
            MSGV1               = SY-MSGV1
            MSGV2               = SY-MSGV2
            MSGV3               = SY-MSGV3
            MSGV4               = SY-MSGV4
          IMPORTING
            MESSAGE_TEXT_OUTPUT = E_MESSAGE.
      ELSE.
*        Save Logs
        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            I_SAVE_ALL       = 'X'
          EXCEPTIONS
            LOG_NOT_FOUND    = 1
            SAVE_NOT_ALLOWED = 2
            NUMBERING_ERROR  = 3
            OTHERS           = 4.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
          E_RETURN = SY-SUBRC.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = E_MESSAGE.
        ELSE.
          E_RETURN = SY-SUBRC.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  endmethod.


  METHOD send_email.
*    ***Local Structure*********
    DATA     : l_str_reclist         TYPE somlreci1,
               l_str_att_content_hex TYPE solix.

***************************
****Local Table************
    DATA   : l_tab_att_content_hex TYPE solix_tab.
***************************
***Local Variables*********
    DATA     : lv_email       TYPE ad_smtpadr,
               lv_dl_name     TYPE so_obj_nam,
               lv_obj_type    TYPE soodk-objtp,    "Attachment Type PDF,XLS
               lv_sent_to_all TYPE os_boolean,
               lv_send        TYPE ad_smtpadr , "VALUE 'xyz@dummy.com',
               lv_size        TYPE so_obj_len,
               lv_pdf_size    TYPE so_obj_len,
               lv_message     TYPE string.
***************************
****Local Objects**********
    DATA   : lo_send_request   TYPE REF TO cl_bcs,
             lo_document       TYPE REF TO cl_document_bcs,
             lo_gr_sender      TYPE REF TO if_sender_bcs,
             lo_recipient      TYPE REF TO if_recipient_bcs,
             lo_attachment     TYPE REF TO if_document_bcs,
             lo_send_exception TYPE REF TO cx_send_req_bcs.   " Send Exception
***************************

    IF im_xstring IS NOT INITIAL.
      l_tab_att_content_hex = cl_document_bcs=>xstring_to_solix( ip_xstring = im_xstring ).
    ELSEIF im_string IS   NOT INITIAL.
      TRY.
          cl_bcs_convert=>string_to_solix(
          EXPORTING
          iv_string = im_string
          iv_codepage = '4103' "suitable for MS Excel, leave empty
          iv_add_bom = 'X' "for other doc types
          IMPORTING
          et_solix = l_tab_att_content_hex
          ev_size =  lv_size ).
        CATCH cx_bcs.
          MESSAGE e445(so).
      ENDTRY.
    ENDIF.

    TRY.
*    *      -------- create persistent send request ------------------------
        lo_send_request = cl_bcs=>create_persistent( ).
      CATCH cx_send_req_bcs INTO lo_send_exception.
    ENDTRY.

    lo_document = cl_document_bcs=>create_document(
      i_type    = im_body_type
      i_text    = im_body_content
      i_subject =  im_subject ).

    IF l_tab_att_content_hex IS NOT INITIAL.
      TRY.
          "Create Attachement
          lo_attachment = cl_document_bcs=>create_document( i_type    = im_attach_typ
                                                            i_hex     = l_tab_att_content_hex
                                                            i_length  = lv_pdf_size
                                                            i_subject = im_attch_name ).
        CATCH cx_document_bcs ."INTO LX_DOCUMENT_BCS.
      ENDTRY.

      "Send Attachment
      TRY.
          lv_obj_type = im_attach_typ.
          lo_document->add_attachment(
            EXPORTING
              i_attachment_type = lv_obj_type
              i_attachment_subject = im_attch_name
              i_att_content_hex = l_tab_att_content_hex ).

        CATCH cx_document_bcs." INTO LX_DOCUMENT_BCS.
      ENDTRY.
    ENDIF.
*     add document object to send request
    lo_send_request->set_document( lo_document ).
    IF im_send_id IS NOT INITIAL.
      lv_send = im_send_id.
      lo_gr_sender =  cl_cam_address_bcs=>create_internet_address( lv_send ).
      "Add sender to send request
      lo_send_request->set_sender( lo_gr_sender ).
    ENDIF.
    TRY.
        "Add recipient to send request
        LOOP AT im_reclist INTO l_str_reclist WHERE receiver IS NOT INITIAL.
          IF l_str_reclist-rec_type = 'U'.         "Email Address
            lv_email = l_str_reclist-receiver.
            lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

          ELSEIF l_str_reclist-rec_type = 'C'.  "Distribution LIst
            lv_dl_name = l_str_reclist-receiver.
            lo_recipient = cl_distributionlist_bcs=>getu_persistent(
            i_dliname = lv_dl_name
            i_private = space ).
          ENDIF.

          "Add recipient to send request
          IF l_str_reclist-copy = 'X'.
            CALL METHOD lo_send_request->add_recipient
              EXPORTING
                i_recipient = lo_recipient
                i_copy      = 'X'.
          ELSE.
            CALL METHOD lo_send_request->add_recipient
              EXPORTING
                i_recipient = lo_recipient
                i_express   = 'X'.
          ENDIF.
        ENDLOOP.

        "Send email
        CALL METHOD lo_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_sent_to_all ).
        IF im_commit = abap_true.
          COMMIT WORK.
        ENDIF.
      CATCH cx_send_req_bcs INTO lo_send_exception.
        IF lo_send_exception IS BOUND.
          CALL METHOD lo_send_exception->if_message~get_text
            RECEIVING
              result = lv_message.
          e_message = lv_message.
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
