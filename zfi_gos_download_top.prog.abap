*&---------------------------------------------------------------------*
*&  Include           ZFI_GOS_DOWNLOAD_TOP
*&---------------------------------------------------------------------*

TABLES : bkpf.

********GLOBAL TYPES**************
TYPES: BEGIN OF ty_final,
         bukrs       TYPE bkpf-bukrs,
         belnr       TYPE bkpf-belnr,
         gjahr       TYPE bkpf-gjahr,
         blart       TYPE bkpf-blart,
         bldat       TYPE bkpf-bldat,
         budat       TYPE bkpf-budat,
         bktxt       TYPE bkpf-bktxt,
         xblnr       TYPE bkpf-xblnr,
         attach(150),
         objtp       TYPE so_obj_tp,
         objyr       TYPE so_obj_yr,
         objno       TYPE so_obj_no,
         objdes      TYPE so_obj_des,
         objlen      TYPE so_obj_len,
         extct       TYPE sood-extct,
         file_ext    TYPE so_fileext,
         is_archive  TYPE char1,
         archiv_id  TYPE toa01-archiv_id,
         ARC_DOC_ID  TYPE toa01-arc_doc_id,
         check       TYPE char1,
         download    TYPE char30,
*         style       TYPE lvc_t_styl,

       END OF ty_final.

********GLOBAL INTERNAL TABLES****
DATA : g_tab_final TYPE STANDARD TABLE OF ty_final,
       g_tab_fcat  TYPE lvc_t_fcat.

*******GLOBAL STRUCTURE***********
DATA : g_str_layo TYPE lvc_s_layo.

********GLOBAL VARIABLE***********
DATA: g_var_path TYPE string.



*Class definition for handling double click
CLASS lcl_eventhandler DEFINITION DEFERRED.

*******GLOBAL REFERENCE***********
DATA: go_alv   TYPE REF TO cl_gui_alv_grid,
      go_cont  TYPE REF TO cl_gui_custom_container,
      go_event TYPE REF TO lcl_eventhandler.


***LOCAL CLASS DEFINATION*******
CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
            es_col_id
            es_row_no
            sender,
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_eventhandler DEFINITION

CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD handle_button_click.
*   define local data
    DATA : l_path TYPE string.
    PERFORM browse_directory CHANGING l_path.
    "Read clicked documents
    READ TABLE g_tab_final INTO DATA(l_str_final) INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      PERFORM download_single_doc USING l_path l_str_final.
    ENDIF.

  ENDMETHOD.                    "handle_button_click

  METHOD handle_double_click .
    PERFORM handle_double_click USING e_row e_column es_row_no .
  ENDMETHOD . "handle_double_click


ENDCLASS.                    "lcl_eventhandler IMPLEMENTATION

CLASS lcl_events_alvgrid DEFINITION.
  PUBLIC SECTION.
    METHODS:
      toolbar              FOR EVENT toolbar
                    OF cl_gui_alv_grid
        IMPORTING e_object
                    e_interactive,

      user_command         FOR EVENT user_command
                    OF cl_gui_alv_grid
        IMPORTING e_ucomm sender.
ENDCLASS.                    "l_cl_events_d0100 DEFINITION

*---------------------------------------------------------------------*
*       CLASS l_cl_events_d0200 DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_events_alvgrid IMPLEMENTATION.
  METHOD toolbar.
    PERFORM event_toolbar USING e_object
                                              e_interactive.
  ENDMETHOD.                    "toolbar
  METHOD user_command.
    PERFORM event_ucomm USING e_ucomm.
  ENDMETHOD.                    "user_command
ENDCLASS.                    "l_cl_events_d0100 IMPLEMENTATION

DATA : go_action TYPE REF TO lcl_events_alvgrid.
