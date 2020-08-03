*&---------------------------------------------------------------------*
*&  Include           ZFI_GOS_DOWNLOAD_SS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : s_budat FOR bkpf-budat,
                 s_bukrs FOR bkpf-bukrs OBLIGATORY,
                 s_gjahr FOR bkpf-gjahr OBLIGATORY,
                 s_belnr FOR bkpf-belnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS : cb_dwnl AS CHECKBOX USER-COMMAND uc1.
PARAMETERS : p_path TYPE string MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK b2.

*"To hide/unhide download path
AT SELECTION-SCREEN OUTPUT.
  IF cb_dwnl = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'GR1'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'GR1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  "Folder selection on P_path f4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM browse_directory CHANGING p_path.
