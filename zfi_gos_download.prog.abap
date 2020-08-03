*&---------------------------------------------------------------------*
*& Report ZFI_GOS_DOWNLOAD
************************************************************************
*                        
************************************************************************
*    Country      : Generic                                            *
*    ABAP Name    : ZFI_GOS_DOWNLOAD                                   *
*    Created by   : Nirav Gohel                                        *
*    Created on   : 16/07/2020   MM/DD/YYYY                            *
*    Document ref.:                                                    *                                                                      *
*    Description  : Download attachemnt from FB03                      *
*                                                                      *
************************************************************************
* Modification Log:                                                    *
*----------------------------------------------------------------------*
*    Date    | Name |  Request   | Description                         *
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------
REPORT zfi_gos_download.

INCLUDE zfi_gos_download_top.
INCLUDE zfi_gos_download_ss.
INCLUDE zfi_gos_download_forms.

START-OF-SELECTION.

  PERFORM : get_attach_data,
*            create_fcat,
            display_alv.
