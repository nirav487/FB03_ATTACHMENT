class ZCL_GOS_ATTACHMENT definition
  public
  create public .

public section.

  class-methods GOS_GET_FILE_LIST
    importing
      !OBJTYPE type SWO_OBJTYP optional
      !OBJKEY type SWO_TYPEID optional
    exporting
      !T_ATTACHMENTS type ZTT_SOOD
    changing
      !ES_RETURN type RETURN optional .
  class-methods GOS_DOWNLOAD_FILE
    importing
      !FILEPATH type STRING optional
      !ATTACHMENT type SOOD optional
      !KEY type STRING optional
    changing
      !ES_RETURN type RETURN optional .
  class-methods ARCHIVELINK_DOWNLOAD
    importing
      !CREP_ID type C
      !FILE_PATH type STRING
      !DOC_ID type C .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GOS_ATTACHMENT IMPLEMENTATION.


  METHOD archivelink_download.

    DATA: lt_attachment   TYPE STANDARD TABLE OF soli." WITH HEADER LINE.
    DATA : lv_filesize     TYPE i.

    CALL FUNCTION 'SCMS_HTTP_GET'
      EXPORTING
        crep_id               = crep_id
        doc_id                = doc_id
      IMPORTING
        length                = lv_filesize
      TABLES
        data                  = lt_attachment
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
    IF sy-subrc = 0.
* Implement suitable error handling here
      "Downoad FIle to the path
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = file_path
          filetype                = 'BIN'
        CHANGING
          data_tab                = lt_attachment
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method GOS_DOWNLOAD_FILE.
    data: ex type ref to cx_root, text type string.
  data: ltp_sortfield type char30,
        lta_objcont type soli_tab,
        ltp_pathfile(1000) type c,
        ltp_filename type string,
        ltp_binfilesize type so_doc_len.

  try .
      concatenate attachment-objtp attachment-objyr attachment-objno into ltp_sortfield.
      import objcont_tab to lta_objcont from database soc3(dt) id ltp_sortfield.

      if sy-subrc = 0.
        concatenate filepath '\' key '_' attachment-objdes '.' attachment-file_ext into ltp_pathfile.

        replace '\\' with '\' into ltp_pathfile+2.
        translate ltp_pathfile using '/  '.

        ltp_binfilesize = attachment-objlen.

        call function 'SO_OBJECT_DOWNLOAD'
          exporting
            bin_filesize     = ltp_binfilesize
            filetype         = 'BIN'
            path_and_file    = ltp_pathfile
            extct            = attachment-extct
            no_dialog        = 'X'
          importing
            act_filename     = ltp_filename
          tables
            objcont          = lta_objcont
          exceptions
            file_write_error = 1
            invalid_type     = 2
            x_error          = 3
            kpro_error       = 4
            others           = 5.

        if sy-subrc <> 0.
*          call method zcl_oh_my_gos=>log_msg
*            exporting
*              i_code    = '021'
*              i_v1      = 'Error Calling: SO_OBJECT_DOWNLOAD'
*            receiving
*              rs_return = es_return.
        else.
*          es_return-text = 'SUCCESS'.
        endif.
      else.
*        call method zcl_oh_my_gos=>log_msg
*          exporting
*            i_code    = '021'
*            i_v1      = 'Error With Object Import From DB'
*          receiving
*            rs_return = es_return.
      endif.
    catch cx_root into ex.
      text = ex->get_text( ).

*      call method zcl_oh_my_gos=>log_msg
*        exporting
*          i_code    = ‘021’
*          i_v1      = text
*        receiving
*          rs_return = es_return.
  endtry.


*Testing:  Here is some sample code for a report program which loops through all attachments of an object and downloads them.
endmethod.


  method GOS_GET_FILE_LIST.
     types: begin of ts_key,
           foltp type so_fol_tp,
           folyr type so_fol_yr,
           folno type so_fol_no,
           objtp type so_obj_tp,
           objyr type so_obj_yr,
           objno type so_obj_no,
           forwarder type so_usr_nam,
         end of ts_key,

         begin of ts_attachment,
          foltp type so_fol_tp,
          folyr type so_fol_yr,
          folno type so_fol_no,
          objtp type so_obj_tp,
          objyr type so_obj_yr,
          objno type so_obj_no,
          brelguid type oblguid32,
          roletype type oblroltype,
         end of ts_attachment,

         tt_attachment type table of ts_attachment.

  data: ta_srgbtbrel type standard table of srgbtbrel,
        wa_srgbtbrel type srgbtbrel,
        lta_sood type standard table of sood,
        lwa_sood type sood, ltp_pathin(1000) type c,
        ltp_filename type string,
        ltp_sortfield type char30,
        lta_objcont type soli_tab,
        lta_attachments type tt_attachment,
        lwa_attachments like line of lta_attachments,
        lo_boritem type ref to cl_sobl_bor_item,
        lo_al_item type ref to cl_gos_al_item,
        li_link type ref to if_browser_link,
        ls_option type obl_s_relt,
        lt_options type obl_t_relt,
        ls_key type ts_key,
        ls_attachment type ts_attachment,
        lt_attachment type tt_attachment,
        lt_links type obl_t_link,
        ls_link  type obl_s_link,
        lp_linkid type blnk_inst,
        gs_lpor type sibflporb.

  if not objtype is initial and not objkey is initial.
    select * from srgbtbrel into table ta_srgbtbrel
      where instid_a eq objkey
       and typeid_a eq objtype
       and catid_a  eq 'BO'.

    if sy-subrc eq 0.
      sort ta_srgbtbrel by instid_a typeid_a catid_a.
      delete adjacent duplicates from ta_srgbtbrel comparing instid_a typeid_a catid_a.

      loop at ta_srgbtbrel into wa_srgbtbrel.
        clear: lt_attachment[], lta_attachments[].

        gs_lpor-instid = wa_srgbtbrel-instid_a.
        gs_lpor-typeid = wa_srgbtbrel-typeid_a.
        gs_lpor-catid  = wa_srgbtbrel-catid_a.

        ls_option-sign = 'I'.
        ls_option-option = 'EQ'.

        ls_option-low = 'ATTA'.
        append ls_option to lt_options.
        ls_option-low = 'NOTE'.
        append ls_option to lt_options.
        ls_option-low = 'URL'.
        append ls_option to lt_options.

        try.
            call method cl_binary_relation=>read_links_of_binrels
              exporting
                is_object           = gs_lpor
                it_relation_options = lt_options
                ip_role             = 'GOSAPPLOBJ'
              importing
                et_links            = lt_links.

            loop at lt_links into ls_link.
              case ls_link-typeid_b .
                when 'MESSAGE'.
                  ls_key = ls_link-instid_b.
                  move-corresponding ls_key to ls_attachment.
                  ls_attachment-roletype = ls_link-roletype_b.
                  if ls_link-brelguid is initial.
                    ls_attachment-brelguid = ls_link-relguidold.
                  else.
                    ls_attachment-brelguid = ls_link-brelguid.
                  endif.
                  append ls_attachment to lt_attachment.
                when others.
                  continue.
              endcase.
            endloop.
          catch cx_obl_parameter_error .
          catch cx_obl_internal_error .
          catch cx_obl_model_error .
          catch cx_root.
        endtry.
      endloop.

      lta_attachments[] = lt_attachment[].
      check lines( lta_attachments ) > 0.

      select * from sood into table lta_sood
        for all entries in lta_attachments
        where
          objtp = lta_attachments-objtp  and
          objyr = lta_attachments-objyr  and
          objno = lta_attachments-objno.
      if sy-subrc eq 0.
        t_attachments[] = lta_sood.
*        es_return-text = 'SUCCESS'.
      endif.

      data rcode type i.
      data objhead_tab type table of soli.
      data objcont_tab type table of soli.
      data objpara_tab type table of selc.
      data objparb_tab type table of soop1.
      data sood_key type soodk.
      data hex_mode type sonv-flag.
      field-symbols <fs> type line of ztt_sood.

      loop at t_attachments assigning <fs>.
        if not ( <fs>-objtp is initial or <fs>-objyr is initial or <fs>-objno is initial ).
          concatenate <fs>-objtp <fs>-objyr <fs>-objno into sood_key.

          perform socx_select in program sapfsso0
                              tables objhead_tab objcont_tab
                                     objpara_tab objparb_tab
                              using  sood_key
                                     hex_mode
                                     rcode.
          if rcode eq 0.
            data moff type i.
            data l_param_search type soli-line.
            data l_param_head type soli-line.
            data value type soli-line.
            data wa_objhead_tab like line of objhead_tab.
            data lt_url_tab type table of so_url.
            data ld_url_tab_size type sytabix.
            l_param_search = '&SO_FILENAME'.
            translate l_param_search to upper case.
            loop at objhead_tab into wa_objhead_tab.
              clear moff.
              find '=' in wa_objhead_tab-line match offset moff.
              check sy-subrc = 0.
              l_param_head = wa_objhead_tab-line(moff).
              translate l_param_head to upper case.
              if l_param_head = l_param_search.
                add 1 to moff.
                value = wa_objhead_tab-line+moff.
                if not ( value is initial ).
                  split value at '.' into table lt_url_tab.
                  describe table lt_url_tab lines ld_url_tab_size.
                  if ld_url_tab_size gt 1.
                    read table lt_url_tab index ld_url_tab_size into <fs>-acnam.
                  endif.
                endif.
              endif.
            endloop.
          endif.
        endif.
      endloop.
    else.
*      call method zcl_oh_my_gos=>log_msg
*        exporting
*          i_code    = '001'
*          i_v1      = 'There are no Attachments on this Business Object'
*        receiving
*          rs_return = es_return.
    endif.
  else.
*    call method zcl_oh_my_gos=>log_msg
*      exporting
*        i_code    = '021'
*        i_v1      = 'No Business Object and/or Key Specified'
*      receiving
*        rs_return = es_return.
  endif.
  endmethod.
ENDCLASS.
