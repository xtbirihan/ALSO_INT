class ZCL_WC_REPLENISHMENT definition
  public
  final
  create public .

public section.

  interfaces ZIF_WC_REPLENISHMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WC_REPLENISHMENT IMPLEMENTATION.


  METHOD zif_wc_replenishment~check_dst_bin_lptyp.

    CLEAR rv_not_ok.

    DATA(rs_pipo_st_typ) = NEW zcl_crud_ztint_pipo_lptyp( )->select_single_by_nltyp( iv_lgnum = iv_lgnum
                                                                                     iv_nltyp = iv_nltyp ).

    DATA(ls_bin_data) = zif_wc_replenishment~read_bin_data( iv_lgnum = iv_lgnum
                                                            iv_lgpla = iv_dst_lgpla ).

    IF rs_pipo_st_typ-pipo_lptyp <> ls_bin_data-lptyp.
      rv_not_ok = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~check_prod_ean_mpn.

    DATA:
        lv_matnr TYPE matnr.

    "Determine possible material numbers
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = iv_prod_ean_mpn
      IMPORTING
        output = lv_matnr
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      lv_matnr = iv_prod_ean_mpn.
    ENDIF.

    SELECT FROM mara
         FIELDS matnr
          WHERE matnr = @lv_matnr
             OR ean11 = @iv_prod_ean_mpn
             OR mfrpn = @iv_prod_ean_mpn
           INTO TABLE @rt_matnr_det.

    IF rt_matnr_det IS INITIAL.
      SELECT FROM mean
           FIELDS matnr
            WHERE ean11 = @iv_prod_ean_mpn
             INTO TABLE @rt_matnr_det.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~check_st_typ.

    DATA:
          ls_t301 TYPE /scwm/s_t301.

    CALL FUNCTION '/SCWM/T301_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
        iv_lgtyp  = iv_lgtyp
      IMPORTING
        es_t301   = ls_t301
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE ID sy-msgid
                 TYPE sy-msgty
               NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~create_wt.

    DATA:
      ls_create_wt TYPE /scwm/s_to_create_int,
      lt_create_wt TYPE /scwm/tt_to_create_int,
      lv_severity  TYPE bapi_mtype.

    CLEAR ls_create_wt.

    ls_create_wt-matid         = is_wt_cur-matid.
    ls_create_wt-batchid       = is_wt_cur-batchid.
    ls_create_wt-owner         = is_wt_cur-owner.
    ls_create_wt-owner_role    = is_wt_cur-owner_role.
    ls_create_wt-entitled      = is_wt_cur-entitled.
    ls_create_wt-entitled_role = is_wt_cur-entitled_role.
    ls_create_wt-anfme         = iv_anfme.
    ls_create_wt-altme         = iv_altme.
    ls_create_wt-nltyp         = is_wt_cur-nltyp.
    ls_create_wt-nlpla         = is_wt_cur-nlpla.
    ls_create_wt-vlpla         = is_wt_cur-vlpla.
    ls_create_wt-vltyp         = is_wt_cur-vltyp.
    ls_create_wt-cat           = is_wt_cur-cat.
    ls_create_wt-procty        = is_wt_cur-procty.

    ls_create_wt-vlenr = iv_huident. " Selected HU ident
    ls_create_wt-nlenr = iv_huident.

    APPEND ls_create_wt TO lt_create_wt.

    " Set warehouse number in global settings
    /scwm/cl_tm=>set_lgnum( iv_lgnum ).

    " Create warehouse task to move HU
    CALL FUNCTION '/SCWM/TO_CREATE'
      EXPORTING
        iv_lgnum       = iv_lgnum
*       iv_wtcode      = wmegc_wtcode_adhoc_hu
        iv_update_task = abap_false
        iv_commit_work = abap_true
        it_create      = lt_create_wt
      IMPORTING
        et_ltap_vb     = et_ltap_vb
        et_bapiret     = et_protocol
        ev_severity    = lv_severity. " Returns highest severity

    IF lv_severity CA wmegc_severity_eax.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_workstation
        EXPORTING
          messages = et_protocol.

    ELSEIF et_ltap_vb IS NOT INITIAL.
      READ TABLE et_ltap_vb INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_ordim_o>).

      " Warehouse task &1 created
      MESSAGE i016(/scwm/l3) WITH <ls_ordim_o>-tanum DISPLAY LIKE /scwm/cl_log=>msgty_success.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~get_defaults.

    GET PARAMETER ID '/SCWM/LGN' FIELD es_defaults-lgnum.
    GET PARAMETER ID '/SCWM/WST' FIELD es_defaults-workcenter.

    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = es_defaults-lgnum
        iv_workstation = es_defaults-workcenter
      IMPORTING
        es_workst      = es_tworkst
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~get_lgtyp.

    CALL FUNCTION '/SCWM/T301_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
        iv_lgtyp  = iv_lgtyp
      IMPORTING
        es_t301   = rs_t301
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~get_open_wts.

    DATA:
           lr_vlpla TYPE rseloption.

    lr_vlpla = VALUE #( FOR lv_lgpla IN it_lgpla ( sign   = wmegc_sign_inclusive
                                                   option = wmegc_option_eq
                                                   low    = lv_lgpla ) ).

    CALL FUNCTION '/SCWM/TO_GET_WIP'
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_open    = abap_true
        iv_srcdata = abap_true
        is_selcrit = VALUE /scwm/s_to_selcrit_mon( r_vlpla = lr_vlpla
                                                   r_nltyp = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                        option = wmegc_option_eq
                                                                        low    = iv_nltyp ) ) )
      IMPORTING
        et_to      = et_to.

  ENDMETHOD.


  METHOD zif_wc_replenishment~get_wc_st_typ.

    DATA:
          ls_workst TYPE /scwm/tworkst.

    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_workstation = iv_workcenter
      IMPORTING
        es_workst      = ls_workst
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    rv_st_typ = ls_workst-lgtyp.

  ENDMETHOD.


  METHOD zif_wc_replenishment~pack_n_print.
    CLEAR: et_bapiret, et_bapiret.

    zif_wc_replenishment~update_n_confirm_wt(
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_tanum    = iv_tanum
        is_confirm  = is_confirm
        is_conf_exc = is_conf_exc
      IMPORTING
        et_bapiret  = et_bapiret ).

    zif_wc_replenishment~create_wt(
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_huident  = iv_huident
        iv_anfme    = iv_anfme
        iv_altme    = iv_altme
        is_wt_cur   = is_wt_cur
      IMPORTING
        et_ltap_vb  = et_ltap_vb
        et_protocol = et_bapiret ).

  ENDMETHOD.


  METHOD zif_wc_replenishment~read_bin_data.

    CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
      EXPORTING
        iv_lgnum      = iv_lgnum
        iv_lgpla      = iv_lgpla
      IMPORTING
        es_lagp       = rs_lagp
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE ID sy-msgid
                 TYPE sy-msgty
               NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~read_hu_data.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme
        iv_lgnum   = iv_lgnum
        iv_huident = iv_huident
      IMPORTING
        es_huhdr   = es_huhdr
        et_huitm   = et_huitm
      EXCEPTIONS
        deleted    = 1
        not_found  = 2
        error      = 3
        OTHERS     = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~read_mat_data.
**********************************************************************
    "<aahmedov>-24072023
    "Changes description: added iv_lgtyp & et_mat_lgtyp
**********************************************************************

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_lgnum      = iv_lgnum
            iv_matid      = iv_matid
            iv_entitled   = iv_entitled
            iv_lgtyp      = iv_lgtyp
          IMPORTING
            es_mat_global = es_mat_global
            es_mat_lgnum  = es_mat_lgnum
            et_mat_uom    = et_mat_uom
            et_mat_lgtyp  = et_mat_lgtyp.

      CATCH /scwm/cx_md.
        RAISE EXCEPTION TYPE zcx_workstation.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_wc_replenishment~read_pack_spec.

    DATA:
          lt_ps_keys TYPE /scwm/tt_ps_header_key.

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matid = VALUE #( ( matid = iv_matid ) ) )
        it_status_rng    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low    = /scwm/cl_ppelipak_cntl_const=>gc_status_active ) )
      IMPORTING
        et_ps_keys       = lt_ps_keys.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps          = VALUE #( lt_ps_keys[ 1 ]-guid_ps OPTIONAL )
        iv_read_elements    = abap_true
        iv_no_buffer        = abap_false
      IMPORTING
        et_packspec_content = rt_packspec_content
      EXCEPTIONS
        error               = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~read_storage_bins.

    CALL FUNCTION '/SCWM/LAGP_READ_RANGE'
      EXPORTING
        iv_lgnum    = iv_lgnum
        ir_lgtyp    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                          option = wmegc_option_eq
                                          low    = iv_lgtyp ) )
        ir_lptyp    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                          option = wmegc_option_eq
                                          low    = iv_lptyp ) )
      IMPORTING
        et_lagp     = et_lagp
      EXCEPTIONS
        wrong_input = 1
        not_found   = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
              MESSAGE ID sy-msgid
                 TYPE sy-msgty
               NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~return_rem_qty.

    DATA:
      ls_workst  TYPE  /scwm/tworkst,
      ls_huhdr   TYPE  /scwm/s_huhdr_int,
      lo_wm_pack TYPE REF TO /scwm/cl_wm_packing.

    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = lo_wm_pack ).

    /scwm/cl_wm_packing=>/scwm/if_pack_bas~cleanup( ).

    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_workstation = iv_workstation
      IMPORTING
        es_workst      = ls_workst
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_wm_pack->init_by_workstation(
      EXPORTING
        is_workstation = ls_workst
      EXCEPTIONS
        error          = 1                " Error, see log
        OTHERS         = 2
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    zif_wc_replenishment~read_hu_data(
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_huident = iv_huident
      IMPORTING
        es_huhdr   = ls_huhdr ).

    lo_wm_pack->hu_return_transfer(
      EXPORTING
        iv_hu  = ls_huhdr-guid_hu
      EXCEPTIONS
        error  = 1
        OTHERS = 2
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD lo_wm_pack->save
      EXPORTING
        iv_commit = space
        iv_wait   = space
      EXCEPTIONS
        OTHERS    = 99.

    IF sy-subrc <> 0.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

    "this clears go_prot, which in turn
    "leads to an exception, when Pack & Print is used
    lo_wm_pack->after_save( ).

    " WT is being created in background for remaining qty
    MESSAGE i068(zmc_workstation) DISPLAY LIKE /scwm/cl_log=>msgty_success.

  ENDMETHOD.


  METHOD zif_wc_replenishment~set_defaults.

    DATA:
      ls_workst TYPE  /scwm/tworkst,
      ls_t300   TYPE /scwm/s_t300,
      lt_def_db TYPE STANDARD TABLE OF /scmb/esdus.

    CALL FUNCTION '/SCWM/T300_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      IMPORTING
        es_t300   = ls_t300
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      " Enter a valid warehouse number
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e007(zmc_workstation).
    ENDIF.

    CALL FUNCTION '/SCWM/TWORKST_READ_SINGLE'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_workstation = iv_workcenter
      IMPORTING
        es_workst      = ls_workst
      EXCEPTIONS
        error          = 1
        not_found      = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      " Enter a valid Workstation Location
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e009(zmc_workstation).
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_replenishment~update_n_confirm_wt.

    DATA:
      lv_lines    TYPE numc4,
      lv_severity TYPE bapi_mtype,
      lt_ordim_c  TYPE /scwm/tt_ordim_c.

    DATA(ls_confirm)  = is_confirm.
    DATA(ls_conf_exc) = is_conf_exc.

    " Get next PAPOS
    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
      EXPORTING
        iv_lgnum     = iv_lgnum
        iv_tanum     = iv_tanum
        iv_flglock   = space
      IMPORTING
        et_ordim_c   = lt_ordim_c
      EXCEPTIONS
        wrong_input  = 1
        not_found    = 2
        foreign_lock = 3
        error        = 4
        OTHERS       = 5.

    IF sy-subrc = 0.
*   Confirmed items found
      DESCRIBE TABLE lt_ordim_c LINES lv_lines.
      ls_confirm-papos = lv_lines + 1.
    ELSE.
*   Confirmed items not found.
      ls_confirm-papos = 1.
    ENDIF.

    IF ls_conf_exc IS NOT INITIAL.
      ls_conf_exc-papos = ls_confirm-papos.
    ENDIF.


    call FUNCTION '/SCWM/TO_INIT_NEW'
      EXPORTING
        iv_lgnum       = iv_lgnum
      .

    CALL FUNCTION '/SCWM/TO_CONFIRM'
      EXPORTING
        iv_lgnum       = iv_lgnum
        iv_wtcode      = wmegc_wtcode_pack
        iv_update_task = abap_false
        iv_commit_work = abap_false
        it_conf        = VALUE /scwm/to_conf_tt( ( ls_confirm ) )
        it_conf_exc    = VALUE /scwm/tt_conf_exc( ( ls_conf_exc ) )
      IMPORTING
        et_bapiret     = et_bapiret
        ev_severity    = lv_severity.

    IF lv_severity CA wmegc_severity_ea.
      ROLLBACK WORK.

      RAISE EXCEPTION TYPE zcx_workstation
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
           EXPORTING
             messages = et_bapiret.
    ELSE.
      COMMIT WORK AND WAIT.
      /scwm/cl_tm=>cleanup( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
