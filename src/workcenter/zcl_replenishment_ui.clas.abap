class ZCL_REPLENISHMENT_UI definition
  public
  inheriting from ZCL_DEFAULT_UI_FUNCTIONS
  final
  create public .

public section.

  interfaces ZIF_WS_SUBSCR_UI .
  interfaces ZIF_WC_UI_MAIN .

  constants C_MAIN_SCREEN type SY-DYNNR value '0101' ##NO_TEXT.
  constants C_MAIN_STATUS type STRING value 'MAIN_SCREEN' ##NO_TEXT.
  constants C_MAIN_TITTLE type STRING value 'MAIN_TITLE' ##NO_TEXT.
  constants C_SUB_SCREEN type SY-DYNNR value '0100' ##NO_TEXT.
  constants C_TAB_REPL_UI type SCXTAB_TABSTRIP-ACTIVETAB value 'REPL_UI' ##NO_TEXT.
  data C_SUBRSCREEN_REPID type SYREPID value 'SAPLZFG_REPLENISHMENT_UI' ##NO_TEXT.

  class-methods START .
protected section.
private section.

  constants C_EXCODE_CHANGE_QTY type /SCWM/DE_EXCCODE value 'DIFH' ##NO_TEXT.
  constants C_FUNC_DEFAULTS type SYUCOMM value 'DEFAULTS' ##NO_TEXT.
  constants C_FUNC_FX_CHNG_POS type SYUCOMM value 'CHNG_POS' ##NO_TEXT.
  constants C_FUNC_FX_CHNG_RPL_DEST type SYUCOMM value 'CHNG_RPL_D' ##NO_TEXT.
  constants C_FUNC_FX_MODIFY type SYUCOMM value 'MODIFY' ##NO_TEXT.
  constants C_FUNC_FX_PACK_N_PRINT type SYUCOMM value 'PCK_N_PRNT' ##NO_TEXT.
  constants C_FUNC_FX_SKIP_WT type SYUCOMM value 'SKIP_WT' ##NO_TEXT.
  constants C_FUNC_LEAVE type SYUCOMM value 'LEAVE' ##NO_TEXT.
  data MO_FUNCTION_LOG type ref to /SCWM/CL_LOG .
  data MO_PTWY_CART_SP type ref to ZIF_WS_PTWY_CART_SP .
  data MS_CONF_EXC type /SCWM/S_CONF_EXC .
  data MS_CUR_WT type /SCWM/S_TO_DET_MON .
  data MS_WC_CHNG_BIN type ZSTR_WC_CHNG_BIN .
  data MS_WC_CHNG_DEST type ZSTR_WC_CHNG_DEST .
  data MS_WC_MOD_REPL type ZSTR_WC_MOD_REPL .
  data MS_WC_REPLENISHMENT type ZSTR_WC_REPLENISHMENT .
  data MT_BINS_ALL type /SCWM/TT_LAGP .
  data MV_BALLOGHNDL type BALLOGHNDL .
  data MV_CART_REL type ABAP_BOOL .
  data MV_CURR_BIN_IND type SYST_TABIX .
  data MV_NEXT_BIN_IND type SYST_TABIX .
  data MV_SHOW_TOTE type ABAP_BOOL .
  data MV_ST_TYP type /SCWM/LGTYP .

  methods SKIP_WT
    raising
      ZCX_WORKSTATION .
  methods SKIP_WT_ORIGINAL
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods PRODUCT_CHANGED
    importing
      !IV_PROD_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods RETURN_REM_QTY
    importing
      !IV_HUIDENT type ZDE_RET_HU
    raising
      ZCX_WORKSTATION .
  methods FILL_PRODUCT_INFO
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods PACK_N_PRINT
    raising
      ZCX_WORKSTATION .
  methods DEST_HU_CHANGED
    importing
      !IV_HUIDENT type /SCWM/DE_HUIDENT
    raising
      ZCX_WORKSTATION .
  methods GET_WC_ST_TYP .
  methods FILL_PROP_REPL
    importing
      !IV_REPL_QTY type /SCWM/LTAP_VSOLM
      !IV_UOM type MEINS
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods FINISH_FUNCTION_LOG
    importing
      !IV_EXTERNAL_ID type STRING
      !IV_SAVE_ONLY_IF_ERROR type ABAP_BOOL
      !IV_DISPLAY type ABAP_BOOL
    exporting
      value(EV_DISPLAYED) type ABAP_BOOL .
  methods START_FUNCTION_LOG .
ENDCLASS.



CLASS ZCL_REPLENISHMENT_UI IMPLEMENTATION.


  METHOD dest_hu_changed.
    mv_cart_rel = zcl_crud_ztinb_curst_styp=>select_single_by_lgtyp(
                      iv_lgnum  = ms_defaults-lgnum                 " Warehouse Number/Warehouse Complex
                      iv_lgtyp  = ms_cur_wt-nltyp                 " Storage Type
                  )-putaway_cart_rel.
    IF mv_cart_rel EQ abap_true.
      mo_ptwy_cart_sp->interpret_huident(
        EXPORTING
          iv_huident       = iv_huident                 " Handling Unit Identification
        IMPORTING
          es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
          ev_no_cart       = DATA(lv_no_cart)
          ev_cart_id       = DATA(lv_cart_id)                      " Handling Unit Identification
          ev_pos_on_cart   = DATA(lv_pos_on_cart)                  " Handling Unit Identification
          ev_tote_id       = DATA(lv_tote_id)                      " Handling Unit Identification
          ev_comp_on_tote  = DATA(lv_comp_on_tote)                  " Handling Unit Identification
      ).
      IF lv_pos_on_cart IS INITIAL
         AND lv_tote_id IS INITIAL
         AND lv_comp_on_tote IS INITIAL.
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e055(zmc_workstation) WITH iv_huident.
      ENDIF.

      ms_wc_replenishment-pmat_no = ls_cart_settings-pmat.

      "BEGIN MOD <AAHMEDOV>-20240123
      /scmtms/cl_res_srv=>get_pmat_description(
        EXPORTING
          iv_pmatnr      = ms_wc_replenishment-pmat_no                  " Product ID
        IMPORTING
          ev_description = DATA(lv_pmtext) ).                " Product ID Description

      ms_wc_replenishment-pmtext = lv_pmtext.
      "END MOD <AAHMEDOV>-20240123

    ENDIF.
    ms_wc_replenishment-huident_prop = iv_huident.

  ENDMETHOD.


  METHOD fill_product_info.

    DATA:
      lt_huitm            TYPE /scwm/tt_huitm_int,
      lo_matlwhst_reader TYPE REF TO /sapapo/cl_matlwhst_reader.

    SELECT SINGLE * FROM mara
                    INTO @DATA(ls_mara)
                   WHERE matnr = @ms_wc_replenishment-matnr_conf.

*    select single
    mo_repl_srv->read_hu_data(
      EXPORTING
        iv_lgnum   = ms_defaults-lgnum
        iv_huident = ms_cur_wt-vlenr
      IMPORTING
        et_huitm   = lt_huitm ).

    cs_wc_repl-matnr_inf      = ms_wc_replenishment-matnr_conf.
    cs_wc_repl-maktx_inf      = ms_wc_replenishment-maktx_conf.
    cs_wc_repl-mpn            = ls_mara-mfrpn.
    cs_wc_repl-ean            = ls_mara-ean11.
    cs_wc_repl-tot_av_pal_qty = lt_huitm[ 1 ]-quan.
    cs_wc_repl-repl_qty       = ms_cur_wt-vsolm.

    fill_prop_repl(
      EXPORTING
        iv_repl_qty = ms_cur_wt-vsolm
        iv_uom      = ms_cur_wt-meins
      CHANGING
        cs_wc_repl  = cs_wc_repl ).

  ENDMETHOD.


  METHOD fill_prop_repl.

    DATA:
      lt_packspec_content TYPE /scwm/tt_packspec_nested,
      mo_cut              TYPE REF TO /scwm/cl_hu_typ.

    mo_cut = NEW /scwm/cl_hu_typ( ).

    "-----------------------------------------------
    " Proposed repl. params
    "-----------------------------------------------
    cs_wc_repl-repl_qty_prop    = iv_repl_qty.
    cs_wc_repl-repl_uom_pc_prop = iv_uom.

    mo_repl_srv->read_mat_data(
      EXPORTING
        iv_lgnum      = ms_defaults-lgnum
        iv_matid      = ms_cur_wt-matid
        iv_entitled   = ms_cur_wt-entitled
        iv_lgtyp      = ms_cur_wt-nltyp
      IMPORTING
        es_mat_global = DATA(ls_bin_data)
        es_mat_lgnum  = DATA(ls_mat_lgnum)
        et_mat_uom    = DATA(lt_mat_uom)
        et_mat_lgtyp  = DATA(lt_mat_lgtyp) ).

    zcl_crud_ztlgtyp_algo=>select_single_by_lgnum_lgtyp(
      EXPORTING
        iv_lgnum        = ms_wc_replenishment-lgnum                " Warehouse Number/Warehouse Complex
        iv_lgtyp        = ms_wc_replenishment-nltyp                " Storage Type
      IMPORTING
        es_ztlgtyp_algo = DATA(ls_lgtyp_algo)                " Capacity calculation algorithms for storage types
    ).

    IF ls_lgtyp_algo-algo EQ zif_wme_c=>gs_algorithms-cuboid.

      IF ls_mat_lgnum-zz1_keepcar_whd = abap_true.
        cs_wc_repl-store_mc_or_pc = 'Store as MC' ##NO_TEXT.
      ELSE.
        cs_wc_repl-store_mc_or_pc = 'Store as PC' ##NO_TEXT.
      ENDIF.

      mv_show_tote = abap_false.

    ELSEIF ls_lgtyp_algo-algo EQ zif_wme_c=>gs_algorithms-flowrack.

      IF lt_mat_lgtyp[ 1 ]-zz1_dirrpl_stt EQ abap_true.
        EXIT.
      ENDIF.

      " UOM = PC per Tote
      lt_packspec_content = mo_repl_srv->read_pack_spec( iv_matid = ms_cur_wt-matid ).
      TRY.
          DATA(lv_qty_per_tot)  = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-trgqty ).
          DATA(lv_uom_per_tote) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-total_unit ).

*          cs_wc_repl-pmat_no = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-hu_mat ).
*          cs_wc_repl-pmtext  = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-hu_mat_text ).

          cs_wc_repl-hutyp = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-hutyp ).
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      mo_cut->/scwm/if_hu_typ~read_hu_type_single(
        EXPORTING
          iv_hutyp    = cs_wc_repl-hutyp
        IMPORTING
          ev_hutyptxt = cs_wc_repl-hutyp_text ).
*        cs_wc_repl-nof_pc_per_tote = lv_qty_per_tot.

      TRY.
          IF iv_repl_qty < lv_qty_per_tot. " If the replenishment quantity is smaller than the qty per tote - just 1 tote
            cs_wc_repl-nof_quan_tots = 1.
          ELSE.
            cs_wc_repl-nof_quan_tots = iv_repl_qty / lv_qty_per_tot.
          ENDIF.
          cs_wc_repl-uom_pc_tote   = lv_uom_per_tote.
        CATCH cx_sy_zerodivide ##NO_HANDLER.
      ENDTRY.

      TRY.
          cs_wc_repl-amt_per_tote = lv_qty_per_tot.
        CATCH cx_sy_zerodivide ##NO_HANDLER.
      ENDTRY.

*        DATA(lv_base_uom) = ls_bin_data-meins. " Base UoM
*        "Always to use the smallest available Z* UOM
*        DELETE lt_mat_uom WHERE meinh = lv_base_uom.
*        SORT lt_mat_uom BY umrez ASCENDING.
*
*        DATA(ls_carton_base) = VALUE #( lt_mat_uom[ 1 ] OPTIONAL ).

      cs_wc_repl-mc_per_tote = 'PC per tote'.
      cs_wc_repl-store_mc_or_pc = 'Store as Tote' ##NO_TEXT.
      mv_show_tote = abap_true.
      "-----------------------------------------------

    ENDIF.

    ms_wc_replenishment = cs_wc_repl.

  ENDMETHOD.


  METHOD FINISH_FUNCTION_LOG.
    DATA: ls_display_profile TYPE  bal_s_prof.
    IF iv_save_only_if_error EQ abap_true AND mo_function_log->get_severity( ) NA 'AEX'.
      RETURN.
    ENDIF.
    mo_function_log->save_applog(
      EXPORTING
        is_log       = VALUE #( extnumber = |Warehouse { ms_defaults-lgnum }, Workcenter. { ms_defaults-workcenter }, Function { iv_external_id } |
                                object = zif_wme_c=>gs_msgobj-zewm
                                subobject = zif_wme_c=>gs_msgsubobj-zworkcenter )
      IMPORTING
        ev_loghandle = mv_balloghndl ) ##NO_TEXT.                 " Log Handle

    TRY.
        mo_function_log->save_applog2db( iv_loghandle = mv_balloghndl ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.

    IF iv_display EQ abap_true AND mo_function_log->get_severity( ) CA 'AEX'.
      TRY.
          CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
            IMPORTING
              e_s_display_profile = ls_display_profile.                  " Display Profile
          mo_function_log->display_log( iv_loghandle = mv_balloghndl is_display_profile = ls_display_profile ).
          ev_displayed = abap_true.
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_wc_st_typ.

    IF mv_st_typ IS INITIAL.
      mv_st_typ = mo_repl_srv->get_wc_st_typ( iv_lgnum      = ms_defaults-lgnum
                                              iv_workcenter = ms_defaults-workcenter ).
    ENDIF.

  ENDMETHOD.


  METHOD pack_n_print.
    DATA: lv_ptway_cart     TYPE /scwm/huident,
          lv_ptway_cart_pos TYPE /scwm/huident,
          lv_cancelled      TYPE flag.

    DATA:
      ls_confirm TYPE /scwm/to_conf,
      lt_ltap_vb TYPE /scwm/tt_ltap_vb,
      lt_bapiret TYPE bapirettab.

    "IF putawy cart relevant and tote is entered then cart id and pos ON cart is requested
    IF mv_cart_rel EQ abap_true.
      mo_ptwy_cart_sp->interpret_huident(
        EXPORTING
          iv_huident       = ms_wc_replenishment-huident_prop                 " Handling Unit Identification
        IMPORTING
          es_cart_settings = DATA(ls_cart_settings)                 " Cart Barcode
      ).
      IF ls_cart_settings-pckg_type EQ mo_ptwy_cart_sp->c_pckg_type_tote.
        CALL FUNCTION 'Z_INB_PTWAY_CART_INFO'
          EXPORTING
            iv_huident        = ms_wc_replenishment-huident_prop
            io_ptwy_cart_sp   = mo_ptwy_cart_sp
          IMPORTING
            ev_ptway_cart     = lv_ptway_cart
            ev_ptway_cart_pos = lv_ptway_cart_pos
            ev_cancelled      = lv_cancelled.
        IF lv_cancelled EQ abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    start_function_log( ).
    TRY.
        ls_confirm = CORRESPONDING #( ms_cur_wt ).

        IF ms_conf_exc IS NOT INITIAL AND ms_wc_mod_repl IS NOT INITIAL.
          "Change replenish qty
          ls_confirm-nista = ms_wc_mod_repl-new_qty .
          ls_confirm-ndifa = ms_cur_wt-vsolm - ms_wc_mod_repl-new_qty.
        ELSE.
          ls_confirm-nista = ms_cur_wt-vsolm.
        ENDIF.

        ls_confirm-nlenr = ms_wc_replenishment-huident_prop.

        IF mv_cart_rel EQ abap_true.
          TRY.
              DATA(ls_pmat) = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(  iv_prodno    = ms_wc_replenishment-pmat_no ).

            CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
              RAISE EXCEPTION TYPE zcx_workstation MESSAGE e057(zmc_workstation) WITH ms_wc_replenishment-pmat_no.
          ENDTRY.
          DATA(lv_pmatid) = ls_pmat-prodid.

          mo_ptwy_cart_sp->pack_and_print_repl(
            EXPORTING
              iv_pmatid   = lv_pmatid
              iv_cart_id  = lv_ptway_cart
              iv_pos_on_cart = lv_ptway_cart_pos
              iv_tanum    = ms_cur_wt-tanum
              is_confirm  = ls_confirm
              is_conf_exc = ms_conf_exc
              iv_huident  = ms_wc_replenishment-huident_prop
              iv_anfme    = ms_wc_replenishment-repl_qty_prop
              iv_altme    = ms_wc_replenishment-repl_uom_pc_prop
              is_wt_cur   = ms_cur_wt ).
        ELSE.
          mo_repl_srv->pack_n_print(
            EXPORTING
              iv_lgnum    = ms_defaults-lgnum
              iv_tanum    = ms_cur_wt-tanum
              is_confirm  = ls_confirm
              is_conf_exc = ms_conf_exc
              iv_huident  = ms_wc_replenishment-huident_prop
              iv_anfme    = ms_wc_replenishment-repl_qty_prop
              iv_altme    = ms_wc_replenishment-repl_uom_pc_prop
              is_wt_cur   = ms_cur_wt
            IMPORTING
              et_ltap_vb  = lt_ltap_vb
              et_bapiret  = lt_bapiret ).
        ENDIF.

        CLEAR: ms_conf_exc.
      CATCH zcx_workstation INTO DATA(lx_ws).
        /scwm/cl_tm=>cleanup( ).
        ROLLBACK WORK.

        DATA(lv_error) = abap_true.
        mo_function_log->add_log( it_prot = lx_ws->messages ).

    ENDTRY.
    IF lv_error EQ abap_true.
      finish_function_log(
        EXPORTING
          iv_external_id        = 'Pack and Print HU' ##no_text
          iv_save_only_if_error = abap_false
          iv_display            = abap_true
        IMPORTING
          ev_displayed          = DATA(lv_displayed)
      ).
      IF lv_displayed EQ abap_false.
        RAISE EXCEPTION lx_ws.
      ENDIF.
      IF lx_ws->no_goods_issue EQ abap_false. "Not a goods issue problem
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD product_changed.

    DATA:
      lv_conf_hu          TYPE char40.

    DATA(ls_wc_repl_orig) = ms_wc_replenishment.

    DATA(lt_matnr_det) = mo_repl_srv->check_prod_ean_mpn( iv_prod_ean_mpn = iv_prod_ean_mpn ).
    DATA(ls_matnr_det) = VALUE #( lt_matnr_det[ 1 ] OPTIONAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = cs_wc_repl-huident_conf
      IMPORTING
        output = lv_conf_hu.

    IF ls_matnr_det-matnr <> cs_wc_repl-matnr_conf AND
       iv_prod_ean_mpn <> lv_conf_hu AND
       iv_prod_ean_mpn <> cs_wc_repl-vlpla_conf .

      CLEAR: ms_wc_replenishment.
      ms_wc_replenishment-lgpla = ls_wc_repl_orig-lgpla.
      ms_wc_replenishment-nltyp = ls_wc_repl_orig-nltyp.

      ms_wc_replenishment-vlpla_conf   = ls_wc_repl_orig-vlpla_conf.
      ms_wc_replenishment-huident_conf = ls_wc_repl_orig-huident_conf.
      ms_wc_replenishment-matnr_conf   = ls_wc_repl_orig-matnr_conf.
      ms_wc_replenishment-maktx_conf   = ls_wc_repl_orig-maktx_conf.

      cs_wc_repl = ms_wc_replenishment.

      "Confirm Product does not match &1 Prod, &2 HU, or Source Bin &3
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e046(zmc_workstation) WITH cs_wc_repl-matnr_conf lv_conf_hu cs_wc_repl-vlpla_conf.
    ENDIF.

    fill_product_info( CHANGING cs_wc_repl = cs_wc_repl ).

  ENDMETHOD.


  METHOD return_rem_qty.

    DATA:
      lv_wt_hu TYPE /scwm/de_huident,
      lv_hu    TYPE /scwm/de_huident.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ms_cur_wt-vlenr
      IMPORTING
        output = lv_wt_hu.

    " Check if the same HU is to be returned
    " The user should be able to scan the bin location
    IF iv_huident <> lv_wt_hu AND iv_huident <> ms_cur_wt-vlpla.
      " HU number or Source location does not match.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e045(zmc_workstation).
    ENDIF.

    IF iv_huident = ms_cur_wt-vlpla.
      lv_hu = ms_cur_wt-vlenr.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = iv_huident
        IMPORTING
          output = lv_hu.
    ENDIF.

    mo_repl_srv->return_rem_qty( iv_lgnum       = ms_defaults-lgnum
                                 iv_workstation = ms_defaults-workcenter
                                 iv_huident     = lv_hu ).

    ms_wc_replenishment-huident_rem = iv_huident.

  ENDMETHOD.


  METHOD skip_wt.

    DATA:
          lt_lagp_use    TYPE  /scwm/tt_lagp.

    DATA(ls_wc_repl_orig) = ms_wc_replenishment.

    mo_repl_srv->get_open_wts(
      EXPORTING
        iv_lgnum = ms_defaults-lgnum
        iv_nltyp = ms_wc_replenishment-nltyp
        it_lgpla = VALUE #( FOR lagps IN mt_bins_all ( lagps-lgpla ) )
      IMPORTING
        et_to    = DATA(lt_wts) ).

    READ TABLE mt_bins_all WITH KEY lgpla = ms_wc_replenishment-vlpla_conf TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      CLEAR ms_wc_replenishment.
      ms_wc_replenishment = ls_wc_repl_orig.
      " Open WTs not found
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e043(zmc_workstation) .
    ENDIF.
    " use only the bins from the current bin to the last one
    INSERT LINES OF mt_bins_all FROM sy-tabix TO lines( mt_bins_all ) INTO TABLE lt_lagp_use.

    " Delete the WTs which source bin is not after "start at bin"
    DATA(lr_bins_to_use) = VALUE rseloption( FOR lagp_use IN lt_lagp_use (
                                                 sign   = wmegc_sign_inclusive
                                                 option = wmegc_option_eq
                                                 low    = lagp_use-lgpla ) ).

    DELETE lt_wts WHERE vlpla NOT IN lr_bins_to_use.

    DELETE lt_wts WHERE tanum = ms_cur_wt-tanum.

    " 3010 = Planned Replenishment
    " 3011 = Order-based replenishment
    " Order-based replenishments have higher priority than the min-max replenishments
    SORT lt_wts BY procty DESCENDING vlpla ASCENDING.

    IF lt_wts IS INITIAL.
      CLEAR ms_wc_replenishment.
      ms_wc_replenishment = ls_wc_repl_orig.
      " Open WTs not found
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e043(zmc_workstation) .
    ENDIF.

    ms_cur_wt = lt_wts[ 1 ].

    mo_repl_srv->read_mat_data(
      EXPORTING
        iv_lgnum      = ms_defaults-lgnum
        iv_entitled   = lt_wts[ 1 ]-entitled
        iv_matid      = lt_wts[ 1 ]-matid
        iv_lgtyp      = lt_wts[ 1 ]-vltyp
      IMPORTING
        es_mat_global = DATA(ls_mat_data) ).

    CLEAR: ms_wc_replenishment-product_ean_mpn, ms_wc_replenishment-huident_prop, ms_wc_replenishment-huident_rem.

    ms_wc_replenishment-lgtyp        = lt_wts[ 1 ]-vltyp.
    ms_wc_replenishment-vlpla_conf   = lt_wts[ 1 ]-vlpla.
    ms_wc_replenishment-huident_conf = lt_wts[ 1 ]-vlenr.
    ms_wc_replenishment-matnr_conf   = ls_mat_data-matnr.
    ms_wc_replenishment-maktx_conf   = ls_mat_data-maktx.

  ENDMETHOD.


  METHOD SKIP_WT_ORIGINAL.

    DATA:
          lt_lagp_use    TYPE  /scwm/tt_lagp.

    DATA(ls_wc_repl_orig) = cs_wc_repl.

    mo_repl_srv->get_open_wts(
      EXPORTING
        iv_lgnum = ms_defaults-lgnum
        iv_nltyp = ms_wc_replenishment-nltyp
        it_lgpla = VALUE #( FOR lagps IN mt_bins_all ( lagps-lgpla ) )
      IMPORTING
        et_to    = DATA(lt_wts) ).

    READ TABLE mt_bins_all WITH KEY lgpla = cs_wc_repl-vlpla_conf TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
      CLEAR cs_wc_repl.
      cs_wc_repl = ls_wc_repl_orig.
      " Open WTs not found
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e043(zmc_workstation) .
    ENDIF.
    " use only the bins from the current bin to the last one
    INSERT LINES OF mt_bins_all FROM sy-tabix TO lines( mt_bins_all ) INTO TABLE lt_lagp_use.

    " Delete the WTs which source bin is not after "start at bin"
    DATA(lr_bins_to_use) = VALUE rseloption( FOR lagp_use IN lt_lagp_use (
                                                 sign   = wmegc_sign_inclusive
                                                 option = wmegc_option_eq
                                                 low    = lagp_use-lgpla ) ).

    DELETE lt_wts WHERE vlpla NOT IN lr_bins_to_use.

    DELETE lt_wts WHERE tanum = ms_cur_wt-tanum.

    " 3010 = Planned Replenishment
    " 3011 = Order-based replenishment
    " Order-based replenishments have higher priority than the min-max replenishments
    SORT lt_wts BY procty DESCENDING vlpla ASCENDING.

    IF lt_wts IS INITIAL.
      CLEAR cs_wc_repl.
      cs_wc_repl = ls_wc_repl_orig.
      " Open WTs not found
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e043(zmc_workstation) .
    ENDIF.

    ms_cur_wt = lt_wts[ 1 ].

    mo_repl_srv->read_mat_data(
      EXPORTING
        iv_lgnum      = ms_defaults-lgnum
        iv_entitled   = lt_wts[ 1 ]-entitled
        iv_matid      = lt_wts[ 1 ]-matid
        iv_lgtyp      = lt_wts[ 1 ]-vltyp
      IMPORTING
        es_mat_global = DATA(ls_mat_data) ).

    CLEAR: cs_wc_repl-product_ean_mpn, cs_wc_repl-huident_prop, cs_wc_repl-huident_rem.

    cs_wc_repl-lgtyp        = lt_wts[ 1 ]-vltyp.
    cs_wc_repl-vlpla_conf   = lt_wts[ 1 ]-vlpla.
    cs_wc_repl-huident_conf = lt_wts[ 1 ]-vlenr.
    cs_wc_repl-matnr_conf   = ls_mat_data-matnr.
    cs_wc_repl-maktx_conf   = ls_mat_data-maktx.

    ms_wc_replenishment = cs_wc_repl.

  ENDMETHOD.


  METHOD start.

    CALL FUNCTION 'Z_INT_WC_START_UI'
      EXPORTING
        io_controller = NEW zcl_replenishment_ui( ).

  ENDMETHOD.


  METHOD START_FUNCTION_LOG.
    mo_function_log = NEW /scwm/cl_log( iv_lgnum     = ms_defaults-lgnum
                                        iv_balobj    = zif_wme_c=>gs_msgobj-zewm
                                        iv_balsubobj = zif_wme_c=>gs_msgsubobj-zworkstation ).
  ENDMETHOD.


  METHOD zif_wc_ui_main~check_st_typ.

    IF iv_lgtyp IS INITIAL.
      RETURN.
    ENDIF.

    mo_repl_srv->check_st_typ(
      EXPORTING
        iv_lgnum = ms_defaults-lgnum
        iv_lgtyp = iv_lgtyp ).

  ENDMETHOD.


  METHOD zif_wc_ui_main~dst_pai.

    CLEAR cs_wc_repl.

    zif_wc_ui_main~check_st_typ( is_new_dst-nltyp ).

    ms_wc_chng_dest = is_new_dst.

    cs_wc_repl-nltyp = is_new_dst-nltyp.

    ms_wc_replenishment = cs_wc_repl.

  ENDMETHOD.


  METHOD zif_wc_ui_main~dst_pbo.

    es_wc_chng_dest-lgnum = ms_defaults-lgnum.

  ENDMETHOD.


  METHOD zif_wc_ui_main~get_main_screen_no.

    rv_dynnr = c_main_screen.

  ENDMETHOD.


  METHOD zif_wc_ui_main~get_status.

    ev_status = c_main_status.

  ENDMETHOD.


  METHOD zif_wc_ui_main~get_title.

    ev_title = c_main_tittle.

    " Replenishment Work Center - Warehouse Number &1 ( WS &2 )
    MESSAGE i038(zmc_workstation) WITH ms_defaults-lgnum ms_defaults-workcenter INTO ev_param.

  ENDMETHOD.


  METHOD zif_wc_ui_main~init.

    zif_ws_ui_defaults~get_defaults( ).

    IF zif_ws_ui_defaults~is_defaults_sufficient( ) = abap_false.
      TRY.
          zif_ws_ui_defaults~set_defaults( is_defaults = ms_defaults ).
        CATCH zcx_workstation.
          CLEAR ms_defaults.
          ev_default_needed = abap_true.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_wc_ui_main~mod_repl_pai.

    ms_conf_exc-tanum     = ms_cur_wt-tanum.
    ms_conf_exc-exccode   = c_excode_change_qty. "'DIFH'.
    ms_conf_exc-buscon    = wmegc_buscon_tim. "Confirm Warehouse Task (int. Move)
    ms_conf_exc-exec_step = wmegc_execstep_26. " 26  RF Pick Point

    ms_wc_mod_repl = is_mod_repl.

    cs_wc_repl-repl_qty_prop = is_mod_repl-new_qty.
    cs_wc_repl-repl_uom_prop = is_mod_repl-new_uom.

    ms_wc_replenishment = cs_wc_repl.

    fill_prop_repl(
      EXPORTING
        iv_repl_qty = CONV /scwm/ltap_vsolm( is_mod_repl-new_qty )
        iv_uom      = is_mod_repl-new_uom
      CHANGING
        cs_wc_repl  = cs_wc_repl ).

  ENDMETHOD.


  METHOD zif_wc_ui_main~mod_repl_pbo.

    es_mod_repl-orig_qty = ms_wc_replenishment-repl_qty_prop.
    es_mod_repl-orig_uom = ms_wc_replenishment-repl_uom_pc_prop.
    es_mod_repl-new_uom  = ms_wc_replenishment-repl_uom_pc_prop.

  ENDMETHOD.


  METHOD zif_wc_ui_main~new_pos_pai.

    DATA:
      lt_lagp_others TYPE  /scwm/tt_lagp,
      lt_lagp_use    TYPE  /scwm/tt_lagp.

    DATA(ls_wc_repl_orig) = cs_wc_repl.
    ms_wc_chng_bin = is_new_pos.

    " Check if the Start at bin location corresponds to the Destination storage type
    DATA(lv_not_ok) = mo_repl_srv->check_dst_bin_lptyp( iv_lgnum     = ms_defaults-lgnum
                                                        iv_nltyp     = cs_wc_repl-nltyp
                                                        iv_dst_lgpla = cs_wc_repl-lgpla ).
    IF lv_not_ok = abap_true.
      " Start bin location &1 does not match destination storage type &2
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e059(zmc_workstation) WITH cs_wc_repl-lgpla cs_wc_repl-nltyp.
    ENDIF.

    mo_repl_srv->get_open_wts(
      EXPORTING
        iv_lgnum = ms_defaults-lgnum
        iv_nltyp = ms_wc_replenishment-nltyp
        it_lgpla = VALUE #( ( is_new_pos-lgpla ) )
      IMPORTING
        et_to    = DATA(lt_wt) ).

    IF mt_bins_all IS INITIAL.
      DATA(ls_bin_data) = mo_repl_srv->read_bin_data( iv_lgnum = ms_defaults-lgnum
                                                      iv_lgpla = is_new_pos-lgpla ).

      mo_repl_srv->read_storage_bins(
        EXPORTING
          iv_lgnum = ms_defaults-lgnum
          iv_lgtyp = ls_bin_data-lgtyp
          iv_lptyp = ls_bin_data-lptyp
        IMPORTING
          et_lagp  = lt_lagp_others ).

      SORT lt_lagp_others BY lgpla ASCENDING.
      mt_bins_all = lt_lagp_others.
    ELSE.
      lt_lagp_others = mt_bins_all.
    ENDIF.

    IF lt_wt IS INITIAL.
      READ TABLE lt_lagp_others WITH KEY lgpla = is_new_pos-lgpla TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        " Storage bin &1 is not assigned to storage bin type &2
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e048(zmc_workstation) WITH is_new_pos-lgpla lt_lagp_others[ 1 ]-lptyp.
      ENDIF.

      " use only the bins from "start at bin" to the last one
*      INSERT LINES OF lt_lagp_others FROM sy-tabix TO lines( lt_lagp_others ) INTO TABLE lt_lagp_use.

      "<AAHMEDOV>-23.08.2023

      "delete entered storage bin loc.
      "in order to avoid reading WTs for this storage bin loc. again
      DELETE lt_lagp_others WHERE lgpla = is_new_pos-lgpla.

      "use first bin after the entered bin, which has WTs
      INSERT LINES OF lt_lagp_others INTO TABLE lt_lagp_use.

      " Find all WTs for KDK-* or FBL-* bins
      mo_repl_srv->get_open_wts(
        EXPORTING
          iv_lgnum = ms_defaults-lgnum
          iv_nltyp = ms_wc_replenishment-nltyp
          it_lgpla = VALUE #( FOR ls_lagp_oth IN lt_lagp_others ( ls_lagp_oth-lgpla ) )
        IMPORTING
          et_to    = DATA(lt_wt_oth) ).

*      " Delete the WTs which source bin is not after "start at bin"
*      DATA(lr_bins_to_use) = VALUE rseloption( FOR lagp_use IN lt_lagp_use (
*                                                   sign   = wmegc_sign_inclusive
*                                                   option = wmegc_option_eq
*                                                   low    = lagp_use-lgpla ) ).
*
*      DELETE lt_wt_oth WHERE vlpla NOT IN lr_bins_to_use.
      lt_wt = lt_wt_oth.

      " 3010 = Planned Replenishment
      " 3011 = Order-based replenishment
      " Order-based replenishments have higher priority than the min-max replenishments
      SORT lt_wt BY procty DESCENDING vlpla ASCENDING.

      IF lt_wt IS INITIAL.
        CLEAR cs_wc_repl.
        cs_wc_repl = ls_wc_repl_orig.
        " Open WTs not found
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e043(zmc_workstation) .
      ENDIF.
    ENDIF.

    " Get current bin index
    READ TABLE lt_lagp_others WITH KEY lgpla = lt_wt[ 1 ]-vlpla TRANSPORTING NO FIELDS.
    mv_curr_bin_ind = sy-tabix.

    ms_cur_wt = lt_wt[ 1 ].

    mo_repl_srv->read_mat_data(
      EXPORTING
        iv_lgnum      = ms_defaults-lgnum
        iv_entitled   = lt_wt[ 1 ]-entitled
        iv_matid      = lt_wt[ 1 ]-matid
        iv_lgtyp      = lt_wt[ 1 ]-vltyp
      IMPORTING
        es_mat_global = DATA(ls_mat_data) ).

    CLEAR: cs_wc_repl.

    " Restore Starting Position Section
    cs_wc_repl-lgnum = ls_wc_repl_orig-lgnum.
    cs_wc_repl-nltyp = ls_wc_repl_orig-nltyp.
    cs_wc_repl-lgtyp = ls_wc_repl_orig-lgtyp.
    cs_wc_repl-lgpla = is_new_pos-lgpla.

    cs_wc_repl-vlpla_conf   = lt_wt[ 1 ]-vlpla.
    cs_wc_repl-huident_conf = lt_wt[ 1 ]-vlenr.
    cs_wc_repl-matnr_conf   = ls_mat_data-matnr.
    cs_wc_repl-maktx_conf   = ls_mat_data-maktx.

    ms_wc_replenishment = cs_wc_repl.

  ENDMETHOD.


  METHOD zif_wc_ui_main~new_pos_pbo.

    CLEAR es_new_pos.

    get_wc_st_typ( ).

    es_new_pos-lgnum = ms_defaults-lgnum.
    es_new_pos-lgtyp = mv_st_typ.

  ENDMETHOD.


  METHOD zif_wc_ui_main~pai.

    DATA(ls_wc_repl) = cs_repl_workcenter.

    IF cs_repl_workcenter-nltyp <> ms_wc_replenishment-nltyp.
      " Destination Storage Type changed
      CLEAR: ms_wc_replenishment, cs_repl_workcenter.

      DATA(ls_st_typ) = mo_repl_srv->get_lgtyp( iv_lgnum = ms_defaults-lgnum
                                                iv_lgtyp = ls_wc_repl-nltyp ).

      IF ls_st_typ IS INITIAL.
        " Storage type &1 does not exist in warehouse number &2
        RAISE EXCEPTION TYPE zcx_workstation MESSAGE e007(/scwm/condtech_basic) WITH cs_repl_workcenter-nltyp ms_defaults-lgnum.
      ENDIF.

      cs_repl_workcenter-nltyp = ls_wc_repl-nltyp.

      ms_wc_chng_dest-nltyp = cs_repl_workcenter-nltyp. " !!!
      ms_wc_chng_dest-lgnum = ms_defaults-lgnum.
      ms_wc_replenishment = cs_repl_workcenter.
      mo_ptwy_cart_sp = zcl_ws_ptwy_cart_sp=>create_instance_repl(
                          iv_lgnum      = ms_defaults-lgnum
                          iv_workcenter = ms_defaults-workcenter
                        ).
    ENDIF.

    IF cs_repl_workcenter-lgpla <> ms_wc_replenishment-lgpla.
      zif_wc_ui_main~new_pos_pai(
        EXPORTING
          is_new_pos = VALUE #( lgnum = ms_defaults-lgnum
                                lgpla = cs_repl_workcenter-lgpla )
        CHANGING
          cs_wc_repl = cs_repl_workcenter ).

    ENDIF.

    IF cs_repl_workcenter-product_ean_mpn <> ms_wc_replenishment-product_ean_mpn.
      product_changed(
        EXPORTING
          iv_prod_ean_mpn = cs_repl_workcenter-product_ean_mpn
        CHANGING
          cs_wc_repl = cs_repl_workcenter ).
    ENDIF.

    IF cs_repl_workcenter-huident_rem <> ms_wc_replenishment-huident_rem.
      return_rem_qty( iv_huident = cs_repl_workcenter-huident_rem ).
    ENDIF.

    IF cs_repl_workcenter-huident_prop <> ms_wc_replenishment-huident_prop.
      dest_hu_changed( iv_huident = cs_repl_workcenter-huident_prop ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_wc_ui_main~pbo.
    es_wc_repl = ms_wc_replenishment.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'LGP'.
          IF ms_wc_replenishment-nltyp IS INITIAL.
            screen-input = '0'.
          ENDIF.

        WHEN 'CNF'.
          IF ms_wc_replenishment-lgpla IS INITIAL.
            screen-input = '0'.
          ENDIF.

        WHEN 'PRO'.
          IF ms_wc_replenishment-product_ean_mpn IS INITIAL.
            screen-input = '0'.
          ENDIF.

        WHEN 'REM'.
          IF ms_wc_replenishment-repl_qty IS INITIAL.
            screen-input = '0'.
          ENDIF.

        WHEN 'TOT'.
          IF mv_show_tote = abap_true.
            screen-invisible = '0'.
          ELSE.
            screen-invisible = '1'.
          ENDIF.

*        WHEN 'MCT'.
*          IF mv_hide_mc = abap_true.
*            screen-invisible = '1'.
*          ENDIF.

      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

    IF ms_wc_replenishment-nltyp IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WC_REPLENISHMENT-NLTYP'.
    ELSEIF ms_wc_replenishment-lgpla IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WC_REPLENISHMENT-LGPLA'.
    ELSEIF ms_wc_replenishment-product_ean_mpn IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WC_REPLENISHMENT-PRODUCT_EAN_MPN'.
    ELSEIF ms_wc_replenishment-huident_prop IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WC_REPLENISHMENT-HUIDENT_PROP'.
    ELSEIF ms_wc_replenishment-huident_rem IS INITIAL.
      SET CURSOR FIELD 'ZSTR_WC_REPLENISHMENT-HUIDENT_REM'.
    ENDIF.

    get_wc_st_typ( ).

    ms_wc_replenishment-lgnum = ms_defaults-lgnum.
    ms_wc_replenishment-lgtyp = mv_st_typ. " Used for field f4 help

    es_wc_repl = ms_wc_replenishment.

  ENDMETHOD.


  METHOD zif_wc_ui_main~process_user_command.

    TRY.
        CASE iv_ucomm.

          WHEN c_func_leave.
            LEAVE TO SCREEN 0.

          WHEN c_func_defaults.
            zif_ws_ui_defaults~call_screen( ).

          WHEN c_func_fx_chng_rpl_dest.
            CALL FUNCTION 'Z_INT_WC_CHNG_REPL_DEST'.

          WHEN c_func_fx_chng_pos.
            CALL FUNCTION 'Z_INT_WC_NEW_STR_POS'.

          WHEN c_func_fx_skip_wt.
            skip_wt( ).

          WHEN c_func_fx_modify.
            CALL FUNCTION 'Z_INT_WC_MOD_REPL'.

          WHEN c_func_fx_pack_n_print.
            pack_n_print( ).
        ENDCASE.

      CATCH zcx_workstation INTO DATA(lx_ws).
          MESSAGE lx_ws TYPE /scwm/cl_log=>msgty_info DISPLAY LIKE /scwm/cl_log=>msgty_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_wc_ui_main~with_tote_place.
    rv_with_tote_place = zcl_switch=>get_switch_state( iv_lgnum = ms_defaults-lgnum
                                                       iv_devid = zif_switch_const=>c_zint_004 ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_status.
    zif_wc_ui_main~get_status(
      IMPORTING
        ev_status   = ev_status
        et_excludes = et_excludes                 " Table of Strings
    ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~get_title.
    zif_wc_ui_main~get_title(
      IMPORTING
        ev_title = ev_title                 " Title Line
        ev_param = ev_param
    ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~init.
    zif_wc_ui_main~init(
      IMPORTING
        ev_default_needed = ev_default_needed                  " Workcenter defaults
    ).
    ev_subscreen_no = c_sub_screen.
    ev_tab = c_tab_repl_ui.
    ev_subscreen_prg = c_subrscreen_repid.
  ENDMETHOD.


  method ZIF_WS_SUBSCR_UI~LEAVE_SCREEN.
   rv_leave = abap_true.
  endmethod.


  METHOD zif_ws_subscr_ui~pai_tab.
    DATA: ls_repl_workcenter TYPE zstr_wc_replenishment.

    ls_repl_workcenter = is_screen_data.
    zif_wc_ui_main~pai(
      CHANGING
        cs_repl_workcenter = ls_repl_workcenter                 " Replenishment workstation
    ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~pbo_tab.
    zif_wc_ui_main~pbo(
      IMPORTING
        es_wc_repl = es_screen_data                 " Replenishment workstation
    ).
  ENDMETHOD.


  METHOD zif_ws_subscr_ui~process_user_command.
    zif_wc_ui_main~process_user_command(
      EXPORTING
        iv_ucomm   = iv_ucomm                 " Function Code
      IMPORTING
        es_bapiret = es_bapiret                 " Return Parameter
    ).
  ENDMETHOD.
ENDCLASS.
