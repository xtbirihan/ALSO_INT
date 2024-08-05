FUNCTION zint_partialrepl_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(SELECTION) TYPE  /SCWM/S_RF_SELECTION
*"     REFERENCE(RESOURCE) TYPE  /SCWM/S_RSRC
*"     REFERENCE(WHO) TYPE  /SCWM/S_WHO_INT
*"     REFERENCE(ORDIM_CONFIRM) TYPE  /SCWM/S_RF_ORDIM_CONFIRM
*"     REFERENCE(NESTED_HU) TYPE  /SCWM/S_RF_NESTED_HU
*"     REFERENCE(TT_ORDIM_CONFIRM) TYPE  /SCWM/TT_RF_ORDIM_CONFIRM
*"     REFERENCE(TT_NESTED_HU) TYPE  /SCWM/TT_RF_NESTED_HU
*"     REFERENCE(T_RF_PICK_HUS) TYPE  /SCWM/TT_RF_PICK_HUS
*"     REFERENCE(WME_VERIF) TYPE  /SCWM/S_WME_VERIF
*"     REFERENCE(ZCS_PARTIAL_REPL_HU) TYPE  ZSTR_RF_PARTIAL_REPL_HU
*"----------------------------------------------------------------------

*  DATA: bapiret     TYPE bapirettab,
*        severity    TYPE bapi_mtype,
*        tanum       TYPE /scwm/tanum,
*        ltap_vb     TYPE /scwm/tt_ltap_vb,
*        create      TYPE /scwm/s_to_create_int,
*        create_hu   TYPE /scwm/s_to_crea_hu,
*        lv_work_who TYPE /scwm/de_who,
*        quantity    TYPE /scwm/de_quantity,
*        error       TYPE bapiret2.
*
*  DATA(line) = /scwm/cl_rf_bll_srvc=>get_line( ).
*
*  DATA(lcl_controller_piplhu) = NEW lcl_controller_piplhu( ordim_confirm-lgnum ).
*
*  CASE /scwm/cl_rf_bll_srvc=>get_fcode( ).
*    WHEN gc_fcode_zpartq.
*      /scwm/cl_rf_bll_srvc=>set_screlm_input_on( gc_scr_elmnt_nista_vrf ).
*      /scwm/cl_rf_bll_srvc=>set_field( CONV #( gc_scr_elmnt_nista_vrf ) ).
*
*      IF ordim_confirm-meins = 'KG'.
*
*        zcl_int_product=>get_instance( )->det_prod_ext(
*          EXPORTING
*            iv_matid    = ordim_confirm-matid
*            iv_lgnum    = ordim_confirm-lgnum
*            iv_entitled = ordim_confirm-entitled
*          RECEIVING
*            es_prod_ext = DATA(ls_prod_ext) ).
*
*        IF lcl_controller_piplhu->is_uom_defined_for_product( iv_matid = ordim_confirm-matid
*                                                              iv_uom   = ls_prod_ext-zewm_auom_rd ).
*
*          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
*            EXPORTING
*              iv_matid     = ordim_confirm-matid
*              iv_quan      = ordim_confirm-vsola
*              iv_unit_from = ordim_confirm-altme
*              iv_unit_to   = ls_prod_ext-zewm_auom_rd
*              iv_batchid   = VALUE /scwm/de_batchid( )
*            IMPORTING
*              ev_quan      = quantity.
*
*          quantity                      = round( val = quantity dec = 0 mode = cl_abap_math=>round_down ).
*          ordim_confirm-vsola_chr       = quantity.
*          ordim_confirm-nista_verif     = ordim_confirm-vsola_chr.
*          zcs_partial_repl_hu-nista_uom = ls_prod_ext-zewm_auom_rd.
*          zcs_partial_repl_hu-vsola_uom = ls_prod_ext-zewm_auom_rd.
*          MODIFY tt_ordim_confirm FROM ordim_confirm INDEX COND #( WHEN line = 0 THEN 1 ELSE line ).
*
*        ELSE.
*          MESSAGE w117(zewm_ret_stag) WITH ls_prod_ext-zewm_auom_rd ordim_confirm-matnr INTO DATA(msg).
*          /scwm/cl_rf_bll_srvc=>message( iv_msg_view = /scwm/cl_rf_bll_srvc=>c_msg_popup ).
*        ENDIF.
*
*      ENDIF.
*
*    WHEN fcode_enterf.
*
*      lcl_controller_piplhu->do_validate_input(
*         CHANGING
*           cs_ordim_confirm   = ordim_confirm
*           ct_ordim_confirm   = tt_ordim_confirm
*           cs_partial_repl_hu = zcs_partial_repl_hu ).
*
*      create = CORRESPONDING /scwm/s_to_create_int( ordim_confirm ).
*
*      IF zcs_partial_repl_hu-partial_qty = abap_true.
*
*        DATA(huident) = ordim_confirm-vlenr.
*        DATA(lgnum)   = ordim_confirm-lgnum.
*
*        CALL FUNCTION 'ZINT_TO_CANCEL'
*          EXPORTING
*            iv_lgnum    = ordim_confirm-lgnum
*            it_cancl    = VALUE /scwm/tt_cancl( ( tanum = ordim_confirm-tanum ) )
*          IMPORTING
*            et_bapiret  = bapiret
*            ev_severity = severity.
*        IF severity CA wmegc_severity_ea.
*          error = VALUE #( bapiret[ type = severity ] OPTIONAL ).
*          MESSAGE ID error-id TYPE severity NUMBER error-number
*                  WITH error-message_v1 error-message_v2 error-message_v3 error-message_v4.
*        ENDIF.
*
*        DATA(configuration) = lcl_controller_piplhu->read_configuration( ).
*        create              = CORRESPONDING /scwm/s_to_create_int( ordim_confirm ).
*        create-procty       = configuration-procty_part_repl.
*        CLEAR: create-letyp, create-nlenr, create-dguid_hu.
*
*        IF zcs_partial_repl_hu-nista_uom <> ordim_confirm-altme.
*
*          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
*            EXPORTING
*              iv_matid     = ordim_confirm-matid
*              iv_quan      = ordim_confirm-nista
*              iv_unit_from = zcs_partial_repl_hu-nista_uom
*              iv_unit_to   = ordim_confirm-meins
*              iv_batchid   = VALUE /scwm/de_batchid( )
*            IMPORTING
*              ev_quan      = quantity.
*
*          create-anfme = ordim_confirm-nista = quantity.
*          create-altme = ordim_confirm-meins.
*
*        ELSE.
*          create-anfme = ordim_confirm-nista.
*        ENDIF.
*
*        CALL FUNCTION '/SCWM/TO_CREATE'
*          EXPORTING
*            iv_lgnum       = ordim_confirm-lgnum
*            iv_commit_work = abap_false
*            it_create      = VALUE /scwm/tt_to_create_int( ( create ) )
*          IMPORTING
*            ev_tanum       = tanum
*            et_ltap_vb     = ltap_vb
*            et_bapiret     = bapiret
*            ev_severity    = severity.
*        IF severity CA wmegc_severity_ea.
*          ROLLBACK WORK.
*          error = VALUE #( bapiret[ type = severity ] OPTIONAL ).
*          MESSAGE ID error-id TYPE severity NUMBER error-number
*                  WITH error-message_v1 error-message_v2 error-message_v3 error-message_v4.
*        ELSE.
*          COMMIT WORK AND WAIT.
*          /scwm/cl_tm=>cleanup( ).
*        ENDIF.
*
*        MOVE-CORRESPONDING ltap_vb[ 1 ] TO ordim_confirm.
*        CLEAR: ordim_confirm-nlenr_o, ordim_confirm-pickhu_verif, ordim_confirm-pickhu.
*        MODIFY tt_ordim_confirm FROM ordim_confirm INDEX COND #( WHEN line = 0 THEN 1 ELSE line ).
*
*      ENDIF.
*
*      CALL FUNCTION '/SCWM/RF_PICK_PIPLHU_PAI'
*        CHANGING
*          selection        = selection
*          resource         = resource
*          who              = who
*          ordim_confirm    = ordim_confirm
*          nested_hu        = nested_hu
*          tt_ordim_confirm = tt_ordim_confirm
*          tt_nested_hu     = tt_nested_hu
*          t_rf_pick_hus    = t_rf_pick_hus
*          wme_verif        = wme_verif.
*
*      IF zcs_partial_repl_hu-partial_qty = abap_true.
*
*        lcl_controller_piplhu->do_putaway_remaining_repl_qty(
*          EXPORTING
*            iv_huident  = huident
*            iv_procty   = configuration-procty_ptwy_part_repl
*            iv_queue    = who-queue
*            iv_resource = resource-rsrc ).
*
*      ENDIF.
*
**      IF /scwm/cl_rf_bll_srvc=>get_last_ltrans( ) = zewm_if_returns_hu_adhoc_srv=>gc_ltrans_zrethu.
**        /scwm/cl_rf_bll_srvc=>set_fcode( /scwm/cl_rf_bll_srvc=>c_fcode_leave ).
**      ENDIF.
*
*    WHEN /scwm/cl_rf_bll_srvc=>c_fcode_enter.
*
*      IF /scwm/cl_rf_bll_srvc=>get_field( ) = gc_scr_elmnt_nista_vrf.
*
*        lcl_controller_piplhu->do_validate_input(
*           CHANGING
*             cs_ordim_confirm   = ordim_confirm
*             ct_ordim_confirm   = tt_ordim_confirm
*             cs_partial_repl_hu = zcs_partial_repl_hu ).
*
*      ELSE.
**        IF sy-uname = 'SAP_XYZ' OR sy-uname = 'SAP_XYZ'.
**          ASSIGN tt_ordim_confirm[ line ] TO FIELD-SYMBOL(<ls_ordim_confirm>).
**          IF sy-subrc = 0 AND <ls_ordim_confirm>-pickhu_verif IS NOT INITIAL.
**            CALL FUNCTION 'CONVERSION_EXIT_HUID_INPUT'
**              EXPORTING
**                input  = <ls_ordim_confirm>-pickhu_verif
**              IMPORTING
**                output = <ls_ordim_confirm>-pickhu_verif.
**          ENDIF.
**        ENDIF.
*
*        CALL FUNCTION '/SCWM/RF_PICK_HU_CLOSE_CHECK'
*          CHANGING
*            selection        = selection
*            resource         = resource
*            who              = who
*            ordim_confirm    = ordim_confirm
*            tt_ordim_confirm = tt_ordim_confirm
*            tt_nested_hu     = tt_nested_hu
*            t_rf_pick_hus    = t_rf_pick_hus.
*
*      ENDIF.
*
*  ENDCASE.





ENDFUNCTION.
