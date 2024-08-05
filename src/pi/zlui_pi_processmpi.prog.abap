**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include          /SCWM/LUI_PI_PROCESSMPI
*&---------------------------------------------------------------------*
CLASS lcl_sp_mapper IMPLEMENTATION.
*----------------------------------------------------------------------*
* METHOD constructor                                           - public
*----------------------------------------------------------------------*
  METHOD constructor.

    mo_stock_fields = lcl_sp=>so_sp_fw->mo_stock_fields.

  ENDMETHOD.                    "constructor
*----------------------------------------------------------------------*
* METHOD convert_get_loc_2_oi_pi_cr                            - public
*----------------------------------------------------------------------*
*   IMPORTING it_item                TYPE /scwm/t_pi_location_for_area
*             iv_reference_ui        TYPE /SCWM/PI_DE_REFERENCE_UI
*             iv_count_time_ui       TYPE /scwm/pi_de_count_time
*   EXPORTING et_asp_oi_pi_cr        TYPE /scwm/t_asp_oi_pi_item_create
*----------------------------------------------------------------------*
  METHOD convert_get_loc_2_oi_pi_cr.

    DATA:
      lo_item         TYPE REF TO /scwm/s_pi_location_for_area,
      ls_asp_oi_pi_cr TYPE /scwm/s_asp_oi_pi_item_create,
      ls_aspd_pi      TYPE /scwm/s_aspd_pi.

    CLEAR et_asp_oi_pi_cr.
    /scmb/cl_base=>get_default_values(
      IMPORTING
        es_values = ls_aspd_pi ).

    LOOP AT it_item REFERENCE INTO lo_item.
      MOVE-CORRESPONDING lo_item->* TO ls_asp_oi_pi_cr.     "#EC ENHOK
      IF ls_aspd_pi-status_select = abap_true.
        ls_asp_oi_pi_cr-status = lcl_sp=>sv_icon_led_green.
      ELSE.
        ls_asp_oi_pi_cr-status = lcl_sp=>sv_icon_led_inactive.
      ENDIF.
*     Set block indicator
      IF ls_aspd_pi-status_blocked = abap_true.
        ls_asp_oi_pi_cr-block_ind = abap_true.
      ENDIF.
*     Set freeze indicator
      IF ls_aspd_pi-status_freeze = abap_true.
        ls_asp_oi_pi_cr-freeze_ind = abap_true.
      ENDIF.
      ls_asp_oi_pi_cr-reference_ui  = iv_reference_ui.
      ls_asp_oi_pi_cr-count_time_ui = iv_count_time_ui.
      APPEND ls_asp_oi_pi_cr TO et_asp_oi_pi_cr.
    ENDLOOP.
    SORT et_asp_oi_pi_cr BY lgnum
                            lgtyp
                            lgber
                            lgpla
                            matnr
                            charg
                            cat
                            entitled
                            owner
                            stock_usage
                            stref_doccat.

  ENDMETHOD.                    "convert_get_loc_2_oi_pi_cr
*----------------------------------------------------------------------*
* METHOD convert_get_stock_2_oi_pi_cr                          - public
*----------------------------------------------------------------------*
*   IMPORTING it_item                TYPE /scwm/t_pi_stock_for_area
*             iv_reference_ui        TYPE /SCWM/PI_DE_REFERENCE_UI
*             iv_count_time_ui       TYPE /scwm/pi_de_count_time
*   EXPORTING et_asp_oi_pi_cr        TYPE /scwm/t_asp_oi_pi_item_create
*----------------------------------------------------------------------*
  METHOD convert_get_stock_2_oi_pi_cr.

    DATA:
      lo_item         TYPE REF TO /scwm/s_pi_stock_for_area,
      ls_asp_oi_pi_cr TYPE /scwm/s_asp_oi_pi_item_create,
      ls_aspd_pi      TYPE /scwm/s_aspd_pi,
      lt_t331         TYPE /scwm/tt_t331.

    BREAK-POINT ID zcg_ui_pi_create.

    CLEAR et_asp_oi_pi_cr.
    /scmb/cl_base=>get_default_values(
      IMPORTING
        es_values = ls_aspd_pi ).

    ASSIGN it_item[ 1 ]-lgnum TO FIELD-SYMBOL(<fs_lgnum>).

    IF <fs_lgnum> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    " get mixed storage type for storage type
    " get HU numbers for storage type 3(wmegc_mixbin_one_hu)
    TRY.
        CALL FUNCTION '/SCWM/T331_READ_MULTI'
          EXPORTING
            iv_lgnum = <fs_lgnum>
            it_lgtyp = VALUE rseloption( FOR GROUPS OF <ls_item> IN it_item
                                   GROUP BY <ls_item>-lgtyp
                                    ( sign = wmegc_sign_inclusive
                                      option = wmegc_option_eq
                                      low = <ls_item>-lgtyp ) )
          IMPORTING
            et_t331  = lt_t331.

      CATCH /scwm/cx_core.
    ENDTRY.

    DELETE lt_t331 WHERE mixbin <> wmegc_mixbin_one_hu.

    DATA(lt_t331_r) = VALUE rseloption( FOR GROUPS OF <ls_t331> IN lt_t331
                                  GROUP BY <ls_t331>-lgtyp
                                ( sign = wmegc_sign_inclusive
                                 option = wmegc_option_eq
                                 low = <ls_t331>-lgtyp ) ).

    LOOP AT it_item REFERENCE INTO lo_item.
      MOVE-CORRESPONDING lo_item->* TO ls_asp_oi_pi_cr.     "#EC ENHOK
      IF ls_aspd_pi-status_select = abap_true.
        ls_asp_oi_pi_cr-status = lcl_sp=>sv_icon_led_green.
      ELSE.
        ls_asp_oi_pi_cr-status = lcl_sp=>sv_icon_led_inactive.
      ENDIF.
*     Set block indicator
      IF ls_aspd_pi-status_blocked = abap_true.
        ls_asp_oi_pi_cr-block_ind = abap_true.
      ENDIF.
*     Set freeze indicator
      IF ls_aspd_pi-status_freeze = abap_true.
        ls_asp_oi_pi_cr-freeze_ind = abap_true.
      ENDIF.
      ls_asp_oi_pi_cr-reference_ui  = iv_reference_ui.
      ls_asp_oi_pi_cr-count_time_ui = iv_count_time_ui.

      ls_asp_oi_pi_cr-zz_mfrnr = VALUE #( it_mara[ matid = lo_item->matid ]-mfrnr OPTIONAL ).
      ls_asp_oi_pi_cr-zz_mfrpn = VALUE #( it_mara[ matid = lo_item->matid ]-mfrpn OPTIONAL ).

      IF lines( lt_t331_r ) > 0
        AND lo_item->lgtyp IN lt_t331_r.
        ls_asp_oi_pi_cr-zz_huident = VALUE #( it_huhdr[ guid_loc = lo_item->guid_loc ]-huident OPTIONAL ).
      ENDIF.

      APPEND ls_asp_oi_pi_cr TO et_asp_oi_pi_cr.

    ENDLOOP.
    SORT et_asp_oi_pi_cr BY lgnum
                            lgtyp
                            lgber
                            lgpla
                            matnr
                            charg
                            cat
                            entitled
                            owner
                            stock_usage
                            stref_doccat.

  ENDMETHOD.                    "convert_get_stock_2_oi_pi_cr
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_oi_pi_pr                          - public
*----------------------------------------------------------------------*
*   IMPORTING it_item                TYPE /lime/pi_t_item_read_getsingle,
*   EXPORTING et_asp_oi_pi_pr        TYPE /scwm/t_asp_oi_pi_item_proces
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_oi_pi_pr.

    DATA:
      lo_items        TYPE REF TO /lime/pi_item_read_get_single,
      ls_asp_oi_pi_pr TYPE /scwm/s_asp_oi_pi_item_proces,
      lv_pi_area      TYPE /lime/pi_de_pi_aread,
      ls_lagp         TYPE /scwm/lagp,
      ls_logitem      TYPE /lime/pi_doc_logitem,
      ls_qdocid       TYPE /scwm/s_qdocid,
      lv_count_date   TYPE /lime/pi_count_date,
      lv_exc_pril     TYPE /lime/ref_doc_id,
      ls_guid_loc     TYPE LINE OF /scwm/tt_guid_loc,
      ls_batchid      TYPE /scwm/dlv_batchid_str,
      ls_matid        TYPE LINE OF /scmb/mdl_matid_tab,
      lt_batchid      TYPE /scwm/dlv_batchid_tab,
      lt_matid        TYPE /scmb/mdl_matid_tab,
      lt_guid_loc     TYPE /scwm/tt_guid_loc.

    FIELD-SYMBOLS:
      <ls_asp_oi_pi_pr> TYPE /scwm/s_asp_oi_pi_item_proces.

    BREAK-POINT ID zcg_ui_pi_process.

    CLEAR et_asp_oi_pi_pr.
    "buffer data
    LOOP AT it_item REFERENCE INTO lo_items.
      IF lo_items->data-type_parent = wmegc_lime_type_loc.
        ls_guid_loc-guid_loc = lo_items->data-guid_parent.
        APPEND ls_guid_loc  TO lt_guid_loc.
      ENDIF.
      IF lo_items->data-stock_item-matid IS NOT INITIAL.
        ls_batchid-batchid = lo_items->data-stock_item-matid.
        COLLECT ls_batchid INTO lt_batchid.
      ENDIF.
      IF lo_items->data-stock_item-batchid IS NOT INITIAL.
        ls_matid-matid   = lo_items->data-stock_item-batchid.
        APPEND ls_matid TO lt_matid.
      ENDIF.
    ENDLOOP.
    SORT lt_guid_loc.
    DELETE ADJACENT DUPLICATES FROM lt_guid_loc.
    CALL FUNCTION '/SCWM/LAGP_READ_MULTI'
      EXPORTING
        it_guid_loc   = lt_guid_loc
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        enqueue_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF lt_matid IS NOT INITIAL.
      SORT lt_matid.
      DELETE ADJACENT DUPLICATES FROM lt_matid.
      mo_stock_fields->prefetch_matkey_by_id( lt_matid ).
    ENDIF.
    IF lt_batchid IS NOT INITIAL.
      mo_stock_fields->prefetch_batchno_by_id( lt_batchid ).
    ENDIF.
    FREE: lt_guid_loc, lt_batchid, lt_matid.

    LOOP AT it_item REFERENCE INTO lo_items.

      CLEAR: ls_asp_oi_pi_pr.
      MOVE-CORRESPONDING lo_items->data TO ls_asp_oi_pi_pr. "#EC ENHOK
*     Get activity area: PI area GUID -> PI area -> aarea
      CALL METHOD lcl_sp=>so_sp_fw->mo_lime_cust->get_pi_aread
        EXPORTING
          i_pi_area  = lo_items->data-pi_area
        IMPORTING
          e_pi_aread = lv_pi_area.

      TRY.
          lcl_sp=>so_sp_fw->mo_scwm_cust->get_aarea_to_pi_area(
            EXPORTING
              iv_pi_area = lv_pi_area
            IMPORTING
              ev_aarea   = ls_asp_oi_pi_pr-pi_area_ui ).
        CATCH /scwm/cx_pi_app .                         "#EC NO_HANDLER
      ENDTRY.
*---- Stock fields
      IF lcl_sp=>so_sp_fw->mo_lime_cust->get_pi_doc_cat(
                                ls_asp_oi_pi_pr-doc_type ) = limpi_object.

        MOVE-CORRESPONDING lo_items->data-stock_item
                               TO ls_asp_oi_pi_pr.          "#EC ENHOK
        ls_asp_oi_pi_pr-stref_doccat
                               = lo_items->data-stock_item-doccat.
*       Enrich stock data
        mo_stock_fields->get_matkey_by_id(
          EXPORTING
            iv_matid = lo_items->data-stock_item-matid
          IMPORTING
            ev_matnr = ls_asp_oi_pi_pr-matnr
            ev_maktx = ls_asp_oi_pi_pr-maktx ).
        ls_asp_oi_pi_pr-charg = mo_stock_fields->get_batchno_by_id(
          ls_asp_oi_pi_pr-batchid ).
*        TRY.->removed because of performance
**           Get batch status
*            CALL FUNCTION '/SCWM/BATCH_GET'
*              EXPORTING
*                iv_lgnum    = lo_items->data-lgnum
*                iv_entitled = lo_items->data-stock_item-entitled
*                iv_matid    = lo_items->data-stock_item-matid
*                iv_batchid  = ls_asp_oi_pi_pr-batchid
*              IMPORTING
*                ev_brestr   = ls_asp_oi_pi_pr-brestr.
*          CATCH /scwm/cx_core.                          "#EC NO_HANDLER
*        ENDTRY.

        ls_asp_oi_pi_pr-cat_text = mo_stock_fields->get_cat_text(
          EXPORTING
            iv_lgnum = lo_items->data-lgnum
            iv_cat   = ls_asp_oi_pi_pr-cat ).

        ls_asp_oi_pi_pr-owner_text = mo_stock_fields->get_partner_text_by_no(
          ls_asp_oi_pi_pr-owner ).

        ls_asp_oi_pi_pr-entitled_text =
          mo_stock_fields->get_partner_text_by_no(
          ls_asp_oi_pi_pr-entitled ).

        ls_asp_oi_pi_pr-stock_usage_text =
          mo_stock_fields->get_stock_usage_text(
          ls_asp_oi_pi_pr-stock_usage ).
        ls_asp_oi_pi_pr-stock_doccat_text =
          lcl_sp=>so_sp_fw->mo_stock_fields->get_stock_doccat_text(
          ls_asp_oi_pi_pr-stock_doccat ).
        ls_asp_oi_pi_pr-stock_docno_ext =
          lcl_sp=>so_sp_fw->mo_stock_fields->get_stock_docno_ext(
          iv_stock_doccat = ls_asp_oi_pi_pr-stock_doccat
          iv_stock_docno  = ls_asp_oi_pi_pr-stock_docno ).
        ls_asp_oi_pi_pr-stref_doccat_text =
          lcl_sp=>so_sp_fw->mo_stock_fields->get_stref_doccat_text(
          ls_asp_oi_pi_pr-stref_doccat ).

        ls_asp_oi_pi_pr-wdatu          = lo_items->data-wdatu.
        CALL METHOD mo_stock_fields->get_ui_wdatu
          EXPORTING
            iv_lgnum    = lo_items->data-lgnum
            iv_wdatu    = lo_items->data-wdatu
          IMPORTING
            ev_ui_wdatu = ls_asp_oi_pi_pr-ui_wdatu
            ev_ui_wdatt = ls_asp_oi_pi_pr-ui_wdatt.
        ls_asp_oi_pi_pr-vfdat          = lo_items->data-vfdat.
        ls_asp_oi_pi_pr-coo            = lo_items->data-coo.
        ls_asp_oi_pi_pr-coo_text =
          mo_stock_fields->get_coo_text( lo_items->data-coo ).
        ls_asp_oi_pi_pr-idplate        = lo_items->data-idplate.
        ls_asp_oi_pi_pr-qdoccat        = lo_items->data-qdoccat.
        ls_asp_oi_pi_pr-qdoccat_text =
          mo_stock_fields->get_qdoccat_text( lo_items->data-qdoccat ).
        ls_asp_oi_pi_pr-qdocid         = lo_items->data-qdocid.
        ls_asp_oi_pi_pr-qitmid         = lo_items->data-qitmid.
        CALL METHOD mo_stock_fields->get_qdocno_by_id
          EXPORTING
            iv_qdoccat = lo_items->data-qdoccat
            iv_qdocid  = lo_items->data-qdocid
            iv_qitmid  = lo_items->data-qitmid
            iv_lgnum   = lo_items->data-lgnum
          IMPORTING
            ev_qdocno  = ls_asp_oi_pi_pr-qdocno
            ev_qitemno = ls_asp_oi_pi_pr-qitemno.
        ls_asp_oi_pi_pr-insptyp        = lo_items->data-insptyp.
        IF NOT ls_asp_oi_pi_pr-insptyp IS INITIAL.
          ls_asp_oi_pi_pr-insptyp_text =
            mo_stock_fields->get_insptyp_text( lo_items->data-insptyp ).
          ls_asp_oi_pi_pr-inspid         = lo_items->data-inspid.
**EHP2
          CLEAR ls_qdocid.
          ls_qdocid-qdoccat = lo_items->data-qdoccat.
          ls_qdocid-qdocid  = lo_items->data-qdoccat.
          ls_qdocid-qitmid  = lo_items->data-qitmid.

          CALL METHOD mo_stock_fields->get_inspdocno_by_id
            EXPORTING
              iv_insptyp   = lo_items->data-insptyp
              iv_inspid    = lo_items->data-inspid
              is_qdocid    = ls_qdocid
            IMPORTING
              ev_inspdocno = ls_asp_oi_pi_pr-inspdocno.
        ENDIF.
      ENDIF.
*---- Location fields
      ls_asp_oi_pi_pr-pi_lgnum = lo_items->data-lgnum.
      ls_asp_oi_pi_pr-loc_type = wmegc_bin.
      ls_asp_oi_pi_pr-lgpla    = lo_items->data-loc_parent-lgpla.
      ls_asp_oi_pi_pr-lgtyp    = lo_items->data-loc_parent-lgtyp.
      ls_asp_oi_pi_pr-lgnum    = lo_items->data-loc_parent-lgnum.
*     Storage bin, fill storage area
      CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
        EXPORTING
          iv_lgnum    = lo_items->data-loc_parent-lgnum
          iv_lgpla    = lo_items->data-loc_parent-lgpla
        IMPORTING
          es_lagp     = ls_lagp
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.

      IF sy-subrc = 0.
        ls_asp_oi_pi_pr-lgber = ls_lagp-lgber.
      ENDIF.
*---- Default for mode is display
      ls_asp_oi_pi_pr-pr_mode = lcl_sp=>sv_icon_display.
      ls_asp_oi_pi_pr-function_mode =
        lcl_sp=>so_sp_fw->mo_sp_services->get_next_function_mode(
        iv_doc_status = ls_asp_oi_pi_pr-doc_status ).
*     Text data
      ls_asp_oi_pi_pr-doc_type_txt =
        lcl_sp=>so_sp_fw->mo_lime_cust->get_pi_doc_type_txt(
        ls_asp_oi_pi_pr-doc_type ).
      ls_asp_oi_pi_pr-function_mode_txt =
        lcl_sp=>so_sp_fw->mo_sp_services->get_func_mode_text(
        ls_asp_oi_pi_pr-function_mode ).
      ls_asp_oi_pi_pr-doc_status_txt =
        lcl_sp=>so_sp_fw->mo_sp_services->get_doc_status_text(
        ls_asp_oi_pi_pr-doc_status ).
      TRY.
          ls_asp_oi_pi_pr-reason_txt =
            lcl_sp=>so_sp_fw->mo_scwm_cust->get_reason_txt(
            i_lgnum  = ls_asp_oi_pi_pr-lgnum
            i_reason = ls_asp_oi_pi_pr-reason ).
        CATCH /scwm/cx_pi_app.                          "#EC NO_HANDLER
      ENDTRY.
      CLEAR:
        ls_logitem,
        lv_count_date,
        lv_exc_pril.
      IF lo_items->data-doc_status = limpi_doc_del.
*       In case of PRIL the document status is 'deleted' and
*       the PRIL user and PRIL timestamp is part of the reference table
        CONCATENATE ls_asp_oi_pi_pr-lgnum
                    wmegc_iprcode_pril
               INTO lv_exc_pril.
        READ TABLE lo_items->t_logitem
             WITH KEY log_type     = limpi_log_del
                      ref_doc_type = wmegc_ref_inte
                      ref_doc_id   = lv_exc_pril
             TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
*         Item with exception PRIL
          ls_asp_oi_pi_pr-ind_pril = abap_true.
*         Get PRIL user
          READ TABLE lo_items->t_logitem INTO ls_logitem
            WITH KEY log_type     = limpi_log_del
                     ref_doc_type = wmegc_ref_plus.
          IF sy-subrc = 0.
            ls_asp_oi_pi_pr-count_user = ls_logitem-ref_doc_id.
          ENDIF.
*         Get PRIL date
          CLEAR: ls_logitem.
          READ TABLE lo_items->t_logitem INTO ls_logitem
            WITH KEY log_type     = limpi_log_del
                     ref_doc_type = wmegc_ref_plda.
          IF sy-subrc = 0.
            lv_count_date = ls_logitem-ref_doc_id.
          ENDIF.
        ELSE.
*         Normal deletion of the PI item
          lv_count_date = lo_items->data-count_date.
        ENDIF.
      ELSE.
        lv_count_date = lo_items->data-count_date.
      ENDIF.
      READ TABLE et_asp_oi_pi_pr ASSIGNING <ls_asp_oi_pi_pr>
        WITH KEY count_user = ls_asp_oi_pi_pr-count_user.
      IF sy-subrc = 0.
        ls_asp_oi_pi_pr-count_user_txt = <ls_asp_oi_pi_pr>-count_user_txt.
      ELSE.
        ls_asp_oi_pi_pr-count_user_txt =
          lcl_sp=>so_sp_fw->mo_sp_services->get_userid_text(
          iv_userid    = ls_asp_oi_pi_pr-count_user
          iv_lm_active = ls_asp_oi_pi_pr-lm_active ).
      ENDIF.
      lcl_sp=>so_sp_fw->mo_sp_services->convert_timestmp_2_date(
        EXPORTING
          iv_lgnum    = ls_asp_oi_pi_pr-pi_lgnum
          iv_timestmp = lv_count_date
        IMPORTING
          ev_date     = ls_asp_oi_pi_pr-count_date_ui
          ev_time     = ls_asp_oi_pi_pr-count_time_ui ).

      APPEND ls_asp_oi_pi_pr TO et_asp_oi_pi_pr.
    ENDLOOP.

    SORT et_asp_oi_pi_pr BY doc_year doc_number item_no.

  ENDMETHOD.                    "convert_item_read_2_oi_pi_pr
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_book                           - public
*----------------------------------------------------------------------*
*   IMPORTING is_item                  TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_book        TYPE /scwm/t_asp_pi_proc_od_book,
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_book.

    DATA:
      lo_items          TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lo_quan           TYPE REF TO /lime/pi_quan,
      ls_ui_stock       TYPE /scwm/s_ui_stock,
      ls_ui_quan        TYPE /scwm/s_ui_quan,
      ls_asp_od_pi_book TYPE /scwm/s_asp_pi_proc_od_book,
      ls_mat_global     TYPE /scwm/s_material_global.

    FIELD-SYMBOLS:
      <ls_asp_od_pi_book> TYPE /scwm/s_asp_pi_proc_od_book.

    CLEAR et_asp_od_pi_book.
    LOOP AT is_item-t_book REFERENCE INTO lo_items
      WHERE data-stock_item-matid IS NOT INITIAL.

      CLEAR: ls_asp_od_pi_book.
*     Header data
      ls_asp_od_pi_book-pi_lgnum = is_item-data-lgnum.
      ls_asp_od_pi_book-guid_doc = is_item-data-guid_doc.
      ls_asp_od_pi_book-item_no  = is_item-data-item_no.
*---- Stock Identifier
      ls_ui_stock = convert_item_read_2_stock( EXPORTING iv_lgnum = is_item-data-lgnum
                                                         is_item  = lo_items->data ).
      MOVE-CORRESPONDING ls_ui_stock TO ls_asp_od_pi_book.  "#EC ENHOK
      ls_ui_quan  = convert_item_read_2_quan( lo_items->data ).
      MOVE-CORRESPONDING ls_ui_quan TO ls_asp_od_pi_book.   "#EC ENHOK
*---- Quantities
*     In the RF UI multiple count entries for same item is possible.
*     In order to allow the display of the RF created counts, we need
*     to sum up the quantities.
*     Assume that zero count for material and additional entry with
*     count quantity should not occur. Count entry should have ended up
*     in error message in that case.
*     Assume as well that unit is always the base unit
*
*     Changes within SCM 6.0. Catch-Weight relevance
      CLEAR: ls_mat_global.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
            EXPORTING
              iv_matid      = lo_items->data-stock_item-matid
            IMPORTING
              es_mat_global = ls_mat_global.
        CATCH /scwm/cx_md.                              "#EC NO_HANDLER
      ENDTRY.
      CLEAR: ls_asp_od_pi_book-quantity,
             ls_asp_od_pi_book-unit,
             ls_asp_od_pi_book-cwquan,
             ls_asp_od_pi_book-cwunit.
      LOOP AT lo_items->t_quan REFERENCE INTO lo_quan.
        IF ls_mat_global-cwunit IS INITIAL.
          IF lo_quan->unit = ls_asp_od_pi_book-unit OR
             ls_asp_od_pi_book-unit IS INITIAL.
            ls_asp_od_pi_book-quantity
                    = ls_asp_od_pi_book-quantity + lo_quan->quantity.
            ls_asp_od_pi_book-unit = lo_quan->unit.
          ENDIF.
        ELSE.
          IF lo_quan->unit = ls_mat_global-meins.
            ls_asp_od_pi_book-quantity
                    = ls_asp_od_pi_book-quantity + lo_quan->quantity.
            ls_asp_od_pi_book-unit = lo_quan->unit.
          ELSEIF lo_quan->unit = ls_mat_global-cwunit.
            ls_asp_od_pi_book-cwquan
                    = ls_asp_od_pi_book-cwquan + lo_quan->quantity.
            ls_asp_od_pi_book-cwunit = lo_quan->unit.
          ENDIF.
        ENDIF.
      ENDLOOP.
*     Clear Soft Stock Attributes Without Quant Separation
      CLEAR: ls_asp_od_pi_book-wdatu,
             ls_asp_od_pi_book-ui_wdatu,
             ls_asp_od_pi_book-ui_wdatt,
             ls_asp_od_pi_book-vfdat,
             ls_asp_od_pi_book-coo,
             ls_asp_od_pi_book-coo_text.
*     Summation per same stock identifiers
      COLLECT ls_asp_od_pi_book INTO et_asp_od_pi_book.
    ENDLOOP.
*   Set LINE_IDX. Be aware that LINE_IDX needs to be renumbered as
*   aspect is summation for more than one book line item with
*   different original LINE_IDX.
    LOOP AT et_asp_od_pi_book ASSIGNING <ls_asp_od_pi_book>.
      <ls_asp_od_pi_book>-line_idx = sy-tabix.
    ENDLOOP.
*   Sort without line_idx as we want to sort by material
    SORT et_asp_od_pi_book
      BY item_no matnr charg cat entitled stock_doccat stock_docno
         stock_itmno owner stock_usage stref_doccat.

  ENDMETHOD.                    "convert_item_read_2_od_book
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_co                             - public
*----------------------------------------------------------------------*
*   IMPORTING is_item             TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_co     TYPE /scwm/t_asp_pi_proc_od,
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_co.

    DATA:
      ls_aspk_oi_pi_pr TYPE /scwm/s_aspk_oi_pi_item_proces,
      ls_asp_oi_pi_pr  TYPE /scwm/s_asp_oi_pi_item_proces,
      ls_ui_stock      TYPE /scwm/s_ui_stock,
      ls_ui_quan       TYPE /scwm/s_ui_quan,
      ls_asp_od_pi_co  TYPE /scwm/s_asp_pi_proc_od,
      ls_main_data     TYPE /scwm/s_hu_main_data,
      ls_huident       TYPE LINE OF /scwm/tt_huident,
      lt_huident       TYPE /scwm/tt_huident,
      lo_asp_od_pi_co  TYPE REF TO /scwm/s_asp_pi_proc_od,
      lo_items         TYPE REF TO /lime/pi_item_sub_read_get_sin.

    CLEAR et_asp_od_pi_co.
    "Get parent
    ls_aspk_oi_pi_pr-guid_doc = is_item-data-guid_doc.
    ls_aspk_oi_pi_pr-item_no  = is_item-data-item_no.
    lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_pr->get_single(
      EXPORTING
        is_aspk_oi_pi_pr = ls_aspk_oi_pi_pr
      IMPORTING
        es_asp_oi_pi_pr  = ls_asp_oi_pi_pr ).

    ls_huident-lgnum = is_item-data-lgnum.

    LOOP AT is_item-t_prop REFERENCE INTO lo_items.
      CLEAR: ls_asp_od_pi_co.
*     Header data
      ls_asp_od_pi_co-pi_lgnum = is_item-data-lgnum.
*     F4 help for batch is using lgnum not pi_lgnum
*     --> lgnum filled with pi_lgnum
      ls_asp_od_pi_co-lgnum    = is_item-data-lgnum.
      ls_asp_od_pi_co-guid_doc = is_item-data-guid_doc.
      ls_asp_od_pi_co-item_no  = is_item-data-item_no.
*---- Transfer general data
      MOVE-CORRESPONDING lo_items->data
                      TO ls_asp_od_pi_co.                   "#EC ENHOK
      MOVE lo_items->data-line_idx TO ls_asp_od_pi_co-line_idx_db.
*---- Set xmanual for empty bin proposal -> Empty bin proposal
*     should allow to change parent type and child type in field
*     selection. Therefore XMANUAL is necessary.
      IF ls_asp_od_pi_co-type_item IS INITIAL.
        ls_asp_od_pi_co-xmanual_item = abap_true.
      ENDIF.
*---- Location fields
      IF lo_items->data-type_parent = wmegc_lime_type_loc.
        MOVE-CORRESPONDING lo_items->data-loc_parent TO ls_asp_od_pi_co.
                                                            "#EC ENHOK
        ls_asp_od_pi_co-loc_type = wmegc_bin.
        ls_asp_od_pi_co-lgber    = ls_asp_oi_pi_pr-lgber.
      ENDIF.
*---- HU fields
      IF lo_items->data-type_parent = wmegc_lime_type_hu.
        ls_huident-huident = ls_asp_od_pi_co-huident = lo_items->data-hu_parent-huident.
        IF ls_huident-huident IS NOT INITIAL.
          APPEND ls_huident TO lt_huident.
        ENDIF.
      ENDIF.
      IF lo_items->data-type_item = wmegc_lime_type_hu.
        ls_huident-huident = ls_asp_od_pi_co-huident_item = lo_items->data-hu_item-huident.
        IF ls_huident-huident IS NOT INITIAL.
          APPEND ls_huident TO lt_huident.
        ENDIF.
      ENDIF.
*---- Indicators
      CASE lo_items->data-ind_item_checked.
        WHEN limpi_check_exist.
          ls_asp_od_pi_co-ind_exist = abap_true.
          ls_asp_od_pi_co-ind_noex  = abap_false.
        WHEN limpi_check_not_ex.
          ls_asp_od_pi_co-ind_exist = abap_false.
          ls_asp_od_pi_co-ind_noex  = abap_true.
        WHEN OTHERS.
          ls_asp_od_pi_co-ind_exist = abap_false.
          ls_asp_od_pi_co-ind_noex  = abap_false.
      ENDCASE.
      ls_asp_od_pi_co-ind_add_item = lo_items->data-ind_add_it_check.

      IF lo_items->data-stock_item-matid IS NOT INITIAL.
*----   Stock Identifier
        ls_ui_stock = convert_item_read_2_stock(
          EXPORTING
            iv_lgnum = is_item-data-lgnum
            is_item  = lo_items->data ).
        MOVE-CORRESPONDING ls_ui_stock TO ls_asp_od_pi_co.  "#EC ENHOK
        ls_ui_quan  = convert_item_read_2_quan( lo_items->data ).
        MOVE-CORRESPONDING ls_ui_quan TO ls_asp_od_pi_co.   "#EC ENHOK
*       get serial customizing
        TRY.
            /scwm/cl_serial=>get_serial(
              EXPORTING
                iv_lgnum    = is_item-data-lgnum
                iv_matid    = lo_items->data-stock_item-matid
                iv_entitled = lo_items->data-stock_item-entitled
              IMPORTING
                ev_stock    = ls_asp_od_pi_co-ser_stock ).
          CATCH /scwm/cx_serial.
            CLEAR ls_asp_od_pi_co-ser_stock.
        ENDTRY.
**       Get batch status->removed because of performance issues!
*        IF ls_asp_od_pi_co-batchid IS NOT INITIAL.
*          TRY.
*              CALL FUNCTION '/SCWM/BATCH_GET'
*                EXPORTING
*                  iv_lgnum    = ls_asp_od_pi_co-pi_lgnum
*                  iv_entitled = lo_items->data-stock_item-entitled
*                  iv_matid    = lo_items->data-stock_item-matid
*                  iv_batchid  = ls_asp_od_pi_co-batchid
*                IMPORTING
*                  ev_brestr   = ls_asp_od_pi_co-brestr.
*            CATCH /scwm/cx_core.                        "#EC NO_HANDLER
*          ENDTRY.
*        ENDIF.

        set_count_quanity(
          EXPORTING
            is_item         = lo_items->*
          CHANGING
            cs_asp_od_pi_co = ls_asp_od_pi_co ).
      ENDIF.
      APPEND ls_asp_od_pi_co TO et_asp_od_pi_co.
    ENDLOOP. "is_item-t_prop into lo_items
    "buffer HU's
    IF lt_huident IS NOT INITIAL.
      SORT lt_huident.
      DELETE ADJACENT DUPLICATES FROM lt_huident.
      ls_main_data-appl  = wmegc_huappl_wme.
      ls_main_data-lgnum = is_item-data-lgnum.
      CALL FUNCTION '/SCWM/HU_GT_FILL'
        EXPORTING
          iv_appl      = wmegc_huappl_wme
          is_main_data = ls_main_data
          it_huident   = lt_huident
        EXCEPTIONS
          error        = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        CLEAR lt_huident.
      ENDIF.
    ENDIF.
*   Set LINE_IDX. Be aware that LINE_IDX needs to be renumbered as
*   aspect has duplicate keys if more than one quantity with the
*   same original LINE_IDX.
    LOOP AT et_asp_od_pi_co REFERENCE INTO lo_asp_od_pi_co.
      lo_asp_od_pi_co->line_idx = sy-tabix.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od_co
*----------------------------------------------------------------------*
* METHOD set_count_quanity                          - private
*----------------------------------------------------------------------*
*   IMPORTING is_item         TYPE /lime/pi_item_sub_read_get_sin
*    CHANGING cs_asp_od_pi_co TYPE /scwm/s_asp_pi_proc_od
*----------------------------------------------------------------------*
  METHOD set_count_quanity.

    DATA:
      lv_lines      TYPE i,
      lv_count      TYPE i,
      ls_mat_global TYPE /scwm/s_material_global.

    FIELD-SYMBOLS:
      <ls_quan>    TYPE /lime/pi_quan.

    CLEAR: cs_asp_od_pi_co-quantity,
           cs_asp_od_pi_co-unit,
           cs_asp_od_pi_co-quantity_b,
           cs_asp_od_pi_co-unit_b,
           cs_asp_od_pi_co-book_quantity,
           cs_asp_od_pi_co-book_quan_unit,
           cs_asp_od_pi_co-ind_zero_count.
*   Check Catch Weight relevanz
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = is_item-data-stock_item-matid
            iv_entitled   = is_item-data-stock_item-entitled
            iv_lgnum      = cs_asp_od_pi_co-pi_lgnum
          IMPORTING
            es_mat_global = ls_mat_global.
      CATCH /scwm/cx_md.
        RETURN.
    ENDTRY.

    DESCRIBE TABLE is_item-t_quan LINES lv_lines.

    IF ( ls_mat_global-cwrel IS NOT INITIAL AND lv_lines < 3 ) OR
       ( ls_mat_global-cwrel IS INITIAL AND lv_lines = 1 ).
      READ TABLE is_item-t_quan ASSIGNING <ls_quan>
        WITH KEY unit = ls_mat_global-meins.
      IF sy-subrc = 0.
*       define the counted quantities for Base UOM first
        cs_asp_od_pi_co-quantity       = <ls_quan>-entered_quantity.
        cs_asp_od_pi_co-unit           = <ls_quan>-entered_unit.
        cs_asp_od_pi_co-quantity_b     = <ls_quan>-quantity.
        cs_asp_od_pi_co-unit_b         = <ls_quan>-unit.
        cs_asp_od_pi_co-book_quantity  = <ls_quan>-book_quantity.
        cs_asp_od_pi_co-book_quan_unit = <ls_quan>-book_quan_unit.
        cs_asp_od_pi_co-ind_zero_count = <ls_quan>-ind_zero_count.
      ENDIF.
      IF ls_mat_global-cwunit IS NOT INITIAL.
*       Catch Weight relevant
        READ TABLE is_item-t_quan ASSIGNING <ls_quan>
          WITH KEY unit = ls_mat_global-cwunit.
        IF sy-subrc = 0.
          cs_asp_od_pi_co-acwquan = <ls_quan>-entered_quantity.
          cs_asp_od_pi_co-acwunit = <ls_quan>-entered_unit.
          cs_asp_od_pi_co-cwquan  = <ls_quan>-quantity.
          cs_asp_od_pi_co-cwunit  = <ls_quan>-unit.
          cs_asp_od_pi_co-cwbquan = <ls_quan>-book_quantity.
          cs_asp_od_pi_co-cwbunit = <ls_quan>-book_quan_unit.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.
*   set multiple count. The quantities will be cumulated
    cs_asp_od_pi_co-unit_b = ls_mat_global-meins.
    CLEAR: lv_count.
*   cumulate the counted quantities for Base UOM
    LOOP AT is_item-t_quan ASSIGNING <ls_quan>
      WHERE unit = ls_mat_global-meins.
      lv_count = lv_count + 1.
      cs_asp_od_pi_co-quantity_b =
         cs_asp_od_pi_co-quantity_b + <ls_quan>-quantity.
*     for first time set entered unit
      IF lv_count = 1.
        cs_asp_od_pi_co-quantity = <ls_quan>-entered_quantity.
        cs_asp_od_pi_co-unit     = <ls_quan>-entered_unit.
      ELSE.
        IF cs_asp_od_pi_co-unit <> <ls_quan>-entered_unit.
          CLEAR: cs_asp_od_pi_co-quantity, cs_asp_od_pi_co-unit.
        ELSE.
          cs_asp_od_pi_co-quantity =
           cs_asp_od_pi_co-quantity + <ls_quan>-entered_quantity.
        ENDIF.
      ENDIF.
      IF <ls_quan>-ind_zero_count IS NOT INITIAL .
        cs_asp_od_pi_co-ind_zero_count = <ls_quan>-ind_zero_count.
      ENDIF.
    ENDLOOP.
*   the zero count could be set for one entry only!
    IF cs_asp_od_pi_co-ind_zero_count IS NOT INITIAL AND
       cs_asp_od_pi_co-quantity_b     IS NOT INITIAL.
      CLEAR cs_asp_od_pi_co-ind_zero_count.
    ENDIF.
    IF ls_mat_global-cwrel IS NOT INITIAL.
      cs_asp_od_pi_co-cwunit = ls_mat_global-cwunit.
      CLEAR: lv_count.
*     cumulate the counted quantities for Base CW UOM
      LOOP AT is_item-t_quan ASSIGNING <ls_quan>
        WHERE unit = ls_mat_global-cwunit.
        lv_count = lv_count + 1.
        cs_asp_od_pi_co-cwquan =
           cs_asp_od_pi_co-cwquan + <ls_quan>-quantity.
*       for first time set entered unit
        IF lv_count = 1.
          cs_asp_od_pi_co-acwquan = <ls_quan>-entered_quantity.
          cs_asp_od_pi_co-acwunit = <ls_quan>-entered_unit.
        ELSE.
          IF cs_asp_od_pi_co-acwunit <> <ls_quan>-entered_unit.
            CLEAR: cs_asp_od_pi_co-acwquan, cs_asp_od_pi_co-acwunit.
          ELSE.
            cs_asp_od_pi_co-acwquan =
             cs_asp_od_pi_co-acwquan + <ls_quan>-entered_quantity.
          ENDIF.
        ENDIF.
      ENDLOOP.
*     set base CW UOM if counted with different UOM's
      IF cs_asp_od_pi_co-acwunit IS INITIAL.
        cs_asp_od_pi_co-acwunit = cs_asp_od_pi_co-cwunit.
        cs_asp_od_pi_co-acwquan = cs_asp_od_pi_co-cwquan.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "set_count_quanity
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_deriv                          - public
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   IMPORTING is_item            TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_deriv TYPE /scwm/t_asp_pi_proc_od_deriv
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_deriv.

    DATA:
      ls_header          TYPE /lime/pi_item_read_get_single,
      lo_items           TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lo_quan            TYPE REF TO /lime/pi_quan,
      ls_ui_stock        TYPE /scwm/s_ui_stock,
      ls_ui_quan         TYPE /scwm/s_ui_quan,
      ls_asp_od_pi_deriv TYPE /scwm/s_asp_pi_proc_od_deriv,
      ls_lagp            TYPE /scwm/lagp,
      ls_mat_global      TYPE /scwm/s_material_global.

    CLEAR et_asp_od_pi_deriv.
    ls_header = is_item.
*   Remove dummy HUs
    me->condense_tree(
      EXPORTING
        is_item = is_item
      IMPORTING
        et_diff = ls_header-t_difference ).

    LOOP AT ls_header-t_difference REFERENCE INTO lo_items.

      CLEAR: ls_asp_od_pi_deriv.
*     Header data
      ls_asp_od_pi_deriv-pi_lgnum = ls_asp_od_pi_deriv-lgnum = ls_header-data-lgnum.
      ls_asp_od_pi_deriv-guid_doc = ls_header-data-guid_doc.
      ls_asp_od_pi_deriv-item_no  = ls_header-data-item_no.
*---- Transfer general data
      MOVE-CORRESPONDING lo_items->data
                                    TO ls_asp_od_pi_deriv.  "#EC ENHOK
*---- Stock Identifier
      ls_ui_stock = convert_item_read_2_stock( EXPORTING iv_lgnum = is_item-data-lgnum
                                                         is_item  = lo_items->data ).
      MOVE-CORRESPONDING ls_ui_stock
                                    TO ls_asp_od_pi_deriv.  "#EC ENHOK
      ls_ui_quan  = convert_item_read_2_quan( lo_items->data ).
      MOVE-CORRESPONDING ls_ui_quan TO ls_asp_od_pi_deriv.  "#EC ENHOK
*     Get batch status
      IF NOT lo_items->data-stock_item-matid IS INITIAL AND
         NOT ls_asp_od_pi_deriv-batchid IS INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/BATCH_GET'
              EXPORTING
                iv_lgnum    = ls_asp_od_pi_deriv-pi_lgnum
                iv_entitled = lo_items->data-stock_item-entitled
                iv_matid    = lo_items->data-stock_item-matid
                iv_batchid  = ls_asp_od_pi_deriv-batchid
              IMPORTING
                ev_brestr   = ls_asp_od_pi_deriv-brestr.
          CATCH /scwm/cx_core.                          "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
*---- Location fields
      MOVE-CORRESPONDING lo_items->data-loc_parent
                                      TO ls_asp_od_pi_deriv. "#EC ENHOK
      ls_asp_od_pi_deriv-loc_type = wmegc_bin.
      ls_asp_od_pi_deriv-ind_loc_par_empt
                              = lo_items->data-ind_loc_par_empt.
*     Determine LGBER
      IF NOT ls_asp_od_pi_deriv-lgnum IS INITIAL.

        CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
          EXPORTING
            iv_lgnum    = ls_asp_od_pi_deriv-lgnum
            iv_lgpla    = ls_asp_od_pi_deriv-lgpla
          IMPORTING
            es_lagp     = ls_lagp
          EXCEPTIONS
            wrong_input = 1
            not_found   = 2
            OTHERS      = 3.
        IF sy-subrc = 0.
          ls_asp_od_pi_deriv-lgber = ls_lagp-lgber.
        ENDIF.
      ENDIF.
*---- HU fields
      ls_asp_od_pi_deriv-huident_item = lo_items->data-hu_item-huident.
      ls_asp_od_pi_deriv-huident      = lo_items->data-hu_parent-huident.
*     Quantities:
*     Changes within SCM 6.0. Catch-Weight relevance.
*     Differences in base and CW unit will be displayed
      CLEAR: ls_mat_global.
      IF NOT lo_items->data-stock_item-matid IS INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
              EXPORTING
                iv_matid      = lo_items->data-stock_item-matid
              IMPORTING
                es_mat_global = ls_mat_global.
          CATCH /scwm/cx_md.                            "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
      CLEAR: ls_asp_od_pi_deriv-quantity,
             ls_asp_od_pi_deriv-unit,
             ls_asp_od_pi_deriv-cwquan,
             ls_asp_od_pi_deriv-cwunit.
      LOOP AT lo_items->t_quan REFERENCE INTO lo_quan.
        IF ls_mat_global-cwrel IS INITIAL.
          IF lo_quan->unit = ls_asp_od_pi_deriv-unit OR
             ls_asp_od_pi_deriv-unit IS INITIAL.
            ls_asp_od_pi_deriv-quantity
                    = ls_asp_od_pi_deriv-quantity + lo_quan->quantity.
            ls_asp_od_pi_deriv-unit = lo_quan->unit.
          ENDIF.
        ELSE.
          IF lo_quan->unit = ls_mat_global-meins.
            ls_asp_od_pi_deriv-quantity
                    = ls_asp_od_pi_deriv-quantity + lo_quan->quantity.
            ls_asp_od_pi_deriv-unit = lo_quan->unit.
          ELSEIF lo_quan->unit = ls_mat_global-cwunit.
            ls_asp_od_pi_deriv-cwquan
                    = ls_asp_od_pi_deriv-cwquan + lo_quan->quantity.
            ls_asp_od_pi_deriv-cwunit = lo_quan->unit.
          ENDIF.
        ENDIF.
      ENDLOOP.
*---- Direction of difference I = + ; O = - (used for HU found/lost)
*     and also for the quantity!! (Changed within development)
      IF ls_asp_od_pi_deriv-type_item = wmegc_lime_type_hu.
        CASE ls_asp_od_pi_deriv-dif_direction.
          WHEN wmegc_lime_post_inbound.
            ls_asp_od_pi_deriv-sign = '+'.
          WHEN wmegc_lime_post_outbound.
            ls_asp_od_pi_deriv-sign = '-'.
          WHEN OTHERS.
        ENDCASE.
*     Set sign of difference quantity
      ELSEIF ls_asp_od_pi_deriv-type_item = wmegc_lime_type_stock.
        CASE ls_asp_od_pi_deriv-dif_direction.
          WHEN wmegc_lime_post_outbound.
            ls_asp_od_pi_deriv-sign = '-'.
          WHEN wmegc_lime_post_inbound.
            ls_asp_od_pi_deriv-sign = '+'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      APPEND ls_asp_od_pi_deriv TO et_asp_od_pi_deriv.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od_deriv
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_diff                           - public
*----------------------------------------------------------------------*
*   IMPORTING ls_item           TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_diff TYPE /scwm/t_asp_pi_proc_od_diff
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_diff.

    DATA:
      lo_items          TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lo_quan           TYPE REF TO /lime/pi_quan,
      ls_ui_stock       TYPE /scwm/s_ui_stock,
      ls_ui_quan        TYPE /scwm/s_ui_quan,
      ls_asp_od_pi_diff TYPE /scwm/s_asp_pi_proc_od_diff,
      lo_asp_od_pi_diff TYPE REF TO /scwm/s_asp_pi_proc_od_diff,
      lv_quantity       TYPE /lime/pi_quantity,
      ls_mat_global     TYPE /scwm/s_material_global.

    CLEAR et_asp_od_pi_diff.
    LOOP AT is_item-t_difference REFERENCE INTO lo_items
*     Only for stock items
      WHERE data-stock_item-matid IS NOT INITIAL.
      CLEAR: ls_asp_od_pi_diff.
*---- Header data
      ls_asp_od_pi_diff-pi_lgnum = is_item-data-lgnum.
      ls_asp_od_pi_diff-guid_doc = is_item-data-guid_doc.
      ls_asp_od_pi_diff-item_no  = is_item-data-item_no.
*---- Stock Identifier
      ls_ui_stock = convert_item_read_2_stock( EXPORTING iv_lgnum = is_item-data-lgnum
                                                         is_item  = lo_items->data ).
      MOVE-CORRESPONDING ls_ui_stock TO ls_asp_od_pi_diff.  "#EC ENHOK
      ls_ui_quan  = convert_item_read_2_quan( lo_items->data ).
      MOVE-CORRESPONDING ls_ui_quan TO ls_asp_od_pi_diff.   "#EC ENHOK
*---- Quantities
*     Changes within SCM 6.0. Catch-Weight relevance
*     Differences in base and CW unit will be displayed
      CLEAR: ls_mat_global.
      IF NOT lo_items->data-stock_item-matid IS INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
              EXPORTING
                iv_matid      = lo_items->data-stock_item-matid
              IMPORTING
                es_mat_global = ls_mat_global.
          CATCH /scwm/cx_md.                            "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
      CLEAR: ls_asp_od_pi_diff-quantity,
             ls_asp_od_pi_diff-unit,
             ls_asp_od_pi_diff-cwquan,
             ls_asp_od_pi_diff-cwunit.
      LOOP AT lo_items->t_quan REFERENCE INTO lo_quan.
        CLEAR: lv_quantity.
        IF ls_mat_global-cwunit IS INITIAL.
          IF lo_quan->unit = ls_asp_od_pi_diff-unit OR
             ls_asp_od_pi_diff-unit IS INITIAL.
*           Set sign of the quantities
            CASE lo_items->data-dif_direction.
              WHEN wmegc_lime_post_inbound.
                lv_quantity = lo_quan->quantity.
              WHEN wmegc_lime_post_outbound.
                lv_quantity = lo_quan->quantity * ( -1 ).
            ENDCASE.
            ls_asp_od_pi_diff-quantity
                    = ls_asp_od_pi_diff-quantity + lv_quantity.
            ls_asp_od_pi_diff-unit = lo_quan->unit.
          ENDIF.
        ELSE.
          IF lo_quan->unit = ls_mat_global-meins.
*           Set sign of the quantities
            CASE lo_items->data-dif_direction.
              WHEN wmegc_lime_post_inbound.
                lv_quantity = lo_quan->quantity.
              WHEN wmegc_lime_post_outbound.
                lv_quantity = lo_quan->quantity * ( -1 ).
            ENDCASE.
            ls_asp_od_pi_diff-quantity
                    = ls_asp_od_pi_diff-quantity + lv_quantity.
            ls_asp_od_pi_diff-unit = lo_quan->unit.
          ELSEIF lo_quan->unit = ls_mat_global-cwunit.
*           Set sign of the quantities
            CASE lo_items->data-dif_direction.
              WHEN wmegc_lime_post_inbound.
                lv_quantity = lo_quan->quantity.
              WHEN wmegc_lime_post_outbound.
                lv_quantity = lo_quan->quantity * ( -1 ).
            ENDCASE.
            ls_asp_od_pi_diff-cwquan
                    = ls_asp_od_pi_diff-cwquan + lv_quantity.
            ls_asp_od_pi_diff-cwunit = lo_quan->unit.
          ENDIF.
        ENDIF.
      ENDLOOP.
*     Get batch status
      IF NOT lo_items->data-stock_item-matid IS INITIAL AND
         NOT ls_asp_od_pi_diff-batchid IS INITIAL.
        TRY.
            CALL FUNCTION '/SCWM/BATCH_GET'
              EXPORTING
                iv_lgnum    = is_item-data-lgnum
                iv_entitled = lo_items->data-stock_item-entitled
                iv_matid    = lo_items->data-stock_item-matid
                iv_batchid  = ls_asp_od_pi_diff-batchid
              IMPORTING
                ev_brestr   = ls_asp_od_pi_diff-brestr.
          CATCH /scwm/cx_core.                          "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
*     Clear Soft Stock Attributes Without Quant Separation
      CLEAR: ls_asp_od_pi_diff-wdatu,
             ls_asp_od_pi_diff-ui_wdatu,
             ls_asp_od_pi_diff-ui_wdatt,
             ls_asp_od_pi_diff-vfdat,
             ls_asp_od_pi_diff-coo,
             ls_asp_od_pi_diff-coo_text.
*     Summation per same stock identifiers
      COLLECT ls_asp_od_pi_diff INTO et_asp_od_pi_diff.
    ENDLOOP.
*   Set LINE_IDX. Be aware that LINE_IDX needs to be renumbered as
*   aspect is summation for more than one difference line item with
*   different original LINE_IDX.
    LOOP AT et_asp_od_pi_diff REFERENCE INTO lo_asp_od_pi_diff.
      lo_asp_od_pi_diff->line_idx = sy-tabix.
    ENDLOOP.
*   Sort without line_idx as we want to sort by material
    SORT et_asp_od_pi_diff
      BY item_no matnr charg cat entitled
         stock_doccat stock_docno stock_itmno
         owner stock_usage stref_doccat.

  ENDMETHOD.                    "convert_item_read_2_od_diff
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_log                            - public
*----------------------------------------------------------------------*
*   IMPORTING ls_item          TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_log TYPE /scwm/t_asp_pi_proc_od_log
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_log.

    DATA:
      ls_asp_od_pi_log TYPE /scwm/s_asp_pi_proc_od_log.

    FIELD-SYMBOLS:
      <ls_items>       TYPE /lime/pi_doc_logitem.

    CLEAR et_asp_od_pi_log.
    LOOP AT is_item-t_logitem ASSIGNING <ls_items>
                              WHERE ref_doc_type IS INITIAL.

      CLEAR: ls_asp_od_pi_log.
      ls_asp_od_pi_log-pi_lgnum      = is_item-data-lgnum.
      ls_asp_od_pi_log-guid_doc      = is_item-data-guid_doc.
      ls_asp_od_pi_log-item_no       = is_item-data-item_no.
      ls_asp_od_pi_log-log_seq       = <ls_items>-log_seq.
      ls_asp_od_pi_log-log_type      = <ls_items>-log_type.
      ls_asp_od_pi_log-userid        = <ls_items>-userid.
      ls_asp_od_pi_log-timestamp     = <ls_items>-timestamp.
      ls_asp_od_pi_log-log_type_text =
        lcl_sp=>so_sp_fw->mo_sp_services->get_log_type_text(
        ls_asp_od_pi_log-log_type ).
      ls_asp_od_pi_log-userid_text =
        lcl_sp=>so_sp_fw->mo_sp_services->get_userid_text(
        iv_userid  = <ls_items>-userid
        iv_syuname = abap_true ).
*     Convert timestamp
      lcl_sp=>so_sp_fw->mo_sp_services->convert_timestmp_2_date(
        EXPORTING
          iv_lgnum    = ls_asp_od_pi_log-pi_lgnum
          iv_timestmp = ls_asp_od_pi_log-timestamp
        IMPORTING
          ev_date     = ls_asp_od_pi_log-date_ui
          ev_time     = ls_asp_od_pi_log-time_ui ).

      APPEND ls_asp_od_pi_log TO et_asp_od_pi_log.
    ENDLOOP.

    SORT et_asp_od_pi_log BY timestamp.

  ENDMETHOD.                    "convert_item_read_2_od_log
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od_ref                            - public
*----------------------------------------------------------------------*
*   IMPORTING ls_item          TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od_pi_ref TYPE /scwm/t_asp_pi_proc_od_ref,
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od_ref.

    DATA:
      lv_mlen          TYPE i,
      lo_items         TYPE REF TO /lime/pi_doc_logitem,
      ls_asp_od_pi_ref TYPE /scwm/s_asp_pi_proc_od_ref.

    CLEAR et_asp_od_pi_ref.
    LOOP AT is_item-t_logitem REFERENCE INTO lo_items
                   WHERE NOT ref_doc_type IS INITIAL.

      CLEAR: ls_asp_od_pi_ref.
      ls_asp_od_pi_ref-pi_lgnum = is_item-data-lgnum.
      ls_asp_od_pi_ref-guid_doc = is_item-data-guid_doc.
      ls_asp_od_pi_ref-item_no  = is_item-data-item_no.
      ls_asp_od_pi_ref-log_seq  = lo_items->log_seq.
      ls_asp_od_pi_ref-log_type = lo_items->log_type.

      ls_asp_od_pi_ref-log_type_text =
        lcl_sp=>so_sp_fw->mo_sp_services->get_log_type_text(
        ls_asp_od_pi_ref-log_type ).

      ls_asp_od_pi_ref-ref_doc_type   = lo_items->ref_doc_type.
*     prepare Warehouse order, exception code and recount
      IF ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_who  OR
         ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_to   OR
         ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_exce OR
         ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_inte OR
         ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_pido.
        lv_mlen = strlen( is_item-data-lgnum ).
        ls_asp_od_pi_ref-ref_doc_id = lo_items->ref_doc_id+lv_mlen.
        IF ls_asp_od_pi_ref-ref_doc_type = wmegc_ref_pido.
          CONCATENATE ls_asp_od_pi_ref-ref_doc_id(4)
                      ls_asp_od_pi_ref-ref_doc_id+4(20)
                      ls_asp_od_pi_ref-ref_doc_id+24(6)
                 INTO ls_asp_od_pi_ref-ref_doc_id SEPARATED BY space.
        ENDIF.
      ELSE.
        ls_asp_od_pi_ref-ref_doc_id = lo_items->ref_doc_id.
      ENDIF.
      ls_asp_od_pi_ref-ref_doc_logsys = lo_items->ref_doc_logsys.

      ls_asp_od_pi_ref-txt_ref_doc_type =
        lcl_sp=>so_sp_fw->mo_lime_cust->get_ref_doctype_txt(
        ls_asp_od_pi_ref-ref_doc_type ).

      APPEND ls_asp_od_pi_ref TO et_asp_od_pi_ref.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od_ref
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_stock                            - private
*----------------------------------------------------------------------*
*   IMPORTING iv_lgnum           TYPE /scwm/lgnum
*             is_item            TYPE /lime/pi_item_pos_sub
*   RETURNING value(es_ui_stock) TYPE /scwm/s_ui_stock,
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_stock.

    CLEAR es_ui_stock.
    IF NOT is_item-stock_item-matid IS INITIAL.
      MOVE-CORRESPONDING is_item-stock_item TO es_ui_stock. "#EC ENHOK
      es_ui_stock-stref_doccat = is_item-stock_item-doccat.
      mo_stock_fields->get_matkey_by_id(
        EXPORTING
          iv_matid = is_item-stock_item-matid
        IMPORTING
          ev_matnr = es_ui_stock-matnr
          ev_maktx = es_ui_stock-maktx ).
      es_ui_stock-charg = mo_stock_fields->get_batchno_by_id(
        is_item-stock_item-batchid ).
      es_ui_stock-cat_text = mo_stock_fields->get_cat_text(
        EXPORTING
          iv_lgnum = iv_lgnum
          iv_cat   = is_item-stock_item-cat ).
      es_ui_stock-owner_text = mo_stock_fields->get_partner_text_by_no(
        is_item-stock_item-owner ).
      es_ui_stock-entitled_text = mo_stock_fields->get_partner_text_by_no(
        is_item-stock_item-entitled ).
      es_ui_stock-stock_usage_text = mo_stock_fields->get_stock_usage_text(
        is_item-stock_item-stock_usage ).
      es_ui_stock-stock_doccat_text = mo_stock_fields->get_stock_doccat_text(
        is_item-stock_item-stock_doccat ).
      es_ui_stock-stock_docno_ext = lcl_sp=>so_sp_fw->mo_stock_fields->get_stock_docno_ext(
        iv_stock_doccat = is_item-stock_item-stock_doccat
        iv_stock_docno  = is_item-stock_item-stock_docno ).
      es_ui_stock-stref_doccat_text = mo_stock_fields->get_stref_doccat_text(
        is_item-stock_item-doccat ).
    ENDIF.

  ENDMETHOD.                    "convert_item_read_2_stock
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_quan                             - private
*----------------------------------------------------------------------*
*   IMPORTING is_item            TYPE  /lime/pi_item_pos_sub
*   RETURNING value(es_ui_quan)  TYPE /scwm/s_ui_quan,
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_quan.

    CLEAR es_ui_quan.
    IF NOT is_item-stock_item-matid IS INITIAL.

      es_ui_quan-wdatu          = is_item-wdatu.
      CALL METHOD mo_stock_fields->get_ui_wdatu
        EXPORTING
          iv_lgnum    = is_item-stock_item-lgnum_stock
          iv_wdatu    = is_item-wdatu
        IMPORTING
          ev_ui_wdatu = es_ui_quan-ui_wdatu
          ev_ui_wdatt = es_ui_quan-ui_wdatt.
      es_ui_quan-vfdat          = is_item-vfdat.
      es_ui_quan-coo            = is_item-coo.
      es_ui_quan-coo_text = mo_stock_fields->get_coo_text( is_item-coo ).
      es_ui_quan-idplate        = is_item-idplate.
      es_ui_quan-qdoccat        = is_item-qdoccat.
      es_ui_quan-qdoccat_text = mo_stock_fields->get_qdoccat_text( is_item-qdoccat ).
      es_ui_quan-qdocid         = is_item-qdocid.
      es_ui_quan-qitmid         = is_item-qitmid.
*     Batch status
      es_ui_quan-brestr         = is_item-brestr.
      CALL METHOD mo_stock_fields->get_qdocno_by_id
        EXPORTING
          iv_qdoccat = is_item-qdoccat
          iv_qdocid  = is_item-qdocid
          iv_qitmid  = is_item-qitmid
          iv_lgnum   = is_item-stock_item-lgnum_stock
        IMPORTING
          ev_qdocno  = es_ui_quan-qdocno
          ev_qitemno = es_ui_quan-qitemno.
      es_ui_quan-insptyp        = is_item-insptyp.
      IF NOT is_item-insptyp IS INITIAL.
        es_ui_quan-insptyp_text = mo_stock_fields->get_insptyp_text( is_item-insptyp ).
        es_ui_quan-inspid         = is_item-inspid.
        CALL METHOD mo_stock_fields->get_inspdocno_by_id
          EXPORTING
            iv_insptyp   = is_item-insptyp
            iv_inspid    = is_item-inspid
          IMPORTING
            ev_inspdocno = es_ui_quan-inspdocno.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "convert_item_read_2_quan
*----------------------------------------------------------------------*
* METHOD convert_bapiret_2_msg_handler                         - public
*----------------------------------------------------------------------*
*   IMPORTING is_bapiret            TYPE bapiret2
*             iv_aspect             TYPE string
*             it_aspect_key         TYPE index table
*   RETURNING value(es_msg_handler) TYPE ys_msg_handler.
*----------------------------------------------------------------------*
  METHOD convert_bapiret_2_msg_handler.

    CLEAR es_msg_handler.
    es_msg_handler-msg    = convert_bapiret_2_msg( is_bapiret ).
    es_msg_handler-aspect = iv_aspect.
    READ TABLE it_aspect_key
               REFERENCE INTO es_msg_handler-o_aspect_key
               INDEX is_bapiret-row.

  ENDMETHOD.                    "convert_bapiret_2_msg_handler
*----------------------------------------------------------------------*
* METHOD convert_bapiret_2_msg                                - private
*----------------------------------------------------------------------*
*   IMPORTING is_bapiret    TYPE bapiret2
*   RETURNING value(es_msg) TYPE symsg.
*----------------------------------------------------------------------*
  METHOD convert_bapiret_2_msg.

    CLEAR es_msg.
    es_msg-msgty = is_bapiret-type.
    es_msg-msgid = is_bapiret-id.
    es_msg-msgno = is_bapiret-number.
    es_msg-msgv1 = is_bapiret-message_v1.
    es_msg-msgv2 = is_bapiret-message_v2.
    es_msg-msgv3 = is_bapiret-message_v3.
    es_msg-msgv4 = is_bapiret-message_v4.

  ENDMETHOD.                    "convert_bapiret_2_msg
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od2_co                            - public
*----------------------------------------------------------------------*
*   IMPORTING is_item        TYPE /lime/pi_item_read_get_single
*   EXPORTING et_asp_od2_co  TYPE /scwm/t_asp_pi_proc_od2
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od2_co.

    DATA:
      ls_asp_od2_pi_co TYPE /scwm/s_asp_pi_proc_od2,
      lv_tabix         TYPE sy-tabix.

    FIELD-SYMBOLS:
      <ls_items>     TYPE /lime/pi_item_sub_read_get_sin,
      <ls_asp_od_co> TYPE /scwm/s_asp_pi_proc_od,
      <ls_usid>      TYPE /lime/pi_usid_biz.

    CLEAR et_asp_od2_co.
*   Header data
    ls_asp_od2_pi_co-pi_lgnum = is_item-data-lgnum.
    ls_asp_od2_pi_co-guid_doc = is_item-data-guid_doc.
    ls_asp_od2_pi_co-item_no  = is_item-data-item_no.

    LOOP AT is_item-t_prop ASSIGNING <ls_items>
      WHERE data-type_item = wmegc_lime_type_stock.
*---- Transfer general data(buffer)
      ls_asp_od2_pi_co-line_idx_db = <ls_items>-data-line_idx.
*     prepare counted serial numbers for display
      READ TABLE it_asp_od_co ASSIGNING <ls_asp_od_co>
        WITH KEY line_idx_db = <ls_items>-data-line_idx
                 BINARY SEARCH.
      CHECK sy-subrc = 0 AND <ls_asp_od_co>-quantity > 0.
      CLEAR lv_tabix.
      ls_asp_od2_pi_co-line_idx = <ls_asp_od_co>-line_idx.
      LOOP AT <ls_items>-t_usid ASSIGNING <ls_usid>.
        lv_tabix = lv_tabix + 1.
        ls_asp_od2_pi_co-ser_seq = lv_tabix.
        ls_asp_od2_pi_co-serid   = <ls_usid>-sernr.
        ls_asp_od2_pi_co-uii     = <ls_usid>-uii.
        APPEND ls_asp_od2_pi_co TO et_asp_od2_co.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od2_co
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od2_book                          - public
*----------------------------------------------------------------------*
*   IMPORTING it_book              TYPE /lime/pi_t_item_sub_read_get_s
*             is_asp_od_pi_book    TYPE /scwm/s_asp_pi_proc_od_book
*   EXPORTING et_asp_od2_bo        TYPE /scwm/t_asp_pi_proc_od2
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od2_book.

    DATA:
      ls_asp_od2_pi_bo TYPE /scwm/s_asp_pi_proc_od2,
      lv_tabix         TYPE sy-tabix.

    FIELD-SYMBOLS:
      <ls_items> TYPE /lime/pi_item_sub_read_get_sin,
      <ls_usid>  TYPE /lime/pi_usid_biz.

    CLEAR: et_asp_od2_bo.
    READ TABLE it_book TRANSPORTING NO FIELDS
      WITH KEY data-type_item               = wmegc_lime_type_stock
               data-stock_item-matid        = is_asp_od_pi_book-matid
               data-stock_item-batchid      = is_asp_od_pi_book-batchid
               data-stock_item-cat          = is_asp_od_pi_book-cat
               data-stock_item-entitled     = is_asp_od_pi_book-entitled
               data-stock_item-owner        = is_asp_od_pi_book-owner
               data-stock_item-stock_doccat =
                                       is_asp_od_pi_book-stock_doccat
               data-stock_item-stock_docno  =
                                       is_asp_od_pi_book-stock_docno
               data-insptyp                 = is_asp_od_pi_book-insptyp
               data-inspid                  = is_asp_od_pi_book-inspid
               data-idplate                 = is_asp_od_pi_book-idplate
               data-qdoccat                 = is_asp_od_pi_book-qdoccat
               data-qdocid                  = is_asp_od_pi_book-qdocid
               data-qitmid                  = is_asp_od_pi_book-qitmid
               BINARY SEARCH.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    LOOP AT it_book ASSIGNING <ls_items> FROM sy-tabix.
      IF <ls_items>-data-type_item               <> wmegc_lime_type_stock OR
         <ls_items>-data-stock_item-matid        <> is_asp_od_pi_book-matid OR
         <ls_items>-data-stock_item-batchid      <> is_asp_od_pi_book-batchid OR
         <ls_items>-data-stock_item-cat          <> is_asp_od_pi_book-cat OR
         <ls_items>-data-stock_item-entitled     <> is_asp_od_pi_book-entitled OR
         <ls_items>-data-stock_item-owner        <> is_asp_od_pi_book-owner OR
         <ls_items>-data-stock_item-stock_doccat <>
                                       is_asp_od_pi_book-stock_doccat OR
         <ls_items>-data-stock_item-stock_docno  <>
                                       is_asp_od_pi_book-stock_docno OR
         <ls_items>-data-insptyp                 <> is_asp_od_pi_book-insptyp OR
         <ls_items>-data-inspid                  <> is_asp_od_pi_book-inspid OR
         <ls_items>-data-idplate                 <> is_asp_od_pi_book-idplate OR
         <ls_items>-data-qdoccat                 <> is_asp_od_pi_book-qdoccat OR
         <ls_items>-data-qdocid                  <> is_asp_od_pi_book-qdocid OR
         <ls_items>-data-qitmid                  <> is_asp_od_pi_book-qitmid.
        EXIT.
      ENDIF.
      CHECK <ls_items>-t_usid IS NOT INITIAL.
      CLEAR ls_asp_od2_pi_bo.
*     Header data
      ls_asp_od2_pi_bo-pi_lgnum = is_asp_od_pi_book-pi_lgnum.
      ls_asp_od2_pi_bo-guid_doc = is_asp_od_pi_book-guid_doc.
      ls_asp_od2_pi_bo-item_no  = is_asp_od_pi_book-item_no.
*     Transfer general data
      ls_asp_od2_pi_bo-line_idx = is_asp_od_pi_book-line_idx.
      LOOP AT <ls_items>-t_usid ASSIGNING <ls_usid>.
        lv_tabix = lv_tabix + 1.
        ls_asp_od2_pi_bo-ser_seq      = lv_tabix.
        ls_asp_od2_pi_bo-serid        = <ls_usid>-sernr.
        ls_asp_od2_pi_bo-uii          = <ls_usid>-uii.
        APPEND ls_asp_od2_pi_bo      TO et_asp_od2_bo.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od2_book
*----------------------------------------------------------------------*
* METHOD convert_item_read_2_od2_diff                          - public
*----------------------------------------------------------------------*
*   IMPORTING it_diff              TYPE /lime/pi_t_item_sub_read_get_s
*             it_asp_od_pi_di      TYPE /scwm/t_asp_pi_proc_od_diff
*   EXPORTING et_asp_od2_di        TYPE /scwm/t_asp_pi_proc_od2_diff
*----------------------------------------------------------------------*
  METHOD convert_item_read_2_od2_diff.

    DATA:
      lv_tabix         TYPE sy-tabix,
      ls_asp_od2_pi_di TYPE /scwm/s_asp_pi_proc_od2_diff,
      lo_items         TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lo_asp_od_pi_di  TYPE REF TO /scwm/s_asp_pi_proc_od_diff,
      lo_asp_od2_pi_di TYPE REF TO /scwm/s_asp_pi_proc_od2_diff,
      lo_usid          TYPE REF TO /lime/pi_usid_biz.

    CLEAR et_asp_od2_di.
    LOOP AT it_asp_od_pi_di REFERENCE INTO lo_asp_od_pi_di.
      LOOP AT it_diff REFERENCE INTO lo_items
        WHERE data-type_item               = wmegc_lime_type_stock
          AND data-stock_item-matid        = lo_asp_od_pi_di->matid
          AND data-stock_item-batchid      = lo_asp_od_pi_di->batchid
          AND data-stock_item-cat          = lo_asp_od_pi_di->cat
          AND data-stock_item-entitled     = lo_asp_od_pi_di->entitled
          AND data-stock_item-owner        = lo_asp_od_pi_di->owner
          AND data-stock_item-stock_doccat =
                                         lo_asp_od_pi_di->stock_doccat
          AND data-stock_item-stock_docno  =
                                         lo_asp_od_pi_di->stock_docno
          AND data-insptyp                 = lo_asp_od_pi_di->insptyp
          AND data-inspid                  = lo_asp_od_pi_di->inspid
          AND data-idplate                 = lo_asp_od_pi_di->idplate
          AND data-qdoccat                 = lo_asp_od_pi_di->qdoccat
          AND data-qdocid                  = lo_asp_od_pi_di->qdocid
          AND data-qitmid                  = lo_asp_od_pi_di->qitmid.

        CLEAR ls_asp_od2_pi_di.
*       Header data
        ls_asp_od2_pi_di-pi_lgnum      = lo_asp_od_pi_di->pi_lgnum.
        ls_asp_od2_pi_di-guid_doc      = lo_asp_od_pi_di->guid_doc.
        ls_asp_od2_pi_di-item_no       = lo_asp_od_pi_di->item_no.
*----   Transfer general data
        MOVE lo_asp_od_pi_di->line_idx TO ls_asp_od2_pi_di-line_idx.
        LOOP AT lo_items->t_usid REFERENCE INTO lo_usid.
          lv_tabix = lv_tabix + 1.
          ls_asp_od2_pi_di-ser_seq = lv_tabix.
          ls_asp_od2_pi_di-serid = lo_usid->sernr.
          ls_asp_od2_pi_di-uii   = lo_usid->uii.
*         Set sign of the direction
          CASE lo_usid->ser_status.
            WHEN wmegc_lime_post_inbound.
              ls_asp_od2_pi_di-dif_direction = '+'.
            WHEN wmegc_lime_post_outbound.
              ls_asp_od2_pi_di-dif_direction = '-'.
            WHEN OTHERS.

          ENDCASE.
          APPEND ls_asp_od2_pi_di TO et_asp_od2_di.
        ENDLOOP.
      ENDLOOP.
*     Define new SERIAL number sequence
      LOOP AT et_asp_od2_di REFERENCE INTO lo_asp_od2_pi_di.
        lo_asp_od2_pi_di->ser_seq = sy-tabix.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "convert_item_read_2_od2_diff
*----------------------------------------------------------------------*
* METHOD condense_tree                                       - private
*----------------------------------------------------------------------*
*  Remove dummy HU levels from tree
*  Source assumes that hierarchie is correct based on level as
*  passed via LIME in RESULT table.
*  Additional assumptions: Fields neither in move_item structure nor
*  in move_parent structure are filled redundant in table.
*----------------------------------------------------------------------*
*  IMPORTING is_item TYPE /lime/pi_item_read_get_single
*  EXPORTING et_diff TYPE /lime/pi_t_item_sub_read_get_s
*----------------------------------------------------------------------*
  METHOD condense_tree.

    TYPES:
*     All fields which should be used from parent
      BEGIN OF ltys_move_parent,
        lvl              TYPE /lime/tree_level,
        guid_parent      TYPE /lime/guid_parent,
        type_parent      TYPE /lime/pi_type_parent,
        loc_parent       TYPE /lime/loc_biz,
        hu_parent        TYPE /lime/hu_biz,
        ind_loc_par_comp TYPE /lime/pi_de_ind_loc_par_compl,
        ind_loc_par_empt TYPE /lime/pi_de_ind_loc_par_empty,
      END OF ltys_move_parent.

    DATA:
      ls_item        TYPE /lime/pi_item_sub_read_get_sin,
      lo_item        TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lo_item_child  TYPE REF TO /lime/pi_item_sub_read_get_sin,
      lt_item        TYPE /lime/pi_t_item_sub_read_get_s,
      lt_item_in     TYPE /lime/pi_t_item_sub_read_get_s,
      ls_move_parent TYPE ltys_move_parent.

    lt_item_in = is_item-t_difference.
*   Remove dummy HU from tree
    LOOP AT lt_item_in REFERENCE INTO lo_item.
      MOVE lo_item->* TO ls_item.
      IF ls_item-data-hu_item-vhi IS INITIAL.
*       No dummy HU as child -> adopt
        APPEND ls_item TO lt_item.
      ELSE.
*       Child is dummy HU -> find all items of this HU and set parent
*       of current item as new parent for child
        MOVE-CORRESPONDING lo_item->data TO ls_move_parent. "#EC ENHOK
        LOOP AT lt_item_in REFERENCE INTO lo_item_child
                WHERE data-guid_parent = lo_item->data-guid_item.
          CLEAR lo_item_child->data-dif_direction.
          MOVE-CORRESPONDING ls_move_parent
                               TO lo_item_child->data.      "#EC ENHOK
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    et_diff = lt_item.

  ENDMETHOD.                    "condense_tree

ENDCLASS.                    "lcl_sp_mapper IMPLEMENTATION
