**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSLO1
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  METHOD pi_document_create                                  - private
*----------------------------------------------------------------------*
*    IMPORTING
*      iv_sim_mode        TYPE c
*    RETURNING
*      value(ev_rejected) TYPE boole_d.
*----------------------------------------------------------------------*

  METHOD pi_document_create.

    DATA:
      l_rc_severity    TYPE bapi_mtype,
      lv_category      TYPE /lime/pi_category,
      ls_head          TYPE /lime/pi_head_create,
      ls_item          TYPE /lime/pi_item_create,
      lv_item_no       TYPE /lime/line_item_id,
      lt_asp_oi_pi_cr  TYPE /scwm/t_asp_oi_pi_item_create,
      ls_aspk_oi_pi_cr TYPE /scwm/s_aspk_oi_pi_item_create,
      lt_item          TYPE /lime/pi_t_item_create,
      lv_timezone      TYPE tznzone,
      lt_bapiret       TYPE bapiret2_t,
      ls_msg           TYPE symsg,
      ls_reference     TYPE /lime/pi_reference,
      lt_reference     TYPE /lime/pi_t_reference,
      ls_pi_doc        TYPE LINE OF /lime/pi_t_item_read,
      lt_pi_doc        TYPE /lime/pi_t_item_read.

    FIELD-SYMBOLS:
      <lo_asp_oi_pi_cr> TYPE /scwm/s_asp_oi_pi_item_create.

    BREAK-POINT ID zcg_ui_pi_process.

    CLEAR ev_rejected.

*   Get data from buffer
    CLEAR lt_asp_oi_pi_cr.
    lt_asp_oi_pi_cr = lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->get( ).

*   Get data from first line to create header
    READ TABLE lt_asp_oi_pi_cr ASSIGNING <lo_asp_oi_pi_cr> INDEX 1.
    IF sy-subrc = 0.
      CLEAR ls_head.
      ls_head-process_type = wmegc_scwm.
      ls_head-lgnum        = <lo_asp_oi_pi_cr>-lgnum.
      ls_head-active       = limpi_doc_active.
    ENDIF.

*   Get data for PI document items
    CLEAR lt_item.
    CLEAR lv_item_no.
    LOOP AT lt_asp_oi_pi_cr ASSIGNING <lo_asp_oi_pi_cr>.
      CLEAR ls_item.

      CHECK <lo_asp_oi_pi_cr>-status = lcl_sp=>sv_icon_led_green.

      lv_item_no              = lv_item_no + 1.
      ls_item-data-item_no    = lv_item_no.
      ls_item-data-doc_type   = <lo_asp_oi_pi_cr>-doc_type.
      ls_item-data-block_ind  = <lo_asp_oi_pi_cr>-block_ind.
      ls_item-data-freeze_ind = <lo_asp_oi_pi_cr>-freeze_ind.

*     Check if reason is filled
      IF <lo_asp_oi_pi_cr>-reason IS INITIAL.
        MESSAGE e018 WITH ls_item-data-item_no INTO ls_msg.
        MOVE-CORRESPONDING <lo_asp_oi_pi_cr> TO ls_aspk_oi_pi_cr.
        MOVE-CORRESPONDING syst TO ls_msg.
        lcl_sp=>so_sp_fw->mo_message_handler->add_message(
          msg        = ls_msg
          aspect     = lif_sp_c=>sc_asp_oi_pi_cr
          aspect_key = ls_aspk_oi_pi_cr ).
        ev_rejected = abap_true.
        EXIT.
      ELSE.
        TRY.
*           Check if entered reason exists
            lcl_sp=>so_sp_fw->mo_scwm_cust->get_reason(
              i_lgnum  = ls_head-lgnum
              i_reason = <lo_asp_oi_pi_cr>-reason ).
          CATCH /scwm/cx_pi_app.
            MOVE-CORRESPONDING <lo_asp_oi_pi_cr> TO ls_aspk_oi_pi_cr.
            MOVE-CORRESPONDING syst TO ls_msg.
            lcl_sp=>so_sp_fw->mo_message_handler->add_message(
              msg        = ls_msg
              aspect     = lif_sp_c=>sc_asp_oi_pi_cr
              aspect_key = ls_aspk_oi_pi_cr ).
            ev_rejected = abap_true.
            EXIT.
        ENDTRY.
      ENDIF.
      ls_item-data-priority   = <lo_asp_oi_pi_cr>-priority.
      ls_item-data-reason     = <lo_asp_oi_pi_cr>-reason.
*     Set create ACTIVE - INACTIVE doc
      IF <lo_asp_oi_pi_cr>-active IS INITIAL.
        ls_item-inactive = abap_true.
        ls_item-active   = abap_false.
      ELSE.
        ls_item-active   = abap_true.
        ls_item-inactive = abap_false.
      ENDIF.
      lv_timezone = lcl_sp=>so_sp_fw->mo_sp_services->get_time_zone(
        <lo_asp_oi_pi_cr>-lgnum ).
      CONVERT DATE <lo_asp_oi_pi_cr>-count_date_ui
              TIME <lo_asp_oi_pi_cr>-count_time_ui
         INTO TIME STAMP ls_item-data-count_date
         TIME ZONE lv_timezone.

*     Determin the category based on the document type
      CLEAR lv_category.
      lv_category = lcl_sp=>so_sp_fw->mo_lime_cust->get_pi_doc_cat(
        ls_item-data-doc_type ).

*     Loc_parent is filled for location- and object-related PI
*     Fill business keys as we have no guid in case of insert
      ls_item-data-type_parent      = wmegc_lime_type_loc.
      ls_item-data-loc_parent-lgnum = <lo_asp_oi_pi_cr>-lgnum.
      ls_item-data-loc_parent-lgtyp = <lo_asp_oi_pi_cr>-lgtyp.
      ls_item-data-loc_parent-lgpla = <lo_asp_oi_pi_cr>-lgpla.

*     Stock data for object-related PI
      IF lv_category = limpi_object.
*       Last checks before save: necessary here as we have the external
*       key for matnr and charg (next layer has only guids)
        TRY.
            lcl_sp=>so_sp_fw->mo_stock_id->check_matnr(
                                       <lo_asp_oi_pi_cr>-matnr ).
            lcl_sp=>so_sp_fw->mo_stock_id->check_charg(
              iv_matnr    = <lo_asp_oi_pi_cr>-matnr
              iv_charg    = <lo_asp_oi_pi_cr>-charg
              iv_entitled = <lo_asp_oi_pi_cr>-entitled ).
          CATCH /scwm/cx_core.
            MOVE-CORRESPONDING <lo_asp_oi_pi_cr> TO ls_aspk_oi_pi_cr.
            MOVE-CORRESPONDING syst TO ls_msg.
            lcl_sp=>so_sp_fw->mo_message_handler->add_message(
              msg        = ls_msg
              aspect     = lif_sp_c=>sc_asp_oi_pi_cr
              aspect_key = ls_aspk_oi_pi_cr ).
            ev_rejected = abap_true.
            EXIT.
        ENDTRY.

*       fill business keys as we have no guid in case of insert
*       define stock item with stock guid and index
        ls_item-data-type_item              = wmegc_lime_type_stock.
        ls_item-data-stock_item-lgnum_stock =
                                    <lo_asp_oi_pi_cr>-lgnum.
        ls_item-data-stock_item-matid   = <lo_asp_oi_pi_cr>-matid.
        ls_item-data-stock_item-batchid = <lo_asp_oi_pi_cr>-batchid.
        ls_item-data-stock_item-owner   = <lo_asp_oi_pi_cr>-owner.
        ls_item-data-stock_item-owner_role =
                                    <lo_asp_oi_pi_cr>-owner_role.
        ls_item-data-stock_item-entitled =
                                    <lo_asp_oi_pi_cr>-entitled.
        ls_item-data-stock_item-entitled_role =
                                    <lo_asp_oi_pi_cr>-entitled_role.
        ls_item-data-stock_item-cat     = <lo_asp_oi_pi_cr>-cat.
        ls_item-data-stock_item-stock_usage  =
                                    <lo_asp_oi_pi_cr>-stock_usage.
        ls_item-data-stock_item-stock_doccat  =
                                    <lo_asp_oi_pi_cr>-stock_doccat.
        ls_item-data-stock_item-stock_docno  =
                                    <lo_asp_oi_pi_cr>-stock_docno.
        ls_item-data-stock_item-stock_itmno  =
                                    <lo_asp_oi_pi_cr>-stock_itmno.
        ls_item-data-stock_item-doccat  =
                                    <lo_asp_oi_pi_cr>-stref_doccat.

*       /SCWM/QUAN fields
        ls_item-data-wdatu      =  <lo_asp_oi_pi_cr>-wdatu.
        ls_item-data-vfdat      =  <lo_asp_oi_pi_cr>-vfdat.
        ls_item-data-coo        =  <lo_asp_oi_pi_cr>-coo.
        ls_item-data-idplate    =  <lo_asp_oi_pi_cr>-idplate.
        ls_item-data-qdoccat    =  <lo_asp_oi_pi_cr>-qdoccat.
        ls_item-data-qdocid     =  <lo_asp_oi_pi_cr>-qdocid.
        ls_item-data-qitmid     =  <lo_asp_oi_pi_cr>-qitmid.
        ls_item-data-insptyp    =  <lo_asp_oi_pi_cr>-insptyp.
        ls_item-data-inspid     =  <lo_asp_oi_pi_cr>-inspid.

        ls_item-data-zz_mfrnr = <lo_asp_oi_pi_cr>-zz_mfrnr.
        ls_item-data-zz_mfrpn = <lo_asp_oi_pi_cr>-zz_mfrpn.
        ls_item-data-zz_huident = <lo_asp_oi_pi_cr>-zz_huident.

      ENDIF. "lv_category = limpi_object

*     Fill UI reference
      IF NOT <lo_asp_oi_pi_cr>-reference_ui IS INITIAL.
        CLEAR: ls_reference, lt_reference.
        ls_reference-ref_doc_id   = <lo_asp_oi_pi_cr>-reference_ui.
        ls_reference-ref_doc_type = wmegc_ref_pi_ui.
        APPEND ls_reference       TO lt_reference.
        ls_item-t_reference       = lt_reference.
      ENDIF.

      APPEND ls_item TO lt_item.
    ENDLOOP.

    IF ev_rejected IS NOT INITIAL.
      RETURN.
    ENDIF.
*   Call PI core to simulate/execute creation of the PI document
    CALL FUNCTION '/SCWM/PI_CALL_DOCUMENT_CREATE'
      EXPORTING
        is_head       = ls_head
        it_item       = lt_item
        i_sim_mode    = iv_sim_mode
      IMPORTING
        et_pi_doc     = lt_pi_doc
        et_bapiret    = lt_bapiret
        e_rc_severity = l_rc_severity.

    IF NOT lt_bapiret IS INITIAL.
*     Log Messages
      CALL METHOD lcl_sp=>log_bapiret
        EXPORTING
          it_bapiret = lt_bapiret.
    ENDIF.

    IF l_rc_severity CA 'AE'.
      ev_rejected = abap_true.
    ENDIF.

    READ TABLE lt_pi_doc INTO ls_pi_doc INDEX 1.
    LOOP AT lt_pi_doc TRANSPORTING NO FIELDS
      WHERE doc_number <> ls_pi_doc-doc_number.
      CLEAR ls_pi_doc.
      EXIT.
    ENDLOOP.
    SET PARAMETER ID '/SCWM/DOC_NUMBER'
               FIELD ls_pi_doc-doc_number."proposed for counting


  ENDMETHOD.
