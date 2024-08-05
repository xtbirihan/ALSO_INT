**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSLOI
*&---------------------------------------------------------------------*
CLASS lcl_sp_locking IMPLEMENTATION.
*----------------------------------------------------------------------*
*  METHOD before_save.                                         - public
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected TYPE boole_d
*----------------------------------------------------------------------*
  METHOD before_save.

    DATA:
      lt_asp_oi_pi_cr TYPE /scwm/t_asp_oi_pi_item_create.

    lt_asp_oi_pi_cr = lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->get( ).

    IF NOT lt_asp_oi_pi_cr IS INITIAL.
      gv_create_dynbin = check_dynbin_create( lt_asp_oi_pi_cr ).
      IF  gv_create_dynbin = abap_false.
        ev_rejected = before_save_create( ).
      ENDIF.
    ELSE.
      ev_rejected = before_save_process( ).
    ENDIF.

  ENDMETHOD.                    "before_save
*----------------------------------------------------------------------*
*  METHOD save.                                                - public
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected  TYPE boole_d
*----------------------------------------------------------------------*
  METHOD save.

    DATA:
      lt_asp_oi_pi_cr TYPE /scwm/t_asp_oi_pi_item_create.

    lt_asp_oi_pi_cr = lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->get( ).
    IF NOT lt_asp_oi_pi_cr IS INITIAL.
      IF gv_create_dynbin = abap_true.
        dynbin_create( it_asp_oi_pi_cr = lt_asp_oi_pi_cr ).
      ELSE.
        ev_rejected = save_create( ).
      ENDIF.
    ELSE.
      ev_rejected = save_process( ).
    ENDIF.

  ENDMETHOD.                    "save
*----------------------------------------------------------------------*
*  METHOD cleanup.                                             - public
*----------------------------------------------------------------------*
*    IMPORTING
*      iv_reason  TYPE string
*----------------------------------------------------------------------*
  METHOD cleanup.

    DATA:
      lv_lgnum   TYPE /scwm/lgnum,
      ls_default TYPE /scwm/s_aspd_pi.
    DATA:
      lo_pi_pm   TYPE REF TO /scwm/cl_pi_pm.

    CASE iv_reason.
      WHEN /scmb/if_sp_transaction=>sc_cleanup_commit.
*       Get warehouse
        lv_lgnum = lcl_sp=>so_sp_fw->mo_sp_query->mv_lgnum.
*       Clear all buffer tables
        lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->commit_refresh( ).
      WHEN OTHERS.
*       Get default warehouse
        /scmb/cl_base=>get_default_values(
          IMPORTING
            es_values = ls_default ).
        lv_lgnum = ls_default-lgnum.
*       Clear all buffer tables
        lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->refresh( ).
    ENDCASE.

    lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_pr->refresh( ).
    lcl_sp=>so_sp_fw->mo_sp_aspect_od_pi_co->refresh( ).
    lcl_sp=>so_sp_fw->mo_sp_aspect_od_pi_bo->refresh( ).
    lcl_sp=>so_sp_fw->mo_sp_aspect_od_pi_di->refresh( ).
    lcl_sp=>so_sp_fw->mo_sp_aspect_od2_pi_co->refresh( ).
    /scwm/cl_pi_ui_appl=>init( ).
*   initialize save Packing Materials for new HUs
    lo_pi_pm = /scwm/cl_pi_pm=>get_instance( ).
    lo_pi_pm->init( ).
*   clear bin buffer
    CALL FUNCTION '/SCWM/BINFUNC_INIT'.
*   Register to clean up WT
    /scwm/cl_tm=>register_cleanup_wt( ).
*   Central cleanup
    /scwm/cl_tm=>cleanup(
      EXPORTING
        iv_reason = iv_reason
        iv_lgnum  = lv_lgnum ).

  ENDMETHOD.                    "clean_up
*----------------------------------------------------------------------*
*  METHOD before_save_create                                  - private
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected  TYPE boole_d
*----------------------------------------------------------------------*
  METHOD before_save_create.

*   Call function module /lime/pi_document_create in simulation mode
    ev_rejected = pi_document_create( iv_sim_mode = 'X' ).

  ENDMETHOD.                    "before_save
*----------------------------------------------------------------------*
*  METHOD before_save_process                                 - private
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected  TYPE boole_d
*----------------------------------------------------------------------*
  METHOD before_save_process.

    ev_rejected = build_process_packages( iv_sim_mode = 'X' ).

  ENDMETHOD.                    "before_save_process
*----------------------------------------------------------------------*
*  METHOD save_create                                         - private
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected  TYPE boole_d
*----------------------------------------------------------------------*
  METHOD save_create.

    DATA:
      lt_asp_oi_pi_cr  TYPE /scwm/t_asp_oi_pi_item_create,
      lo_asp_oi_pi_cr  TYPE REF TO /scwm/s_asp_oi_pi_item_create,
      lt_aspk_oi_pi_cr TYPE /scwm/t_aspk_oi_pi_item_create,
      ls_aspk_oi_pi_cr TYPE /scwm/s_aspk_oi_pi_item_create.

*   Call function module /lime/pi_document_create in posting mode
    ev_rejected = pi_document_create( iv_sim_mode = space ).

    IF ev_rejected = abap_true.

*     Reset status in case of ROLLBACK
      lt_asp_oi_pi_cr
        = lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->get( ).
      LOOP AT lt_asp_oi_pi_cr REFERENCE INTO lo_asp_oi_pi_cr.
        ls_aspk_oi_pi_cr-guid_doc = lo_asp_oi_pi_cr->guid_doc.
        APPEND ls_aspk_oi_pi_cr TO lt_aspk_oi_pi_cr.
      ENDLOOP.

      lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_cr->set_status(
        iv_action        = lif_sp_c=>sc_act_cr_cancel
        it_aspk_oi_pi_cr = lt_aspk_oi_pi_cr ).

    ENDIF.

  ENDMETHOD.                    "save_create
*----------------------------------------------------------------------*
*  METHOD save_process                                        - private
*----------------------------------------------------------------------*
*    EXPORTING
*      ev_rejected  TYPE boole_d
*----------------------------------------------------------------------*
  METHOD save_process.

    ev_rejected = build_process_packages( iv_sim_mode = space ).

  ENDMETHOD.                    "save_process
*----------------------------------------------------------------------*
*  METHOD build_process_packages                              - private
*----------------------------------------------------------------------*
*    IMPORTING
*      iv_sim_mode        TYPE c
*    RETURNING
*      value(ev_rejected) TYPE boole_d.
*----------------------------------------------------------------------*
  METHOD build_process_packages.

    DATA:
      lv_mlen                   TYPE i,
      lv_rc_severity            TYPE bapi_mtype,
      lo_pack                   TYPE REF TO /scwm/cl_wm_packing,
      ls_doc_ref                TYPE /lime/pi_doc_ref,
      ls_who                    TYPE LINE OF /scwm/tt_whoid,
      ls_aspk_oi_pi_pr          TYPE /scwm/s_aspk_oi_pi_item_proces,
      ls_item_read              TYPE /lime/pi_item_read_get_single,
      lt_doc_ref                TYPE STANDARD TABLE OF /lime/pi_doc_ref,
      lt_who                    TYPE /scwm/tt_whoid,
      lt_bapiret                TYPE bapiret2_t,
      lt_asp_oi_pi_pr           TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_count     TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_post      TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_delete    TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_recount   TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_change    TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_print     TYPE /scwm/t_asp_oi_pi_item_proces,
      lt_asp_oi_pi_pr_status_ch TYPE /scwm/t_asp_oi_pi_item_proces.

    FIELD-SYMBOLS:
      <ls_logitem>      TYPE /lime/pi_doc_logitem,
      <lo_asp_oi_pi_pr> TYPE /scwm/s_asp_oi_pi_item_proces.

*   Get data from buffer
    lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_pr->get(
      IMPORTING
        et_asp_oi_pi_pr = lt_asp_oi_pi_pr ).
    lv_mlen = strlen( lcl_sp=>so_sp_fw->mo_sp_query->mv_lgnum ).
*   create packages depending on activity
    LOOP AT lt_asp_oi_pi_pr ASSIGNING <lo_asp_oi_pi_pr>
      WHERE pr_mode = lcl_sp=>sv_icon_change.
*     prepare data for WHO update
      ls_doc_ref-doc_number = <lo_asp_oi_pi_pr>-doc_number.
      ls_doc_ref-doc_year   = <lo_asp_oi_pi_pr>-doc_year.
      ls_doc_ref-item_no    = <lo_asp_oi_pi_pr>-item_no.

      CASE <lo_asp_oi_pi_pr>-function_mode.
        WHEN limpi_doc_cou.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_count.
          APPEND ls_doc_ref        TO lt_doc_ref.
        WHEN limpi_doc_pos.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_post.
          APPEND ls_doc_ref        TO lt_doc_ref.
        WHEN limpi_doc_del.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_delete.
          APPEND ls_doc_ref        TO lt_doc_ref.
        WHEN limpi_doc_rec.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_recount.
          APPEND ls_doc_ref        TO lt_doc_ref.
        WHEN limpi_doc_cha.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_change.
          APPEND ls_doc_ref        TO lt_doc_ref.
        WHEN limpi_doc_prn.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_print.
        WHEN  limpi_doc_ina OR
              limpi_doc_act.
          APPEND <lo_asp_oi_pi_pr> TO lt_asp_oi_pi_pr_status_ch.
          IF <lo_asp_oi_pi_pr>-function_mode = limpi_doc_ina.
*           get WHO reference
            ls_aspk_oi_pi_pr-guid_doc = <lo_asp_oi_pi_pr>-guid_doc.
            ls_aspk_oi_pi_pr-item_no  = <lo_asp_oi_pi_pr>-item_no.
            lcl_sp=>so_sp_fw->mo_sp_aspect_oi_pi_pr->get_query_result(
              EXPORTING
                is_aspk_oi_pi_pr = ls_aspk_oi_pi_pr
              IMPORTING
                es_item          = ls_item_read ).
            CHECK ls_item_read IS NOT INITIAL.
            READ TABLE ls_item_read-t_logitem ASSIGNING <ls_logitem>
              WITH KEY ref_doc_type = wmegc_ref_who.
            CHECK sy-subrc = 0.
            ls_who-who     = <ls_logitem>-ref_doc_id+lv_mlen.
            APPEND ls_who TO lt_who.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
*   prepare call for service: set block stock
    /scwm/cl_wm_packing=>get_instance(
      IMPORTING
        eo_instance = lo_pack ).

    lo_pack->init( iv_lgnum = lcl_sp=>so_sp_fw->mo_sp_query->mv_lgnum ).

    IF NOT lt_asp_oi_pi_pr_count IS INITIAL.
*     call FM /lime/pi_document_count in simulation/execution mode
      ev_rejected = pi_document_count(
        it_items_count = lt_asp_oi_pi_pr_count
        iv_sim_mode    = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_post IS INITIAL.
*     call FM /lime/pi_document_post in simulation/execution mode
      ev_rejected = pi_document_post(
        it_items_post = lt_asp_oi_pi_pr_post
        iv_sim_mode   = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_delete IS INITIAL.
*     call FM /lime/pi_document_delete in simulation/execution mode
      ev_rejected = pi_document_delete(
        it_items_delete = lt_asp_oi_pi_pr_delete
        iv_sim_mode     = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_recount IS INITIAL.
*     call FM /lime/pi_document_recount in simulation/execution mode
      ev_rejected = pi_document_recount(
        it_items_recount = lt_asp_oi_pi_pr_recount
        iv_sim_mode      = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_change IS INITIAL.
*     call FM /lime/pi_document_change in simulation/execution mode
      ev_rejected = pi_document_change(
        it_items_change = lt_asp_oi_pi_pr_change
        iv_sim_mode     = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_print IS INITIAL.
*     call FM /lime/pi_document_print in simulation/execution mode
      ev_rejected = pi_document_print(
        it_items_print = lt_asp_oi_pi_pr_print
        iv_sim_mode    = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    IF NOT lt_asp_oi_pi_pr_status_ch IS INITIAL.
*     call FM /LIME/PI_CHANGE_CREATE_STATUS in simulation/execution mode
      ev_rejected = pi_change_status(
        it_items    = lt_asp_oi_pi_pr_status_ch
        iv_sim_mode = iv_sim_mode ).
      IF ev_rejected <> abap_false.
        RETURN.
      ENDIF.
    ENDIF.
    SORT lt_who.
    DELETE ADJACENT DUPLICATES FROM lt_who.
    IF iv_sim_mode IS INITIAL.
      lo_pack->save(
        iv_commit = space
        iv_wait   = space ).
      CALL FUNCTION '/SCWM/PI_WHO_MAINBIN_UPDATE' IN BACKGROUND TASK
        EXPORTING
          iv_lgnum        = lcl_sp=>so_sp_fw->mo_sp_query->mv_lgnum
          it_item         = lt_doc_ref
          it_who          = lt_who
          iv_confirmed_by = sy-uname.
    ELSE.
      CALL FUNCTION '/SCWM/PI_WHO_MAINBIN_UPDATE'
        EXPORTING
          iv_lgnum        = lcl_sp=>so_sp_fw->mo_sp_query->mv_lgnum
          it_item         = lt_doc_ref
          it_who          = lt_who
          iv_sim          = iv_sim_mode
          iv_confirmed_by = sy-uname
        IMPORTING
          et_bapiret      = lt_bapiret
          e_rc_severity   = lv_rc_severity.

      IF NOT lt_bapiret IS INITIAL.
*       Log Messages
        CALL METHOD lcl_sp=>log_bapiret
          EXPORTING
            it_bapiret = lt_bapiret.
      ENDIF.
      IF lv_rc_severity CA wmegc_severity_eax.
        ev_rejected = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "build_process_packages
*----------------------------------------------------------------------*
*  METHOD pi_document_create
*----------------------------------------------------------------------*
  INCLUDE zlui_pi_processlo1.
*----------------------------------------------------------------------*
*  METHOD pi_document_count
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo2.
*----------------------------------------------------------------------*
*  METHOD pi_document_post
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo3.
*----------------------------------------------------------------------*
*  METHOD pi_document_delete
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo4.
*----------------------------------------------------------------------*
*  METHOD pi_document_recount
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo5.
*----------------------------------------------------------------------*
*  METHOD pi_document_change
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo6.
*----------------------------------------------------------------------*
*  METHOD pi_document_print
*  METHOD pi_change_status
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo7.
*----------------------------------------------------------------------*
*  METHOD pi_document_count_fill
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo8.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  METHOD dynbin_create
*----------------------------------------------------------------------*
  INCLUDE /scwm/lui_pi_processlo9.
*----------------------------------------------------------------------*
ENDCLASS.                    "lcl_sp_locking
