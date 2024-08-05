FUNCTION zint_to_cancel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_SUBST) TYPE  /SCWM/RL03TSUBST OPTIONAL
*"     VALUE(IV_QNAME) TYPE  UNAME OPTIONAL
*"     VALUE(IV_HUMI) TYPE  XFELD OPTIONAL
*"     VALUE(IV_DLV_NOT_ASYNC) TYPE  XFELD OPTIONAL
*"     VALUE(IV_UPDATE_TASK) TYPE  /SCWM/RL03AVERBU OPTIONAL
*"     VALUE(IV_COMMIT_WORK) TYPE  /SCWM/RL03ACOMIT OPTIONAL
*"     VALUE(IT_CANCL) TYPE  /SCWM/TT_CANCL
*"     VALUE(IT_CANCL_EXC) TYPE  /SCWM/TT_CONF_EXC OPTIONAL
*"  EXPORTING
*"     VALUE(ET_BAPIRET) TYPE  BAPIRETTAB
*"     VALUE(EV_SEVERITY) TYPE  BAPI_MTYPE
*"----------------------------------------------------------------------


  DATA: lv_severity TYPE bapi_mtype.
  DATA: lt_bapiret TYPE bapirettab.
  DATA: lt_tap TYPE /scwm/tt_ltap_vb.                       "#EC NEEDED

  FIELD-SYMBOLS: <bapiret> TYPE bapiret2.

  CLEAR: et_bapiret, ev_severity.

* central cleanup
  /scwm/cl_tm=>cleanup( EXPORTING iv_lgnum = iv_lgnum ).

  IF iv_dlv_not_async IS NOT INITIAL.
    CALL FUNCTION '/SCWM/TAP_CREATE'
      EXPORTING
        it_tap           = lt_tap
        iv_dlv_not_async = iv_dlv_not_async.
  ENDIF.

  CALL FUNCTION '/SCWM/TO_CANC_INT'
    EXPORTING
      iv_subst     = iv_subst
      iv_qname     = iv_qname
      iv_humi      = iv_humi
      it_cancl     = it_cancl
      it_cancl_exc = it_cancl_exc
    IMPORTING
      et_bapiret   = lt_bapiret
      ev_severity  = ev_severity.

  APPEND LINES OF lt_bapiret TO et_bapiret.

  CALL FUNCTION '/SCWM/TO_POST'
    EXPORTING
      iv_update_task   = iv_update_task
      iv_commit_work   = iv_commit_work
      iv_processor_det = abap_true
    IMPORTING
      et_bapiret       = lt_bapiret
      ev_severity      = ev_severity.

  APPEND LINES OF lt_bapiret TO et_bapiret.

ENDFUNCTION.
