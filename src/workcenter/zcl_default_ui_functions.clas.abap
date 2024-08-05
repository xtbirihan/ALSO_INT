CLASS zcl_default_ui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ws_ui_defaults .

    CONSTANTS c_default_screen TYPE sy-dynnr VALUE '0200' ##NO_TEXT.

    METHODS constructor .
  PROTECTED SECTION.

    DATA mo_repl_srv TYPE REF TO zif_wc_replenishment .
    DATA ms_workcenter TYPE /scwm/tworkst .
    DATA ms_defaults TYPE zstr_wc_defaults .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DEFAULT_UI_FUNCTIONS IMPLEMENTATION.


  METHOD constructor.

    mo_repl_srv = NEW zcl_wc_replenishment( ).

  ENDMETHOD.


  METHOD zif_ws_ui_defaults~call_screen.
    CALL FUNCTION 'Z_INT_WC_DEFAULTS_POPUP'.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_defaults.

    IF ms_defaults IS INITIAL.
      mo_repl_srv->get_defaults(
      IMPORTING
        es_defaults = ms_defaults
        es_tworkst  = ms_workcenter ).
    ENDIF.

    IF es_defaults IS REQUESTED.
      MOVE-CORRESPONDING ms_defaults TO es_defaults.
    ENDIF.
  ENDMETHOD.


  METHOD zif_ws_ui_defaults~get_default_screen_no.

    ev_dynnr = c_default_screen.

  ENDMETHOD.


  METHOD zif_ws_ui_defaults~is_defaults_sufficient.

    rv_yes = abap_true.

    IF ms_defaults-lgnum IS INITIAL OR ms_defaults-workcenter IS INITIAL.
      rv_yes = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_ws_ui_defaults~set_defaults.
    DATA ls_defaults TYPE zstr_wc_defaults .
    ls_defaults = CORRESPONDING #( is_defaults ).

    IF ls_defaults-lgnum IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e001(zmc_workstation).
    ENDIF.
    IF ls_defaults-workcenter IS INITIAL.
      RAISE EXCEPTION TYPE zcx_workstation MESSAGE e002(zmc_workstation).
    ENDIF.

    mo_repl_srv->set_defaults(
      EXPORTING
        iv_lgnum      = ls_defaults-lgnum
        iv_workcenter = ls_defaults-workcenter
      IMPORTING
        es_tworkst    = ms_workcenter ).

    ms_defaults = ls_defaults.

  ENDMETHOD.
ENDCLASS.
