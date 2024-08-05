FUNCTION z_int_wc_defaults_popup .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ES_DEFAULTS) TYPE  ZSTR_WC_DEFAULTS
*"----------------------------------------------------------------------

  IF go_controller IS BOUND.
    go_def_controller = go_controller.
  ELSE.
    go_def_controller = NEW zcl_default_ui_functions( ).
  ENDIF.

  go_def_controller->get_defaults(
    IMPORTING
      es_defaults = zstr_wc_defaults
  ).
  go_def_controller->get_default_screen_no(
    IMPORTING
      ev_dynnr = DATA(lv_screen)
      ev_repid = DATA(lv_repid)
  ).

  CALL SCREEN lv_screen STARTING AT 10 10.

  es_defaults = zstr_wc_defaults.

ENDFUNCTION.
