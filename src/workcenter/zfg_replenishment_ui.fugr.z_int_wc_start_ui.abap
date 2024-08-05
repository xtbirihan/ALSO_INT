FUNCTION z_int_wc_start_ui .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_CONTROLLER) TYPE REF TO  ZIF_WC_UI_MAIN
*"----------------------------------------------------------------------

  go_controller = io_controller.

  go_controller->init(
    IMPORTING
      ev_default_needed = DATA(lv_default_needed) ).

  go_def_controller = go_controller.

  IF lv_default_needed EQ abap_true.
    go_def_controller->get_defaults(
      IMPORTING
        es_defaults = zstr_wc_defaults
    ).
    CALL FUNCTION 'Z_INT_WC_DEFAULTS_POPUP'.

    IF NOT go_def_controller->is_defaults_sufficient( ).
      RETURN.
    ENDIF.
  ENDIF.

  IF go_controller->with_tote_place( ).
    CALL FUNCTION 'Z_INB_START_TABBED_UI'
      EXPORTING
        io_controller = io_controller                 " Workstation UI Main Screen
        it_tabs       = VALUE zif_ws_ui_main=>tt_tabs( ( zcl_ws_main=>c_tab_repl_ui )
                                                       ( zcl_ws_main=>c_tab_tote_place )
                                                     ).
  ELSE.
    DATA(lv_main_screen) = go_controller->get_main_screen_no( ).
    CALL SCREEN lv_main_screen.
  ENDIF.

ENDFUNCTION.
