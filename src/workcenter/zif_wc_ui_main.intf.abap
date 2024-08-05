interface ZIF_WC_UI_MAIN
  public .


  interfaces ZIF_WS_SUBSCR_UI .
  interfaces ZIF_WS_UI_DEFAULTS .

  constants C_OKCODE_OK type SYUCOMM value 'OK' ##NO_TEXT.
  constants C_OKCODE_CANCEL type SYUCOMM value 'CANCEL' ##NO_TEXT.
  constants C_OKCODE_ENTER type SYUCOMM value 'ENTER' ##NO_TEXT.

  methods DST_PBO
    exporting
      !ES_WC_CHNG_DEST type ZSTR_WC_CHNG_DEST .
  methods GET_STATUS
    exporting
      !EV_STATUS type STRING
      !ET_EXCLUDES type STRING_TABLE .
  methods CHECK_ST_TYP
    importing
      !IV_LGTYP type /SCWM/LGTYP
    raising
      ZCX_WORKSTATION .
  methods GET_TITLE
    exporting
      !EV_TITLE type SYTITLE
      !EV_PARAM type STRING .
  methods PROCESS_USER_COMMAND
    importing
      !IV_UCOMM type SYUCOMM
    exporting
      !ES_BAPIRET type BAPIRET2 .
  methods PAI
    changing
      !CS_REPL_WORKCENTER type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods PBO
    exporting
      !ES_WC_REPL type ZSTR_WC_REPLENISHMENT .
  methods INIT
    exporting
      !EV_DEFAULT_NEEDED type ABAP_BOOL .
  methods GET_MAIN_SCREEN_NO
    returning
      value(RV_DYNNR) type SY-DYNNR .
  methods NEW_POS_PBO
    exporting
      !ES_NEW_POS type ZSTR_WC_CHNG_BIN .
  methods DST_PAI
    importing
      !IS_NEW_DST type ZSTR_WC_CHNG_DEST
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods NEW_POS_PAI
    importing
      !IS_NEW_POS type ZSTR_WC_CHNG_BIN
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods MOD_REPL_PBO
    exporting
      !ES_MOD_REPL type ZSTR_WC_MOD_REPL .
  methods MOD_REPL_PAI
    importing
      !IS_MOD_REPL type ZSTR_WC_MOD_REPL
    changing
      !CS_WC_REPL type ZSTR_WC_REPLENISHMENT
    raising
      ZCX_WORKSTATION .
  methods WITH_TOTE_PLACE
    returning
      value(RV_WITH_TOTE_PLACE) type ABAP_BOOL .
endinterface.
