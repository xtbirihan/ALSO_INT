interface ZIF_MODIFY_INT_COCKPIT
  public .


  interfaces IF_BADI_INTERFACE .

  methods MODIFY_FCAT
    importing
      value(IV_INT_TYPE) type ZINT_000_DE_ITYPE
      value(IV_INT_SUBTYPE) type ZINT_000_DE_ISTYPE
      !IV_TABNAME type TYPENAME
    changing
      value(CT_FCAT) type LVC_T_FCAT .
  methods MODIFY_TABLE
    importing
      value(IV_INT_TYPE) type ZINT_000_DE_ITYPE
      value(IV_INT_SUBTYPE) type ZINT_000_DE_ISTYPE
      !IV_TABNAME type TYPENAME
    changing
      value(CT_TABLE) type ANY TABLE .
endinterface.
