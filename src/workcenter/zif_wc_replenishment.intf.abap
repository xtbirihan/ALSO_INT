interface ZIF_WC_REPLENISHMENT
  public .


  constants C_ACTION_WORKSTATION type /SCMB/ESDUS-ACTION value 'ZWORKSTATION' ##NO_TEXT.
  constants C_ELEMENT_WAREHOUSE type /SCMB/ESDUS-ELEMENT value 'WAREHOUSE' ##NO_TEXT.
  constants C_ELEMENT_WORKST_LOC type /SCMB/ESDUS-ELEMENT value 'WORKST_LOC' ##NO_TEXT.

  methods GET_DEFAULTS
    exporting
      !ES_DEFAULTS type ZSTR_WC_DEFAULTS
      !ES_TWORKST type /SCWM/TWORKST .
  methods SET_DEFAULTS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKCENTER type /SCWM/DE_WORKSTATION
    exporting
      !ES_TWORKST type /SCWM/TWORKST
    raising
      ZCX_WORKSTATION .
  methods GET_LGTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
    returning
      value(RS_T301) type /SCWM/S_T301 .
  methods CHECK_ST_TYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
    raising
      ZCX_WORKSTATION .
  methods GET_OPEN_WTS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_NLTYP type /SCWM/LTAP_NLTYP
      !IT_LGPLA type /SCWM/TT_LGPLA
    exporting
      !ET_TO type /SCWM/TT_TO_DET_MON .
  methods READ_BIN_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGPLA type /SCWM/LGPLA
    returning
      value(RS_LAGP) type /SCWM/LAGP
    raising
      ZCX_WORKSTATION .
  methods READ_MAT_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MATID type /SCWM/DE_MATID
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_LGTYP type /SCWM/DE_LGTYP
    exporting
      !ES_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL
      !ES_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM
      !ET_MAT_UOM type /SCWM/TT_MATERIAL_UOM
      !ET_MAT_LGTYP type /SCWM/TT_MATERIAL_LGTYP
    raising
      ZCX_WORKSTATION .
  methods READ_STORAGE_BINS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
      !IV_LPTYP type /SCWM/LVS_LPTYP
    exporting
      !ET_LAGP type /SCWM/TT_LAGP
    raising
      ZCX_WORKSTATION .
  methods RETURN_REM_QTY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKSTATION type /SCWM/DE_WORKSTATION
      !IV_HUIDENT type /SCWM/DE_HUIDENT
    raising
      ZCX_WORKSTATION .
  methods READ_PACK_SPEC
    importing
      !IV_MATID type /SCWM/DE_MATID
    returning
      value(RT_PACKSPEC_CONTENT) type /SCWM/TT_PACKSPEC_NESTED
    raising
      ZCX_WORKSTATION .
  methods READ_HU_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUIDENT type /SCWM/DE_HUIDENT
    exporting
      !ES_HUHDR type /SCWM/S_HUHDR_INT
      !ET_HUITM type /SCWM/TT_HUITM_INT
    raising
      ZCX_WORKSTATION .
  methods CHECK_PROD_EAN_MPN
    importing
      !IV_PROD_EAN_MPN type ZDE_INB_PRODUCT_EAN_MPN
    returning
      value(RT_MATNR_DET) type QMATNR_TAB
    raising
      ZCX_WORKSTATION .
  methods GET_WC_ST_TYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_WORKCENTER type /SCWM/DE_WORKSTATION
    returning
      value(RV_ST_TYP) type /SCWM/LGTYP .
  methods CREATE_WT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_ANFME type /SCWM/DE_UI_VSOLM
      !IV_ALTME type /SCWM/LRMEI
      !IS_WT_CUR type /SCWM/S_TO_DET_MON
    exporting
      !ET_LTAP_VB type /SCWM/TT_LTAP_VB
      !ET_PROTOCOL type BAPIRETTAB
    raising
      ZCX_WORKSTATION .
  methods UPDATE_N_CONFIRM_WT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_TANUM type /SCWM/TANUM
      !IS_CONFIRM type /SCWM/TO_CONF
      !IS_CONF_EXC type /SCWM/S_CONF_EXC
    exporting
      !ET_BAPIRET type BAPIRETTAB
    raising
      ZCX_WORKSTATION .
  methods CHECK_DST_BIN_LPTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_NLTYP type /SCWM/T333_NLTYP
      !IV_DST_LGPLA type /SCWM/LGPLA
    returning
      value(RV_NOT_OK) type ABAP_BOOL
    raising
      ZCX_WORKSTATION .
  methods PACK_N_PRINT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_TANUM type /SCWM/TANUM
      !IS_CONFIRM type /SCWM/TO_CONF
      !IS_CONF_EXC type /SCWM/S_CONF_EXC
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_ANFME type /SCWM/DE_UI_VSOLM
      !IV_ALTME type /SCWM/LRMEI
      !IS_WT_CUR type /SCWM/S_TO_DET_MON
    exporting
      !ET_BAPIRET type BAPIRETTAB
      !ET_LTAP_VB type /SCWM/TT_LTAP_VB
    raising
      ZCX_WORKSTATION .
endinterface.
