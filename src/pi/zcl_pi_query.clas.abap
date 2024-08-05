class ZCL_PI_QUERY definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods SELECT_HUS_BY_MATID
    importing
      !IT_MATID type /SCWM/TT_MATID
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !ET_HUHDR type /SCWM/TT_HUHDR_INT
      !ET_HUITM type /SCWM/TT_HUITM_INT .
  class-methods SELECT_MARA_BY_MATNR
    importing
      !IT_MATNR type /SCWM/TT_MATNR_R
    exporting
      !ET_MARA type ZTT_MARA_SEL .
  class-methods SELECT_MARA_BY_MFRNR_MFRPN
    importing
      !IT_MFRNR type RSELOPTION
      !IT_MFRPN type RSELOPTION
    exporting
      !ET_MARA type ZTT_MARA_SEL .
  class-methods SELECT_HUS_GEN
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_MATNR type RSELOPTION optional
      !IT_OWNER type RSELOPTION optional
      !IT_ENTITLED type RSELOPTION optional
      !IT_LGPLA type RSELOPTION optional
      !IT_CAT type RSELOPTION optional
      !IT_HUIDENT type RSELOPTION optional
    exporting
      !ET_HUHDR type /SCWM/TT_HUHDR_INT
      !ET_HUITM type /SCWM/TT_HUITM_INT .
  class-methods SELECT_MARA_BY_MATID
    importing
      !IT_MATID type /SCWM/TT_MATID_R
    exporting
      !ET_MARA type ZTT_MARA_SEL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PI_QUERY IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  endmethod.


  METHOD select_hus_by_matid.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-014 Additional criteria for PI creation"
********************************************************************
*& Description  :
********************************************************************

    BREAK-POINT ID zcg_ui_pi_download.

    DATA: lt_huhdr TYPE /scwm/tt_huhdr_int,
          lt_huitm TYPE  /scwm/tt_huitm_int.

    IF it_matid IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum     = iv_lgnum
        it_matid     = it_matid
        ir_vhi       = VALUE rseloption( ( sign = wmegc_sign_inclusive "select only real HUs
                                           option = wmegc_option_eq
                                           low = wmegc_vhi_real ) )
      IMPORTING
        et_huhdr     = lt_huhdr
        et_huitm     = lt_huitm
      EXCEPTIONS
        wrong_input  = 1
        not_possible = 2
        error        = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lt_huhdr IS NOT INITIAL.
      et_huhdr = lt_huhdr.
      et_huitm = lt_huitm.
    ENDIF.

  ENDMETHOD.


  METHOD select_hus_gen.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-014 Additional criteria for PI creation"
********************************************************************
*& Description  :
********************************************************************

    DATA: lt_huhdr TYPE /scwm/tt_huhdr_int,
          lt_huitm TYPE  /scwm/tt_huitm_int,
          lt_t331  TYPE /scwm/tt_t331.

    BREAK-POINT ID zcg_ui_pi_process.

    CALL FUNCTION '/SCWM/HU_SELECT_GEN'
      EXPORTING
        iv_lgnum     = iv_lgnum
        ir_matnr     = it_matnr
        ir_owner     = it_owner
        ir_entitled  = it_entitled
        ir_lgpla     = it_lgpla
        ir_huident   = it_huident
        ir_vhi       = VALUE rseloption( ( sign = wmegc_sign_inclusive "select only real HUs
                                           option = wmegc_option_eq
                                           low = wmegc_vhi_real ) )
      IMPORTING
        et_huhdr     = lt_huhdr
        et_huitm     = lt_huitm
      EXCEPTIONS
        wrong_input  = 1
        not_possible = 2
        error        = 3
        OTHERS       = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "validate HU number selection
    LOOP AT it_huident ASSIGNING FIELD-SYMBOL(<ls_huident>).
      IF NOT line_exists( lt_huhdr[ huident = CONV /scwm/de_huident( <ls_huident>-low ) ] ).
        RETURN.
      ENDIF.
    ENDLOOP.

    " get mixed storage type for storage type
    " get HU numbers for storage type 3(wmegc_mixbin_one_hu)
    TRY.
        CALL FUNCTION '/SCWM/T331_READ_MULTI'
          EXPORTING
            iv_lgnum = iv_lgnum
            it_lgtyp = VALUE rseloption( FOR GROUPS OF <ls_huhdr> IN lt_huhdr
                                   GROUP BY <ls_huhdr>-lgtyp
                                    ( sign = wmegc_sign_inclusive
                                      option = wmegc_option_eq
                                      low = <ls_huhdr>-lgtyp ) )
          IMPORTING
            et_t331  = lt_t331.

        DELETE lt_t331 WHERE mixbin <> wmegc_mixbin_one_hu.

        IF lt_t331 IS INITIAL.
          RETURN.
        ENDIF.

        DATA(lr_t331) = VALUE rseloption( FOR GROUPS OF <ls_t331> IN lt_t331
                                      GROUP BY <ls_t331>-lgtyp
                                    ( sign = wmegc_sign_inclusive
                                     option = wmegc_option_eq
                                     low = <ls_t331>-lgtyp ) ).

        DELETE lt_huhdr WHERE lgtyp NOT IN lr_t331.

        LOOP AT lt_huitm ASSIGNING FIELD-SYMBOL(<ls_huitm>).
          CHECK NOT line_exists( lt_huhdr[ guid_hu = <ls_huitm>-guid_parent ] ).
          DELETE lt_huitm.
        ENDLOOP.

      CATCH /scwm/cx_core ##NO_HANDLER.
    ENDTRY.

    et_huhdr = lt_huhdr.
    et_huitm = lt_huitm.

  ENDMETHOD.


  METHOD select_mara_by_matid.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-014 Additional criteria for PI creation"
********************************************************************
*& Description  :
********************************************************************

    DATA: lt_mara TYPE ztt_mara_sel.

    BREAK-POINT ID zcg_ui_pi_process.

    IF it_matid IS INITIAL.
      RETURN.
    ENDIF.

    SELECT mfrnr, mfrpn, scm_matid_guid16 AS matid, matnr
      FROM mara
      WHERE scm_matid_guid16 IN @it_matid
      AND mfrnr IS NOT INITIAL
      AND mfrpn IS NOT INITIAL
      INTO TABLE @lt_mara.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    et_mara = lt_mara.

  ENDMETHOD.


  METHOD select_mara_by_matnr.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-014 Additional criteria for PI creation"
********************************************************************
*& Description  :
********************************************************************

    DATA: lt_mara TYPE ztt_mara_sel.

    BREAK-POINT ID zcg_ui_pi_process.
    BREAK-POINT ID zcg_ui_pi_create.

    IF it_matnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT mfrnr, mfrpn, scm_matid_guid16 AS matid, matnr
      FROM mara
      WHERE matnr IN @it_matnr
      AND mfrnr IS NOT INITIAL
      AND mfrpn IS NOT INITIAL
      INTO TABLE @lt_mara.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    et_mara = lt_mara.

  ENDMETHOD.


  METHOD select_mara_by_mfrnr_mfrpn.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-014 Additional criteria for PI creation"
********************************************************************
*& Description  :
********************************************************************

    DATA: lt_mara TYPE ztt_mara_sel.

    BREAK-POINT ID zcg_ui_pi_process.

    SELECT  mfrnr, mfrpn, scm_matid_guid16 AS matid, matnr
      FROM mara
      WHERE mfrpn IN @it_mfrpn
       AND mfrnr IN @it_mfrnr
      AND mfrpn IS NOT INITIAL
      AND mfrnr IS NOT INITIAL
      INTO TABLE @lt_mara.                              "#EC CI_NOFIELD

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_mfrpn ASSIGNING FIELD-SYMBOL(<ls_mfrpn>).
      IF NOT line_exists( lt_mara[ mfrpn = CONV mfrpn( <ls_mfrpn>-low ) ] ).
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT it_mfrnr ASSIGNING FIELD-SYMBOL(<ls_mfrnr>).
      IF NOT line_exists( lt_mara[ mfrnr = CONV mfrnr( <ls_mfrnr>-low ) ] ).
        RETURN.
      ENDIF.
    ENDLOOP.

    et_mara = lt_mara.

  ENDMETHOD.
ENDCLASS.
