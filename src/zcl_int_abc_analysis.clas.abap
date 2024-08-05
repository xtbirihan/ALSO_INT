CLASS zcl_int_abc_analysis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_abc_line,
             indicator TYPE /scwm/de_abc_indicator,
             mat_p     TYPE /scwm/de_abc_limit_basis_perc,
             mat_a     TYPE /scwm/de_abc_limit_basis_no,
             matpp     TYPE /scwm/de_abc_limit_prod_perc,
             matpa     TYPE /scwm/de_abc_limit_prod_no,
             paci      TYPE /scwm/de_put_stra,
             lgbkz     TYPE /scwm/lvs_lgbkz,
             ccind     TYPE /scwm/pi_ccind,
           END OF ty_abc_line.
    TYPES tty_abc_lines TYPE STANDARD TABLE OF ty_abc_line.

    DATA mv_returncode TYPE sy-subrc READ-ONLY.

    METHODS constructor IMPORTING iv_lgnum     TYPE /scwm/lgnum
                                  it_entit     TYPE table
                                  it_matnr     TYPE table
                                  it_matgr     TYPE table
                                  iv_excl      TYPE /scwm/de_abc_excl_prod_wo_wt
                                  it_conf      TYPE table
                                  it_trart     TYPE table
                                  it_procty    TYPE table
                                  it_vltyp     TYPE table
                                  it_nltyp     TYPE table
                                  it_aunit     TYPE table
                                  iv_basis     TYPE /scwm/de_abc_analysis_basis
                                  iv_class     TYPE /scwm/de_abc_categories_cnt
                                  iv_b_per     TYPE /scwm/de_abc_basis_in_perc
                                  iv_b_abs     TYPE /scwm/de_abc_basis_in_no
                                  iv_pr_per    TYPE /scwm/de_abc_prod_in_perc
                                  iv_pr_abs    TYPE /scwm/de_abc_prod_in_no
                                  iv_paci      TYPE /scwm/de_abc_upd_paci
                                  iv_lgbkz     TYPE /scwm/de_abc_upd_stosecind
                                  iv_ccind     TYPE /scwm/de_upd_ccind
                                  iv_level     TYPE char0001
                                  it_abc_lines TYPE tty_abc_lines
                                  iv_bupd      TYPE /scwm/de_abc_back_proc
                                  it_dispi     TYPE table.
    METHODS call_standard_abc_analysis.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_select_option TYPE TABLE OF rsparams.

    DATA mv_lgnum TYPE /scwm/lgnum.
    DATA mt_entit TYPE RANGE OF /scwm/de_entitled.
    DATA mt_matnr TYPE RANGE OF /scwm/de_matnr.
    DATA mt_matgr TYPE RANGE OF /scmb/de_whmatgr.
    DATA mv_excl TYPE /scwm/de_abc_excl_prod_wo_wt.
    DATA mt_conf TYPE RANGE OF /scwm/de_confirmed_date.
    DATA mt_trart TYPE RANGE OF /scwm/lvs_trart.
    DATA mt_procty TYPE RANGE OF /scwm/de_procty.
    DATA mt_vltyp TYPE RANGE OF /scwm/ltap_vltyp.
    DATA mt_nltyp TYPE RANGE OF /scwm/ltap_nltyp.
    DATA mt_aunit TYPE RANGE OF /scwm/de_aunit.
    DATA mv_basis TYPE /scwm/de_abc_analysis_basis.
    DATA mv_class TYPE /scwm/de_abc_categories_cnt.
    DATA mv_b_per TYPE /scwm/de_abc_basis_in_perc.
    DATA mv_b_abs TYPE /scwm/de_abc_basis_in_no.
    DATA mv_pr_per TYPE /scwm/de_abc_prod_in_perc.
    DATA mv_pr_abs TYPE /scwm/de_abc_prod_in_no.
    DATA mv_paci TYPE /scwm/de_abc_upd_paci.
    DATA mv_lgbkz TYPE /scwm/de_abc_upd_stosecind.
    DATA mv_ccind TYPE /scwm/de_upd_ccind.
    DATA mv_level TYPE char0001.
    DATA mt_abc_lines TYPE tty_abc_lines.
    DATA mv_bupd TYPE /scwm/de_abc_back_proc.
    DATA mt_dispi TYPE RANGE OF zz1_disp.

    METHODS prepare_parameters.
    METHODS apply_custom_selection.
ENDCLASS.



CLASS ZCL_INT_ABC_ANALYSIS IMPLEMENTATION.


  METHOD apply_custom_selection.
**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Applies the custom selection to the material selection before passing the
*& parameter to the standard ABC Report
**********************************************************************
    IF mt_dispi IS INITIAL.
      RETURN.
    ENDIF.

    SELECT mkey~matnr
      INTO TABLE @DATA(lt_material_matches)
      FROM /sapapo/matlwh AS lwh INNER JOIN /sapapo/matkey AS mkey
           ON lwh~matid = mkey~matid
     WHERE mkey~matnr IN @mt_matnr
       AND lwh~zz1_disp_whd IN @mt_dispi.

    IF lt_material_matches IS INITIAL.
      mv_returncode = 1. " No matching data found.
      RETURN.
    ENDIF.

    " Replace material numbers form the selection screen with the selected values
    DELETE mt_select_option WHERE selname = 'P_MATNR'.

    mt_select_option = VALUE #( BASE mt_select_option FOR ls_match IN lt_material_matches ( selname = 'P_MATNR'
                                                                                               kind = 'S'
                                                                                               sign = 'I'
                                                                                             option = 'EQ'
                                                                                                low = ls_match-matnr ) ).

  ENDMETHOD.


  METHOD call_standard_abc_analysis.
**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Passes the parameter and calls the standard SAP ABC Analysis
*&
**********************************************************************
    prepare_parameters(  ).

    IF mv_returncode = 0.
      SUBMIT /scwm/r_abc_analysis WITH SELECTION-TABLE mt_select_option AND RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Constructor
*&
**********************************************************************
    mv_lgnum = iv_lgnum.
    mt_entit = it_entit.
    mt_matnr = it_matnr.
    mt_matgr = it_matgr.
    mv_excl = iv_excl.
    mt_conf = it_conf.
    mt_trart = it_trart.
    mt_procty = it_procty.
    mt_vltyp = it_vltyp.
    mt_nltyp = it_nltyp.
    mt_aunit = it_aunit.
    mv_basis = iv_basis.
    mv_class = iv_class.
    mv_b_per = iv_b_per.
    mv_b_abs = iv_b_abs.
    mv_pr_per = iv_pr_per.
    mv_paci = iv_paci.
    mv_ccind = iv_ccind.
    mv_level = iv_level.
    mt_abc_lines = it_abc_lines.
    mv_bupd = iv_bupd.
    mt_dispi = it_dispi.
  ENDMETHOD.


  METHOD prepare_parameters.
**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Prepares the parameter for the pass through.
*&
**********************************************************************
    REFRESH mt_select_option.

    " Warehouse Number (LGNUM)
    IF mv_lgnum IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_LGNUM'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_lgnum ) ).
    ENDIF.

    " Party Entitled to Dispose(ENTIT)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_entit IN mt_entit ( selname = 'SO_ENTIT'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_entit-sign
                                                                                          option = ls_sel_set_entit-option
                                                                                             low = ls_sel_set_entit-low
                                                                                            high = ls_sel_set_entit-high ) ).

    " Material Number (MATNR)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_matnr IN mt_matnr ( selname = 'P_MATNR'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_matnr-sign
                                                                                          option = ls_sel_set_matnr-option
                                                                                             low = ls_sel_set_matnr-low
                                                                                            high = ls_sel_set_matnr-high ) ).

    " Material Group (MATGR)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_matgr IN mt_matgr ( selname = 'P_MATGR'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_matgr-sign
                                                                                          option = ls_sel_set_matgr-option
                                                                                             low = ls_sel_set_matgr-low
                                                                                            high = ls_sel_set_matgr-high ) ).

    " Exclude Products w/o WTs (EXCL)
    IF mv_excl = abap_true.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_EXCL'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_excl ) ).
    ENDIF.

    " Confirmation Date (CONF)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_conf IN mt_conf ( selname = 'P_CONF'
                                                                                          kind = 'S'
                                                                                          sign = ls_sel_set_conf-sign
                                                                                        option = ls_sel_set_conf-option
                                                                                           low = ls_sel_set_conf-low
                                                                                          high = ls_sel_set_conf-high ) ).

    " Warehouse Process Category (TRART)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_trart IN mt_trart ( selname = 'P_TRART'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_trart-sign
                                                                                          option = ls_sel_set_trart-option
                                                                                             low = ls_sel_set_trart-low
                                                                                            high = ls_sel_set_trart-high ) ).
    " Warehouse Process Type (PROCTY)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_procty IN mt_procty ( selname = 'P_PROCTY'
                                                                                              kind = 'S'
                                                                                              sign = ls_sel_set_procty-sign
                                                                                            option = ls_sel_set_procty-option
                                                                                               low = ls_sel_set_procty-low
                                                                                              high = ls_sel_set_procty-high ) ).
    " Source Storage Type (VLTYP)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_vltyp IN mt_vltyp ( selname = 'P_VLTYP'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_vltyp-sign
                                                                                          option = ls_sel_set_vltyp-option
                                                                                             low = ls_sel_set_vltyp-low
                                                                                            high = ls_sel_set_vltyp-high ) ).

    " Destination Storage Type (NLTYP)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_nltyp IN mt_nltyp ( selname = 'P_NLTYP'
                                                                                            kind = 'S'
                                                                                             sign = ls_sel_set_nltyp-sign
                                                                                          option = ls_sel_set_nltyp-option
                                                                                             low = ls_sel_set_nltyp-low
                                                                                            high = ls_sel_set_nltyp-high ) ).
    " Unit of Measure (AUNIT)
    mt_select_option = VALUE #( BASE mt_select_option FOR ls_sel_set_aunit IN mt_aunit ( selname = 'P_AUNIT'
                                                                                            kind = 'S'
                                                                                            sign = ls_sel_set_aunit-sign
                                                                                          option = ls_sel_set_aunit-option
                                                                                             low = ls_sel_set_aunit-low
                                                                                            high = ls_sel_set_aunit-high ) ).

    " Analysis Basis (BASIS)
    IF mv_basis IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_BASIS'
                                                         kind = 'P'
                                                         sign = 'I'
                                                       option = 'EQ'
                                                          low = mv_basis ) ).
    ENDIF.

    " No of ABC Categories (CLASS)
    IF mv_class IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_CLASS'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_class ) ).
    ENDIF.

    " Basis in % (B_PER)
    IF mv_b_per IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_B_PER'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_b_per ) ).
    ENDIF.

    " Basis Count (B_ABS)
    IF mv_b_abs IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_B_ABS'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_b_abs ) ).
    ENDIF.

    " Products in % (PR_PER)
    IF mv_pr_per IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_PR_PER'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_pr_per ) ).
    ENDIF.

    " Products Count (PR_ABS)
    IF mv_pr_abs IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_PR_ABS'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_pr_abs ) ).
    ENDIF.

    " Update PACI (PACI)
    IF mv_paci IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_PACI'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_paci ) ).
    ENDIF.

    " Update Stosecind (LGBKZ)
    IF mv_lgbkz IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_LGBKZ'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_lgbkz ) ).
    ENDIF.

    " Update CCIND (CCIND)
    IF mv_ccind IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_CCIND'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_ccind ) ).
    ENDIF.

    " StorSecInd Update Level (LEVEL)
    IF mv_level IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_LEVEL'
                                                             kind = 'P'
                                                             sign = 'I'
                                                           option = 'EQ'
                                                              low = mv_level ) ).
    ENDIF.

    " Values for the different ABC Lines
    LOOP AT mt_abc_lines ASSIGNING FIELD-SYMBOL(<abc_line>).
      " !! In this loop { } will be replaced with a specific char to match the specific parameters

      " Indicator ({ })
      IF <abc_line>-indicator IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_{ <abc_line>-indicator }|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-indicator ) ).
      ENDIF.

      " Basis in % (MAT{ }_P)
      IF <abc_line>-mat_p IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_MAT{ <abc_line>-indicator }_P|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-mat_p ) ).
      ENDIF.

      " Basis Count (MAT{ }_A)
      IF <abc_line>-mat_a IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_MAT{ <abc_line>-indicator }_A|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-mat_a ) ).
      ENDIF.

      " Products in % (MAT{ }PP)
      IF <abc_line>-matpp IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_MAT{ <abc_line>-indicator }PP|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-matpp ) ).
      ENDIF.

      " Products Count (MAT{ }PA)
      IF <abc_line>-matpa IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_MAT{ <abc_line>-indicator }PA|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-matpa ) ).
      ENDIF.

      " Update PACI (PACI_{ })
      IF <abc_line>-paci IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_PACI_{ <abc_line>-indicator }|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-paci ) ).
      ENDIF.

      " Update Stosecind (LGBKZ{ })
      IF <abc_line>-lgbkz IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_LGBKZ{ <abc_line>-indicator }|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-lgbkz ) ).
      ENDIF.

      " Update CCIND (CCIND{ })
      IF <abc_line>-ccind IS NOT INITIAL.
        mt_select_option = VALUE #( BASE mt_select_option ( selname = |P_CCIND{ <abc_line>-indicator }|
                                                               kind = 'P'
                                                               sign = 'I'
                                                             option = 'EQ'
                                                                low = <abc_line>-ccind ) ).
      ENDIF.
    ENDLOOP.

    " Update Indicators in Background Processing Mode
    IF mv_bupd IS NOT INITIAL.
      mt_select_option = VALUE #( BASE mt_select_option ( selname = 'P_BUPD'
                                                         kind = 'P'
                                                         sign = 'I'
                                                       option = 'EQ'
                                                          low = mv_bupd ) ).
    ENDIF.


    apply_custom_selection(  ).
  ENDMETHOD.
ENDCLASS.
