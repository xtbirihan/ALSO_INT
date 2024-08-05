**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Custom ABC Analysis
*& This Program is a wrapper for /SCWM/R_ABC_ANALYSIS enhanced by
**********************************************************************
REPORT zint_abc_analysis MESSAGE-ID /scwm/abc_analysis.

DATA gt_abc_lines TYPE TABLE OF zcl_int_abc_analysis=>ty_abc_line.

INCLUDE zint_abc_analysis_sel.

START-OF-SELECTION.

  gt_abc_lines = VALUE #( ( indicator = 'A' mat_p = p_mata_p mat_a = p_mata_a matpp = p_matapp matpa = p_matapa paci = p_paci_a lgbkz = p_lgbkza ccind = p_ccinda  )
                          ( indicator = 'B' mat_p = p_matb_p mat_a = p_matb_a matpp = p_matbpp matpa = p_matbpa paci = p_paci_b lgbkz = p_lgbkzb ccind = p_ccindb  )
                          ( indicator = 'C' mat_p = p_matc_p mat_a = p_matc_a matpp = p_matcpp matpa = p_matcpa paci = p_paci_c lgbkz = p_lgbkzc ccind = p_ccindc  )
                          ( indicator = 'D' mat_p = p_matd_p mat_a = p_matd_a matpp = p_matdpp matpa = p_matdpa paci = p_paci_d lgbkz = p_lgbkzd ccind = p_ccindd  )
                          ( indicator = 'E' mat_p = p_mate_p mat_a = p_mate_a matpp = p_matepp matpa = p_matepa paci = p_paci_e lgbkz = p_lgbkze ccind = p_ccinde  )
                          ( indicator = 'F' mat_p = p_matf_p mat_a = p_matf_a matpp = p_matfpp matpa = p_matfpa paci = p_paci_f lgbkz = p_lgbkzf ccind = p_ccindf  )
                          ( indicator = 'G' mat_p = p_matg_p mat_a = p_matg_a matpp = p_matgpp matpa = p_matgpa paci = p_paci_g lgbkz = p_lgbkzg ccind = p_ccindg  )
                          ( indicator = 'H' mat_p = p_math_p mat_a = p_math_a matpp = p_mathpp matpa = p_mathpa paci = p_paci_h lgbkz = p_lgbkzh ccind = p_ccindh  )
                          ( indicator = 'I' mat_p = p_mati_p mat_a = p_mati_a matpp = p_matipp matpa = p_matipa paci = p_paci_i lgbkz = p_lgbkzi ccind = p_ccindi  )
                          ( indicator = 'J' mat_p = p_matj_p mat_a = p_matj_a matpp = p_matjpp matpa = p_matjpa paci = p_paci_j lgbkz = p_lgbkzj ccind = p_ccindj  )
                          ( indicator = 'K' mat_p = p_matk_p mat_a = p_matk_a matpp = p_matkpp matpa = p_matkpa paci = p_paci_k lgbkz = p_lgbkzk ccind = p_ccindk  )
                          ( indicator = 'L' mat_p = p_matl_p mat_a = p_matl_a matpp = p_matlpp matpa = p_matlpa paci = p_paci_l lgbkz = p_lgbkzl ccind = p_ccindl  )
                        ).

  DATA(lo_abc_analysis) = NEW zcl_int_abc_analysis( iv_lgnum = p_lgnum
                                                    it_entit = so_entit[]
                                                    it_matnr = p_matnr[]
                                                    it_matgr = p_matgr[]
                                                    iv_excl = p_excl
                                                    it_conf = p_conf[]
                                                    it_trart = p_trart[]
                                                    it_procty = p_procty[]
                                                    it_vltyp = p_vltyp[]
                                                    it_nltyp = p_nltyp[]
                                                    it_aunit = p_aunit[]
                                                    iv_basis = p_basis
                                                    iv_class = p_class
                                                    iv_b_per = p_b_per
                                                    iv_b_abs = p_b_abs
                                                    iv_pr_per = p_pr_per
                                                    iv_pr_abs = p_pr_abs
                                                    iv_paci = p_paci
                                                    iv_lgbkz = p_lgbkz
                                                    iv_ccind = p_ccind
                                                    iv_level = p_level
                                                    it_abc_lines = gt_abc_lines
                                                    iv_bupd = p_bupd
                                                    it_dispi = p_dispi[] ).

  lo_abc_analysis->call_standard_abc_analysis(  ).

  IF lo_abc_analysis->mv_returncode <> 0.
    MESSAGE e012.
  ENDIF.
