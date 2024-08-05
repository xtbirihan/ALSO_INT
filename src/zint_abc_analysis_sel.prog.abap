**********************************************************************
*& Key           : AD-230509
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Include for Selection Screen of Custom ABC Analysis
*&
**********************************************************************
 CONSTANTS: BEGIN OF gc_default ##NEEDED,
              basis_perc_a TYPE /scwm/de_abc_limit_basis_perc VALUE '80',
              prod_perc_a  TYPE /scwm/de_abc_limit_prod_perc  VALUE '20',
              basis_perc_b TYPE /scwm/de_abc_limit_basis_perc VALUE '15',
              prod_perc_b  TYPE /scwm/de_abc_limit_prod_perc  VALUE '30',
              basis_perc_c TYPE /scwm/de_abc_limit_basis_perc VALUE '5',
              prod_perc_c  TYPE /scwm/de_abc_limit_prod_perc  VALUE '50',
            END OF gc_default.

 CONSTANTS: BEGIN OF gc_categories_cnt,
              minimum TYPE /scwm/de_abc_categories_cnt VALUE 3,
              maximum TYPE /scwm/de_abc_categories_cnt VALUE 12,
            END OF gc_categories_cnt.

 CONSTANTS:
   "numeric boolean
   BEGIN OF gc_bool,
     true  TYPE sap_bool VALUE 1,
     false TYPE sap_bool VALUE 0,
   END OF gc_bool.

 DATA ok_code TYPE sy-ucomm ##NEEDED.
 DATA gv_execute_triggered TYPE abap_bool ##NEEDED.
 DATA gt_limits     TYPE /scwm/tt_abc_limits ##NEEDED.
 DATA gt_categories TYPE TABLE OF /scwm/de_abc_indicator ##NEEDED.

 " Variables for SELECTION-OPTIONS
 DATA gv_entit  TYPE /scwm/s_abc_analysis_f4-entitled.
 DATA gv_matnr  TYPE /scwm/s_abc_analysis_f4-matnr.
 DATA gv_matgr  TYPE /scwm/s_abc_analysis_f4-matgr.
 DATA gv_trart  TYPE /scwm/s_abc_analysis_f4-trart.
 DATA gv_procty TYPE /scwm/s_abc_analysis_f4-procty.
 DATA gv_nltyp  TYPE /scwm/s_abc_analysis_f4-nltyp.
 DATA gv_vltyp  TYPE /scwm/s_abc_analysis_f4-vltyp.
 DATA gv_altme  TYPE /scwm/s_abc_analysis_f4-altme.
 DATA gv_conf   TYPE /scwm/s_abc_analysis_f4-confirmed_at.
 DATA gv_dispatch_indicator TYPE zz1_disp.

 DATA gv_acc    TYPE abap_bool.
*------------------------------------------------------------------------------------------------------------
 SELECTION-SCREEN BEGIN OF BLOCK prod_sel WITH FRAME TITLE TEXT-002.

   PARAMETERS:
     p_lgnum TYPE /scwm/s_abc_analysis_f4-lgnum OBLIGATORY MODIF ID wh MEMORY ID /scwm/lgn. "warehouse
   SELECT-OPTIONS: so_entit FOR gv_entit. "party entitled to dispose
   SELECT-OPTIONS: p_matnr FOR gv_matnr. "product
   SELECT-OPTIONS: p_dispi FOR gv_dispatch_indicator. " Dispatchable indicator
   SELECT-OPTIONS: p_matgr FOR gv_matgr. "product group


   PARAMETERS: p_excl TYPE /scwm/de_abc_excl_prod_wo_wt AS CHECKBOX DEFAULT abap_false.

 SELECTION-SCREEN END OF BLOCK prod_sel.
*------------------------------------------------------------------------------------------------------------
 SELECTION-SCREEN BEGIN OF BLOCK wt_sel WITH FRAME TITLE TEXT-001.
   "A selection to filter the confirmed warehouse tasks by following criteria:

   SELECTION-SCREEN BEGIN OF BLOCK conf_at.
     SELECT-OPTIONS: p_conf FOR gv_conf. "confirmation date
   SELECTION-SCREEN END OF BLOCK conf_at.

   SELECTION-SCREEN ULINE.

   SELECTION-SCREEN BEGIN OF BLOCK whse_process.
     SELECT-OPTIONS: p_trart FOR gv_trart. "warehouse process category
     SELECT-OPTIONS: p_procty FOR gv_procty. "warehouse process type
   SELECTION-SCREEN END OF BLOCK whse_process.

   SELECTION-SCREEN ULINE.

   SELECTION-SCREEN BEGIN OF BLOCK source_lgtyp.
     SELECT-OPTIONS: p_vltyp FOR gv_vltyp MODIF ID vlt. "source storage type
   SELECTION-SCREEN END OF BLOCK source_lgtyp.
   SELECTION-SCREEN BEGIN OF BLOCK dest_lgtyp.
     SELECT-OPTIONS: p_nltyp FOR gv_nltyp MODIF ID nlt. "destination storage type
   SELECTION-SCREEN END OF BLOCK dest_lgtyp.

   SELECT-OPTIONS: p_aunit FOR gv_altme. "unit of measure
 SELECTION-SCREEN END OF BLOCK wt_sel.
*------------------------------------------------------------------------------------------------------------
 SELECTION-SCREEN BEGIN OF BLOCK strategy_sel WITH FRAME TITLE TEXT-003.

   "analysis basis
   PARAMETERS: p_basis TYPE /scwm/de_abc_analysis_basis AS LISTBOX VISIBLE LENGTH 20 DEFAULT /scwm/if_abc_c=>sc_basis-cnt OBLIGATORY.

   SELECTION-SCREEN BEGIN OF BLOCK categories_cnt.
     "no of abc categories
     PARAMETERS: p_class TYPE /scwm/de_abc_categories_cnt DEFAULT 3 ##SEL_WRONG.
   SELECTION-SCREEN END OF BLOCK categories_cnt.

   SELECTION-SCREEN ULINE.
   SELECTION-SCREEN BEGIN OF BLOCK strategy_table.

     "radio buttons / check boxes for analysis strategy
     SELECTION-SCREEN BEGIN OF LINE.
       SELECTION-SCREEN POSITION 10.
       PARAMETERS: p_b_per TYPE /scwm/de_abc_basis_in_perc
       DEFAULT 'X' RADIOBUTTON GROUP hit MODIF ID rb1 USER-COMMAND strategy_hit.

       SELECTION-SCREEN POSITION 20.
       PARAMETERS: p_b_abs TYPE /scwm/de_abc_basis_in_no RADIOBUTTON GROUP hit MODIF ID rb2.

       SELECTION-SCREEN POSITION 29.
       PARAMETERS: p_pr_per TYPE /scwm/de_abc_prod_in_perc RADIOBUTTON GROUP hit MODIF ID rb3.

       SELECTION-SCREEN POSITION 38.
       PARAMETERS: p_pr_abs TYPE /scwm/de_abc_prod_in_no RADIOBUTTON GROUP hit MODIF ID rb4.

       SELECTION-SCREEN POSITION 49.
       PARAMETERS: p_paci TYPE /scwm/de_abc_upd_paci AS CHECKBOX MODIF ID cb1 USER-COMMAND indicator_check ##SEL_WRONG.

       SELECTION-SCREEN POSITION 60.
       PARAMETERS: p_lgbkz TYPE /scwm/de_abc_upd_stosecind AS CHECKBOX MODIF ID cb2 USER-COMMAND indicator_check ##SEL_WRONG.

       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccind TYPE /scwm/de_upd_ccind AS CHECKBOX MODIF ID cb3 USER-COMMAND indicator_check ##SEL_WRONG.

     SELECTION-SCREEN END OF LINE.

     "descriptions for RBs / CBs
     SELECTION-SCREEN BEGIN OF LINE.
       SELECTION-SCREEN COMMENT 7(10)  TEXT-007 FOR FIELD p_b_per. "basis in perc
       SELECTION-SCREEN COMMENT 17(9)  TEXT-008 FOR FIELD p_b_abs. "basis count
       SELECTION-SCREEN COMMENT 26(10) TEXT-010 FOR FIELD p_pr_per. "products in perc
       SELECTION-SCREEN COMMENT 36(9)  TEXT-011 FOR FIELD p_pr_abs. "products count
       SELECTION-SCREEN COMMENT 45(10) TEXT-012 FOR FIELD p_paci. "update paci
       SELECTION-SCREEN COMMENT 55(14) TEXT-013 FOR FIELD p_lgbkz. "update stosecind
       SELECTION-SCREEN COMMENT 74(10) TEXT-029 FOR FIELD p_ccind. "update ccind
     SELECTION-SCREEN END OF LINE.

     "StoSecInd update level selection
     SELECTION-SCREEN BEGIN OF LINE.
       SELECTION-SCREEN POSITION 55.
       PARAMETERS: p_level TYPE c AS LISTBOX VISIBLE LENGTH 16 DEFAULT /scwm/if_abc_c=>sc_stosecind_upd_level-whse OBLIGATORY MODIF ID cb2 USER-COMMAND level_change ##SEL_WRONG.
     SELECTION-SCREEN END OF LINE.

     "category rows A-L
     SELECTION-SCREEN BEGIN OF LINE. "A
       PARAMETERS: p_a TYPE /scwm/de_abc_indicator DEFAULT TEXT-004 MODIF ID ta ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_mata_p TYPE /scwm/de_abc_limit_basis_perc DEFAULT gc_default-basis_perc_a MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_mata_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matapp TYPE /scwm/de_abc_limit_prod_perc  DEFAULT gc_default-prod_perc_a MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matapa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_a TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkza TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccinda TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "B
       PARAMETERS: p_b TYPE /scwm/de_abc_indicator DEFAULT TEXT-005 MODIF ID tb ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matb_p TYPE /scwm/de_abc_limit_basis_perc DEFAULT gc_default-basis_perc_b MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matb_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matbpp TYPE /scwm/de_abc_limit_prod_perc DEFAULT gc_default-prod_perc_b MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matbpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_b TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzb TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindb TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "C
       PARAMETERS: p_c TYPE /scwm/de_abc_indicator DEFAULT TEXT-006 MODIF ID tc ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matc_p TYPE /scwm/de_abc_limit_basis_perc DEFAULT gc_default-basis_perc_c MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matc_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matcpp TYPE /scwm/de_abc_limit_prod_perc DEFAULT gc_default-prod_perc_c MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matcpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_c TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzc TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindc TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "D
       PARAMETERS: p_d TYPE /scwm/de_abc_indicator DEFAULT TEXT-039 MODIF ID td ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matd_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matd_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matdpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matdpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_d TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzd TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindd TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "E
       PARAMETERS: p_e TYPE /scwm/de_abc_indicator DEFAULT TEXT-040 MODIF ID te ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_mate_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_mate_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matepp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matepa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_e TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkze TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccinde TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "F
       PARAMETERS: p_f TYPE /scwm/de_abc_indicator DEFAULT TEXT-041 MODIF ID tf ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matf_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matf_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matfpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matfpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_f TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzf TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindf TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "G
       PARAMETERS: p_g TYPE /scwm/de_abc_indicator DEFAULT TEXT-043 MODIF ID tg ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matg_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matg_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matgpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matgpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_g TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzg TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindg TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "H
       PARAMETERS: p_h TYPE /scwm/de_abc_indicator DEFAULT TEXT-044 MODIF ID th ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_math_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_math_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_mathpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_mathpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_h TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzh TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindh TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "I
       PARAMETERS: p_i TYPE /scwm/de_abc_indicator DEFAULT TEXT-045 MODIF ID ti ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_mati_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_mati_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matipp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matipa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_i TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzi TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindi TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "J
       PARAMETERS: p_j TYPE /scwm/de_abc_indicator DEFAULT TEXT-046 MODIF ID tj ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matj_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matj_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matjpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matjpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_j TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzj TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindj TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "K
       PARAMETERS: p_k TYPE /scwm/de_abc_indicator DEFAULT TEXT-047 MODIF ID tk ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matk_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matk_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matkpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matkpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_k TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzk TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindk TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

     SELECTION-SCREEN BEGIN OF LINE. "L
       PARAMETERS: p_l TYPE /scwm/de_abc_indicator DEFAULT TEXT-048 MODIF ID tl ##SEL_WRONG.
       SELECTION-SCREEN POSITION 9.
       PARAMETERS: p_matl_p TYPE /scwm/de_abc_limit_basis_perc MODIF ID rb1 ##SEL_WRONG. "basis in perc
       SELECTION-SCREEN POSITION 16.
       PARAMETERS: p_matl_a TYPE /scwm/de_abc_limit_basis_no MODIF ID rb2 ##SEL_WRONG. "basis cnt
       SELECTION-SCREEN POSITION 28.
       PARAMETERS: p_matlpp TYPE /scwm/de_abc_limit_prod_perc MODIF ID rb3 ##SEL_WRONG. "products in perc
       SELECTION-SCREEN POSITION 35.
       PARAMETERS: p_matlpa TYPE /scwm/de_abc_limit_prod_no MODIF ID rb4 ##SEL_WRONG. "products cnt
       SELECTION-SCREEN POSITION 47.
       PARAMETERS: p_paci_l TYPE /scwm/s_abc_analysis_f4-put_stra MODIF ID cb1 ##SEL_WRONG. "paci
       SELECTION-SCREEN POSITION 57.
       PARAMETERS: p_lgbkzl TYPE /scwm/s_abc_analysis_f4-lgbkz MODIF ID cb2 ##SEL_WRONG. "stosecind
       SELECTION-SCREEN POSITION 76.
       PARAMETERS: p_ccindl TYPE /scwm/s_abc_analysis_f4-ccind MODIF ID cb3 ##SEL_WRONG. "ccind
     SELECTION-SCREEN END OF LINE.

   SELECTION-SCREEN END OF BLOCK strategy_table.

 SELECTION-SCREEN END OF BLOCK strategy_sel.

 SELECTION-SCREEN BEGIN OF BLOCK backgr_proc_sel WITH FRAME TITLE TEXT-015.
   SELECTION-SCREEN BEGIN OF LINE.

     "flag whether to execute in background processing
     PARAMETERS p_bupd TYPE /scwm/de_abc_back_proc AS CHECKBOX  ##SEL_WRONG.
     SELECTION-SCREEN COMMENT 5(79) TEXT-014 FOR FIELD p_bupd.
   SELECTION-SCREEN END OF LINE.
 SELECTION-SCREEN END OF BLOCK backgr_proc_sel.

 AT SELECTION-SCREEN ON BLOCK categories_cnt.
   "catch invalid number of abc categories
   IF p_class < gc_categories_cnt-minimum.
     MESSAGE s000.
     p_class = gc_categories_cnt-minimum.
     MODIFY SCREEN.
   ELSEIF p_class > gc_categories_cnt-maximum.
     MESSAGE s001.
     p_class = gc_categories_cnt-maximum.
     MODIFY SCREEN.
   ENDIF.

 AT SELECTION-SCREEN ON BLOCK conf_at.
   "catch confirmed date in future
   IF p_conf IS NOT INITIAL.
     DATA(lv_date_okay) = abap_false.
     LOOP AT p_conf ASSIGNING FIELD-SYMBOL(<conf>).
       IF p_conf-low <= sy-datum.
         lv_date_okay = abap_true.
       ENDIF.
     ENDLOOP.
     IF lv_date_okay = abap_false.
       MESSAGE e031.
     ENDIF.
   ENDIF.

 AT SELECTION-SCREEN ON BLOCK strategy_sel.

   IF sy-ucomm <> 'STRATEGY_HIT' AND sy-ucomm <> 'INDICATOR_CHECK'.

     DATA: lt_limits           TYPE TABLE OF int4,
           lv_no_filled_fields TYPE i,
           lv_no_fields        TYPE i,
           lv_total_per        TYPE i.

     FIELD-SYMBOLS: <lv_limit> TYPE int4.

     "get category limits
     IF p_b_per IS NOT INITIAL.
       DATA: lt_b_per TYPE TABLE OF int4.

       APPEND p_mata_p TO lt_b_per.
       APPEND p_matb_p TO lt_b_per.
       APPEND p_matc_p TO lt_b_per.
       APPEND p_matd_p TO lt_b_per.
       APPEND p_mate_p TO lt_b_per.
       APPEND p_matf_p TO lt_b_per.
       APPEND p_matg_p TO lt_b_per.
       APPEND p_math_p TO lt_b_per.
       APPEND p_mati_p TO lt_b_per.
       APPEND p_matj_p TO lt_b_per.
       APPEND p_matk_p TO lt_b_per.
       APPEND p_matl_p TO lt_b_per.

       DELETE lt_b_per FROM p_class + 1.
       lt_limits = lt_b_per.

     ELSEIF p_pr_per IS NOT INITIAL.
       DATA: lt_pr_per TYPE TABLE OF int4.

       APPEND p_matapp TO lt_pr_per.
       APPEND p_matbpp TO lt_pr_per.
       APPEND p_matcpp TO lt_pr_per.
       APPEND p_matdpp TO lt_pr_per.
       APPEND p_matepp TO lt_pr_per.
       APPEND p_matfpp TO lt_pr_per.
       APPEND p_matgpp TO lt_pr_per.
       APPEND p_mathpp TO lt_pr_per.
       APPEND p_matipp TO lt_pr_per.
       APPEND p_matjpp TO lt_pr_per.
       APPEND p_matkpp TO lt_pr_per.
       APPEND p_matlpp TO lt_pr_per.

       DELETE lt_pr_per FROM p_class + 1.
       lt_limits = lt_pr_per.

     ELSEIF p_b_abs IS NOT INITIAL.
       DATA: lt_b_abs TYPE TABLE OF int4.

       APPEND p_mata_a TO lt_b_abs.
       APPEND p_matb_a TO lt_b_abs.
       APPEND p_matc_a TO lt_b_abs.
       APPEND p_matd_a TO lt_b_abs.
       APPEND p_mate_a TO lt_b_abs.
       APPEND p_matf_a TO lt_b_abs.
       APPEND p_matg_a TO lt_b_abs.
       APPEND p_math_a TO lt_b_abs.
       APPEND p_mati_a TO lt_b_abs.
       APPEND p_matj_a TO lt_b_abs.
       APPEND p_matk_a TO lt_b_abs.
       APPEND p_matl_a TO lt_b_abs.

       DELETE lt_b_abs FROM p_class + 1.
       lt_limits = lt_b_abs.

     ELSEIF p_pr_abs IS NOT INITIAL.
       DATA: lt_pr_abs TYPE TABLE OF int4.

       APPEND p_matapa TO lt_b_abs.
       APPEND p_matbpa TO lt_b_abs.
       APPEND p_matcpa TO lt_b_abs.
       APPEND p_matdpa TO lt_b_abs.
       APPEND p_matepa TO lt_b_abs.
       APPEND p_matfpa TO lt_b_abs.
       APPEND p_matgpa TO lt_b_abs.
       APPEND p_mathpa TO lt_b_abs.
       APPEND p_matipa TO lt_b_abs.
       APPEND p_matjpa TO lt_b_abs.
       APPEND p_matkpa TO lt_b_abs.
       APPEND p_matlpa TO lt_b_abs.

       DELETE lt_pr_abs FROM p_class + 1.
       lt_limits = lt_pr_abs.

     ENDIF.

     "get number of not filled cat. limit fields
     LOOP AT lt_limits ASSIGNING <lv_limit>.
       IF <lv_limit> IS NOT INITIAL.
         lv_no_filled_fields = lv_no_filled_fields + 1.
       ENDIF.
     ENDLOOP.
     DESCRIBE TABLE lt_limits LINES lv_no_fields.

     IF lv_no_filled_fields <= 2 AND lv_no_filled_fields < lv_no_fields - 1.
       "case: not enough category limit fields filled
       MESSAGE e010.

     ELSEIF lv_no_filled_fields = lv_no_fields - 1 AND ( p_pr_per IS NOT INITIAL OR p_b_per IS NOT INITIAL ).
       "case: all but 1 input field filled in percentage

       "calculate missing field
       LOOP AT lt_limits ASSIGNING <lv_limit>.
         IF <lv_limit> IS NOT INITIAL.
           lv_total_per = lv_total_per + <lv_limit>.
         ENDIF.
       ENDLOOP.


       LOOP AT lt_limits ASSIGNING <lv_limit>.
         IF <lv_limit> = 0 AND lv_total_per < 100.
           <lv_limit> = 100 - lv_total_per.
           lv_total_per = 100.
         ENDIF.
       ENDLOOP.

       IF p_b_per IS NOT INITIAL.
         READ TABLE lt_limits INDEX 1 INTO p_mata_p.
         READ TABLE lt_limits INDEX 2 INTO p_matb_p.
         READ TABLE lt_limits INDEX 3 INTO p_matc_p.
         READ TABLE lt_limits INDEX 4 INTO p_matd_p.
         READ TABLE lt_limits INDEX 5 INTO p_mate_p.
         READ TABLE lt_limits INDEX 6 INTO p_matf_p.
         READ TABLE lt_limits INDEX 7 INTO p_matg_p.
         READ TABLE lt_limits INDEX 8 INTO p_math_p.
         READ TABLE lt_limits INDEX 9 INTO p_mati_p.
         READ TABLE lt_limits INDEX 10 INTO p_matj_p.
         READ TABLE lt_limits INDEX 11 INTO p_matk_p ##NUMBER_OK.
         READ TABLE lt_limits INDEX 12 INTO p_matl_p ##NUMBER_OK.
       ELSE.
         READ TABLE lt_limits INDEX 1 INTO p_matapp.
         READ TABLE lt_limits INDEX 2 INTO p_matbpp.
         READ TABLE lt_limits INDEX 3 INTO p_matcpp.
         READ TABLE lt_limits INDEX 4 INTO p_matdpp.
         READ TABLE lt_limits INDEX 5 INTO p_matepp.
         READ TABLE lt_limits INDEX 6 INTO p_matfpp.
         READ TABLE lt_limits INDEX 7 INTO p_matgpp.
         READ TABLE lt_limits INDEX 8 INTO p_mathpp.
         READ TABLE lt_limits INDEX 9 INTO p_matipp.
         READ TABLE lt_limits INDEX 10 INTO p_matjpp.
         READ TABLE lt_limits INDEX 11 INTO p_matkpp ##NUMBER_OK.
         READ TABLE lt_limits INDEX 12 INTO p_matlpp ##NUMBER_OK.
       ENDIF.
       MODIFY SCREEN.
     ENDIF.

     "check whether percentual limits are 100% in total
     DATA: lv_total_limits TYPE i.

     LOOP AT lt_limits ASSIGNING FIELD-SYMBOL(<limit>).
       lv_total_limits = lv_total_limits + <limit>.
     ENDLOOP.

     IF ( p_b_per IS NOT INITIAL OR p_pr_per IS NOT INITIAL ) AND lv_total_limits <> 100.
       IF gv_execute_triggered = abap_true.
         DATA(lv_type) = wmegc_severity_err.
       ELSE.
         lv_type = wmegc_severity_suc.
       ENDIF.
       MESSAGE ID '/SCWM/ABC_ANALYSIS' TYPE lv_type NUMBER 002.
     ENDIF.

   ENDIF.

   DATA: lt_cats             TYPE TABLE OF c,
         ls_selopt           TYPE rsdsselopt,
         lt_selopt_cat_names TYPE rseloption.

   "check whether all categories have names
   ls_selopt-sign = 'I'.
   ls_selopt-option = 'EQ'.

   APPEND p_a TO lt_cats.
   APPEND p_b TO lt_cats.
   APPEND p_c TO lt_cats.
   APPEND p_d TO lt_cats.
   APPEND p_e TO lt_cats.
   APPEND p_f TO lt_cats.
   APPEND p_g TO lt_cats.
   APPEND p_h TO lt_cats.
   APPEND p_i TO lt_cats.
   APPEND p_j TO lt_cats.
   APPEND p_k TO lt_cats.
   APPEND p_l TO lt_cats.

   DELETE lt_cats FROM p_class + 1.

   LOOP AT lt_cats ASSIGNING FIELD-SYMBOL(<cat>).
     IF <cat> IS INITIAL.
       MESSAGE e011.
     ENDIF.

     IF lt_selopt_cat_names IS INITIAL OR ( lt_selopt_cat_names IS NOT INITIAL AND <cat> NOT IN lt_selopt_cat_names ) ##BOOL_OK.
       ls_selopt-low = <cat>.
       APPEND ls_selopt TO lt_selopt_cat_names.
     ELSE.
       MESSAGE e034.
     ENDIF.
   ENDLOOP.

   "check whether any indicator update is selected
   DATA: lt_paci   TYPE TABLE OF char10,
         lt_lgbkz  TYPE TABLE OF char10,
         lt_ccind  TYPE TABLE OF char10,
         lv_filled TYPE abap_bool.

   IF sy-ucomm <> 'INDICATOR_CHECK' AND sy-ucomm <> 'LEVEL_CHANGE'.
     "check if at least 1 update value for selected indicator is filled
     IF p_paci = abap_true.
       APPEND p_paci_a TO lt_paci.
       APPEND p_paci_b TO lt_paci.
       APPEND p_paci_c TO lt_paci.
       APPEND p_paci_d TO lt_paci.
       APPEND p_paci_e TO lt_paci.
       APPEND p_paci_f TO lt_paci.
       APPEND p_paci_g TO lt_paci.
       APPEND p_paci_h TO lt_paci.
       APPEND p_paci_i TO lt_paci.
       APPEND p_paci_j TO lt_paci.
       APPEND p_paci_k TO lt_paci.
       APPEND p_paci_l TO lt_paci.
       DELETE lt_paci FROM p_class + 1.
       lv_filled = abap_false.

       LOOP AT lt_paci ASSIGNING FIELD-SYMBOL(<lv_paci>).
         IF <lv_paci> IS NOT INITIAL.
           lv_filled = abap_true.
           EXIT.
         ENDIF.
       ENDLOOP.

       IF lv_filled = abap_false.
         MESSAGE w003.
         LOOP AT SCREEN.
           IF screen-group1 = 'CB1'.
             screen-input = gc_bool-true.
           ENDIF.
         ENDLOOP.
       ENDIF.
     ENDIF.
     IF p_lgbkz = abap_true .
       APPEND p_lgbkza TO lt_lgbkz.
       APPEND p_lgbkzb TO lt_lgbkz.
       APPEND p_lgbkzc TO lt_lgbkz.
       APPEND p_lgbkzd TO lt_lgbkz.
       APPEND p_lgbkze TO lt_lgbkz.
       APPEND p_lgbkzf TO lt_lgbkz.
       APPEND p_lgbkzg TO lt_lgbkz.
       APPEND p_lgbkzh TO lt_lgbkz.
       APPEND p_lgbkzi TO lt_lgbkz.
       APPEND p_lgbkzj TO lt_lgbkz.
       APPEND p_lgbkzk TO lt_lgbkz.
       APPEND p_lgbkzl TO lt_lgbkz.
       DELETE lt_lgbkz FROM p_class + 1.
       lv_filled = abap_false.

       LOOP AT lt_lgbkz ASSIGNING FIELD-SYMBOL(<lv_lgbkz>).
         IF <lv_lgbkz> IS NOT INITIAL.
           lv_filled = abap_true.
           EXIT.
         ENDIF.
       ENDLOOP.
       IF lv_filled = abap_false.
         MESSAGE w004.
       ENDIF.
     ENDIF.
     IF p_ccind = abap_true AND p_ccinda IS INITIAL AND p_ccindb IS INITIAL AND p_ccindc IS INITIAL.
       APPEND p_ccinda TO lt_ccind.
       APPEND p_ccindb TO lt_ccind.
       APPEND p_ccindc TO lt_ccind.
       APPEND p_ccindd TO lt_ccind.
       APPEND p_ccinde TO lt_ccind.
       APPEND p_ccindf TO lt_ccind.
       APPEND p_ccindg TO lt_ccind.
       APPEND p_ccindh TO lt_ccind.
       APPEND p_ccindi TO lt_ccind.
       APPEND p_ccindj TO lt_ccind.
       APPEND p_ccindk TO lt_ccind.
       APPEND p_ccindl TO lt_ccind.
       DELETE lt_ccind FROM p_class + 1.
       lv_filled = abap_false.

       LOOP AT lt_ccind ASSIGNING FIELD-SYMBOL(<lv_ccind>).
         IF <lv_ccind> IS NOT INITIAL.
           lv_filled = abap_true.
           EXIT.
         ENDIF.
       ENDLOOP.

       IF lv_filled = abap_false.
         MESSAGE w005.
       ENDIF.
     ENDIF.

     LOOP AT SCREEN.
       IF screen-group1 = 'CB1' AND p_paci IS NOT INITIAL OR
          screen-group1 = 'CB2' AND p_lgbkz IS NOT INITIAL OR
         screen-group1 = 'CB3' AND p_ccind IS NOT INITIAL.
         screen-input = gc_bool-true.
         MODIFY SCREEN.
       ELSE.
         screen-required = 0.
         IF screen-group1 = 'CB1' AND screen-name <> 'P_PACI' OR
          screen-group1 = 'CB2' AND screen-name <> 'P_LGBKZ' OR
           screen-group1 = 'CB3' AND screen-name <> 'P_CCIND'.
           screen-input = gc_bool-false.
         ENDIF.
         MODIFY SCREEN.
       ENDIF.
     ENDLOOP.
   ENDIF.

 AT SELECTION-SCREEN ON BLOCK source_lgtyp.
   "if StoSecInd is to be updated on Source Storage Type Level but no Storage Type is put in show message
   IF sy-ucomm <> 'LEVEL_CHANGE' AND sy-ucomm <> 'INDICATOR_CHECK'.
     IF p_lgbkz = abap_true AND p_level = /scwm/if_abc_c=>sc_stosecind_upd_level-source AND p_vltyp IS INITIAL.
       MESSAGE e006.
     ENDIF.
   ENDIF.

 AT SELECTION-SCREEN ON BLOCK dest_lgtyp.
   "if StoSecInd is to be updated on Destination Storage Type Level but no Storage Type is put in show message
   IF sy-ucomm <> 'LEVEL_CHANGE' AND sy-ucomm <> 'INDICATOR_CHECK'.
     IF p_lgbkz = abap_true AND p_level = /scwm/if_abc_c=>sc_stosecind_upd_level-dest AND p_nltyp IS INITIAL.
       MESSAGE e007.
     ENDIF.
   ENDIF.

 AT SELECTION-SCREEN ON BLOCK whse_process.
   DATA: lt_t333           TYPE /scwm/tt_t333,
         lv_bool_proctrart TYPE c LENGTH 1.

   FIELD-SYMBOLS:
         <ls_t333>           TYPE /scwm/t333.

   IF p_procty IS NOT INITIAL AND p_trart IS NOT INITIAL.
     IF lt_t333 IS INITIAL. "read t333 for first time
       CALL FUNCTION '/SCWM/T333_READ_SINGLE'
         EXPORTING
           iv_lgnum = p_lgnum
         IMPORTING
           et_t333  = lt_t333.
     ELSE.
       READ TABLE lt_t333 ASSIGNING <ls_t333> INDEX 1.
       IF <ls_t333>-lgnum NE p_lgnum. "re-read t333 if lgnum has changed
         CALL FUNCTION '/SCWM/T333_READ_SINGLE'
           EXPORTING
             iv_lgnum = p_lgnum
           IMPORTING
             et_t333  = lt_t333.
       ENDIF.
     ENDIF.

     "check whether process type and process category match for at least 1 proc. type
     lv_bool_proctrart = abap_false.
     LOOP AT lt_t333 ASSIGNING <ls_t333>.
       IF <ls_t333>-procty IN p_procty AND <ls_t333>-trart IN p_trart.
         lv_bool_proctrart = abap_true.
       ENDIF.
     ENDLOOP.
     IF lv_bool_proctrart = abap_false.
       MESSAGE e008.
     ENDIF.
   ENDIF.


 AT SELECTION-SCREEN.
   DATA: lv_limit TYPE /scwm/de_abc_limit_basis_no.
   CLEAR gt_categories.

   gv_execute_triggered = abap_true.

   ok_code = sy-ucomm.
   CASE ok_code.
     WHEN 'OK'.

     WHEN 'BACK'.
       LEAVE PROGRAM.

     WHEN 'CANCEL'.
       LEAVE PROGRAM.

     WHEN 'EXIT'.
       LEAVE PROGRAM.
   ENDCASE  .

   CLEAR: lv_limit, gt_limits.

   IF p_b_per = abap_true.
     lv_limit = p_mata_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matb_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matc_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matd_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mate_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matf_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matg_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_math_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mati_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matj_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matk_p.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matl_p.
     APPEND lv_limit TO gt_limits.

   ELSEIF p_b_abs = abap_true.

     lv_limit = p_mata_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matb_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matc_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matd_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mate_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matf_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matg_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_math_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mati_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matj_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matk_a.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matl_a.
     APPEND lv_limit TO gt_limits.

   ELSEIF p_pr_per = abap_true.
     lv_limit = p_matapp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matbpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matcpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matdpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matepp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matfpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matgpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mathpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matipp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matjpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matkpp.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matlpp.
     APPEND lv_limit TO gt_limits.

   ELSEIF p_pr_abs = abap_true.
     lv_limit = p_matapa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matbpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matcpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matdpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matepa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matfpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matgpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_mathpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matipa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matjpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matkpa.
     APPEND lv_limit TO gt_limits.

     lv_limit = p_matlpa.
     APPEND lv_limit TO gt_limits.

   ENDIF.

   DELETE gt_limits FROM p_class + 1.

   APPEND p_a TO gt_categories.
   APPEND p_b TO gt_categories.
   APPEND p_c TO gt_categories.
   APPEND p_d TO gt_categories.
   APPEND p_e TO gt_categories.
   APPEND p_f TO gt_categories.
   APPEND p_g TO gt_categories.
   APPEND p_h TO gt_categories.
   APPEND p_i TO gt_categories.
   APPEND p_j TO gt_categories.
   APPEND p_k TO gt_categories.
   APPEND p_l TO gt_categories.

   DELETE gt_categories FROM p_class + 1.

   "check whether percentual limits are 100% in total
   DATA: lv_total_limits TYPE i.

   LOOP AT gt_limits ASSIGNING FIELD-SYMBOL(<limit>).
     lv_total_limits = lv_total_limits + <limit>.
   ENDLOOP.

   IF ( p_b_per IS NOT INITIAL OR p_pr_per IS NOT INITIAL )
     AND lv_total_limits <> 100
     AND ok_code <> 'STRATEGY_HIT' AND ok_code IS NOT INITIAL.
     MESSAGE e002.
   ENDIF.


 AT SELECTION-SCREEN OUTPUT.
   DATA:
     lv_category   TYPE c LENGTH 2,
     lt_categories LIKE TABLE OF lv_category,
     lt_literals   TYPE TABLE OF c,
     lv_limit      TYPE string,
     lv_paci       TYPE string,
     lv_lgbkz      TYPE string,
     lv_ccind      TYPE string.

   FIELD-SYMBOLS:
     <literal>  TYPE c,
     <category> LIKE lv_category.

   gv_execute_triggered = abap_false.

   APPEND 'A' TO lt_literals.
   APPEND 'B' TO lt_literals.
   APPEND 'C' TO lt_literals.
   APPEND 'D' TO lt_literals.
   APPEND 'E' TO lt_literals.
   APPEND 'F' TO lt_literals.
   APPEND 'G' TO lt_literals.
   APPEND 'H' TO lt_literals.
   APPEND 'I' TO lt_literals.
   APPEND 'J' TO lt_literals.
   APPEND 'K' TO lt_literals.
   APPEND 'L' TO lt_literals.

   LOOP AT lt_literals ASSIGNING <literal>.
     CONCATENATE 'T' <literal> INTO lv_category.
     APPEND lv_category TO lt_categories.
   ENDLOOP.

   LOOP AT SCREEN.

     "display all to be displayed abc categories
     LOOP AT lt_categories ASSIGNING <category>.
       IF sy-tabix LE p_class.
         IF screen-group1 = <category>.
           screen-invisible = gc_bool-false.
           screen-output = gc_bool-true.
           screen-active = gc_bool-true.
           MODIFY SCREEN.
           EXIT.
         ENDIF.
       ELSE.
         IF screen-group1 = <category>.
           screen-invisible = gc_bool-true.
           screen-output = gc_bool-false.
           screen-active = gc_bool-false.
           MODIFY SCREEN.
           EXIT.
         ENDIF.
       ENDIF.
     ENDLOOP.

     LOOP AT lt_literals ASSIGNING <literal>.
       CONCATENATE 'P_MAT' <literal> INTO lv_limit.
       CONCATENATE 'P_PACI_' <literal> INTO lv_paci.
       CONCATENATE 'P_LGBKZ' <literal> INTO lv_lgbkz.
       CONCATENATE 'P_CCIND' <literal> INTO lv_ccind.
       IF sy-tabix LE p_class.
         IF screen-name CS lv_limit
           OR screen-name CS lv_paci
           OR screen-name CS lv_lgbkz
           OR screen-name CS lv_ccind.
           screen-invisible = gc_bool-false.
           screen-input = gc_bool-true.
           screen-output = gc_bool-true.
           screen-active = gc_bool-true.
           MODIFY SCREEN.
           EXIT.
         ENDIF.
       ELSE.
         IF screen-name CS lv_limit
            OR screen-name CS lv_paci
            OR screen-name CS lv_lgbkz
            OR screen-name CS lv_ccind.
           screen-invisible = gc_bool-true.
           screen-input = gc_bool-false.
           screen-output = gc_bool-false.
           screen-active = gc_bool-false.
           MODIFY SCREEN.
           EXIT.
         ENDIF.
       ENDIF.
     ENDLOOP.

     IF screen-name CS 'P_MATNR' OR screen-name CS 'P_MATGR'.
       screen-invisible = gc_bool-false.
       screen-input = gc_bool-true.
       screen-output = gc_bool-true.
       screen-active = gc_bool-true.
       MODIFY SCREEN.
     ENDIF.


     IF screen-group1 = 'RB1' AND p_b_per IS NOT INITIAL OR
       screen-group1 = 'RB2' AND p_b_abs IS NOT INITIAL OR
       screen-group1 = 'RB3' AND p_pr_per IS NOT INITIAL OR
       screen-group1 = 'RB4' AND p_pr_abs IS NOT INITIAL OR
       screen-group1 = 'WH'.
       screen-input = gc_bool-true.

       MODIFY SCREEN.
     ELSEIF screen-group1 = 'CB1' AND p_paci IS NOT INITIAL OR
       screen-group1 = 'CB2' AND p_lgbkz IS NOT INITIAL OR
       screen-group1 = 'CB3' AND p_ccind IS NOT INITIAL.
       screen-input = gc_bool-true.
       MODIFY SCREEN.
     ELSE.
       IF screen-group1 = 'RB1' AND screen-name <> 'P_B_PER' OR
        screen-group1 = 'RB2' AND screen-name <> 'P_B_ABS' OR
        screen-group1 = 'RB3' AND screen-name <> 'P_PR_PER' OR
        screen-group1 = 'RB4' AND screen-name <> 'P_PR_ABS' OR
        screen-group1 = 'CB1' AND screen-name <> 'P_PACI' OR
        screen-group1 = 'CB2' AND screen-name <> 'P_LGBKZ' OR
         screen-group1 = 'CB3' AND screen-name <> 'P_CCIND'.
         screen-input = gc_bool-false.
       ENDIF.
       MODIFY SCREEN.
     ENDIF.

     IF screen-name = '%_P_MATNR_%_APP_%-OPTI_PUSH'
       OR screen-name = '%_P_MATGR_%_APP_%-OPTI_PUSH'.
       screen-input = 0.
       MODIFY SCREEN.
     ENDIF.

   ENDLOOP.

 AT SELECTION-SCREEN ON EXIT-COMMAND.
   ok_code = sy-ucomm.
   CASE ok_code.

     WHEN 'CEND' OR 'CBAC' OR 'CCAN'.

       CLEAR ok_code.

       LEAVE PROGRAM.

   ENDCASE.

 INITIALIZATION.
   DATA: ls_list TYPE vrm_value ##NEEDED,
         lt_list TYPE vrm_values ##NEEDED.

   CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
     IMPORTING
       accessibility     = gv_acc
     EXCEPTIONS
       its_not_available = 1
       OTHERS            = 2.

   IF sy-subrc <> 0.
     "display report with accessible screen
     LEAVE TO TRANSACTION '/SCWM/ABC_SCR2'.
   ENDIF.

   IF gv_acc = abap_true.
     LEAVE TO TRANSACTION '/SCWM/ABC_SCR2'.
   ENDIF.

   "fill analysis basis listbox
   ls_list-key = /scwm/if_abc_c=>sc_basis-cnt.
   ls_list-text = TEXT-037.
   APPEND ls_list TO lt_list.
   ls_list-key = /scwm/if_abc_c=>sc_basis-quan.
   ls_list-text = TEXT-038.
   APPEND ls_list TO lt_list.

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = 'P_BASIS'
       values = lt_list.

   "fill StoSecInd update level listbox
   CLEAR: ls_list, lt_list.
   ls_list-key = /scwm/if_abc_c=>sc_stosecind_upd_level-whse.
   ls_list-text = TEXT-026.
   APPEND ls_list TO lt_list.
   ls_list-key = /scwm/if_abc_c=>sc_stosecind_upd_level-source.
   ls_list-text = TEXT-027.
   APPEND ls_list TO lt_list.
   ls_list-key = /scwm/if_abc_c=>sc_stosecind_upd_level-dest.
   ls_list-text = TEXT-028.
   APPEND ls_list TO lt_list.

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = 'P_LEVEL'
       values = lt_list.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dispi-low.
   PERFORM value_help_dispi CHANGING p_dispi-low.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dispi-high.
   PERFORM value_help_dispi CHANGING p_dispi-high.

 FORM value_help_dispi CHANGING cv_value TYPE zz1_disp.
   DATA lt_return TYPE TABLE OF ddshretval.

   CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
     EXPORTING
       tabname           = '/SAPAPO/MATLWH'
       fieldname         = 'ZZ1_DISP_WHD'
     TABLES
       return_tab        = lt_return
     EXCEPTIONS
       field_not_found   = 1           " Field does not exist in the Dictionary
       no_help_for_field = 2           " No F4 help is defined for the field
       inconsistent_help = 3           " F4 help for the field is inconsistent
       no_values_found   = 4           " No values found
       OTHERS            = 5.
   IF sy-subrc = 0.
     cv_value = VALUE #( lt_return[ 1 ]-fieldval OPTIONAL ).
   ENDIF.
 ENDFORM.
