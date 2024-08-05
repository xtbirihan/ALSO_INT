**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSSEL
*&---------------------------------------------------------------------*

*--- Process: PI creation
SELECTION-SCREEN BEGIN OF SCREEN 2005 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK pigen WITH FRAME TITLE TEXT-gen.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(31) TEXT-cdt FOR FIELD gv_cdt_c.
      PARAMETERS: gv_cdt_c TYPE /scwm/s_aspq_pi_create-count_date_ui
                  DEFAULT gv_today.
      PARAMETERS: gv_cti_c TYPE /scwm/s_aspq_pi_create-count_time_ui ##NEEDED
                  DEFAULT limpi_end_time.
    SELECTION-SCREEN END OF LINE.
    PARAMETERS: gv_dty_c TYPE /scwm/s_aspq_pi_create-doc_type VALUE CHECK ##NEEDED.
    PARAMETERS: gv_rea_c TYPE /scwm/s_aspq_pi_create-reason ##NEEDED.
    PARAMETERS: gv_ref_c TYPE /scwm/s_aspq_pi_create-reference_ui ##NEEDED.
    PARAMETERS: gv_inv_c TYPE /scwm/s_aspq_pi_create-ind_inv ##NEEDED
                AS CHECKBOX.
    PARAMETERS: gv_kzl_c TYPE /scwm/s_aspq_pi_create-ind_kzl ##NEEDED
                AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK pigen.

  SELECTION-SCREEN BEGIN OF BLOCK piloc WITH FRAME TITLE TEXT-loc.
    PARAMETERS:     gv_lgn_c TYPE /scwm/s_aspq_pi_create-pi_lgnum
                    MEMORY ID /scwm/lgn NO-DISPLAY.
    SELECT-OPTIONS: gr_pia_c FOR /scwm/s_aspq_pi_create-pi_area_ui ##NEEDED
                    MEMORY ID /scwm/pi_area.
    SELECT-OPTIONS: gr_lgp_c FOR /scwm/s_aspq_pi_create-lgpla ##NEEDED.
    SELECT-OPTIONS: gr_psa_c FOR /scwm/s_aspq_pi_create-psa ##NEEDED.
  SELECTION-SCREEN END OF BLOCK piloc.

  SELECTION-SCREEN BEGIN OF BLOCK piprod WITH FRAME TITLE TEXT-pro.
    SELECT-OPTIONS: gr_mat_c FOR /scwm/s_aspq_pi_create-matnr ##NEEDED.
    SELECT-OPTIONS: gr_ent_c FOR /scwm/s_aspq_pi_create-entitled ##NEEDED.
    SELECT-OPTIONS: gr_own_c FOR /scwm/s_aspq_pi_create-owner ##NEEDED.
  SELECTION-SCREEN END OF BLOCK piprod.

  SELECTION-SCREEN BEGIN OF BLOCK adata WITH FRAME TITLE TEXT-add.
    SELECT-OPTIONS: so_mfrnr FOR wsd_basic_mat_sty-mfrnr ##NEEDED.
    SELECT-OPTIONS: so_mfrpn FOR wsd_basic_mat_sty-mfrpn ##NEEDED.
    SELECT-OPTIONS: so_huid FOR /scwm/s_huhdr-huident ##NEEDED.
  SELECTION-SCREEN END OF BLOCK adata.

SELECTION-SCREEN END OF SCREEN 2005.

*--- Process: PI processing
SELECTION-SCREEN BEGIN OF SCREEN 2010 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK pidoc WITH FRAME TITLE TEXT-pid.
    PARAMETERS:     gv_lgn_p TYPE /scwm/s_aspq_pi_process-pi_lgnum ##NEEDED
                        MEMORY ID /scwm/lgn NO-DISPLAY,
                    gv_yea_p TYPE /scwm/s_aspq_pi_process-doc_year ##NEEDED
                        MEMORY ID /scwm/doc_year NO-DISPLAY.
    SELECT-OPTIONS: gr_doc_p FOR /scwm/s_aspq_pi_process-doc_number ##NEEDED,
                    gr_itm_p FOR /scwm/s_aspq_pi_process-item_no ##NEEDED,
                    gr_who_p FOR /scwm/s_aspq_pi_process-who ##NEEDED,
                    gr_ref_p FOR /scwm/s_aspq_pi_process-reference_ui ##NEEDED.
  SELECTION-SCREEN END OF BLOCK pidoc.
SELECTION-SCREEN END OF SCREEN 2010.
