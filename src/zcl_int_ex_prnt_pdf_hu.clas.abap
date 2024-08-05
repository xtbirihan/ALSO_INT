CLASS zcl_int_ex_prnt_pdf_hu DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_prnt_pdf_hu .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_EX_PRNT_PDF_HU IMPLEMENTATION.


  METHOD /scwm/if_ex_prnt_pdf_hu~get_add_hu_data.
**********************************************************************
*& Key           : AD-230320
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_PRNT_PDF_HU
*&
**********************************************************************
    BREAK-POINT ID zcg_ex_prnt_pdf_hu.

    DATA(lo_hu_printing) = NEW zcl_int_hu_printing(
                              iv_application_log = iv_application_log
                              iv_form_name = iv_form_name
                              it_huhdr_int = it_huhdr_int
                              it_huitm_int = it_huitm_int
                              it_hutree = it_hutree
                              it_top_huhdr = it_top_huhdr
                              is_output_params = is_output_params
                              is_doc_params = is_doc_params
                              is_print = is_print ).

    lo_hu_printing->process_hu_printig(
                         IMPORTING
                              et_huhdr = et_huhdr
                              et_hu_hazard_mat = et_hu_hazard_mat
                              et_hu_ser_label = et_hu_ser_label
                              et_hu_shplabel = et_hu_shplabel
                              et_hu_wt_info = et_hu_wt_info
                              et_hu_print_tree = et_hu_print_tree
                              et_hu_content = et_hu_content
                              ev_printed = ev_printed ).
  ENDMETHOD.
ENDCLASS.
