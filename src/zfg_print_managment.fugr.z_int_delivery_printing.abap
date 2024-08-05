FUNCTION z_int_delivery_printing.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(/1BCDWB/DOCPARAMS) TYPE  SFPDOCPARAMS OPTIONAL
*"     REFERENCE(DELIVERY_DETAILS) TYPE  /SCDL/DLV_NOTE_STR OPTIONAL
*"     REFERENCE(ITEAM_DETAILS) TYPE  /SCWM/DLVNOTE_ITEMDATA_TBL_PDF
*"       OPTIONAL
*"     VALUE(HEADER_DETAILS) TYPE  /SCWM/DLVNOTE_HEADER_PDF OPTIONAL
*"     REFERENCE(FOOTER_DETAILS) TYPE  /SCWM/S_SP_A_HEAD_TEXT OPTIONAL
*"     REFERENCE(HIER_SORT) TYPE  /SCWM/T_DLV_HIER_SORT_PRINT OPTIONAL
*"  EXPORTING
*"     REFERENCE(/1BCDWB/FORMOUTPUT) TYPE  FPFORMOUTPUT
*"  EXCEPTIONS
*"      USAGE_ERROR
*"      SYSTEM_ERROR
*"      INTERNAL_ERROR
*"----------------------------------------------------------------------

  DATA(lo_delilvery_printing) = NEW zcl_int_delivery_printing( iv_form             = zcl_int_print_management=>mv_form
                                                               is_docparams        = /1bcdwb/docparams
                                                               is_delivery_details = delivery_details
                                                               it_item_details     = iteam_details
                                                               is_header_details   = header_details
                                                               it_hier_sort        = hier_sort ).


  /1bcdwb/formoutput = lo_delilvery_printing->print(  ).
ENDFUNCTION.
