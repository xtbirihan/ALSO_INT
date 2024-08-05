class ZCL_INT_PRINT_MANAGEMENT definition
  public
  final
  create public .

public section.

  types:
    tt_vind_data TYPE STANDARD TABLE OF zaf_vind_data_i WITH EMPTY KEY .

  constants C_DELIVERY_NOTE_DEFAULT type TDSFNAME value '/SCWM/DLV_NOTE' ##NO_TEXT.
  constants C_DELIVERY_NOTE type TDSFNAME value 'ZAF_DELNOTE' ##NO_TEXT.
  constants C_RETOURE type TDSFNAME value 'ZAF_RETOURE' ##NO_TEXT.
  constants C_DELIVERY_NOTE_FUNCTION type FUNCNAME value 'Z_INT_DELIVERY_PRINTING' ##NO_TEXT.
  constants C_RETOURE_FUNCTION type FUNCNAME value 'Z_INT_RETOURE_PRINTING' ##NO_TEXT.
  class-data MV_FORM type TDSFNAME .
  class-data MV_PROGNAME type PROGNAME .

  class-methods DETERMINE_FUNCTION_NAME
    importing
      !IV_FORM type TDSFNAME
    changing
      !CV_FUNCTION_NAME type FUNCNAME
      !CV_INTERFACE_TYPE type FPINTERFACETYPE .
  class-methods GET_FUNCNAME
    importing
      !IV_FORMNAME type TDSFNAME
      !IV_PROGNAME type PROGNAME
    changing
      !CV_FUNCNAME type FUNCNAME .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INT_PRINT_MANAGEMENT IMPLEMENTATION.


  METHOD determine_function_name.
**********************************************************************
*& Key           : AD-230413
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the function name and interface type for custom forms
*& This is used to override the standard print behavior.
**********************************************************************
    DATA lt_call_stack TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = lt_call_stack.

    " Call Stack Level 1 = Class ZCL_INT_PRINT_MANAGEMENT->DETERMINE_FUNCTION_NAME
    " Call Stack Level 2 = Function Module FP_FUNCTION_MODULE_NAME
    " Call Stack Level 3 = Either the Z- Function Module or the default Program.
    IF VALUE #( lt_call_stack[ 3 ]-progname OPTIONAL ) = c_delivery_note_function.
      " If the call comes from within the Z- function module we cannot override the function name again.
      " This would create an infinite loop.
      RETURN.
    ENDIF.

    CASE iv_form.
      WHEN c_delivery_note.
        cv_function_name = c_delivery_note_function.
        CLEAR cv_interface_type.
    ENDCASE.
  ENDMETHOD.


  METHOD get_funcname.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get function name
**********************************************************************
    DATA: lt_callstack TYPE sys_callst,
          lv_run       TYPE boolean VALUE abap_on.

    " Check / Call ME
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = lt_callstack.

    LOOP AT lt_callstack TRANSPORTING NO FIELDS FROM 2 WHERE progname CS 'ZCL_INT_DELIVERY_PRINTING'
                                                          OR progname CS 'ZCL_INT_RETOURE_PRINTING'.
      lv_run = abap_off.
      EXIT.
    ENDLOOP.

    CHECK lv_run = abap_on.

    TYPES: BEGIN OF ts_func,
             formname TYPE tdsfname,
             funcname TYPE funcname,
           END OF ts_func.

    TYPES: tt_func     TYPE STANDARD TABLE OF ts_func WITH EMPTY KEY.

    DATA(l_it_form) = VALUE tt_func( funcname = zcl_int_print_management=>c_delivery_note_function ( formname = zcl_int_print_management=>c_delivery_note ) " AF Lieferschein
                                                                                                   ( formname = '/SCWM/DLV_NOTE' )                          " SAP Standard Lieferschein
                                     funcname = zcl_int_print_management=>c_retoure_function       ( formname = 'ZAF_RETOURE_ZVT1' )                        " AF Retoure Versatel
                                                                                                   ( formname = 'ZAF_RETOURE_ZHTP' )                        " AF Retoure HTP
                                                                                                   ( formname = 'ZAF_RETOURE_ZEW1' ) ).                     " AF Retoure SWB

    mv_form     = iv_formname.
    " --- TEST --------------------------------------------------
*    mv_form     = 'ZAF_RETOURE_ZTV1'.
    " -----------------------------------------------------------

    cv_funcname = VALUE #( l_it_form[ formname = mv_form ]-funcname OPTIONAL ).
    IF cv_funcname IS INITIAL.
      CLEAR mv_form.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
