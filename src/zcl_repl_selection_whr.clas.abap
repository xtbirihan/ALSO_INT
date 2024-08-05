CLASS zcl_repl_selection_whr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_repl_selection_whr .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REPL_SELECTION_WHR IMPLEMENTATION.


  METHOD /scwm/if_ex_repl_selection_whr~selection_whr.
********************************************************************
*& Key          : <BSUGAREV>-11.05.2023 12:56:01
*& Request No.  :
********************************************************************
*& Description  : Add all doc. items from failed waves (wave status = "E")
*&
********************************************************************
    DATA: lt_dlv_item TYPE  /scwm/dlv_item_out_prd_tab.

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_repl_selection_whr.

    IF zcl_switch=>get_switch_state(
          iv_lgnum = iv_lgnum  iv_devid = zif_switch_const=>c_zint_002 ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/WAVE_SELECT'
      EXPORTING
        ir_status   = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                          option = wmegc_option_eq
                                          low    = wmegc_stwave_error ) )
      IMPORTING
        et_dlv_item = lt_dlv_item.

    APPEND LINES OF lt_dlv_item TO ct_dlv_item.

    SORT ct_dlv_item BY docid itemid.
    DELETE ADJACENT DUPLICATES FROM ct_dlv_item COMPARING docid itemid.
  ENDMETHOD.
ENDCLASS.
