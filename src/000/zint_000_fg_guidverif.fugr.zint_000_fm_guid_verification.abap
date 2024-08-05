FUNCTION zint_000_fm_guid_verification .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_CHECK_GUID) TYPE  STRING
*"     REFERENCE(IV_CHECK_TRAN_NR) TYPE  STRING
*"     REFERENCE(IT_RETURN) TYPE  BAPIRET2_TAB
*"     REFERENCE(IT_HEADER_MESSAGE) TYPE
*"        ZINT_000_DT_GUID_CONFIRMA_TAB1 OPTIONAL
*"     REFERENCE(IT_ITEM_MESSAGE) TYPE  ZINT_000_DT_GUID_CONFIRMAT_TAB
*"       OPTIONAL
*"     REFERENCE(IT_DETAIL_MESSAGE) TYPE
*"        ZINT_000_DT_GUID_CONFIRMA_TAB2 OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_VERIFICATION_GUID) TYPE  ZINT_000_DE_GUID
*"----------------------------------------------------------------------

  CHECK it_header_message[] IS NOT INITIAL OR
        it_item_message[] IS NOT INITIAL OR
        it_detail_message[] IS NOT INITIAL .

  DATA(lo_verification) = NEW zint_000_co_si_guid_confirmati( ).
  DATA(ls_verification) = VALUE zint_000_mt_guid_confirmation( ).

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZINT_GUID'
    IMPORTING
      number                  = ev_verification_guid
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  ls_verification-mt_guid_confirmation_req-header-tran_nr = '22'.
  ls_verification-mt_guid_confirmation_req-header-guid = ev_verification_guid.
  ls_verification-mt_guid_confirmation_req-data-check_guid = iv_check_guid.
  ls_verification-mt_guid_confirmation_req-data-check_tran_nr = iv_check_tran_nr.
  ls_verification-mt_guid_confirmation_req-data-header_message = it_header_message.
  ls_verification-mt_guid_confirmation_req-data-item_message = it_item_message.
  ls_verification-mt_guid_confirmation_req-data-detail_message = it_detail_message.

  TRY .
      lo_verification->si_guid_confirmation_asyn_out( ls_verification ).
    CATCH cx_ai_system_fault INTO DATA(lx_ai_system_fault).
  ENDTRY.

ENDFUNCTION.
