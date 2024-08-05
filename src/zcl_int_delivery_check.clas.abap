CLASS zcl_int_delivery_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS barcode_is_valid IMPORTING iv_barcode                 TYPE zde_barcode
                                       iv_entitled                TYPE /scwm/de_entitled
                             RETURNING VALUE(rv_barcode_is_valid) TYPE abap_bool.
    METHODS barcode_lenght_is_valid IMPORTING iv_barcode                TYPE zde_barcode
                                    RETURNING VALUE(rv_lenght_is_valid) TYPE abap_bool.
    METHODS barcode_is_numberic IMPORTING iv_barcode                    TYPE zde_barcode
                                RETURNING VALUE(rv_barcode_is_numberic) TYPE abap_bool.
    METHODS barcode_checksum_is_valid IMPORTING iv_barcode                          TYPE zde_barcode
                                      RETURNING VALUE(rv_barcode_checksum_is_valid) TYPE abap_bool.
    METHODS barcode_companycode_is_valid IMPORTING iv_barcode                          TYPE zde_barcode
                                                   iv_entitled                         TYPE /scwm/de_entitled
                                         RETURNING VALUE(rv_barcode_compcode_is_valid) TYPE abap_bool.
    METHODS shipmentnumber_is_valid IMPORTING iv_shipmentnumber                 TYPE zde_inbship
                                    RETURNING VALUE(rv_shipmentnumber_is_valid) TYPE abap_bool.

    METHODS shipmentnumber_has_hu IMPORTING iv_shipmentnumber               TYPE zde_inbship
                                  RETURNING VALUE(rv_shipmentnumber_has_hu) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_barcode_lenght TYPE int4 VALUE 12.
    CONSTANTS c_barcode_allowed_chars TYPE c LENGTH 10 VALUE '0123456789'.
    CONSTANTS c_multiplier_even TYPE int8 VALUE 1.
    CONSTANTS c_multiplier_odd TYPE int8 VALUE 3.
ENDCLASS.



CLASS ZCL_INT_DELIVERY_CHECK IMPLEMENTATION.


  METHOD barcode_checksum_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Check, if the barcode checksum is valid
*&
**********************************************************************
    DATA(lv_check_sum) = CONV int8( iv_barcode(1) * c_multiplier_odd   +
                                    iv_barcode+1(1) * c_multiplier_even +
                                    iv_barcode+2(1) * c_multiplier_odd +
                                    iv_barcode+3(1) * c_multiplier_even +
                                    iv_barcode+4(1) * c_multiplier_odd +
                                    iv_barcode+5(1) * c_multiplier_even +
                                    iv_barcode+6(1) * c_multiplier_odd +
                                    iv_barcode+7(1) * c_multiplier_even +
                                    iv_barcode+8(1) * c_multiplier_odd +
                                    iv_barcode+9(1) * c_multiplier_even +
                                    iv_barcode+10(1) * c_multiplier_odd ).
    " Get Check digit
    DATA(lv_check_digit) = CONV int8( iv_barcode+11(1) ).

    " Calculate Modulo 10 of sum.
    DATA(lv_modulo_result) = lv_check_sum MOD 10.

    " For Barcodes with the check digit 0 we have to override the modulo result.
    IF lv_modulo_result = 0.
      lv_modulo_result = 10.
    ENDIF.

    " Compare modulo result and check digit.
    IF ( 10 - lv_modulo_result ) = lv_check_digit.
      rv_barcode_checksum_is_valid = abap_true.
    ELSE.
      rv_barcode_checksum_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD barcode_companycode_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Check, if the company code at the beginning of the barcode is valid
*&
**********************************************************************
    SELECT SINGLE comp_code
      INTO @DATA(lv_comp_code)
      FROM ztinb_entitled
     WHERE entitled = @iv_entitled.

    IF sy-subrc <> 0.
      rv_barcode_compcode_is_valid = abap_false.
      RETURN.
    ENDIF.

    IF lv_comp_code = iv_barcode(4).
      rv_barcode_compcode_is_valid = abap_true.
    ELSE.
      rv_barcode_compcode_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD barcode_is_numberic.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Check, if the barcode is numeric (contains only numbers)
*&
**********************************************************************
    IF iv_barcode CO c_barcode_allowed_chars.
      rv_barcode_is_numberic = abap_true.
    ELSE.
      rv_barcode_is_numberic = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD barcode_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Performs several checks, to determine if the barcode is valid.
*& The method will stop the check after the first violation.
**********************************************************************
    "----------------------------------
    " Check: Barcode length
    "----------------------------------
    IF NOT barcode_lenght_is_valid( iv_barcode ).
      rv_barcode_is_valid = abap_false.
      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Barcode contains only numbers
    "----------------------------------
    IF NOT barcode_is_numberic( iv_barcode ).
      rv_barcode_is_valid = abap_false.
      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Barcode check sum is valid
    "----------------------------------
    IF NOT barcode_checksum_is_valid( iv_barcode ).
      rv_barcode_is_valid = abap_false.
      RETURN.
    ENDIF.

    "----------------------------------
    " Check: Company Code in Barcode (First 4 digits of barcode) is valid
    "----------------------------------
    IF NOT barcode_companycode_is_valid( iv_barcode = iv_barcode iv_entitled = iv_entitled ).
      rv_barcode_is_valid = abap_false.
      RETURN.
    ENDIF.

    " All checks passed sucsessfully
    rv_barcode_is_valid = abap_true.
  ENDMETHOD.


  METHOD barcode_lenght_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Check, if the b has the correct lenght
*&
**********************************************************************
    IF strlen( iv_barcode ) = c_barcode_lenght.
      rv_lenght_is_valid = abap_true.
    ELSE.
      rv_lenght_is_valid = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD shipmentnumber_has_hu.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Check, if the shipment number can be found in a handling unit
*&
**********************************************************************
    SELECT COUNT( * ) UP TO 1 ROWS
      INTO @DATA(lv_counter)
      FROM /scwm/hu_ident
     WHERE idart = 'I'
     AND huident = @iv_shipmentnumber.

    IF lv_counter > 0.
      rv_shipmentnumber_has_hu = abap_true.
    ELSE.
      rv_shipmentnumber_has_hu = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD shipmentnumber_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   :
**********************************************************************
*& Description (short)
*& Performs several checks, to determine if the shipmentnumber is valid.
*& The method will stop the check after the first violation.
**********************************************************************
    IF NOT shipmentnumber_has_hu( iv_shipmentnumber ).
      rv_shipmentnumber_is_valid = abap_false.
      RETURN.
    ENDIF.

    " All checks passed sucsessfully
    rv_shipmentnumber_is_valid = abap_true.
  ENDMETHOD.
ENDCLASS.
