CLASS zcl_int_package_scale DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_weight IMPORTING iv_ip_address              TYPE string
                                 iv_port                    TYPE string
                                 VALUE(iv_frame_terminator) TYPE string OPTIONAL
                                 VALUE(iv_message_to_send)  TYPE string OPTIONAL
                                 VALUE(iv_unit_of_measure)  TYPE string OPTIONAL
                       RETURNING VALUE(rv_weight)           TYPE dec16_3.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_default_frame_terminator TYPE string VALUE '0A'.
    CONSTANTS c_default_message_to_send TYPE string VALUE 'q$'.
    CONSTANTS c_default_unit_of_measure TYPE string VALUE 'kg'.
ENDCLASS.



CLASS ZCL_INT_PACKAGE_SCALE IMPLEMENTATION.


  METHOD get_weight.
**********************************************************************
*& Key           : AD-240208
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Connects to the package scale, using the provided information, and
*& returns its current weight.
**********************************************************************

    DATA lv_position_of_first_number TYPE i.
    DATA lv_position_of_unit TYPE i.

    IF iv_frame_terminator IS NOT SUPPLIED.
      iv_frame_terminator  = c_default_frame_terminator.
    ENDIF.

    IF iv_unit_of_measure IS NOT SUPPLIED.
      iv_unit_of_measure = c_default_unit_of_measure.
    ENDIF.

    IF iv_message_to_send IS NOT SUPPLIED.
      iv_message_to_send = c_default_message_to_send.
    ENDIF.

    NEW zcl_core_apc_tcp_request( )->process_tcp_request( EXPORTING
                                                             iv_destination_ip = iv_ip_address
                                                             iv_destination_port = iv_port
                                                             iv_frame_terminator = iv_frame_terminator
                                                             iv_message_to_send = iv_message_to_send
                                                          IMPORTING
                                                             ev_return_code = DATA(lv_return_code)
                                                             ev_return_message = DATA(lv_return_message) ).

    IF lv_return_code <> 0.
      CLEAR rv_weight.
      RETURN.
    ENDIF.

    " Find the position of the first nummeric char in the string.
    FIND FIRST OCCURRENCE OF PCRE '[0-9]' IN lv_return_message MATCH OFFSET lv_position_of_first_number.
    " Find position of the unit of measure in the stirng.
    FIND FIRST OCCURRENCE OF PCRE iv_unit_of_measure IN lv_return_message MATCH OFFSET lv_position_of_unit.

    IF lv_position_of_first_number <= 0 OR lv_position_of_unit <= 0.
      CLEAR rv_weight.
      RETURN.
    ENDIF.

    " Get length of the weight part.
    DATA(lv_length) = lv_position_of_unit - lv_position_of_first_number.

    IF lv_length <= 0.
      CLEAR rv_weight.
      RETURN.
    ENDIF.

    DATA(weigth_part_of_string) = substring( val =  lv_return_message off = lv_position_of_first_number len = lv_length ).
    weigth_part_of_string = replace( val = weigth_part_of_string sub = ',' with = '.').

    IF weigth_part_of_string CN '0123456789.'.
      CLEAR rv_weight.
      RETURN.
    ENDIF.

    rv_weight = weigth_part_of_string.
  ENDMETHOD.
ENDCLASS.
