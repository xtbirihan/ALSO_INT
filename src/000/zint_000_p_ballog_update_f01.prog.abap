*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_BALLOG_UPDATE_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data .

  SELECT *
    FROM zint_000_t_900
    INTO TABLE @DATA(lt_900).
  CHECK sy-subrc EQ 0.

  LOOP AT lt_900 INTO DATA(ls_900).

    IF go_ballog IS NOT INITIAL.
      FREE go_ballog.
    ENDIF.
    CREATE OBJECT go_ballog.
    DATA(lv_handle) = go_ballog->log_create( iv_int_type = ls_900-int_type iv_int_subtype = ls_900-int_subtype iv_send = ls_900-send ).

    LOOP AT lt_900 INTO DATA(ls_900_2) WHERE int_type EQ ls_900-int_type AND int_subtype = ls_900-int_subtype AND guid = ls_900-guid.
      go_ballog->log_add( is_message = VALUE #( msgty = ls_900_2-type msgid = ls_900_2-id msgno = ls_900_2-m_number msgv1 = ls_900_2-message_v1 msgv2 = ls_900_2-message_v2 msgv3 = ls_900_2-message_v3 msgv4 = ls_900_2-message_v4  ) ).
    ENDLOOP.
    IF sy-subrc EQ 0.
      DELETE lt_900 WHERE int_type EQ ls_900-int_type AND int_subtype = ls_900-int_subtype AND guid = ls_900-guid.
      DELETE FROM zint_000_t_900 WHERE int_type = ls_900-int_type AND int_subtype = ls_900-int_subtype AND guid = ls_900-guid. COMMIT WORK.
      go_ballog->log_save( EXPORTING iv_guid = ls_900-guid iv_commit = 'X' IMPORTING es_logno = DATA(ls_logno) ).
    ENDIF.
  ENDLOOP.

ENDFORM.
