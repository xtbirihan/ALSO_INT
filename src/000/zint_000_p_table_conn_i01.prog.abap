*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_TABLE_CONN_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA(lv_ucomm) = okcode.
  CLEAR: okcode.
  CASE lv_ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'GETIR'.
      PERFORM command_getir.
    WHEN 'SAVE'.
      PERFORM command_save.
    WHEN 'A_ROW'.
      PERFORM command_add_row.
    WHEN 'D_ROW'.
      PERFORM command_delete_row.
  ENDCASE.

  IF gs_header-int_type IS NOT INITIAL AND gs_header-int_subtype IS NOT INITIAL.
    IF gs_header-description IS NOT INITIAL.
      SELECT SINGLE *
        FROM zint_000_t_002
        INTO @DATA(ls_002)
        WHERE int_type EQ @gs_header-int_type AND
              int_subtype EQ @gs_header-int_subtype.
      IF sy-subrc EQ 0.
        gs_header-description = ls_002-description.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: gs_header-description.
  ENDIF.
ENDMODULE.
