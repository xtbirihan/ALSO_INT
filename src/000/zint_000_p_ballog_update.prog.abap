*&---------------------------------------------------------------------*
*& Report ZINT_000_P_BALLOG_UPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zint_000_p_ballog_update.

INCLUDE zint_000_p_ballog_update_top.
INCLUDE zint_000_p_ballog_update_f01.

START-OF-SELECTION.
  PERFORM process_data.
