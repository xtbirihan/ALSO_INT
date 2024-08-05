*&---------------------------------------------------------------------*
*& Report ZINT_000_P_TABLE_CONNECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zint_000_p_table_connection.

INCLUDE zint_000_p_table_conn_top.
INCLUDE zint_000_p_table_conn_zor.
INCLUDE zint_000_p_table_conn_f01.
INCLUDE zint_000_p_table_conn_o01.
INCLUDE zint_000_p_table_conn_i01.

START-OF-SELECTION.
  CALL SCREEN 9000.
