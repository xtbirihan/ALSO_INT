*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_TABLE_CONN_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form command_getir
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM command_getir .
  CLEAR: gt_9001 , gt_9002.
  IF gs_header-int_type IS INITIAL.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF gs_header-int_subtype IS INITIAL.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR: gt_9001.
  SELECT SINGLE *
       FROM zint_000_t_002
       INTO @DATA(ls_002)
       WHERE int_type EQ @gs_header-int_type AND
             int_subtype EQ @gs_header-int_subtype.
  CHECK sy-subrc EQ 0.
  gs_header-description = ls_002-description.

  DATA: ls_9001 LIKE LINE OF gt_9001.
  ls_9001-tabname = 'ZINT_000_T_003'. APPEND ls_9001 TO gt_9001.
  IF ls_002-table_header IS NOT INITIAL.
    ls_9001-tabname = ls_002-table_header. APPEND ls_9001 TO gt_9001.
  ENDIF.
  IF ls_002-table_item IS NOT INITIAL.
    ls_9001-tabname = ls_002-table_item. APPEND ls_9001 TO gt_9001.
  ENDIF.
  IF ls_002-table_item_detail IS NOT INITIAL.
    ls_9001-tabname = ls_002-table_item_detail. APPEND ls_9001 TO gt_9001.
  ENDIF.

  SELECT *
    FROM zint_000_t_008
    INTO TABLE @DATA(lt_008)
    WHERE int_type EQ @gs_header-int_type AND
             int_subtype EQ @gs_header-int_subtype.

  LOOP AT lt_008 INTO DATA(ls_008).
    APPEND INITIAL LINE TO gt_9002 REFERENCE INTO DATA(lr_9002).
    MOVE-CORRESPONDING ls_008 TO lr_9002->*.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form command_save
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM command_save .

  DATA: ls_008 TYPE zint_000_t_008,
        lt_008 TYPE TABLE OF zint_000_t_008.
  LOOP AT gt_9002 INTO DATA(ls_9002).
    CLEAR: ls_008.
    ls_008 = VALUE #(
    int_type = gs_header-int_type
    int_subtype = gs_header-int_subtype
    parent_table = ls_9002-parent_table
    parent_field = ls_9002-parent_field
    operation = ls_9002-operation
    child_table = ls_9002-child_table
    child_field = ls_9002-child_field
    ).
    APPEND ls_008 TO lt_008.
  ENDLOOP.

  IF lt_008 IS NOT INITIAL.
    DELETE FROM zint_000_t_008 WHERE int_type = gs_header-int_type AND int_subtype = gs_header-int_subtype. COMMIT WORK.
    MODIFY zint_000_t_008 FROM TABLE lt_008. COMMIT WORK.
    MESSAGE TEXT-003 TYPE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form command_add_row
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM command_add_row .

  DATA: ls_9002 LIKE LINE OF gt_9002.
  ls_9002-operation = 'EQ'.
  APPEND ls_9002 TO gt_9002.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form command_delete_row
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM command_delete_row .
  go_grid9002->get_selected_rows( IMPORTING et_row_no = DATA(lt_rows) ).
  SORT lt_rows BY row_id DESCENDING.
  LOOP AT lt_rows INTO DATA(ls_rows).
    DELETE gt_9002 INDEX ls_rows-row_id.
  ENDLOOP.
ENDFORM.
