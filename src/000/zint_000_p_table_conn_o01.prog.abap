*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_TABLE_CONN_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'S9000'.
  SET TITLEBAR 'T9000'.

  IF go_grid9001 IS NOT BOUND.
    gs_layout9001-cwidth_opt = 'X'.
    zor_cl_local=>list_alv(
       EXPORTING
         iv_structure_name = 'ZINT_000_S_TAB1'
         is_layout = gs_layout9001
         it_data = 'GT_9001'
         iv_con_name = 'CON9001'
         iv_no_toolbar = 'X'
         iv_save = 'A'
       CHANGING
         co_custom_container = go_container9001
         co_grid = go_grid9001
         ct_fcat = gt_fcat9001
         ).
  ELSE.
    go_grid9001->refresh_table_display( ).
  ENDIF.

  IF go_grid9002 IS NOT BOUND.
*    gs_layout9002-cwidth_opt = 'X'.
    DATA: lt_property9002 LIKE TABLE OF gs_property,
          lt_exclude      TYPE ui_functions,
          lt_button       TYPE TABLE OF stb_button.
    lt_property9002 = VALUE #(
      ( fieldname = 'PARENT_TABLE' property = 'EDIT' value = 'X' )
      ( fieldname = 'PARENT_FIELD' property = 'EDIT' value = 'X' )
      ( fieldname = 'CHILD_TABLE' property = 'EDIT' value = 'X' )
      ( fieldname = 'CHILD_FIELD' property = 'EDIT' value = 'X' )
    ).

    lt_exclude = VALUE #(
                           (  cl_gui_alv_grid=>mc_fc_loc_delete_row )
                           (  cl_gui_alv_grid=>mc_fc_loc_append_row )
                           (  cl_gui_alv_grid=>mc_fc_loc_insert_row )
                           (  cl_gui_alv_grid=>mc_fc_loc_cut )
                           (  cl_gui_alv_grid=>mc_fc_loc_paste )
                           (  cl_gui_alv_grid=>mc_fc_loc_copy_row )
                           (  cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                           (  cl_gui_alv_grid=>mc_fc_loc_undo )
                           (  cl_gui_alv_grid=>mc_fc_loc_copy )
                         ).
    zor_cl_local=>list_alv(
       EXPORTING
         iv_structure_name = 'ZINT_000_S_TAB2'
         is_layout = gs_layout9002
         it_data = 'GT_9002'
         iv_con_name = 'CON9002'
         it_fcat_change = lt_property9002
         it_add_button = lt_button
         iv_no_toolbar = 'X'
         it_exclude = lt_exclude
         iv_save = 'A'
         iv_editable = 'X'
       CHANGING
         co_custom_container = go_container9002
         co_grid = go_grid9002
         ct_fcat = gt_fcat9002
         ).
  ELSE.
    go_grid9002->refresh_table_display( ).
  ENDIF.

ENDMODULE.
