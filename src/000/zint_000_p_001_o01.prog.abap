*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_001_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'S9000'.
  SET TITLEBAR 'T9000'.
  DATA: gro_doc_container TYPE REF TO cl_gui_docking_container.

  IF go_grid1 IS NOT BOUND.
    gs_layout1-cwidth_opt = 'X'.
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      CREATE OBJECT go_custom_container
        EXPORTING
          container_name = 'CUSTOM_CONTAINER'.

      CREATE OBJECT splitter
        EXPORTING
          parent  = go_custom_container
          rows    = 3
          columns = 1
          align   = 15.

      CALL METHOD splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = go_container1.

      splitter->set_row_height( EXPORTING id = 1 height = 100 ).
      splitter->set_column_width( EXPORTING id = 1 width = 100 ).
    ELSE."\ For background processing
      CREATE OBJECT go_grid1
        EXPORTING
          i_parent = gro_doc_container.
    ENDIF.
    DATA: lt_button TYPE TABLE OF stb_button,
          ls_button TYPE stb_button.
    ls_button = VALUE #( function = '9001_REWORK' butn_type = 0 text = TEXT-013 icon = '@KO@' ). APPEND ls_button TO lt_button.
*    ls_button = VALUE #( function = '9001_KAYDET' butn_type = 0 text = TEXT-009 icon = '@68@' ). APPEND ls_button TO lt_button.
*    ls_button = VALUE #( function = '9001_SIL' butn_type = 0 text = TEXT-012 icon = '@11@' ). APPEND ls_button TO lt_button.

    DATA: lt_property9001 LIKE TABLE OF gs_property.
    lt_property9001 = VALUE #(
     ( fieldname = 'FLG' property = 'TECH' value = 'X')
     ( fieldname = 'REWORK' property = 'TECH' value = 'X')
     ( fieldname = 'TECHNICAL_NAME' property = 'TECH' value = 'X')
     ).
    gs_layout1-ctab_fname  = 'CELLCOLOR'.
    zor_cl_local=>list_alv(
       EXPORTING
         iv_structure_name = 'ZINT_000_S_ALV1'
         is_layout = gs_layout1
         it_data = 'GT_ALV1'
         iv_con_name = 'C9001'
         iv_split_con = 'X'
         it_add_button = lt_button[]
         it_fcat_change = lt_property9001
         iv_hotspot = 'X'
         iv_save = 'X'
         iv_double_click = 'X'
       CHANGING
         co_custom_container = go_custom_container
         co_gui_container = go_container1
         co_grid = go_grid1
         ct_fcat = gt_fcat1
         ).

  ENDIF.

ENDMODULE.
