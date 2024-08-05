*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_001_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form list_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM list_screen .
  CALL SCREEN 9000.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form sh_sitype
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sh_sitype .

  IF s_itype-low IS INITIAL.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA: BEGIN OF ls_shelp,
          int_type    LIKE zint_000_t_002-int_type,
          int_subtype LIKE zint_000_t_002-int_subtype,
          description LIKE zint_000_t_002-description,
        END OF ls_shelp.
  DATA: lt_shelp LIKE TABLE OF ls_shelp.
  DATA: lt_return TYPE TABLE OF ddshretval WITH HEADER LINE.

  SELECT int_type , int_subtype , description
    FROM zint_000_t_002
    INTO CORRESPONDING FIELDS OF TABLE @lt_shelp
    WHERE int_type = @s_itype-low.
  SORT lt_shelp BY int_subtype ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INT_SUBTYPE'
      value_org       = 'S'
    TABLES
      value_tab       = lt_shelp
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  s_sitype-low = lt_return-fieldval.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  CLEAR: gt_alv1.

  SELECT *
    FROM zint_000_t_004
    INTO TABLE @DATA(lt_004).

  DATA: lr_direction TYPE RANGE OF zint_000_t_002-direction.
  DATA: lr_statu TYPE RANGE OF zint_000_t_003-statu.
  IF pa_inb EQ 'X'.
    APPEND INITIAL LINE TO lr_direction REFERENCE INTO DATA(lr_dir).
    lr_dir->sign = 'I'. lr_dir->option = 'EQ'. lr_dir->low = 'I'.
  ELSE.
    APPEND INITIAL LINE TO lr_direction REFERENCE INTO lr_dir.
    lr_dir->sign = 'E'. lr_dir->option = 'EQ'. lr_dir->low = 'I'.
  ENDIF.
  IF pa_outb EQ 'X'.
    APPEND INITIAL LINE TO lr_direction REFERENCE INTO lr_dir.
    lr_dir->sign = 'I'. lr_dir->option = 'EQ'. lr_dir->low = 'O'.
  ELSE.
    APPEND INITIAL LINE TO lr_direction REFERENCE INTO lr_dir.
    lr_dir->sign = 'E'. lr_dir->option = 'EQ'. lr_dir->low = 'O'.
  ENDIF.

  LOOP AT lt_004 INTO DATA(ls_004).
    IF ls_004-type EQ 'S'.
      IF pa_ok EQ 'X'.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO DATA(lr_st).
        lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ELSE.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO lr_st.
        lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ENDIF.
    ELSEIF ls_004-type EQ 'E'.
      IF pa_nok EQ 'X'.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO lr_st.
        lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ELSE.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO lr_st.
        lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ENDIF.
    ELSEIF ls_004-type EQ 'W'.
      IF pa_non EQ 'X'.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO lr_st.
        lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ELSE.
        APPEND INITIAL LINE TO lr_statu REFERENCE INTO lr_st.
        lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
      ENDIF.
    ENDIF.
  ENDLOOP.


  SELECT a~int_type , a~int_subtype , b~description , b~direction , a~guid , a~statu , a~datum , a~uzeit , a~username , b~rework , b~technical_name
    FROM zint_000_t_003 AS a INNER JOIN zint_000_t_002 AS b ON
    b~int_type EQ a~int_type AND
    b~int_subtype EQ a~int_subtype
    INNER JOIN zint_000_t_001 AS c ON
    c~int_type EQ a~int_type
    INTO TABLE @DATA(lt_003)
    WHERE a~int_type IN @s_itype AND
          a~int_subtype IN @s_sitype AND
          a~guid IN @s_guid AND
          a~datum IN @s_datum AND
          a~uzeit IN @s_uzeit AND
          a~username IN @s_user AND
          a~statu IN @lr_statu AND
          b~direction IN @lr_direction.

  CHECK lt_003 IS NOT INITIAL.
  SORT lt_003 BY datum DESCENDING uzeit DESCENDING.

  LOOP AT lt_003 INTO DATA(ls_003).
    CLEAR: gs_alv1.

    gs_alv1 = VALUE #(
    int_type = ls_003-int_type
    int_subtype = ls_003-int_subtype
    description = ls_003-description
    direction = ls_003-direction
    guid = ls_003-guid
    statu = ls_003-statu
    datum = ls_003-datum
    uzeit = ls_003-uzeit
    username = ls_003-username
    rework = ls_003-rework
    technical_name = ls_003-technical_name
    ).

    READ TABLE lt_004 INTO ls_004 WITH KEY direction = ls_003-direction statu = ls_003-statu.
    IF sy-subrc EQ 0.
      gs_alv1-statu_tanim = ls_004-description.
      IF ls_004-type EQ 'W'.
        gs_alv1-durum = '@09@'.
      ELSEIF ls_004-type EQ 'S'.
        gs_alv1-durum = '@08@'.
      ELSEIF ls_004-type EQ 'E'.
        gs_alv1-durum = '@0A@'.
      ENDIF.
    ENDIF.

    IF gs_alv1-direction = 'O'.
      gs_alv1-direction_icon = '@9T@'.
    ELSEIF gs_alv1-direction EQ 'I'.
      gs_alv1-direction_icon = '@9S@'.
    ENDIF.

    APPEND gs_alv1 TO gt_alv1.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .
  lv_icon1 = icon_arrow_left.
  lv_icon2 = icon_arrow_right.
  lv_icon3 = icon_green_light.
  lv_icon4 = icon_yellow_light.
  lv_icon5 = icon_red_light.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form double_click_grid1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM double_click_grid1 .
  CLEAR: gs_alv2_kirilim , gt_alv2_kirilim.
  "Her ihtimale karşı grid temizleniyor
  IF go_grid2 IS BOUND.
    go_grid2->free( ).
    CLEAR: go_grid2.
  ENDIF.
  IF go_grid2_3 IS BOUND.
    go_grid2_3->free( ).
    CLEAR: go_grid2_3.
  ENDIF.
  IF go_grid2_2 IS BOUND.
    go_grid2_2->free( ).
    CLEAR: go_grid2_2.
  ENDIF.
  IF go_grid2_1 IS BOUND.
    go_grid2_1->free( ).
    CLEAR: go_grid2_1.
  ENDIF.
  IF go_grid3_1 IS BOUND.
    go_grid3_1->free( ).
    CLEAR: go_grid3_1.
  ENDIF.
  IF go_grid3_2 IS BOUND.
    go_grid3_2->free( ).
    CLEAR: go_grid3_2.
  ENDIF.
  IF go_container2 IS BOUND.
    IF go_container2_3 IS BOUND.
      go_container2_3->free( ).
      CLEAR:go_container2_3.
    ENDIF.
    IF go_container2_2 IS BOUND.
      go_container2_2->free( ).
      CLEAR:go_container2_2.
    ENDIF.
    IF go_container2_1 IS BOUND.
      go_container2_1->free( ).
      CLEAR:go_container2_1.
    ENDIF.
    IF splitter_2 IS BOUND.
      splitter_2->free( ).
      CLEAR: splitter_2.
    ENDIF.
  ENDIF.
  IF go_container3 IS BOUND.
    IF go_container3_1 IS BOUND.
      go_container3_1->free( ).
      CLEAR: go_container3_1.
    ENDIF.
    IF go_container3_2 IS BOUND.
      go_container3_2->free( ).
      CLEAR: go_container3_2.
    ENDIF.
    IF splitter_3 IS BOUND.
      splitter_3->free( ).
      CLEAR: splitter_3.
    ENDIF.
  ENDIF.

*  PERFORM get_data_alv2.

  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
  IF sy-subrc EQ 0.
    DATA: lv_lines TYPE i.
    SELECT SINGLE *
    FROM zint_000_t_002
    INTO @DATA(ls_002)
    WHERE int_type = @ls_alv1-int_type AND
          int_subtype = @ls_alv1-int_subtype.
    IF ls_002-table_header IS NOT INITIAL.
      lv_lines = lv_lines + 1.
    ENDIF.
    IF ls_002-table_item IS NOT INITIAL.
      lv_lines = lv_lines + 1.
    ENDIF.
    IF ls_002-table_item_detail IS NOT INITIAL.
      lv_lines = lv_lines + 1.
    ENDIF.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = go_container2.
    CREATE OBJECT splitter_2
      EXPORTING
        parent  = go_container2
        rows    = lv_lines
        columns = 1
        align   = 15.
  ENDIF.
  DATA: lv_rowno TYPE lvc_index .
  PERFORM get_alv2_grid_connections USING 'GO_GRID1' lv_rowno.

  PERFORM get_data_alv3.
  IF go_grid3_1 IS NOT BOUND.

    DATA: lt_property9003 LIKE TABLE OF gs_property.
    lt_property9003 = VALUE #(
    ( fieldname = 'INT_TYPE' property = 'TECH' value = 'X' )
    ( fieldname = 'INT_SUBTYPE' property = 'TECH' value = 'X' )
    ( fieldname = 'GUID' property = 'TECH' value = 'X' )
    ( fieldname = 'BALLOG_ID' property = 'TECH' value = 'X' )
    ( fieldname = 'BALLOG_NUMBER' property = 'TECH' value = 'X' )
    ( fieldname = 'FLG' property = 'TECH' value = 'X' )
    ).

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 3
        column    = 1
      RECEIVING
        container = go_container3.

    splitter->set_row_height( EXPORTING id = 1 height = 40 ).
    splitter->set_row_height( EXPORTING id = 2 height = 40 ).
    splitter->set_row_height( EXPORTING id = 3 height = 20 ).

    CREATE OBJECT splitter_3
      EXPORTING
        parent  = go_container3
        rows    = 1
        columns = 2
        align   = 15.

    CALL METHOD splitter_3->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = go_container3_1.
    splitter_3->set_column_width( EXPORTING id = 1  width = 100 ).

    gs_layout3-cwidth_opt = 'X'.
    gs_layout3-ctab_fname  = 'CELLCOLOR'.
    zor_cl_local=>list_alv(
      EXPORTING
        iv_structure_name = 'ZINT_000_S_ALV3'
        is_layout = gs_layout3
        it_data = 'GT_ALV3'
        iv_con_name = 'CON9003'
*        iv_data_change_finished = 'X'
        iv_double_click = 'X'
        iv_split_con = 'X'
*        it_add_button = lt_button[]
        it_fcat_change = lt_property9003
*        iv_editable = 'X'
         iv_no_toolbar = 'X'
*        it_exclude = lt_exclude[]
        iv_hotspot = 'X'
      CHANGING
        co_custom_container = go_custom_container
        co_gui_container = go_container3_1
        co_grid = go_grid3_1
        ct_fcat = gt_fcat3_1
        ).
    go_grid3_1->set_gridtitle( EXPORTING i_gridtitle = TEXT-011 ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_alv2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_alv2 .
  CLEAR: gt_fcat2_1 , gt_fcat2_2 , gt_fcat2_3 , gt_alv2_kirilim.
  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
  CHECK sy-subrc EQ 0.
  SELECT SINGLE *
    FROM zint_000_t_002
    INTO @DATA(ls_002)
    WHERE int_type = @ls_alv1-int_type AND
          int_subtype = @ls_alv1-int_subtype.
  IF sy-subrc EQ 0.
    CLEAR: gs_alv2_kirilim.
    IF ls_002-table_header IS NOT INITIAL.
      gs_alv2_kirilim-sira = gs_alv2_kirilim-sira + 1.
      gs_alv2_kirilim-structure = ls_002-table_header.
      APPEND gs_alv2_kirilim TO gt_alv2_kirilim.
      PERFORM get_data_alv2_1 USING ls_002-table_header ls_alv1.
    ENDIF.
    IF ls_002-table_item IS NOT INITIAL.
      gs_alv2_kirilim-sira = gs_alv2_kirilim-sira + 1.
      gs_alv2_kirilim-structure = ls_002-table_item.
      APPEND gs_alv2_kirilim TO gt_alv2_kirilim.
    ENDIF.
    IF ls_002-table_item_detail IS NOT INITIAL.
      gs_alv2_kirilim-sira = gs_alv2_kirilim-sira + 1.
      gs_alv2_kirilim-structure = ls_002-table_item_detail.
      APPEND gs_alv2_kirilim TO gt_alv2_kirilim.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zint_000_t_008
    INTO TABLE @DATA(lt_008)
    WHERE int_type = @ls_alv1-int_type AND
          int_subtype = @ls_alv1-int_subtype.
  IF sy-subrc EQ 0.


  ELSE.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM get_data_alv2_new USING iv_rowno TYPE lvc_index.
  DATA: lt_dyn_table TYPE REF TO data,
        lo_badi      TYPE REF TO modify_alv.
  TYPES: BEGIN OF ty_s_clause.
  TYPES:   line(72)  TYPE c.
  TYPES: END OF ty_s_clause.
  DATA: lt_where_clauses  TYPE STANDARD TABLE OF ty_s_clause WITH DEFAULT KEY.
  DATA : BEGIN OF ls_condtab.
           INCLUDE STRUCTURE hrcond.
  DATA : END OF ls_condtab.
  DATA : lt_condtab LIKE TABLE OF ls_condtab.
  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
  CHECK sy-subrc EQ 0.
  SELECT *
    FROM zint_000_t_008
    INTO TABLE @DATA(lt_008)
    WHERE int_type EQ @ls_alv1-int_type AND
          int_subtype EQ @ls_alv1-int_subtype.

  FIELD-SYMBOLS: <lv_tabname>    TYPE any,
                 <lt_tab>        TYPE ANY TABLE,
                 <lt_tab_temp>   TYPE STANDARD TABLE,
                 <lt_table_temp> TYPE table.
  DATA: lt_fcat TYPE lvc_t_fcat.
  LOOP AT gt_alv2_structures INTO gs_alv2_structures WHERE displayed IS INITIAL .
    UNASSIGN <lv_tabname>.
    ASSIGN (gs_alv2_structures-int_tabname) TO <lv_tabname> .
    IF <lv_tabname> IS NOT ASSIGNED.
      zor_cl_local=>build_fieldcatalog(
      EXPORTING iv_structure_name = gs_alv2_structures-tabname
      RECEIVING rt_fcat = lt_fcat[] ).

      GET BADI lo_badi FILTERS int_type = ls_alv1-int_type int_subtype = ls_alv1-int_subtype.
      CALL BADI lo_badi->modify_fcat
        EXPORTING
          iv_int_type    = ls_alv1-int_type
          iv_int_subtype = ls_alv1-int_subtype
          iv_tabname     = gs_alv2_structures-tabname
        CHANGING
          ct_fcat        = lt_fcat[].

      IF ls_alv1-technical_name EQ 'X'.
        LOOP AT lt_fcat REFERENCE INTO DATA(lr_fcat).
          lr_fcat->scrtext_l = lr_fcat->fieldname.
          lr_fcat->scrtext_m = lr_fcat->fieldname.
          lr_fcat->scrtext_s = lr_fcat->fieldname.
          lr_fcat->reptext = lr_fcat->fieldname.
        ENDLOOP.
      ENDIF.

      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog = lt_fcat[]
        IMPORTING
          ep_table        = lt_dyn_table.


      IF gs_alv2_structures-grid_name EQ 'GO_GRID2_1'..
        gt_fcat2_1[] = lt_fcat[].
        ASSIGN lt_dyn_table->* TO <gt_alv2_1>.
        ASSIGN <gt_alv2_1> TO <lt_tab>.
      ELSEIF gs_alv2_structures-grid_name EQ 'GO_GRID2_2'..
        gt_fcat2_2[] = lt_fcat[].
        ASSIGN lt_dyn_table->* TO <gt_alv2_2>.
        ASSIGN <gt_alv2_2> TO <lt_tab>.
      ELSEIF gs_alv2_structures-grid_name EQ 'GO_GRID2_3'..
        gt_fcat2_3[] = lt_fcat[].
        ASSIGN lt_dyn_table->* TO <gt_alv2_3>.
        ASSIGN <gt_alv2_3> TO <lt_tab>.
      ENDIF.


      LOOP AT lt_008 INTO DATA(ls_008) WHERE child_table EQ gs_alv2_structures-tabname.
        IF ls_008-parent_table EQ 'ZINT_000_T_003'.
          ASSIGN COMPONENT ls_008-parent_field OF STRUCTURE ls_alv1 TO FIELD-SYMBOL(<fs>).
          IF <fs> IS ASSIGNED.
            ls_condtab = VALUE #( field = ls_008-child_field opera = 'EQ' low = <fs> ). APPEND ls_condtab TO lt_condtab.
          ENDIF.
        ELSE.
          IF iv_rowno IS NOT INITIAL.
            READ TABLE gt_alv2_structures INTO DATA(ls_alv2_structures) WITH KEY tabname = ls_008-parent_table.
            IF sy-subrc EQ 0.
              UNASSIGN <lt_tab_temp>.
              ASSIGN (ls_alv2_structures-int_tabname) TO <lt_tab_temp>.
              READ TABLE <lt_tab_temp> ASSIGNING FIELD-SYMBOL(<ls_tab_temp>) INDEX iv_rowno.
              IF <ls_tab_temp> IS ASSIGNED.
                ASSIGN COMPONENT ls_008-parent_field OF STRUCTURE <ls_tab_temp> TO FIELD-SYMBOL(<fs2>).
                IF <fs2> IS ASSIGNED.
                  ls_condtab = VALUE #( field = ls_008-child_field opera = 'EQ' low = <fs2> ). APPEND ls_condtab TO lt_condtab.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
*
      ENDLOOP.
      IF sy-subrc EQ 0.
        CLEAR: lt_where_clauses.
        CALL FUNCTION 'RH_DYNAMIC_WHERE_BUILD'
          EXPORTING
            dbtable         = space " can be empty
          TABLES
            condtab         = lt_condtab
            where_clause    = lt_where_clauses
          EXCEPTIONS
            empty_condtab   = 01
            no_db_field     = 02
            unknown_db      = 03
            wrong_condition = 04.

        UNASSIGN <lt_table_temp>.
        IF gs_alv2_structures-grid_name EQ 'GO_GRID2_1'..
          SELECT *
            FROM (gs_alv2_structures-tabname)
            INTO CORRESPONDING FIELDS OF TABLE <gt_alv2_1>
            WHERE (lt_where_clauses).

*          LOOP AT <gt_alv2_1> ASSIGNING FIELD-SYMBOL(<fs1>).
*            SELECT SINGLE kunnr
*            FROM knvp
*            WHERE parvw EQ 'RE'
*              AND kunn2 EQ @<fs1>-carikod
*            INTO <fs1>-kunnr.
*          ENDLOOP.

          ASSIGN <gt_alv2_1> TO <lt_table_temp>.
        ELSEIF gs_alv2_structures-grid_name EQ 'GO_GRID2_2'..
          SELECT *
           FROM (gs_alv2_structures-tabname)
           INTO CORRESPONDING FIELDS OF TABLE <gt_alv2_2>
           WHERE (lt_where_clauses).
          ASSIGN <gt_alv2_2> TO <lt_table_temp>.
        ELSEIF gs_alv2_structures-grid_name EQ 'GO_GRID2_3'..
          SELECT *
           FROM (gs_alv2_structures-tabname)
           INTO CORRESPONDING FIELDS OF TABLE <gt_alv2_3>
           WHERE (lt_where_clauses).
          ASSIGN <gt_alv2_3> TO <lt_table_temp>.
        ENDIF.
        IF <lt_table_temp> IS ASSIGNED.
          CALL BADI lo_badi->modify_table
            EXPORTING
              iv_int_type    = ls_alv1-int_type
              iv_int_subtype = ls_alv1-int_subtype
              iv_tabname     = gs_alv2_structures-tabname
            CHANGING
              ct_table       = <lt_table_temp>.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_alv3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_alv3 .
  CLEAR: gt_alv3.

*  Seçili satırın bulunması
  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
  CHECK sy-subrc EQ 0.

  SELECT *
    FROM zint_000_t_005
    INTO TABLE @DATA(lt_005)
    WHERE int_type = @ls_alv1-int_type AND
          int_subtype = @ls_alv1-int_subtype AND
          guid = @ls_alv1-guid.
  CHECK lt_005 IS NOT INITIAL.

  SELECT *
    FROM zint_000_t_004
    INTO TABLE @DATA(lt_004).

  SORT lt_005 BY counter DESCENDING.
  LOOP AT lt_005 INTO DATA(ls_005).
    CLEAR: gs_alv3.
    gs_alv3 = VALUE #(
    int_type = ls_005-int_type
    int_subtype = ls_005-int_subtype
    guid = ls_005-guid
    counter = ls_005-counter
    datum = ls_005-datum
    uzeit = ls_005-uzeit
    username = ls_005-username
    statu = ls_005-statu
    eski_statu = ls_005-eski_statu
    ballog_id = ls_005-ballog_id
    ballog_number = ls_005-ballog_number
    ).

    READ TABLE lt_004 INTO DATA(ls_004) WITH KEY statu = ls_005-statu.
    IF sy-subrc EQ 0.
      gs_alv3-statu_tanim = ls_004-description.
      IF ls_004-type EQ 'S'.
        gs_alv3-durum = '@WB@'.
      ELSEIF ls_004-type EQ 'E'.
        gs_alv3-durum = '@WD@'.
      ELSE.
        gs_alv3-durum = '@WC@'.
      ENDIF.
    ENDIF.

    READ TABLE lt_004 INTO ls_004 WITH KEY statu = ls_005-eski_statu.
    IF sy-subrc EQ 0.
      gs_alv3-eski_statu_tanim = ls_004-description.
    ENDIF.

    APPEND gs_alv3 TO gt_alv3.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form double_click_grid3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM double_click_grid3 .

  CALL METHOD splitter_3->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = go_container3_2.

  splitter_3->set_column_width( EXPORTING id = 1  width = 50 ).

  PERFORM get_data_alv3_2.

  IF go_grid3_2 IS NOT BOUND.
    gs_layout3_2-cwidth_opt = 'X'.
    gs_layout3_2-ctab_fname  = 'CELLCOLOR'.
    zor_cl_local=>list_alv(
      EXPORTING
        iv_structure_name = 'ZINT_000_S_ALV3_2'
        is_layout = gs_layout3_2
        it_data = 'GT_ALV3_2'
        iv_con_name = 'CON9003_2'
*        iv_data_change_finished = 'X'
*        iv_double_click = 'X'
        iv_split_con = 'X'
*        it_add_button = lt_button[]
*        it_fcat_change = lt_property9003
*        iv_editable = 'X'
         iv_no_toolbar = 'X'
*        it_exclude = lt_exclude[]
        iv_hotspot = 'X'
      CHANGING
        co_custom_container = go_custom_container
        co_gui_container = go_container3_2
        co_grid = go_grid3_2
        ct_fcat = gt_fcat3_2
        ).
  ELSE.
    go_grid3_2->refresh_table_display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_alv3_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_alv3_2 .
  CLEAR: gt_alv3_2.
  READ TABLE gt_alv3 INTO DATA(ls_alv3) WITH KEY flg = 'X'.
  IF sy-subrc EQ 0.

    zint_000_cl_bal_log=>read_ballog(
      EXPORTING iv_ballog_number = ls_alv3-ballog_number
      IMPORTING et_messages = DATA(lt_messages) ).
    LOOP AT lt_messages INTO DATA(ls_messages).
      CLEAR: gs_alv3_2.
      IF ls_messages-type EQ 'E' OR ls_messages-type EQ 'A'.
        gs_alv3_2-type = '@1B@'.
      ELSEIF ls_messages-type EQ 'W'.
        gs_alv3_2-type = '@1A@'.
      ELSEIF ls_messages-type EQ 'S'.
        gs_alv3_2-type = '@1C@'.
      ENDIF.
      gs_alv3_2-message = ls_messages-message.
      APPEND gs_alv3_2 TO gt_alv3_2.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_alv2_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_alv2_1 USING iv_tabname TYPE typename is_alv1 TYPE zint_000_s_alv1.
  DATA: lt_dyn_table TYPE REF TO data,
        lo_badi      TYPE REF TO modify_alv.
  UNASSIGN <gt_alv2_1>.
  CLEAR: gt_fcat2_1.
  zor_cl_local=>build_fieldcatalog( EXPORTING iv_structure_name = iv_tabname
                                    RECEIVING rt_fcat = gt_fcat2_1[] ).

  GET BADI lo_badi FILTERS int_type = is_alv1-int_type int_subtype = is_alv1-int_subtype.
  CALL BADI lo_badi->modify_fcat EXPORTING iv_int_type = is_alv1-int_type iv_int_subtype = is_alv1-int_subtype iv_tabname = iv_tabname CHANGING ct_fcat = gt_fcat2_1.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat2_1[]
    IMPORTING
      ep_table        = lt_dyn_table.
  ASSIGN lt_dyn_table->* TO <gt_alv2_1>.

  DATA : BEGIN OF ls_condtab.
           INCLUDE STRUCTURE hrcond.
  DATA : END OF ls_condtab.
  DATA : lt_condtab LIKE TABLE OF ls_condtab.

  TYPES: BEGIN OF ty_s_clause.
  TYPES:   line(72)  TYPE c.
  TYPES: END OF ty_s_clause.
  DATA: lt_where_clauses  TYPE STANDARD TABLE OF ty_s_clause WITH DEFAULT KEY.

  ls_condtab = VALUE #( field = 'INT_TYPE' opera = 'EQ' low = is_alv1-int_type ). APPEND ls_condtab TO lt_condtab.
  ls_condtab = VALUE #( field = 'INT_SUBTYPE' opera = 'EQ' low = is_alv1-int_subtype ). APPEND ls_condtab TO lt_condtab.
  ls_condtab = VALUE #( field = 'GUID' opera = 'EQ' low = is_alv1-guid ). APPEND ls_condtab TO lt_condtab.

  CALL FUNCTION 'RH_DYNAMIC_WHERE_BUILD'
    EXPORTING
      dbtable         = space " can be empty
    TABLES
      condtab         = lt_condtab
      where_clause    = lt_where_clauses
    EXCEPTIONS
      empty_condtab   = 01
      no_db_field     = 02
      unknown_db      = 03
      wrong_condition = 04.

  DATA: lo_data TYPE REF TO data.
  FIELD-SYMBOLS: <lt_itab> TYPE table.
  CREATE DATA lo_data TYPE TABLE OF (iv_tabname).
  ASSIGN lo_data->* TO <lt_itab>.
  SELECT *
    FROM (iv_tabname)
    INTO CORRESPONDING FIELDS OF TABLE <lt_itab>
    WHERE (lt_where_clauses).
  IF <lt_itab> IS ASSIGNED.
    FIELD-SYMBOLS <ls_alv2_1> TYPE any.
    LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>) .
      APPEND INITIAL LINE TO <gt_alv2_1> ASSIGNING <ls_alv2_1>.
      MOVE-CORRESPONDING <ls_itab> TO <ls_alv2_1>.
    ENDLOOP.
    CALL BADI lo_badi->modify_table
      EXPORTING
        iv_int_type    = is_alv1-int_type    " Entegrasyon tipi
        iv_int_subtype = is_alv1-int_subtype " Entegrasyon alt tipi
        iv_tabname     = iv_tabname
      CHANGING
        ct_table       = <gt_alv2_1>.       " Tablo
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form double_click_grid2_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM double_click_grid2_1 USING iv_rowno TYPE lvc_index.
  PERFORM get_alv2_grid_connections USING 'GO_GRID2_1' iv_rowno.
*  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
*  CALL METHOD splitter_2->get_container
*    EXPORTING
*      row       = 2
*      column    = 1
*    RECEIVING
*      container = go_container2_2.
*  CHECK sy-subrc EQ 0.
*  READ TABLE gt_alv2_kirilim INTO DATA(ls_alv2_kirilim) WITH KEY sira = '02'.
*  IF sy-subrc EQ 0.
*
*    splitter_2->set_row_height( EXPORTING id = 1 height = 50 ).
*
*    PERFORM get_data_alv2_2 USING ls_alv2_kirilim-structure ls_alv1.
*
*    IF go_grid2_2 IS NOT BOUND AND <gt_alv2_2> IS ASSIGNED.
*      gs_layout2_2-cwidth_opt = 'X'.
**    gs_layout2_2-ctab_fname  = 'CELLCOLOR'.
*      zor_cl_local=>list_alv(
*        EXPORTING
*          iv_structure_name = ls_alv2_kirilim-structure
*          is_layout = gs_layout2_2
*          it_data = '<GT_ALV2_2>'
*          iv_con_name = 'CON9002_2'
**        iv_data_change_finished = 'X'
**        iv_double_click = 'X'
*          iv_split_con = 'X'
**        it_add_button = lt_button[]
**        it_fcat_change = lt_property9003
**        iv_editable = 'X'
*           iv_no_toolbar = 'X'
**        it_exclude = lt_exclude[]
**        iv_hotspot = 'X'
*        CHANGING
*          co_custom_container = go_custom_container
*          co_gui_container = go_container2_2
*          co_grid = go_grid2_2
*          ct_fcat = gt_fcat2_2
*          ).
*    ENDIF.
*
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_alv2_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_alv2_2 USING iv_tabname TYPE typename is_alv1 TYPE zint_000_s_alv1 .
  DATA: lt_dyn_table TYPE REF TO data,
        lo_badi      TYPE REF TO modify_alv.
  UNASSIGN <gt_alv2_2>.
  CLEAR: gt_fcat2_2.
  zor_cl_local=>build_fieldcatalog( EXPORTING iv_structure_name = iv_tabname
                                   RECEIVING rt_fcat = gt_fcat2_2[] ).

  GET BADI lo_badi FILTERS int_type = is_alv1-int_type int_subtype = is_alv1-int_subtype.
  CALL BADI lo_badi->modify_fcat EXPORTING iv_int_type = is_alv1-int_type iv_int_subtype = is_alv1-int_subtype iv_tabname = iv_tabname CHANGING ct_fcat = gt_fcat2_2.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = gt_fcat2_2[]
    IMPORTING
      ep_table        = lt_dyn_table.
  ASSIGN lt_dyn_table->* TO <gt_alv2_2>.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_alv2_grids
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_alv2_grids .
  DATA: lt_property_9002 LIKE TABLE OF gs_property.
  lt_property_9002 = VALUE #(
( fieldname = 'INT_TYPE' property = 'TECH' value = 'X' )
( fieldname = 'INT_SUBTYPE' property = 'TECH' value = 'X' )
( fieldname = 'GUID' property = 'TECH' value = 'X' )
).
*
  LOOP AT gt_alv2_structures REFERENCE INTO DATA(lr_str) WHERE displayed IS INITIAL.

    IF lr_str->grid_name EQ 'GO_GRID2_1' AND <gt_alv2_1> IS ASSIGNED.
      CALL METHOD splitter_2->get_container
        EXPORTING
          row       = lr_str->lines
          column    = 1
        RECEIVING
          container = go_container2_1.
      gs_layout2_1-cwidth_opt = 'X'.
      zor_cl_local=>list_alv(
     EXPORTING
       iv_structure_name = lr_str->tabname
       is_layout = gs_layout2_1
       it_data = '<GT_ALV2_1>'
       iv_con_name = 'CON9002'
       iv_double_click = 'X'
       iv_split_con = 'X'
*        it_add_button = lt_button[]
       it_fcat_change = lt_property_9002
*        iv_editable = 'X'
       iv_no_toolbar = 'X'
*        it_exclude = lt_exclude[]
       iv_hotspot = 'X'
    CHANGING
       co_custom_container = go_custom_container
       co_gui_container = go_container2_1
       co_grid = go_grid2_1
       ct_fcat = gt_fcat2_1
    ).
    ELSEIF lr_str->grid_name EQ 'GO_GRID2_2' AND <gt_alv2_2> IS ASSIGNED.
      CALL METHOD splitter_2->get_container
        EXPORTING
          row       = lr_str->lines
          column    = 1
        RECEIVING
          container = go_container2_2.
      gs_layout2_2-cwidth_opt = 'X'.
      zor_cl_local=>list_alv(
     EXPORTING
       iv_structure_name = lr_str->tabname
       is_layout = gs_layout2_2
       it_data = '<GT_ALV2_2>'
       iv_con_name = 'CON9002'
       iv_double_click = 'X'
       iv_split_con = 'X'
*        it_add_button = lt_button[]
       it_fcat_change = lt_property_9002
*        iv_editable = 'X'
       iv_no_toolbar = 'X'
*        it_exclude = lt_exclude[]
       iv_hotspot = 'X'
    CHANGING
       co_custom_container = go_custom_container
       co_gui_container = go_container2_2
       co_grid = go_grid2_2
       ct_fcat = gt_fcat2_2
    ).
    ELSEIF lr_str->grid_name EQ 'GO_GRID2_3' AND <gt_alv2_3> IS ASSIGNED.
      CALL METHOD splitter_2->get_container
        EXPORTING
          row       = lr_str->lines
          column    = 1
        RECEIVING
          container = go_container2_3.
      gs_layout2_3-cwidth_opt = 'X'.
      zor_cl_local=>list_alv(
     EXPORTING
       iv_structure_name = lr_str->tabname
       is_layout = gs_layout2_3
       it_data = '<GT_ALV2_3>'
       iv_con_name = 'CON9002'
       iv_double_click = 'X'
       iv_split_con = 'X'
*        it_add_button = lt_button[]
       it_fcat_change = lt_property_9002
*        iv_editable = 'X'
       iv_no_toolbar = 'X'
*        it_exclude = lt_exclude[]
       iv_hotspot = 'X'
    CHANGING
       co_custom_container = go_custom_container
       co_gui_container = go_container2_3
       co_grid = go_grid2_3
       ct_fcat = gt_fcat2_3
    ).
    ENDIF.
    lr_str->displayed = 'X'.
  ENDLOOP.

*  "Split alv satır aralıklarının ayarlanması
  DATA: lv_lines TYPE n LENGTH 2.
  lv_lines = 0.
  LOOP AT gt_alv2_structures TRANSPORTING NO FIELDS WHERE displayed EQ 'X'.
    lv_lines = lv_lines + 1.
  ENDLOOP.
  IF lv_lines EQ 1.
    splitter_2->set_row_height( EXPORTING id = 1 height = 100 ).
  ELSEIF lv_lines EQ 2.
    splitter_2->set_row_height( EXPORTING id = 1 height = 50 ).
    splitter_2->set_row_height( EXPORTING id = 2 height = 50 ).
  ELSEIF lv_lines EQ 3.
    splitter_2->set_row_height( EXPORTING id = 1 height = 40 ).
    splitter_2->set_row_height( EXPORTING id = 2 height = 40 ).
    splitter_2->set_row_height( EXPORTING id = 3 height = 30 ).
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_alv2_grid_connections
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_alv2_grid_connections USING iv_grid TYPE char10 iv_rowno TYPE lvc_index.
  "Burayı tam dinamik yapabilirdik ama vakit kalmadı :(
  READ TABLE gt_alv1 INTO DATA(ls_alv1) WITH KEY flg = 'X'.
  IF sy-subrc EQ 0.
    SELECT *
    FROM zint_000_t_008
    INTO TABLE @DATA(lt_008)
    WHERE int_type = @ls_alv1-int_type AND
          int_subtype = @ls_alv1-int_subtype.
  ENDIF.
  DATA: lv_lines TYPE n LENGTH 1.
  IF iv_grid EQ 'GO_GRID1'.
    CLEAR: gt_alv2_structures,gt_alv2_connection.
    UNASSIGN: <gt_alv2_1>,<gt_alv2_2>,<gt_alv2_3>.

    CLEAR: lv_lines.
    LOOP AT lt_008 INTO DATA(ls_008) WHERE parent_table EQ 'ZINT_000_T_003'.
      READ TABLE gt_alv2_connection TRANSPORTING NO FIELDS WITH KEY child_table = ls_008-child_table.
      IF sy-subrc NE 0.
        CLEAR: gs_alv2_structures , gs_alv2_connection.
        lv_lines = lv_lines + 1.
        gs_alv2_structures-lines = lv_lines.
        gs_alv2_structures-grid_name = 'GO_GRID2_' && lv_lines.
        gs_alv2_structures-tabname = ls_008-child_table.
        gs_alv2_structures-int_tabname = '<GT_ALV2_' && lv_lines && '>'.
        APPEND gs_alv2_structures TO gt_alv2_structures.
        gs_alv2_connection = VALUE #( parent_table = ls_008-parent_table child_table = ls_008-child_table ).
        APPEND gs_alv2_connection TO gt_alv2_connection.
      ENDIF.
    ENDLOOP.
  ELSEIF iv_grid EQ 'GO_GRID2_1'.
    READ TABLE gt_alv2_structures INTO DATA(ls_alv2_structures) WITH KEY grid_name = iv_grid.
    IF sy-subrc EQ 0.
      CLEAR: lv_lines.
      DESCRIBE TABLE gt_alv2_structures LINES lv_lines.
      LOOP AT lt_008 INTO ls_008 WHERE parent_table = ls_alv2_structures-tabname.
        READ TABLE gt_alv2_connection TRANSPORTING NO FIELDS WITH KEY child_table = ls_008-child_table.
        IF sy-subrc NE 0.
          CLEAR: gs_alv2_structures , gs_alv2_connection.
          lv_lines = lv_lines + 1.
          gs_alv2_structures-lines = lv_lines.
          gs_alv2_structures-grid_name = 'GO_GRID2_' && lv_lines.
          gs_alv2_structures-tabname = ls_008-child_table.
          gs_alv2_structures-int_tabname = '<GT_ALV2_' && lv_lines && '>'.
          APPEND gs_alv2_structures TO gt_alv2_structures.
          gs_alv2_connection = VALUE #( parent_table = ls_008-parent_table child_table = ls_008-child_table ).
          APPEND gs_alv2_connection TO gt_alv2_connection.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.

  ENDIF.
  PERFORM get_data_alv2_new USING iv_rowno.
  PERFORM get_alv2_grids.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form command_9001_rework
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM command_9001_rework .
  DATA: lt_param TYPE TABLE OF rsparams,
        ls_param TYPE rsparams,
        flag(1).

  SELECT *
    FROM zint_000_t_004
    INTO TABLE @DATA(lt_004).
  CLEAR flag.
  READ TABLE gt_alv1 REFERENCE INTO DATA(lr_alv1) WITH KEY flg = 'X'.
  IF sy-subrc EQ 0.
    READ TABLE lt_004 INTO DATA(ls_004) WITH KEY direction = lr_alv1->direction statu = lr_alv1->statu.
    IF sy-subrc EQ 0 AND ( ( ls_004-direction EQ 'I' AND  ls_004-type NE 'S' ) OR ( ls_004-direction EQ 'O' )  ).
      IF lr_alv1->rework EQ 'X'.

        SELECT SINGLE *
          FROM zint_000_t_002
          INTO @DATA(ls_002)
          WHERE int_type = @lr_alv1->int_type AND
                int_subtype = @lr_alv1->int_subtype.
        IF sy-subrc EQ 0.
          IF ls_002-object IS NOT INITIAL.
*-------> Added by S.KURTALAN 09.09.2021 14:00:36
            "Her guide ait zsd_012_t_05h tablosunda kayıt olur, statu 20 ve 22 bekleyen ve hatalı gözüken satırlar için tabloda sales_doc ya da purschase
            "alanları dolu ise bu guid işlenmiş demek oluyor bu yüzden zint_000_t_003 tablosundaki statusu 21 olarak güncelliyoruz
            IF lr_alv1->statu EQ '20' OR lr_alv1->statu EQ '22'.
              IF s_sitype-low EQ '10'.
                SELECT * FROM zsd_012_t_05h
                  WHERE int_type EQ '05' AND
                        int_subtype EQ '10' AND
                        guid EQ @lr_alv1->guid AND
                        sales_document IS INITIAL AND
                        purchase_order IS INITIAL
                   INTO TABLE @DATA(lt_05h).
              ELSEIF s_sitype-low EQ '11'.
                SELECT * FROM zsd_012_t_06h
                  WHERE int_type EQ '05' AND
                        int_subtype EQ '11' AND
                        guid EQ @lr_alv1->guid AND
                        sales_document IS INITIAL AND
                        purchase_order IS INITIAL
                   INTO TABLE @DATA(lt_06h).
              ENDIF.
              IF sy-subrc NE 0.
                UPDATE zint_000_t_003 SET statu = '21' WHERE guid = lr_alv1->guid AND int_subtype = s_sitype-low.
                COMMIT WORK AND WAIT .
                flag = 'X'.
              ENDIF.
            ENDIF.
*-------> Ended by S.KURTALAN 09.09.2021 14:00:36
            IF flag EQ space.
              CLEAR: ls_param,lt_param.
              ls_param = VALUE #( selname = 'SO_GUID' kind = 'S' sign = 'I' option = 'EQ' low = lr_alv1->guid ). APPEND ls_param TO lt_param.
              SUBMIT (ls_002-object)
              WITH SELECTION-TABLE lt_param
              AND RETURN.
            ENDIF.
            PERFORM double_click_grid1.

            SELECT SINGLE *
              FROM zint_000_t_003
              INTO @DATA(ls_003)
              WHERE guid EQ @lr_alv1->guid
              AND   int_subtype EQ @s_sitype-low.
            IF sy-subrc EQ 0 AND ls_003-statu NE lr_alv1->statu.
              lr_alv1->statu = ls_003-statu.
              READ TABLE lt_004 INTO DATA(ls_004_2) WITH KEY direction = lr_alv1->direction statu = lr_alv1->statu.
              IF ls_004_2-type EQ 'W'.
*                İşlenmeiş
                lr_alv1->durum = '@09@'.
              ELSEIF ls_004_2-type EQ 'S'.
*                Başarılı
                lr_alv1->durum = '@08@'.
              ELSEIF ls_004_2-type EQ 'E'..
*                Başarısız
                lr_alv1->durum = '@0A@'.
              ENDIF.
              lr_alv1->statu_tanim = ls_004_2-description.
              go_grid1->refresh_table_display( EXPORTING is_stable = VALUE #( row = 'X' col = 'X' ) ).
            ENDIF.

          ELSE.
            MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
