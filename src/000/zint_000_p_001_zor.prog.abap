CLASS zor_cl_local DEFINITION INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.

    CLASS-DATA : mt_fcat             TYPE lvc_t_fcat,
                 mo_grid             TYPE REF TO cl_gui_alv_grid,
                 mo_custom_container TYPE REF TO cl_gui_custom_container,
                 ms_layout           TYPE lvc_s_layo.

    DATA: BEGIN OF ms_property,
            fieldname TYPE fieldname,
            property  TYPE fieldname,
            value(30),
          END OF ms_property .
    DATA: mt_property LIKE TABLE OF ms_property.
    DATA: mt_exclude TYPE ui_functions.

    CLASS-DATA: mt_button TYPE TABLE OF stb_button.

    CLASS-DATA: lo_excel    TYPE ole2_object,        " Excel object
                lo_mapl     TYPE ole2_object,         " list of workbooks
                lo_map      TYPE ole2_object,          " workbook
                lo_zl       TYPE ole2_object,           " cell
                lo_f        TYPE ole2_object,            " font
                lo_interior TYPE ole2_object.          "interior

    CLASS-METHODS build_fieldcatalog
      IMPORTING
        VALUE(iv_structure_name) TYPE tabname
      RETURNING
        VALUE(rt_fcat)           TYPE lvc_t_fcat .

    CLASS-METHODS list_alv
      IMPORTING
        iv_structure_name       TYPE tabname OPTIONAL
        it_fcat_change          LIKE mt_property OPTIONAL
        is_layout               TYPE lvc_s_layo OPTIONAL
        VALUE(iv_save)          TYPE char1 OPTIONAL
        iv_con_name             TYPE scrfname
        it_data                 TYPE string "String olarak tablo ismi verilecek
        iv_no_toolbar           TYPE char1 OPTIONAL "Toolbardaki standart butonları kaldırır
        it_exclude              LIKE mt_exclude OPTIONAL "Toolbardan kaldırılacak standart butonlar listesi
        it_add_button           LIKE mt_button OPTIONAL "Eğer custom button eklenmek isteniyorsa
        iv_hotspot              TYPE char1 OPTIONAL"Hotspot eventinin aktifleştirilmesi
        iv_data_change_finished TYPE char1 OPTIONAL"Data change finished eventinin aktifleştirilmesi
        iv_data_change          TYPE char1 OPTIONAL "Data change eventinin aktifleştirilmesi
        iv_double_click         TYPE char1 OPTIONAL "Double click eventinin aktifleştirilmesi
        it_f4                   TYPE lvc_t_f4 OPTIONAL "F4 eventi eklenecek fieldlar
        iv_split_con            TYPE char1 OPTIONAL "Split container kullanıldığını gösterir.
        iv_editable             TYPE char1 OPTIONAL "Editable eventinin aktifleştirilmesi
      EXPORTING
        ev_subrc                TYPE sy-subrc
      CHANGING
        ct_fcat                 TYPE lvc_t_fcat OPTIONAL
        co_custom_container     TYPE REF TO cl_gui_custom_container
        co_gui_container        TYPE REF TO cl_gui_container OPTIONAL
        co_grid                 TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS change_fcat
      IMPORTING
        it_property LIKE mt_property
      CHANGING
        ct_fieldcat TYPE lvc_t_fcat .

    CLASS-METHODS exclude_buttons
      IMPORTING it_exclude        LIKE mt_exclude OPTIONAL
      RETURNING
                VALUE(et_exclude) LIKE mt_exclude.

    CLASS-METHODS get_filename
      RETURNING VALUE(ev_filename) TYPE rlgrap-filename.

    CLASS-METHODS excel_sablon
      IMPORTING iv_structure TYPE string.                "#EC CI_VALPAR

    CLASS-METHODS excel_sablon_with_table
      IMPORTING iv_structure TYPE string                 "#EC CI_VALPAR
                it_data      TYPE string .

    CLASS-METHODS get_excel
      IMPORTING iv_file    TYPE rlgrap-filename
                iv_tabname TYPE string.

    "ALV EVENTLARI
    CLASS-METHODS: handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive
                sender.

    CLASS-METHODS: handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm sender.

    CLASS-METHODS:handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        sender.

    CLASS-METHODS:handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        er_data_changed
        e_onf4
        e_onf4_before
        e_onf4_after
        e_ucomm
        sender.

    CLASS-METHODS:handle_data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING
        e_modified
        et_good_cells
        sender.

    CLASS-METHODS:handle_on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                es_row_no
                er_event_data
                et_bad_cells
                e_display
                sender.

    CLASS-METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        sender
        e_row
        e_column
        es_row_no.

    CLASS-METHODS:  fill_cell
      IMPORTING satir TYPE i
                sutun TYPE i
                bold  TYPE i
                val   TYPE string
                color TYPE string OPTIONAL.

  PRIVATE SECTION.



ENDCLASS.

CLASS zor_cl_local IMPLEMENTATION.

  METHOD build_fieldcatalog.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = iv_structure_name
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc EQ 0 .

    ELSE.

    ENDIF.
  ENDMETHOD.

  METHOD list_alv.

    IF ct_fcat[] IS INITIAL AND iv_structure_name IS NOT INITIAL.
      "Fcat türetilecektir.
      ct_fcat[] = build_fieldcatalog(
      EXPORTING iv_structure_name = iv_structure_name ).
    ELSEIF ct_fcat[] IS INITIAL.
      ev_subrc = 1.
      RETURN.
    ENDIF.

    IF it_fcat_change[] IS NOT INITIAL.
      change_fcat(
      EXPORTING
        it_property = it_fcat_change
      CHANGING
        ct_fieldcat = ct_fcat[] ).
    ENDIF.

    IF iv_save IS INITIAL.
      iv_save = 'S'.
    ENDIF.

    " Menü butonlarının ayarlanması
    IF iv_no_toolbar EQ 'X'.
      DATA(lt_exclude) = exclude_buttons( ).
    ELSE.
      IF it_exclude IS NOT INITIAL.
        lt_exclude = exclude_buttons( it_exclude = it_exclude ).
      ELSE.

      ENDIF.
    ENDIF.

    IF iv_split_con IS INITIAL.
      IF co_custom_container IS INITIAL.
        CREATE OBJECT co_custom_container
          EXPORTING
            container_name = iv_con_name.

        CREATE OBJECT co_grid
          EXPORTING
            i_parent = co_custom_container.
      ENDIF.
    ELSE.
      "Split alvdir container yaratılmış olmalıdır
      CREATE OBJECT co_grid
        EXPORTING
          i_parent = co_gui_container.
    ENDIF.

    FIELD-SYMBOLS: <ft_data> TYPE ANY TABLE.
    ASSIGN (it_data) TO <ft_data>.

    "Handler eklemeleri

    IF it_add_button[] IS NOT INITIAL.
      "Toolbar butonları ekleniyor
      LOOP AT it_add_button INTO DATA(ls_button).
        COLLECT ls_button INTO mt_button.
      ENDLOOP.
*      mt_button = it_add_button.
      SET HANDLER zor_cl_local=>handle_toolbar FOR co_grid.
      SET HANDLER zor_cl_local=>handle_user_command FOR co_grid.
    ENDIF.

    IF iv_data_change EQ 'X'.
      "Data change handler aktifleştirme
      SET HANDLER zor_cl_local=>handle_data_changed FOR co_grid.
    ENDIF.

    IF iv_data_change_finished EQ 'X'.
      "Data vhange finished handler aktifleştirme
      SET HANDLER zor_cl_local=>handle_data_changed_finished FOR co_grid.
    ENDIF.

    IF iv_hotspot EQ 'X'.
      SET HANDLER zor_cl_local=>handle_hotspot_click FOR co_grid.
    ENDIF.

    IF iv_double_click EQ 'X'.
      SET HANDLER zor_cl_local=>handle_double_click FOR co_grid.
    ENDIF.

    IF it_f4[] IS NOT INITIAL.
      CALL METHOD co_grid->register_f4_for_fields
        EXPORTING
          it_f4 = it_f4[].
      SET HANDLER zor_cl_local=>handle_on_f4 FOR co_grid.
    ENDIF.
    IF iv_save EQ 'A'.
      DATA: ls_variant TYPE disvariant.
      ls_variant-report = sy-repid.
    ENDIF.
    CALL METHOD co_grid->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer            = 'X'
        is_layout                     = is_layout
        it_toolbar_excluding          = lt_exclude
        i_save                        = iv_save
        is_variant                    = ls_variant
      CHANGING
        it_outtab                     = <ft_data>
        it_fieldcatalog               = ct_fcat[]
*       it_sort                       =
*       it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF iv_editable EQ 'X'.
        CALL METHOD co_grid->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

        CALL METHOD co_grid->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.
      ENDIF.
    ENDIF.
*    ENDIF.

  ENDMETHOD.

  METHOD change_fcat.
    DATA: ls_property LIKE LINE OF it_property.
    LOOP AT it_property INTO ls_property.
      TRY.
          ASSIGN COMPONENT ls_property-property
           OF STRUCTURE ct_fieldcat[ fieldname = ls_property-fieldname ]
            TO FIELD-SYMBOL(<lv_property>).
          IF sy-subrc = 0 AND <lv_property> IS ASSIGNED.
            <lv_property> = ls_property-value.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD exclude_buttons.
    CLEAR: et_exclude.
    IF it_exclude[] IS INITIAL.
      et_exclude = VALUE #(
                            (  cl_gui_alv_grid=>mc_fc_save_variant )
                            (  cl_gui_alv_grid=>mc_fc_print )
                            (  cl_gui_alv_grid=>mc_fc_detail )
                            (  cl_gui_alv_grid=>mc_fc_sum )
                            (  cl_gui_alv_grid=>mc_fc_subtot )
                            (  cl_gui_alv_grid=>mc_fc_graph )
                            (  cl_gui_alv_grid=>mc_fc_info )
                            (  cl_gui_alv_grid=>mc_fc_current_variant )
                            (  cl_gui_alv_grid=>mc_fc_loc_delete_row )
                            (  cl_gui_alv_grid=>mc_fc_loc_append_row )
                            (  cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            (  cl_gui_alv_grid=>mc_fc_loc_cut )
                            (  cl_gui_alv_grid=>mc_fc_loc_paste )
                            (  cl_gui_alv_grid=>mc_fc_loc_copy_row )
                            (  cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                            (  cl_gui_alv_grid=>mc_fc_loc_undo )
                            (  cl_gui_alv_grid=>mc_mb_view )
                            (  cl_gui_alv_grid=>mc_mb_sum )
                            (  cl_gui_alv_grid=>mc_mb_export )
                            (  cl_gui_alv_grid=>mc_fc_refresh )
                            (  cl_gui_alv_grid=>mc_fc_check )
                            (  cl_gui_alv_grid=>mc_fc_filter )
                            (  cl_gui_alv_grid=>mc_fc_find )
                            (  cl_gui_alv_grid=>mc_fg_sort )
                            (  cl_gui_alv_grid=>mc_fc_loc_copy )
                          ).
    ELSE.
      et_exclude[] = it_exclude[].
    ENDIF.
  ENDMETHOD.

  METHOD get_filename.
    DATA: lt_files TYPE filetable,
          ls_files TYPE file_table,
          lv_rc    TYPE i.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      CHANGING
        file_table              = lt_files
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0 OR lv_rc < 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_files INDEX 1 INTO ls_files.
      ev_filename = ls_files-filename.
    ENDIF.

  ENDMETHOD.

  METHOD excel_sablon.
    "TOP include içerisine yazılacak kısım
*TABLES: sscrfields.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN PUSHBUTTON 1(25) text-901 USER-COMMAND cmd1.
*SELECTION-SCREEN END OF LINE.

    "Main içerisine
* AT SELECTION-SCREEN
*  IF sscrfields-ucomm = 'CMD1'.
*    zor_cl_local=>excel_sablon(
*    EXPORTING iv_structure = 'ZFICA_002_SOZHES_ALV' ).
*  ENDIF.

*  * start Excel
    CREATE OBJECT lo_excel 'EXCEL.APPLICATION'.
    IF sy-subrc <> 0.
*      WRITE: / TEXT-017 , sy-subrc.
      RETURN.
    ENDIF.

    SET PROPERTY OF lo_excel  'Visible' = 1.

* get list of workbooks, initially empty
    CALL METHOD OF lo_excel 'Workbooks' = lo_mapl.

* add a new workbook
    CALL METHOD OF lo_mapl 'Add' = lo_map.

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE abap_component_tab,
          ls_comp   TYPE abap_componentdescr.
    lo_struct ?= cl_abap_typedescr=>describe_by_name( iv_structure ).
    lt_comp = lo_struct->get_components( ).
    DATA(lt_object) = lo_struct->get_ddic_object( ).
    DATA: lv_sutun TYPE i,
          lv_name  TYPE string.
    LOOP AT lt_comp INTO ls_comp.
      lv_sutun = lv_sutun + 1.
      READ TABLE lt_object INTO DATA(ls_object)
                 WITH KEY fieldname = ls_comp-name.
      IF sy-subrc EQ 0.
        SELECT SINGLE *                       "#EC CI_ALL_FIELDS_NEEDED
          FROM dd04t
          INTO @DATA(ls_dd04t)
          WHERE ddlanguage EQ @sy-langu AND
                as4local = 'A' AND
                rollname EQ @ls_object-rollname.            "#EC WARNOK
        IF sy-subrc EQ 0.
          lv_name = ls_dd04t-scrtext_l.
        ELSE.
          lv_name = ls_object-rollname.
        ENDIF.
      ELSE.
        lv_name = ls_comp-name.
      ENDIF.
      fill_cell( satir = 1 sutun = lv_sutun bold = 1 val = lv_name ).
    ENDLOOP.

    FREE OBJECT lo_excel.

  ENDMETHOD.

  METHOD excel_sablon_with_table.
    "TOP include içerisine yazılacak kısım
*TABLES: sscrfields.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN PUSHBUTTON 1(25) text-901 USER-COMMAND cmd1.
*SELECTION-SCREEN END OF LINE.

    "Main içerisine
* AT SELECTION-SCREEN
*  IF sscrfields-ucomm = 'CMD1'.
*    zor_cl_local=>excel_sablon(
*    EXPORTING iv_structure = 'ZFICA_002_SOZHES_ALV' ).
*  ENDIF.

*  * start Excel
    CREATE OBJECT lo_excel 'EXCEL.APPLICATION'.
    IF sy-subrc <> 0.
*      WRITE: / TEXT-017 , sy-subrc.
      RETURN.
    ENDIF.

    SET PROPERTY OF lo_excel  'Visible' = 1.

* get list of workbooks, initially empty
    CALL METHOD OF lo_excel 'Workbooks' = lo_mapl.

* add a new workbook
    CALL METHOD OF lo_mapl 'Add' = lo_map.

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE abap_component_tab,
          ls_comp   TYPE abap_componentdescr.
    lo_struct ?= cl_abap_typedescr=>describe_by_name( iv_structure ).
    lt_comp = lo_struct->get_components( ).
    DATA(lt_object) = lo_struct->get_ddic_object( ).
    DATA: lv_sutun TYPE i,
          lv_satir TYPE i,
          lv_name  TYPE string,
          lv_color TYPE string.
    lv_satir = 1.
    LOOP AT lt_comp INTO ls_comp.
      lv_sutun = lv_sutun + 1.
      READ TABLE lt_object INTO DATA(ls_object)
                 WITH KEY fieldname = ls_comp-name.
      IF sy-subrc EQ 0.
        SELECT SINGLE *                       "#EC CI_ALL_FIELDS_NEEDED
          FROM dd04t
          INTO @DATA(ls_dd04t)
          WHERE ddlanguage EQ @sy-langu AND
                as4local = 'A' AND
                rollname EQ @ls_object-rollname.            "#EC WARNOK
        IF sy-subrc EQ 0.
          lv_name = ls_dd04t-scrtext_l.
        ELSE.
          lv_name = ls_object-rollname.
        ENDIF.
      ELSE.
        lv_name = ls_comp-name.
      ENDIF.
      fill_cell( satir = lv_satir sutun = lv_sutun bold = 1 val = lv_name ).
    ENDLOOP.

    "Alt satırların yazdırılması
    FIELD-SYMBOLS: <lv_fs>    TYPE any,
                   <ls_fs>    TYPE any,
                   <lt_table> TYPE ANY TABLE.
    ASSIGN (it_data) TO <lt_table>.
    IF <lt_table> IS ASSIGNED.
*    LOOP AT gt_excel2 INTO DATA(ls_excel2).
      LOOP AT <lt_table> ASSIGNING <ls_fs>.
        CLEAR: lv_sutun.
        lv_satir = lv_satir + 1.
        LOOP AT lt_comp INTO ls_comp.
          lv_sutun = lv_sutun + 1.
          UNASSIGN <lv_fs>.
          ASSIGN COMPONENT ls_comp-name OF STRUCTURE <ls_fs> TO <lv_fs>.
          IF <lv_fs> IS ASSIGNED.
            lv_name = <lv_fs>.
            IF ls_comp-type->type_kind EQ 'D'.
              lv_name = lv_name+6(2) && '.' && lv_name+4(2) && '.' && lv_name(4).
            ENDIF.
            CLEAR: lv_color.
            IF ls_comp-name EQ 'VRKME' OR ls_comp-name EQ 'KMEIN'.
              lv_color = 48.
            ENDIF.
            fill_cell( satir = lv_satir sutun = lv_sutun bold = 0 val = lv_name color = lv_color ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
    FREE OBJECT lo_excel.

  ENDMETHOD.

  METHOD get_excel.
    TYPE-POOLS: truxs.
    DATA: lt_trd TYPE truxs_t_text_data.
    FIELD-SYMBOLS: <ft_data> TYPE STANDARD TABLE.

    ASSIGN (iv_tabname) TO <ft_data>.

    IF <ft_data> IS ASSIGNED.
      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_tab_raw_data       = lt_trd
          i_filename           = iv_file
          i_line_header        = 'X'
        TABLES
          i_tab_converted_data = <ft_data>
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc EQ 0.

      ELSE.

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_toolbar.
    CASE sender.
      WHEN go_grid1.
        sy-ucomm = 'GRID1'.
          DELETE e_object->mt_toolbar WHERE function(4) EQ '9001'.
    ENDCASE.

    LOOP AT mt_button INTO DATA(ls_button).
      IF sy-ucomm EQ 'GRID1'.
        IF ls_button-function(4) NE '9001'.
          CONTINUE.
        ENDIF.
      ELSEIF sy-ucomm EQ 'GRID2'.

      ENDIF.
      APPEND ls_button TO e_object->mt_toolbar.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE sender.
      WHEN go_grid1.
        CASE e_ucomm.
          WHEN '9001_REWORK'.
            PERFORM command_9001_rework.
        ENDCASE.
*      WHEN go_grid2.
*        CASE e_ucomm.
*          WHEN '9002_BTESLIMAT'.
*            PERFORM command_9002_teslimat.
*          WHEN '9002_MTESLIMAT'.
*            PERFORM command_9002_ugteslimat.
*          WHEN '9002_SAS'.
*            PERFORM command_9002_sas.
*          WHEN '9002_MGIRIS'.
*            PERFORM command_9002_501teslimat.
*        ENDCASE.
*      WHEN OTHERS.
    ENDCASE.
*
    sender->refresh_table_display( EXPORTING is_stable = VALUE #( row = 'X' col = 'X' ) ).
  ENDMETHOD.

  METHOD handle_hotspot_click.
    CASE sender.
*      WHEN go_grid9001.
*        CASE e_column_id.
*          WHEN 'ICON'.
*            PERFORM hotspot_9001_icon USING e_row_id.
*        	WHEN .
*        	WHEN OTHERS.
*        ENDCASE.
*    	WHEN .
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.
*    CASE sender.
*      WHEN go_grid1.
*      WHEN go_grid2.
*    ENDCASE.

  ENDMETHOD.

  METHOD handle_data_changed_finished.
*    CASE sender.
*      WHEN go_grid1.
*        PERFORM alv1_data_changed_finished.
*      WHEN go_grid2.
*        PERFORM alv2_data_changed_finished.
*      WHEN OTHERS.
*    ENDCASE.
*    IF e_modified EQ 'X'.
*      sender->refresh_table_display( ).
*    ENDIF.
  ENDMETHOD.

  METHOD handle_on_f4.
*    CASE sender.
*      WHEN go_grid4.
*        IF e_fieldname EQ 'NICEL'.
*        ENDIF.
*        go_grid4->refresh_table_display( EXPORTING is_stable = VALUE lvc_s_stbl( row = 'X' ) ) .
*    ENDCASE.
*    er_event_data->m_event_handled = 'X'.
  ENDMETHOD.

  METHOD handle_double_click.
    CASE sender.
      WHEN go_grid1.
        LOOP AT gt_alv1 REFERENCE INTO DATA(lr_alv1) WHERE cellcolor IS NOT INITIAL.
          CLEAR: lr_alv1->cellcolor[] , lr_alv1->flg.
        ENDLOOP.
        READ TABLE gt_alv1 REFERENCE INTO lr_alv1 INDEX e_row.
        IF sy-subrc EQ 0.
          DATA: ls_cellcolor LIKE LINE OF lr_alv1->cellcolor.
          ls_cellcolor-color-col = 5.
          INSERT ls_cellcolor INTO lr_alv1->cellcolor[] INDEX 1.
          lr_alv1->flg = 'X'.
          PERFORM double_click_grid1.
        ENDIF.
        go_grid1->refresh_table_display( EXPORTING is_stable = VALUE #( row = 'X' col = 'X' ) ).
      WHEN go_grid2_1.
        if e_row-index IS NOT INITIAL.
        PERFORM double_click_grid2_1 USING e_row-index.
        endif.
      WHEN go_grid3_1.
        LOOP AT gt_alv3 REFERENCE INTO DATA(lr_alv3) WHERE cellcolor IS NOT INITIAL.
          CLEAR: lr_alv3->cellcolor[] , lr_alv3->flg.
        ENDLOOP.
        READ TABLE gt_alv3 REFERENCE INTO lr_alv3 INDEX e_row-index.
        IF sy-subrc EQ 0.
          CLEAR: ls_cellcolor.
          ls_cellcolor-color-col = 5.
          INSERT ls_cellcolor INTO lr_alv3->cellcolor[] INDEX 1.
          lr_alv3->flg = 'X'.
          PERFORM double_click_grid3.
        ENDIF.
        go_grid3_1->refresh_table_display( EXPORTING is_stable = VALUE #( row = 'X' col = 'X' ) ).
**      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD fill_cell.
    CALL METHOD OF lo_excel 'Cells' = lo_zl
    EXPORTING
    #1 = satir
    #2 = sutun.
    SET PROPERTY OF lo_zl 'Value' = val .
    GET PROPERTY OF lo_zl 'Font' = lo_f.
    SET PROPERTY OF lo_f 'Bold' = bold .

    IF color IS NOT INITIAL.
      GET PROPERTY OF lo_zl 'Interior' = lo_interior .
      SET PROPERTY OF lo_interior 'ColorIndex' = color.
*      SET PROPERTY OF lo_interior 'Color' = color.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

************************************************************
******** <<<<< SPLIT LIST_ALV ÇAĞIRMA >>>>>> ***************

* IF go_grid1 IS NOT BOUND.
*    gs_layout1-col_opt = 'X'.
*    gs_layout1-ctab_fname = 'CELLCOLOR'.
*
*    CREATE OBJECT go_custom_container
*      EXPORTING
*        container_name = 'CUSTOM_CONTAINER'.
*
*    CREATE OBJECT splitter
*      EXPORTING
*        parent  = go_custom_container
*        rows    = 2
*        columns = 1
*        align   = 15.
*
*    CALL METHOD splitter->get_container
*      EXPORTING
*        row       = 1
*        column    = 1
*      RECEIVING
*        container = go_container1.
*
*    zor_cl_local=>list_alv(
*       EXPORTING
*         iv_structure_name = 'ZPP_011_S_ALV1'
*         is_layout = gs_layout1
*         it_data = 'GT_DATA1'
*         iv_con_name = 'CON9001'
*         iv_double_click = 'X'
*         iv_split_con = 'X'
*       CHANGING
*         co_custom_container = go_custom_container
*         co_gui_container = go_container1
*         co_grid = go_grid1
*         ct_fcat = gt_fcat1
*         ).
*
*  ENDIF.

************************************************************
******** <<<<< SPLIT LIST_ALV ÇAĞIRMA - bitiş >>>>>> *******
************************************************************
