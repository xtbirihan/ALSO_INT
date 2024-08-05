*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_001_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: icon.
TABLES: zint_000_t_003.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: s_itype FOR zint_000_t_003-int_type NO INTERVALS NO-EXTENSION MATCHCODE OBJECT zint_000_sh_itype OBLIGATORY,
                  s_sitype FOR zint_000_t_003-int_subtype NO INTERVALS NO-EXTENSION,
                  s_guid  FOR zint_000_t_003-guid,
                  s_datum FOR zint_000_t_003-datum,
                  s_uzeit FOR zint_000_t_003-uzeit,
                  s_user  FOR zint_000_t_003-username.
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-006.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(4) lv_icon1.
    SELECTION-SCREEN COMMENT 7(25) TEXT-004.
    PARAMETERS: pa_inb AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 46(4) lv_icon3.
    SELECTION-SCREEN COMMENT 50(25) TEXT-007.
    PARAMETERS: pa_ok AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(4) lv_icon2.
    SELECTION-SCREEN COMMENT 7(25) TEXT-005.
    PARAMETERS: pa_outb AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 46(4) lv_icon4.
    SELECTION-SCREEN COMMENT 50(25) TEXT-008.
    PARAMETERS: pa_non AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 46(4) lv_icon5.
    SELECTION-SCREEN COMMENT 50(25) TEXT-009.
    PARAMETERS: pa_nok AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl2.

DATA: okcode TYPE sy-ucomm.

DATA: gs_alv1 TYPE zint_000_s_alv1,
      gt_alv1 TYPE TABLE OF zint_000_s_alv1.

DATA: BEGIN OF gs_alv2_kirilim,
        sira      TYPE numc2,
        structure TYPE typename,
      END OF gs_alv2_kirilim.
DATA: gt_alv2_kirilim LIKE TABLE OF gs_alv2_kirilim.

DATA: BEGIN OF gs_alv2_connection,
        parent_table TYPE zint_000_de_ptable,
        child_table  TYPE zint_000_de_ctable,
      END OF gs_alv2_connection.
DATA: gt_alv2_connection LIKE TABLE OF gs_alv2_connection.
DATA: BEGIN OF gs_alv2_structures,
        lines       TYPE i,
        grid_name   TYPE char10,
        tabname     TYPE typename,
        int_tabname TYPE typename,
        displayed   TYPE char1,
      END OF gs_alv2_structures.
DATA: gt_alv2_structures like TABLE OF gs_alv2_structures.


FIELD-SYMBOLS: <gt_alv2_1> TYPE table,
               <gt_alv2_2> TYPE table,
               <gt_alv2_3> TYPE table.

DATA: gs_alv3 TYPE zint_000_s_alv3,
      gt_alv3 TYPE TABLE OF zint_000_s_alv3.

DATA: gs_alv3_2 TYPE zint_000_s_alv3_2,
      gt_alv3_2 TYPE TABLE OF zint_000_s_alv3_2.

DATA: BEGIN OF gs_property,
        fieldname TYPE fieldname,
        property  TYPE fieldname,
        value(30),
      END OF gs_property .

DATA: splitter            TYPE REF TO cl_gui_splitter_container,
      go_custom_container TYPE REF TO cl_gui_custom_container.
DATA: splitter_2            TYPE REF TO cl_gui_splitter_container,
      splitter_3            TYPE REF TO cl_gui_splitter_container,
      go_custom_container_2 TYPE REF TO cl_gui_custom_container.

DATA : gt_fcat1      TYPE lvc_t_fcat,
       go_grid1      TYPE REF TO cl_gui_alv_grid,
       go_container1 TYPE REF TO cl_gui_container,
       gs_layout1    TYPE lvc_s_layo.

DATA : gt_fcat2      TYPE lvc_t_fcat,
       go_grid2      TYPE REF TO cl_gui_alv_grid,
       go_container2 TYPE REF TO cl_gui_container,
       gs_layout2    TYPE lvc_s_layo.

DATA : gt_fcat2_1      TYPE lvc_t_fcat,
       go_grid2_1      TYPE REF TO cl_gui_alv_grid,
       go_container2_1 TYPE REF TO cl_gui_container,
       gs_layout2_1    TYPE lvc_s_layo.

DATA : gt_fcat2_2      TYPE lvc_t_fcat,
       go_grid2_2      TYPE REF TO cl_gui_alv_grid,
       go_container2_2 TYPE REF TO cl_gui_container,
       gs_layout2_2    TYPE lvc_s_layo.

DATA : gt_fcat2_3      TYPE lvc_t_fcat,
       go_grid2_3      TYPE REF TO cl_gui_alv_grid,
       go_container2_3 TYPE REF TO cl_gui_container,
       gs_layout2_3    TYPE lvc_s_layo.

DATA : gt_fcat3      TYPE lvc_t_fcat,
       go_grid3      TYPE REF TO cl_gui_alv_grid,
       go_container3 TYPE REF TO cl_gui_container,
       gs_layout3    TYPE lvc_s_layo.

DATA : gt_fcat3_1      TYPE lvc_t_fcat,
       go_grid3_1      TYPE REF TO cl_gui_alv_grid,
       go_container3_1 TYPE REF TO cl_gui_container,
       gs_layout3_1    TYPE lvc_s_layo.

DATA : gt_fcat3_2      TYPE lvc_t_fcat,
       go_grid3_2      TYPE REF TO cl_gui_alv_grid,
       go_container3_2 TYPE REF TO cl_gui_container,
       gs_layout3_2    TYPE lvc_s_layo.
