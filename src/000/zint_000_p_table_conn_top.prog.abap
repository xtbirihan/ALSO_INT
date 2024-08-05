*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_TABLE_CONN_TOP
*&---------------------------------------------------------------------*
DATA: okcode TYPE sy-ucomm.
DATA: gs_header TYPE zint_000_s_header,
      gt_9001   TYPE TABLE OF zint_000_s_tab1,
      gt_9002   TYPE TABLE OF zint_000_s_tab2.

DATA : gt_fcat9001      TYPE lvc_t_fcat,
       go_grid9001      TYPE REF TO cl_gui_alv_grid,
       go_container9001 TYPE REF TO cl_gui_custom_container,
       gs_layout9001    TYPE lvc_s_layo.

DATA : gt_fcat9002      TYPE lvc_t_fcat,
       go_grid9002      TYPE REF TO cl_gui_alv_grid,
       go_container9002 TYPE REF TO cl_gui_custom_container,
       gs_layout9002    TYPE lvc_s_layo.

DATA: BEGIN OF gs_property,
        fieldname TYPE fieldname,
        property  TYPE fieldname,
        value(30),
      END OF gs_property .
