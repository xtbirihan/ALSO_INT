**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
* Added additional parameters to method convert_get_stock_2_oi_pi_cr
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSMPD
*&---------------------------------------------------------------------*

CLASS lcl_sp_mapper DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ys_msg_handler,
        msg          TYPE symsg,
        aspect       TYPE string,
        o_aspect_key TYPE REF TO data,
        aspect_field TYPE string,
      END OF ys_msg_handler.

    METHODS:
      constructor,

      convert_get_loc_2_oi_pi_cr
        IMPORTING
          it_item          TYPE /scwm/t_pi_location_for_area
          iv_reference_ui  TYPE /scwm/pi_de_reference_ui
          iv_count_time_ui TYPE /scwm/pi_de_count_time
        EXPORTING
          et_asp_oi_pi_cr  TYPE /scwm/t_asp_oi_pi_item_create,

      convert_get_stock_2_oi_pi_cr
        IMPORTING
          it_item          TYPE /scwm/t_pi_stock_for_area
          it_huhdr         TYPE /scwm/tt_huhdr_int
          it_huitm         TYPE  /scwm/tt_huitm_int
          iv_reference_ui  TYPE /scwm/pi_de_reference_ui
          iv_count_time_ui TYPE /scwm/pi_de_count_time
          it_mara          TYPE ztt_mara_sel
        EXPORTING
          et_asp_oi_pi_cr  TYPE /scwm/t_asp_oi_pi_item_create,

      convert_item_read_2_oi_pi_pr
        IMPORTING
          it_item         TYPE /lime/pi_t_item_read_getsingle
        EXPORTING
          et_asp_oi_pi_pr TYPE /scwm/t_asp_oi_pi_item_proces,

      convert_item_read_2_od_book
        IMPORTING
          is_item           TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_book TYPE /scwm/t_asp_pi_proc_od_book,

      convert_item_read_2_od_co
        IMPORTING
          is_item         TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_co TYPE /scwm/t_asp_pi_proc_od,

      convert_item_read_2_od_deriv
        IMPORTING
          is_item            TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_deriv TYPE /scwm/t_asp_pi_proc_od_deriv,

      convert_item_read_2_od_diff
        IMPORTING
          is_item           TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_diff TYPE /scwm/t_asp_pi_proc_od_diff,

      convert_item_read_2_od_log
        IMPORTING
          is_item          TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_log TYPE /scwm/t_asp_pi_proc_od_log,

      convert_item_read_2_od_ref
        IMPORTING
          is_item          TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_asp_od_pi_ref TYPE /scwm/t_asp_pi_proc_od_ref,

      convert_bapiret_2_msg_handler
        IMPORTING
          is_bapiret            TYPE bapiret2
          iv_aspect             TYPE string
          it_aspect_key         TYPE INDEX TABLE
        RETURNING
          VALUE(es_msg_handler) TYPE ys_msg_handler,

      convert_item_read_2_od2_co
        IMPORTING
          is_item       TYPE /lime/pi_item_read_get_single
          it_asp_od_co  TYPE /scwm/t_asp_pi_proc_od
        EXPORTING
          et_asp_od2_co TYPE /scwm/t_asp_pi_proc_od2,

      convert_item_read_2_od2_book
        IMPORTING
          it_book           TYPE /lime/pi_t_item_sub_read_get_s
          is_asp_od_pi_book TYPE /scwm/s_asp_pi_proc_od_book
        EXPORTING
          et_asp_od2_bo     TYPE /scwm/t_asp_pi_proc_od2,

      convert_item_read_2_od2_diff
        IMPORTING
          it_diff         TYPE /lime/pi_t_item_sub_read_get_s
          it_asp_od_pi_di TYPE /scwm/t_asp_pi_proc_od_diff
        EXPORTING
          et_asp_od2_di   TYPE /scwm/t_asp_pi_proc_od2_diff.


  PRIVATE SECTION.

    DATA:
      mo_stock_fields TYPE REF TO /scwm/cl_ui_stock_fields.

    METHODS:

      set_count_quanity
        IMPORTING
          is_item         TYPE /lime/pi_item_sub_read_get_sin
        CHANGING
          cs_asp_od_pi_co TYPE /scwm/s_asp_pi_proc_od,
*     Suppress dummy-HUs after LIME query call
      condense_tree
        IMPORTING
          is_item TYPE /lime/pi_item_read_get_single
        EXPORTING
          et_diff TYPE /lime/pi_t_item_sub_read_get_s,

      convert_item_read_2_stock
        IMPORTING
          iv_lgnum           TYPE /scwm/lgnum
          is_item            TYPE /lime/pi_item_pos_sub
        RETURNING
          VALUE(es_ui_stock) TYPE /scwm/s_ui_stock,

      convert_item_read_2_quan
        IMPORTING
          is_item           TYPE /lime/pi_item_pos_sub
        RETURNING
          VALUE(es_ui_quan) TYPE /scwm/s_ui_quan,

      convert_bapiret_2_msg
        IMPORTING
          is_bapiret    TYPE bapiret2
        RETURNING
          VALUE(es_msg) TYPE symsg.

ENDCLASS.                    "lcl_sp_mapper DEFINITION
