**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSGDT
*&---------------------------------------------------------------------*

TABLES:
  /scwm/s_ui_pi,
  /scwm/s_aspq_pi_create,
  /scwm/s_aspq_pi_process,
  /scmb/s_ta_fields,
  /scwm/s_asp_oi_pi_item_create,
  /scwm/s_asp_oi_pi_item_proces,
  /scwm/s_asp_pi_proc_od,
  /scwm/s_asp_pi_proc_od_diff,
  /scwm/s_asp_pi_proc_od_book,
  /scwm/s_asp_pi_proc_od_deriv,
  /scwm/s_asp_pi_proc_od_ref,
  /scwm/s_asp_pi_proc_od_log,
  wsd_basic_mat_sty,
  /scwm/s_huhdr,
  /scwm/s_asp_pi_proc_od2,
  /scwm/s_asp_pi_proc_od2_diff,
  /scwm/s_asp_pi_proc_od2_book.

* For external call via FM
DATA:
  gv_process      TYPE /scwm/de_ui_pi_process ##NEEDED,
  gt_selection    TYPE /scmb/t_sp_selection ##NEEDED,
  gt_item_process TYPE /scwm/pi_t_aspk_item ##NEEDED.

* Today
DATA:
  gv_today TYPE sy-datum ##NEEDED.

* Message text (only for message statement to fill message variables)
DATA:
  gv_message_text TYPE string ##NEEDED.

* Dynpro Elements
DATA:
  gv_okcode                TYPE syucomm,
* Subscreens
  gv_sub_complete          TYPE sydynnr,
  gv_sub_complete_oip      TYPE sydynnr,
  gv_sub_complete_odp      TYPE sydynnr,
  gv_sub_complete_odp2     TYPE sydynnr,
  gv_sub_search_value      TYPE sydynnr,
  gv_sub_adv_search        TYPE sydynnr,
  gv_sub_sel_screen        TYPE sydynnr,

  gv_sub_oip               TYPE sydynnr,

  gv_sub_oip_data          TYPE sydynnr,

  gv_sub_odp_pi_count      TYPE sydynnr,
  gv_sub_odp_pi_diff       TYPE sydynnr,

  gv_sub_odp_pi_data       TYPE sydynnr,
  gv_sub_odp_pi_diff_data  TYPE sydynnr,

  gv_sub_odp_pi_book       TYPE sydynnr,
  gv_sub_odp_pi_book_data  TYPE sydynnr,

  gv_sub_odp_pi_deriv      TYPE sydynnr,
  gv_sub_odp_pi_deriv_data TYPE sydynnr,

  gv_sub_odp_pi_ref        TYPE sydynnr,
  gv_sub_odp_pi_ref_data   TYPE sydynnr,

  gv_sub_odp_pi_log        TYPE sydynnr,
  gv_sub_odp_pi_log_data   TYPE sydynnr,

  gv_sub_odp2_pi_count     TYPE sydynnr,
  gv_sub_odp2_pi_diff      TYPE sydynnr,

  gv_sub_odp2_pi_data      TYPE sydynnr,
  gv_sub_odp2_pi_diff_data TYPE sydynnr,

  gv_sub_odp2_pi_book      TYPE sydynnr,
  gv_sub_odp2_pi_book_data TYPE sydynnr,

* Tabs
  gv_tab_odp_pi_count      TYPE char20,   "Count result TAB
  gv_tab_odp_pi_book       TYPE char20,   "Book quantity TAB
  gv_tab_odp_pi_deriv      TYPE char20,   "Derivations TAB
  gv_tab_odp_pi_diff       TYPE char20,   "Differences TAB
  gv_tab_odp_pi_log        TYPE char20,   "LOG entries TAB
  gv_tab_odp_pi_ref        TYPE char20,   "Refference TAB
  gv_tab_odp2_pi_count     TYPE char20,   "Count result TAB
  gv_tab_odp2_pi_book      TYPE char20,   "Book quantity TAB
  gv_tab_odp2_pi_diff      TYPE char20,   "Derivations TAB

* Reference to UI fields
  gs_ta_process            TYPE /scmb/s_transaction_process , "#EC NEEDED

* Pushbuttons
  cmd_toggle_advanced      TYPE char30.

* Tab Controls
CONTROLS:
  gv_tab_oip  TYPE TABSTRIP,
  gv_tab_odp  TYPE TABSTRIP,
  gv_tab_odp2 TYPE TABSTRIP.

* Transaction manager
DATA:
  go_ta_manager TYPE REF TO lcl_ta_manager.                 "#EC NEEDED

* Flexible Bin create
DATA:
  gv_create_dynbin TYPE c .                                 "#EC NEEDED

* Advanced search selection screen
INCLUDE zui_pi_processsel.
