*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_002_TOP
*&---------------------------------------------------------------------*

report zint_000_p_002.

class cl_main definition create private.
  public section.
    types:
      begin of mty_stab,

        s_itype  type range of zint_000_t_003-int_type,
        s_sitype type range of zint_000_t_003-int_subtype,
        s_guid   type range of zint_000_t_003-guid,
        s_datum  type range of zint_000_t_003-datum,
        s_uzeit  type range of zint_000_t_003-uzeit,
        s_user   type range of zint_000_t_003-username,

      end of mty_stab.
    data:ms_stab type mty_stab.

    types : begin of ty_data,
              flg            type  char1,
              durum          type  zint_000_de_durum,
              int_type       type  zint_000_de_itype,
              int_subtype    type  zint_000_de_istype,
              description    type  zint_000_de_idesc,
              direction      type  zint_000_de_direction,
              direction_icon type  zint_000_de_direction_icon,
              guid           type  zint_000_de_guid,
              statu          type  zint_000_de_statu,
              statu_tanim    type  zint_000_de_statu_tanim,
              datum          type  datum,
              uzeit          type  uzeit,
              username       type  xubname,
              rework         type  zint_000_de_rework,
              technical_name type  zint_000_de_tname,
              cellcolor      type  lvc_t_scol,
            end of ty_data.

    data : mt_alv_data type table of ty_data,
           ms_alv_data type ty_data.
    data : gv_field_dats type lvc_s_fcat-fieldname.
    data :
      mo_grid      type ref to cl_gui_alv_grid,
      mo_container type ref to cl_gui_docking_container.

    class-methods:
      create_instance returning value(r_obj) type ref to cl_main.

    methods :
      pbo importing value(iv_dynnr) type syst_dynnr,
      pai,
      fieldcat changing ct_fcat type lvc_t_fcat,
      exclude_button exporting et_exclude  type ui_functions,
      run,
      display,
      set_seltab importing value(iv_repid) type sy-repid,
      read_data exceptions no_data_found ,
      f4_s_sitype changing s_sitype type zint_000_de_istype,
      set_initial_values,
      display_popup_alv exceptions display_popup.
    methods:
      handle_hotspot_click for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id sender.
  private section.
    class-data:
      mo_instance type ref to cl_main.
endclass.
type-pools: icon.
tables: zint_000_t_003.
data : go_obj type ref to cl_main.
data : begin of ls_condtab.
         include structure hrcond.
data : end of ls_condtab.
data : lt_condtab like table of ls_condtab.

types: begin of ty_s_clause.
types:   line(72)  type c.
types: end of ty_s_clause.
data: lt_where_clauses  type standard table of ty_s_clause with default key.
data : wa_dyn_line type ref to data.

data : lv_str_name type tabname,
       lt_fcat     type lvc_t_fcat,
       rt_fcat     type lvc_t_fcat.
data: lt_dyn_table type ref to data.
data: lt_dyn_table2 type ref to data.
field-symbols: <gt_alv2_1> type table.
field-symbols: <lt_tab> type table.
field-symbols: <wa_dyn> type any.
field-symbols: <fs_fcat> type lvc_t_fcat.
field-symbols: <fs_popup> type table.

selection-screen begin of block bl1 with frame title text-003.
  select-options: s_itype  for zint_000_t_003-int_type no intervals no-extension matchcode object zint_000_sh_itype obligatory,
                  s_sitype for zint_000_t_003-int_subtype no intervals no-extension ,
                  s_guid   for zint_000_t_003-guid,
                  s_datum  for zint_000_t_003-datum,
                  s_uzeit  for zint_000_t_003-uzeit,
                  s_user   for zint_000_t_003-username,
                  s_statu  for zint_000_t_003-statu.
selection-screen end of block bl1.
selection-screen begin of block bl2 with frame title text-006.
  selection-screen begin of line.
    selection-screen comment 2(4) lv_icon1.
    selection-screen comment 7(25) text-004.
    parameters: pa_inb as checkbox default 'X'.
    selection-screen comment 46(4) lv_icon3.
    selection-screen comment 50(25) text-007.
    parameters: pa_ok as checkbox default 'X'.
  selection-screen end of line.
  selection-screen begin of line.
    selection-screen comment 2(4) lv_icon2.
    selection-screen comment 7(25) text-005.
    parameters: pa_outb as checkbox default 'X'.
    selection-screen comment 46(4) lv_icon4.
    selection-screen comment 50(25) text-008.
    parameters: pa_non as checkbox default 'X'.
  selection-screen end of line.
  selection-screen begin of line.
    selection-screen comment 46(4) lv_icon5.
    selection-screen comment 50(25) text-009.
    parameters: pa_nok as checkbox default 'X'.
  selection-screen end of line.
selection-screen end of block bl2.
