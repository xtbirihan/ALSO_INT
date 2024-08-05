*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_002_CLS
*&---------------------------------------------------------------------*
class cl_main implementation.
  method create_instance.
    if mo_instance is initial.
      mo_instance  = new cl_main( ).
    endif.
    lv_icon1 = icon_arrow_left.
    lv_icon2 = icon_arrow_right.
    lv_icon3 = icon_green_light.
    lv_icon4 = icon_yellow_light.
    lv_icon5 = icon_red_light.
    r_obj = mo_instance .
  endmethod.
  method set_seltab.
    data: lr_line    type ref to data,
          lv_field   type lvc_fname,
          lv_selname type rsscr_name.

    data: lt_criteria type rsparams_tt.

    field-symbols: <fs_soption>      type table,
                   <fs_parameter>    type any,
                   <fs_criteria>     type rsparams,
                   <fs_soption_line> type any.

    call function 'RS_REFRESH_FROM_SELECTOPTIONS'
      exporting
        curr_report     = iv_repid
      tables
        selection_table = lt_criteria
      exceptions
        not_found       = 1
        no_report       = 2
        others          = 3.

    clear: me->ms_stab.

    loop at lt_criteria assigning <fs_criteria>
                        where ( kind eq 'P' and low  is not initial )
                           or ( kind eq 'S' and sign is not initial ).
      if lv_selname ne <fs_criteria>-selname.
        lv_selname = <fs_criteria>-selname.
        unassign <fs_soption>.
      endif.
      lv_field = 'ME->MS_STAB-' && <fs_criteria>-selname.
      case <fs_criteria>-kind.
        when 'P'.
          assign (lv_field) to <fs_parameter>.
          check sy-subrc = 0.
          <fs_parameter> = <fs_criteria>-low.
        when 'S'.
          assign (lv_field) to <fs_soption>.
          check sy-subrc = 0.
          create data lr_line like line of <fs_soption>.
          check sy-subrc = 0.
          assign lr_line->* to <fs_soption_line>.
          check sy-subrc = 0.

          move-corresponding <fs_criteria> to <fs_soption_line>.
          append <fs_soption_line> to <fs_soption>.
      endcase.
    endloop.
  endmethod.
  method run.
    me->read_data(
      exceptions
        no_data_found = 1 ).

    check sy-subrc eq 0.

    me->display( ).
  endmethod.
  method read_data.
    if s_sitype[] is initial .
      message 'Entegrasyon alt tipini giriniz!' type 'S' display like 'E'.
      raise no_data_found.
    endif.
    select *
      from zint_000_t_004
      into table @data(lt_004)
      where statu in @s_statu.

    data: lr_direction type range of zint_000_t_002-direction.
    data: lr_statu type range of zint_000_t_003-statu.
    if pa_inb eq 'X'.
      append initial line to lr_direction reference into data(lr_dir).
      lr_dir->sign = 'I'. lr_dir->option = 'EQ'. lr_dir->low = 'I'.
    else.
      append initial line to lr_direction reference into lr_dir.
      lr_dir->sign = 'E'. lr_dir->option = 'EQ'. lr_dir->low = 'I'.
    endif.
    if pa_outb eq 'X'.
      append initial line to lr_direction reference into lr_dir.
      lr_dir->sign = 'I'. lr_dir->option = 'EQ'. lr_dir->low = 'O'.
    else.
      append initial line to lr_direction reference into lr_dir.
      lr_dir->sign = 'E'. lr_dir->option = 'EQ'. lr_dir->low = 'O'.
    endif.

    loop at lt_004 into data(ls_004).
      if ls_004-type eq 'S'.
        if pa_ok eq 'X'.
          append initial line to lr_statu reference into data(lr_st).
          lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        else.
          append initial line to lr_statu reference into lr_st.
          lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        endif.
      elseif ls_004-type eq 'E'.
        if pa_nok eq 'X'.
          append initial line to lr_statu reference into lr_st.
          lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        else.
          append initial line to lr_statu reference into lr_st.
          lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        endif.
      elseif ls_004-type eq 'W'.
        if pa_non eq 'X'.
          append initial line to lr_statu reference into lr_st.
          lr_st->sign = 'I'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        else.
          append initial line to lr_statu reference into lr_st.
          lr_st->sign = 'E'. lr_st->option = 'EQ'. lr_st->low = ls_004-statu.
        endif.
      endif.
    endloop.


    select a~int_type , a~int_subtype , b~description , b~direction , a~guid , a~statu , a~datum , a~uzeit , a~username , b~rework , b~technical_name
      from zint_000_t_003 as a inner join zint_000_t_002 as b on
      b~int_type eq a~int_type and
      b~int_subtype eq a~int_subtype
      inner join zint_000_t_001 as c on
      c~int_type eq a~int_type
      into table @data(lt_003)
      where a~int_type in @s_itype and
            a~int_subtype in @s_sitype and
            a~guid in @s_guid and
            a~datum in @s_datum and
            a~uzeit in @s_uzeit and
            a~username in @s_user and
            a~statu in @lr_statu and
            b~direction in @lr_direction.

*    check lt_003 is not initial.
    sort lt_003 by datum descending uzeit descending.

    loop at lt_003 into data(ls_003).
      clear: me->ms_alv_data.

      me->ms_alv_data = value #(
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

      read table lt_004 into ls_004 with key direction = ls_003-direction statu = ls_003-statu.
      if sy-subrc eq 0.
        me->ms_alv_data-statu_tanim = ls_004-description.
        if ls_004-type eq 'W'.
          me->ms_alv_data-durum = '@09@'.
        elseif ls_004-type eq 'S'.
          me->ms_alv_data-durum = '@08@'.
        elseif ls_004-type eq 'E'.
          me->ms_alv_data-durum = '@0A@'.
        endif.
      endif.

      if me->ms_alv_data-direction = 'O'.
        me->ms_alv_data-direction_icon = '@9T@'.
      elseif me->ms_alv_data-direction eq 'I'.
        me->ms_alv_data-direction_icon = '@9S@'.
      endif.

      append me->ms_alv_data to me->mt_alv_data.
    endloop.


    if me->mt_alv_data is initial.
      message i789(m7) display like 'E'.
      raise no_data_found.
    endif.
*--------------------------------------------------------------------*
    "dinamik alanları alma
    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
        i_structure_name       = 'ZINT_000_S_ALV1'
      changing
        ct_fieldcat            = lt_fcat
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    loop at lt_fcat reference into data(wr_fcat).
      case wr_fcat->fieldname.
        when 'GUID'.
          wr_fcat->hotspot = 'X'.
      endcase.
    endloop.
    append value #(
        fieldname = 'MKPF_MBLNR'
        tabname   = 'MKPF'
        scrtext_l = 'MKPF_MBLNR'
        scrtext_m = 'MKPF_MBLNR'
        scrtext_s = 'MKPF_MBLNR'
        reptext  = 'MKPF_MBLNR'
        hotspot = 'X'
     ) to lt_fcat .

    append value #(
    fieldname = 'DURUM2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Durum2'
    scrtext_m = 'Durum2'
    scrtext_s = 'Durum2'
    reptext  = 'Durum2'
 ) to lt_fcat .

    append value #(
    fieldname = 'COUNTER'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Sayaç'
    scrtext_m = 'Sayaç'
    scrtext_s = 'Sayaç'
    reptext  = 'Sayaç'
 ) to lt_fcat .

    append value #(
    fieldname = 'DATUM2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Tarih2'
    scrtext_m = 'Tarih2'
    scrtext_s = 'Tarih2'
    reptext  = 'Tarih2'
    datatype = 'DATS'
 ) to lt_fcat .

    append value #(
    fieldname = 'UZEIT2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Saat2'
    scrtext_m = 'Saat2'
    scrtext_s = 'Saat2'
    reptext  = 'Saat2'
    datatype = 'TIMS'
 ) to lt_fcat .

    append value #(
    fieldname = 'USERNAME2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Kullanıcı2'
    scrtext_m = 'Kullanıcı2'
    scrtext_s = 'Kullanıcı2'
    reptext  = 'Kullanıcı2'
 ) to lt_fcat .

    append value #(
    fieldname = 'STATU2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Statü2'
    scrtext_m = 'Statü2'
    scrtext_s = 'Statü2'
    reptext  = 'Statü2'
) to lt_fcat .

    append value #(
    fieldname = 'STATU_TANIM2'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Statü tanımı2'
    scrtext_m = 'Statü tanımı2'
    scrtext_s = 'Statü tanımı2'
    reptext  = 'Statü tanımı2'
    datatype = 'CHAR'
    intlen = '000050'
) to lt_fcat .

    append value #(
    fieldname = 'ESKI_STATU'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Eski statü'
    scrtext_m = 'Eski statü'
    scrtext_s = 'Eski statü'
    reptext  = 'Eski statü'
) to lt_fcat .

    append value #(
    fieldname = 'ESKI_STATU_TANIM'
    tabname   = 'ZINT_000_S_ALV3'
    scrtext_l = 'Statü tanımı'
    scrtext_m = 'Statü tanımı'
    scrtext_s = 'Statü tanımı'
    reptext  = 'Statü tanımı'
    datatype = 'CHAR'
    intlen = '000050'
) to lt_fcat .

    append value #(
    fieldname = 'BALLOG_ID'
    tabname   = 'ZINT_000_S_ALV3'
    no_out = 'X'
    tech = 'X'
    domname = 'SYSUUID_22'
    datatype = 'CHAR'
    intlen = '000022'
) to lt_fcat .

    append value #(
    fieldname = 'BALLOG_NUMBER'
    tabname   = 'ZINT_000_S_ALV3'
    no_out = 'X'
    tech = 'X'
    domname = 'BALOGNR'
    datatype = 'CHAR'
    intlen = '000020'
) to lt_fcat .



    delete lt_fcat where fieldname = 'FLG'.
    delete lt_fcat where fieldname = 'REWORK'.
    delete lt_fcat where fieldname = 'TECHNICAL_NAME'.

    select single * from zint_000_t_002 into @data(ls_t002)
      where int_type in @s_itype
        and int_subtype in @s_sitype.

    lv_str_name = ls_t002-table_header.

    call function 'LVC_FIELDCATALOG_MERGE'
      exporting
        i_structure_name       = lv_str_name
      changing
        ct_fieldcat            = rt_fcat
      exceptions
        inconsistent_interface = 1
        program_error          = 2
        others                 = 3.

    if ls_t002-technical_name = 'X'.
      loop at rt_fcat reference into data(lr_fcat).
        lr_fcat->scrtext_l = lr_fcat->fieldname.
        lr_fcat->scrtext_m = lr_fcat->fieldname.
        lr_fcat->scrtext_s = lr_fcat->fieldname.
        lr_fcat->reptext = lr_fcat->fieldname.
      endloop.
    endif.

    call method cl_alv_table_create=>create_dynamic_table
      exporting
        it_fieldcatalog = rt_fcat
      importing
        ep_table        = lt_dyn_table2.
    assign lt_dyn_table2->* to <gt_alv2_1>.

    loop at lt_fcat into data(ls_fcat).
      delete rt_fcat where fieldname = ls_fcat-fieldname.
      delete rt_fcat where fieldname = 'MANDT'.
    endloop.

    read table rt_fcat into data(rs_fcat) with key key = 'X'.
    data(lv_field) = rs_fcat-fieldname.
    read table rt_fcat into data(rs_fcat2) with key datatype = 'DATS'.
    data(lv_field_dats) = rs_fcat2-fieldname.
    me->gv_field_dats = lv_field_dats.
    append lines of rt_fcat to lt_fcat.

    call method cl_alv_table_create=>create_dynamic_table
      exporting
        it_fieldcatalog = lt_fcat
      importing
        ep_table        = lt_dyn_table.

    assign lt_dyn_table->* to <lt_tab>.
    assign lt_fcat to <fs_fcat>.

    select *
      from zint_000_t_008
      into table @data(lt_008)
      where int_type in @s_itype and
            int_subtype in @s_sitype.

    field-symbols : <fs_ch10>  type any,
                    <fs_dats>  type sy-datum,
                    <fs_mblnr> type any.

    data: ls_alv3 type zint_000_s_alv3,
          lt_alv3 type table of zint_000_s_alv3.

    loop at me->mt_alv_data into data(ls_alv1).
*      append initial line to <lt_tab> assigning field-symbol(<fs3>).
*      <fs3> = corresponding #( ls_alv1 ).
      free : lt_condtab , lt_alv3.
      loop at lt_008 into data(ls_008) where child_table eq lv_str_name.
        if ls_008-parent_table eq 'ZINT_000_T_003' .
          assign component ls_008-parent_field of structure ls_alv1 to field-symbol(<fs>).
          if <fs> is assigned.
            ls_condtab = value #( field = ls_008-child_field opera = 'EQ' low = <fs> ). append ls_condtab to lt_condtab.
          endif.
        endif.
      endloop.
      if lt_condtab is not initial.
        call function 'RH_DYNAMIC_WHERE_BUILD'
          exporting
            dbtable         = space " can be empty
          tables
            condtab         = lt_condtab
            where_clause    = lt_where_clauses
          exceptions
            empty_condtab   = 01
            no_db_field     = 02
            unknown_db      = 03
            wrong_condition = 04.

        select *
          from (lv_str_name)
          into corresponding fields of table <gt_alv2_1>
          where (lt_where_clauses).

*        read table <gt_alv2_1> assigning field-symbol(<fs2>) with key ('GUID') = ls_alv1-guid.
        loop at <gt_alv2_1> assigning field-symbol(<fs2>) .
          assign component 'GUID' of structure <fs2> to field-symbol(<fs_guid>).
          if sy-subrc eq 0.
            loop at me->mt_alv_data into data(ms_alv) where guid = <fs_guid>.

              append initial line to <lt_tab> assigning field-symbol(<fs3>).
              <fs3> = corresponding #( ls_alv1 ).
              move-corresponding <fs2> to <fs3> expanding nested tables.

              if lv_field is not initial and
                 lv_field_dats is not initial.

                assign component lv_field of structure <fs2> to <fs_ch10>.
                assign component lv_field_dats of structure <fs2> to <fs_dats>.
                if strlen( <fs_ch10> ) = 10.
                  select mblnr from mkpf into table @data(lt_mblnr)
                   where xblnr = @<fs_ch10>
                     and mjahr = @<fs_dats>(4).
                endif.

                sort lt_mblnr by mblnr descending.
                read table lt_mblnr into data(ls_mblnr) index 1.
                if sy-subrc eq 0.
                  assign component 'MKPF_MBLNR' of structure <fs3> to <fs_mblnr>.
                  if sy-subrc eq 0.
                    <fs_mblnr> = ls_mblnr-mblnr.
                  endif.
                endif.

              endif.

              select *
                from zint_000_t_005
                into table @data(lt_005)
                 where int_type = @ls_alv1-int_type and
                       int_subtype = @ls_alv1-int_subtype and
                       guid = @ls_alv1-guid.
              check lt_005 is not initial.

              sort lt_005 by counter descending.

              loop at lt_005 into data(ls_005).
                clear: ls_alv3.
                ls_alv3 = value #(
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

                read table lt_004 into ls_004 with key statu = ls_005-statu.
                if sy-subrc eq 0.
                  ls_alv3-statu_tanim = ls_004-description.
                  if ls_004-type eq 'S'.
                    ls_alv3-durum = '@WB@'.
                  elseif ls_004-type eq 'E'.
                    ls_alv3-durum = '@WD@'.
                  else.
                    ls_alv3-durum = '@WC@'.
                  endif.
                endif.

                read table lt_004 into ls_004 with key statu = ls_005-eski_statu.
                if sy-subrc eq 0.
                  ls_alv3-eski_statu_tanim = ls_004-description.
                endif.

                append ls_alv3 to lt_alv3.
              endloop.
              clear ls_alv3.
              read table lt_alv3 into ls_alv3 index 1.
              if sy-subrc eq 0.
                assign component 'DURUM2' of structure <fs3> to field-symbol(<fs_d2>).
                if sy-subrc eq 0.
                  <fs_d2> = ls_alv3-durum.
                endif.
                assign component 'COUNTER' of structure <fs3> to field-symbol(<fs_ct>).
                if sy-subrc eq 0.
                  <fs_ct> = ls_alv3-counter.
                endif.
                assign component 'DATUM2' of structure <fs3> to field-symbol(<fs_dat2>).
                if sy-subrc eq 0.
                  <fs_dat2> = ls_alv3-datum.
                endif.
                assign component 'UZEIT2' of structure <fs3> to field-symbol(<fs_uz2>).
                if sy-subrc eq 0.
                  <fs_uz2> = ls_alv3-uzeit.
                endif.
                assign component 'USERNAME2' of structure <fs3> to field-symbol(<fs_us2>).
                if sy-subrc eq 0.
                  <fs_us2> = ls_alv3-username.
                endif.
                assign component 'STATU2' of structure <fs3> to field-symbol(<fs_st2>).
                if sy-subrc eq 0.
                  <fs_st2> = ls_alv3-statu.
                endif.
                assign component 'STATU_TANIM2' of structure <fs3> to field-symbol(<fs_st_t2>).
                if sy-subrc eq 0.
                  <fs_st_t2> = ls_alv3-statu_tanim.
                endif.
                assign component 'ESKI_STATU' of structure <fs3> to field-symbol(<fs_est>).
                if sy-subrc eq 0.
                  <fs_est> = ls_alv3-eski_statu.
                endif.
                assign component 'ESKI_STATU_TANIM' of structure <fs3> to field-symbol(<fs_est_t>).
                if sy-subrc eq 0.
                  <fs_est_t> = ls_alv3-eski_statu_tanim.
                endif.
                assign component 'BALLOG_ID' of structure <fs3> to field-symbol(<fs_bal_id>).
                if sy-subrc eq 0.
                  <fs_bal_id> = ls_alv3-ballog_id.
                endif.
                assign component 'BALLOG_NUMBER' of structure <fs3> to field-symbol(<fs_bal_num>).
                if sy-subrc eq 0.
                  <fs_bal_num> = ls_alv3-ballog_number.
                endif.
              endif.


            endloop.
          endif.



        endloop.
*        if sy-subrc eq 0.
*          move-corresponding <fs2> to <fs3> expanding nested tables.
*
*          if lv_field is not initial and
*             lv_field_dats is not initial.
*
*            assign component lv_field of structure <fs2> to <fs_ch10>.
*            assign component lv_field_dats of structure <fs2> to <fs_dats>.
*            if strlen( <fs_ch10> ) = 10.
*              select mblnr from mkpf into table @data(lt_mblnr)
*               where xblnr = @<fs_ch10>
*                 and mjahr = @<fs_dats>(4).
*            endif.
*
*            sort lt_mblnr by mblnr descending.
*            read table lt_mblnr into data(ls_mblnr) index 1.
*            if sy-subrc eq 0.
*              assign component 'MKPF_MBLNR' of structure <fs3> to <fs_mblnr>.
*              if sy-subrc eq 0.
*                <fs_mblnr> = ls_mblnr-mblnr.
*              endif.
*            endif.
*
*          endif.
*        endif.
      endif.

*      select *
*        from zint_000_t_005
*        into table @data(lt_005)
*        where int_type = @ls_alv1-int_type and
*              int_subtype = @ls_alv1-int_subtype and
*              guid = @ls_alv1-guid.
*      check lt_005 is not initial.
*
**      select *
**        from zint_000_t_004
**        into table @data(lt_004).
*
*      sort lt_005 by counter descending.
*
*      loop at lt_005 into data(ls_005).
*        clear: ls_alv3.
*        ls_alv3 = value #(
*        int_type = ls_005-int_type
*        int_subtype = ls_005-int_subtype
*        guid = ls_005-guid
*        counter = ls_005-counter
*        datum = ls_005-datum
*        uzeit = ls_005-uzeit
*        username = ls_005-username
*        statu = ls_005-statu
*        eski_statu = ls_005-eski_statu
*        ballog_id = ls_005-ballog_id
*        ballog_number = ls_005-ballog_number
*        ).
*
*        read table lt_004 into ls_004 with key statu = ls_005-statu.
*        if sy-subrc eq 0.
*          ls_alv3-statu_tanim = ls_004-description.
*          if ls_004-type eq 'S'.
*            ls_alv3-durum = '@WB@'.
*          elseif ls_004-type eq 'E'.
*            ls_alv3-durum = '@WD@'.
*          else.
*            ls_alv3-durum = '@WC@'.
*          endif.
*        endif.
*
*        read table lt_004 into ls_004 with key statu = ls_005-eski_statu.
*        if sy-subrc eq 0.
*          ls_alv3-eski_statu_tanim = ls_004-description.
*        endif.
*
*        append ls_alv3 to lt_alv3.
*      endloop.
*      clear ls_alv3.
*      read table lt_alv3 into ls_alv3 index 1.
*      if sy-subrc eq 0.
*        assign component 'DURUM2' of structure <fs3> to field-symbol(<fs_d2>).
*        if sy-subrc eq 0.
*          <fs_d2> = ls_alv3-durum.
*        endif.
*        assign component 'COUNTER' of structure <fs3> to field-symbol(<fs_ct>).
*        if sy-subrc eq 0.
*          <fs_ct> = ls_alv3-counter.
*        endif.
*        assign component 'DATUM2' of structure <fs3> to field-symbol(<fs_dat2>).
*        if sy-subrc eq 0.
*          <fs_dat2> = ls_alv3-datum.
*        endif.
*        assign component 'UZEIT2' of structure <fs3> to field-symbol(<fs_uz2>).
*        if sy-subrc eq 0.
*          <fs_uz2> = ls_alv3-uzeit.
*        endif.
*        assign component 'USERNAME2' of structure <fs3> to field-symbol(<fs_us2>).
*        if sy-subrc eq 0.
*          <fs_us2> = ls_alv3-username.
*        endif.
*        assign component 'STATU2' of structure <fs3> to field-symbol(<fs_st2>).
*        if sy-subrc eq 0.
*          <fs_st2> = ls_alv3-statu.
*        endif.
*        assign component 'STATU_TANIM2' of structure <fs3> to field-symbol(<fs_st_t2>).
*        if sy-subrc eq 0.
*          <fs_st_t2> = ls_alv3-statu_tanim.
*        endif.
*        assign component 'ESKI_STATU' of structure <fs3> to field-symbol(<fs_est>).
*        if sy-subrc eq 0.
*          <fs_est> = ls_alv3-eski_statu.
*        endif.
*        assign component 'ESKI_STATU_TANIM' of structure <fs3> to field-symbol(<fs_est_t>).
*        if sy-subrc eq 0.
*          <fs_est_t> = ls_alv3-eski_statu_tanim.
*        endif.
*        assign component 'BALLOG_ID' of structure <fs3> to field-symbol(<fs_bal_id>).
*        if sy-subrc eq 0.
*          <fs_bal_id> = ls_alv3-ballog_id.
*        endif.
*        assign component 'BALLOG_NUMBER' of structure <fs3> to field-symbol(<fs_bal_num>).
*        if sy-subrc eq 0.
*          <fs_bal_num> = ls_alv3-ballog_number.
*        endif.
*      endif.
    endloop.
    delete adjacent duplicates from <lt_tab> comparing all fields.


  endmethod.
  method display.
    call screen 0001.
  endmethod.
  method pbo.
    data : ls_layout  type lvc_s_layo,
           ls_variant type disvariant,
           lt_fcat    type lvc_t_fcat,
           lt_exclude type ui_functions.

    set pf-status 'PF100'.
    set titlebar 'T100'.

    if me->mo_grid is initial.
      create object me->mo_container
        exporting
          repid                       = sy-repid
          dynnr                       = iv_dynnr
          side                        = mo_container->dock_at_left
          extension                   = cl_gui_docking_container=>ws_maximizebox
        exceptions
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          others                      = 6.


      create object me->mo_grid
        exporting
          i_parent = me->mo_container.

      ls_layout-zebra      = 'X'.
      ls_layout-col_opt    = 'X'.
      ls_layout-cwidth_opt = 'X'.
      ls_layout-sel_mode   = 'A'.
      ls_layout-edit_mode  = 'A'.
      ls_layout-no_rowmark = 'X'.
*      ls_layout-info_fname = 'ROW_COLOR'.

      ls_variant-report   = sy-repid.
      ls_variant-handle   = 1.
      ls_variant-username = sy-uname.

*      me->fieldcat( changing ct_fcat = lt_fcat ).

      me->exclude_button( importing et_exclude = lt_exclude ).

      field-symbols: <ft_data> type any table.
      assign <lt_tab> to <ft_data>.

*      set handler : me->handle_data_changed for me->mo_grid.
*      SET HANDLER me->handle_toolbar FOR me->mo_grid.
*      SET HANDLER me->handle_user_command FOR me->mo_grid.
      set handler : me->handle_hotspot_click for me->mo_grid.

      call method me->mo_grid->set_table_for_first_display
        exporting
          i_bypassing_buffer   = 'X'
          is_layout            = ls_layout
          is_variant           = ls_variant
          i_save               = 'A'
          it_toolbar_excluding = lt_exclude
        changing
          it_fieldcatalog      = <fs_fcat>
          it_outtab            = <ft_data>.

      call method me->mo_grid->set_toolbar_interactive( ).
      call method me->mo_grid->check_changed_data( ).
      call method me->mo_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified
        exceptions
          error      = 1
          others     = 2.
      call method me->mo_grid->set_ready_for_input
        exporting
          i_ready_for_input = 1.

    else.
      me->mo_grid->refresh_table_display( ).
    endif.
  endmethod.
  method fieldcat.
    data:
      lo_columns      type ref to cl_salv_columns_table,
      lo_aggregations type ref to cl_salv_aggregations,
      lo_salv_table   type ref to cl_salv_table,
      lt_alv_data_ref type ref to data.

    get reference of me->mt_alv_data into lt_alv_data_ref.
    assign lt_alv_data_ref->* to field-symbol(<fs_datatab1>).

    try.
        cl_salv_table=>factory(
          exporting
            list_display = abap_false
          importing
            r_salv_table = lo_salv_table
          changing
            t_table      = <fs_datatab1> ).
      catch cx_salv_msg.
    endtry.

    lo_columns      = lo_salv_table->get_columns( ).
    lo_aggregations = lo_salv_table->get_aggregations( ).

    ct_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                       r_columns = lo_columns
                                  r_aggregations = lo_aggregations ).

    loop at ct_fcat reference into data(ls_fcat).
*      case ls_fcat->fieldname.
*        when 'STATUS'.
*          ls_fcat->scrtext_s = 'STATUS'.
*          ls_fcat->scrtext_l = 'STATUS'.
*          ls_fcat->scrtext_m = 'STATUS'.
*      endcase.
    endloop.

  endmethod.
  method exclude_button.
    append cl_gui_alv_grid=>mc_fc_loc_copy_row      to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_delete_row    to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_append_row    to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_insert_row    to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_move_row      to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_copy          to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_cut           to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_paste         to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_paste_new_row to et_exclude.
    append cl_gui_alv_grid=>mc_fc_loc_undo          to et_exclude.
    append cl_gui_alv_grid=>mc_fc_refresh           to et_exclude.
    append cl_gui_alv_grid=>mc_fc_print             to et_exclude.
    append cl_gui_alv_grid=>mc_fc_select_all        to et_exclude.
    append cl_gui_alv_grid=>mc_fc_deselect_all      to et_exclude.
    append cl_gui_alv_grid=>mc_fc_graph             to et_exclude.
    append cl_gui_alv_grid=>mc_fc_info              to et_exclude.
    append cl_gui_alv_grid=>mc_fc_print_prev        to et_exclude.
    append cl_gui_alv_grid=>mc_fc_call_abc          to et_exclude.
    append cl_gui_alv_grid=>mc_fc_sum               to et_exclude.
  endmethod.
  method pai.
    data(lv_ucomm) = sy-ucomm.
    clear sy-ucomm.

    case lv_ucomm.
      when '&F03' or '&F12' or '&F15'.
        set screen 0.
        leave screen.
    endcase.
  endmethod.
  method f4_s_sitype.

    if s_itype-low is initial.
      message text-001 type 'S' display like 'E'.
      return.
    endif.

    data: begin of ls_shelp,
            int_type    type zint_000_t_002-int_type,
            int_subtype type zint_000_t_002-int_subtype,
            description type zint_000_t_002-description,
          end of ls_shelp.
    data: lt_shelp like table of ls_shelp.
    data: lt_return type table of ddshretval .

    select int_type , int_subtype , description
      from zint_000_t_002
      into corresponding fields of table @lt_shelp
      where int_type = @s_itype-low.
    sort lt_shelp by int_subtype ascending.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'INT_SUBTYPE'
        value_org       = 'S'
      tables
        value_tab       = lt_shelp
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.
    try.
        s_sitype = lt_return[ 1 ]-fieldval.
      catch cx_sy_itab_line_not_found.

    endtry.



  endmethod .
  method handle_hotspot_click.
    data: ls_alv3_2 type zint_000_s_alv3_2,
          lt_alv3_2 type table of zint_000_s_alv3_2.
    read table <lt_tab> assigning field-symbol(<fs_alv>) index e_row_id-index.
    if sy-subrc = 0.
      assign component e_column_id-fieldname of structure <fs_alv> to field-symbol(<fs_value>).
    endif.

    case e_column_id-fieldname.
      when 'GUID'.
        assign component 'BALLOG_NUMBER' of structure <fs_alv> to field-symbol(<fs_bal_num>).
        if sy-subrc eq 0.
          zint_000_cl_bal_log=>read_ballog(
          exporting iv_ballog_number = <fs_bal_num>
          importing et_messages = data(lt_messages) ).
        endif.
        loop at lt_messages into data(ls_messages).
          clear: ls_alv3_2.
          if ls_messages-type eq 'E'.
            ls_alv3_2-type = '@1B@'.
          elseif ls_messages-type eq 'W'.
            ls_alv3_2-type = '@1A@'.
          elseif ls_messages-type eq 'S'.
            ls_alv3_2-type = '@1C@'.
          endif.
          ls_alv3_2-message = ls_messages-message.
          append ls_alv3_2 to lt_alv3_2.
        endloop.

        assign lt_alv3_2 to <fs_popup>.
        if sy-subrc eq 0.
          me->display_popup_alv(
            exceptions
              display_popup = 1
              others        = 2
          ).
        endif.

      when 'MKPF_MBLNR'.
        assign component 'MKPF_MBLNR' of structure <fs_alv> to field-symbol(<fs_mblnr>).
        assign component me->gv_field_dats of structure <fs_alv> to field-symbol(<fs_dats>).

        if <fs_mblnr> is not initial and
           <fs_mblnr> is assigned and
           <fs_dats> is not initial and
           <fs_dats> is assigned.

          select mblnr ,
                 mjahr ,
                 bwart ,
                 menge ,
                 matnr ,
                 meins from mseg into table @data(lt_mseg)
            where mblnr = @<fs_mblnr>
              and mjahr = @<fs_dats>(4).

          assign lt_mseg to <fs_popup>.
          if sy-subrc eq 0.
            me->display_popup_alv(
              exceptions
                display_popup = 1
                others        = 2
            ).
          endif.

        endif.

    endcase.
  endmethod.
  method set_initial_values.

    append value #( sign = 'I' option = 'EQ'
                    low = '22' ) to s_statu.

  endmethod.
  method display_popup_alv.

    if <fs_popup> is initial or <fs_popup> is not assigned.
      message i789(m7) display like 'E'.
      raise display_popup.
    endif.

    data: o_popup_alv type ref to cl_salv_table.
    data: lo_functions type ref to cl_salv_functions_list.
    data: t_t100 type standard table of t100.
    data: lit_functions_list type salv_t_ui_func,
          lwa_functions_list like line of lit_functions_list,
          lo_column          type ref to cl_salv_column,
          lo_colum           type ref to cl_salv_columns.


    try.
        cl_salv_table=>factory(
          importing
            r_salv_table   = o_popup_alv
          changing
            t_table        = <fs_popup> ).
      catch cx_salv_msg.

    endtry.
    lo_functions = o_popup_alv->get_functions( ).
    lo_functions->set_default( 'X' ).
    lo_colum = o_popup_alv->get_columns( ).
    lo_colum->set_optimize( abap_true ).
    lit_functions_list = lo_functions->get_functions( ).
    loop at lit_functions_list into lwa_functions_list.
      lwa_functions_list-r_function->set_visible( abap_false ).
    endloop.
* ALV as Popup
    o_popup_alv->set_screen_popup(
      start_column = 80
      end_column   = 140
      start_line   = 3
      end_line     = 10 ).

* Display
    o_popup_alv->display( ).

  endmethod.
endclass.
