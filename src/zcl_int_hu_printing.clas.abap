CLASS zcl_int_hu_printing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_split,
        line TYPE c LENGTH 100,
      END OF ts_split .
    TYPES:
      tt_split TYPE STANDARD TABLE OF ts_split WITH EMPTY KEY .

    METHODS constructor
      IMPORTING
        !iv_application_log TYPE balloghndl
        !iv_form_name       TYPE fpname
        !it_huhdr_int       TYPE /scwm/tt_huhdr_int
        !it_huitm_int       TYPE /scwm/tt_huitm_int
        !it_hutree          TYPE /scwm/tt_hutree
        !it_top_huhdr       TYPE /scwm/tt_huhdr_int
        !is_output_params   TYPE sfpoutputparams
        !is_doc_params      TYPE sfpdocparams
        !is_print           TYPE /scwm/s_print .
    METHODS process_hu_printig
      EXPORTING
        !et_huhdr         TYPE /scwm/tt_huhdr_int
        !et_hu_hazard_mat TYPE /scwm/t_hu_hazard_mat
        !et_hu_ser_label  TYPE /scwm/tt_ser_label
        !et_hu_shplabel   TYPE /scwm/tt_shplabel
        !et_hu_wt_info    TYPE /scwm/tt_wo_info
        !et_hu_print_tree TYPE /scwm/tt_hu_print_tree
        !et_hu_content    TYPE /scwm/tt_hu_material
        !ev_printed       TYPE xfeld .
  PROTECTED SECTION.
private section.

  data MV_APPLICATION_LOG type BALLOGHNDL .
  data MV_FORM_NAME type FPNAME .
  data MT_HUHDR_INT type /SCWM/TT_HUHDR_INT .
  data MT_HUITM_INT type /SCWM/TT_HUITM_INT .
  data MT_HUTREE type /SCWM/TT_HUTREE .
  data MT_TOP_HUHDR type /SCWM/TT_HUHDR_INT .
  data MS_OUTPUT_PARAMS type SFPOUTPUTPARAMS .
  data MS_DOC_PARAMS type SFPDOCPARAMS .
  data MS_PRINT type /SCWM/S_PRINT .
  data MT_HUHDR type /SCWM/TT_HUHDR_INT .
  data MT_HU_HAZARD_MAT type /SCWM/T_HU_HAZARD_MAT .
  data MT_HU_SER_LABEL type /SCWM/TT_SER_LABEL .
  data MT_HU_SHPLABEL type /SCWM/TT_SHPLABEL .
  data MT_HU_WT_INFO type /SCWM/TT_WO_INFO .
  data MT_HU_PRINT_TREE type /SCWM/TT_HU_PRINT_TREE .
  data MT_HU_CONTENT type /SCWM/TT_HU_MATERIAL .
  data MV_PRINTED type XFELD .
  data MO_DEL type ref to ZCL_INT_DELIVERY_PRINTING .

  methods PROCESS_ZHU_LABEL_DUMMY .
  methods PROCESS_ZHU_LABEL_PUTAWAY .
  methods PROCESS_ZHU_LABEL_AMAZON_SSCC .
  methods READ_USER_INFOS
    importing
      value(IV_USER) type SY-UNAME optional
    exporting
      !EV_TERMINAL_ID type UTID
      !EV_CLIENT type MANDT
      !EV_USER type UBNAME
      !EV_TCODE type UTCODE
      !EV_TERMINAL type UTERM
      !EV_TIME type UDTIME
      !EV_MASTER type UMASTER
      !EV_HOST_ADDRESS type MSHOSTADR
      !EV_TRACE type USER_TRACE
      !EV_EXTERNAL_MODE type UMODE
      !EV_INTERNAL_MODE type UMODE
      !EV_TYPE type INT4
      !EV_STATUS type INT4
      !EV_PROTOCOLL type INT4
      !EV_GUI_VERSION type CHAR10
      !EV_RFC_TYPE type CHAR1 .
  methods CONV_TIMESTAMP_DATE_TIME
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_TIMESTAMP type TZNTSTMPS
    exporting
      !EV_DATE type SY-DATUM
      !EV_TIME type SY-UZEIT .
  methods READ_HU_DATA
    importing
      !IV_HUIDENT type /SCWM/HUIDENT
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !EV_MIX type XFELD
      !ES_HUHDR type /SCWM/S_HUHDR_INT
      !ET_HUHDR type /SCWM/TT_HUHDR_INT
      !ET_IDENT type /SCWM/TT_IDENT_INT
      !ET_HUREF type /SCWM/TT_HUREF_INT
      !ET_HUTREE type /SCWM/TT_HUTREE
      !ET_HUITM type /SCWM/TT_HUITM_INT .
  methods READ_MAT_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MATID type /SCWM/DE_MATID
    exporting
      !ES_MAT_GLOBAL type /SCWM/S_MATERIAL_GLOBAL
      !ES_MAT_PACK type /SCWM/S_MATERIAL_PACK
      !ES_MAT_HAZARD type /SCWM/S_MATERIAL_HAZARD
      !ES_MAT_LGNUM type /SCWM/S_MATERIAL_LGNUM
      !ES_MAT_LGTYP type /SCWM/S_MATERIAL_LGTYP
      !ET_MAT_LGTYP type /SCWM/TT_MATERIAL_LGTYP
      !ET_MAT_UOM type /SCWM/TT_MATERIAL_UOM
      !ET_MAT_MEAN type /SCWM/TT_MAT_MEAN
      !EV_APPLIC_PROC type /SCWM/DE_BLOCAPPL_PROC .
  methods GET_ADRS_AG .
  methods GET_ADRNR_AG
    returning
      value(RV_ADRNR) type KNA1-ADRNR .
  methods GET_ADRS_DATA
    importing
      !IV_ADRNR type ADRC-ADDRNUMBER
    returning
      value(RT_ADRS_DATA) type TT_SPLIT .
ENDCLASS.



CLASS ZCL_INT_HU_PRINTING IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : AD-230322
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Constructor
*&
**********************************************************************
    mv_application_log = iv_application_log.
    mv_form_name = iv_form_name.
    mt_huhdr_int = it_huhdr_int.
    mt_huitm_int = it_huitm_int.
    mt_hutree = it_hutree.
    mt_top_huhdr = it_top_huhdr.
    ms_output_params = is_output_params.
    ms_doc_params = is_doc_params.
    ms_print = is_print.
  ENDMETHOD.


  METHOD conv_timestamp_date_time.
**********************************************************************
*& Key           : AD-230323
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Converts Timestamp into separate fields for date and time.
*&
**********************************************************************
    DATA: lt_date_time TYPE /scwm/tt_tstmp_date_time.

    CALL FUNCTION '/SCWM/CONVERT_TIMESTAMP'
      EXPORTING
        iv_lgnum     = iv_lgnum
        it_timestamp = VALUE /scwm/tt_timestamp( ( iv_timestamp ) )
      IMPORTING
        et_date_time = lt_date_time.

    IF sy-subrc = 0 AND line_exists( lt_date_time[ 1 ] ).
      ev_date = lt_date_time[ 1 ]-date.
      ev_time = lt_date_time[ 1 ]-time.
    ENDIF.
  ENDMETHOD.


  METHOD GET_ADRNR_AG.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get Adress no / AG
**********************************************************************
*    rv_adrnr = ms_header_details-stprt_addrno.
*
*    SELECT SINGLE adrnr FROM kna1
*      INTO rv_adrnr
*      WHERE kunnr = ms_data_spec-is_head-kunag.

  ENDMETHOD.


  METHOD GET_ADRS_AG.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data / AG
**********************************************************************
*    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.
*
*    TYPES: BEGIN OF ts_split,
*             line TYPE c LENGTH 50,
*           END OF ts_split.
*
*    DATA: lt_adrs_line TYPE tt_split,
*          lt_split     TYPE STANDARD TABLE OF ts_split,
*          lv_fieldname TYPE fieldname,
*          lv_line      TYPE n LENGTH 1.
*
*    FIELD-SYMBOLS: <lv_value> TYPE data.
*
*    IF ms_header_details-stprt_addrno <> get_adrnr_ag( ).
*      lt_adrs_line = get_adrs_data( get_adrnr_ag( ) ).
*
*      APPEND INITIAL LINE TO ms_data_spec-it_adrs ASSIGNING FIELD-SYMBOL(<ls_adrs>).
*      LOOP AT lt_adrs_line FROM 1 TO 7 ASSIGNING FIELD-SYMBOL(<ls_adrs_line>).
*        <ls_adrs>-addresslines = VALUE #( BASE <ls_adrs>-addresslines ( line = CONV #( <ls_adrs_line> ) ) ).
*      ENDLOOP.
*      ms_data_spec-it_adrs_left  = ms_data_spec-it_adrs.
*      ms_data_spec-it_adrs_right = ms_data_spec-it_adrs.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_ADRS_DATA.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data
**********************************************************************
*
*    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.
*
*    DATA ls_adrs_data TYPE adrs_print.
*
*    SELECT SINGLE
*       tsad3t~title_medi AS line0,  " Salutation
*       @cv_separator && adrc~name1 AS line1,
*       @cv_separator && adrc~name2 AS line2,
*       @cv_separator && adrc~name3 AS line3,
*       @cv_separator && adrc~name4 AS line4,
*       @cv_separator && adrc~street && ' ' &&  adrc~house_num1 AS line5,
*       @cv_separator && adrc~post_code1 && ' ' &&  adrc~city1 AS line6,
*       @cv_separator && adrc~country AS line7
*    FROM adrc
*    LEFT OUTER JOIN tsad3t ON tsad3t~title = adrc~title AND tsad3t~langu = @sy-langu
*    WHERE addrnumber = @iv_adrnr
*    INTO @ls_adrs_data.
*
*    SPLIT ls_adrs_data AT cv_separator INTO TABLE rt_adrs_data.
*    DELETE rt_adrs_data WHERE line IS INITIAL.

  ENDMETHOD.


  METHOD process_hu_printig.
**********************************************************************
*& Key           : AD-230322
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Processing the custom label HU labels
*&
**********************************************************************
    BREAK-POINT ID zcg_hu_printing.

    CASE mv_form_name.
      WHEN 'ZHU_LABEL_DUMMY' OR 'ZHU_LABEL_DUMMY_ADMIN'.
        process_zhu_label_dummy( ).
      WHEN 'ZHU_LABEL_PUTAWAY'.
        process_zhu_label_putaway(  ).
      WHEN 'ZHU_LABEL_AMAZON_SSCC'.
        process_zhu_label_amazon_sscc(  ).
    ENDCASE.

    et_huhdr =  mt_huhdr.
    et_hu_hazard_mat =  mt_hu_hazard_mat.
    et_hu_ser_label =  mt_hu_ser_label.
    et_hu_shplabel = mt_hu_shplabel.
    et_hu_wt_info = mt_hu_wt_info.
    et_hu_print_tree = mt_hu_print_tree.
    et_hu_content = mt_hu_content.
    ev_printed = mv_printed.
  ENDMETHOD.


  METHOD process_zhu_label_amazon_sscc.
**********************************************************************
*& Key           : AD-240118
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Processing the custom label ZHU_LABEL_AMAZON_SSCC
*&
**********************************************************************

    " Tabellen in denen Daten enthalten
    " MT_HUHDR_INT
    " MT_HUITM_INT
    " MT_TOP_HUHDR

    " Collect data for printing Dummy HU label.
    DATA lv_dest_storage_type TYPE /scwm/de_lgtyp.
    DATA lv_dest_storage_bin TYPE /scwm/de_lgpla .
    DATA lv_terminal TYPE uterm.
    DATA lv_date TYPE sy-datum.
    DATA lv_time TYPE sy-uzeit.

    DATA lv_material_id TYPE /scwm/de_matnr.
    DATA lv_material_txt TYPE c LENGTH 40.
    DATA lv_manuf_part_number TYPE mfrpn.
    DATA lv_gtin TYPE ean11.
    DATA lv_offset TYPE i.
    DATA lv_batch TYPE charg_d.
    DATA lt_warehouse_tasks TYPE  /scwm/tt_to_det_mon.

    DATA :lv_var_1 TYPE /scwm/s_huhdr_int-zz_var_01,
          lv_var_2 TYPE /scwm/s_huhdr_int-zz_var_02,
          lv_var_3 TYPE /scwm/s_huhdr_int-zz_var_03.

    LOOP AT mt_huhdr_int ASSIGNING FIELD-SYMBOL(<hu_header>).
      CLEAR: lv_terminal, lv_date, lv_time, lv_dest_storage_type, lv_dest_storage_bin.

      " User Terminal
      read_user_infos( EXPORTING iv_user     = sy-uname "<hu_header>-created_by
                       IMPORTING ev_terminal = lv_terminal ).

      " Date and Time
      conv_timestamp_date_time( EXPORTING iv_lgnum = <hu_header>-lgnum
                                          iv_timestamp = <hu_header>-created_at
                                IMPORTING ev_date = lv_date
                                          ev_time = lv_time )."

      " Destination storage bin and type
      CALL FUNCTION '/SCWM/TO_GET_WIP'
        EXPORTING
          iv_lgnum   = <hu_header>-lgnum
          iv_open    = abap_true " Only select open Warehouse tasks
          iv_srcdata = abap_true " The provided values for LGTYP, LGPLA, LENR are source data
          is_selcrit = VALUE /scwm/s_to_selcrit_mon( r_lenr = VALUE rseloption( ( sign = 'I' option = 'EQ' low = <hu_header>-huident ) ) )
        IMPORTING
          et_to      = lt_warehouse_tasks.

      IF lines( lt_warehouse_tasks ) = 1.
        lv_dest_storage_type = lt_warehouse_tasks[ 1 ]-nltyp.
        lv_dest_storage_bin  = lt_warehouse_tasks[ 1 ]-nlpla.
        lv_batch             = lt_warehouse_tasks[ 1 ]-charg.
      ENDIF.

      IF line_exists( mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ] ).
        CLEAR: lv_material_id, lv_material_txt, lv_manuf_part_number, lv_gtin, lv_batch.

        " Read additional material data
        read_mat_data( EXPORTING iv_lgnum = <hu_header>-lgnum
                                 iv_matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid
                       IMPORTING es_mat_global = DATA(ls_mat_global)
                                 et_mat_uom = DATA(lt_mat_uom) ).

        " Material number and -text
        lv_material_id = ls_mat_global-matnr.
        lv_material_txt = ls_mat_global-maktx.

        " Manufacturer part number
        " There is acutaly no other standard Function Module or class to read this information.
        " Therefore, we have to read directly from table MARA.
        SELECT SINGLE mfrpn
          INTO @lv_manuf_part_number
          FROM mara
         WHERE matnr = @lv_material_id.

*        lv_var_1 = COND #( WHEN lv_batch IS INITIAL THEN |{ mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled }|
*                                                    ELSE |{ lv_batch } { mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled }| ).

        " GTIN
        IF line_exists( lt_mat_uom[ matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid  meinh = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-meins ] ).
          lv_gtin = lt_mat_uom[ matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid meinh = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-meins ]-gtin.
          SHIFT lv_gtin LEFT DELETING LEADING space.
          SHIFT lv_gtin LEFT DELETING LEADING '0'.
          IF strlen( lv_gtin ) >= 4.
            lv_offset = strlen( lv_gtin ) - 4.
            lv_var_2 = lv_gtin(lv_offset).
            lv_var_3 = lv_gtin+lv_offset(4).
            CLEAR lv_gtin+lv_offset(4).
          ENDIF.
        ENDIF.

        " Pass data to export parameter.
        mt_huhdr = VALUE /scwm/tt_huhdr_int( BASE mt_huhdr
                                           ( huident = <hu_header>-huident
                                             lgtyp = COND #( WHEN lv_dest_storage_type IS INITIAL THEN '#TYPE#' ELSE lv_dest_storage_type )
                                             lgpla = COND #( WHEN lv_dest_storage_bin IS INITIAL THEN '#STORAGE_BIN#' ELSE lv_dest_storage_bin )
                                             s_huatt-zz_terminal = COND #( WHEN lv_terminal IS INITIAL THEN '#TERMINAL#' ELSE lv_terminal )
                                             created_by = sy-uname
                                             s_huatt-zz_date = lv_date "|{ lv_date DATE = USER }|
                                             s_huatt-zz_time = lv_time "|{ lv_time TIME = USER }|
                                             s_huatt-zz_mpart_no = COND #( WHEN lv_manuf_part_number IS INITIAL THEN '#ZZ_MPART_NO#' ELSE lv_manuf_part_number )
                                             zz_var_01 = COND #( WHEN <hu_header>-g_weight < 15 THEN abap_on )
                                             zz_var_02 = abap_off
                                             zz_var_03 = lv_var_3
                                            ) ).

        " Batch number (Batch number is out of scope for now. So there might be a task in the future.)

        " Pass data to export parameter.
        mt_hu_content = VALUE /scwm/tt_hu_material( BASE mt_hu_content
                                                  (
                                                    guid_parent = <hu_header>-guid_hu
                                                    matnr = lv_material_id
                                                    maktx = lv_material_txt
                                                    entitled = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled
                                                    charg = lv_batch
                                                    quan = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-quan
                                                  ) ).

        " Pass data to export parameter.
        mt_hu_wt_info = VALUE /scwm/tt_wo_info( BASE mt_hu_wt_info
                                              (
                                                matnr = lv_material_id
                                                ean_gtin = lv_gtin
                                              ) ).
      ENDIF.

    ENDLOOP.

*    z_label_1 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'MATERIALNUMMER' iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_2 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'MENGE'          iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_3 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LAGERPLATZ'     iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_4 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LAGERTYP'       iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_5 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'HERSTELLERMAT'  iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_6 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'EAN_UPC_CODE'   iv_langu = sy-langu iv_form_name = mv_form_name ).

*    " The default will handle the actual printing
    mv_printed = abap_false.

  ENDMETHOD.


  METHOD process_zhu_label_dummy.
**********************************************************************
*& Key           : AD-230322
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Processing the custom label ZHU_LABEL_DUMMY
*&
**********************************************************************
    DATA lt_alternative_huident TYPE /scwm/tt_ident_int .
    DATA lv_alt_ident_i TYPE /scwm/de_ident.
    DATA lv_alt_ident_r TYPE /scwm/de_ident.
    DATA lv_terminal TYPE uterm.
    DATA lv_date TYPE sy-datum.
    DATA lv_time TYPE sy-uzeit.

    BREAK-POINT ID zcg_inb_hu_dummy_label.

    " Collect data for printing Dummy HU label.
    LOOP AT mt_huhdr_int ASSIGNING FIELD-SYMBOL(<hu_header>).
      CLEAR: lv_alt_ident_i, lv_alt_ident_r, lv_terminal, lv_date, lv_time.
      REFRESH: lt_alternative_huident.

      "------------------------------
      " Read alternative HU Identifier
      "------------------------------
      read_hu_data( EXPORTING
                       iv_lgnum = <hu_header>-lgnum
                       iv_huident = <hu_header>-huident
                    IMPORTING
                       et_ident = lt_alternative_huident ).

      IF line_exists( lt_alternative_huident[ idart = 'I' ] ).
        lv_alt_ident_i = lt_alternative_huident[ idart = 'I' ]-huident.
      ENDIF.
      IF line_exists( lt_alternative_huident[ idart = 'R' ] ).
        lv_alt_ident_r = lt_alternative_huident[ idart = 'R' ]-huident.
      ENDIF.

      "------------------------------
      " Read User Info
      "------------------------------
      read_user_infos( IMPORTING
                          ev_terminal = lv_terminal ).

      conv_timestamp_date_time( EXPORTING
                                   iv_lgnum = <hu_header>-lgnum
                                   iv_timestamp = <hu_header>-created_at
                                IMPORTING
                                   ev_date = lv_date
                                   ev_time = lv_time ).

      " Pass data to export parameter.
      mt_huhdr = VALUE /scwm/tt_huhdr_int( BASE mt_huhdr
                                         ( huident = <hu_header>-huident
                                           lgtyp = <hu_header>-lgtyp
                                           lgpla = <hu_header>-lgpla
                                           s_huatt-zz_ident_r = lv_alt_ident_r
                                           s_huatt-zz_ident_i = lv_alt_ident_i
                                           s_huatt-zz_terminal = COND #( WHEN lv_terminal IS INITIAL THEN '#TERMINAL#' ELSE lv_terminal )
                                           created_by = <hu_header>-created_by
                                           s_huatt-zz_date = lv_date
                                           s_huatt-zz_time = lv_time
                                          ) ).
    ENDLOOP.

*    " The default will handle the actual printing
    mv_printed = abap_false.

  ENDMETHOD.


  METHOD process_zhu_label_putaway.
**********************************************************************
*& Key           : AD-230322
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Processing the custom label ZHU_LABEL_PUTAWAY
*&
**********************************************************************
    " Collect data for printing Dummy HU label.
    DATA lv_dest_storage_type TYPE /scwm/de_lgtyp.
    DATA lv_dest_storage_bin TYPE /scwm/de_lgpla .
    DATA lv_terminal TYPE uterm.
    DATA lv_date TYPE sy-datum.
    DATA lv_time TYPE sy-uzeit.

    DATA lv_material_id TYPE /scwm/de_matnr.
    DATA lv_material_txt TYPE c LENGTH 40.
    DATA lv_manuf_part_number TYPE mfrpn.
    DATA lv_gtin TYPE ean11.
    DATA lv_offset TYPE i.
    DATA lv_batch TYPE charg_d.
    DATA lt_warehouse_tasks TYPE  /scwm/tt_to_det_mon.

    DATA :lv_var_1 TYPE /scwm/s_huhdr_int-zz_var_01,
          lv_var_2 TYPE /scwm/s_huhdr_int-zz_var_02,
          lv_var_3 TYPE /scwm/s_huhdr_int-zz_var_03.

    LOOP AT mt_huhdr_int ASSIGNING FIELD-SYMBOL(<hu_header>).
      CLEAR: lv_terminal, lv_date, lv_time, lv_dest_storage_type, lv_dest_storage_bin.

      " User Terminal
      read_user_infos( EXPORTING iv_user     = sy-uname "<hu_header>-created_by
                       IMPORTING ev_terminal = lv_terminal ).

      " Date and Time
      conv_timestamp_date_time( EXPORTING
                                   iv_lgnum = <hu_header>-lgnum
                                   iv_timestamp = <hu_header>-created_at
                                IMPORTING
                                   ev_date = lv_date
                                   ev_time = lv_time )."

      " Destination storage bin and type
      CALL FUNCTION '/SCWM/TO_GET_WIP'
        EXPORTING
          iv_lgnum   = <hu_header>-lgnum
          iv_open    = abap_true " Only select open Warehouse tasks
          iv_srcdata = abap_true " The provided values for LGTYP, LGPLA, LENR are source data
          is_selcrit = VALUE /scwm/s_to_selcrit_mon( r_lenr = VALUE rseloption( ( sign = 'I' option = 'EQ' low = <hu_header>-huident ) )
                                                   )
        IMPORTING
          et_to      = lt_warehouse_tasks.

      IF lines( lt_warehouse_tasks ) = 1.
        lv_dest_storage_type = lt_warehouse_tasks[ 1 ]-nltyp.
        lv_dest_storage_bin  = lt_warehouse_tasks[ 1 ]-nlpla.
        lv_batch             = lt_warehouse_tasks[ 1 ]-charg.
      ENDIF.

      IF line_exists( mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ] ).
        CLEAR: lv_material_id, lv_material_txt, lv_manuf_part_number, lv_gtin, lv_batch.

        " Read additional material data
        read_mat_data( EXPORTING
                          iv_lgnum = <hu_header>-lgnum
                          iv_matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid
                       IMPORTING
                          es_mat_global = DATA(ls_mat_global)
                          et_mat_uom = DATA(lt_mat_uom) ).

        " Material number and -text
        lv_material_id = ls_mat_global-matnr.
        lv_material_txt = ls_mat_global-maktx.

        " Manufacturer part number
        " There is acutaly no other standard Function Module or class to read this information.
        " Therefore, we have to read directly from table MARA.
        SELECT SINGLE mfrpn
          INTO @lv_manuf_part_number
          FROM mara
         WHERE matnr = @lv_material_id.

        lv_var_1 = COND #( WHEN lv_batch IS INITIAL THEN |{ mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled }|
                                                    ELSE |{ lv_batch } { mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled }| ).

        " GTIN
        IF line_exists( lt_mat_uom[ matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid  meinh = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-meins ] ).
          lv_gtin = lt_mat_uom[ matid = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-matid meinh = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-meins ]-gtin.
          SHIFT lv_gtin LEFT DELETING LEADING space.
          SHIFT lv_gtin LEFT DELETING LEADING '0'.
          IF strlen( lv_gtin ) >= 4.
            lv_offset = strlen( lv_gtin ) - 4.
            lv_var_2 = lv_gtin(lv_offset).
            lv_var_3 = lv_gtin+lv_offset(4).
            CLEAR lv_gtin+lv_offset(4).
          ENDIF.
        ENDIF.

        " Pass data to export parameter.
        mt_huhdr = VALUE /scwm/tt_huhdr_int( BASE mt_huhdr
                                           ( huident = <hu_header>-huident
                                             lgtyp = COND #( WHEN lv_dest_storage_type IS INITIAL THEN '#TYPE#' ELSE lv_dest_storage_type )
                                             lgpla = COND #( WHEN lv_dest_storage_bin IS INITIAL THEN '#STORAGE_BIN#' ELSE lv_dest_storage_bin )
                                             s_huatt-zz_terminal = COND #( WHEN lv_terminal IS INITIAL THEN '#TERMINAL#' ELSE lv_terminal )
                                             created_by = sy-uname "<hu_header>-created_by
                                             s_huatt-zz_date = lv_date
                                             s_huatt-zz_time = lv_time
                                             s_huatt-zz_mpart_no = COND #( WHEN lv_manuf_part_number IS INITIAL THEN '#ZZ_MPART_NO#' ELSE lv_manuf_part_number )
                                             zz_var_01 = lv_var_1
                                             zz_var_02 = lv_var_2
                                             zz_var_03 = lv_var_3
                                            ) ).

        " Batch number (Batch number is out of scope for now. So there might be a task in the future.)

        " Pass data to export parameter.
        mt_hu_content = VALUE /scwm/tt_hu_material( BASE mt_hu_content
                                                  (
                                                    guid_parent = <hu_header>-guid_hu
                                                    matnr = lv_material_id
                                                    maktx = lv_material_txt
                                                    entitled = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-entitled
                                                    charg = lv_batch
                                                    quan = mt_huitm_int[ guid_parent = <hu_header>-guid_hu  ]-quan
                                                  ) ).

        " Pass data to export parameter.
        mt_hu_wt_info = VALUE /scwm/tt_wo_info( BASE mt_hu_wt_info
                                              (
                                                matnr = lv_material_id
                                                ean_gtin = lv_gtin
                                              ) ).
      ENDIF.

    ENDLOOP.

*    z_label_1 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'MATERIALNUMMER' iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_2 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'MENGE'          iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_3 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LAGERPLATZ'     iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_4 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LAGERTYP'       iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_5 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'HERSTELLERMAT'  iv_langu = sy-langu iv_form_name = mv_form_name ).
*    z_label_6 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'EAN_UPC_CODE'   iv_langu = sy-langu iv_form_name = mv_form_name ).

*    " The default will handle the actual printing
    mv_printed = abap_false.

  ENDMETHOD.


  METHOD read_hu_data.
**********************************************************************
*& Key           : AD-230323
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads HU Data
*&
**********************************************************************
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme
        iv_lgnum   = iv_lgnum
        iv_huident = iv_huident
      IMPORTING
        ev_mix     = ev_mix
        es_huhdr   = es_huhdr
        et_huhdr   = et_huhdr
        et_ident   = et_ident
        et_huref   = et_huref
        et_hutree  = et_hutree
        et_huitm   = et_huitm
      EXCEPTIONS
        deleted    = 1
        not_found  = 2
        error      = 3
        OTHERS     = 4.
  ENDMETHOD.


  METHOD read_mat_data.
**********************************************************************
*& Key           : AD-230324
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Reads Material Data
*&
**********************************************************************
    DATA ls_mat_global TYPE /scwm/s_material_global .
    DATA lt_mat_uom TYPE /scwm/tt_material_uom.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_lgnum      = iv_lgnum
            iv_matid      = iv_matid
          IMPORTING
            es_mat_global = ls_mat_global
            "es_mat_pack    = es_mat_pack
            "es_mat_hazard  = es_mat_hazard
            "es_mat_lgnum  = es_mat_lgnum
            "es_mat_lgtyp   = es_mat_lgtyp
            "et_mat_lgtyp   = et_mat_lgtyp
            et_mat_uom    = lt_mat_uom
            "et_mat_mean    = et_mat_mean
            "ev_applic_proc = ev_applic_proc
          .

        es_mat_global = ls_mat_global.
        et_mat_uom = lt_mat_uom.

      CATCH /scwm/cx_md INTO DATA(lx_ex).
    ENDTRY.
  ENDMETHOD.


  METHOD read_user_infos.
**********************************************************************
*& Key           : AD-230322
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determine User Info
*&
**********************************************************************
    DATA lt_user_info TYPE TABLE OF uinfo.
    IF iv_user IS NOT SUPPLIED.
      iv_user = sy-uname.
    ENDIF.

    CALL FUNCTION 'THUSRINFO' TABLES usr_tabl = lt_user_info.

    DELETE lt_user_info WHERE bname <> iv_user.
    DELETE lt_user_info WHERE rfc_type IS NOT INITIAL.

    ev_terminal_id    = VALUE #(  lt_user_info[ 1 ]-tid OPTIONAL ).
    ev_client         = VALUE #(  lt_user_info[ 1 ]-mandt OPTIONAL ).
    ev_user           = VALUE #(  lt_user_info[ 1 ]-bname OPTIONAL ).
    ev_tcode          = VALUE #(  lt_user_info[ 1 ]-tcode OPTIONAL ).
    ev_terminal       = VALUE #(  lt_user_info[ 1 ]-term OPTIONAL ).
    ev_time           = VALUE #(  lt_user_info[ 1 ]-zeit OPTIONAL ).
    ev_master         = VALUE #(  lt_user_info[ 1 ]-master OPTIONAL ).
    ev_host_address   = VALUE #(  lt_user_info[ 1 ]-hostadr OPTIONAL ).
    ev_trace          = VALUE #(  lt_user_info[ 1 ]-trace OPTIONAL ).
    ev_external_mode  = VALUE #(  lt_user_info[ 1 ]-extmodi OPTIONAL ).
    !ev_internal_mode = VALUE #(  lt_user_info[ 1 ]-intmodi OPTIONAL ).
    !ev_type          = VALUE #(  lt_user_info[ 1 ]-type OPTIONAL ).
    !ev_status        = VALUE #(  lt_user_info[ 1 ]-stat OPTIONAL ).
    !ev_protocoll     = VALUE #(  lt_user_info[ 1 ]-protocol OPTIONAL ).
    !ev_gui_version   = VALUE #(  lt_user_info[ 1 ]-guiversion OPTIONAL ).
    !ev_rfc_type      = VALUE #(  lt_user_info[ 1 ]-rfc_type OPTIONAL ).

  ENDMETHOD.
ENDCLASS.
