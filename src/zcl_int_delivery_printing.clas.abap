CLASS zcl_int_delivery_printing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ts_org_data TYPE zaf_s_org_data_values .
    TYPES:
      BEGIN OF ts_adrs_labels,
        name1 TYPE zaf_transl_value,
        name2 TYPE zaf_transl_value,
        name3 TYPE zaf_transl_value,
        name4 TYPE zaf_transl_value,
      END OF ts_adrs_labels .
    TYPES:
      BEGIN OF ty_signatureblock,
        row1 TYPE zaf_transl_value,
        row2 TYPE zaf_transl_value,
        row3 TYPE zaf_transl_value,
        row4 TYPE zaf_transl_value,
      END OF ty_signatureblock .
    TYPES:
      tt_signatureblock TYPE STANDARD TABLE OF ty_signatureblock WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ts_ccode_data_head,
        ccode_row1 TYPE zaf_transl_value,
        ccode_row2 TYPE zaf_transl_value,
        ccode_row3 TYPE zaf_transl_value,
      END OF ts_ccode_data_head .
    TYPES:
      BEGIN OF ts_ccode_data_total,
        total_label TYPE zaf_transl_value,
        brgew       TYPE char30, " vbdpl-brgew
        ntgew       TYPE char30, " vbdpl-ntgew
        all_qty     TYPE zaf_transl_value,
      END OF ts_ccode_data_total .
    TYPES:
      BEGIN OF ts_ccode_data,
        stawn TYPE vbdpl-stawn,
        brgew TYPE char30, " vbdpl-brgew
        ntgew TYPE char30, " vbdpl-ntgew
      END OF ts_ccode_data .
    TYPES:
      tt_ccodes_data TYPE STANDARD TABLE OF ts_ccode_data WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ts_ccode,
        vbeln TYPE vbdpl-vbeln,
        stawn TYPE vbdpl-stawn,
        brgew TYPE vbdpl-brgew,
        ntgew TYPE vbdpl-ntgew,
        gewei TYPE vbdpl-gewei,
      END OF ts_ccode .
    TYPES:
      tts_ccodes TYPE STANDARD TABLE OF ts_ccode WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ts_after_all_pos_vse,
        vsenr1 TYPE vhilm_ku,
        vsenr2 TYPE vhilm_ku,
        vsenr3 TYPE vhilm_ku,
        vsenr4 TYPE vhilm_ku,
      END OF ts_after_all_pos_vse .
    TYPES:
      BEGIN OF ts_after_pos,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos .
    TYPES:
      BEGIN OF ts_after_pos_barcode,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_barcode .
    TYPES:
      BEGIN OF ts_after_pos_ident,
        posnr     TYPE posnr,
        selnum    TYPE c LENGTH 3, "zcs_selnum,
        sellabel  TYPE string,
        clip      TYPE char01,
        str_ident TYPE string,
      END OF ts_after_pos_ident .
    TYPES:
      BEGIN OF ts_after_pos_ident2,
        posnr      TYPE posnr,
        str_ident1 TYPE string,
        str_ident2 TYPE string,
      END OF ts_after_pos_ident2 .
    TYPES:
      BEGIN OF ts_after_pos_ident_barcode,
        posnr     TYPE posnr,
        selnum    TYPE c LENGTH 3, "zcs_selnum,
        str_ident TYPE string,
      END OF ts_after_pos_ident_barcode .
    TYPES:
      BEGIN OF ts_after_pos_ident_pal,
        posnr   TYPE posnr,
        zlabel  TYPE zaf_transl_value,
        zvalue  TYPE iacxmlpropertyvalue,
        zvalue2 TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_ident_pal .
    TYPES:
      BEGIN OF ts_after_pos_retourentext,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        ztext1 TYPE text30,
        ztext2 TYPE text40,
      END OF ts_after_pos_retourentext .
    TYPES:
      BEGIN OF ts_after_pos_sernr,
        posnr     TYPE posnr,
        zlabel    TYPE zaf_transl_value,
        str_sernr TYPE string,
      END OF ts_after_pos_sernr .
    TYPES:
      BEGIN OF ts_after_pos_pick_p,
        posnr       TYPE posnr,
        tanum_label TYPE zaf_transl_value,
        tanum       TYPE ltap-tanum,
        tapos_label TYPE zaf_transl_value,
        tapos       TYPE ltap-tapos,
        vltyp_label TYPE zaf_transl_value,
        vltyp       TYPE ltap-vltyp,
        vlpla_label TYPE zaf_transl_value,
        vlpla       TYPE ltap-vlpla,
        pickq_label TYPE zaf_transl_value,
        pickq       TYPE char13,
      END OF ts_after_pos_pick_p .
    TYPES:
      BEGIN OF ts_after_pos_sn_text,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_after_pos_sn_text .
    TYPES:
      BEGIN OF ts_before_pos,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
        zvalue TYPE iacxmlpropertyvalue,
      END OF ts_before_pos .
    TYPES:
      BEGIN OF ts_bundle_text,
        vbeln  TYPE vbeln,
        posnr  TYPE posnr,
        zlabel TYPE zaf_transl_value,
      END OF ts_bundle_text .
    TYPES:
      BEGIN OF ts_head,
        vgbel              TYPE vbdkl-vgbel,
        sland              TYPE vbdkl-sland,
        adrnr_ag           TYPE vbdkl-adrnr_ag,
        adrnr_we           TYPE vbdkl-adrnr_we,
        vbeln              TYPE vbdkl-vbeln,
        erdat              TYPE vbdkl-erdat,
        kunag              TYPE vbdkl-kunag,
        nachnahme_label    TYPE zaf_transl_value,
        kunden_ref_label   TYPE zaf_transl_value,
        kunden_ref_barcode TYPE vbkd-bstkd,
        delivery_weight    TYPE zaf_transl_value,
        btgew              TYPE char30 , "vbdkl-btgew,
        unit               TYPE zaf_transl_value,
        decl_of_goods      TYPE zaf_transl_value,
        goods_discription  TYPE zaf_transl_value,
        iln_label          TYPE zaf_transl_value,
        iln                TYPE char13,
      END OF ts_head .
    TYPES:
      BEGIN OF ts_info_data,
        label1  TYPE char40,
        value1  TYPE char40,
        label2  TYPE char40,
        value2  TYPE char40,
        label3  TYPE char40,
        value3  TYPE char40,
        label4  TYPE char40,
        value4  TYPE char40,
        label5  TYPE char40,
        value5  TYPE char40,
        label6  TYPE char40,
        value6  TYPE char40,
        label7  TYPE char40,
        value7  TYPE char40,
        label8  TYPE char40,
        value8  TYPE char40,
        label9  TYPE char40,
        value9  TYPE char40,
        label10 TYPE char40,
        value10 TYPE char40,
        label11 TYPE char40,
        value11 TYPE char40,
      END OF ts_info_data .
    TYPES:
      BEGIN OF ts_key_params.
        INCLUDE TYPE stxh_key.
        INCLUDE TYPE zaf_s_text_params.
    TYPES: END OF ts_key_params .
    TYPES:
      BEGIN OF ts_layout_posnr,
        vbeln TYPE vbeln,
        posnr TYPE posnr,
        uepos TYPE uepos,
        unpos TYPE posnr,
      END OF ts_layout_posnr .
    TYPES:
      BEGIN OF ts_log_fields,
        system_id        TYPE sysysid,
        tcode            TYPE sytcode,
        form_name        TYPE fpwbformname,
        zstceg_versender TYPE stceg,
        kunag            TYPE kunag,
        zstceg_kunde     TYPE stceg,
        ansprechpartner  TYPE char70,
        ztelnr           TYPE telfx,
        ztelfx           TYPE telfx,
        smtp_empf        TYPE ad_smtpadr,
        versandstelle    TYPE bezei30,
        vbeln_vauf_label TYPE text30,
        vbeln_vauf       TYPE vbdkl-vgbel,
        kunden_ref_label TYPE text30,
        kunden_ref       TYPE vbkd-bstkd,
      END OF ts_log_fields .
    TYPES:
      BEGIN OF ts_pos,
        posnr        TYPE char6,
        h_pos        TYPE char6,
        u_pos        TYPE char6,
        matnr        TYPE vbdpl-matnr,
        arktx        TYPE vbdpl-arktx,
        lfimg        TYPE char13,
        vrkme        TYPE vbdpl-vrkme,
        bundle_label TYPE zaf_transl_value,
        stawn_label  TYPE zaf_transl_value,
        stawn        TYPE vbdpl-stawn,
        brgew_label  TYPE zaf_transl_value,
        brgew        TYPE char30, " vbdpl-brgew,
        ntgew_label  TYPE zaf_transl_value,
        ntgew        TYPE char30, " vbdpl-ntgew,
      END OF ts_pos .
    TYPES:
      BEGIN OF ts_translation,
        title_vdelnote          TYPE zaf_transl_value,
        title_vdelnote_mail     TYPE zaf_transl_value,
        title_versandbest       TYPE zaf_transl_value,
        lieferanschrift         TYPE zaf_transl_value,
        datum                   TYPE zaf_transl_value,
        lieferscheinnr          TYPE zaf_transl_value,
        ust1nr                  TYPE zaf_transl_value,
        kundennr                TYPE zaf_transl_value,
        ust2nr                  TYPE zaf_transl_value,
        ansprechpartner         TYPE zaf_transl_value,
        telefon                 TYPE zaf_transl_value,
        fax                     TYPE zaf_transl_value,
        versandstelle           TYPE zaf_transl_value,
        seite                   TYPE zaf_transl_value,
        kundenbestellnr         TYPE zaf_transl_value,
        auftragnr               TYPE zaf_transl_value,
        pos                     TYPE zaf_transl_value,
        artikelnummer1          TYPE zaf_transl_value,
        bezeichnung1            TYPE zaf_transl_value,
        menge1                  TYPE zaf_transl_value,
        herstellermat           TYPE zaf_transl_value,
        ean_upc_code            TYPE zaf_transl_value,
        serialnummer            TYPE zaf_transl_value,
        rma_nr                  TYPE zaf_transl_value,
        warranty_code           TYPE zaf_transl_value,
        serialnr                TYPE zaf_transl_value,
        versandelement          TYPE zaf_transl_value,
        vsebezeichnung1         TYPE zaf_transl_value,
        vsebezeichnung2         TYPE zaf_transl_value,
        kundenmat               TYPE zaf_transl_value,
        sn_text                 TYPE zaf_transl_value,
        set_positions_text      TYPE zaf_transl_value,
        bewertungsart           TYPE zaf_transl_value,
        stueck_kurz             TYPE zaf_transl_value,
        email                   TYPE zaf_transl_value,
        qm_nummer               TYPE zaf_transl_value,
        nachnahme               TYPE zaf_transl_value,
        statitische_warennummer TYPE zaf_transl_value,
        nettogewicht            TYPE zaf_transl_value,
        bruttogewicht           TYPE zaf_transl_value,
        gesamt_total            TYPE zaf_transl_value,
        alle_gewichte_in_kg     TYPE zaf_transl_value,
        ta_nr                   TYPE zaf_transl_value,
        ta_pos                  TYPE zaf_transl_value,
        lagertyp                TYPE zaf_transl_value,
        lagerplatz              TYPE zaf_transl_value,
        menge                   TYPE zaf_transl_value,
        absender                TYPE zaf_transl_value,
        unterschrift            TYPE zaf_transl_value,
        nachname                TYPE zaf_transl_value,
        fahrer                  TYPE zaf_transl_value,
        lkw_typ_nummernschild   TYPE zaf_transl_value,
        empfaenger              TYPE zaf_transl_value,
        absender_beladung       TYPE zaf_transl_value,
        spediteur               TYPE zaf_transl_value,
        warenempfaenger         TYPE zaf_transl_value,
        warenuebergabe          TYPE zaf_transl_value,
        absender_uebergabeplatz TYPE zaf_transl_value,
        rechnungsadresse        TYPE zaf_transl_value,
        liefergewicht           TYPE zaf_transl_value,
        kg                      TYPE zaf_transl_value,
        warenbeschreibung       TYPE zaf_transl_value,
        warenbeschr_text        TYPE zaf_transl_value,
        iln                     TYPE zaf_transl_value,
        kundenreferenz          TYPE zaf_transl_value,
      END OF ts_translation .
    TYPES:
      BEGIN OF ts_vbeln_vauf_bstkd,
        posnr      TYPE posnr,
        vbeln_vauf TYPE vbeln_vauf,
        bstkd      TYPE bstkd,

      END OF ts_vbeln_vauf_bstkd .
    TYPES:
      tt_after_all_pos_vse       TYPE STANDARD TABLE OF ts_after_all_pos_vse WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos               TYPE STANDARD TABLE OF ts_after_pos WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_barcode       TYPE STANDARD TABLE OF ts_after_pos_barcode WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_ident         TYPE STANDARD TABLE OF ts_after_pos_ident WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_ident2        TYPE STANDARD TABLE OF ts_after_pos_ident2 WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_ident_barcode TYPE STANDARD TABLE OF ts_after_pos_ident_barcode WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_ident_pal     TYPE STANDARD TABLE OF ts_after_pos_ident_pal WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_retourentexte TYPE STANDARD TABLE OF ts_after_pos_retourentext WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_sernr         TYPE STANDARD TABLE OF ts_after_pos_sernr WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_pick_p         TYPE STANDARD TABLE OF ts_after_pos_pick_p WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_after_pos_sn_texte      TYPE STANDARD TABLE OF ts_after_pos_sn_text WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_before_pos              TYPE STANDARD TABLE OF ts_before_pos WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_bundle_text             TYPE STANDARD TABLE OF ts_bundle_text WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_key_params              TYPE STANDARD TABLE OF ts_key_params .
    TYPES:
      tt_layout_posnr            TYPE STANDARD TABLE OF ts_layout_posnr WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      tt_pos                     TYPE STANDARD TABLE OF ts_pos WITH DEFAULT KEY .
    TYPES:
      tt_vbeln_vauf_bstkd        TYPE STANDARD TABLE OF ts_vbeln_vauf_bstkd .

    TYPES: BEGIN OF ts_vind_footer,
             line001 TYPE text100,
             line002 TYPE text100,
             line003 TYPE text100,
             line004 TYPE text100,
             line005 TYPE text100,
           END OF ts_vind_footer.

    TYPES:
      BEGIN OF ts_data_spec,
        is_sample_printout           TYPE zaf_s_sample_printout,
        is_head                      TYPE ts_head,
        is_adrs_head_we              TYPE adrs_print,
        is_info_data                 TYPE ts_info_data,
        is_addressbock_labels        TYPE ts_adrs_labels,
        it_texts                     TYPE zaf_t_text,
        it_adrs                      TYPE zaf_t_adrs,
        it_adrs_left                 TYPE zaf_t_adrs,
        it_adrs_right                TYPE zaf_t_adrs,
        it_pos                       TYPE tt_pos,
        it_data_before_pos           TYPE tt_before_pos,
        it_data_after_pos            TYPE tt_after_pos,
        it_barcode_after_pos         TYPE tt_after_pos_barcode,
        it_after_pos_ident           TYPE tt_after_pos_ident,
        it_after_pos_ident_barcode   TYPE tt_after_pos_ident_barcode,
        it_after_pos_ident2          TYPE tt_after_pos_ident2,
        it_after_pos_ident_pal       TYPE tt_after_pos_ident_pal,
        it_after_pos_sernr           TYPE tt_after_pos_sernr,
        it_after_all_pos_pick_p      TYPE tt_after_pos_pick_p,
        it_after_all_pos_vse         TYPE tt_after_all_pos_vse,
        it_after_all_pos_ccodes      TYPE tt_ccodes_data,
        is_after_all_pos_ccode_head  TYPE ts_ccode_data_head,
        is_after_all_pos_ccode_total TYPE ts_ccode_data_total,
        it_after_pos_retourentexte   TYPE tt_after_pos_retourentexte,
        it_after_pos_sn_texte        TYPE tt_after_pos_sn_texte,
        it_bundle_text               TYPE tt_bundle_text,
        it_after_all_pos_signaturebl TYPE tt_signatureblock,
        is_vind_org_data             TYPE zcl_af_vind=>ts_vind_org_data,
      END OF ts_data_spec .
    TYPES:
      BEGIN OF ts_split,
        line TYPE c LENGTH 100,
      END OF ts_split .
    TYPES:
      tt_split TYPE STANDARD TABLE OF ts_split WITH EMPTY KEY .
    TYPES:
**********************************************************************
      BEGIN OF ty_delivery_transalation,
        lbl_customer TYPE string,
        lbl_material TYPE string,
      END OF ty_delivery_transalation .

    CLASS-DATA mv_form TYPE tdsfname .

    METHODS constructor
      IMPORTING
        VALUE(iv_form)             TYPE tdsfname
        VALUE(is_docparams)        TYPE sfpdocparams OPTIONAL
        VALUE(is_delivery_details) TYPE /scdl/dlv_note_str OPTIONAL
        VALUE(it_item_details)     TYPE /scwm/dlvnote_itemdata_tbl_pdf OPTIONAL
        VALUE(is_header_details)   TYPE /scwm/dlvnote_header_pdf OPTIONAL
        VALUE(it_hier_sort)        TYPE /scwm/t_dlv_hier_sort_print OPTIONAL .
    METHODS print
      RETURNING
        VALUE(ev_formoutput) TYPE fpformoutput .
    CLASS-METHODS get_doc_ret_fname
      RETURNING
        VALUE(rv_formname) TYPE tdsfname .
  PROTECTED SECTION.
private section.

  data MS_DOCPARAMS type SFPDOCPARAMS .
  data MS_DELIVERY_DETAILS type /SCDL/DLV_NOTE_STR .
  data MT_ITEM_DETAILS type /SCWM/DLVNOTE_ITEMDATA_TBL_PDF .
  data MS_HEADER_DETAILS type /SCWM/DLVNOTE_HEADER_PDF .
  data MT_HIER_SORT type /SCWM/T_DLV_HIER_SORT_PRINT .
  data MV_FORMOUTPUT type FPFORMOUTPUT .
  data MS_TRANSLATIONS type TS_TRANSLATION .
  data MS_DATA_SPEC type TS_DATA_SPEC .
  data MS_DATA_CORE type ZCL_AF_CORE_OUTPUT=>TS_DATA_CORE .
  data CV_LOG_OBJECT type BALOBJ_D value 'DEL_NOTE' ##NO_TEXT.

  methods PRINT_ZDLV_NOTE .
  methods GET_TRANSLATIONS .
  methods GET_HEADER_DATA .
  methods GET_ADRS_RECIPIENT .
  methods GET_ADRS_DATA
    importing
      !IV_ADRNR type ADRC-ADDRNUMBER
    returning
      value(RT_ADRS_DATA) type TT_SPLIT .
  methods GET_SAMPLE_PRINT_OUT .
  methods GET_POS_DATA .
  methods GET_ORG_DATA .
  methods GET_ADRS_AG .
  methods GET_ADRNR_AG
    returning
      value(RV_ADRNR) type KNA1-ADRNR .
  methods GET_TEXTS .
  methods PREPARE_TEXT
    importing
      !IT_KEY_VALUE type ZAF_T_TEXTS_KEY_VALUE optional
    changing
      !CH_TEXT type TSFTEXT .
  methods PAYMENT_CONDITIONS
    importing
      !IV_COND type CHAR2
    returning
      value(RV_COND_TEXT) type CHAR50 .
  methods DELIVERY_CONDITIONS
    importing
      !IV_COND type CHAR2
    returning
      value(RV_COND_TEXT) type CHAR50 .
  methods INCOTERMS
    importing
      !IV_COND type CHAR2
    returning
      value(RV_COND_TEXT) type CHAR50 .
  methods GET_VAT_NO
    importing
      !IV_KUNNR type KNA1-KUNNR
    returning
      value(RV_VAT_NO) type KNA1-STCEG .
ENDCLASS.



CLASS ZCL_INT_DELIVERY_PRINTING IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : AD-230412
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Constructor
*&
**********************************************************************

    mv_form = iv_form.

    IF is_docparams IS SUPPLIED.
      ms_docparams = is_docparams.
    ENDIF.
    IF is_delivery_details IS SUPPLIED.
      ms_delivery_details = is_delivery_details.
    ENDIF.
    IF it_item_details IS SUPPLIED.
      mt_item_details = it_item_details.
    ENDIF.
    IF is_header_details IS SUPPLIED.
      ms_header_details = is_header_details.
    ENDIF.
    IF it_hier_sort IS SUPPLIED.
      mt_hier_sort = it_hier_sort.
    ENDIF.

  ENDMETHOD.


   METHOD delivery_conditions.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Delivery conditions
**********************************************************************

    rv_cond_text = SWITCH #( iv_cond WHEN 'VB' THEN ms_header_details-delterm_txt ).

  ENDMETHOD.


  METHOD get_adrnr_ag.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get Adress no / AG
**********************************************************************
    rv_adrnr = ms_header_details-stprt_addrno.

    SELECT SINGLE adrnr FROM kna1
      INTO rv_adrnr
      WHERE kunnr = ms_data_spec-is_head-kunag.

  ENDMETHOD.


  METHOD get_adrs_ag.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data / AG
**********************************************************************
    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.

    TYPES: BEGIN OF ts_split,
             line TYPE c LENGTH 50,
           END OF ts_split.

    DATA: lt_adrs_line TYPE tt_split,
          lt_split     TYPE STANDARD TABLE OF ts_split,
          lv_fieldname TYPE fieldname,
          lv_line      TYPE n LENGTH 1.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    IF ms_header_details-stprt_addrno <> get_adrnr_ag( ).
      lt_adrs_line = get_adrs_data( get_adrnr_ag( ) ).

      APPEND INITIAL LINE TO ms_data_spec-it_adrs ASSIGNING FIELD-SYMBOL(<ls_adrs>).
      LOOP AT lt_adrs_line FROM 1 TO 7 ASSIGNING FIELD-SYMBOL(<ls_adrs_line>).
        <ls_adrs>-addresslines = VALUE #( BASE <ls_adrs>-addresslines ( line = CONV #( <ls_adrs_line> ) ) ).
      ENDLOOP.
      ms_data_spec-it_adrs_left  = ms_data_spec-it_adrs.
      ms_data_spec-it_adrs_right = ms_data_spec-it_adrs.
    ENDIF.

  ENDMETHOD.


  METHOD get_adrs_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data
**********************************************************************

    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.

    DATA ls_adrs_data TYPE adrs_print.

    SELECT SINGLE
       tsad3t~title_medi AS line0,  " Salutation
       @cv_separator && adrc~name1 AS line1,
       @cv_separator && adrc~name2 AS line2,
       @cv_separator && adrc~name3 AS line3,
       @cv_separator && adrc~name4 AS line4,
       @cv_separator && adrc~street && ' ' &&  adrc~house_num1 AS line5,
       @cv_separator && adrc~post_code1 && ' ' &&  adrc~city1 AS line6,
       @cv_separator && adrc~country AS line7
    FROM adrc
    LEFT OUTER JOIN tsad3t ON tsad3t~title = adrc~title AND tsad3t~langu = @sy-langu
    WHERE addrnumber = @iv_adrnr
    INTO @ls_adrs_data.

    SPLIT ls_adrs_data AT cv_separator INTO TABLE rt_adrs_data.
    DELETE rt_adrs_data WHERE line IS INITIAL.

  ENDMETHOD.


  METHOD get_adrs_recipient.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get adress data / Recipient of goods
**********************************************************************
    CONSTANTS: cv_separator TYPE c LENGTH 1 VALUE ';'.

    TYPES: BEGIN OF ts_split,
             line TYPE c LENGTH 50,
           END OF ts_split.

    DATA: lt_adrs_line TYPE tt_split,
          lt_split     TYPE STANDARD TABLE OF ts_split,
          lv_fieldname TYPE fieldname,
          lv_line      TYPE n LENGTH 1.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    lt_adrs_line = get_adrs_data( ms_header_details-stprt_addrno ).

    LOOP AT lt_adrs_line ASSIGNING FIELD-SYMBOL(<ls_adrs_line>).
      lv_line = sy-tabix - 1.
      lv_fieldname = |line{ lv_line }|.
      UNASSIGN <lv_value>.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE ms_data_spec-is_adrs_head_we TO <lv_value>.
      CHECK <lv_value> IS  ASSIGNED.
      <lv_value> = <ls_adrs_line>-line.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_doc_ret_fname.
**********************************************************************
*& Key           : WG-230630
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get document formname retoure
**********************************************************************

    " Ermittlung des ^Formularnamens muss noch beschrieben werden
    " LOGO u. FOOTER / Kundenabhängig

    " --- Test ----------------------------------------------------
    rv_formname = zcl_int_print_management=>c_delivery_note.

  ENDMETHOD.


  METHOD get_header_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get header data
**********************************************************************


    DATA(lv_vkorg) = VALUE #( ms_delivery_details-hd_sapext_fd[ 1 ]-vkorg OPTIONAL ).

    ms_data_spec-is_head = VALUE #( vgbel              = VALUE #( mt_item_details[ 1 ]-ord_no OPTIONAL )
                                    "sland              = '#SLAND#'
                                    adrnr_ag           = ms_header_details-stprt_addrno
                                    adrnr_we           = ms_header_details-sfprt_addrno
                                    vbeln              = ms_header_details-erpno
                                    erdat              = ms_header_details-crdate
                                    kunag              = ms_header_details-stprt_no
                                    nachnahme_label    = SWITCH #( ms_header_details-delterm WHEN 'SI'
                                                         THEN zcl_af_core_translator=>get_single_translation( iv_labelname = 'NACHNAHME' iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form ) )
                                    kunden_ref_barcode = '1234567890' " Ref. ´muss noch ermittelt werden
                                    kunden_ref_label   = zcl_af_core_translator=>get_single_translation( iv_labelname = 'REFERENZ'  iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
                                  ).

    " INFO Window

    ms_data_spec-is_info_data = VALUE #( BASE ms_data_spec-is_info_data value1  = |{ ms_data_spec-is_head-erdat DATE = USER }|
                                                                        value2  = |{ ms_data_spec-is_head-vbeln ALPHA = OUT }|
                                                                        value3  = |{ ms_data_spec-is_head-vgbel ALPHA = OUT }|
                                                                        value4  = get_vat_no( |{ ms_data_spec-is_head-kunag ALPHA = IN }| )
                                                                        value5  = |{ ms_data_spec-is_head-kunag ALPHA = OUT }|
                                                                        value6  = ms_data_spec-is_head-kunden_ref_barcode
                                                                        value7  = get_vat_no( |{ ms_data_spec-is_head-kunag ALPHA = IN }| )
                                                                        "value8  = '#VALUE8#'
                                                                        "value9  = '#VALUE9#'
                                                                        "value10 = '#VALUE10#'
                                                                        value11 = VALUE #( ms_delivery_details-hd_partyloc[ party_role = 'WH' ]-locationno OPTIONAL )
                                       ).

  ENDMETHOD.


  METHOD get_org_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get organisation data
**********************************************************************
    DATA: ls_org_data_key TYPE zaf_s_org_data_search_keys.

    ls_org_data_key = VALUE #( vkorg     = VALUE #( ms_delivery_details-hd_sapext_fd[ 1 ]-vkorg OPTIONAL )
                               langu     = sy-langu
                               form_name = mv_form
                               channel   = 'PRV'
                               datum     = sy-datum ).

    TRY.
        DATA(lo_org_data) = zcl_af_core_org_data=>get_instance( iv_log_object    = cv_log_object
                                                                is_org_data_keys = ls_org_data_key ).

        IF lo_org_data IS BOUND.
          ms_data_core-wa_org_data = lo_org_data->get_org_data( ).
        ENDIF.
      CATCH cx_root INTO DATA(lo_error).
        DATA(l_text) = lo_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_pos_data.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get positions data
**********************************************************************
    DATA lv_qty TYPE c LENGTH 13.

    SELECT matnr, ean11, mfrpn FROM mara
      FOR ALL ENTRIES IN @mt_item_details
      WHERE matnr = @mt_item_details-prod_no
      INTO TABLE @DATA(lt_mara).

    FIELD-SYMBOLS: <ls_mara> LIKE LINE OF lt_mara.

    DATA(lv_vkorg) = VALUE #( ms_delivery_details-hd_sapext_fd[ 1 ]-vkorg OPTIONAL ).

    LOOP AT mt_item_details ASSIGNING FIELD-SYMBOL(<ls_item_detail>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <ls_item_detail>-prod_no
        IMPORTING
          output = <ls_item_detail>-prod_no.

      lv_qty = <ls_item_detail>-qty.

      WRITE <ls_item_detail>-qty TO lv_qty DECIMALS 0 RIGHT-JUSTIFIED.

      ms_data_spec-it_pos = VALUE #( BASE ms_data_spec-it_pos ( posnr = <ls_item_detail>-item_no+4
                                                                h_pos = <ls_item_detail>-item_no+4
                                                                matnr = <ls_item_detail>-prod_no
                                                                arktx = <ls_item_detail>-prod_txt
                                                                lfimg = lv_qty
                                                                vrkme = <ls_item_detail>-uom
                                                              ) ).

      READ TABLE lt_mara ASSIGNING <ls_mara> WITH KEY matnr = |{ <ls_item_detail>-prod_no(18) ALPHA = IN }|.
      IF sy-subrc = 0.
        ms_data_spec-it_data_after_pos = VALUE #( BASE ms_data_spec-it_data_after_pos
                     posnr = <ls_item_detail>-item_no+4 ( zlabel = zcl_af_core_translator=>get_single_translation( iv_labelname = 'EAN_UPC_CODE' iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
                                                          zvalue = <ls_mara>-ean11 )
                                                        ( zlabel = zcl_af_core_translator=>get_single_translation( iv_labelname = 'HERSTELLERMAT' iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
                                                          zvalue = <ls_mara>-mfrpn ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_sample_print_out.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get data / sample print out
**********************************************************************

    CHECK sy-sysid(2) <> 'PA'.

    ms_data_spec-is_sample_printout = VALUE #( title        = 'SAMPLE-PRINTOUT'
                                               uname_lb     = 'User:'
                                               uname        = sy-uname
                                               tcode_lb     = 'Document:'
                                               tcode        = ms_delivery_details-hd_gen_txt-doctype_txt
                                               form_name_lb = 'Form:'
                                               form_name    = mv_form
                                               sysid_lb     = 'System:'
                                               sysid        = sy-sysid
                                               datum_lb     = 'Printdate:'
                                               datum        = sy-datum ).

  ENDMETHOD.


  METHOD get_texts.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get texts
**********************************************************************

    DATA: lo_inb_delivery TYPE REF TO /scwm/cl_dlv_management_prd,
          lt_selection    TYPE STANDARD TABLE OF /scwm/dlv_selection_str,
          ls_read_options TYPE /scwm/dlv_query_contr_str,
          ls_include      TYPE /scwm/dlv_query_incl_str_prd.

    lo_inb_delivery = /scwm/cl_dlv_management_prd=>get_instance( ).

    " Set up the selection parameter
    lt_selection = VALUE #( ( fieldname = /scdl/if_dl_logfname_c=>sc_refdocno_erp_h " This constant determines the mapping for the database field with the ERP DOC ID.
                              sign      = 'I'
                              option    = 'EQ'
                              low       = ms_header_details-erpno(10) )  ).

    " Set up the read options
    ls_read_options-data_retrival_only      = abap_true. " We only want to read data.
    ls_read_options-mix_in_object_instances = /scwm/if_dl_c=>sc_mix_in_load_instance.

    " Determine, which data should be read by setting the related flag in the structure
    ls_include-head_status   = abap_true.
    ls_include-head_textline = abap_true.
    ls_include-item_status   = abap_true.
    ls_include-item_textline = abap_true.
    ls_include-item_text     = abap_true.
    ls_include-head_text     = abap_true.
    " Execute Query
    TRY.
        lo_inb_delivery->query( EXPORTING iv_doccat         = /scdl/if_dl_c=>sc_doccat_out_prd
                                          it_selection      = lt_selection
                                          is_read_options   = ls_read_options
                                          is_include_data   = ls_include
                                          iv_whno           = 'DE50'
                                IMPORTING et_headers        = DATA(lt_headers)
                                          et_items          = DATA(lt_items)
                                          eo_message        = DATA(lo_meassges)
                                          et_itemtype_texts = DATA(lt_itemtype_texts)
                                          et_itemcat_texts  = DATA(lt_itemcat_texts)
                                          et_od_headers     = DATA(lt_od_headers) ).

      CATCH /scdl/cx_delivery INTO DATA(lx_delivery).
        "MESSAGE lx_delivery type 'S' DISPLAY LIKE 'A'.
    ENDTRY.

    DATA: lt_text_lines  TYPE STANDARD TABLE OF tline,
          lo_text_loader TYPE REF TO zcl_af_core_text_loader.

* Instanziierung der CORE-Textklasse
    LOOP AT lt_headers ASSIGNING FIELD-SYMBOL(<ls_headers>).
      LOOP AT <ls_headers>-text ASSIGNING FIELD-SYMBOL(<ls_header_text>).
        prepare_text( EXPORTING it_key_value = SWITCH #( <ls_header_text>-text_type WHEN '6010' " BOTTOM
                                                                                    THEN VALUE #( " Header data
                                                                                                ( placeholder = 'VBELN'    value = ms_header_details-erpno )
                                                                                                ( placeholder = 'ADRNR_AG' value = ms_header_details-stprt_addrno )
                                                                                                ( placeholder = 'ADRNR_WE' value = ms_header_details-sfprt_addrno )
                                                                                                )
                                                                                    WHEN '6035' "AGB
                                                                                    THEN VALUE #( "Delivery condition
                                                                                                ( placeholder = 'VB'       value = ms_header_details-delterm_txt )
                                                                                                "( placeholder = '???'      value = delivery_conditions( '???' ) )
                                                                                                "Incoterm
                                                                                                ( placeholder = 'I1'       value = ms_delivery_details-hd_incoterms-inco1 )
                                                                                                ( placeholder = 'I2'       value = ms_delivery_details-hd_incoterms-inco2 )
                                                                                                ( placeholder = 'IB'       value = ms_delivery_details-hd_incoterms_txt-inco1_txt )
                                                                                                "( placeholder = '???'      value = incoterms( '???' ) )
*                                                                                                "Pay conditions
*                                                                                                ( placeholder = 'Z1'       value = '???' )
*                                                                                                ( placeholder = 'ZB'       value = '???' )
                                                                                                 "( placeholder = '???'      value = payment_conditions( '???' ) )
                                                                                                 )
                                                        )
                      CHANGING ch_text = <ls_header_text>-text_lines ).

        ms_data_spec-it_texts = VALUE #( BASE ms_data_spec-it_texts  key1 = ms_header_details-erpno ( key2      = '000000'
                                                                                                      key3      = <ls_header_text>-text_type
                                                                                                      filter    = SWITCH #( <ls_header_text>-text_type WHEN '6010' THEN 'BOTTOM'
                                                                                                                                                       WHEN '6035' THEN 'AGB'
                                                                                                                                                       ELSE 'BOTTOM' )
                                                                                                      textlines = <ls_header_text>-text_lines ) ).
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      LOOP AT <ls_item>-text ASSIGNING FIELD-SYMBOL(<ls_item_text>).

        prepare_text( CHANGING ch_text = <ls_item_text>-text_lines ).
*        prepare_text( EXPORTING it_key_value = SWITCH #( <ls_item_text>-text_type WHEN '6010' THEN VALUE #( ( placeholder = 'VBELN'    value = ms_header_details-erpno )
*                                                                                                            ( placeholder = 'ADRNR_AG' value = ms_header_details-stprt_addrno )
*                                                                                                            ( placeholder = 'ADRNR_WE' value = ms_header_details-sfprt_addrno ) )
*                                                                                  WHEN '6035' THEN VALUE #( ( placeholder = 'ZI'       value = 'TEXT ZI' )
*                                                                                                            ( placeholder = 'ZB'       value = 'TEXT ZB' ) )
*                                                       )
*                       CHANGING ch_text = <ls_item_text>-text_lines ).

        ms_data_spec-it_texts = VALUE #( BASE ms_data_spec-it_texts  key1 = ms_header_details-erpno ( key2      = <ls_item>-itemid
                                                                                                      key3      = <ls_item_text>-text_type
                                                                                                      filter    = SWITCH #( <ls_item_text>-text_type WHEN '6010' THEN 'AFTER_POS'
                                                                                                                                                     WHEN '6035' THEN 'BEFORE_POS'
                                                                                                                                                     ELSE 'AFTER_POS' )
                                                                                                      textlines = <ls_item_text>-text_lines ) ).

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_translations.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get translations
**********************************************************************
    " Collect data for translations
    DATA(lo_translation) = NEW zcl_af_core_translator( is_transl_keys = VALUE #( vkorg     = VALUE #( ms_delivery_details-hd_sapext_fd[ 1 ]-vkorg OPTIONAL )
                                                                                 form_name = mv_form
                                                                                 langu     = sy-langu ) ).

    lo_translation->fill_translations( CHANGING cs_transl = ms_translations ).

    " Translations / INFO Window
    DATA(lv_vkorg) = VALUE #( ms_delivery_details-hd_sapext_fd[ 1 ]-vkorg OPTIONAL ).

    ms_data_spec-is_info_data = VALUE #(
      label1  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'DATUM'           iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label2  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'LIEFERSCHEINNR'  iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label3  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'AUFTRAGNR'       iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label4  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'UST1NR'          iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label5  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'KUNDENNR'        iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label6  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'REFERENZ'        iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label7  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'UST2NR'          iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label8  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'ANSPRECHPARTNER' iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label9  = zcl_af_core_translator=>get_single_translation( iv_labelname = 'TELEFON'         iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label10 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'FAX'             iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form )
      label11 = zcl_af_core_translator=>get_single_translation( iv_labelname = 'VERSANDSTELLE'   iv_langu = sy-langu iv_vkorg = lv_vkorg iv_form_name = mv_form ) ).

  ENDMETHOD.


  METHOD get_vat_no.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Get Adress no / AG
**********************************************************************
    CLEAR rv_vat_no.

    SELECT SINGLE stceg FROM kna1
      INTO rv_vat_no
      WHERE kunnr = iv_kunnr.

  ENDMETHOD.


  METHOD incoterms.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Delivery conditions
**********************************************************************

    rv_cond_text = SWITCH #( iv_cond WHEN 'I1' THEN ms_delivery_details-hd_incoterms-inco1
                                     WHEN 'I2' THEN ms_delivery_details-hd_incoterms-inco2
                                     WHEN 'IB' THEN ms_delivery_details-hd_incoterms_txt-inco1_txt ).

  ENDMETHOD.


  METHOD payment_conditions.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Payment conditions
**********************************************************************


*    rv_cond_text = SWITCH #( iv_cond WHEN 'Z1' THEN |PAY COND. { iv_cond }|
*                                     WHEN 'ZB' THEN |PAY COND. { iv_cond }| ).

  ENDMETHOD.


  METHOD prepare_text.
**********************************************************************
*& Key           : WG-230425
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Prepare text
**********************************************************************
    TRY.
        zcl_af_core_text_loader=>get_instance( EXPORTING iv_log_object  = cv_log_object
                                               RECEIVING ro_text_loader = DATA(lo_text_loader) ).

        lo_text_loader->change_text( EXPORTING it_key_value             = it_key_value
                                                iv_placeholder_startkey = '&'
                                                iv_placeholder_endkey   = '&'
                                      CHANGING  ct_textlines            = ch_text ).

        lo_text_loader->delete_crlf( CHANGING ct_textlines = ch_text ).

        " Leerzeilen am Ende des Textes entfernen
        lo_text_loader->delete_empty_lines( CHANGING ct_textlines = ch_text ).

        "Nicht ersetzte Variablen ändern
        lo_text_loader->change_unused_variables( CHANGING ct_textlines = ch_text ).

      CATCH cx_root INTO DATA(cx_error).
        DATA(lv_text) = cx_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD print.
**********************************************************************
*& Key           : AD-230412
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Handles the printing for deliveries
*&
**********************************************************************

    CASE mv_form.
      WHEN '/SCWM/DLV_NOTE'.                           print_zdlv_note(  ).
      WHEN zcl_int_print_management=>c_delivery_note.  print_zdlv_note(  ).
      WHEN OTHERS. RETURN.
    ENDCASE.

    ev_formoutput = mv_formoutput.
  ENDMETHOD.


  METHOD print_zdlv_note.
**********************************************************************
*& Key           : AD-230412
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Handles the printing for Form ZDVL_NOTE
*&
**********************************************************************
    DATA lv_function_name TYPE funcname.

    get_sample_print_out( ).
    get_org_data( ).     " Logo, Footer, Sender ...
    get_translations( ). " Translations

    get_header_data( ).  " Mapping from interface
    get_adrs_recipient( ).
    get_adrs_ag( ).
    get_pos_data( ).     " Mapping from interface
    get_texts( ).        " Kopf u. Pos. - Texts from interface

    " Get generated name for Form function module
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = mv_form
      IMPORTING
        e_funcname = lv_function_name.

    TRY.
        " Call Form function module
        CALL FUNCTION lv_function_name
          EXPORTING
            is_transl     = ms_translations
            is_data_core  = ms_data_core
            is_data_spec  = ms_data_spec
          IMPORTING
            ev_formoutput = mv_formoutput.
      CATCH cx_sy_dyn_call_param_missing INTO DATA(lo_error).
        DATA(l_text) = lo_error->get_text( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
