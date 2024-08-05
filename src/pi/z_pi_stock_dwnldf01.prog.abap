**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-160323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/R_PI_STOCK_DWNLDF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SELECT_DIRECTORY
*&---------------------------------------------------------------------*
*  To select the directory
*----------------------------------------------------------------------*

FORM select_directory CHANGING cv_path TYPE string.

  DATA:
    lv_pcfile    TYPE c,
    lv_title     TYPE string,
    lv_filter    TYPE string,
    lv_rc        TYPE i,
    ls_dynpread  TYPE dynpread,
    lt_filetable TYPE filetable,
    ls_filetable TYPE file_table,
    lt_dynpread  TYPE TABLE OF dynpread.

* get the current value of pv_fg_ph
  ls_dynpread-fieldname = 'P_PCFILE'.
  APPEND ls_dynpread   TO lt_dynpread.
* we have to get the parameter value from the screen buffer
* (this is a historical artifact)
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    READ TABLE lt_dynpread INTO ls_dynpread
      WITH KEY fieldname = 'P_PCFILE'.
    lv_pcfile = ls_dynpread-fieldvalue.
  ENDIF.

  IF lv_pcfile IS NOT INITIAL.
*   Title
    lv_title = TEXT-008.
    CONCATENATE '(*.CSV)|*.CSV' '|'
                  cl_gui_frontend_services=>filetype_excel
           INTO lv_filter.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = lv_title                " Title Of File Open Dialog
        default_extension       = lv_filter                " Default Extension
      CHANGING
        file_table              = lt_filetable               " Table Holding Selected Files
        rc                      = lv_rc                " Return Code, Number of Files or -1 If Error Occurred
      EXCEPTIONS
        file_open_dialog_failed = 1                " "Open File" dialog failed
        cntl_error              = 2                " Control error
        error_no_gui            = 3                " No GUI available
        not_supported_by_gui    = 4                " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
    READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    cv_path = ls_filetable-filename.
    TRANSLATE cv_path TO LOWER CASE.
  ENDIF.

ENDFORM.                    "select_directory
*&---------------------------------------------------------------------*
*& Form CHECK_DIRECTORY
*&---------------------------------------------------------------------*
*  to check the directory
*----------------------------------------------------------------------*
FORM check_directory CHANGING cv_filename TYPE string
                              ct_bapiret  TYPE bapirettab.

  DATA:
    lv_msg       TYPE bapiret2-message,                     "#EC NEEDED
    lv_clen      TYPE i,
    lv_offset    TYPE i,
    lv_directory TYPE authb-filename,
    lv_file_part TYPE string,
    lv_filename  TYPE string.                               "#EC NEEDED

  CLEAR cv_filename.

  IF NOT p_pcfile IS INITIAL.
    IF p_path IS INITIAL.
      MESSAGE e005 INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.
    cv_filename = p_path.
    TRANSLATE cv_filename TO LOWER CASE.

    lv_clen = strlen( cv_filename ).
    lv_offset = lv_clen - 4.
    IF cv_filename+lv_offset(4) <> gc_xls AND
       cv_filename+lv_offset(4) <> gc_csv.
      MESSAGE e037 INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      CLEAR cv_filename.
      RETURN.
    ENDIF.
*   check directory is not possible here. It will be checked by
*   FM 'GUI_DOWNLOAD'
  ELSE.
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        client           = sy-mandt
        logical_filename = gc_lf_name
        parameter_1      = p_lgnum
        parameter_2      = p_cntr
      IMPORTING
        file_name        = cv_filename
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      PERFORM add_bapiret CHANGING ct_bapiret.
      CLEAR cv_filename.
      RETURN.
    ENDIF.
    lv_file_part = gc_lf_name.
    TRANSLATE lv_file_part TO LOWER CASE.
    SPLIT cv_filename AT lv_file_part INTO lv_directory lv_filename.
*   Check authorization
    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
      EXPORTING
        activity         = sabc_act_write
        filename         = lv_directory
      EXCEPTIONS
        no_authority     = 1
        activity_unknown = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      CLEAR cv_filename.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_directory
*&---------------------------------------------------------------------*
*&      Form  execute_sel
*&---------------------------------------------------------------------*
FORM execute_sel USING ir_area        TYPE /scwm/pi_r_aarea
                       ir_lgpla       TYPE /scwm/tt_lgpla_r
                       it_fieldcat    TYPE lvc_t_fcat
              CHANGING ct_stock_dwnld TYPE tyt_stock_dwnld
                       ct_bapiret     TYPE bapirettab.
  DATA:
    lv_msg        TYPE bapiret2-message,                    "#EC NEEDED
    lv_filename   TYPE string,
    lv_count_date TYPE /lime/pi_count_date,
    lv_timezone   TYPE tznzone,
    lt_rows       TYPE lvc_t_row,
    ls_rsdsselopt TYPE rsdsselopt,
    ls_loc_range  TYPE /scwm/s_pi_loc_range,
    lt_picust_are TYPE /scwm/t_picust_are.

  FIELD-SYMBOLS:
    <ls_r_area>     TYPE LINE OF /scwm/pi_r_aarea,
    <ls_picust_are> TYPE LINE OF /scwm/t_picust_are.

* determine time zone for conversion tasks
  CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
    EXPORTING
      iv_lgnum        = p_lgnum
    IMPORTING
      ev_tzone        = lv_timezone
    EXCEPTIONS
      interface_error = 1
      data_not_found  = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE s011 WITH p_lgnum INTO lv_msg.
    PERFORM add_bapiret CHANGING ct_bapiret.
    RETURN.
  ENDIF.
  CONVERT DATE p_cound TIME limpi_end_time
    INTO TIME STAMP lv_count_date TIME ZONE lv_timezone.

  PERFORM check_directory CHANGING lv_filename
                                   ct_bapiret.
  IF lv_filename IS NOT INITIAL.
*   get PI Areas
    TRY.
        lt_picust_are =
          go_cust_scwm->get_pi_area_to_lgnum(
          iv_lgnum = p_lgnum
          ir_aarea = ir_area ).
      CATCH /scwm/cx_pi_app.                            "#EC NO_HANDLER
    ENDTRY.
    IF lt_picust_are IS INITIAL.
      MESSAGE e008(/scwm/pi_cust) WITH p_lgnum INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
    ENDIF.
*   set Storage bins
    ls_loc_range-r_lgpla = ir_lgpla.
    LOOP AT ir_area ASSIGNING <ls_r_area>.
      MOVE-CORRESPONDING <ls_r_area> TO ls_rsdsselopt.
      APPEND ls_rsdsselopt           TO ls_loc_range-r_aarea.
    ENDLOOP.
*   Check Location or product based
    IF p_loc IS NOT INITIAL.
      LOOP AT lt_picust_are ASSIGNING <ls_picust_are>.
        CHECK go_cust_lime->check_pi_doc_type(
                 i_pi_aread    = <ls_picust_are>-pi_area
                 i_pi_doc_type = gc_doc_type_el ) IS INITIAL.
        DELETE lt_picust_are.
      ENDLOOP.
      IF lt_picust_are IS NOT INITIAL.
*       at least one selected PI area is correct maintained
        PERFORM select_location_data USING lv_count_date
                                           ls_loc_range
                                  CHANGING ct_stock_dwnld
                                           ct_bapiret.
      ELSE.
        MESSAGE e045 WITH gc_doc_type_el space INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
      ENDIF.
    ELSE.
      LOOP AT lt_picust_are ASSIGNING <ls_picust_are>.
        CHECK go_cust_lime->check_pi_doc_type(
                 i_pi_aread    = <ls_picust_are>-pi_area
                 i_pi_doc_type = gc_doc_type_es ) IS INITIAL.
        DELETE lt_picust_are.
      ENDLOOP.
      IF lt_picust_are IS NOT INITIAL.

*       at least one selected PI area is correct maintained
        PERFORM select_stock_data USING lv_count_date
                                        ls_loc_range
                               CHANGING ct_stock_dwnld
                                        ct_bapiret.
      ELSE.
        MESSAGE e045 WITH gc_doc_type_es space INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
      ENDIF.
    ENDIF.

  ENDIF.

  IF sy-batch IS NOT INITIAL.
    PERFORM export_to_file USING lv_filename
                                 it_fieldcat
                                 lt_rows
                        CHANGING ct_stock_dwnld
                                 ct_bapiret.

    PERFORM write_application_log.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    "execute_sel
*&---------------------------------------------------------------------*
*& FORM SELECT_LOCATION_DATA
*&---------------------------------------------------------------------*
* Retrieves location based data.
*----------------------------------------------------------------------*
FORM select_location_data
     USING iv_count_date  TYPE /lime/pi_count_date
           is_loc_range   TYPE /scwm/s_pi_loc_range
  CHANGING ct_stock_dwnld TYPE tyt_stock_dwnld
           ct_bapiret     TYPE bapirettab.

  DATA:
    lv_msg               TYPE bapiret2-message,             "#EC NEEDED
    ls_stock_dwnld       TYPE /scwm/s_pi_stock_dwnld,
    lt_location_for_area TYPE /scwm/t_pi_location_for_area.

  FIELD-SYMBOLS:
    <ls_location_for_area> TYPE /scwm/s_pi_location_for_area.

* Location based selection
  CALL FUNCTION '/SCWM/PI_GET_LOCATION_FOR_AREA'
    EXPORTING
      iv_lgnum             = p_lgnum
      iv_count_date        = iv_count_date
      iv_doc_type          = gc_doc_type_el
      iv_ind_inv           = abap_true
      iv_reason            = p_rea
      is_loc_range         = is_loc_range
    IMPORTING
      et_location_for_area = lt_location_for_area
      et_bapiret           = ct_bapiret.

* no selection results --> send status message
  IF lt_location_for_area IS INITIAL.
    MESSAGE s038(/scwm/pi_appl) INTO lv_msg.
    PERFORM add_bapiret CHANGING ct_bapiret.
  ELSE.
    LOOP AT lt_location_for_area ASSIGNING <ls_location_for_area>.
      MOVE-CORRESPONDING <ls_location_for_area>
                      TO ls_stock_dwnld.                    "#EC ENHOK
      APPEND ls_stock_dwnld TO ct_stock_dwnld.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "SELECT_LOCATION_DATA
*&---------------------------------------------------------------------*
*& FORM SELECT_STOCK_DATA
*&---------------------------------------------------------------------*
*  Retrieves product based data
*----------------------------------------------------------------------*
FORM select_stock_data USING iv_count_date  TYPE /lime/pi_count_date
                             is_loc_range   TYPE /scwm/s_pi_loc_range
                    CHANGING ct_stock_dwnld TYPE tyt_stock_dwnld
                             ct_bapiret     TYPE bapirettab.

  DATA:
    lv_msg              TYPE bapiret2-message,              "#EC NEEDED
    lv_id_val_lgnum     TYPE xuval,
    lv_id_val_entl      TYPE xuval,
*   structures and ranges for selection
    ls_stock_range      TYPE /scwm/s_stock_range,
    ls_r_entitled       TYPE LINE OF /scwm/tt_entitled_r,
    ls_r_matnr          TYPE rseloption,
    ls_s_matnr          TYPE rsdsselopt,
    ls_s_matid          TYPE /scwm/s_matid_r,
    ls_stock_dwnld      TYPE /scwm/s_pi_stock_dwnld,
    ls_matid            TYPE /scwm/s_matid_value_get,
    ls_mat_global       TYPE /scwm/s_material_global,
    lt_entitled         TYPE /scwm/tt_entitled,
    lr_entl_auth        TYPE /scwm/tt_entitled_r,
*   material mapping
    lt_matid_value      TYPE /scwm/tt_matid_value,
    lt_matid            TYPE /scwm/tt_matid_value_get,
    lt_matid_matnr      TYPE /scwm/tt_matid_matnr,
*   for stock area
    lt_stock_for_area   TYPE /scwm/t_pi_stock_for_area,
    lv_quantity_in_auom TYPE /scwm/de_quantity.

  DATA: lt_huhdr     TYPE /scwm/tt_huhdr_int,
        lt_huitm     TYPE /scwm/tt_huitm_int,
        ls_loc_range TYPE /scwm/s_pi_loc_range,
        lt_mara      TYPE ztt_mara_sel.

  FIELD-SYMBOLS:
    <ls_entitled>       TYPE LINE OF /scwm/tt_entitled,
    <ls_matid_value>    TYPE /scwm/s_matid_value,
    <ls_stock_for_area> TYPE /scwm/s_pi_stock_for_area,
    <fs_matid_matnr>    TYPE /scwm/s_matid_matnr,
    <fs_s_matnr>        LIKE s_matnr.

  ls_loc_range = is_loc_range.

  IF so_huid IS NOT INITIAL.
    "if the user has entered HU numbers:
    "1. get the storage bin & materials for given HUs
    DATA(lr_huident) = VALUE rseloption( FOR GROUPS OF <ls_huid> IN so_huid
                                            GROUP BY <ls_huid>-low
                                            ( sign = <ls_huid>-sign
                                              option = <ls_huid>-option
                                              low = <ls_huid>-low
                                              high = <ls_huid>-high ) ).
    zcl_pi_query=>select_hus_gen(
      EXPORTING
        iv_lgnum   = p_lgnum
        it_huident = lr_huident
      IMPORTING
        et_huhdr   = lt_huhdr
        et_huitm   = lt_huitm
    ).
    "if lt_huhdr < lr_huident -> there is at least 1 invalid input
    IF lines( lt_huhdr ) < lines( lr_huident ).
      MESSAGE e001(/scmb/se_incmd_msig). "invalid input
    ENDIF.

    "if HU item contains at least 1 product, add this matid to the search criteria
    IF lines( lt_huitm ) <> 0.
      APPEND LINES OF VALUE
         /scwm/tt_matid_r( FOR GROUPS OF <ls_huitm>
          IN lt_huitm
         GROUP BY <ls_huitm>-matid
          ( sign = wmegc_sign_inclusive
            option = wmegc_option_eq
            low = <ls_huitm>-matid ) ) TO ls_stock_range-r_matid.
    ELSE.
      MESSAGE e876(/sapapo/rsp).
    ENDIF.

    "add storage bins of entered HUs to search criteria
    APPEND LINES OF VALUE
     /scwm/tt_lgpla_r( FOR GROUPS OF <ls_lgpla>
      IN lt_huhdr
     GROUP BY <ls_lgpla>-lgpla
      ( sign = wmegc_sign_inclusive
        option = wmegc_option_eq
        low = <ls_lgpla>-lgpla ) ) TO ls_loc_range-r_lgpla.

    "select manufacturer number & manufacturer part number for materials which are in the entered HUs
    zcl_pi_query=>select_mara_by_matid(
      EXPORTING
        it_matid = VALUE /scwm/tt_matid_r( FOR GROUPS OF <ls_matid> IN lt_huitm
                                            GROUP BY <ls_matid>-matid
                                            ( sign = wmegc_sign_inclusive
                                             option = wmegc_option_eq
                                             low = <ls_matid>-matid  ) )                " Range Table Type for Product
      IMPORTING
        et_mara  = lt_mara                 " Table Type for MARA selection of mfrnr, mfrpn, matid
    ).

  ENDIF.

  IF so_mfrnr IS NOT INITIAL
    OR so_mfrpn IS NOT INITIAL.

    " if the user has entered manufacturer number/manufacturer part number,
    " perfrom a select on MARA with those criteria
    zcl_pi_query=>select_mara_by_mfrnr_mfrpn(
      EXPORTING
        it_mfrnr = VALUE rseloption( FOR GROUPS OF <ls_mfrnr> IN so_mfrnr
                                            GROUP BY <ls_mfrnr>-low
                                            ( sign = wmegc_sign_inclusive
                                             option = wmegc_option_eq
                                             low = <ls_mfrnr>-low
                                             high = <ls_mfrnr>-high  ) )
        it_mfrpn = VALUE rseloption( FOR GROUPS OF <ls_mfrpn> IN so_mfrpn
                                            GROUP BY <ls_mfrpn>-low
                                            ( sign = wmegc_sign_inclusive
                                             option = wmegc_option_eq
                                             low = <ls_mfrpn>-low
                                             high = <ls_mfrpn>-high  ) )
      IMPORTING
        et_mara  = lt_mara                " Table Type for MARA selection of mfrnr, mfrpn, matid
    ).

    IF lt_mara IS INITIAL.
      MESSAGE e876(/sapapo/rsp).
    ENDIF.

    " add found material ids for corresponding
    " manufacturer number/manufacturer part number to search criteria
    ls_stock_range-r_matid = VALUE #( FOR <ls_mara> IN lt_mara
                                      ( sign = wmegc_sign_inclusive
                                        option = wmegc_option_eq
                                        low = <ls_mara>-matid ) ).
  ENDIF.

* Product
  IF s_matnr[] IS NOT INITIAL.
    CLEAR ls_r_matnr.
    LOOP AT s_matnr[] ASSIGNING <fs_s_matnr>.
      "check if entered material number corresponds to
      " any of the entered manufacturer numbers/manufacturer part numbers
      IF lt_mara IS NOT INITIAL
        AND NOT line_exists( lt_mara[ matnr = <fs_s_matnr>-low ] ).
        MESSAGE e001(/scmb/se_incmd_msig). "invalid input
      ENDIF.
      MOVE-CORRESPONDING <fs_s_matnr> TO ls_s_matnr.
      APPEND ls_s_matnr               TO ls_r_matnr.
    ENDLOOP.

*   get the material id's
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_RANGE'
          EXPORTING
            it_matnr_range = ls_r_matnr
            iv_get_only_id = abap_true
          IMPORTING
            et_matid       = lt_matid_matnr.
      CATCH /scwm/cx_md.
*       No material found -> no selection results expected
        MESSAGE e002(/scwm/md) INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
        RETURN.
    ENDTRY.
    IF lt_matid_matnr[] IS INITIAL.
*     No material found -> no selection results expected
      MESSAGE e002(/scwm/md) INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.
    CLEAR ls_s_matid.
    LOOP AT lt_matid_matnr ASSIGNING <fs_matid_matnr>.
      "check if material number has not been already added to selection criteria.
      CHECK NOT line_exists( ls_stock_range-r_matid[ low = <fs_matid_matnr>-matid ] ).
      ls_s_matid-sign    = wmegc_sign_inclusive.
      ls_s_matid-option  = wmegc_option_eq.
      ls_s_matid-low     = <fs_matid_matnr>-matid.
      APPEND ls_s_matid TO ls_stock_range-r_matid.
    ENDLOOP.
  ENDIF.

* Person responsible
  ls_stock_range-r_owner = s_owner[].
* Entitled
  ls_stock_range-r_entitled = s_entitl[].
* Stock type
  ls_stock_range-r_cat = s_cat_p[] .
* Product based selection
  CALL FUNCTION '/SCWM/PI_GET_STOCK_FOR_AREA'
    EXPORTING
      iv_lgnum          = p_lgnum
      iv_count_date     = iv_count_date
      iv_doc_type       = gc_doc_type_es
      iv_ind_inv        = abap_true
      iv_reason         = p_rea
      is_loc_range      = ls_loc_range
      is_stock_range    = ls_stock_range
    IMPORTING
      et_stock_for_area = lt_stock_for_area
      et_bapiret        = ct_bapiret.

* no selection results --> send status message
  IF lt_stock_for_area IS INITIAL.
    MESSAGE s038(/scwm/pi_appl) INTO lv_msg.
    PERFORM add_bapiret CHANGING ct_bapiret.
  ELSE.
    CLEAR ls_stock_range.

    "select mara to get mfrnr & mfrpn
    "in case the user has not entered
    "any filter values for mfrnr/mfrpn/product
    IF lt_mara IS INITIAL.
      zcl_pi_query=>select_mara_by_matnr(
        EXPORTING
          it_matnr = VALUE /scwm/tt_matnr_r( FOR GROUPS OF <ls_stock_matnr>
                                         IN lt_stock_for_area
                                         GROUP BY <ls_stock_matnr>-matnr
                                         ( sign = wmegc_sign_inclusive
                                           option = wmegc_option_eq
                                           low = <ls_stock_matnr>-matnr ) )
        IMPORTING
          et_mara  = lt_mara                " Table Type for MARA selection of mfrnr, mfrpn, matid
      ).
    ENDIF.

    " if user has not entered HU as a search criteria,
    " get HUs from entries from lt_stock_area
    IF lt_huhdr IS INITIAL.

      zcl_pi_query=>select_hus_gen(
        EXPORTING
          iv_lgnum = p_lgnum               " Warehouse Number/Warehouse Complex
          it_matnr = VALUE rseloption( FOR GROUPS OF <ls_mara> IN lt_mara
                                     GROUP BY <ls_mara>-matnr
                                     ( sign = wmegc_sign_inclusive
                                       option = wmegc_option_eq
                                       low = <ls_mara>-matnr ) )                  " SELECT-OPTIONS Table
          it_lgpla = VALUE rseloption( FOR GROUPS OF <ls_stock> IN lt_stock_for_area
                                    GROUP BY <ls_stock>-lgpla
                                    ( sign = wmegc_sign_inclusive
                                      option = wmegc_option_eq
                                      low = <ls_stock>-lgpla ) )             " SELECT-OPTIONS Table
        IMPORTING
          et_huhdr = lt_huhdr                " Table Type for HU Headers in the Internal Structure
          et_huitm = lt_huitm                " Material Items in the HU
      ).

    ENDIF.
  ENDIF.

*   get selected entitled
  CALL FUNCTION '/SCWM/ENTITLED_FOR_LGNUM_READ'
    EXPORTING
      iv_lgnum    = p_lgnum
    IMPORTING
      et_entitled = lt_entitled.
*   fill work table without matid: will be selected later!
  lv_id_val_lgnum = p_lgnum.
  LOOP AT lt_entitled ASSIGNING <ls_entitled>
    WHERE entitled IN s_entitl[].
*     check if the authorization switsched on and the user does have the authorization
*     to display the prices/difference values
    lv_id_val_entl = <ls_entitled>-entitled.
    CHECK 0 = cl_sacf=>auth_check_spec( id_name = '/SCWM/PRODUCT_PRICES'
                                        id_suso = '/SCWM/SLFE'
                                        id_fld1 = '/SCWM/LGNU'
                                        id_val1 = lv_id_val_lgnum
                                        id_fld2 = '/SCWM/ENTL'
                                        id_val2 = lv_id_val_entl
                                        id_fld3 = '/SCWM/SLFU'
                                        id_val3 = 'VD' ).
*     the user does have display athorization for the entitled
    ls_r_entitled-sign    = wmegc_sign_inclusive.
    ls_r_entitled-option  = wmegc_option_eq.
    ls_r_entitled-low     = <ls_entitled>-entitled.
    APPEND ls_r_entitled TO lr_entl_auth.
  ENDLOOP.
  IF lr_entl_auth IS NOT INITIAL.
*     get valuation data for authorized lgnum&entitleds
    LOOP AT lt_stock_for_area ASSIGNING <ls_stock_for_area>
      WHERE entitled IN lr_entl_auth.
      ls_matid-matid    = <ls_stock_for_area>-matid.
      ls_matid-entitled = <ls_stock_for_area>-entitled.
      APPEND ls_matid  TO lt_matid.
    ENDLOOP.
  ENDIF.

  IF lt_matid IS NOT INITIAL.
    SORT lt_matid.
    DELETE ADJACENT DUPLICATES FROM lt_matid.
    CALL FUNCTION '/SCWM/MATERIAL_VALUATION_GET'
      EXPORTING
        i_lgnum        = p_lgnum
        it_material    = lt_matid
      IMPORTING
        et_matid_value = lt_matid_value
      EXCEPTIONS
        wrong_input    = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.
  ENDIF.
  FREE: lt_matid, lt_entitled, ls_stock_range.
  SORT lt_matid_value BY matid entitled.

  LOOP AT lt_stock_for_area ASSIGNING <ls_stock_for_area>.

    CLEAR ls_mat_global.
    CLEAR ls_stock_dwnld.
    MOVE-CORRESPONDING <ls_stock_for_area>
                    TO ls_stock_dwnld.                      "#EC ENHOK
*     read product master
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = <ls_stock_for_area>-matid
            iv_entitled   = <ls_stock_for_area>-entitled
            iv_lgnum      = p_lgnum
          IMPORTING
            es_mat_global = ls_mat_global.
      CATCH /scwm/cx_md.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
        RETURN.
    ENDTRY.
*     Calculate material values
    READ TABLE lt_matid_value ASSIGNING <ls_matid_value>
      WITH KEY matid    = <ls_stock_for_area>-matid
               entitled = <ls_stock_for_area>-entitled
               BINARY SEARCH.
    IF sy-subrc = 0.
*       Calculate material values
      IF ls_mat_global-cwunit IS NOT INITIAL.
        CASE <ls_matid_value>-vprsv.
          WHEN gc_standard_price.
            ls_stock_dwnld-value = abs( <ls_matid_value>-stprs *
              <ls_stock_for_area>-cwquan / <ls_matid_value>-peinh ).
          WHEN gc_moving_average_price.
            ls_stock_dwnld-value = abs( <ls_matid_value>-verpr *
              <ls_stock_for_area>-cwquan / <ls_matid_value>-peinh ).
          WHEN OTHERS.
        ENDCASE.
      ELSE.
        CASE <ls_matid_value>-vprsv.
          WHEN gc_standard_price.
            ls_stock_dwnld-value = abs( <ls_matid_value>-stprs *
              <ls_stock_for_area>-quan / <ls_matid_value>-peinh ).
          WHEN gc_moving_average_price.
            ls_stock_dwnld-value = abs( <ls_matid_value>-verpr *
              <ls_stock_for_area>-quan / <ls_matid_value>-peinh ).
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      ls_stock_dwnld-vprsv = <ls_matid_value>-vprsv.
      ls_stock_dwnld-verpr = <ls_matid_value>-verpr.
      ls_stock_dwnld-waers = <ls_matid_value>-waers.
    ENDIF.
    IF ls_stock_dwnld-stock_docno IS NOT INITIAL.
      ls_stock_dwnld-stock_docno_ext =
        go_stock_fields->get_stock_docno_ext(
        iv_stock_doccat = ls_stock_dwnld-stock_doccat
        iv_stock_docno  = ls_stock_dwnld-stock_docno ).
    ENDIF.
    IF ls_stock_dwnld-idplate IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_stock_dwnld-idplate
        IMPORTING
          output = ls_stock_dwnld-idplate.
    ENDIF.
    IF ls_stock_dwnld-qdocno IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_stock_dwnld-qdocno
        IMPORTING
          output = ls_stock_dwnld-qdocno.
    ENDIF.
    IF ls_stock_dwnld-inspdocno IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_stock_dwnld-inspdocno
        IMPORTING
          output = ls_stock_dwnld-inspdocno.
    ENDIF.

    BREAK-POINT ID /scwm/suom.
    "convert quantity into alternative UoM; if the alternative UoM is not filled
    "there are multiple alternative UoMs with the same stock separating attributes
    "on the bin. in this case the fields for the alternative UoM stay empty.
    IF ls_stock_dwnld-aunit IS NOT INITIAL.
      IF ls_stock_dwnld-aunit <> ls_stock_dwnld-unit.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = <ls_stock_for_area>-matid
                iv_quan      = <ls_stock_for_area>-quan
                iv_unit_from = <ls_stock_for_area>-unit
                iv_unit_to   = <ls_stock_for_area>-aunit
                iv_batchid   = <ls_stock_for_area>-batchid
              IMPORTING
                ev_quan      = lv_quantity_in_auom.

            ls_stock_dwnld-quana = lv_quantity_in_auom.

          CATCH /scwm/cx_md.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
            PERFORM add_bapiret CHANGING ct_bapiret.
            RETURN.
        ENDTRY.
      ELSE.
        "if the alternative UoM is the same as the base UoM we just
        "take over the quantity from the base Uom field
        ls_stock_dwnld-quana = ls_stock_dwnld-quan.
      ENDIF.
    ENDIF.

    TRY.
        ls_stock_dwnld-zz_mfrnr = VALUE #( lt_mara[ matnr = ls_stock_dwnld-matnr ]-mfrnr OPTIONAL ).
        ls_stock_dwnld-zz_mfrpn = VALUE #( lt_mara[ matnr = ls_stock_dwnld-matnr ]-mfrpn OPTIONAL ).
        ASSIGN lt_huhdr[ lgpla = ls_stock_dwnld-lgpla ] TO FIELD-SYMBOL(<ls_huhdr>).
        IF sy-subrc = 0.
          ls_stock_dwnld-zz_huident = VALUE #( lt_huhdr[ guid_hu = lt_huitm[ matid = lt_mara[ matnr = ls_stock_dwnld-matnr ]-matid
                                                                                    guid_parent = <ls_huhdr>-guid_hu ]-guid_parent ]-huident ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
    APPEND ls_stock_dwnld TO ct_stock_dwnld.
  ENDLOOP.

ENDFORM.                    "SELECT_STOCK_DATA
*&---------------------------------------------------------------------*
*&      FORM add_bapiret
*&---------------------------------------------------------------------*
FORM add_bapiret CHANGING ct_bapiret TYPE bapirettab.

  DATA:
     ls_return TYPE bapiret2.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = sy-msgty
      cl     = sy-msgid
      number = sy-msgno
      par1   = sy-msgv1
      par2   = sy-msgv2
      par3   = sy-msgv3
      par4   = sy-msgv4
    IMPORTING
      return = ls_return.

  APPEND ls_return TO ct_bapiret.

ENDFORM.                    "add_bapiret
*&---------------------------------------------------------------------*
*&      FORM CREATE_LOG
*&---------------------------------------------------------------------*
*       Create application log.
*----------------------------------------------------------------------*
FORM create_log CHANGING co_log TYPE REF TO /scwm/cl_log.

  DATA:
    ls_log TYPE bal_s_log.
*
  IF co_log IS BOUND.
    CLEAR: co_log.
  ENDIF.

  ls_log = VALUE #( object = wmegc_apl_object_wme
                    subobject = wmegc_apl_subob_gen
                    extnumber = '1' ).

  CREATE OBJECT co_log.

  CALL METHOD co_log->create_log
    EXPORTING
      is_log = ls_log.

ENDFORM.                    "CREATE_LOG
*&---------------------------------------------------------------------*
*&      FORM DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Display application log.
*----------------------------------------------------------------------*
FORM display_log USING io_log     TYPE REF TO /scwm/cl_log
                       it_bapiret TYPE bapirettab.

  DATA:
    ls_disp_profile TYPE bal_s_prof.

  IF io_log IS NOT BOUND.
    RETURN.
  ENDIF.
  IF it_bapiret IS INITIAL.
    MESSAGE s068(/scwm/pi_appl).
    RETURN.
  ENDIF.

  io_log->convert_bapiret2applog( it_bapiret = it_bapiret ).
* display log as popup window
  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    IMPORTING
      e_s_display_profile = ls_disp_profile.
  ls_disp_profile-use_grid = abap_true.
  TRY.
      CALL METHOD io_log->display_log
        EXPORTING
          is_display_profile = ls_disp_profile.
    CATCH /scwm/cx_basics.                              "#EC NO_HANDLER
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

ENDFORM.                    "DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  initialize_alv
*&---------------------------------------------------------------------*
*       Initialize ALV Grid
*----------------------------------------------------------------------*
FORM initialize_alv CHANGING co_alvgrid TYPE REF TO cl_gui_alv_grid.

* Create ALV grid
  CREATE OBJECT co_alvgrid
    EXPORTING
      i_parent = cl_gui_container=>screen0.

ENDFORM.                    " initialize_alv
*&--------------------------------------------------------------------*
*&      Form  HIDE_PRODUCT
*&--------------------------------------------------------------------*
FORM hide_product CHANGING ct_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <ls_fieldcat> TYPE lvc_s_fcat.

  LOOP AT ct_fieldcat ASSIGNING <ls_fieldcat>.
    CASE <ls_fieldcat>-fieldname.
      WHEN 'LGNUM'.
        <ls_fieldcat>-col_pos = 1.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'LGPLA'.
        <ls_fieldcat>-col_pos = 2.
      WHEN 'LGTYP'.
        <ls_fieldcat>-col_pos = 3.
      WHEN 'LGBER'.
        <ls_fieldcat>-col_pos = 4.
      WHEN 'ACTIVE'.
        <ls_fieldcat>-col_pos = 5.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'PRIORITY'.
        <ls_fieldcat>-col_pos = 6.
      WHEN 'PRIORITY_TXT'.
        <ls_fieldcat>-col_pos = 7.
      WHEN 'COUNT_DATE_UI'.
        <ls_fieldcat>-col_pos = 8.
      WHEN 'REASON'.
        <ls_fieldcat>-col_pos = 9.
      WHEN 'REASON_TXT'.
        <ls_fieldcat>-col_pos = 10.
      WHEN 'PI_AREA_UI'.
        <ls_fieldcat>-col_pos = 11.
      WHEN OTHERS.
        <ls_fieldcat>-tech = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "HIDE_PRODUCT
*&--------------------------------------------------------------------*
*&      Form  HIDE_LOCATION
*&--------------------------------------------------------------------*
FORM hide_location CHANGING ct_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS:
    <ls_fieldcat> TYPE lvc_s_fcat.

  LOOP AT ct_fieldcat ASSIGNING <ls_fieldcat>.
    CASE <ls_fieldcat>-fieldname.
      WHEN 'LGNUM'.
        <ls_fieldcat>-col_pos = 1.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'LGPLA'.
        <ls_fieldcat>-col_pos = 2.
      WHEN 'LGTYP'.
        <ls_fieldcat>-col_pos = 3.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'LGBER'.
        <ls_fieldcat>-col_pos = 4.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'ACTIVE'.
        <ls_fieldcat>-col_pos = 5.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'PRIORITY'.
        <ls_fieldcat>-col_pos = 6.
      WHEN 'PRIORITY_TXT'.
        <ls_fieldcat>-col_pos = 7.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'COUNT_DATE_UI'.
        <ls_fieldcat>-col_pos = 8.
      WHEN 'REASON'.
        <ls_fieldcat>-col_pos = 9.
      WHEN 'REASON_TXT'.
        <ls_fieldcat>-col_pos = 10.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'MATNR'.
        <ls_fieldcat>-col_pos = 11.
      WHEN 'MAKTX'.
        <ls_fieldcat>-col_pos = 12.
      WHEN 'CHARG'.
        <ls_fieldcat>-col_pos = 13.
      WHEN 'CAT'.
        <ls_fieldcat>-col_pos = 14.
      WHEN 'CAT_TEXT'.
        <ls_fieldcat>-col_pos = 15.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'UI_WDATU'.
        <ls_fieldcat>-col_pos = 16.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'VFDAT'.
        <ls_fieldcat>-col_pos = 17.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'COO'.
        <ls_fieldcat>-col_pos = 18.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'COO_TEXT'.
        <ls_fieldcat>-col_pos = 19.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'IDPLATE'.
        <ls_fieldcat>-col_pos = 20.
      WHEN 'QDOCCAT'.
        <ls_fieldcat>-col_pos = 21.
      WHEN 'QDOCCAT_TEXT'.
        <ls_fieldcat>-col_pos = 22.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'QDOCNO'.
        <ls_fieldcat>-col_pos = 23.
      WHEN 'QITEMNO'.
        <ls_fieldcat>-col_pos = 24.
      WHEN 'INSPTYP'.
        <ls_fieldcat>-col_pos = 25.
      WHEN 'INSPTYP_TEXT'.
        <ls_fieldcat>-col_pos = 26.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'INSPDOCNO'.
        <ls_fieldcat>-col_pos = 27.
      WHEN 'VPRSV'.
        <ls_fieldcat>-col_pos = 28.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'VERPR'.
        <ls_fieldcat>-col_pos = 29.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'WAERS'.
        <ls_fieldcat>-col_pos = 30.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'QUAN'.
        <ls_fieldcat>-col_pos = 31.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'UNIT'.
        <ls_fieldcat>-col_pos = 32.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'QUANA'.
        <ls_fieldcat>-col_pos = 33.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'AUNIT'.
        <ls_fieldcat>-col_pos = 34.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'VALUE'.
        <ls_fieldcat>-col_pos = 35.
        <ls_fieldcat>-no_out  = abap_true.
      WHEN 'PI_AREA_UI'.
        <ls_fieldcat>-col_pos = 36.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'ENTITLED'.
        <ls_fieldcat>-col_pos = 37.
**********************************************************************
        "GAP-14, aahmedov, 16.03.2023
      WHEN 'ZZ_MFRNR'.
        <ls_fieldcat>-col_pos = 38.
      WHEN 'ZZ_MFRPN'.
        <ls_fieldcat>-col_pos = 39.
      WHEN 'ZZ_HUIDENT'.
        <ls_fieldcat>-col_pos = 40.
**********************************************************************
      WHEN 'ENTITLED_ROLE'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'ENTITLED_TEXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'OWNER'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'OWNER_ROLE'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'OWNER_TEXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_DOCCAT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_DOCCAT_TEXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_DOCNO'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_DOCNO_EXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_ITMNO'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_USAGE'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STOCK_USAGE_TEXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STREF_DOCCAT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'STREF_DOCCAT_TEXT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN 'UI_WDATT'.
        <ls_fieldcat>-no_out = abap_true.
      WHEN OTHERS.
        <ls_fieldcat>-tech   = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "HIDE_LOC_PROD
*&---------------------------------------------------------------------*
*&      Form  Display_alv
*&---------------------------------------------------------------------*
*       Display alv
*----------------------------------------------------------------------*
FORM display_alv CHANGING ct_fieldcat       TYPE lvc_t_fcat
                          ct_stock_dwnld    TYPE tyt_stock_dwnld
                          co_alvgrid        TYPE REF TO cl_gui_alv_grid
                          co_event_receiver TYPE REF TO lcl_alv_menu.

  DATA:
    ls_variant TYPE disvariant,
    ls_layout  TYPE lvc_s_layo.    " Layout structure

* create objects if necessary
  PERFORM initialize_alv CHANGING co_alvgrid.

  ls_variant-report = sy-repid.
  ls_variant-handle = 'DWLD'.
* select multiple lines
  ls_layout-sel_mode   = 'D'.
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
* display alv - location based
  CALL METHOD co_alvgrid->set_table_for_first_display
    EXPORTING
      i_structure_name = '/SCWM/S_PI_STOCK_DWNLD'
      is_variant       = ls_variant
      i_save           = 'A'
      is_layout        = ls_layout
    CHANGING
      it_fieldcatalog  = ct_fieldcat
      it_outtab        = ct_stock_dwnld.
* handle the user command
  co_event_receiver = NEW lcl_alv_menu( ).
  SET HANDLER co_event_receiver->handle_user_command FOR co_alvgrid. "UNCOMMENT when executing
  SET HANDLER co_event_receiver->handle_toolbar      FOR co_alvgrid. "UNCOMMENT when executing

* Call method 'set_toolbar_interactive' to raise event TOOLBAR.
  co_alvgrid->set_toolbar_interactive( ). "when executing

ENDFORM.                    "display_alv
*&--------------------------------------------------------------------*
*&      Form  EXPORT_TO_FILE
*&--------------------------------------------------------------------*
* If foreground, downloads selected rows to the presentation or
* application server file based on the selection criteria.
* If background, Downloads all the data to the application server based
* on the selection criteria.
*---------------------------------------------------------------------*
FORM export_to_file USING iv_filename    TYPE string
                          it_fieldcat    TYPE lvc_t_fcat
                          it_rows        TYPE lvc_t_row
                 CHANGING ct_stock_dwnld TYPE tyt_stock_dwnld
                          ct_bapiret     TYPE bapirettab.

  DATA: BEGIN OF ls_fields,
          name TYPE aqadef-fname, " store column headline and
        END OF ls_fields.

  DATA:
    lv_msg           TYPE bapiret2-message,                 "#EC NEEDED
    lv_clen          TYPE i,
    lv_offset        TYPE i,
    lv_mess(60)      TYPE c,
    lv_tabix         TYPE sytabix,
    lv_phys_filename TYPE string,
    lv_answer(1)     TYPE c,
    lv_file_exist    TYPE abap_bool,
    lt_fields        LIKE STANDARD TABLE OF ls_fields,
    lt_stock_dwnld   TYPE STANDARD TABLE OF /scwm/s_pi_stock_dwnld,
    lt_stringtab     TYPE stringtab,
    lo_wa_dyn_table  TYPE REF TO data,
    lo_dyn_table     TYPE REF TO data.

  FIELD-SYMBOLS:
    <fs_dyn_table>     TYPE STANDARD TABLE,
    <fs_wa_dyn_table>  TYPE any,
    <fs_stock_dwnld>   TYPE /scwm/s_pi_stock_dwnld,
    <fs_string>        TYPE string,
    <fs_selected_line> TYPE lvc_s_row,
    <fs_fieldcatalog>  TYPE lvc_s_fcat,
    <fs_field>         TYPE any.

  IF p_pcfile = abap_true.
    IF p_app IS NOT INITIAL OR p_crea IS NOT INITIAL.
*     check if the file exist -> if yes, do not create head line
      cl_gui_frontend_services=>file_exist(
        EXPORTING
          file                 = iv_filename
        RECEIVING
          result               = lv_file_exist
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
        RETURN.
      ENDIF.
    ENDIF.
    IF p_crea        IS NOT INITIAL AND
       lv_file_exist IS NOT INITIAL AND
       sy-batch      IS INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = TEXT-009
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        PERFORM add_bapiret CHANGING ct_bapiret.
        RETURN.
      ENDIF.
      IF lv_answer = 2.
        RETURN.
      ENDIF.
      CLEAR lv_file_exist.
    ENDIF.
    IF p_crea IS NOT INITIAL OR
       p_repl IS NOT INITIAL.
      CLEAR lv_file_exist.
    ENDIF.
    lv_clen = strlen( iv_filename ).
    lv_offset = lv_clen - 4.
    IF iv_filename+lv_offset(4) = gc_xls.
*     Create dynamic table for PC download
      /scwm/cl_pi_ui_appl=>create_dynamictable(
        EXPORTING
          it_fieldcat  = it_fieldcat
        IMPORTING
          eo_dyn_table = lo_dyn_table ).
*     Get access to new table using field symbol.
      ASSIGN lo_dyn_table->* TO <fs_dyn_table>.
*     Create work area for new table.
      CREATE DATA lo_wa_dyn_table LIKE LINE OF <fs_dyn_table>.
*     Get access to new work area using field symbol.
      ASSIGN lo_wa_dyn_table->* TO <fs_wa_dyn_table>.

      IF lines( it_rows ) = lines( ct_stock_dwnld ).
        IF p_prod IS NOT INITIAL.
          LOOP AT ct_stock_dwnld ASSIGNING <fs_stock_dwnld>.
*           Populate PC dynamic download table.
            MOVE-CORRESPONDING <fs_stock_dwnld> TO <fs_wa_dyn_table>.
            ASSIGN COMPONENT 'COUNT_DATE_UI' OF STRUCTURE <fs_wa_dyn_table>
                          TO <fs_field>.
            IF sy-subrc = 0.
              CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                EXPORTING
                  date_internal            = <fs_stock_dwnld>-count_date_ui
                IMPORTING
                  date_external            = <fs_field>
                EXCEPTIONS
                  date_internal_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.
                CLEAR <fs_field>.
              ENDIF.
            ENDIF.
*           convert fields
            PERFORM convert_prod_xls USING <fs_stock_dwnld>
                                  CHANGING <fs_wa_dyn_table>.
            APPEND <fs_wa_dyn_table> TO <fs_dyn_table>.
          ENDLOOP.
        ELSE.
          LOOP AT ct_stock_dwnld ASSIGNING <fs_stock_dwnld>.
*           Populate PC dynamic download table.
            MOVE-CORRESPONDING <fs_stock_dwnld> TO <fs_wa_dyn_table>.
            ASSIGN COMPONENT 'COUNT_DATE_UI' OF STRUCTURE <fs_wa_dyn_table>
                          TO <fs_field>.
            IF sy-subrc = 0.
              CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
                EXPORTING
                  date_internal            = <fs_stock_dwnld>-count_date_ui
                IMPORTING
                  date_external            = <fs_field>
                EXCEPTIONS
                  date_internal_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.
                CLEAR <fs_field>.
              ENDIF.
            ENDIF.
            APPEND <fs_wa_dyn_table> TO <fs_dyn_table>.
          ENDLOOP.
        ENDIF.
      ELSE.

        LOOP AT it_rows ASSIGNING <fs_selected_line>.
*         read selected row from internal table gt_location_for_area
          READ TABLE ct_stock_dwnld ASSIGNING <fs_stock_dwnld>
               INDEX <fs_selected_line>-index.
          CHECK sy-subrc = 0.
*         Populate PC dynamic download table.
          MOVE-CORRESPONDING <fs_stock_dwnld> TO <fs_wa_dyn_table>.
          ASSIGN COMPONENT 'COUNT_DATE_UI' OF STRUCTURE <fs_wa_dyn_table>
                        TO <fs_field>.
          IF sy-subrc = 0.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = <fs_stock_dwnld>-count_date_ui
              IMPORTING
                date_external            = <fs_field>
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              CLEAR <fs_field>.
            ENDIF.
          ENDIF.
          IF p_prod IS NOT INITIAL.
*           convert fields
            PERFORM convert_prod_xls USING <fs_stock_dwnld>
                                  CHANGING <fs_wa_dyn_table>.
          ENDIF.
          APPEND <fs_wa_dyn_table> TO <fs_dyn_table>.
        ENDLOOP.
      ENDIF.
*     Populate column headings
      IF lv_file_exist IS INITIAL.
        LOOP AT it_fieldcat ASSIGNING <fs_fieldcatalog>.
          IF <fs_fieldcatalog>-scrtext_m IS NOT INITIAL.
            ls_fields-name = <fs_fieldcatalog>-scrtext_m.
          ELSEIF <fs_fieldcatalog>-scrtext_l IS NOT INITIAL.
            ls_fields-name = <fs_fieldcatalog>-scrtext_l.
          ELSEIF <fs_fieldcatalog>-scrtext_s IS NOT INITIAL.
            ls_fields-name = <fs_fieldcatalog>-scrtext_s.
          ELSEIF <fs_fieldcatalog>-reptext IS NOT INITIAL.
            ls_fields-name = <fs_fieldcatalog>-reptext.
          ELSEIF <fs_fieldcatalog>-rollname IS NOT INITIAL.
            ls_fields-name = <fs_fieldcatalog>-rollname.
          ENDIF.
          APPEND ls_fields TO lt_fields.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = iv_filename
          append                = p_app
          write_field_separator = abap_true
        TABLES
          data_tab              = <fs_dyn_table>
          fieldnames            = lt_fields
        EXCEPTIONS
          OTHERS                = 22.

    ELSEIF iv_filename+lv_offset(4) = gc_csv.

      PERFORM create_table_csv USING it_fieldcat
                                     it_rows
                                     ct_stock_dwnld
                                     lv_file_exist
                            CHANGING lt_stringtab.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = iv_filename
          append                = p_app
          write_field_separator = abap_true
        TABLES
          data_tab              = lt_stringtab
        EXCEPTIONS
          OTHERS                = 22.
    ELSE.
      MESSAGE e037 INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.
    IF sy-subrc <> 0.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ELSE.
      MESSAGE s035 WITH iv_filename INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
    ENDIF.

  ELSE.
    lv_phys_filename = iv_filename.
    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        client                     = sy-mandt
        logical_filename           = gc_lf_name
        parameter_1                = p_lgnum
        parameter_2                = p_cntr
      CHANGING
        physical_filename          = lv_phys_filename
      EXCEPTIONS
        logical_filename_not_found = 1
        validation_failed          = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      MESSAGE e008 WITH gc_lf_name lv_mess INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.
    CLEAR lv_file_exist.
    IF p_app IS NOT INITIAL.
*     check if exist
      OPEN DATASET lv_phys_filename
                   FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc <> 8.
        lv_file_exist = abap_true.
      ENDIF.
      CLOSE DATASET lv_phys_filename.
    ENDIF.
*   Application server download
    PERFORM create_table_csv USING it_fieldcat
                                   it_rows
                                   ct_stock_dwnld
                                   lv_file_exist
                          CHANGING lt_stringtab.

    IF p_app IS INITIAL.
      IF p_crea IS NOT INITIAL AND sy-batch IS INITIAL.
*       check if exist and ask for replace
        OPEN DATASET lv_phys_filename
                     FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc = 0.
          CLOSE DATASET lv_phys_filename.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = TEXT-009
              display_cancel_button = ' '
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            PERFORM add_bapiret CHANGING ct_bapiret.
            RETURN.
          ENDIF.
          IF lv_answer = 2.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      OPEN DATASET lv_phys_filename
                   FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
           MESSAGE lv_mess.
    ELSE.
      OPEN DATASET lv_phys_filename
                   FOR APPENDING IN TEXT MODE ENCODING DEFAULT
           MESSAGE lv_mess.
    ENDIF.
    IF sy-subrc <> 0.
      PERFORM add_bapiret CHANGING ct_bapiret.
      MESSAGE e008 WITH gc_lf_name lv_mess INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ENDIF.

    LOOP AT lt_stringtab ASSIGNING <fs_string>.
      TRANSFER <fs_string> TO lv_phys_filename.
      IF sy-subrc <> 0.
        MESSAGE e009 WITH gc_lf_name INTO lv_msg.
        PERFORM add_bapiret CHANGING ct_bapiret.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLOSE DATASET lv_phys_filename.
    IF sy-subrc <> 0.
      MESSAGE e010 WITH gc_lf_name INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
      RETURN.
    ELSE.
      MESSAGE s035 WITH gc_lf_name INTO lv_msg.
      PERFORM add_bapiret CHANGING ct_bapiret.
    ENDIF.
  ENDIF.
* was there an error?
  LOOP AT ct_bapiret TRANSPORTING NO FIELDS
    WHERE type CA wmegc_severity_eax.
    RETURN.
  ENDLOOP.

  IF sy-batch IS INITIAL.
    IF lines( it_rows ) = lines( ct_stock_dwnld ).
      CLEAR ct_stock_dwnld.
    ELSE.
      CLEAR lt_stock_dwnld.
      LOOP AT ct_stock_dwnld ASSIGNING <fs_stock_dwnld>.
        lv_tabix = sy-tabix.
        READ TABLE it_rows INDEX lv_tabix TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.
        APPEND <fs_stock_dwnld> TO lt_stock_dwnld.
      ENDLOOP.
      ct_stock_dwnld = lt_stock_dwnld.
    ENDIF.
  ELSE.
    CLEAR ct_stock_dwnld.
  ENDIF.

ENDFORM.                    "EXPORT_TO_FILE
*&---------------------------------------------------------------------*
*&      Form  create_table_csv
*&---------------------------------------------------------------------*
FORM create_table_csv USING it_fieldcat    TYPE lvc_t_fcat
                            it_rows        TYPE lvc_t_row
                            it_stock_dwnld TYPE tyt_stock_dwnld
                            iv_prc_result  TYPE abap_bool
                   CHANGING ct_stringtab   TYPE stringtab.

  DATA:
    lv_row_index    TYPE lvc_index,
    lv_counter      TYPE i,
    lv_value        TYPE string,
    lv_cnt_date(10) TYPE c,
    lv_name         TYPE aqadef-fname,
    ls_string       TYPE string.

  FIELD-SYMBOLS:
    <fs_selected_line> TYPE lvc_s_row,
    <fs_stock_dwnld>   TYPE /scwm/s_pi_stock_dwnld,
    <ls_fieldcatalog>  TYPE lvc_s_fcat,
    <fs_field>         TYPE any.

  CLEAR: ct_stringtab.
* Populate column headings
  IF iv_prc_result IS INITIAL.
    LOOP AT it_fieldcat ASSIGNING <ls_fieldcatalog>.
*     get description
      IF <ls_fieldcatalog>-scrtext_m IS NOT INITIAL.
        lv_name = <ls_fieldcatalog>-scrtext_m.
      ELSEIF <ls_fieldcatalog>-scrtext_l IS NOT INITIAL.
        lv_name = <ls_fieldcatalog>-scrtext_l.
      ELSEIF <ls_fieldcatalog>-scrtext_s IS NOT INITIAL.
        lv_name = <ls_fieldcatalog>-scrtext_s.
      ELSEIF <ls_fieldcatalog>-reptext IS NOT INITIAL.
        lv_name = <ls_fieldcatalog>-reptext.
      ELSEIF <ls_fieldcatalog>-rollname IS NOT INITIAL.
        lv_name = <ls_fieldcatalog>-rollname.
      ENDIF.

      lv_counter = lv_counter + 1.
      IF lv_counter = 1.
        ls_string = lv_name.
      ELSE.
        CONCATENATE ls_string ';' lv_name INTO ls_string.
      ENDIF.
    ENDLOOP.
    CONCATENATE ls_string ';' INTO ls_string.
    APPEND ls_string TO ct_stringtab.
  ENDIF.
  IF sy-batch IS INITIAL.
    LOOP AT it_rows ASSIGNING <fs_selected_line>.
      lv_row_index = <fs_selected_line>-index.
*     read selected row from internal table gt_location_for_area
      READ TABLE it_stock_dwnld INDEX lv_row_index
       ASSIGNING <fs_stock_dwnld>.
      CHECK sy-subrc = 0.
      CLEAR: lv_counter, ls_string.
*     Populate column data
      LOOP AT it_fieldcat ASSIGNING <ls_fieldcatalog>.
        lv_counter = lv_counter + 1.
        ASSIGN COMPONENT <ls_fieldcatalog>-fieldname
            OF STRUCTURE <fs_stock_dwnld> TO <fs_field>.
        CHECK sy-subrc = 0.
        IF <fs_field> IS INITIAL.
          ASSIGN space TO <fs_field>.
        ENDIF.
        IF lv_counter = 1.
          ls_string = <fs_field>.
        ELSE.
          IF ( <ls_fieldcatalog>-fieldname = 'VFDAT' OR
               <ls_fieldcatalog>-fieldname = 'UI_WDATU' OR
               <ls_fieldcatalog>-fieldname = 'COUNT_DATE_UI' ) AND
             <fs_field> IS NOT INITIAL.
            CLEAR lv_cnt_date.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = <fs_field>
              IMPORTING
                date_external            = lv_cnt_date
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              CLEAR lv_cnt_date.
            ENDIF.
            lv_value = lv_cnt_date.
          ELSE.
            lv_value = <fs_field>.
          ENDIF.
          CONCATENATE ls_string ';' lv_value INTO ls_string.
        ENDIF.
      ENDLOOP.
      CONCATENATE ls_string ';' INTO ls_string.
      APPEND ls_string TO ct_stringtab.
    ENDLOOP.
  ELSE.
    LOOP AT it_stock_dwnld ASSIGNING <fs_stock_dwnld>.
      CLEAR: lv_counter, ls_string.
*     Populate column data
      LOOP AT it_fieldcat ASSIGNING <ls_fieldcatalog>.
        lv_counter = lv_counter + 1.
        ASSIGN COMPONENT <ls_fieldcatalog>-fieldname
            OF STRUCTURE <fs_stock_dwnld> TO <fs_field>.
        CHECK sy-subrc = 0.
        IF <fs_field> IS INITIAL.
          ASSIGN space TO <fs_field>.
        ENDIF.
        IF lv_counter = 1.
          ls_string = <fs_field>.
        ELSE.
          IF ( <ls_fieldcatalog>-fieldname = 'VFDAT' OR
               <ls_fieldcatalog>-fieldname = 'UI_WDATU' OR
               <ls_fieldcatalog>-fieldname = 'COUNT_DATE_UI' ) AND
             <fs_field> IS NOT INITIAL.
            CLEAR lv_cnt_date.
            CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
              EXPORTING
                date_internal            = <fs_field>
              IMPORTING
                date_external            = lv_cnt_date
              EXCEPTIONS
                date_internal_is_invalid = 1
                OTHERS                   = 2.
            IF sy-subrc <> 0.
              CLEAR lv_cnt_date.
            ENDIF.
            lv_value = lv_cnt_date.
          ELSE.
            lv_value = <fs_field>.
          ENDIF.
          CONCATENATE ls_string ';' lv_value INTO ls_string.
        ENDIF.
      ENDLOOP.
      CONCATENATE ls_string ';' INTO ls_string.
      APPEND ls_string TO ct_stringtab.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " create_table_csv
*&---------------------------------------------------------------------*
*&      Form  convert_prod_xls
*&---------------------------------------------------------------------*
FORM convert_prod_xls USING is_stock_dwnld  TYPE /scwm/s_pi_stock_dwnld
              CHANGING cs_wa_dyn_table TYPE any.

  FIELD-SYMBOLS:
    <fs_field> TYPE any.

  ASSIGN COMPONENT 'QUAN' OF STRUCTURE cs_wa_dyn_table
                TO <fs_field>.
  IF sy-subrc = 0.
    WRITE is_stock_dwnld-quan
       TO <fs_field> UNIT is_stock_dwnld-unit.
    IF is_stock_dwnld-quan IS INITIAL.
      CLEAR <fs_field>.
    ENDIF.
  ENDIF.
  ASSIGN COMPONENT 'VALUE' OF STRUCTURE cs_wa_dyn_table
                TO <fs_field>.
  IF sy-subrc = 0.
    WRITE is_stock_dwnld-value
       TO <fs_field> CURRENCY is_stock_dwnld-waers.
    IF is_stock_dwnld-value IS INITIAL.
      CLEAR <fs_field>.
    ENDIF.
  ENDIF.
  ASSIGN COMPONENT 'VERPR' OF STRUCTURE cs_wa_dyn_table
                TO <fs_field>.
  IF sy-subrc = 0.
    WRITE is_stock_dwnld-verpr
       TO <fs_field> CURRENCY is_stock_dwnld-waers.
    IF is_stock_dwnld-verpr IS INITIAL.
      CLEAR <fs_field>.
    ENDIF.
  ENDIF.
  ASSIGN COMPONENT 'ENTERED_QUANTITY' OF STRUCTURE cs_wa_dyn_table
                TO <fs_field>.
  IF sy-subrc = 0.
    CLEAR <fs_field>.
  ENDIF.
  IF is_stock_dwnld-vfdat IS NOT INITIAL.
    ASSIGN COMPONENT 'VFDAT' OF STRUCTURE cs_wa_dyn_table
                  TO <fs_field>.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal            = is_stock_dwnld-vfdat
        IMPORTING
          date_external            = <fs_field>
        EXCEPTIONS
          date_internal_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
        CLEAR <fs_field>.
      ENDIF.
    ENDIF.
  ENDIF.
  IF is_stock_dwnld-ui_wdatu IS NOT INITIAL.
    ASSIGN COMPONENT 'UI_WDATU' OF STRUCTURE cs_wa_dyn_table
                  TO <fs_field>.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal            = is_stock_dwnld-ui_wdatu
        IMPORTING
          date_external            = <fs_field>
        EXCEPTIONS
          date_internal_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
        CLEAR <fs_field>.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " convert_prod_xls

*&---------------------------------------------------------------------*
*&      Form  write_application_log
*&---------------------------------------------------------------------*
FORM write_application_log.
  DATA:
    ls_log_act    TYPE /scwm/log_act,
    lv_create_log TYPE boole_d,
    ls_log        TYPE bal_s_log,
    lo_log        TYPE REF TO /scwm/cl_log.

  IF gt_bapiret IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
    EXPORTING
      iv_lgnum     = p_lgnum
      iv_subobject = wmegc_apl_subob_pi
    IMPORTING
      es_log_act   = ls_log_act
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    "loggin is not activated for PI, no log is written
    RETURN.
  ENDIF.

  CREATE OBJECT lo_log
    EXPORTING
      iv_lgnum     = p_lgnum
      iv_balobj    = wmegc_apl_object_wme
      iv_balsubobj = wmegc_apl_subob_pi.
  lo_log->add_log( EXPORTING it_prot = gt_bapiret ).

  "determine if the log is written based on the set logging level
  CASE ls_log_act-actglob.
    WHEN wmegc_log_vip.
      IF lo_log->get_severity( ) CA 'A'.
        lv_create_log = 'X'.
      ENDIF.
    WHEN wmegc_log_imp.
      IF lo_log->get_severity( ) CA 'AE'.
        lv_create_log = 'X'.
      ENDIF.
    WHEN wmegc_log_med.
      IF lo_log->get_severity( ) CA 'AEW'.
        lv_create_log = 'X'.
      ENDIF.
    WHEN wmegc_log_add.
      IF lo_log->get_severity( ) CA 'AEWIS'.
        lv_create_log = 'X'.
      ENDIF.
    WHEN OTHERS.
      CLEAR: lv_create_log. " do not write a log
  ENDCASE.

  IF lv_create_log = abap_true.
    CALL FUNCTION '/SCWM/APP_LOG_EXPIRY_DATE_DET'
      EXPORTING
        is_log_act = ls_log_act
      CHANGING
        cs_log     = ls_log.

    ls_log-object = wmegc_apl_object_wme.
    ls_log-subobject = wmegc_apl_subob_pi.
    ls_log-extnumber = ls_log_act-lgnum.
    ls_log-alprog = sy-repid.

    lo_log->create_log( EXPORTING is_log = ls_log ).
    lo_log->convert_bapiret2applog( ).
    TRY.
        lo_log->save_applog2db( ).
      CATCH /scwm/cx_basics ##no_handler.
        "in case the application log can't be written there is no application log
    ENDTRY.
  ENDIF.
ENDFORM.                    "write_application_log
