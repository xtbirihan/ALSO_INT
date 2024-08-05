**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/R_PI_FILEUPLDNOCNT
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  get_selected_items
*&--------------------------------------------------------------------*
*      Gets the selected rows in case of online mode and processes all
*      the records in background mode.
*---------------------------------------------------------------------*

FORM get_selected_items
     USING it_pi_items       TYPE tyt_pi_items
           ir_alvgrid_head   TYPE REF TO cl_gui_alv_grid
           it_stock_for_area TYPE /scwm/t_pi_stock_for_area
  CHANGING ct_item           TYPE /lime/pi_t_item_create
           ct_bapiret        TYPE bapirettab.

  DATA:
    lv_msg          TYPE bapiret2-message,                  "#EC NEEDED
    lv_tabix        TYPE sy-tabix,
    lv_item_counter TYPE /lime/line_item_id,
    ls_item_head    TYPE /lime/pi_item_create,
    lt_rows         TYPE lvc_t_row.

  FIELD-SYMBOLS:
    <ls_selected_line> TYPE lvc_s_row,
    <ls_pi_items>      TYPE /scwm/s_pi_stock_dwnld.

* Clear item number - inv. create
  CLEAR: ct_item.

  IF sy-batch = abap_true.
    LOOP AT it_pi_items ASSIGNING <ls_pi_items>.

      IF <ls_pi_items>-status <> icon_green_light.
        MESSAGE e140(/scwm/pi_appl) INTO lv_msg.
        PERFORM add_messages USING sy-tabix 'STATUS' space space
                          CHANGING ct_bapiret.
        CLEAR ct_item.
        EXIT.
      ENDIF.

      lv_tabix = sy-tabix.
      CLEAR ls_item_head.
      lv_item_counter = lv_item_counter + 1.
      PERFORM fill_data_create_head USING lv_tabix
                                          <ls_pi_items>
                                          lv_item_counter
                                          it_stock_for_area
                                 CHANGING ls_item_head
                                          ct_bapiret.
      IF ls_item_head IS NOT INITIAL.
        APPEND ls_item_head TO ct_item.
      ELSE.
        CLEAR ct_item.
        EXIT.
      ENDIF.
    ENDLOOP.

  ELSE.
    ir_alvgrid_head->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows               " Indexes of Selected Rows
    ).

    LOOP AT lt_rows ASSIGNING <ls_selected_line>.
      lv_tabix = <ls_selected_line>-index.
*     read selected row from internal table gt_pi_items
      READ TABLE it_pi_items INDEX lv_tabix
            ASSIGNING <ls_pi_items>.
      CHECK sy-subrc = 0.
      IF <ls_pi_items>-status <> icon_green_light.
        MESSAGE e140(/scwm/pi_appl) INTO lv_msg.
        PERFORM add_messages USING sy-tabix 'STATUS' space space
                          CHANGING ct_bapiret.
        CLEAR ct_item.
        EXIT.
      ENDIF.

      CLEAR ls_item_head.
      lv_item_counter = lv_item_counter + 1.
      PERFORM fill_data_create_head USING lv_tabix
                                          <ls_pi_items>
                                          lv_item_counter
                                          it_stock_for_area
                                 CHANGING ls_item_head
                                          ct_bapiret.
      IF ls_item_head IS NOT INITIAL.
        APPEND ls_item_head TO ct_item.
      ELSE.
        CLEAR ct_item.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "get_selected_items
*&--------------------------------------------------------------------*
*&      Form  check_file_data_head
*&--------------------------------------------------------------------*
*       Checks the uploaded data.
*---------------------------------------------------------------------*
FORM check_file_data_head
     USING iv_tabix          TYPE sy-tabix
           it_stock_for_area TYPE /scwm/t_pi_stock_for_area
  CHANGING ct_bapiret        TYPE bapirettab
           cs_pi_items       TYPE /scwm/s_pi_stock_dwnld.
  DATA:
    lv_msg        TYPE bapiret2-message,                    "#EC NEEDED
    lv_var1       TYPE sy-msgv1,
    lv_var2       TYPE sy-msgv2,
    lv_rejected   TYPE abap_bool,
    lv_category   TYPE /lime/pi_category,
    lv_count_date TYPE /lime/pi_count_date,                 "#EC NEEDED
    lv_doc_type   TYPE /lime/pi_document_type,
    lv_timestamp  TYPE tzntstmpl,
    lv_ui_date    TYPE sydate,
    ls_lagp       TYPE /scwm/lagp,
    lo_pi_selects TYPE REF TO /lime/if_pi_selects.

  CALL FUNCTION '/LIME/PI_GET_TOOLS_INSTANCE'
    IMPORTING
      e_if_pi_selects = lo_pi_selects.
  cs_pi_items-status = icon_green_light.
  IF cs_pi_items-matnr IS NOT INITIAL.
    lv_doc_type = gc_doc_type_es. "Object based
  ELSE.
    lv_doc_type = gc_doc_type_el. "Location based
  ENDIF.
  GET TIME STAMP FIELD lv_timestamp.
* Check document type
  TRY.
      lo_pi_selects->get_pi_doctype(
        EXPORTING
          iv_doc_type = lv_doc_type                 " Physical Inventory Procedure (Document Type of Phys. Inv.)
        IMPORTING
          ev_category = lv_category                 " Physical Inventory Process Category
      ).
    CATCH /lime/cx_pi_app.
      cs_pi_items-status = icon_red_light.
      MESSAGE e027 WITH lv_doc_type INTO lv_msg.
      MOVE lv_doc_type TO lv_var1.
      PERFORM add_messages USING iv_tabix 'DOC_TYPE' lv_var1 space
                        CHANGING ct_bapiret.
      RETURN.
  ENDTRY.

  CASE lv_category.
    WHEN limpi_loc.
      IF cs_pi_items-matnr IS NOT INITIAL.
        cs_pi_items-status = icon_red_light.
        MESSAGE e019(/scwm/pi_appl) WITH iv_tabix INTO lv_msg.
        WRITE iv_tabix TO lv_var1 LEFT-JUSTIFIED.
        PERFORM add_messages USING iv_tabix 'DOC_TYPE' lv_var1 space
                          CHANGING ct_bapiret.
        RETURN.
      ENDIF.
    WHEN limpi_object.
      IF cs_pi_items-matnr IS INITIAL.
        cs_pi_items-status = icon_red_light.
        MESSAGE e019(/scwm/pi_appl) WITH iv_tabix INTO lv_msg.
        WRITE iv_tabix TO lv_var1 LEFT-JUSTIFIED.
        PERFORM add_messages USING iv_tabix 'DOC_TYPE' lv_var1 space
                          CHANGING ct_bapiret.
        RETURN.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.
* reason from file or default if initial
  TRY.
      IF cs_pi_items-reason IS INITIAL.
        cs_pi_items-reason =
          go_scwm_pi_cust->get_default_reason(
          i_lgnum    = p_lgnum
          i_doc_type = lv_doc_type ).
      ELSE.
        go_scwm_pi_cust->get_reason(
          i_lgnum  = p_lgnum
          i_reason = cs_pi_items-reason ).
      ENDIF.
    CATCH /scwm/cx_pi_app.
      cs_pi_items-status = icon_red_light.
      MESSAGE e018(/scwm/ui_pi) WITH iv_tabix INTO lv_msg.
      WRITE iv_tabix TO lv_var1 LEFT-JUSTIFIED.
      PERFORM add_messages USING iv_tabix 'REASON' lv_var1 space
                        CHANGING ct_bapiret.
      RETURN.
  ENDTRY.
* check count user
  IF p_cnt                  IS NOT INITIAL AND
    cs_pi_items-count_user  IS INITIAL.
    cs_pi_items-status = icon_red_light.
    MESSAGE e032(/scwm/pi_appl) INTO lv_msg.
    WRITE iv_tabix TO lv_var1 LEFT-JUSTIFIED.
    PERFORM add_messages USING iv_tabix 'COUNT_USER' lv_var1 space
                      CHANGING ct_bapiret.
    RETURN.
  ENDIF.
* check or fill planned count date
  CONVERT TIME STAMP lv_timestamp TIME ZONE gv_timezone
           INTO DATE lv_ui_date.
  IF cs_pi_items-count_date_ui IS INITIAL.
    IF p_nocnt IS NOT INITIAL.
      cs_pi_items-count_date_ui = lv_ui_date.
    ELSEIF p_cnt IS NOT INITIAL.
      cs_pi_items-status = icon_red_light.
      MESSAGE e026(/scwm/pi_appl) INTO lv_msg.
      PERFORM add_messages USING iv_tabix 'COUNT_DATE' space space
                        CHANGING ct_bapiret.
      RETURN.
    ENDIF.
  ENDIF.
  IF cs_pi_items-count_time_ui IS INITIAL.
    IF p_nocnt IS NOT INITIAL.
      cs_pi_items-count_time_ui = limpi_end_time.
    ENDIF.
  ENDIF.
* test convertion
  CONVERT DATE cs_pi_items-count_date_ui
        TIME cs_pi_items-count_time_ui
   INTO TIME STAMP lv_count_date
   TIME ZONE gv_timezone.
  IF sy-subrc <> 0.
    cs_pi_items-status = icon_red_light.
    MESSAGE e026(/scwm/pi_appl) INTO lv_msg.
    PERFORM add_messages USING iv_tabix 'COUNT_DATE' space space
                      CHANGING ct_bapiret.
    RETURN.
  ELSE.
    IF p_cnt IS NOT INITIAL AND lv_count_date > lv_timestamp.
      cs_pi_items-status = icon_red_light.
      MESSAGE e058(/scwm/pi_appl) INTO lv_msg.
      lv_var1 = cs_pi_items-count_date_ui.
      lv_var2 = cs_pi_items-count_time_ui.
      PERFORM add_messages USING iv_tabix 'COUNT_DATE' lv_var1 lv_var2
                        CHANGING ct_bapiret.
    ENDIF.
  ENDIF.
* Loc_parent is filled for location- and object-related PI
* check storage bin
  CALL FUNCTION '/SCWM/LAGP_READ_SINGLE'
    EXPORTING
      iv_lgnum      = p_lgnum
      iv_lgpla      = cs_pi_items-lgpla
    IMPORTING
      es_lagp       = ls_lagp
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      enqueue_error = 3
      OTHERS        = 4.
  IF sy-subrc <> 0.
    cs_pi_items-status = icon_red_light.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO lv_msg.
    PERFORM add_messages USING iv_tabix 'LGPLA' sy-msgv1 sy-msgv2
                      CHANGING ct_bapiret.
    RETURN.
  ELSE.
    cs_pi_items-lgtyp = ls_lagp-lgtyp.
  ENDIF.
* Stock data for object-related PI
  IF lv_category = limpi_object.
*   check product field entries
    PERFORM check_product_fields USING iv_tabix
                                       cs_pi_items
                                       it_stock_for_area
                              CHANGING ct_bapiret
                                       lv_rejected.
  ENDIF.
  IF lv_rejected IS NOT INITIAL.
    cs_pi_items-status = icon_red_light.
    RETURN.
  ENDIF.

ENDFORM.                    "check_file_data_head
*&--------------------------------------------------------------------*
*&      Form  fill_data_create_head
*&--------------------------------------------------------------------*
*       Checks the uploaded data.
*---------------------------------------------------------------------*
FORM fill_data_create_head
     USING iv_tabix          TYPE sytabix
           is_pi_items       TYPE /scwm/s_pi_stock_dwnld
           iv_item_counter   TYPE /lime/line_item_id
           it_stock_for_area TYPE /scwm/t_pi_stock_for_area
  CHANGING cs_item           TYPE /lime/pi_item_create
           ct_bapiret        TYPE bapirettab.

  DATA:
    lv_msg       TYPE bapiret2-message,                     "#EC NEEDED
    lv_category  TYPE /lime/pi_category,
    ls_reference TYPE /lime/pi_reference.

  CLEAR: cs_item.
  cs_item-data-item_no = iv_item_counter.
* fill type parent & type item
  PERFORM fill_type_parent_item USING is_pi_items
                             CHANGING cs_item-data-type_parent
                                      cs_item-data-type_item.
* Selection parameter...
  IF is_pi_items-matnr IS NOT INITIAL.
    cs_item-data-doc_type = gc_doc_type_es. "Object based
  ELSE.
    cs_item-data-doc_type = gc_doc_type_el. "Location based
  ENDIF.
* Get document category
  TRY.
      lv_category = go_lime_cust->get_pi_category(
        doc_type = cs_item-data-doc_type ).
    CATCH /lime/cx_pi.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_msg.
      PERFORM add_messages USING iv_tabix 'DOC_TYPE' sy-msgv1 sy-msgv2
                        CHANGING ct_bapiret.
      CLEAR cs_item.
      RETURN.
  ENDTRY.

  cs_item-data-block_ind  = is_pi_items-block_ind.
  cs_item-data-freeze_ind = is_pi_items-freeze_ind.
  cs_item-data-priority   = is_pi_items-priority.
  cs_item-data-reason     = is_pi_items-reason.

  cs_item-data-zz_huident = is_pi_items-zz_huident.
  cs_item-data-zz_mfrnr = is_pi_items-zz_mfrnr.
  cs_item-data-zz_mfrpn = is_pi_items-zz_mfrpn.

* Set create ACTIVE - INACTIVE doc if create only
  IF p_nocnt = abap_true.
    IF is_pi_items-active IS INITIAL.
      cs_item-inactive = abap_true.
      cs_item-active   = abap_false.
    ELSE.
      cs_item-active   = abap_true.
      cs_item-inactive = abap_false.
    ENDIF.
  ENDIF.

  CONVERT DATE is_pi_items-count_date_ui
          TIME is_pi_items-count_time_ui
     INTO TIME STAMP cs_item-data-count_date
     TIME ZONE gv_timezone.
  IF sy-subrc <> 0.
    MESSAGE e026(/scwm/pi_appl) INTO lv_msg.
    PERFORM add_messages USING iv_tabix 'COUNT_DATE' space space
                      CHANGING ct_bapiret.
    CLEAR cs_item.
    RETURN.
  ENDIF.
* Loc_parent is filled for location- and object-related PI
  cs_item-data-loc_parent-lgnum = is_pi_items-lgnum.
  cs_item-data-loc_parent-lgtyp = is_pi_items-lgtyp.
  cs_item-data-loc_parent-lgpla = is_pi_items-lgpla.
* Stock data for object-related PI
  IF lv_category = limpi_object.

    PERFORM fill_stock_data USING is_pi_items
                                  it_stock_for_area
                         CHANGING cs_item-data-stock_item
                                  cs_item-data-insptyp
                                  cs_item-data-idplate
                                  cs_item-data-qdoccat
                                  cs_item-data-wdatu
                                  cs_item-data-vfdat
                                  cs_item-data-coo
                                  cs_item-data-inspid
                                  cs_item-data-qdocid
                                  cs_item-data-qitmid.

  ENDIF.
* fill refenrence if delievered
  IF is_pi_items-ext_doc_number IS NOT INITIAL.
    ls_reference-ref_doc_id   = is_pi_items-ext_doc_number.
    ls_reference-ref_doc_type = 'SCWM_PIEXT'.
    APPEND ls_reference      TO cs_item-t_reference.
  ENDIF.

ENDFORM.                    "fill_data_create_head
