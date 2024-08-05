*&---------------------------------------------------------------------*
*& Report  /SCWM/R_PI_FILEUPLD
*&
*&---------------------------------------------------------------------*
INCLUDE /scwm/r_pi_fileupldtop        .  " global Data
INCLUDE /scwm/r_pi_fileupldo01        .  " PBO-Modules
INCLUDE /scwm/r_pi_fileupldi01        .  " PAI-Modules
INCLUDE /scwm/r_pi_fileupldmenu       .  " Menu class & methods
INCLUDE /scwm/r_pi_fileupldf01        .  " FORM-Routines(general)
INCLUDE z_pi_fileupldnocnt.
INCLUDE /scwm/r_pi_fileupldcount      .  " With Count-Routines
INCLUDE /scwm/r_pi_fileupldchecks     .  " Data checks
*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.

  GET PARAMETER ID '/SCWM/LGN' FIELD p_lgnum.
* prepare work. Create objects
  CREATE OBJECT go_stock_id.
  CREATE OBJECT go_stock_fields.
  CALL FUNCTION '/SCWM/GET_PI_CUST_INSTANCE'
    IMPORTING
      eo_pi_cust = go_scwm_pi_cust.
  CALL FUNCTION '/LIME/PI_GET_CUST_INSTANCE'
    IMPORTING
      e_if_pi_cust = go_lime_cust.
* create fieldcat
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = gc_stock_upload
      i_client_never_display = abap_true
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = gc_insptyp
      state         = 'A'
    TABLES
      dd07v_tab     = gt_domvalues
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_LFNAME'.
        screen-input  = 0.
        MODIFY SCREEN.
      WHEN 'P_PATH'.
        IF p_pcfile IS NOT INITIAL.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'P_CNTR'.
        IF p_pcfile IS NOT INITIAL.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      WHEN 'P_CRDATE'.
        IF p_pcfile IS NOT INITIAL.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
*----------------------------------------------------------------------*
* Process on selection screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_lgnum.
* Check warehouse number
  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
    EXPORTING
      iv_lgnum  = p_lgnum
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH p_lgnum. "Enter a valid Warehouse number
  ENDIF.
  SET PARAMETER ID '/SCWM/LGN' FIELD p_lgnum.
*----------------------------------------------------------------------*
* Process for file selection
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.

  PERFORM select_directory CHANGING p_path.
*----------------------------------------------------------------------*
* Main program
*----------------------------------------------------------------------*
START-OF-SELECTION.

* AUTHORITY CHECK
  AUTHORITY-CHECK OBJECT '/SCWM/PIPR'
           ID '/SCWM/LGNU' FIELD p_lgnum
           ID '/SCWM/AREA' DUMMY
           ID '/SCWM/ENTL' DUMMY
           ID 'ACTVT'      FIELD '03'.

  IF sy-subrc <> 0.
    MESSAGE s154(/scwm/pi_appl) WITH p_lgnum DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
* Check warehouse number
  CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
    EXPORTING
      iv_lgnum  = p_lgnum
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc NE 0.
    MESSAGE s001 WITH p_lgnum DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
* determine timezone
  CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
    EXPORTING
      iv_lgnum        = p_lgnum
    IMPORTING
      ev_tzone        = gv_timezone
    EXCEPTIONS
      interface_error = 1
      data_not_found  = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE e107(/scwm/pi_appl) WITH p_lgnum DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF p_pcfile IS NOT INITIAL AND
     p_path   IS INITIAL.
    MESSAGE s016 DISPLAY LIKE 'E'. "Enter filename to upload
    RETURN.
  ENDIF.

  /scwm/cl_tm=>cleanup(
    EXPORTING
      iv_lgnum = p_lgnum ).

  CLEAR: gt_bapiret, gt_pi_head,
         gt_pi_items, gt_stock_for_area.
  PERFORM read_file_data USING gt_fieldcat
                      CHANGING gt_bapiret
                               gt_pi_head
                               gt_pi_items
                               gt_stock_for_area.

  IF sy-batch IS NOT INITIAL.
*   AUTHORITY CHECK
    AUTHORITY-CHECK OBJECT '/SCWM/PIPR'
             ID '/SCWM/LGNU' FIELD p_lgnum
             ID '/SCWM/AREA' DUMMY
             ID '/SCWM/ENTL' DUMMY
             ID 'ACTVT'      FIELD '01'.
    IF sy-subrc <> 0.
      MESSAGE e153(/scwm/pi_appl) WITH p_lgnum.
      RETURN.
    ENDIF.
*   to send the app. log to spool or display only
*   display protocol?
    READ TABLE gt_pi_items TRANSPORTING NO FIELDS
      WITH KEY status = icon_red_light.
    IF sy-subrc = 0.
      PERFORM create_log CHANGING go_log.
      PERFORM display_log USING go_log
                                gt_bapiret.
      RETURN.
    ENDIF.
    IF p_nocnt = abap_true.
      PERFORM create_inv_docs USING gr_alvgrid_head
                                    gt_stock_for_area
                           CHANGING gt_pi_items
                                    gt_bapiret.
    ELSE.
      PERFORM create_inv_docs_with_count USING gr_alvgrid_head
                                               gt_stock_for_area
                                      CHANGING gt_pi_items
                                               gt_pi_head
                                               gt_bapiret.
    ENDIF.
  ELSE.

    PERFORM initialize_alv USING gt_fieldcat
                        CHANGING gr_alvgrid_head
                                 gr_alvgrid_item
                                 gt_fieldcat_head
                                 gt_fieldcat_count
                                 gt_pi_head
                                 gt_pi_items
                                 gt_pi_count_data.
*   to display protocol?
    READ TABLE gt_pi_items TRANSPORTING NO FIELDS
      WITH KEY status = icon_red_light.
    IF sy-subrc = 0 OR gt_pi_items IS INITIAL.
      PERFORM create_log CHANGING go_log.
      PERFORM display_log USING go_log
                                gt_bapiret.
    ENDIF.
    CALL SCREEN 100.
  ENDIF.
