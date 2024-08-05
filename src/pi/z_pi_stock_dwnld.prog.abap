*&---------------------------------------------------------------------*
*& Report  /SCWM/R_PI_STOCK_DWNLD
*&
*&---------------------------------------------------------------------*
INCLUDE z_pi_stock_dwnldtop.
INCLUDE /scwm/r_pi_stock_dwnldo01          .  " PBO-Modules
INCLUDE /scwm/r_pi_stock_dwnldi01          .  " PAI-Modules
INCLUDE /scwm/r_pi_stock_dwnldmenu         .  " Menu class & methods
INCLUDE z_pi_stock_dwnldf01.
*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.

  GET PARAMETER ID '/SCWM/LGN' FIELD p_lgnum.

  CREATE OBJECT go_stock_fields.
  CALL FUNCTION '/LIME/PI_GET_CUST_INSTANCE'
    IMPORTING
      e_if_pi_cust = go_cust_lime.
  CALL FUNCTION '/SCWM/GET_PI_CUST_INSTANCE'
    IMPORTING
      eo_pi_cust = go_cust_scwm.
* Get default reason
  TRY.
      IF p_loc IS NOT INITIAL AND
         p_lgnum IS NOT INITIAL.
        p_rea = go_cust_scwm->get_default_reason(
          i_lgnum    = p_lgnum
          i_doc_type = gc_doc_type_el ).
      ELSEIF p_prod IS NOT INITIAL AND
             p_lgnum IS NOT INITIAL.
        p_rea = go_cust_scwm->get_default_reason(
          i_lgnum    = p_lgnum
          i_doc_type = gc_doc_type_es ).
      ENDIF.
    CATCH /scwm/cx_pi_app.                              "#EC NO_HANDLER
  ENDTRY.
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
      WHEN OTHERS.
    ENDCASE.
    IF screen-group1 = 'ID1'.
      IF p_loc IS NOT INITIAL.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
* Get default reason
  IF p_rea   IS INITIAL AND
     p_lgnum IS NOT INITIAL.
    TRY.
        IF p_loc IS NOT INITIAL.
          p_rea = go_cust_scwm->get_default_reason(
            i_lgnum    = p_lgnum
            i_doc_type = gc_doc_type_el ).
        ELSEIF p_prod IS NOT INITIAL.
          p_rea = go_cust_scwm->get_default_reason(
            i_lgnum    = p_lgnum
            i_doc_type = gc_doc_type_es ).
        ENDIF.
      CATCH /scwm/cx_pi_app.                            "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
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
* Process on selection screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_rea.
* Check reason
  IF p_rea   IS NOT INITIAL AND
     p_lgnum IS NOT INITIAL.
    TRY.
        go_cust_scwm->get_reason( i_lgnum  = p_lgnum
                                  i_reason = p_rea ).
      CATCH /scwm/cx_pi_app.                            "#EC NO_HANDLER
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ENDIF.
*----------------------------------------------------------------------*
* Process for directory selection
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
           ID 'ACTVT'      FIELD '01'.

  IF sy-subrc <> 0.
    MESSAGE s154(/scwm/pi_appl)
       WITH p_lgnum DISPLAY LIKE 'E'.
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

  IF p_pcfile = abap_true AND p_path IS INITIAL.
    MESSAGE s005 DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF p_asfile = abap_true AND p_lfname IS INITIAL.
    RETURN.
  ENDIF.

  /scwm/cl_tm=>cleanup(
    EXPORTING
      iv_lgnum = p_lgnum ).
  CLEAR: gt_bapiret, gt_stock_dwnld, go_alvgrid, gt_fieldcat.
* create fieldcat
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = gc_stock_dwnld
      i_client_never_display = abap_true
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
* modify fieldcat
  IF p_loc IS NOT INITIAL.
    PERFORM hide_product CHANGING gt_fieldcat.
  ELSE.
    PERFORM hide_location CHANGING gt_fieldcat.
  ENDIF.
  PERFORM execute_sel USING s_area[]
                            s_lpla[]
                            gt_fieldcat
                   CHANGING gt_stock_dwnld
                            gt_bapiret.
* to send the app. log to spool or display only
  IF gt_bapiret IS NOT INITIAL.
    LOOP AT gt_bapiret TRANSPORTING NO FIELDS
      WHERE type CA wmegc_severity_eax.
      PERFORM create_log CHANGING go_log.
      PERFORM display_log USING go_log
                                gt_bapiret.
      EXIT.
    ENDLOOP.
  ENDIF.
  IF sy-batch IS INITIAL.
    CALL SCREEN 100.
  ENDIF.
