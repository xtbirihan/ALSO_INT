*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_004_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  REFRESH gt_data.
  SELECT a~guid
         a~datum
         a~username
         a~statu
         b~description
         FROM zint_000_t_003 AS a
         INNER JOIN zint_000_t_004 AS b
         ON a~statu EQ b~statu
         INTO CORRESPONDING FIELDS OF TABLE gt_data
         WHERE a~guid IN s_guid
           AND a~int_type EQ '13'
           AND a~int_subtype EQ '01'
           AND a~datum IN s_datum
           AND a~statu EQ '22'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  DATA: lt_fldcat  TYPE slis_t_fieldcat_alv,
        ls_variant TYPE disvariant,
        ls_layout  TYPE slis_layout_alv,
        ls_fldcat  TYPE slis_fieldcat_alv.
  DATA: ok_code TYPE sy-ucomm.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'GS_DATA'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = lt_fldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK lt_fldcat[] IS NOT INITIAL.

  LOOP AT lt_fldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'SEL'.
        <fs_fcat>-tech = abap_true.
    ENDCASE.
  ENDLOOP.

  ls_layout-zebra             = 'X'.
  ls_layout-box_fieldname     = 'SEL'.
  ls_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer       = 'X'
      i_save                   = 'A'
      i_callback_program       = sy-repid
      is_variant               = Ls_variant
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = Ls_layout
      it_fieldcat              = Lt_fldcat[]
    TABLES
      t_outtab                 = gt_data[].

ENDFORM.
FORM set_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD' .

ENDFORM. "FRM_PF_STATUS.
FORM user_command USING ucomm LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  IF ucomm EQ 'DELETE'.
    PERFORM kaydi_sil.
    selfield-refresh    = 'X'.
    selfield-col_stable = 'X'.
    selfield-row_stable = 'X'.
  ENDIF.

ENDFORM. "user_command
*&---------------------------------------------------------------------*
*& Form kaydi_sil
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM kaydi_sil .

  LOOP AT gt_data INTO gs_data WHERE sel EQ 'X'.
    UPDATE zint_000_t_003
       SET statu = 'ZZ'
     WHERE int_type    EQ '13'
       AND int_subtype EQ '01'
       AND guid        EQ gs_data-guid.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.

  PERFORM get_data.
  MESSAGE TEXT-003 TYPE 'S'.

ENDFORM.
