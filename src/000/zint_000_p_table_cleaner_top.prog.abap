*&---------------------------------------------------------------------*
*& Include          ZINT_000_P_TABLE_CLEANER_TOP
*&---------------------------------------------------------------------*

TABLES :
  zint_000_t_003.

TYPES : BEGIN OF ty_003,
          guid        TYPE zint_000_t_003-guid,
          int_type    TYPE zint_000_t_003-int_type,
          int_subtype TYPE zint_000_t_003-int_subtype,
        END OF ty_003.

TYPES : BEGIN OF ty_002,
          int_type          TYPE zint_000_t_002-int_type,
          int_subtype       TYPE zint_000_t_002-int_subtype,
          table_header      TYPE zint_000_t_002-table_header,
          table_item        TYPE zint_000_t_002-table_item,
          table_item_detail TYPE zint_000_t_002-table_item_detail,
        END OF ty_002.

DATA :
  gt_003 TYPE STANDARD TABLE OF ty_003,
  gt_002 TYPE STANDARD TABLE OF ty_002.

SELECTION-SCREEN BEGIN OF BLOCK beleg WITH FRAME TITLE TEXT-a01.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-p01 FOR FIELD p_datum.
    PARAMETERS: p_datum LIKE vbco7-fkdat.
    SELECTION-SCREEN COMMENT 52(5) TEXT-p02 FOR FIELD p_datums.
    PARAMETERS: p_datums LIKE vbco7-fkdat DEFAULT sy-datlo.
  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS :
    s_guid  FOR zint_000_t_003-guid,
    s_datum FOR zint_000_t_003-datum NO-DISPLAY.

  PARAMETERS :
    p_type TYPE zint_000_t_003-int_type OBLIGATORY.

  SELECT-OPTIONS :
  s_stype FOR zint_000_t_003-int_subtype OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK beleg.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_type.
  PERFORM f4_int_type CHANGING p_type.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stype-high.
  PERFORM f4_sub_type_high CHANGING s_stype-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stype-low.
  PERFORM f4_sub_type_low CHANGING s_stype-low.

AT SELECTION-SCREEN ON s_datum.
  IF s_guid[] IS INITIAL AND s_datum[] IS INITIAL.
    MESSAGE 'Lütfen tarih giriniz' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.

AT SELECTION-SCREEN ON p_datums.
  IF sy-dynnr = 1000 AND
     NOT p_datums IS INITIAL AND
     p_datum GT p_datums.
    SET CURSOR FIELD 'P_DATUMS'.
    MESSAGE e650(db).
  ELSE.
    APPEND INITIAL LINE TO s_datum ASSIGNING FIELD-SYMBOL(<fs_datum>).
    <fs_datum>-sign   = 'I'.
    <fs_datum>-option = 'BT'.
    <fs_datum>-low    = p_datum.
    <fs_datum>-high   = p_datums.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'S_STYPE-HIGH'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


FORM f4_int_type CHANGING p_type.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  SELECT int_type, description
    FROM zint_000_t_001
   ORDER BY int_type ASCENDING
    INTO TABLE @DATA(lt_001).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INT_TYPE'
      window_title    = 'Entegrasyon Tipleri'
      dynprofield     = 'P_TYPE'
      value_org       = 'S'
    TABLES
      value_tab       = lt_001
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    p_type = ls_return-fieldval.
  ENDIF.

ENDFORM.


FORM f4_sub_type_high CHANGING p_stype.

  DATA:
    lt_return TYPE STANDARD TABLE OF ddshretval.

*> read int_type
  DATA :
    tb_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE,
    lv_int_type   TYPE char2.

  CLEAR:   tb_dynpfields.
  REFRESH: tb_dynpfields.

  MOVE 'P_TYPE' TO tb_dynpfields-fieldname.
  APPEND tb_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = tb_dynpfields
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

  IF sy-subrc = 0.

  ENDIF.

  READ TABLE tb_dynpfields INDEX 1.
  IF sy-subrc EQ 0.
    lv_int_type = tb_dynpfields-fieldvalue.
  ENDIF.
*< read int type

  SELECT int_subtype, description
    FROM zint_000_t_002
    INTO TABLE @DATA(lt_002)
   WHERE int_type = @lv_int_type
   ORDER BY int_subtype ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INT_SUBTYPE'
      window_title    = 'Entegrasyon alt tipleri'
      dynprofield     = 'S_STYPE-HIGH'
      value_org       = 'S'
    TABLES
      value_tab       = lt_002
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    p_stype = ls_return-fieldval.
  ENDIF.

ENDFORM.

FORM f4_sub_type_low CHANGING p_stype.

  DATA:
    lt_return TYPE STANDARD TABLE OF ddshretval.

*> read int_type
  DATA :
    tb_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE,
    lv_int_type   TYPE char2.

  CLEAR:   tb_dynpfields.
  REFRESH: tb_dynpfields.

  MOVE 'P_TYPE' TO tb_dynpfields-fieldname.
  APPEND tb_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = tb_dynpfields
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

  IF sy-subrc = 0.

  ENDIF.

  READ TABLE tb_dynpfields INDEX 1.
  IF sy-subrc EQ 0.
    lv_int_type = tb_dynpfields-fieldvalue.
  ENDIF.
*< read int type

  SELECT int_subtype, description
    FROM zint_000_t_002
    INTO TABLE @DATA(lt_002)
   WHERE int_type = @lv_int_type
   ORDER BY int_subtype ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INT_SUBTYPE'
      window_title    = 'Entegrasyon alt tipleri'
      dynprofield     = 'S_STYPE-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_002
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    p_stype = ls_return-fieldval.
  ENDIF.

ENDFORM.
