*&---------------------------------------------------------------------*
*& Report ZINT_000_P_TABLE_CLEANER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zint_000_p_table_cleaner.

INCLUDE zint_000_p_table_cleaner_top.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  PERFORM process.

FORM get_data.

  IF s_guid[] IS NOT INITIAL.

    SELECT guid,
           int_type,
           int_subtype
      FROM zint_000_t_003
     WHERE guid         IN @s_guid
       AND datum        IN @s_datum
       AND int_type     EQ @p_type
       AND int_subtype  IN @s_stype
      INTO TABLE @gt_003.

  ELSE.

    SELECT guid,
           int_type,
           int_subtype
      FROM zint_000_t_003
     WHERE datum        IN @s_datum
       AND int_type     EQ @p_type
       AND int_subtype  IN @s_stype
      INTO TABLE @gt_003.

  ENDIF.

  SELECT DISTINCT
       t2~int_type,
       t2~int_subtype,
       t2~table_header,
       t2~table_item,
       t2~table_item_detail
  FROM zint_000_t_002 AS t2
 INNER JOIN @gt_003   AS t3 ON t3~int_type    = t2~int_type
                           AND t3~int_subtype = t2~int_subtype
  INTO TABLE @gt_002.

ENDFORM.


FORM process.

  DATA :
    lv_table_name(20) TYPE c,
    lv_cnt            TYPE i,
    lv_subrc1         TYPE sysubrc,
    lv_subrc2         TYPE sysubrc,
    lv_subrc3         TYPE sysubrc.

  LOOP AT gt_003 ASSIGNING FIELD-SYMBOL(<fs_003>).
    READ TABLE gt_002 ASSIGNING FIELD-SYMBOL(<fs_002>) WITH KEY int_type = <fs_003>-int_type int_subtype = <fs_003>-int_subtype.
    IF sy-subrc = 0.

      IF <fs_002>-table_header IS NOT INITIAL.
        lv_table_name = <fs_002>-table_header.

        CLEAR lv_cnt.

        SELECT COUNT(*)
          INTO lv_cnt
          FROM (lv_table_name)
         WHERE guid        = <fs_003>-guid
           AND int_type    = <fs_003>-int_type
           AND int_subtype = <fs_003>-int_subtype.

        IF lv_cnt > 0.

          DELETE FROM (lv_table_name)
           WHERE guid        = <fs_003>-guid
             AND int_type    = <fs_003>-int_type
             AND int_subtype = <fs_003>-int_subtype.

          lv_subrc1 = sy-subrc.
        ELSE.
          lv_subrc1 = 0.
        ENDIF.
      ELSE.
        lv_subrc1 = 0.
      ENDIF.

      IF <fs_002>-table_item IS NOT INITIAL.
        lv_table_name = <fs_002>-table_item.

        CLEAR lv_cnt.

        SELECT COUNT(*)
          INTO lv_cnt
          FROM (lv_table_name)
         WHERE guid        = <fs_003>-guid
           AND int_type    = <fs_003>-int_type
           AND int_subtype = <fs_003>-int_subtype.

        IF lv_cnt > 0.

          DELETE FROM (lv_table_name)
           WHERE guid        = <fs_003>-guid
             AND int_type    = <fs_003>-int_type
             AND int_subtype = <fs_003>-int_subtype.

          lv_subrc2 = sy-subrc.
        ELSE.
          lv_subrc2 = 0.
        ENDIF.
      ELSE.
        lv_subrc2 = 0.
      ENDIF.

      IF <fs_002>-table_item_detail IS NOT INITIAL.
        lv_table_name = <fs_002>-table_item_detail.

        CLEAR lv_cnt.

        SELECT COUNT(*)
          INTO lv_cnt
          FROM (lv_table_name)
         WHERE guid        = <fs_003>-guid
           AND int_type    = <fs_003>-int_type
           AND int_subtype = <fs_003>-int_subtype.

        IF lv_cnt > 0.

          DELETE FROM (lv_table_name)
           WHERE guid        = <fs_003>-guid
             AND int_type    = <fs_003>-int_type
             AND int_subtype = <fs_003>-int_subtype.

          lv_subrc3 = sy-subrc.
        ELSE.
          lv_subrc3 = 0.
        ENDIF.
      ELSE.
        lv_subrc3 = 0.
      ENDIF.

      IF lv_subrc1 = 0 AND lv_subrc2 = 0 AND lv_subrc3 = 0.
        DELETE FROM zint_000_t_003
         WHERE guid        = <fs_003>-guid
           AND int_type    = <fs_003>-int_type
           AND int_subtype = <fs_003>-int_subtype.

        "27.06.2022 fsucu/dyavuz
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.
        "27.06.2022 fsucu/dyavuz

      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.
