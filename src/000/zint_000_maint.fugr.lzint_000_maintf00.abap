*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZINT_000_MV_002.................................*
FORM GET_DATA_ZINT_000_MV_002.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZINT_000_T_002 WHERE
(VIM_WHERETAB) .
    CLEAR ZINT_000_MV_002 .
ZINT_000_MV_002-MANDT =
ZINT_000_T_002-MANDT .
ZINT_000_MV_002-INT_TYPE =
ZINT_000_T_002-INT_TYPE .
ZINT_000_MV_002-INT_SUBTYPE =
ZINT_000_T_002-INT_SUBTYPE .
ZINT_000_MV_002-DESCRIPTION =
ZINT_000_T_002-DESCRIPTION .
ZINT_000_MV_002-DIRECTION =
ZINT_000_T_002-DIRECTION .
ZINT_000_MV_002-INT_STYLE =
ZINT_000_T_002-INT_STYLE .
ZINT_000_MV_002-BALOBJ =
ZINT_000_T_002-BALOBJ .
ZINT_000_MV_002-BALSUBOBJ =
ZINT_000_T_002-BALSUBOBJ .
ZINT_000_MV_002-OBJECT =
ZINT_000_T_002-OBJECT .
ZINT_000_MV_002-TABLE_HEADER =
ZINT_000_T_002-TABLE_HEADER .
ZINT_000_MV_002-TABLE_ITEM =
ZINT_000_T_002-TABLE_ITEM .
    SELECT SINGLE * FROM ZINT_000_T_001 WHERE
INT_TYPE = ZINT_000_T_002-INT_TYPE .
    IF SY-SUBRC EQ 0.
    ENDIF.
<VIM_TOTAL_STRUC> = ZINT_000_MV_002.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZINT_000_MV_002 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZINT_000_MV_002.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZINT_000_MV_002-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZINT_000_T_002 WHERE
  INT_TYPE = ZINT_000_MV_002-INT_TYPE AND
  INT_SUBTYPE = ZINT_000_MV_002-INT_SUBTYPE .
    IF SY-SUBRC = 0.
    DELETE ZINT_000_T_002 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZINT_000_T_002 WHERE
  INT_TYPE = ZINT_000_MV_002-INT_TYPE AND
  INT_SUBTYPE = ZINT_000_MV_002-INT_SUBTYPE .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZINT_000_T_002.
    ENDIF.
ZINT_000_T_002-MANDT =
ZINT_000_MV_002-MANDT .
ZINT_000_T_002-INT_TYPE =
ZINT_000_MV_002-INT_TYPE .
ZINT_000_T_002-INT_SUBTYPE =
ZINT_000_MV_002-INT_SUBTYPE .
ZINT_000_T_002-DESCRIPTION =
ZINT_000_MV_002-DESCRIPTION .
ZINT_000_T_002-DIRECTION =
ZINT_000_MV_002-DIRECTION .
ZINT_000_T_002-INT_STYLE =
ZINT_000_MV_002-INT_STYLE .
ZINT_000_T_002-BALOBJ =
ZINT_000_MV_002-BALOBJ .
ZINT_000_T_002-BALSUBOBJ =
ZINT_000_MV_002-BALSUBOBJ .
ZINT_000_T_002-OBJECT =
ZINT_000_MV_002-OBJECT .
ZINT_000_T_002-TABLE_HEADER =
ZINT_000_MV_002-TABLE_HEADER .
ZINT_000_T_002-TABLE_ITEM =
ZINT_000_MV_002-TABLE_ITEM .
    IF SY-SUBRC = 0.
    UPDATE ZINT_000_T_002 ##WARN_OK.
    ELSE.
    INSERT ZINT_000_T_002 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZINT_000_MV_002-UPD_FLAG,
STATUS_ZINT_000_MV_002-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZINT_000_MV_002.
  SELECT SINGLE * FROM ZINT_000_T_002 WHERE
INT_TYPE = ZINT_000_MV_002-INT_TYPE AND
INT_SUBTYPE = ZINT_000_MV_002-INT_SUBTYPE .
ZINT_000_MV_002-MANDT =
ZINT_000_T_002-MANDT .
ZINT_000_MV_002-INT_TYPE =
ZINT_000_T_002-INT_TYPE .
ZINT_000_MV_002-INT_SUBTYPE =
ZINT_000_T_002-INT_SUBTYPE .
ZINT_000_MV_002-DESCRIPTION =
ZINT_000_T_002-DESCRIPTION .
ZINT_000_MV_002-DIRECTION =
ZINT_000_T_002-DIRECTION .
ZINT_000_MV_002-INT_STYLE =
ZINT_000_T_002-INT_STYLE .
ZINT_000_MV_002-BALOBJ =
ZINT_000_T_002-BALOBJ .
ZINT_000_MV_002-BALSUBOBJ =
ZINT_000_T_002-BALSUBOBJ .
ZINT_000_MV_002-OBJECT =
ZINT_000_T_002-OBJECT .
ZINT_000_MV_002-TABLE_HEADER =
ZINT_000_T_002-TABLE_HEADER .
ZINT_000_MV_002-TABLE_ITEM =
ZINT_000_T_002-TABLE_ITEM .
    SELECT SINGLE * FROM ZINT_000_T_001 WHERE
INT_TYPE = ZINT_000_T_002-INT_TYPE .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZINT_000_MV_002 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZINT_000_MV_002-INT_TYPE TO
ZINT_000_T_002-INT_TYPE .
MOVE ZINT_000_MV_002-INT_SUBTYPE TO
ZINT_000_T_002-INT_SUBTYPE .
MOVE ZINT_000_MV_002-MANDT TO
ZINT_000_T_002-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZINT_000_T_002'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZINT_000_T_002 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZINT_000_T_002'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZINT_000_MV_002 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZINT_000_T_002-MANDT =
ZINT_000_MV_002-MANDT .
ZINT_000_T_002-INT_TYPE =
ZINT_000_MV_002-INT_TYPE .
ZINT_000_T_002-INT_SUBTYPE =
ZINT_000_MV_002-INT_SUBTYPE .
ZINT_000_T_002-DESCRIPTION =
ZINT_000_MV_002-DESCRIPTION .
ZINT_000_T_002-DIRECTION =
ZINT_000_MV_002-DIRECTION .
ZINT_000_T_002-INT_STYLE =
ZINT_000_MV_002-INT_STYLE .
ZINT_000_T_002-BALOBJ =
ZINT_000_MV_002-BALOBJ .
ZINT_000_T_002-BALSUBOBJ =
ZINT_000_MV_002-BALSUBOBJ .
ZINT_000_T_002-OBJECT =
ZINT_000_MV_002-OBJECT .
ZINT_000_T_002-TABLE_HEADER =
ZINT_000_MV_002-TABLE_HEADER .
ZINT_000_T_002-TABLE_ITEM =
ZINT_000_MV_002-TABLE_ITEM .
    SELECT SINGLE * FROM ZINT_000_T_001 WHERE
INT_TYPE = ZINT_000_T_002-INT_TYPE .
    IF SY-SUBRC EQ 0.
    ELSE.
      CLEAR SY-SUBRC.
    ENDIF.
ENDFORM.

* base table related FORM-routines.............
INCLUDE LSVIMFTX .
