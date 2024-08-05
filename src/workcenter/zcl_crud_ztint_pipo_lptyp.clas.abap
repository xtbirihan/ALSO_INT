class ZCL_CRUD_ZTINT_PIPO_LPTYP definition
  public
  final
  create public .

public section.

  types:
    tt_ztint_pipo_lptyp TYPE STANDARD TABLE OF ztint_pipo_lptyp WITH EMPTY KEY .

  methods SELECT_SINGLE_BY_NLTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_NLTYP type /SCWM/T333_NLTYP
    returning
      value(RS_RESULT) type ZTINT_PIPO_LPTYP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTINT_PIPO_LPTYP IMPLEMENTATION.


  METHOD select_single_by_nltyp.
**********************************************************************
*& Key           : RM-230801
*& Request No.   : GAP 32 - Simple UI for Partial Replenishment
**********************************************************************
*& Description (short)
*&
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztint_pipo_lptyp
                    INTO @rs_result
                   WHERE lgnum = @iv_lgnum
                     AND nltyp = @iv_nltyp.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ZTINT_PIPO_LPTYP' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
