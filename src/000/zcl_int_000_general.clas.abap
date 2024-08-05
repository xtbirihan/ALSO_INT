class ZCL_INT_000_GENERAL definition
  public
  final
  create public .

public section.

  class-methods GR_REVERSAL
    importing
      !IV_ALINAN_TESLIMAT_NO type ZSD_040_ALINAN_TESLIMAT_NO
      !IV_DATUM type DATUM default SY-DATUM
    exporting
      !ET_MESSAGES type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_000_GENERAL IMPLEMENTATION.


  METHOD gr_reversal.

*/  Linklemeden kopyalandı - tgursoy
    DATA:
      ls_s0017    TYPE zsd_040_s_0017,
      lt_messages TYPE bapiret2_t.

    SUBMIT zsd_040_004
      WITH p_tesno = iv_alinan_teslimat_no
      WITH p_datum = iv_datum
      AND RETURN.

    IMPORT ls_s0017 TO ls_s0017 FROM MEMORY ID 'ZSD_040_S_0017'.

*    IF ls_s0017-malzeme_belge_no IS INITIAL.
*      MESSAGE e083 WITH iv_alinan_teslimat_no INTO ycl_acf_log=>dummy.
*      ycl_acf_bapiret2=>insert( CHANGING i_bapiret2 = lt_messages ).
*    ENDIF.

*    et_messages = lt_messages.

  ENDMETHOD.
ENDCLASS.
