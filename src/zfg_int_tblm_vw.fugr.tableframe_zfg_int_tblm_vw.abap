*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_INT_TBLM_VW
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_INT_TBLM_VW    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
