*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZINT_000_T_009..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_009                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_009                .
CONTROLS: TCTRL_ZINT_000_T_009
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZINT_000_T_009                .
TABLES: ZINT_000_T_009                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
