*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZINT_000_T_004..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_004                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_004                .
CONTROLS: TCTRL_ZINT_000_T_004
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZINT_000_T_004                .
TABLES: ZINT_000_T_004                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
