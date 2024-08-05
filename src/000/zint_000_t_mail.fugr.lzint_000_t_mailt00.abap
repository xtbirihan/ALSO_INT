*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZINT_000_T_MAIL.................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_MAIL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_MAIL               .
CONTROLS: TCTRL_ZINT_000_T_MAIL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZINT_000_T_MAIL               .
TABLES: ZINT_000_T_MAIL                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
