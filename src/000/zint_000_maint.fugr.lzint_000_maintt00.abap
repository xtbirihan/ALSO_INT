*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZINT_000_MV_002.................................*
TABLES: ZINT_000_MV_002, *ZINT_000_MV_002. "view work areas
CONTROLS: TCTRL_ZINT_000_MV_002
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZINT_000_MV_002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZINT_000_MV_002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZINT_000_MV_002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZINT_000_MV_002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZINT_000_MV_002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZINT_000_MV_002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZINT_000_MV_002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZINT_000_MV_002_TOTAL.

*...processing: ZINT_000_T_001..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_001                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_001                .
CONTROLS: TCTRL_ZINT_000_T_001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZINT_000_T_002..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_002                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_002                .
CONTROLS: TCTRL_ZINT_000_T_002
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZINT_000_T_006..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_006                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_006                .
CONTROLS: TCTRL_ZINT_000_T_006
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZINT_000_T_007..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_007                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_007                .
CONTROLS: TCTRL_ZINT_000_T_007
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZINT_000_T_008..................................*
DATA:  BEGIN OF STATUS_ZINT_000_T_008                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINT_000_T_008                .
CONTROLS: TCTRL_ZINT_000_T_008
            TYPE TABLEVIEW USING SCREEN '0006'.
*.........table declarations:.................................*
TABLES: *ZINT_000_T_001                .
TABLES: *ZINT_000_T_002                .
TABLES: *ZINT_000_T_006                .
TABLES: *ZINT_000_T_007                .
TABLES: *ZINT_000_T_008                .
TABLES: ZINT_000_T_001                 .
TABLES: ZINT_000_T_002                 .
TABLES: ZINT_000_T_006                 .
TABLES: ZINT_000_T_007                 .
TABLES: ZINT_000_T_008                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
