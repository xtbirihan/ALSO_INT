*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTINT_CONFIG....................................*
DATA:  BEGIN OF STATUS_ZTINT_CONFIG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTINT_CONFIG                  .
CONTROLS: TCTRL_ZTINT_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTINT_CONFIG                  .
TABLES: ZTINT_CONFIG                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
