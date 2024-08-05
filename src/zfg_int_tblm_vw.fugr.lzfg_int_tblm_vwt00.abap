*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTINT_LOSC_TYPES................................*
DATA:  BEGIN OF STATUS_ZTINT_LOSC_TYPES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTINT_LOSC_TYPES              .
CONTROLS: TCTRL_ZTINT_LOSC_TYPES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTINT_LOSC_TYPES              .
TABLES: ZTINT_LOSC_TYPES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
