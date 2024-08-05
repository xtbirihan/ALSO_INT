*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_PIPO_LPTYP..................................*
TABLES: ZMV_PIPO_LPTYP, *ZMV_PIPO_LPTYP. "view work areas
CONTROLS: TCTRL_ZMV_PIPO_LPTYP
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_PIPO_LPTYP. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_PIPO_LPTYP.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_PIPO_LPTYP_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_PIPO_LPTYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PIPO_LPTYP_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_PIPO_LPTYP_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_PIPO_LPTYP.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PIPO_LPTYP_TOTAL.

*.........table declarations:.................................*
TABLES: ZTINT_PIPO_LPTYP               .
