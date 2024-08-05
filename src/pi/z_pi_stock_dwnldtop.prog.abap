**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-160323
**********************************************************************
*&---------------------------------------------------------------------*
*& Include /SCWM/R_PI_STOCK_DWNLDTOP
*&
*&---------------------------------------------------------------------*
REPORT /scwm/r_pi_stock_dwnld MESSAGE-ID /scwm/pi_appl_report.
*----------------------------------------------------------------------*
*--- global declarations
*----------------------------------------------------------------------*

TABLES:
  sscrfields ##NEEDED,
  /scwm/s_pi_stock_dwnld,
  wsd_basic_mat_sty,
  /scwm/s_huhdr.

TYPE-POOLS: wmegc, limpi, icon, sabc.

TYPES:
  tyt_stock_dwnld TYPE STANDARD TABLE OF /scwm/s_pi_stock_dwnld.
* local class for event handling
CLASS lcl_alv_menu  DEFINITION DEFERRED.

CONSTANTS:
* Document type set internally - Temporary solution
  gc_doc_type_el             TYPE /lime/pi_document_type VALUE 'EL',
  gc_doc_type_es             TYPE /lime/pi_document_type VALUE 'ES',
  gc_csv                     TYPE char4 VALUE '.csv',
  gc_xls                     TYPE char4 VALUE '.xls',
  gc_stock_dwnld             TYPE tabname VALUE '/SCWM/S_PI_STOCK_DWNLD',
  gc_moving_average_price(1) TYPE c VALUE 'V',
  gc_standard_price(1)       TYPE c VALUE 'S',
  gc_lf_name                 TYPE fileintern VALUE 'EWM_PI_DOWNLOAD'.

DATA:
  ok_code           LIKE sy-ucomm ##NEEDED,
  gt_bapiret        TYPE bapirettab ##NEEDED,
  gt_stock_dwnld    TYPE tyt_stock_dwnld ##NEEDED,
  gt_fieldcat       TYPE lvc_t_fcat ##NEEDED,  " Field catalog table
  go_stock_fields   TYPE REF TO /scwm/cl_ui_stock_fields ##NEEDED,
  go_cust_scwm      TYPE REF TO /scwm/if_pi_cust ##NEEDED,
  go_cust_lime      TYPE REF TO /lime/if_pi_cust ##NEEDED,
  go_alvgrid        TYPE REF TO cl_gui_alv_grid ##NEEDED,
  go_event_receiver TYPE REF TO lcl_alv_menu ##NEEDED,
  go_log            TYPE REF TO /scwm/cl_log ##NEEDED.
*----------------------------------------------------------------------*
*--- definition of the selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK pigen WITH FRAME TITLE TEXT-010.
  PARAMETERS:     p_cound TYPE /scwm/pi_de_ui_count_date
                               DEFAULT sy-datlo,
                  p_rea   TYPE /scwm/s_pi_stock_dwnld-reason,    "Reason
                  p_loc   TYPE xfeld RADIOBUTTON GROUP type
                               USER-COMMAND fi3 DEFAULT 'X',
                  p_prod  TYPE xfeld RADIOBUTTON GROUP type.
SELECTION-SCREEN END OF BLOCK pigen.
*Location-based selection for Warehouse
SELECTION-SCREEN BEGIN OF BLOCK piloc WITH FRAME TITLE TEXT-003.
  PARAMETERS:     p_lgnum   TYPE /scwm/s_pi_stock_dwnld-lgnum OBLIGATORY.
  SELECT-OPTIONS: s_area    FOR /scwm/s_pi_stock_dwnld-pi_area_ui,
                  s_lpla    FOR /scwm/s_pi_stock_dwnld-lgpla.
SELECTION-SCREEN END OF BLOCK piloc.
*Product-based selection for Warehouse
SELECTION-SCREEN BEGIN OF BLOCK piprod WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS: s_matnr   FOR /scwm/s_pi_stock_dwnld-matnr MODIF ID id1,
                  s_owner   FOR /scwm/s_pi_stock_dwnld-owner MODIF ID id1,
                  s_entitl  FOR /scwm/s_pi_stock_dwnld-entitled
                                MODIF ID id1,
                  s_cat_p   FOR /scwm/s_pi_stock_dwnld-cat MODIF ID id1.
SELECTION-SCREEN END OF BLOCK piprod.
SELECTION-SCREEN BEGIN OF BLOCK adata WITH FRAME TITLE TEXT-add.
  SELECT-OPTIONS: so_mfrnr FOR wsd_basic_mat_sty-mfrnr MODIF ID id1 ##NEEDED,
  so_mfrpn FOR wsd_basic_mat_sty-mfrpn MODIF ID id1 ##NEEDED,
  so_huid FOR /scwm/s_huhdr-huident MODIF ID id1 ##NEEDED.
SELECTION-SCREEN END OF BLOCK adata.
*** Block for target directory and file location
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-006.
  PARAMETERS:     p_pcfile  TYPE xfeld RADIOBUTTON GROUP floc
                                 USER-COMMAND fil DEFAULT 'X'," PC download
                  p_path    TYPE string LOWER CASE,
                  p_asfile  TYPE xfeld RADIOBUTTON GROUP floc, " App.ser.Dwld
                  p_lfname  TYPE fileintern DEFAULT gc_lf_name,
                  p_cntr(2) TYPE n,
                  p_crea    TYPE xfeld RADIOBUTTON GROUP mode
                                 USER-COMMAND fi2 DEFAULT 'X',
                  p_repl    TYPE xfeld RADIOBUTTON GROUP mode,
                  p_app     TYPE xfeld RADIOBUTTON GROUP mode.
SELECTION-SCREEN END   OF BLOCK a1.
