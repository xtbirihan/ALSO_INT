*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*             GENERAL PROGRAM INFORMATION
*----------------------------------------------------------------------*
"Program    : ZINT_000_P_002
"Title      : Entegrasyon Raporu
"Programmer : Furkan HÜLAGÜ - Ulaş Söylemez
"Description: Entegrasyon ile gelen verilerin eventlera takılmadan
"tek bir ekranda raporlamak için yapılmıştır.
*----------------------------------------------------------------------*
*             HISTORY OF REVISIONS
*----------------------------------------------------------------------*
"Date       :
"Programmer :
"Incident   :
"Description:
*&---------------------------------------------------------------------*
include : zint_000_p_002_top ,
          zint_000_p_002_cls ,
          zint_000_p_002_sub .

initialization.
  go_obj = cl_main=>create_instance( ).
  go_obj->set_initial_values( ).

at selection-screen.
  go_obj->set_seltab( sy-repid ).

at selection-screen on value-request for s_sitype-low.
  go_obj->f4_s_sitype(
    changing
      s_sitype = s_sitype-low
  ).

start-of-selection.
  go_obj->run( ).
