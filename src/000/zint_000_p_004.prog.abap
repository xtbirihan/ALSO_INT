*---------------------------------------------------------------------*
* Tanım : zmm_r_001 - Asis Entegrasyon
*---------------------------------------------------------------------*
* Geliştirme Danışmanı    : Oğulcan Gökkurt
* Uygulama Danışmanı      :
* Geliştirme No           :
* Tarih                   : 29.09.2022
*---------------------------------------------------------------------*
* Değişiklik Günlüğü
*---------------------------------------------------------------------*
* Değişiklik/Request No   :
* Tarih                   :
* Geliştirme Danışmanı    :
*---------------------------------------------------------------------*

REPORT zint_000_p_004.

INCLUDE ZINT_000_P_004_top.
INCLUDE ZINT_000_P_004_f01.

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.

END-OF-SELECTION.

  PERFORM display_data.
