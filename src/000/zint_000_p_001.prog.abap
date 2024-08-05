*&---------------------------------------------------------------------*
*& Report ZINT_000_P_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zint_000_p_001.

INCLUDE zint_000_p_001_top.
INCLUDE zint_000_p_001_zor.
INCLUDE zint_000_p_001_c01.
INCLUDE zint_000_p_001_o01.
INCLUDE zint_000_p_001_i01.
INCLUDE zint_000_p_001_f01.

INITIALIZATION.
  PERFORM init.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sitype-low.
  PERFORM sh_sitype.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  IF gt_alv1 IS NOT INITIAL.
    PERFORM list_screen.
  ELSE.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
