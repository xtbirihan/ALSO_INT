**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-014
*& Author        : <aahmedov>-270323
**********************************************************************
*&---------------------------------------------------------------------*
*&  Include           /SCWM/LUI_PI_PROCESSIFT
*&---------------------------------------------------------------------*

INTERFACE lif_ta_c.

* Constants used by the TA manager
  CONSTANTS:
*   TCodes
    sc_tcode_cr                 TYPE tcode VALUE 'ZPI_CREATE',
    sc_tcode_pr                 TYPE tcode VALUE 'ZPI_PROCESS',
    sc_tcode_co                 TYPE tcode VALUE 'ZPI_COUNT',
    sc_tcode_al                 TYPE tcode VALUE 'ZPI',

*   Set/Get for search criteria
    sc_sg_p                     TYPE memoryid VALUE '/SCWM/SG_SS_PI_PROC',
    sc_sg_c                     TYPE memoryid VALUE '/SCWM/SG_SS_PI_CREA',

*   Ok-Codes
    sc_ok_exit                  TYPE syucomm VALUE 'OK_EXIT',
    sc_ok_back                  TYPE syucomm VALUE 'OK_BACK',
    sc_ok_canc                  TYPE syucomm VALUE 'OK_CANC',
    sc_ok_save                  TYPE syucomm VALUE 'OK_SAVE',

    sc_ok_process_crea          TYPE syucomm VALUE 'OK_CREA',
    sc_ok_process_proc          TYPE syucomm VALUE 'OK_PROC',

    sc_ok_go_cr                 TYPE syucomm VALUE 'OK_GO_CR',
    sc_ok_go_pr                 TYPE syucomm VALUE 'OK_GO_PR',
    sc_ok_go_mon                TYPE syucomm VALUE 'OK_GO_MON',

*   OIP tabs
    sc_ok_tab_oip_pi_cr         TYPE syucomm VALUE 'OK_TAB_OIP_PI_CR',
    sc_ok_tab_oip_pi_pr         TYPE syucomm VALUE 'OK_TAB_OIP_PI_PR',

*   ODP tabs
    sc_ok_tab_odp_pi_count      TYPE syucomm VALUE 'OK_TAB_ODP_PI_COUNT',
    sc_ok_tab_odp_pi_diff       TYPE syucomm VALUE 'OK_TAB_ODP_PI_DIFF',
    sc_ok_tab_odp_pi_book       TYPE syucomm VALUE 'OK_TAB_ODP_PI_BOOK',
    sc_ok_tab_odp_pi_deriv      TYPE syucomm VALUE 'OK_TAB_ODP_PI_DERIV',
    sc_ok_tab_odp_pi_ref        TYPE syucomm VALUE 'OK_TAB_ODP_PI_REF',
    sc_ok_tab_odp_pi_log        TYPE syucomm VALUE 'OK_TAB_ODP_PI_LOG',

*   ODP2 tabs
    sc_ok_tab_odp2_pi_count     TYPE syucomm VALUE 'OK_TAB_ODP2_PI_COUNT',
    sc_ok_tab_odp2_pi_diff      TYPE syucomm VALUE 'OK_TAB_ODP2_PI_DIFF',
    sc_ok_tab_odp2_pi_book      TYPE syucomm VALUE 'OK_TAB_ODP2_PI_BOOK',
    sc_ok_tab_odp2_pi_deriv     TYPE syucomm VALUE 'OK_TAB_ODP2_PI_DERIV',

*   Ok-Codes OI PI CR
    sc_ok_oi_pi_cr_insert       TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_INSERT',

    sc_ok_oi_pi_cr_delete       TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_DELETE',

    sc_ok_oi_pi_cr_check        TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_CHECK',

    sc_ok_oi_pi_cr_adopt        TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_ADOPT',
    sc_ok_oi_pi_cr_cancel       TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_CANCEL',

    sc_ok_oi_pi_cr_lock         TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_LOCK',
    sc_ok_oi_pi_cr_unlock       TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_UNLOCK',

    sc_ok_oi_pi_cr_fixb         TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_FIXB',
    sc_ok_oi_pi_cr_unfixb       TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_UNFIXB',

    sc_ok_oi_pi_cr_act          TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_ACT',
    sc_ok_oi_pi_cr_inact        TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_INACT',

    sc_ok_oi_pi_cr_mass_change  TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_MASSCH',

    sc_ok_oi_pi_cr_save         TYPE syucomm
                                 VALUE 'OK_OI_PI_CR_SAVE',

*   Ok-Codes OI PI PR
    sc_ok_oi_pi_pr_change       TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_CHANGE',

    sc_ok_oi_pi_pr_display      TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_DISPLAY',

    sc_ok_oi_pi_pr_check        TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_CHECK',
    sc_ok_oi_pi_pr_save         TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_SAVE',
    sc_ok_oi_pi_pr_active       TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_ACTIVE',
    sc_ok_oi_pi_pr_print        TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_PRINT',
    sc_ok_oi_pi_pr_activate     TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_ACTIVATE',
    sc_ok_oi_pi_pr_inactivate   TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_INACTIVATE',
    sc_ok_oi_pi_pr_count        TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_COUNT',
    sc_ok_oi_pi_pr_post         TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_POST',
    sc_ok_oi_pi_pr_recount      TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_RECOUNT',
    sc_ok_oi_pi_pr_change_count TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_CHANGE_COUNT',
    sc_ok_oi_pi_pr_delete       TYPE syucomm
                                 VALUE 'OK_OI_PI_PR_DELETE',

*   Ok-Codes OD PI COUNT
    sc_ok_od_pi_co_insert       TYPE syucomm
                                 VALUE 'OK_OD_PI_CO_INSERT',
    sc_ok_od_pi_co_copy         TYPE syucomm
                                 VALUE 'OK_OD_PI_CO_COPY',
    sc_ok_od_pi_co_delete       TYPE syucomm
                                 VALUE 'OK_OD_PI_CO_DELETE',

*   Ok-Codes OD2 PI COUNT
    sc_ok_od2_pi_co_insert      TYPE syucomm
                                 VALUE 'OK_OD2_PI_CO_INSERT',
    sc_ok_od2_pi_co_delete      TYPE syucomm
                                 VALUE 'OK_OD2_PI_CO_DELETE',
    sc_ok_od2_pi_co_serial      TYPE syucomm
                                 VALUE 'OK_OD2_PI_CO_SERIAL',

*----ALV Layouts--------------------------------------------------------
*    1.    Digit: Process I, O, T, M
*    2.+3. Digit: Pattern Component: IO for OIP, D1 for ODP1 and D2
*                 for ODP2
*    4.    Digit: Number of tab
    sc_handle_oi_pi_cr          TYPE char4 VALUE 'CIP1',
    sc_handle_oi_pi_pr          TYPE char4 VALUE 'PIP1',
    sc_handle_od_pi_count       TYPE char4 VALUE 'PD11',
    sc_handle_od_pi_diff        TYPE char4 VALUE 'PD12',
    sc_handle_od_pi_book        TYPE char4 VALUE 'PD13',
    sc_handle_od_pi_deriv       TYPE char4 VALUE 'PD14',
    sc_handle_od_pi_ref         TYPE char4 VALUE 'PD15',
    sc_handle_od_pi_log         TYPE char4 VALUE 'PD16',
    sc_handle_od2_pi_count      TYPE char4 VALUE 'PD21',
    sc_handle_od2_pi_diff       TYPE char4 VALUE 'PD22',
    sc_handle_od2_pi_book       TYPE char4 VALUE 'PD23',

*----Messages-----------------------------------------------------------
    sc_msg_odp_activate         TYPE char30 VALUE 'MSG_ODP_ACTIVATE',

*----Screen modification groups ---------------------------------------
    sc_modif_nco                TYPE char3 VALUE 'NCO',

*----Constants for Default Value Handling
    sc_def_pi                   TYPE /scmb/de_dusaction
                                VALUE 'ZPI'.

ENDINTERFACE.                    "lif_ta_c
