CLASS zcl_int_ex_core_lsc_layout DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_core_lsc_layout.
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA st_assigned_bins TYPE RANGE OF /scwm/lgpla.
    METHODS direct_repmlenishment IMPORTING iv_lgnum                        TYPE /scwm/lgnum
                                            iv_dest_storage_type            TYPE /scwm/lgtyp
                                            iv_guid_hu                      TYPE /scwm/guid_hu
                                  RETURNING VALUE(rv_direct_repmlenishment) TYPE abap_bool.
    METHODS determine_interm_settings IMPORTING iv_lgnum          TYPE /scwm/lgnum
                                                iv_src_stor_type  TYPE /scwm/ltap_vltyp
                                                iv_dst_stor_type  TYPE /scwm/ltap_nltyp
                                      CHANGING  cv_interm_type    TYPE /scwm/de_iltyp
                                                cv_interm_section TYPE /scwm/de_ilber
                                                cv_interm_bin     TYPE /scwm/de_ilpla.
ENDCLASS.



CLASS ZCL_INT_EX_CORE_LSC_LAYOUT IMPLEMENTATION.


  METHOD /scwm/if_ex_core_lsc_layout~layout.
**********************************************************************
*& Key           : AD-230329
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_CORE_LSC_LAYOUT
*& Influences the Layout oriented storage control
**********************************************************************
    BREAK-POINT ID zcg_ex_core_lsc_layout.

    " Get all Destination storage types, for which the direct replenishment should be checked.
    zcl_param=>get_parameter(
                      EXPORTING
                          iv_lgnum = iv_lgnum
                          iv_process  = zif_param_const=>c_zint_0001
                          iv_parameter = zif_param_const=>c_dest_storage_type
                      IMPORTING
                          et_list = DATA(lt_param_list) ).

    " Check, whether the storage type is relevant for the check.
    IF line_exists( lt_param_list[ table_line =  iv_nltyp  ] ).
      " Check, whether the material in the HU is destined for direct replenishment
      IF direct_repmlenishment( iv_lgnum = iv_lgnum
                                iv_dest_storage_type = iv_nltyp
                                iv_guid_hu = is_huhdr-guid_hu  ) = abap_true.

        " Override LOSC settings, so that the pick point will be skipped.
        " No further checks a necessary.
        CLEAR: cv_iltyp, cv_ilber, cv_ilpla, cv_iprocty.
        RETURN.
      ENDIF.
    ENDIF.

    " Determine the storage bin within the pick point area.
    determine_interm_settings(
            EXPORTING
                iv_lgnum = iv_lgnum
                iv_src_stor_type = iv_vltyp
                iv_dst_stor_type = iv_nltyp
            CHANGING
                cv_interm_type = cv_iltyp
                cv_interm_section = cv_ilber
                cv_interm_bin = cv_ilpla ).
  ENDMETHOD.


  METHOD determine_interm_settings.
**********************************************************************
*& Key           : AD-230329
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Determines the intermediate setting for storage type, -bin and -section
*& w
**********************************************************************

    " Get Mapping: Source/Destination storage type to intermediate storage bin
    SELECT sequence, interm_bin_type
      INTO TABLE @DATA(lt_losc_mappings)
      FROM ztint_losc_types
     WHERE lgnum = @iv_lgnum
       AND src_stor_type = @iv_src_stor_type
       AND dst_stor_type = @iv_dst_stor_type
      ORDER BY sequence.

    " If no mapping was found, the intermediate storage bin can not be determined.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_losc_mappings ASSIGNING FIELD-SYMBOL(<lv_losc_mapping>).
      " The  FBL and KDL storage area will share bins.
      " If the system can't find an available bin for the designated area, it should look for an available bin in the other area(s).

      " Search for a matching storage bin with the intermediate storage type
      SELECT SINGLE lgpla
        INTO @DATA(lv_storage_bin)
        FROM /scwm/lagp
       WHERE lgnum = @iv_lgnum
         AND lgtyp = @cv_interm_type
         AND lptyp = @<lv_losc_mapping>-interm_bin_type
         AND skzue = ' '
         AND kzler = 'X'
         AND lgpla IN @st_assigned_bins.

      IF sy-subrc = 0.
        " Available bin found => change storage bin and remember the assigned bin for further determinations.
        st_assigned_bins = VALUE #( BASE st_assigned_bins ( sign = 'E' option = 'EQ' low =  lv_storage_bin  ) ).
        cv_interm_bin = lv_storage_bin.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD direct_repmlenishment.
**********************************************************************
*& Key           : AD-230329
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Checks, whether the material in the HU can be replenished directly
*& without detour to a pick point.
**********************************************************************
    DATA lt_hu_items TYPE /scwm/tt_huitm_int.
    DATA ls_material_storage_type_data TYPE /scwm/s_material_lgtyp.

    " Get HU data
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_guid_hu = iv_guid_hu
      IMPORTING
        et_huitm   = lt_hu_items
      EXCEPTIONS
        deleted    = 1
        error      = 2
        not_found  = 3
        OTHERS     = 4.

    IF sy-subrc <> 0.
      rv_direct_repmlenishment = abap_false.
      RETURN.
    ENDIF.

    " Check, whether the HU has an item
    IF NOT line_exists( lt_hu_items[ 1 ] ).
      rv_direct_repmlenishment = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " Get Material data
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_lgnum     = iv_lgnum
            iv_entitled  = lt_hu_items[ 1 ]-entitled
            iv_matid     = lt_hu_items[ 1 ]-matid
            iv_lgtyp     = iv_dest_storage_type
          IMPORTING
            es_mat_lgtyp = ls_material_storage_type_data.
      CATCH /scwm/cx_md  INTO DATA(lx_exception).
        rv_direct_repmlenishment = abap_false.
        RETURN.
    ENDTRY.

    " Is the product destined for direct replenishment?
    IF ls_material_storage_type_data-zz1_dirrpl_stt = abap_true.
      rv_direct_repmlenishment = abap_true.
    ELSE.
      rv_direct_repmlenishment = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
