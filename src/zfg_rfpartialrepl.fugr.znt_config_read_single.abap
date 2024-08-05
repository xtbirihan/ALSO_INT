FUNCTION znt_config_read_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"  EXPORTING
*"     REFERENCE(ES_CONFIG) TYPE  ZTINT_CONFIG
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
************************************************************************
* 1/ Functional module name ZNT_CONFIG_READ_SINGLE
************************************************************************
************************************************************************
* 2/ Description/program functions
************************************************************************
* Functionality for reading general consumer products parameters configuration.
************************************************************************
  DATA:
        ls_config TYPE Ztint_config.

  READ TABLE gt_prod_param_config INTO ls_config WITH KEY lgnum = iv_lgnum.
  IF sy-subrc <> 0.
    SELECT * FROM ztint_config INTO TABLE gt_prod_param_config WHERE lgnum = iv_lgnum.
    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.
    READ TABLE gt_prod_param_config INTO ls_config WITH KEY lgnum = iv_lgnum.
  ENDIF.

  MOVE-CORRESPONDING ls_config TO es_config.





ENDFUNCTION.
