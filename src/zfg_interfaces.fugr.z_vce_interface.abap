FUNCTION Z_VCE_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_HTTP_REQUEST) TYPE  STRING
*"     VALUE(IV_WEBSERVICE) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_RESULT) TYPE  STRING
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : AD-231227
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& This RFC module represents the intefrace for VCE via WebMethod.
*& The FM will receive a http request and passt it to WebMethods where
*& it will be processed. WebMethods then will return the result from
*& the VCE web service to the FM.
**********************************************************************





ENDFUNCTION.
