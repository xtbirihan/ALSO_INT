class ZINT_000_CO_SI_SAP2EKOL_GUIDCO definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods SI_SAP2EKOL_GUIDCONFIRMATION_A
    importing
      !OUTPUT type ZINT_000_MT_SAP2EKOL_GUIDCONFI
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINT_000_CO_SI_SAP2EKOL_GUIDCO IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINT_000_CO_SI_SAP2EKOL_GUIDCO'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method SI_SAP2EKOL_GUIDCONFIRMATION_A.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'OUTPUT' kind = '0' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'SI_SAP2EKOL_GUIDCONFIRMATION_A'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
