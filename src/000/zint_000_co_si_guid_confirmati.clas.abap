class ZINT_000_CO_SI_GUID_CONFIRMATI definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods SI_GUID_CONFIRMATION_ASYN_OUT
    importing
      !OUTPUT type ZINT_000_MT_GUID_CONFIRMATION
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINT_000_CO_SI_GUID_CONFIRMATI IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINT_000_CO_SI_GUID_CONFIRMATI'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method SI_GUID_CONFIRMATION_ASYN_OUT.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'OUTPUT' kind = '0' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'SI_GUID_CONFIRMATION_ASYN_OUT'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
