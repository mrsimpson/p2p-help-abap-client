interface ZIF_P2PH_UI_CONTROLLER
  public .


  types:
    BEGIN OF ty_help_info .
  INCLUDE TYPE help_info AS dynpro_context.
  TYPES:          wd_application TYPE string,
                  END OF ty_help_info .
  types:
    BEGIN OF ty_environment,
      appl_package             TYPE devclass,
      client                   LIKE sy-mandt,
      system                   LIKE sy-sysid,
      application_component_id TYPE df14l-ps_posid.
  INCLUDE TYPE ty_help_info AS help_info.
  TYPES:
    latest_ok_code LIKE sy-ucomm,
    latest_message TYPE string,
    release        TYPE string,
    END OF ty_environment .

  constants:
    BEGIN OF gc_help_source,
      application_help TYPE zp2ph_help_source VALUE 'H',
      peer             TYPE zp2ph_help_source VALUE 'P',
      undefined        TYPE zp2ph_help_source VALUE ' ',
    END OF gc_help_source .

  methods SELECT_HELP_SOURCE
    importing
      !IV_REMEMBER_DECISION type BOOLE_D optional
    returning
      value(RV_SOURCE) type ZP2PH_HELP_SOURCE
    raising
      ZCX_P2PH_UI .
  methods ASK_FOR_SUPPORT
    importing
      !IS_HELP_INFO type TY_HELP_INFO optional
    exporting
      !EV_FAILED type ABAP_BOOL .
  methods GET_CONFIGURED_HELP_SOURCE
    returning
      value(RV_SOURCE) type ZP2PH_HELP_SOURCE .
  methods IS_P2P_HELP_POSSIBLE
    importing
      !IS_HELP_INFO type ZIF_P2PH_UI_CONTROLLER=>TY_HELP_INFO
    returning
      value(RV_IS_ENABLED) type ABAP_BOOL .
endinterface.
