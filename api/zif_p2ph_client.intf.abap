interface ZIF_P2PH_CLIENT
  public .


  types:
    BEGIN OF ty_help_location,
          provider TYPE string,
          url      TYPE string,
          provider_details TYPE string, "JSON
         END OF ty_help_location .
  types:
    BEGIN OF ty_message,
            message_text TYPE string,
    END OF ty_message .

  constants:
    BEGIN OF support_area,
                sap_application_help TYPE zif_p2ph_chat_proxy=>ty_support_area VALUE 'SAP_Application',
             END OF support_area .

  methods ASK_FOR_SUPPORT
    importing
      !IV_MESSAGE type ZP2PH_MESSAGE_TEXT
      !IV_EXPERT_AREA type ZP2PH_EXPERT_AREA
      !IR_ENVIRONMENT type ref to DATA
    exporting
      !EV_COUNT_PROVIDERS type I
      !EV_HELP_LOCATION type ZIF_P2PH_CHAT_PROXY=>TY_URL
    raising
      ZCX_P2PH .
endinterface.
