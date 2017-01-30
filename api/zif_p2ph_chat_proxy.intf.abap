interface ZIF_P2PH_CHAT_PROXY
  public .


  types TY_QUESTION type STRING .
  types TY_SUPPORT_AREA type STRING .
  types TY_URL type STRING .
  types:
    BEGIN OF ty_user,
          email type string,
          id    type string,
          level type zp2ph_expert_level,
         end of ty_user .
  types:
    tt_user type SORTED TABLE OF ty_user WITH NON-UNIQUE KEY email .
  types:
    BEGIN OF ty_help_discussion_request,
              support_area    type ty_support_area,
              seeker          TYPE ty_user,
              providers       TYPE tt_user,
              message         TYPE ty_question,
              environment     TYPE REF TO data,
              callback_url    TYPE ty_url,
         END OF ty_help_discussion_request .
  types:
    BEGIN OF ty_help_discussion_created,
              url                 TYPE ty_url,
              providers_joined   TYPE tt_user,
         END OF ty_help_discussion_created .

  methods CREATE_HELP_DISCUSSION
    importing
      !IS_REQUEST type TY_HELP_DISCUSSION_REQUEST
    returning
      value(RS_RESULT) type TY_HELP_DISCUSSION_CREATED
    raising
      ZCX_P2PH .
endinterface.
