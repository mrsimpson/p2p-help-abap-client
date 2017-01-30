CLASS zcl_p2ph_ui_controller DEFINITION
  PUBLIC
  ABSTRACT
 .

  PUBLIC SECTION.

    INTERFACES zif_p2ph_ui_controller .

    TYPES ty_ui_technology TYPE c LENGTH 1.
    CONSTANTS: BEGIN OF co_ui_technology,
                    sapgui TYPE ty_ui_technology VALUE 'D',
                    webdynpro TYPE ty_ui_technology VALUE 'W',
               END OF co_ui_technology.

    CLASS-METHODS create
        IMPORTING
           iv_ui_technology TYPE ty_ui_technology
      RETURNING
        VALUE(ro_controller) TYPE REF TO zif_p2ph_ui_controller .
protected section.

  constants CO_PARAM_NAME_HELP_SOURCE type MEMORYID value 'ZP2PH_HELP_SOURCE' ##NO_TEXT.
  data MO_CLIENT type ref to ZIF_P2PH_CLIENT .

  methods LAUNCH_BROWSER
  abstract
    importing
      !IV_URL type ZIF_P2PH_CHAT_PROXY=>TY_URL .
  methods RENDER_CREATION_RESULT_HTML
    importing
      !IV_QUESTION type ZIF_P2PH_CHAT_PROXY=>TY_QUESTION
      !IV_COUNT_PROVIDERS_JOINED type I
      !IV_URL type ZIF_P2PH_CHAT_PROXY=>TY_URL
    returning
      value(RV_HTML) type STRING .
  methods INIT .
  methods GET_INITIAL_QUESTION
  abstract
    exporting
      !EV_FULL_TEXT type STRING
      !EV_CANCELLED type ABAP_BOOL .
  methods GET_ENVIRONMENT
    importing
      !IS_HELP_INFO type ZIF_P2PH_UI_CONTROLLER=>TY_HELP_INFO
    returning
      value(RR_ENVIRONMENT) type ref to ZIF_P2PH_UI_CONTROLLER=>TY_ENVIRONMENT .
ENDCLASS.



CLASS ZCL_P2PH_UI_CONTROLLER IMPLEMENTATION.


  METHOD create.
    CASE iv_ui_technology.
      WHEN co_ui_technology-sapgui.
        CREATE OBJECT ro_controller TYPE zcl_p2ph_sapgui_controller.
      WHEN co_ui_technology-webdynpro.
        CREATE OBJECT ro_controller TYPE zcl_p2ph_webdynpro_controller.
    ENDCASE.

  ENDMETHOD.


  METHOD get_environment.
    DATA lt_system_context TYPE STANDARD TABLE OF sdokpropty WITH DEFAULT KEY.

    CREATE DATA rr_environment.

    rr_environment->client = sy-mandt.
    rr_environment->system = sy-sysid.
    rr_environment->latest_ok_code = sy-ucomm.

    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO rr_environment->latest_message.
    ENDIF.

    CALL FUNCTION 'IWB_SYSTEM_CONTEXT_GET'
      TABLES
        context = lt_system_context.
    READ TABLE lt_system_context INTO DATA(ls_system_context) WITH KEY name = 'RELEASE'.

    IF sy-subrc EQ 0.
      rr_environment->release = ls_system_context-value.
    ENDIF.

    rr_environment->help_info = is_help_info.

    IF rr_environment->tcode IS NOT INITIAL.
      SELECT SINGLE a~ps_posid FROM tadir AS t
         INNER JOIN tdevc AS p ON
           t~devclass = p~devclass
         INNER JOIN df14l AS a ON
           p~component = a~fctr_id
*         and a~as4local = sy-langu
         INTO rr_environment->application_component_id
         WHERE   t~pgmid    = 'R3TR'
           AND   t~object   = 'TRAN'
           AND   t~obj_name = rr_environment->tcode.
    ELSE.
      IF rr_environment->program IS NOT INITIAL.
        SELECT SINGLE a~ps_posid FROM tadir AS t
           INNER JOIN tdevc AS p ON
             t~devclass = p~devclass
           INNER JOIN df14l AS a ON
             p~component = a~fctr_id
*                 and a~as4local = sy-langu
           INTO rr_environment->application_component_id
           WHERE   t~pgmid    = 'R3TR'
             AND   t~object   = 'PROG'
             AND   t~obj_name = rr_environment->program.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD init.
    mo_client = zcl_p2ph_rocketchat=>create( iv_support_area = zif_p2ph_client=>support_area-sap_application_help ).
  ENDMETHOD.


  METHOD render_creation_result_html.
    DATA lv_count_providers_joined TYPE string.
    lv_count_providers_joined = iv_count_providers_joined.

    rv_html = |<html><body><div>|.

    rv_html = rv_html && |<span>| .
    rv_html = rv_html && |<a href={ iv_url }>{ replace( val = text-004 sub = '&1' with = lv_count_providers_joined ) }</a>| .
    rv_html = rv_html && || .
    rv_html = rv_html && |</span>| .
    rv_html = rv_html && |</div></body></html>| .
  ENDMETHOD.


METHOD zif_p2ph_ui_controller~ask_for_support.
  CLEAR: ev_failed.

  me->get_initial_question(
      IMPORTING
          ev_full_text = DATA(lv_full_text)
          ev_cancelled = DATA(lv_cancelled)
  ).

  IF lv_cancelled = abap_true.
    RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ENDIF.

  TRY.
      mo_client->ask_for_support(
        EXPORTING
          iv_message       = lv_full_text
          iv_expert_area   = zcl_p2ph_expertise_rules=>get_instance( )->determine_expert_area( me->get_environment( is_help_info ) )
          ir_environment   = me->get_environment( is_help_info )
        IMPORTING
          ev_help_location    = DATA(lv_help_location)
          ev_count_providers  = DATA(lv_count_providers)
      ).

      IF lv_help_location IS NOT INITIAL.

        me->launch_browser( lv_help_location ).

      ENDIF.
    CATCH zcx_p2ph INTO DATA(lx_p2ph).
      DATA(lv_message) = lx_p2ph->get_text( ).
      ev_failed = abap_true.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ENDTRY.

ENDMETHOD.


  METHOD zif_p2ph_ui_controller~get_configured_help_source.

    GET PARAMETER ID 'ZP2PH_HELP_SOURCE' FIELD rv_source.

    IF    ( rv_source NE zif_p2ph_ui_controller=>gc_help_source-application_help )
      AND ( rv_source NE zif_p2ph_ui_controller=>gc_help_source-peer ).

      rv_source = zif_p2ph_ui_controller=>gc_help_source-undefined.
      SET PARAMETER ID 'ZP2PH_HELP_SOURCE' FIELD rv_source.
    ENDIF.

  ENDMETHOD.


METHOD zif_p2ph_ui_controller~is_p2p_help_possible.
  rv_is_enabled = boolc( zcl_p2ph_expertise_rules=>get_instance( )->determine_expert_area( ir_environment = me->get_environment( is_help_info ) ) IS NOT INITIAL ).
ENDMETHOD.


  METHOD zif_p2ph_ui_controller~select_help_source.
    ##needed
*    Depends on the UI technology, but cannot be abstract as it's part of an interface
  ENDMETHOD.
ENDCLASS.
