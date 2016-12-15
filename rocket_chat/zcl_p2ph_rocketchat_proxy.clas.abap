CLASS zcl_p2ph_rocketchat_proxy DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_p2ph_chat_proxy.

    CLASS-METHODS create
      RETURNING VALUE(ro_proxy) TYPE REF TO zif_p2ph_chat_proxy.

  PROTECTED SECTION.

    METHODS get_http_client
      RETURNING VALUE(ro_client) TYPE REF TO if_http_client.

  PRIVATE SECTION.
    CONSTANTS co_dummy_response_element TYPE string VALUE 'dummy'.

    DATA mv_authorization TYPE string.

    METHODS constructor
        IMPORTING iv_url TYPE zif_p2ph_chat_proxy=>ty_url OPTIONAL.

    METHODS get_authorization
      RETURNING
        VALUE(rv_auth_header) TYPE string.

    METHODS create_request_json
      IMPORTING
        is_request TYPE zif_p2ph_chat_proxy=>ty_help_discussion_request
      RETURNING
        VALUE(rv_json) TYPE string.

ENDCLASS.



CLASS ZCL_P2PH_ROCKETCHAT_PROXY IMPLEMENTATION.


  METHOD constructor.
    mv_authorization = me->get_authorization( ).
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_proxy TYPE zcl_p2ph_rocketchat_proxy.
  ENDMETHOD.


  METHOD create_request_json.

    DATA lv_json TYPE xstring.
    DATA(lo_json_writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).

    CALL TRANSFORMATION id
        SOURCE request = is_request
        RESULT XML lo_json_writer.

    lv_json = CAST cl_sxml_string_writer( lo_json_writer )->get_output(  ).

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring = lv_json
*       im_encoding = 'UTF-8'
      IMPORTING
        ex_string  = rv_json.
*    Danach haben wir ein REQUEST-Objekt mit Uppercase-Namen

  ENDMETHOD.


  METHOD get_authorization.
    "TODO: Add rule for basic-auth-token
    CLEAR rv_auth_header.
  ENDMETHOD.


  METHOD get_http_client.
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = 'P2P_HELP'
      IMPORTING
        client                   =  ro_client   " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF mv_authorization IS NOT INITIAL.
      ro_client->request->set_header_field(
        EXPORTING
          name  =     'authorization'
          value =     mv_authorization
      ).
    ENDIF.

    ro_client->request->set_content_type( content_type = 'application/json' ).

  ENDMETHOD.


  METHOD zif_p2ph_chat_proxy~create_help_discussion.

*    Prepare request

    DATA(lo_http_client) = me->get_http_client( ).

    DATA(lv_json) = create_request_json( is_request ).

    lo_http_client->request->set_cdata( lv_json ).

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_p2ph
        EXPORTING
          mv_message_text = CONV #( 'Could not reach collaboration-system. Please contact an admin.'(001) )
          previous     = zcx_p2ph=>create_from_symsg( ).
    ENDIF.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
      lo_http_client->close(
        EXCEPTIONS
          http_invalid_state = 1
          OTHERS             = 2
      ).
      RAISE EXCEPTION TYPE zcx_p2ph
        EXPORTING
          mv_message_text = CONV #( 'Could not reach collaboration-system. Please contact an admin.'(001) )
          previous     = zcx_p2ph=>create_from_symsg( ).
    ENDIF.


*    Unpack result
    lo_http_client->response->get_status(
      IMPORTING
        code   = DATA(lv_status)
        reason = DATA(lv_reason)
    ).
    IF lv_status EQ 200.
      DATA(lv_response_data) = lo_http_client->response->get_cdata( ).

      lo_http_client->close(
        EXCEPTIONS
          http_invalid_state = 1
          OTHERS             = 2
      ).
      IF sy-subrc <> 0.
        zcx_p2ph=>create_from_symsg( )->raise( ).
      ENDIF.

      TRY.
          CALL TRANSFORMATION id
              SOURCE XML lv_response_data
              RESULT result = rs_result.
        CATCH cx_st_error INTO DATA(lx_st).
          RAISE EXCEPTION TYPE zcx_p2ph
            EXPORTING
              mv_message_text = |Couldn't extract data from response. Potentially erroneous (or empty) body|
              previous     = lx_st.
        CATCH cx_xslt_runtime_error into data(lx_xslt).
          RAISE EXCEPTION TYPE zcx_p2ph
            EXPORTING
              mv_message_text = |No XML content retrieved. Is the RFC connection properly configured?|
              previous     = lx_xslt.
      ENDTRY.

    ELSE.
      lo_http_client->close(
        EXCEPTIONS
          http_invalid_state = 1
          OTHERS             = 2
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_p2ph
          EXPORTING
            mv_message_text = CONV #( 'Could not reach collaboration-system. Please contact an admin.'(001) )
            previous     = zcx_p2ph=>create_from_symsg( ).
      ENDIF.
      RAISE EXCEPTION TYPE zcx_p2ph
        EXPORTING
          mv_message_text = lv_reason.
    ENDIF.
  ENDMETHOD.
ENDCLASS.