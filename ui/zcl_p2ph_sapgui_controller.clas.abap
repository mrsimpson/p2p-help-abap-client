CLASS zcl_p2ph_sapgui_controller DEFINITION
  PUBLIC
  INHERITING FROM zcl_p2ph_ui_controller
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_p2ph_ui_controller .

  PUBLIC SECTION.
    METHODS zif_p2ph_ui_controller~select_help_source REDEFINITION.

  PROTECTED SECTION.
    METHODS launch_browser REDEFINITION.
    METHODS get_initial_question REDEFINITION.

  PRIVATE SECTION.
    METHODS constructor.
ENDCLASS.



CLASS ZCL_P2PH_SAPGUI_CONTROLLER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->init( ).
  ENDMETHOD.


  METHOD get_initial_question.
    DATA lv_full_text TYPE string.
    DATA lt_input_text TYPE catsxt_longtext_itab.

    CLEAR: ev_cancelled, ev_full_text.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title = 'Einfach fragen!'
*       IM_DISPLAY_MODE       = ' '
*       IM_START_COLUMN       = 10
*       IM_START_ROW          = 10
      CHANGING
        ch_text  = lt_input_text.
    IF sy-ucomm = 'CX_CONT'.

      CLEAR lv_full_text.
      LOOP AT lt_input_text INTO DATA(lv_input_text).
        lv_full_text = | { lv_full_text } { lv_input_text }|.
      ENDLOOP.
      CONDENSE lv_full_text.
      ev_full_text = lv_full_text.
    ELSE.
      MESSAGE 'Frage wird nicht gestellt'(002) TYPE 'I' DISPLAY LIKE 'S'.
      ev_cancelled = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD launch_browser.
*    cl_abap_browser=>show_url opens an inlined Internet-Explorer
*    However, this browser seems to be somewhat limited with respect to Javascript
*    Thus, directly launch a browser, preferrably Chrome
*    @see http://scn.sap.com/docs/DOC-55914
    DATA lv_url           TYPE string.
    DATA lv_char_url      TYPE char1024.
    DATA lo_html_viewer   TYPE REF TO cl_gui_html_viewer.

    lv_char_url = lv_url = iv_url.

    DATA lv_platform TYPE i.
    lv_platform = cl_gui_frontend_services=>get_platform( ).
    IF lv_platform <> cl_gui_frontend_services=>platform_macosx AND lv_platform <> cl_gui_frontend_services=>platform_linux.
      " Chrome should be used for the UI5 Application on windows
      " Therefore we first try to launch chrome.
      " If it fails we fall back to the default browser.

      REPLACE ALL OCCURRENCES OF '"' IN lv_url WITH '%34'.
      CONCATENATE '"' lv_url '"' INTO lv_url.
      cl_gui_frontend_services=>execute(
        EXPORTING
          application = 'chrome.exe'
          parameter  = lv_url
        EXCEPTIONS
          OTHERS     = 1
          ).
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
    ENDIF.

    TRY.
        DATA lo_empty_co TYPE REF TO cl_gui_container.      "#EC NEEDED
        CREATE OBJECT lo_html_viewer
          EXPORTING
            parent = lo_empty_co.

        CALL METHOD lo_html_viewer->('DETACH_URL_IN_BROWSER')
          EXPORTING
            url = lv_char_url.
        cl_gui_cfw=>flush( ).
      CATCH cx_root.
        CALL FUNCTION 'CALL_BROWSER'
          EXPORTING
            url                    = lv_char_url
          EXCEPTIONS
            frontend_not_supported = 1
            frontend_error         = 2
            prog_not_found         = 3
            no_batch               = 4
            unspecified_error      = 5.
        IF sy-subrc NE 0.
          MESSAGE e001(00) WITH 'Cannot start browser'(108).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_p2ph_ui_controller~select_help_source.

    DATA lv_answer TYPE c LENGTH 1.

    GET PARAMETER ID co_param_name_help_source FIELD rv_source.
    IF rv_source IS NOT INITIAL AND rv_source NE zif_p2ph_ui_controller=>gc_help_source-undefined.
*      User has a clear preference where to get help and does not want to be bothered again
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Bei wem suchst Du Hilfe?'(001)
*       diagnose_object       = SPACE    " Diagnosetext (Pflege über SE61)
        text_question         = 'Bei wem suchst Du Hilfe?'(001)
        text_button_1         = 'Kollege'(002)    " Text auf der ersten Drucktaste
        icon_button_1         = 'ICON_OPERATOR'
        text_button_2         = 'Helpdesk'(003)    " Text auf der zweiten Drucktaste
        icon_button_2         = 'ICON_SYSTEM_EXTENDED_HELP'
        display_cancel_button = abap_true
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc EQ 0.
      CASE lv_answer.
        WHEN '1'.
          rv_source = zif_p2ph_ui_controller=>gc_help_source-peer.
        WHEN '2'.
          rv_source = zif_p2ph_ui_controller=>gc_help_source-application_help.
        WHEN 'A'.
          rv_source = zif_p2ph_ui_controller=>gc_help_source-undefined.
*          RAISE EXCEPTION TYPE zcx_p2ph_ui EXPORTING message_text = 'Action aborted'.
        WHEN OTHERS.
          rv_source = zif_p2ph_ui_controller=>gc_help_source-undefined.
      ENDCASE.

      IF iv_remember_decision = abap_true.
        SET PARAMETER ID co_param_name_help_source FIELD rv_source.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.