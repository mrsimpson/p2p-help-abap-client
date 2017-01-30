*&---------------------------------------------------------------------*
*& Report  ZPHPH_EXPERT_MAINTENANCE
*&
*&---------------------------------------------------------------------*
*& This report launches the expert-profile-maintenance webdynpro application
*& In order to do this, it re-uses functionality from the BRF+ workbench
*&---------------------------------------------------------------------*
REPORT zphph_expert_maintenance.

CLASS lcl_viewer DEFINITION INHERITING FROM cl_fdt_wd_viewer.
  PUBLIC SECTION.
    METHODS show_expert_maintenance.

  PROTECTED SECTION.
*  Unfortunately, some methods of the brf-workbench are private.
*  Thus, we had to copy some code
    METHODS call_browser
      IMPORTING
        iv_application     TYPE  string
        iv_use_smartclient TYPE  boole_d DEFAULT abap_false
        it_url_parameter   TYPE  tihttpnvp OPTIONAL.

ENDCLASS.

CLASS lcl_viewer IMPLEMENTATION.
  METHOD call_browser.

    CONSTANTS: lcon_sso_url    TYPE  string VALUE '/sap/public/myssocntl'.

    DATA: lv_url            TYPE string,
          lv_char_url(2048) TYPE c,
          lv_msgno          TYPE sy-msgno,
          lv_sso_active     TYPE icfactive,
          lo_html_viewer    TYPE REF TO cl_gui_html_viewer,
          lo_container      TYPE REF TO cl_gui_container.

    DATA: ls_message TYPE if_fdt_types=>s_message,
          lt_message TYPE if_fdt_types=>t_message.
**********************************************************************


    cl_wd_utilities=>construct_wd_url(
        EXPORTING application_name = iv_application
                  in_parameters    = it_url_parameter
        IMPORTING out_absolute_url = lv_url ).

    lv_char_url = lv_url.

*check if SSO is configured
    CALL METHOD cl_icf_tree=>if_icf_tree~service_from_url
      EXPORTING
        url             = lcon_sso_url
        hostnumber      = 0
        authority_check = ' '
      IMPORTING
        icfactive       = lv_sso_active.

    IF lv_sso_active = abap_true.
      CREATE OBJECT lo_html_viewer
        EXPORTING
          parent = lo_container.

      CALL METHOD lo_html_viewer->enable_sapsso
        EXPORTING
          enabled = 'X'
        EXCEPTIONS
          OTHERS  = 0.

      IF sy-subrc = 0.
        CALL METHOD lo_html_viewer->detach_url_in_browser
          EXPORTING
            url        = lv_char_url
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2.

        IF sy-subrc <> 0.
          lv_msgno = 104. "Start of Browser failed: Error occured in SAPGU
        ENDIF.

      ELSE.
        lv_msgno = 104. "Start of Browser failed: Error occured in SAPGU
      ENDIF.
      cl_gui_cfw=>flush( ).
    ELSE.
      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          url                    = lv_char_url
*         WINDOW_NAME            = ' '
*         NEW_WINDOW             = ' '
*         BROWSER_TYPE           =
*         CONTEXTSTRING          =
        EXCEPTIONS
          frontend_not_supported = 1
          frontend_error         = 2
          prog_not_found         = 3
          no_batch               = 4
          OTHERS                 = 6.
      CASE sy-subrc.
        WHEN 0. "OK
        WHEN 1.
          lv_msgno = 103. "Start of Browser failed: Frontend is not supported
        WHEN 2.
          lv_msgno = 104. "Start of Browser failed: Error occured in SAPGU
        WHEN 3.
          lv_msgno = 105. "Start of Browser failed: Cannot be executed in background
        WHEN OTHERS.
          lv_msgno = 106. "Unknown error
      ENDCASE.
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE ID 'FDT_WD_INFRA' TYPE 'E' NUMBER lv_msgno INTO ls_message-text.
      MOVE-CORRESPONDING syst TO ls_message.
      ls_message-source = cx_fdt=>get_source_name( ).
      APPEND ls_message TO lt_message.
      RAISE EXCEPTION TYPE cx_fdt_system
        EXPORTING
          mt_message = lt_message.
    ENDIF.


  ENDMETHOD.                    "call_browser


  METHOD show_expert_maintenance.
    DATA lt_parameter     TYPE tihttpnvp.
    DATA ls_parameter LIKE LINE OF lt_parameter.
    DATA lx_fdt_system    TYPE REF TO cx_fdt_system.

    TRY.
* set URL parameters - we're not allowed to use the private method of
* the super class, so copy the relevant part
*Language
        ls_parameter-name  = 'sap-language'.                "#EC NOTEXT
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
          EXPORTING
            input  = sy-langu
          IMPORTING
            output = ls_parameter-value.
        APPEND ls_parameter TO lt_parameter.
*Client
        ls_parameter-name  = 'sap-client'.                  "#EC NOTEXT
        ls_parameter-value = cl_abap_syst=>get_client( ).
        APPEND ls_parameter TO lt_parameter.


*call browser
        call_browser(
            iv_application      = 'ZP2P_EXPERT_OVP' ##no_text
            it_url_parameter    = lt_parameter
            iv_use_smartclient  = abap_false ).

      CATCH cx_fdt_system INTO lx_fdt_system.
        cl_fdt_wd_service=>convert_backend_exception( lx_fdt_system ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  NEW lcl_viewer( )->show_expert_maintenance( ).
