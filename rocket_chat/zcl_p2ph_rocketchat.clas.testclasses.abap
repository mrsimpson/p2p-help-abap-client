*"* use this source file for your ABAP unit test classes

CLASS ltcl_p2p_rc DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      create_simple_discussion FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_p2p_rc IMPLEMENTATION.

  METHOD create_simple_discussion.
    zcl_p2ph_rocketchat=>create( iv_support_area = 'unittest' )->ask_for_support(
      EXPORTING
        iv_message       = 'Was kann ich mit BOPF so alles anfangen'
        iv_expert_area   = 'DEVELOPMENT'
        ir_environment   = new zif_p2ph_ui_controller=>ty_environment( )
      IMPORTING
        ev_help_location = DATA(lv_help_url)
    ).

    cl_aunit_assert=>assert_not_initial( lv_help_url ).

  ENDMETHOD.

ENDCLASS.