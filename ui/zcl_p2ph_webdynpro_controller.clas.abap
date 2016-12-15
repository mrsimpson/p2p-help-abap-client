class ZCL_P2PH_WEBDYNPRO_CONTROLLER definition
  public
  inheriting from ZCL_P2PH_UI_CONTROLLER
  final
  create public .

public section.

  methods CONSTRUCTOR .
  PROTECTED SECTION.
    METHODS launch_browser REDEFINITION.
    METHODS get_initial_question REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_P2PH_WEBDYNPRO_CONTROLLER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->init( ).
  ENDMETHOD.


  METHOD get_initial_question.
    CLEAR: ev_cancelled, ev_full_text.
  ENDMETHOD.


METHOD launch_browser.
  DATA(lo_navigate_to) = cl_fpm=>get_instance( )->get_navigate_to( ).

  lo_navigate_to->launch_url(
    EXPORTING
      is_url_fields            = VALUE #( header_text = 'Peer-to-Peer-Help'(001)
                                          url         = iv_url )    " Fields to launch a URL
      is_additional_parameters = VALUE #( navigation_mode = 'EXTERNAL' )    " Portal Parameters
  ).

ENDMETHOD.
ENDCLASS.