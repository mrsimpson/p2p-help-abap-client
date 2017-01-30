class ZCL_P2PH_ROCKETCHAT definition
  public
  create private .

public section.

  interfaces ZIF_P2PH_CLIENT .

  class-methods CREATE
    importing
      !IV_SUPPORT_AREA type ZIF_P2PH_CHAT_PROXY=>TY_SUPPORT_AREA
    returning
      value(RO_CLIENT) type ref to ZIF_P2PH_CLIENT .
  methods CONSTRUCTOR
    importing
      !IV_SUPPORT_AREA type ZIF_P2PH_CHAT_PROXY=>TY_SUPPORT_AREA .
protected section.

  data MO_CHAT_PROXY type ref to ZIF_P2PH_CHAT_PROXY .

  methods GET_SEEKER
    returning
      value(RS_USER) type ZIF_P2PH_CHAT_PROXY=>TY_USER .
  methods GET_PROVIDERS
    importing
      !IV_EXPERT_AREA type ZP2PH_EXPERT_AREA
    returning
      value(RT_USER) type ZIF_P2PH_CHAT_PROXY=>TT_USER .
private section.

  data MV_SUPPORT_AREA type ZIF_P2PH_CHAT_PROXY=>TY_SUPPORT_AREA .

  methods GET_CALLBACK_URL
    returning
      value(RV_URL) type ZIF_P2PH_CHAT_PROXY=>TY_URL .
ENDCLASS.



CLASS ZCL_P2PH_ROCKETCHAT IMPLEMENTATION.


  METHOD constructor.
    mv_support_area = iv_support_area.
    mo_chat_proxy = zcl_p2ph_rocketchat_proxy=>create( ).
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_client TYPE zcl_p2ph_rocketchat
      EXPORTING
        iv_support_area = iv_support_area.
  ENDMETHOD.


  METHOD get_callback_url.

  ENDMETHOD.


METHOD get_providers.
*  get experts by expert profile
  DATA(lo_sm_expert) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_p2ph_expert=>sc_bo_key ).
  DATA lt_expert_root TYPE zp2ph_exp_t_root.

  lo_sm_expert->query(
    EXPORTING
      iv_query_key            = zif_p2ph_expert=>sc_query-root-select_by_area    " Query
      it_selection_parameters = VALUE #(
                                        ( attribute_name = zif_p2ph_expert=>sc_query_attribute-root-select_by_area-area  sign = 'I'  option = 'EQ'  low = iv_expert_area )
                                        ( attribute_name = zif_p2ph_expert=>sc_query_attribute-root-select_by_area-available  sign = 'E'  option = 'EQ'  low = 'N' )
                                )
      iv_fill_data            = abap_true
      it_requested_attributes = VALUE #( ( zif_p2ph_expert=>sc_node_attribute-root-email_address ) )
    IMPORTING
      et_data                 = lt_expert_root
  ).

  LOOP AT lt_expert_root ASSIGNING FIELD-SYMBOL(<ls_expert_root>).
    INSERT VALUE #( email = <ls_expert_root>-email_address ) INTO TABLE rt_user.
  ENDLOOP.

*  Add the Bot
  INSERT VALUE #( email = |hasso@beimir.net| ) INTO TABLE rt_user.
ENDMETHOD.


METHOD get_seeker.
  rs_user-email = 'seeker@beimir.net'.
ENDMETHOD.


METHOD zif_p2ph_client~ask_for_support.
  "============> Call Rocket Chat to create a new room
  DATA(ls_creation_result) =  mo_chat_proxy->create_help_discussion(
        is_request = VALUE #(
                      support_area = mv_support_area
                      seeker = me->get_seeker( )
                      providers = me->get_providers( iv_expert_area )
                      message = iv_message
                      environment = ir_environment
                      callback_url = me->get_callback_url( )
                     )  ).

  ev_help_location = ls_creation_result-url.
  ev_count_providers = lines( ls_creation_result-providers_joined ).

ENDMETHOD.
ENDCLASS.
