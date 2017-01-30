class ZCL_P2PH_EXP_D_USER_DETAILS definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  methods /BOBF/IF_FRW_DETERMINATION~CHECK_DELTA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_P2PH_EXP_D_USER_DETAILS IMPLEMENTATION.


METHOD /bobf/if_frw_determination~check_delta.
  io_read->compare(
    EXPORTING
      iv_node_key        = is_ctx-node_key
      it_key             = ct_key    " Key Table
      iv_fill_attributes = abap_true
      iv_scope           = /bobf/if_frw_c=>sc_scope_local
    IMPORTING
      eo_change          = DATA(lo_change)    " Interface of Change Object
  ).

  lo_change->get_changes(
      EXPORTING
        is_change_mode  = VALUE #( create = abap_true  update = abap_true )
        iv_failed       = abap_false
        iv_node_key     = is_ctx-node_key
      IMPORTING
        et_change       = DATA(lt_change_root)    " Node Table
*        et_changed_key  = et_changed_key    " Key Table
*        et_changed_node = et_changed_node    " Node Table
    ).

  LOOP AT ct_key ASSIGNING FIELD-SYMBOL(<ls_key>).
    READ TABLE lt_change_root ASSIGNING FIELD-SYMBOL(<ls_change_root>) WITH KEY key = <ls_key>-key.
    IF sy-subrc NE 0.
*      no change found => determination not relevant
      DELETE ct_key.
      CONTINUE. ">>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    READ TABLE <ls_change_root>-attributes TRANSPORTING NO FIELDS WITH KEY table_line = zif_p2ph_expert=>sc_node_attribute-root-uname.
    IF sy-subrc NE 0.
*      username not changed => determination not relevant
      DELETE ct_key.
      CONTINUE. ">>>>>>>>>>>>>>>>>>>>>
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD /bobf/if_frw_determination~execute.

  DATA lt_expert_root TYPE zp2ph_exp_t_root.
  DATA ls_bapiaddr3   TYPE bapiaddr3.
  DATA lt_return      TYPE bapiret2_tab.

  io_read->retrieve(
    EXPORTING
      iv_node                 = is_ctx-node_key    " Node Name
      it_key                  = it_key    " Key Table
      it_requested_attributes = VALUE #( ( zif_p2ph_expert=>sc_node_attribute-root-uname ) )
    IMPORTING
      et_data                 = lt_expert_root    " Data Return Structure
  ).

  LOOP AT lt_expert_root REFERENCE INTO DATA(lr_expert_root).
    DATA(lt_changed_field) = VALUE /bobf/t_frw_name( ).

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lr_expert_root->uname
      IMPORTING
        address  = ls_bapiaddr3
      TABLES
        return   = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CONTINUE. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    IF ls_bapiaddr3-e_mail NE lr_expert_root->email_address.
      INSERT zif_p2ph_expert=>sc_node_attribute-root-email_address INTO TABLE lt_changed_field.
      lr_expert_root->email_address = ls_bapiaddr3-e_mail.
    ENDIF.

    IF ls_bapiaddr3-fullname NE lr_expert_root->full_name.
      INSERT zif_p2ph_expert=>sc_node_attribute-root-full_name INTO TABLE lt_changed_field.
      lr_expert_root->full_name = ls_bapiaddr3-fullname.
    ENDIF.

    IF lt_changed_field IS NOT INITIAL.
      io_modify->update(
          iv_node           = is_ctx-node_key    " Node
          iv_key            = lr_expert_root->key    " Key
          is_data           = lr_expert_root    " Data
          it_changed_fields = lt_changed_field
      ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
