class ZCL_P2PH_V_EXPERTISE_LEVEL definition
  public
  inheriting from /BOBF/CL_LIB_V_SUPERCLASS
  final
  create public .

public section.

  methods /BOBF/IF_FRW_VALIDATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_P2PH_V_EXPERTISE_LEVEL IMPLEMENTATION.


METHOD /BOBF/IF_FRW_VALIDATION~EXECUTE.

  clear: et_failed_key, eo_message.

  DATA lt_area TYPE zp2ph_exp_t_area.
  io_read->retrieve(
    EXPORTING
      iv_node                 = zif_p2ph_expert=>sc_node-area    " Node Name
      it_key                  = it_key    " Key Table
      it_requested_attributes = VALUE #( ( zif_p2ph_expert=>sc_node_attribute-area-expertise ) )
    IMPORTING
      et_data                 = lt_area
  ).

  LOOP AT lt_area ASSIGNING FIELD-SYMBOL(<ls_area>).

    IF <ls_area>-expertise < 0 OR <ls_area>-expertise > 100.
      INSERT value #( key = <ls_area>-key ) INTO TABLE et_failed_key.

      zcl_p2ph_message_helper=>add_message(
        EXPORTING
          im_frw     = new zcm_p2ph_expert(
                          textid = zcm_p2ph_expert=>level_invalid
                          severity = /bobf/cm_frw=>co_severity_error
                          symptom  = /bobf/if_frw_message_symptoms=>co_bo_inconsistency
                          mv_level = <ls_area>-expertise )
          is_ctx     = is_ctx    " Context of det/act/val - fills origin location if provided
        CHANGING
          co_message = eo_message    " Interface of Message Object (possibly initial reference)
      ).

    ENDIF.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.