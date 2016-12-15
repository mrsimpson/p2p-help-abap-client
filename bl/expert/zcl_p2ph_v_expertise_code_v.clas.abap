class ZCL_P2PH_V_EXPERTISE_CODE_V definition
  public
  inheriting from /BOFU/CL_V_CODE_VALUES
  final
  create public .

public section.
protected section.

  methods GET_VALIDATION_CONFIGURATION
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_P2PH_V_EXPERTISE_CODE_V IMPLEMENTATION.


method GET_VALIDATION_CONFIGURATION.

  et_attribute_details = value #(
                          ( attribute_name = zif_p2ph_expert=>sc_node_attribute-area-area       type_name = 'ZP2PH_EXPERT_AREA' )
*                          ( attribute_name = zif_p2ph_expert=>sc_node_attribute-area-expertise  type_name = 'ZP2PH_EXPERT_LEVEL' ) "ranges not supported by lib => dedicated validation
                          ).

endmethod.
ENDCLASS.