class ZCL_P2PH_EXPERTISE_RULES definition
  public
  final
  create private .

public section.

  methods DETERMINE_EXPERT_AREA
    importing
      !IR_ENVIRONMENT type ref to zif_p2ph_ui_controller=>TY_ENVIRONMENT
    returning
      value(RV_EXPERT_AREA) type ZP2PH_EXPERT_AREA .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_P2PH_EXPERTISE_RULES .
protected section.
private section.
ENDCLASS.



CLASS ZCL_P2PH_EXPERTISE_RULES IMPLEMENTATION.


METHOD determine_expert_area.
  CONSTANTS:lv_function_id TYPE if_fdt_types=>id VALUE '14187758B6551EE6A7B84EEAFC8D1D5E'.
  DATA:lv_timestamp   TYPE timestamp,
       lt_name_value  TYPE abap_parmbind_tab,
       ls_name_value  TYPE abap_parmbind,
       lr_data        TYPE REF TO data,
       lx_fdt         TYPE REF TO cx_fdt,
       lr_environment TYPE REF TO data.
  FIELD-SYMBOLS: <la_any>         TYPE any,
                 <lr_environment> TYPE any.

  GET TIME STAMP FIELD lv_timestamp.
****************************************************************************************************
  ls_name_value-name = 'ENVIRONMENT'.
  cl_fdt_function_process=>move_data_to_data_object( EXPORTING ir_data             = ir_environment
                                                               iv_function_id      = lv_function_id
                                                               iv_data_object      = '14187758B6551EE6A7B854857347DD6F' "ENVIRONMENT
                                                               iv_timestamp        = lv_timestamp
                                                               iv_trace_generation = abap_false
                                                               iv_has_ddic_binding = abap_false
                                                     IMPORTING er_data             = ls_name_value-value ).
  INSERT ls_name_value INTO TABLE lt_name_value.

  DATA lr_expert_area TYPE REF TO data.
  CREATE DATA lr_expert_area TYPE REF TO zp2ph_exp_area.
  cl_fdt_function_process=>get_data_object_reference( EXPORTING iv_function_id      = lv_function_id
                                                                iv_data_object      = '_V_RESULT'
                                                                iv_timestamp        = lv_timestamp
                                                                iv_trace_generation = abap_false
                                                      IMPORTING er_data             = lr_expert_area ).
  ASSIGN lr_expert_area->* TO <la_any>.
  TRY.
      cl_fdt_function_process=>process( EXPORTING iv_function_id = lv_function_id
                                                  iv_timestamp   = lv_timestamp
                                        IMPORTING ea_result      = <la_any>
                                        CHANGING  ct_name_value  = lt_name_value ).
      rv_expert_area = <la_any>.
    CATCH cx_fdt INTO lx_fdt.
****************************************************************************************************
*You can check CX_FDT->MT_MESSAGE for error handling.
****************************************************************************************************
  ENDTRY.
ENDMETHOD.


  METHOD get_instance.

    STATICS so_singleton LIKE ro_instance.

    IF so_singleton IS INITIAL.
      CREATE OBJECT so_singleton.
    ENDIF.

    ro_instance = so_singleton.

  ENDMETHOD.
ENDCLASS.