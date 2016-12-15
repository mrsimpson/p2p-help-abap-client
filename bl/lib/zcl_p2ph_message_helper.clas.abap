class ZCL_P2PH_MESSAGE_HELPER definition
  public
  final
  create private .

public section.

  constants MC_NAME_BO_KEY type FIELDNAME value 'BO_KEY' ##NO_TEXT.

*"* public components of class ZCL_P2PH_MESSAGE_HELPER
*"* do not include other source files here!!!
  class-methods ADD_MESSAGE
    importing
      !IM_FRW type ref to /BOBF/CM_FRW
      !IS_CTX type ANY optional
    changing
      !CO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE .
  class-methods ADD_FRW_MESSAGE
    importing
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !IS_CTX type ANY optional
    changing
      !CO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE .
  PROTECTED SECTION.
*"* protected components of class ZCL_P2PH_MESSAGE_HELPER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_P2PH_MESSAGE_HELPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_P2PH_MESSAGE_HELPER IMPLEMENTATION.


  METHOD ADD_FRW_MESSAGE.

    IF io_message IS NOT INITIAL.

*    initialize data container if necessary
      IF co_message IS INITIAL.
        co_message = /bobf/cl_frw_factory=>get_message( ).
      ENDIF.

      co_message->add( io_message = io_message ).

    ENDIF.

  ENDMETHOD.


  METHOD ADD_MESSAGE.

*    initialize data container if necessary
    IF co_message IS INITIAL.
      co_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

*    fill origin location if context is provided
    IF is_ctx IS NOT INITIAL.
      FIELD-SYMBOLS         <lv_bo_key>   TYPE /bobf/conf_key.
      FIELD-SYMBOLS         <lv_node_key> TYPE /bobf/conf_key.
      FIELD-SYMBOLS         <ls_ctx>      TYPE any.
      DATA ls_location      LIKE im_frw->ms_origin_location.

      ASSIGN is_ctx TO <ls_ctx>.

*      extract bo_key and node_key from context - this is common to all contexts
      ASSIGN COMPONENT mc_name_bo_key   OF STRUCTURE <ls_ctx> TO <lv_bo_key>.
      ASSIGN COMPONENT /bobf/if_conf_c=>sc_attribute_name_node_key OF STRUCTURE <ls_ctx> TO <lv_node_key>.

*    update origin location
      ls_location           = im_frw->ms_origin_location.
      ls_location-bo_key    = <lv_bo_key>.
      ls_location-node_key  = <lv_node_key>.

      im_frw->set_origin_location( ls_location ).

    ENDIF.

    co_message->add_cm(
       EXPORTING
         io_message = im_frw
*        it_message = it_message
     ).

  ENDMETHOD.
ENDCLASS.