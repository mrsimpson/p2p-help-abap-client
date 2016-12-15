class ZCX_P2PH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MESSAGE_TEXT type STRING optional .
  class-methods CREATE_FROM_SYMSG
    returning
      value(RX_SYMSG) type ref to ZCX_P2PH .
  methods RAISE
    raising
      ZCX_P2PH .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.

  data MV_MESSAGE_TEXT type STRING .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_P2PH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD create_from_symsg.
    DATA lv_message_text TYPE string.
    IF sy-msgid IS NOT INITIAL AND sy-msgty IS NOT INITIAL AND sy-msgno IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message_text.

      CREATE OBJECT rx_symsg
        EXPORTING
          mv_message_text = lv_message_text.
    ENDIF.

  ENDMETHOD.


METHOD if_message~get_text.
  IF  if_t100_message~t100key EQ if_t100_message=>default_textid
    AND mv_message_text IS NOT INITIAL.

    result = mv_message_text.
  ELSE.
    super->if_message~get_text( ).
  ENDIF.
ENDMETHOD.


  METHOD raise.
    RAISE EXCEPTION me.
  ENDMETHOD.
ENDCLASS.