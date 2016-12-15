class ZCL_P2PH_EXP_AUTH_CHECK definition
  public
  inheriting from /BOBF/CL_LIB_AUTHORITY_CHECK
  final
  create public .

public section.

  methods CHECK_AUTHORITY
    redefinition .
  methods CHECK_AUTHORITY_STATICALLY
    redefinition .
protected section.
private section.

  methods SKIP_AUTH_CHECK
    returning
      value(RV_SKIP) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_P2PH_EXP_AUTH_CHECK IMPLEMENTATION.


METHOD check_authority.
  CLEAR: et_failed_key, eo_message.

  IF me->skip_auth_check( ) = abap_true.
    CLEAR: et_failed_key, eo_message.
  ELSE.
    super->check_authority(
      EXPORTING
        is_ctx        = is_ctx    " Context Information for Validations
        it_key        = it_key    " Key Table
        io_read       = io_read    " Interface to Read Data
      IMPORTING
        et_failed_key = et_failed_key    " Key Table
        eo_message    = eo_message    " Interface of Message Object
    ).
  ENDIF.
ENDMETHOD.


  METHOD check_authority_statically.
    CLEAR: ev_failed.

    IF me->skip_auth_check( ) = abap_true.
      ev_failed = abap_false.
    ELSE.
      super->check_authority_statically(
        EXPORTING
          is_ctx     = is_ctx
        IMPORTING
          ev_failed  = ev_failed
          eo_message = eo_message
             ).
    ENDIF.
  ENDMETHOD.


  METHOD skip_auth_check.

    rv_skip = abap_true.

  ENDMETHOD.
ENDCLASS.