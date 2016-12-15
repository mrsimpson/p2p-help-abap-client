REPORT zp2ph_extract_transactions.

CLASS lcl_extractor DEFINITION.
  PUBLIC SECTION.
    METHODS export_xml.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_package_inf,
            obj_name TYPE tadir-obj_name,
            devclass TYPE tdevc-devclass,
            component TYPE df14l-ps_posid,
           END OF ty_package_inf.

    TYPES: BEGIN OF ty_transaction,
                transaction_code      TYPE tstc-tcode,
                program               TYPE tstc-pgmna,
                package               TYPE ty_package_inf-devclass,
                application_component TYPE ty_package_inf-component,
           END OF ty_transaction,
           BEGIN OF ty_short_text,
                language(2) TYPE c,
                text        TYPE tstct-ttext,
           END OF ty_short_text,
           tt_short_text TYPE SORTED TABLE OF ty_short_text WITH UNIQUE KEY language.

    TYPES: BEGIN OF ty_transaction_metadata.
    INCLUDE TYPE ty_transaction AS transaction.
    TYPES: short_texts TYPE tt_short_text,
           END OF ty_transaction_metadata,
           tt_transaction_metadata TYPE SORTED TABLE OF ty_transaction_metadata WITH UNIQUE KEY transaction_code.

    METHODS get_data
        EXPORTING
            et_transaction_metadata TYPE tt_transaction_metadata.

ENDCLASS.

CLASS lcl_extractor IMPLEMENTATION.

  METHOD get_data.
    CLEAR et_transaction_metadata.

    DATA lt_tstc TYPE STANDARD TABLE OF tstc WITH DEFAULT KEY.
    DATA lt_tstct TYPE STANDARD TABLE OF tstct WITH DEFAULT KEY WITH NON-UNIQUE SORTED KEY tx COMPONENTS tcode.
    DATA lt_package_inf TYPE SORTED TABLE OF ty_package_inf WITH UNIQUE KEY primary_key COMPONENTS obj_name.

    SELECT * FROM tstc INTO TABLE lt_tstc.
    SELECT * FROM tstct INTO TABLE lt_tstct
        WHERE   sprsl = 'E'
            OR  sprsl = 'D'.

    SELECT t~obj_name p~devclass a~ps_posid AS component FROM tadir AS t
        INNER JOIN tdevc AS p ON
          t~devclass = p~devclass
        INNER JOIN df14l AS a ON
          p~component = a~fctr_id
*         and a~as4local = sy-langu
        INTO TABLE lt_package_inf
        WHERE   t~pgmid   = 'R3TR'
          AND   t~object  = 'TRAN'.

    DATA ls_transaction_metadata LIKE LINE OF et_transaction_metadata.
    DATA ls_short_text LIKE LINE OF ls_transaction_metadata-short_texts.
    LOOP AT lt_tstc ASSIGNING FIELD-SYMBOL(<ls_tstc>).
      CLEAR ls_transaction_metadata.
      ls_transaction_metadata-transaction_code = <ls_tstc>-tcode.

      READ TABLE lt_package_inf ASSIGNING FIELD-SYMBOL(<ls_package_inf>) WITH KEY obj_name = <ls_tstc>-tcode.
      IF sy-subrc = 0.
        ls_transaction_metadata-application_component = <ls_package_inf>-component.
        ls_transaction_metadata-package = <ls_package_inf>-devclass.
      ELSE.
        ASSERT 1 = 1.
      ENDIF.

      LOOP AT lt_tstct ASSIGNING FIELD-SYMBOL(<ls_tstct>) USING KEY tx WHERE tcode = <ls_tstc>-tcode.
        CLEAR ls_short_text.
        CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
          EXPORTING
            input  = <ls_tstct>-sprsl
          IMPORTING
            output = ls_short_text-language.

        ls_short_text-text = <ls_tstct>-ttext.
        INSERT ls_short_text INTO TABLE ls_transaction_metadata-short_texts.

      ENDLOOP.

      INSERT ls_transaction_metadata INTO TABLE et_transaction_metadata.

    ENDLOOP.

  ENDMETHOD.


  METHOD export_xml.

    me->get_data(
      IMPORTING
        et_transaction_metadata = DATA(lt_transactions)
    ).

    DATA lv_xml_string TYPE string.
    DATA lt_xml_string TYPE STANDARD TABLE OF string.

    CALL TRANSFORMATION id
        SOURCE transactions = lt_transactions
        RESULT XML lv_xml_string.

    APPEND lv_xml_string TO lt_xml_string.
    DATA lv_filename TYPE string.
    DATA lv_path TYPE string.
    DATA lv_fullpath TYPE string.
    DATA lv_action LIKE cl_gui_frontend_services=>action_ok.

    cl_gui_frontend_services=>file_save_dialog(
*      EXPORTING
*        window_title              =     " Window Title
*        default_extension         =     " Default Extension
*        default_file_name         =     " Default File Name
*        with_encoding             =
*        file_filter               =     " File Type Filter Table
*        initial_directory         =     " Initial Directory
*        prompt_on_overwrite       = 'X'
      CHANGING
        filename                  =  lv_filename
        path                      =  lv_path
        fullpath                  =  lv_fullpath
        user_action               =  lv_action   " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*        file_encoding             =
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF lv_action NE cl_gui_frontend_services=>action_cancel.
        cl_gui_frontend_services=>gui_download(
          EXPORTING
            filename                  = lv_fullpath
          CHANGING
            data_tab                  =  lt_xml_string
          EXCEPTIONS
            file_write_error          = 1
            no_batch                  = 2
            gui_refuse_filetransfer   = 3
            invalid_type              = 4
            no_authority              = 5
            unknown_error             = 6
            header_not_allowed        = 7
            separator_not_allowed     = 8
            filesize_not_allowed      = 9
            header_too_long           = 10
            dp_error_create           = 11
            dp_error_send             = 12
            dp_error_write            = 13
            unknown_dp_error          = 14
            access_denied             = 15
            dp_out_of_memory          = 16
            disk_full                 = 17
            dp_timeout                = 18
            file_not_found            = 19
            dataprovider_exception    = 20
            control_flush_error       = 21
            not_supported_by_gui      = 22
            error_no_gui              = 23
            OTHERS                    = 24
        ).
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  NEW lcl_extractor( )->export_xml( ).