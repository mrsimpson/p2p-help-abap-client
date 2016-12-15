CLASS lcl_sql_statement_test DEFINITION INHERITING FROM cl_sql_statement.
  PUBLIC SECTION.
    METHODS get_parameter_value
        IMPORTING
            iv_param_index TYPE i
        RETURNING value(rv_value) TYPE REF TO data.
ENDCLASS.

CLASS lcl_sql_statement_test IMPLEMENTATION.

  METHOD get_parameter_value.
    DATA ls_param LIKE LINE OF parameters->param_tab.
    READ TABLE parameters->param_tab INTO ls_param INDEX iv_param_index.
    cl_abap_unit_assert=>assert_equals( msg = |No parameter with index number { iv_param_index } found in sql statement| exp = 0 act = sy-subrc ).

    rv_value = ls_param-data_ref.
  ENDMETHOD.
ENDCLASS.

CLASS lct_generic_query DEFINITION
  INHERITING FROM zcl_p2ph_q_db_view
  ABSTRACT.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_selection,
            fieldname TYPE fieldname,
            sign      TYPE c LENGTH 1,
            option    TYPE c LENGTH 2,
            low       TYPE string,
            high      TYPE string,
           END OF ty_selection,
           tt_selection TYPE SORTED TABLE OF ty_selection WITH NON-UNIQUE KEY fieldname.

    METHODS get_where_range
          IMPORTING it_selection TYPE tt_selection
          EXPORTING
            et_where        TYPE tt_string
            er_range        TYPE REF TO data.

    METHODS create_sql_statement
        RETURNING value(ro_sql_statement) TYPE REF TO lcl_sql_statement_test.

    METHODS reduce_where_for_comparison
        IMPORTING
            iv_where TYPE string
        RETURNING value(rv_compacted) TYPE string.

    METHODS validate_parameter
        IMPORTING
            io_statement        TYPE REF TO lcl_sql_statement_test
            iv_param_index      TYPE i
            iv_expected_string  TYPE string.

  PRIVATE SECTION.
    METHODS get_range_descriptor
      IMPORTING it_selection TYPE tt_selection
      RETURNING value(ro_ranges_descr) TYPE REF TO cl_abap_structdescr.


ENDCLASS.

CLASS lct_generic_query IMPLEMENTATION.

  METHOD create_sql_statement.
    CREATE OBJECT ro_sql_statement.
  ENDMETHOD.

  METHOD get_range_descriptor.

    DATA lt_component_selection TYPE cl_abap_structdescr=>component_table.
    DATA ls_component_selection LIKE LINE OF lt_component_selection.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_field.

*    the range descriptor has one component per field on which at least one selection criterion
*    is specified. This component is a range table of the field's type.
*    If multiple criteria for the same attribute exist, the component's range tab has multiple entries

    DATA ls_selection LIKE LINE OF it_selection.

    CLEAR lt_component_selection.
    LOOP AT it_selection INTO ls_selection.
      AT NEW fieldname.
        DATA lt_component_range TYPE cl_abap_structdescr=>component_table.
        DATA ls_component_range LIKE LINE OF lt_component_range.
        DATA lo_structdescr_range TYPE REF TO cl_abap_structdescr.

        READ TABLE mt_field ASSIGNING <ls_field> WITH KEY view_fieldname COMPONENTS view_fieldname = ls_selection-fieldname.
        ASSERT sy-subrc = 0.
        CLEAR lt_component_range.

        CLEAR ls_component_range.
        ls_component_range-name = 'SIGN'.
        ls_component_range-type ?= cl_abap_typedescr=>describe_by_data( ls_selection-sign ).
        INSERT ls_component_range INTO TABLE lt_component_range.

        CLEAR ls_component_range.
        ls_component_range-name = 'OPTION'.
        ls_component_range-type ?= cl_abap_typedescr=>describe_by_data( ls_selection-option ).
        INSERT ls_component_range INTO TABLE lt_component_range.

        CLEAR ls_component_range.
        ls_component_range-name = 'LOW'.
        ls_component_range-type ?= cl_abap_typedescr=>describe_by_name( |{ <ls_field>-source_table }-{ <ls_field>-source_fieldname }| ).
        INSERT ls_component_range INTO TABLE lt_component_range.

        CLEAR ls_component_range.
        ls_component_range-name = 'HIGH'.
***        ls_component_range-type ?= cl_abap_typedescr=>describe_by_data( <lv_view_field> ).
        ls_component_range-type ?= cl_abap_typedescr=>describe_by_name( |{ <ls_field>-source_table }-{ <ls_field>-source_fieldname }| ).
        APPEND ls_component_range TO lt_component_range.

        TRY.
            lo_structdescr_range = cl_abap_structdescr=>get(
                p_components = lt_component_range
            ).
          CATCH cx_sy_struct_creation.  " Exception when creating a structure description
            ASSERT 1 = 0.
        ENDTRY.

        ls_component_selection-name = ls_selection-fieldname.
        TRY.
            ls_component_selection-type = cl_abap_tabledescr=>get(
                p_line_type  = lo_structdescr_range
                p_table_kind = cl_abap_tabledescr=>tablekind_std
                p_unique     = abap_false
                p_key_kind   = cl_abap_tabledescr=>keydefkind_default
            ).
          CATCH cx_sy_table_creation.  " Exception when Creating a Table Type
            ASSERT 1 = 0.
        ENDTRY.
        APPEND ls_component_selection TO lt_component_selection.

      ENDAT.

      TRY.
          ro_ranges_descr = cl_abap_structdescr=>get(
              p_components = lt_component_selection
          ).
        CATCH cx_sy_struct_creation.  " Exception when creating a structure description
          ASSERT 1 = 0.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_where_range.
    FIELD-SYMBOLS <ls_range>            TYPE any.
    DATA lr_range                       TYPE REF TO data.
    DATA lo_range_descriptor            TYPE REF TO cl_abap_structdescr.
    DATA lr_component_range             TYPE REF TO data.
    FIELD-SYMBOLS <lt_component_range>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_component_range>  TYPE any.
    FIELD-SYMBOLS <lv_sign>             TYPE ty_selection-sign.
    FIELD-SYMBOLS <lv_option>           TYPE ty_selection-option.
    FIELD-SYMBOLS <lv_low>              TYPE any.
    FIELD-SYMBOLS <lv_high>             TYPE any.

    CLEAR: et_where, er_range.

*    this methods creates a dynamic range-structure and fills it with the selection

    lo_range_descriptor = me->get_range_descriptor( it_selection ).

    CREATE DATA lr_range TYPE HANDLE lo_range_descriptor.
    ASSIGN lr_range->* TO <ls_range>.


    DATA ls_selection LIKE LINE OF it_selection.
    LOOP AT it_selection INTO ls_selection.
      AT NEW fieldname.
*        assign the corresponding segment of the range-structure so that the actual
*        criteria can be added to its range table
        ASSIGN COMPONENT ls_selection-fieldname OF STRUCTURE <ls_range> TO <lt_component_range>.
      ENDAT.

      CREATE DATA lr_component_range LIKE LINE OF <lt_component_range>.
      ASSIGN lr_component_range->* TO <ls_component_range>.


*        move selection criteria to the range table
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_component_range> TO <lv_sign>.
      <lv_sign> = ls_selection-sign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_component_range> TO <lv_option>.
      <lv_option> = ls_selection-option.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_component_range> TO <lv_low>.
      <lv_low> = ls_selection-low.
      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_component_range> TO <lv_high>.
      <lv_high> = ls_selection-high.

      APPEND <ls_component_range> TO <lt_component_range>.

    ENDLOOP.

    GET REFERENCE OF <ls_range> INTO er_range.

*    finally, construct the where-clause which refers to components of the range-tab:
*    [fieldname1] IN <LS_RANGE>-[fieldname1]
*    AND
*    [fieldname2] IN <LS_RANGE>-[fieldname2]

    LOOP AT it_selection INTO ls_selection.
      IF et_where IS NOT INITIAL.
        APPEND 'AND' TO et_where.
      ENDIF.
      DATA lv_where LIKE LINE OF et_where.
      lv_where = |{ ls_selection-fieldname } IN <LS_RANGE>-{ ls_selection-fieldname }|.
      APPEND lv_where TO et_where.
    ENDLOOP.

  ENDMETHOD.

  METHOD reduce_where_for_comparison.
    rv_compacted = |{ condense( replace( val = iv_where sub = `  ` with = ` ` ) ) }|.
  ENDMETHOD.

  METHOD validate_parameter.
    DATA lr_value TYPE REF TO data.
    FIELD-SYMBOLS <lv_string> TYPE any.

    lr_value = io_statement->get_parameter_value( iv_param_index ).

    ASSIGN lr_value->* TO <lv_string>.

    cl_abap_unit_assert=>assert_equals( msg = |Parameter with index { iv_param_index } of created statement does not meet expectations| exp = iv_expected_string act = <lv_string> ).

  ENDMETHOD.

ENDCLASS.