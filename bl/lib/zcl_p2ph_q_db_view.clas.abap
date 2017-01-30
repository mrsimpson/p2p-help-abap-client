class ZCL_P2PH_Q_DB_VIEW definition
  public
  inheriting from ZCL_P2PH_Q_OPEN_SQL
  abstract
  create public .

public section.

  types:
    BEGIN OF ty_field_ext_roll,
        source_table    TYPE tabname,
        view_fieldname  TYPE string,
        actual_rollname TYPE rollname,
        extend_rollname TYPE rollname,
      END OF ty_field_ext_roll .
  types:
    tt_field_ext_roll TYPE TABLE OF ty_field_ext_roll WITH NON-UNIQUE SORTED KEY view_fieldname COMPONENTS source_table view_fieldname .

  constants CO_ROLLNAME_COPID type ROLLNAME value 'ZPRI_COP_ID' ##NO_TEXT.

  methods CONSTRUCTOR .
protected section.

  types:
    tt_dd26v TYPE STANDARD TABLE OF dd26v WITH DEFAULT KEY .
  types:
    tt_dd28j TYPE  STANDARD TABLE OF dd28j WITH DEFAULT KEY .
  types:
    ty_join_type TYPE c LENGTH 1 .
  types:
    BEGIN OF ty_join,
      ltab      TYPE dd28j-ltab,
      rtab      TYPE dd28j-rtab,
      join_type TYPE ty_join_type,
    END OF ty_join .
  types:
    tt_join TYPE SORTED TABLE OF ty_join WITH UNIQUE KEY ltab rtab
                                               WITH NON-UNIQUE SORTED KEY reverse COMPONENTS rtab .
  types:
    BEGIN OF ty_join_sequence,
      index      LIKE sy-tabix,
      level      TYPE i,
      left_tab   TYPE tabname,
      right_tab  TYPE tabname,
      identifier TYPE tabname, "for dedicated joins: Allows to identify different joins between the same tables (like an alias)
      join       TYPE ty_join,
      string     TYPE string,
    END OF ty_join_sequence .
  types:
    tt_join_sequence TYPE SORTED TABLE OF ty_join_sequence WITH UNIQUE KEY index
                                                                 WITH NON-UNIQUE SORTED KEY left_tab COMPONENTS left_tab
                                                                 WITH NON-UNIQUE SORTED KEY right_tab COMPONENTS right_tab
                                                                 WITH NON-UNIQUE SORTED KEY level COMPONENTS level .
  types:
    BEGIN OF ty_join_criterion,
      source_fieldname TYPE fieldname,
      joined_fieldname TYPE fieldname,
    END OF ty_join_criterion .
  types:
    tt_join_criterion TYPE SORTED TABLE OF ty_join_criterion WITH UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ty_alias_spec_join_criterion,
      identifier TYPE tabname.
  INCLUDE TYPE ty_join_criterion AS join_criterion.
  TYPES  END OF ty_alias_spec_join_criterion .
  types:
    tt_alias_spec_join_criterion TYPE SORTED TABLE OF ty_alias_spec_join_criterion WITH UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ty_view_field_def,
      tab_fieldname TYPE fieldname,
      alias_name    TYPE fieldname,
    END OF ty_view_field_def .
  types:
    tt_view_field_def TYPE STANDARD TABLE OF ty_view_field_def WITH DEFAULT KEY .
  types:
    BEGIN OF ty_view_field,
      view_fieldname   TYPE string,
      source_table     TYPE tabname,
      source_fieldname TYPE fieldname,
      sql_field_alias  TYPE string,
* --> Step: 1 Begin of Insert by Iyanknnu 14 Jan 2015
      field_alias_name TYPE string,
* --> Step: 1 End of Insert by Iyanknnu 14 Jan 2015
      sql_table_alias  TYPE tabname,
      no_case          TYPE abap_bool,
      dd27p_ref        TYPE REF TO dd27p,
      is_boolean       TYPE abap_bool,
    END OF ty_view_field .
  types:
    ty_token_type TYPE c LENGTH 1 .
  types:
    ty_compilation_transformation TYPE c LENGTH 1 .
  types:
    BEGIN OF ty_parsed_where,
      index                      TYPE i,
      token                      TYPE string,
      token_type                 TYPE ty_token_type,
      compilation_transformation TYPE ty_compilation_transformation,
    END OF ty_parsed_where .
  types:
    tt_parsed_where TYPE SORTED TABLE OF ty_parsed_where WITH UNIQUE KEY index .
  types:
    BEGIN OF ty_table,
      tabname      TYPE tabname,
      t_field      TYPE rsdsfields_t,
      t_field_desc TYPE drf_t_field_descr,
      t_frange     TYPE rsds_frange_t,
    END OF ty_table .
  types:
    tt_table TYPE SORTED TABLE OF ty_table WITH UNIQUE KEY tabname .
  types:
    BEGIN OF ty_additional_token,
      insert_after_index TYPE i,
      token              TYPE ty_parsed_where,
    END OF ty_additional_token .
  types:
    tt_additional_token    TYPE STANDARD TABLE OF ty_additional_token WITH DEFAULT KEY .

  constants:
    BEGIN OF co_token_type,
      logical_expression TYPE ty_token_type VALUE 'L',      "#EC NOTEXT
      search_text        TYPE ty_token_type VALUE 'S',      "#EC NOTEXT
      operator           TYPE ty_token_type VALUE 'O',      "#EC NOTEXT
      fieldname          TYPE ty_token_type VALUE 'F',      "#EC NOTEXT
    END OF co_token_type .
  constants:
    BEGIN OF co_compilation_transformation,
      uppercase TYPE ty_compilation_transformation VALUE 'U',
      escape    TYPE ty_compilation_transformation VALUE 'E',
    END OF co_compilation_transformation .
  constants CO_STRING_PREFIX_ALIAS type STRING value 'T' ##NO_TEXT.
  constants CO_ALIAS_ANCHOR_TABLE type STRING value 'T0' ##NO_TEXT.
  constants CO_STRING_AS type STRING value 'as' ##NO_TEXT.
  constants CO_LEFT_OUTER type TY_JOIN_TYPE value 'L' ##NO_TEXT.
  constants CO_INNER type TY_JOIN_TYPE value 'I' ##NO_TEXT.
  constants CO_DOUBLE_HYPHEN type STRING value '&double_hyphen&' ##NO_TEXT.
  constants CO_ESCAPE_TOKEN type STRING value '&escape&' ##NO_TEXT.
  constants CO_PRIMA_DOMAIN type DOMNAME value 'ZPRI_ID' ##NO_TEXT.
  data MV_JOIN_TYPE type TY_JOIN_TYPE .
  data MV_DB_VIEW_NAME type TABNAME .
  data:
    mt_field TYPE TABLE OF ty_view_field
          WITH NON-UNIQUE SORTED KEY view_fieldname COMPONENTS view_fieldname
          WITH NON-UNIQUE SORTED KEY sql_field_alias COMPONENTS sql_field_alias
          WITH NON-UNIQUE SORTED KEY no_case COMPONENTS no_case .

  methods EXTEND_FIELD_ROLLNAME
    importing
      value(IV_FIELD_NAME) type STRING
      value(IV_ACTUAL_ROLLNAME) type ROLLNAME
    exporting
      !EV_CHANGED type BOOLE_D
      value(EV_ROLLNAME) type ROLLNAME .
  methods ADD_DEDICATED_FIELDS
    importing
      !IT_FIELD_DEF type TT_VIEW_FIELD_DEF
      !IV_SOURCE_TABLE type TABNAME
      !IV_ALIAS type TABNAME
      !IV_JOINED_FROM_TABLE type TABNAME
      !IV_JOIN_TYPE type TY_JOIN_TYPE default CO_LEFT_OUTER
      !IT_JOIN_CRITERION type TT_JOIN_CRITERION .
  methods ADJUST_PARSED_WHERE
    importing
      !IV_TABNAME type TABNAME
    changing
      !CT_PARSED_WHERE type TT_PARSED_WHERE .
  methods INITIALIZE .
  methods GET_WHERE_CLAUSE
    importing
      !IT_WHERE type TT_STRING
      !IS_RANGE type ANY
      !IO_STATEMENT type ref to CL_SQL_STATEMENT
    returning
      value(RV_WHERE_CLAUSE) type STRING .
  methods ADD_ADDITIONAL_TOKENS
    importing
      !IT_PARSED_WHERE type TT_PARSED_WHERE
      !IT_ADDITIONAL_TOKEN type TT_ADDITIONAL_TOKEN
    exporting
      !ET_PARSED_WHERE type TT_PARSED_WHERE .

  methods ADAPT_SELECTION_PARAMETERS
    final redefinition .
  methods DO_SELECT
    redefinition .
private section.

  data MT_DD26V type TT_DD26V .
  data MT_DD28J type TT_DD28J .
  data MT_DD27P type DD27PTAB .
  data MT_JOIN_SEQUENCE type TT_JOIN_SEQUENCE .
  data MO_CONNECTION type ref to CL_SQL_CONNECTION .
  data MV_DBMS type DBCON_DBMS .
  data MT_DEDICATED_FIELD_DEF type DD27PTAB .
  data MT_DEDICATED_JOIN type TT_JOIN_SEQUENCE .
  data MT_DEDICATED_JOIN_CRITERION type TT_ALIAS_SPEC_JOIN_CRITERION .
  data MT_FIELD_DOMAIN_EXT type TT_FIELD_EXT_ROLL .

  methods ESCAPE_ALIASES
    changing
      !CT_SELECT_RANGES type RSDS_TRANGE
      !CT_FIELD type RSDSFIELDS_T
      !CT_FIELD_DESC type DRF_T_FIELD_DESCR .
  methods UNESCAPE_ALIASES
    changing
      !CT_SELECT_RANGES type RSDS_TWHERE .
  methods ADJUST_WHERE .   "#EC WARNOK - Obsolete - Only kept for versioning purposes
  methods ANALYZE_DDIC_DEFINITION .
  methods ANALYZE_FIELDS .
  methods COMPILE_FROM_CLAUSE .
  methods COMPILE_PARSED_WHERE
    importing
      !IT_PARSED_WHERE type TT_PARSED_WHERE
      !IO_STATEMENT type ref to CL_SQL_STATEMENT
    returning
      value(RV_WHERE_CLAUSE) type STRING .
  methods DETERMINE_JOINS .
  methods DO_SELECT_NATIVE_SQL
    importing
      !IT_WHERE type TT_STRING
      !IT_ORDER_BY type TT_STRING              "for further enhancement
      !IV_MAX_ROWS type I
      !IS_RANGE type ANY
    exporting
      !ET_KEY type /BOBF/T_FRW_KEY
      !EV_COUNT type SYST-DBCNT
      !EV_RC type SUBRC .
  methods GET_WHERE_CLIENT
    returning
      value(RV_WHERE_CLIENT) type STRING .
  methods PARSE_WHERE_BY_TABLE
    importing
      !IS_WHERE_BY_TABLE type RSDS_WHERE
    returning
      value(RT_PARSED_WHERE) type TT_PARSED_WHERE .
  methods GET_SELECT_RANGE_FOR_TABLE
    importing
      !IS_RANGE type ANY
    exporting
      !ET_TABLE type TT_TABLE .
ENDCLASS.



CLASS ZCL_P2PH_Q_DB_VIEW IMPLEMENTATION.


  METHOD ADAPT_SELECTION_PARAMETERS.
    DATA ls_additional_sel_param LIKE LINE OF ct_selection_parameter.
    DATA lt_selection_param TYPE /bobf/t_frw_query_selparam.
    DATA lt_selparam_adapted TYPE /bobf/t_frw_query_selparam.
    DATA lv_return TYPE boole_d.
    " this conversion of selection option might cause the performance issue onthe
    " selection so weeed to restrict the adoption . So now we allow this adaptation
    " only for the VIEW abse table and Base table attributes which has the Domain type ZPRI_ID

    LOOP AT mt_field_domain_ext INTO DATA(ls_field_domain_ext).
      " could be change to request the same attribute with ' * '
      " multiple times

      LOOP AT ct_selection_parameter INTO ls_additional_sel_param
                  WHERE attribute_name = ls_field_domain_ext-view_fieldname
                    AND sign = 'I' ##no_text
                    AND option = 'EQ' ##no_text
                    AND ( low CS '*' ##no_text
                       OR high CS '*' ##no_text
                        ).

        DATA(lv_index) = sy-tabix.
        " this below logic will build the combination of select option table
        " like below Exaample if the id has the search value as 3* the attribute length is
        " 10 (Domain length 10 char) we adapt the selection value like below
        " the Combination will build 10 times
*--------------------------------------------------------------------*
*I  CP  3*               " this will lead us to some performance issue
*I  CP  03*              " if we allow this combination build for all
*I  CP  003*             " all the numeric attribute to this process
*I  CP  0003*            " so we restrict this conversion with respect
*I  CP  00003*           " to ID field
*I  CP  000003*
*I  CP  0000003*
*I  CP  00000003*
*I  CP  000000003*
*I  CP  0000000003*
*--------------------------------------------------------------------*
        CLEAR lt_selection_param.

        CALL FUNCTION 'ZPRI_SELOPT_EXPAND_CONVEXIT'
          EXPORTING
            iv_view_name           = mv_db_view_name
            is_selection_param     = ls_additional_sel_param
          IMPORTING
            ev_failed              = lv_return
            et_selection_parameter = lt_selection_param.


        IF lv_return IS INITIAL.
          " conversion done successfully.
          DELETE ct_selection_parameter INDEX lv_index.
*          DELETE ct_selection_parameter WHERE attribute_name = ls_field_domain_ext-view_fieldname
*                                          AND sign = zif_pri_generic_constants=>sc_sign-include
*                                          AND option = zif_pri_generic_constants=>sc_option-equal .
          " append the value to same table inside the loop will cause the
          " infinity loop
          APPEND LINES OF lt_selection_param TO lt_selparam_adapted.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " all the adapted selection param value to the final query parameter table
    IF lt_selparam_adapted IS NOT INITIAL.
      APPEND LINES OF lt_selparam_adapted TO ct_selection_parameter.
    ENDIF.
  ENDMETHOD.


  METHOD ADD_ADDITIONAL_TOKENS.

    FIELD-SYMBOLS <ls_parsed_where> LIKE LINE OF et_parsed_where.

    IF it_additional_token IS NOT INITIAL.
      CLEAR et_parsed_where.

*    We need to re-construct the tokens-table in order to have unique indices
      DATA ls_combined_parsed_where LIKE LINE OF et_parsed_where.
      DATA lv_index LIKE <ls_parsed_where>-index.
      DATA ls_additional_token LIKE LINE OF it_additional_token.
      et_parsed_where = it_parsed_where.

      CLEAR et_parsed_where.
      LOOP AT it_parsed_where ASSIGNING <ls_parsed_where>.
        ADD 1 TO lv_index.
        ls_combined_parsed_where = <ls_parsed_where>.
        ls_combined_parsed_where-index = lv_index.
        INSERT ls_combined_parsed_where INTO TABLE et_parsed_where.

        LOOP AT it_additional_token INTO ls_additional_token WHERE insert_after_index = <ls_parsed_where>-index.
          ADD 1 TO lv_index.
          ls_combined_parsed_where = ls_additional_token-token.
          ls_combined_parsed_where-index = lv_index.
          INSERT ls_combined_parsed_where INTO TABLE et_parsed_where.
        ENDLOOP.

      ENDLOOP.
    ELSE.
      et_parsed_where = it_parsed_where.
    ENDIF.
  ENDMETHOD.


  METHOD ADD_DEDICATED_FIELDS.

    DATA ls_dedicated_join              LIKE LINE OF mt_dedicated_join.
    DATA ls_dedicated_join_criterion    LIKE LINE OF mt_dedicated_join_criterion.
    DATA ls_dedicated_field_def         LIKE LINE OF mt_dedicated_field_def.
    DATA lt_dd03p                       TYPE dd03ptab.
    DATA lv_tab_as_objname              TYPE ddobjname.
    DATA ls_field_def                   LIKE LINE OF it_field_def.

    FIELD-SYMBOLS <ls_dd03p>            LIKE LINE OF lt_dd03p.

*  1) Get the definition of the field in the table.
*  This is necessary since we need type-information of the view-field as well.
    lv_tab_as_objname = iv_source_table. "Another move bites the dust
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_tab_as_objname
      TABLES
        dd03p_tab     = lt_dd03p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0. "Table has to be valid and active

*  translate each relevant field of the joined table to
*  a pseudo dedicated view-field
    LOOP AT it_field_def INTO ls_field_def.
      READ TABLE lt_dd03p ASSIGNING <ls_dd03p> WITH KEY fieldname = ls_field_def-tab_fieldname.
      ASSERT sy-subrc = 0. "Field has to be part of the table

      CLEAR ls_dedicated_field_def.
      ls_dedicated_field_def-viewfield = ls_field_def-alias_name.

*    Mis-use the viewname-attribute as container for the alias with whose help
*    the actual join can be identified lateron
      ls_dedicated_field_def-viewname = iv_alias.

*    Transport the type information from the table field-definition
*    to a view-field-definition
      MOVE-CORRESPONDING <ls_dd03p> TO ls_dedicated_field_def.

*    Some Meta-Data in the view deviates from the Meta-Data in the table
      IF ls_dedicated_field_def-flength IS INITIAL.
        IF ls_dedicated_field_def-intlen IS NOT INITIAL.
          ls_dedicated_field_def-flength = ls_dedicated_field_def-intlen.
        ENDIF.
      ENDIF.

      INSERT ls_dedicated_field_def INTO TABLE mt_dedicated_field_def.

    ENDLOOP.


*  2)Register the join and its criteria
    CLEAR ls_dedicated_join.
    ls_dedicated_join-index = lines( mt_join_sequence ) + lines( mt_dedicated_join ) + 1.
    ls_dedicated_join-identifier = iv_alias.
    ls_dedicated_join-join-join_type = iv_join_type.
    ls_dedicated_join-left_tab = ls_dedicated_join-join-ltab = iv_joined_from_table.
    ls_dedicated_join-right_tab = ls_dedicated_join-join-rtab = iv_source_table.
    INSERT ls_dedicated_join INTO TABLE mt_dedicated_join.

    LOOP AT it_join_criterion INTO ls_dedicated_join_criterion-join_criterion.
      ls_dedicated_join_criterion-identifier = iv_alias.
      INSERT ls_dedicated_join_criterion INTO TABLE mt_dedicated_join_criterion.
    ENDLOOP.

  ENDMETHOD.


  METHOD ADJUST_PARSED_WHERE.

*    There are requirements which make it necessary to adopt particular tokens of the where-clause.
*    In addition, as the selection is executed as native SQL, some DB-specific changes need to be performed

    DATA lv_current_operator    TYPE string.
    DATA: BEGIN OF ls_additional_token,
            insert_after_index TYPE i,
            token              TYPE ty_parsed_where,
          END OF ls_additional_token.
    DATA lt_additional_token    LIKE STANDARD TABLE OF ls_additional_token WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_current_view_field> LIKE LINE OF mt_field.
    FIELD-SYMBOLS <ls_parsed_where>       LIKE LINE OF ct_parsed_where.

    LOOP AT ct_parsed_where ASSIGNING <ls_parsed_where>.

      CASE <ls_parsed_where>-token_type.
        WHEN co_token_type-logical_expression.
          CONTINUE. "The sequence and how the tokens are connected logically must not change
        WHEN co_token_type-operator.
          lv_current_operator = <ls_parsed_where>-token.

*replace operators
          REPLACE ALL OCCURRENCES OF 'EQ' IN <ls_parsed_where>-token WITH '='.
          REPLACE ALL OCCURRENCES OF 'BT' IN <ls_parsed_where>-token WITH 'BETWEEN'.
          REPLACE ALL OCCURRENCES OF 'NB' IN <ls_parsed_where>-token WITH 'NOT BETWEEN'.
          REPLACE ALL OCCURRENCES OF 'CP' IN <ls_parsed_where>-token WITH 'LIKE'.
          REPLACE ALL OCCURRENCES OF 'LE' IN <ls_parsed_where>-token WITH '<='.
          REPLACE ALL OCCURRENCES OF 'GE' IN <ls_parsed_where>-token WITH '>='.
          REPLACE ALL OCCURRENCES OF 'NE' IN <ls_parsed_where>-token WITH '<>'.
          REPLACE ALL OCCURRENCES OF 'NP' IN <ls_parsed_where>-token WITH 'NOT LIKE'.
          REPLACE ALL OCCURRENCES OF 'GT' IN <ls_parsed_where>-token WITH '>'.
          REPLACE ALL OCCURRENCES OF 'LT' IN <ls_parsed_where>-token WITH '<'.

        WHEN co_token_type-fieldname.
          READ TABLE mt_field ASSIGNING <ls_current_view_field> WITH KEY sql_field_alias COMPONENTS sql_field_alias = <ls_parsed_where>-token.
          ASSERT sy-subrc = 0.

*          Case-insensitive search: Instruct the DB to transform the attribute to uppercase
*            if the current attribute shall be searched independent of the case of the text
          READ TABLE mt_field TRANSPORTING NO FIELDS WITH KEY sql_field_alias COMPONENTS
            sql_field_alias = <ls_parsed_where>-token
            no_case = abap_true.
          IF sy-subrc = 0.
            <ls_parsed_where>-compilation_transformation = co_compilation_transformation-uppercase.
          ENDIF.

        WHEN co_token_type-search_text.
*      The wildcard in OpenSQL ("*") deviates from the SQL-standard ("%")
          REPLACE ALL OCCURRENCES OF '*' IN <ls_parsed_where>-token WITH '%'.


*   all joined tables of which attributes are requested to be initial
*   need to be added an "IS NULL" as alternative, since this expresses that a join does not resolve
*   in order to do that, we need to know what would be the initial value of the attribute
          DATA lr_initial_value TYPE REF TO data.
          FIELD-SYMBOLS <lv_initial> TYPE any.

          CREATE DATA lr_initial_value TYPE (<ls_current_view_field>-dd27p_ref->rollname).
          ASSIGN lr_initial_value->* TO <lv_initial>.

          IF lv_current_operator = 'EQ'
              AND condense(  <ls_parsed_where>-token ) = <lv_initial>
              AND iv_tabname <> mv_anchor_table
              AND <ls_current_view_field>-is_boolean = abap_false.
            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 1.
            ls_additional_token-token-token_type = co_token_type-logical_expression.
            ls_additional_token-token-token = 'OR'.
            APPEND ls_additional_token TO lt_additional_token.

            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 2.
            ls_additional_token-token-token_type = co_token_type-fieldname.
            ls_additional_token-token-token = <ls_current_view_field>-sql_field_alias.
            APPEND ls_additional_token TO lt_additional_token.

            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 3.
            ls_additional_token-token-token_type = co_token_type-operator.
            ls_additional_token-token-token = 'IS NULL'.
            APPEND ls_additional_token TO lt_additional_token.
          ENDIF.


          IF lv_current_operator = 'NE'
              AND condense(  <ls_parsed_where>-token ) EQ <lv_initial>
*              AND condense(  <ls_parsed_where>-token ) <> <lv_initial>
              AND iv_tabname <> mv_anchor_table
              AND <ls_current_view_field>-is_boolean = abap_false .
            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 1.
            ls_additional_token-token-token_type = co_token_type-logical_expression.
            ls_additional_token-token-token = 'OR'.
            APPEND ls_additional_token TO lt_additional_token.

            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 2.
            ls_additional_token-token-token_type = co_token_type-fieldname.
            ls_additional_token-token-token = <ls_current_view_field>-sql_field_alias.
            APPEND ls_additional_token TO lt_additional_token.

            ls_additional_token-insert_after_index = <ls_parsed_where>-index.
            ls_additional_token-token-index = 3.
            ls_additional_token-token-token_type = co_token_type-operator.
            ls_additional_token-token-token = 'IS NOT NULL'.
            APPEND ls_additional_token TO lt_additional_token.
          ENDIF.

          IF <ls_current_view_field>-is_boolean = abap_false.
            <ls_parsed_where>-token = |{ condense( <ls_parsed_where>-token ) }|.
*          Case-insensitive search: Transform the term itself to upper-case
            IF <ls_current_view_field>-no_case = abap_true.
              TRANSLATE <ls_parsed_where>-token TO UPPER CASE.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.
    ENDLOOP.

*    finally, insert the additional tokens to the complete sequence - at the right place ;)
    IF lt_additional_token IS NOT INITIAL.
      DATA lt_combined_parsed_where LIKE ct_parsed_where.
      me->add_additional_tokens(
        EXPORTING
          it_parsed_where     = ct_parsed_where
          it_additional_token = lt_additional_token
        IMPORTING
          et_parsed_where = lt_combined_parsed_where
      ).

      ct_parsed_where = lt_combined_parsed_where.
    ENDIF.

  ENDMETHOD.


  METHOD ADJUST_WHERE.
    ASSERT 1 = 0.

*    METHOD is obsolete - ONLY kept IN the CLASS for being able TO READ PREVIOUS versions OF the METHOD

  ENDMETHOD.


  METHOD ANALYZE_DDIC_DEFINITION.

*  we need to read the view definition and then translate
*  all the modeled joins to ON-statements

    FIELD-SYMBOLS <ls_dd27p>  LIKE LINE OF mt_dd27p.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = mv_db_view_name
        state         = 'A'
      TABLES
        dd26v_tab     = mt_dd26v "View header
        dd27p_tab     = mt_dd27p "View fields
        dd28j_tab     = mt_dd28j "Conditions
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0. "The DB-view has to exist in an active version

*  Determine anchor table
    READ TABLE mt_dd27p ASSIGNING <ls_dd27p> WITH KEY viewfield = /bobf/if_conf_c=>sc_attribute_name_db_key.
    ASSERT sy-subrc = 0.
    mv_anchor_table = <ls_dd27p>-tabname.

*  create an ordered sequence of joins
*  hint: If there are joins necessary which cannot be expressed when modling the view in DDIC,
*  these fields can be added as "dedicated fields".
*  during the subsequent methods, these additional join-paths are being integrated
    me->determine_joins( ).

    me->analyze_fields( ).

*    after the fields are known, we can compile the from clause, as the view fields for the ON-criteria might be aliased
    me->compile_from_clause( ).

  ENDMETHOD.


  METHOD ANALYZE_FIELDS.

    DATA lo_typedesc_actual TYPE REF TO cl_abap_typedescr.
    DATA lo_typedesc_extend TYPE REF TO cl_abap_typedescr.
    DATA lv_changed         TYPE boole_d.

    FIELD-SYMBOLS <lt_dd27p> LIKE mt_dd27p.
    FIELD-SYMBOLS <lt_join_sequence> LIKE mt_join_sequence.
    FIELD-SYMBOLS <ls_join_sequence> LIKE LINE OF <lt_join_sequence>.
    DATA lr_dd27p TYPE REF TO dd27p.
    DATA ls_view_field LIKE LINE OF mt_field.
    DATA ls_field_ext_roll TYPE ty_field_ext_roll.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN mt_dd27p TO <lt_dd27p>.
          ASSIGN mt_join_sequence TO <lt_join_sequence>.
        WHEN 2.
          ASSIGN mt_dedicated_field_def TO <lt_dd27p>.
          ASSIGN mt_dedicated_join TO <lt_join_sequence>.
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

      LOOP AT <lt_dd27p> REFERENCE INTO lr_dd27p.
        CLEAR ls_view_field.

*            get the field-alias under which the view-field is addressed technically during the select
        IF lr_dd27p->tabname = mv_anchor_table.
          ls_view_field-sql_field_alias = co_alias_anchor_table && '.' && lr_dd27p->fieldname.
* --> Step: 2 Begin of Insert by Iyanknnu 14 Jan 2015
          ls_view_field-field_alias_name = co_alias_anchor_table && '~' && lr_dd27p->fieldname.
* --> Step: 2 end of Insert by Iyanknnu 14 Jan 2015
          ls_view_field-sql_table_alias = co_alias_anchor_table.
        ELSE.
          IF sy-index = 1.
            READ TABLE <lt_join_sequence> ASSIGNING <ls_join_sequence> WITH KEY right_tab COMPONENTS right_tab = lr_dd27p->tabname .
          ELSE.
*      In the case of dedicated fields, their tables can be joined multiple times with
*      different roles (aliases) => The viewname-attribute is misused in ADD_DEDICATED_FIELDS
*      in order to buffer the alias
            READ TABLE <lt_join_sequence> ASSIGNING <ls_join_sequence> WITH KEY identifier = lr_dd27p->viewname. "#EC CI_SORTSEQ
            ASSERT sy-subrc = 0.
          ENDIF.
          ls_view_field-sql_field_alias = <ls_join_sequence>-identifier && '.' && lr_dd27p->fieldname.
* --> Step: 3 Begin of Insert by Iyanknnu 14 Jan 2015
          ls_view_field-field_alias_name = <ls_join_sequence>-identifier && '~' && lr_dd27p->fieldname.
* --> Step: 3 end of Insert by Iyanknnu 14 Jan 2015
          ls_view_field-sql_table_alias = <ls_join_sequence>-identifier.
        ENDIF.

        ls_view_field-source_table = lr_dd27p->tabname.
        ls_view_field-source_fieldname = lr_dd27p->fieldname.
        ls_view_field-view_fieldname = lr_dd27p->viewfield.

        IF lr_dd27p->lowercase = abap_true.
          ls_view_field-no_case = abap_true.
        ENDIF.

        IF lr_dd27p->domname EQ 'BOOLE_D'  ##no_text
            OR lr_dd27p->domname EQ 'SAP_BOOL'  ##no_text
            OR lr_dd27p->domname EQ 'BOOLEAN_FLG'  ##no_text
            .
          ls_view_field-is_boolean = abap_true.
        ENDIF.

        ls_view_field-dd27p_ref = lr_dd27p.
        INSERT ls_view_field INTO TABLE mt_field.

* make sure the below sub class has to tell the roll name adaptation is done or NOt
* if not no need to append the value to member variabl
        ls_field_ext_roll = VALUE #( source_table = ls_view_field-source_table
                        view_fieldname = ls_view_field-view_fieldname
                        actual_rollname = lr_dd27p->rollname
                        ).
        extend_field_rollname(
          EXPORTING
            iv_field_name      = ls_field_ext_roll-view_fieldname    " Field Name
            iv_actual_rollname = ls_field_ext_roll-actual_rollname    " Data element
          IMPORTING
            ev_changed         = lv_changed    " Data element BOOLE: TRUE (='X') and FALSE (=' ')
            ev_rollname        = ls_field_ext_roll-extend_rollname    " Data element
        ).
        IF lv_changed IS NOT INITIAL.
          IF ls_field_ext_roll-actual_rollname NE ls_field_ext_roll-extend_rollname.
            " to make sure the the both roll element length is same else raise the assert
            lo_typedesc_actual = cl_abap_typedescr=>describe_by_name( ls_field_ext_roll-actual_rollname ).
            lo_typedesc_extend = cl_abap_typedescr=>describe_by_name( ls_field_ext_roll-extend_rollname ).

            ASSERT lo_typedesc_actual->length EQ lo_typedesc_extend->length.
          ENDIF.

          INSERT ls_field_ext_roll INTO TABLE mt_field_domain_ext.

        ENDIF.
        ASSERT sy-subrc = 0.
      ENDLOOP.
    ENDDO.
  ENDMETHOD.


  METHOD COMPILE_FROM_CLAUSE.
    DATA lt_join_sequence                   LIKE mt_join_sequence.
    DATA lv_is_first_condition              TYPE abap_bool.
    DATA lv_current_index                   TYPE ty_join_sequence-index.
    DATA lv_last_level                      TYPE ty_join_sequence-level.

    FIELD-SYMBOLS <ls_sequenced_join>       LIKE LINE OF mt_join_sequence.
    FIELD-SYMBOLS <ls_dd28j>                LIKE LINE OF mt_dd28j.

*dbtab_syntax = `( ( scarr AS c `
*  & ` INNER JOIN spfli AS p ON p~carrid  = c~carrid`
*  & ` AND p~cityfrom = p_cityfr`
*  & ` AND p~cityto   = p_cityto )`
*  & ` INNER JOIN sflight AS f ON f~carrid = p~carrid `
*  & ` AND f~connid = p~connid )`.

*  move the table in order to write the strings into it
    lt_join_sequence = mt_join_sequence.

*  now, create the partial strings with the conditions
    LOOP AT lt_join_sequence ASSIGNING <ls_sequenced_join>.

*    Table name of the joined table including an alias
      <ls_sequenced_join>-string = |{ <ls_sequenced_join>-right_tab } { co_string_as } { <ls_sequenced_join>-identifier }|.

      lv_is_first_condition = abap_true.
      LOOP AT mt_dd28j ASSIGNING <ls_dd28j>
        WHERE ltab = <ls_sequenced_join>-join-ltab  "join-ltab and
          AND rtab = <ls_sequenced_join>-join-rtab. "join-rtab are the origin sides of the join,
        "so we don't have to care mor about left / right

        IF lv_is_first_condition = abap_true.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } ON|.
          lv_is_first_condition = abap_false.
        ELSE.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } AND|.
        ENDIF.

*      determine the alias of the source-table.
        FIELD-SYMBOLS <ls_prev_join> LIKE LINE OF lt_join_sequence.
        DATA lv_left_alias           TYPE string.
        DATA lv_prev_level           LIKE <ls_prev_join>-level.

*      the lower level database-tables are relevant
        lv_prev_level = <ls_sequenced_join>-level - 1 .

        READ TABLE lt_join_sequence ASSIGNING <ls_prev_join> WITH KEY right_tab COMPONENTS
          level     = lv_prev_level
          right_tab = <ls_sequenced_join>-left_tab.
        IF sy-subrc = 0.
          lv_left_alias = |{ <ls_prev_join>-identifier }|.
        ELSE.
          lv_left_alias = |{ co_alias_anchor_table }|. "anchor table
        ENDIF.

*      depending on whether the join is defined "left-to-right" or inverse,
*      we need to specify also the left or right field left
        IF <ls_sequenced_join>-join-ltab = <ls_sequenced_join>-left_tab.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } { <ls_sequenced_join>-identifier }~{ <ls_dd28j>-rfield }|.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } = { lv_left_alias }~{ <ls_dd28j>-lfield }|.
        ELSE.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } { lv_left_alias }~{ <ls_dd28j>-rfield }|.
          <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } = { <ls_sequenced_join>-identifier }~{ <ls_dd28j>-lfield }|.
        ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0.

*      The join of the sequence has no join-criteria defined in the view-definition =>
*      It must be a dedicated join.

        DATA ls_dedicated_join_criterion LIKE LINE OF mt_dedicated_join_criterion.

*    Table name of the joined table including an alias
        <ls_sequenced_join>-string = |{ <ls_sequenced_join>-right_tab } { co_string_as } { <ls_sequenced_join>-identifier }|.

*      the lower level database-tables are relevant
        lv_prev_level = <ls_sequenced_join>-level - 1 .

*    Determine the joined table and its identifier
        READ TABLE lt_join_sequence ASSIGNING <ls_prev_join> WITH KEY right_tab COMPONENTS
          level     = lv_prev_level
          right_tab = <ls_sequenced_join>-left_tab.
        IF sy-subrc = 0.
          lv_left_alias = |{ <ls_prev_join>-identifier }|.
        ELSE.
          lv_left_alias = |{ co_alias_anchor_table }|. "anchor table
        ENDIF.

*      add the strings for the join criteria
        lv_is_first_condition = abap_true.
        LOOP AT mt_dedicated_join_criterion INTO ls_dedicated_join_criterion
          WHERE identifier = <ls_sequenced_join>-identifier.
          IF lv_is_first_condition = abap_true.
            <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } ON|.
            lv_is_first_condition = abap_false.
          ELSE.
            <ls_sequenced_join>-string = |{ <ls_sequenced_join>-string } AND|.
          ENDIF.

*
          DATA lv_joined_table TYPE string.
          DATA lv_joined_field TYPE string.
          FIELD-SYMBOLS <ls_joined_field> LIKE LINE OF mt_field.

          READ TABLE mt_field ASSIGNING <ls_joined_field> WITH KEY
            sql_table_alias = lv_left_alias
            source_fieldname = ls_dedicated_join_criterion-joined_fieldname.
          IF sy-subrc = 0.
*            get the effective field-information in order to know the alias of the field in the join
*           if the field from which was joined was part of the DB view
            lv_joined_table = <ls_joined_field>-sql_table_alias.
            lv_joined_field = <ls_joined_field>-source_fieldname. "This attribute contains only the real tables fieldname
          ELSE.
            lv_joined_table = lv_left_alias.
            lv_joined_field = ls_dedicated_join_criterion-joined_fieldname.
          ENDIF.

          <ls_sequenced_join>-string = <ls_sequenced_join>-string
                                    && | { <ls_sequenced_join>-identifier }~{ ls_dedicated_join_criterion-source_fieldname }|
                                    && | = { lv_joined_table }~{ lv_joined_field }|.

        ENDLOOP.

      ENDIF.

    ENDLOOP.

*  finally, compile the whole string with the actual joins

*  we need to traverse the sequence in reverse order in order to
*  properly add the brackets. As there is no reverse index, we need to use
*  a WHILE-Loop

    lv_current_index = lines( lt_join_sequence ).
    WHILE lv_current_index > 0.
      READ TABLE lt_join_sequence ASSIGNING <ls_sequenced_join> WITH TABLE KEY index = lv_current_index.

**    In OpenSQL, the right side of a join must
**    be either a db-table or a view, not a join.
**    Due to this restriction, we need to use NativeSQL for
**    query execution.
*     Brackets would be nice for beautification, but are semantically
*     not necessary
*    IF lv_last_level > <ls_sequenced_join>-level.
*      "we need brackets
*      rv_from_clause = |( { rv_from_clause } ) |.
*    ENDIF.


*    Add INNER / LEFT OUTER JOIN
      IF lv_current_index < lines( lt_join_sequence ). "No trailing JOIN-statement
        CASE <ls_sequenced_join>-join-join_type.
          WHEN co_inner.
            mv_from_clause = |INNER JOIN { mv_from_clause }|.
          WHEN co_left_outer.
            mv_from_clause = |LEFT OUTER JOIN { mv_from_clause }|.
          WHEN OTHERS.
            ASSERT 1 = 0.
        ENDCASE.
      ENDIF.

      mv_from_clause = |{ <ls_sequenced_join>-string } { mv_from_clause }|.

      IF <ls_sequenced_join>-level < lv_last_level
        OR lv_last_level IS INITIAL.
        lv_last_level = <ls_sequenced_join>-level.
      ENDIF.
      SUBTRACT 1 FROM lv_current_index.
    ENDWHILE.


*  finally, add the join to the anchor table
    CASE <ls_sequenced_join>-join-join_type.
      WHEN co_inner.
        mv_from_clause = |{ mv_anchor_table } { co_string_as } { co_alias_anchor_table } INNER JOIN { mv_from_clause }|.
      WHEN co_left_outer.
        mv_from_clause = |{ mv_anchor_table } { co_string_as } { co_alias_anchor_table } LEFT OUTER JOIN { mv_from_clause }|.
      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.


  ENDMETHOD.


  METHOD COMPILE_PARSED_WHERE.
    DATA ls_parsed_where LIKE LINE OF it_parsed_where.

    IF io_statement IS INITIAL.
*    This should actually not be used anymore. The prepared statement is the preferred alternative as it prevents code injection
      LOOP AT it_parsed_where INTO ls_parsed_where.
        CASE ls_parsed_where-token_type.
          WHEN co_token_type-fieldname.
            IF  ls_parsed_where-compilation_transformation = co_compilation_transformation-uppercase.
              ls_parsed_where-token =  |UPPER({  ls_parsed_where-token })|.
            ENDIF.
            rv_where_clause = |{ rv_where_clause } { ls_parsed_where-token }|.
          WHEN co_token_type-search_text.
            rv_where_clause = |{ rv_where_clause } '{ ls_parsed_where-token }'|.
          WHEN OTHERS.
            rv_where_clause = |{ rv_where_clause } { ls_parsed_where-token }|.
        ENDCASE.
      ENDLOOP.

      rv_where_clause = |{ condense( rv_where_clause ) }|.
    ELSE.
*        use a prepared statement
      DATA lr_param TYPE REF TO data.
      DATA lx_parameter_invalid TYPE REF TO cx_parameter_invalid.
      FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_field.
      FIELD-SYMBOLS <lv_param> TYPE any.

      LOOP AT it_parsed_where INTO ls_parsed_where.

        CASE ls_parsed_where-token_type.
          WHEN co_token_type-fieldname.
            READ TABLE mt_field ASSIGNING <ls_field> WITH KEY sql_field_alias COMPONENTS sql_field_alias = ls_parsed_where-token.
            ASSERT sy-subrc = 0.

            IF  ls_parsed_where-compilation_transformation = co_compilation_transformation-uppercase.
              ls_parsed_where-token =  |UPPER({  ls_parsed_where-token })|.
            ENDIF.
            rv_where_clause = |{ rv_where_clause } { ls_parsed_where-token }|.
          WHEN co_token_type-search_text.
            rv_where_clause = |{ rv_where_clause } ?|. "? is the placeholder for the data reference
            TRY.
                READ TABLE mt_field_domain_ext INTO DATA(ls_field_domain_ext) WITH KEY view_fieldname
                                    COMPONENTS source_table = <ls_field>-source_table view_fieldname = <ls_field>-view_fieldname.
                IF sy-subrc EQ 0.
                  CREATE DATA lr_param TYPE (ls_field_domain_ext-extend_rollname).
                ELSE.
                  CREATE DATA lr_param TYPE (<ls_field>-dd27p_ref->rollname).
                ENDIF.
                " you have some with your attribute name issue
                ASSERT lr_param IS BOUND.
                ASSIGN lr_param->* TO <lv_param>.
                <lv_param> = ls_parsed_where-token.

                io_statement->set_param(
                  EXPORTING
                    data_ref = lr_param    " Reference to a Data Object
                    inout    = cl_sql_statement=>c_param_in    " IN/OUT Parameters
                    is_lob   = abap_false " Seems as if this does not have to be set for SSTRINGS
                ).

              CATCH cx_parameter_invalid INTO lx_parameter_invalid.    " Superclass for Parameter Error
                ASSERT 1 = 0.
            ENDTRY.

          WHEN OTHERS.
            rv_where_clause = |{ rv_where_clause } { ls_parsed_where-token }|.
        ENDCASE.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    super->constructor( ).

*  default join type
    mv_join_type = co_inner.

*  DB connection for native SQL if needed
    TRY.
        mo_connection = cl_sql_connection=>get_connection( ).

        mv_dbms = mo_connection->get_dbms( ).

      CATCH cx_sql_exception.  " Exception Class for SQL Error
        ASSERT 1 = 0. "Default connection has to be there
    ENDTRY.

  ENDMETHOD.


  METHOD DETERMINE_JOINS.

    DATA lt_join                            TYPE tt_join.
    DATA ls_join                            LIKE LINE OF lt_join.
    DATA lt_dd28j                           LIKE mt_dd28j.
    DATA lt_processed_join                  TYPE tt_join.
    DATA ls_next_level_join                 LIKE LINE OF mt_join_sequence.
    DATA lv_while_index                     TYPE i.
    DATA lv_processed_join_level            TYPE ty_join_sequence-level.

    FIELD-SYMBOLS <ls_dd28j>                LIKE LINE OF mt_dd28j.
    FIELD-SYMBOLS <ls_just_processed_join>  LIKE LINE OF mt_join_sequence.

*    There is not representation of the joins in the view metadata,
*    we need to derive this information from the conditions
*    all conditions between the same tables define the join

    lt_dd28j = mt_dd28j.

    SORT lt_dd28j BY ltab rtab.
    DELETE ADJACENT DUPLICATES FROM lt_dd28j COMPARING ltab rtab.

*  translate joins into joins with a join type
    LOOP AT lt_dd28j ASSIGNING <ls_dd28j>.
      MOVE-CORRESPONDING <ls_dd28j> TO ls_join.
      ls_join-join_type = mv_join_type.
      INSERT ls_join INTO TABLE lt_join.
    ENDLOOP.

* Building the string has to consist of two parts: the actual ON
* and the definition of the brackets.
* For the latter, it is important to bring the joins into a logical order:
* Independent tables have to be processed first, dependent table's conditions
* need to be put in brackets

*  Algorithm: for all tables, try to find them on the right side of a join.
*  only if the left part of that join is not already registered as "higher level",
*  this join represents the next level

*    we need to start analyzing with the leftmost table and its joins
*    the left-most table is the anchor table

*    the definition in the DDIC is not directional, as
*    the logic of the inner join does not need a direction, so we need to analyze both directions

*    at the left is noted the left table
    ls_next_level_join-level = 0.
    LOOP AT lt_join INTO ls_next_level_join-join WHERE ltab = mv_anchor_table.
      ADD 1 TO ls_next_level_join-index.
      ls_next_level_join-right_tab = ls_next_level_join-join-rtab.
      ls_next_level_join-left_tab = ls_next_level_join-join-ltab.
      ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
      INSERT ls_next_level_join INTO TABLE mt_join_sequence.
      INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
    ENDLOOP.
*    at the right is noted the left table
    LOOP AT lt_join INTO ls_next_level_join-join USING KEY reverse WHERE rtab = mv_anchor_table.
      ADD 1 TO ls_next_level_join-index.
      ls_next_level_join-right_tab = ls_next_level_join-join-ltab.
      ls_next_level_join-left_tab = ls_next_level_join-join-rtab.
      ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
      INSERT ls_next_level_join INTO TABLE mt_join_sequence.
      INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
    ENDLOOP.

*    process all joins
    lv_processed_join_level = 0.
    ADD 1 TO ls_next_level_join-level.

    WHILE lines( lt_processed_join ) < lines( lt_join ).

*      prevent endless loops in case of erroneous definitions
*      or erroneous algorithm
      ADD 1 TO lv_while_index.
      IF lv_while_index > 1000.
        ASSERT 'this is an endless loop' = 'is it' ##no_text.
      ENDIF."Endless loop prevented - debug it


*      find the next joins for the currently processed level
      LOOP AT mt_join_sequence ASSIGNING <ls_just_processed_join> USING KEY level
          WHERE level = lv_processed_join_level.
        LOOP AT lt_join INTO ls_next_level_join-join WHERE ltab = <ls_just_processed_join>-join-rtab.
*          ensure that the join has not already processed in order
*          to avoid that we are traversing a way back we came from
          READ TABLE lt_processed_join TRANSPORTING NO FIELDS WITH TABLE KEY
            ltab = ls_next_level_join-join-ltab
            rtab = ls_next_level_join-join-rtab.
          IF sy-subrc <> 0. "not already processed
            ADD 1 TO ls_next_level_join-index.
            ls_next_level_join-right_tab = ls_next_level_join-join-rtab.
            ls_next_level_join-left_tab = ls_next_level_join-join-ltab.
            ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
            INSERT ls_next_level_join INTO TABLE mt_join_sequence.
            INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
          ENDIF.
        ENDLOOP.
*    at the right is noted the left table
        LOOP AT lt_join INTO ls_next_level_join-join USING KEY reverse WHERE rtab = <ls_just_processed_join>-join-rtab.
          READ TABLE lt_processed_join TRANSPORTING NO FIELDS WITH TABLE KEY
            ltab = ls_next_level_join-join-ltab
            rtab = ls_next_level_join-join-rtab.
          IF sy-subrc <> 0. "not already processed
            ADD 1 TO ls_next_level_join-index.
            ls_next_level_join-right_tab = ls_next_level_join-join-ltab.
            ls_next_level_join-left_tab = ls_next_level_join-join-rtab.
            ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
            INSERT ls_next_level_join INTO TABLE mt_join_sequence.
            INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
          ENDIF.
        ENDLOOP.

*   and once again: assuming the right is noted on the left

        LOOP AT lt_join INTO ls_next_level_join-join WHERE ltab = <ls_just_processed_join>-join-ltab.
*          ensure that the join has not already processed in order
*          to avoid that we are traversing a way back we came from
          READ TABLE lt_processed_join TRANSPORTING NO FIELDS WITH TABLE KEY
            ltab = ls_next_level_join-join-ltab
            rtab = ls_next_level_join-join-rtab.
          IF sy-subrc <> 0. "not already processed
            ADD 1 TO ls_next_level_join-index.
            ls_next_level_join-right_tab = ls_next_level_join-join-rtab.
            ls_next_level_join-left_tab = ls_next_level_join-join-ltab.
            ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
            INSERT ls_next_level_join INTO TABLE mt_join_sequence.
            INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
          ENDIF.
        ENDLOOP.
*    at the right is noted the left table
        LOOP AT lt_join INTO ls_next_level_join-join USING KEY reverse WHERE rtab = <ls_just_processed_join>-join-ltab.
          READ TABLE lt_processed_join TRANSPORTING NO FIELDS WITH TABLE KEY
            ltab = ls_next_level_join-join-ltab
            rtab = ls_next_level_join-join-rtab.
          IF sy-subrc <> 0. "not already processed
            ADD 1 TO ls_next_level_join-index.
            ls_next_level_join-right_tab = ls_next_level_join-join-ltab.
            ls_next_level_join-left_tab = ls_next_level_join-join-rtab.
            ls_next_level_join-identifier = |{ co_string_prefix_alias }{ ls_next_level_join-index }|.
            INSERT ls_next_level_join INTO TABLE mt_join_sequence.
            INSERT ls_next_level_join-join INTO TABLE lt_processed_join.
          ENDIF.
        ENDLOOP.

      ENDLOOP. "at just processed joins

      READ TABLE mt_join_sequence TRANSPORTING NO FIELDS WITH KEY level COMPONENTS
        level = lv_processed_join_level.
*      At least one join found => increase join-level
*      in order to find the dependent joins
      IF sy-subrc = 0.
        lv_processed_join_level = ls_next_level_join-level.
        ADD 1 TO ls_next_level_join-level.
      ENDIF.
    ENDWHILE.



*    finally, add the joins which are not part of the DB-view-definition but
*    which have been declared to be joined
    LOOP AT mt_dedicated_join INTO ls_next_level_join.

*     The joins in the dedicated joins table shall be sorted
*     so that the left table is actually the left table

      READ TABLE mt_join_sequence ASSIGNING <ls_just_processed_join>
        WITH KEY right_tab COMPONENTS
          right_tab = ls_next_level_join-left_tab. "every table to which we join is either joined (and thus noted on the right somewhere) or it's the anchor table
      IF sy-subrc <> 0.
        ASSERT ls_next_level_join-left_tab = mv_anchor_table.
        ls_next_level_join-level = 1.
      ELSE.
        ls_next_level_join-level = <ls_just_processed_join>-level + 1.
      ENDIF.

      ls_next_level_join-index = lines( mt_join_sequence ) + 1.


      INSERT ls_next_level_join INTO TABLE mt_join_sequence.

    ENDLOOP.

  ENDMETHOD.


  METHOD DO_SELECT.
    DATA lv_select_clause LIKE iv_select_clause.


    ASSERT mv_from_clause IS NOT INITIAL. "INITIALIZE( ) needs to be called by the concrete constructor

    IF mv_from_clause <> mv_db_view_name.
*  since we use aliasing of the DB tables joined,
*  we need to provide the alias also in the from-clause.
*  we do not only select from a DB-view, but from a complex join
      ASSERT iv_select_clause = |DB_KEY AS KEY|.
      lv_select_clause = |{ co_alias_anchor_table }~{ iv_select_clause }|.
*  OpenSQL does not provide the feature to join left outer to
*  joins. Therefore, we need to use nativesql
*  cl_sql_statement encapsulates this.
*
      me->do_select_native_sql(
        EXPORTING
          it_where       = it_where
          it_order_by    = it_order_by
          iv_max_rows    = iv_max_rows
          is_range       = is_range
        IMPORTING
          et_key         = et_key    " Key Table
          ev_count       = ev_count    " Processed Database Table Rows
          ev_rc          = ev_rc    " Subroutines for return code
      ).
    ELSE.
      lv_select_clause = iv_select_clause.
      super->do_select(
        EXPORTING
          iv_select_clause = lv_select_clause
          it_where         = it_where
          it_order_by      = it_order_by
          is_range         = is_range
          iv_max_rows      = iv_max_rows
        IMPORTING
          et_key           = et_key    " Key Table
          ev_count         = ev_count    " Processed Database Table Rows
          ev_rc            = ev_rc    " Subroutines for return code
      ).
    ENDIF.

  ENDMETHOD.


  METHOD DO_SELECT_NATIVE_SQL.

*  Before being able to execute the query, we need to
*  convert the incredibly comfortable select-options to
*  dumb where clauses. Function module FVD_SELECT_OPTIONS_2_WHERE
*  provides additional check of parameters, but as we can
*  rely on the parameters being passed contract-compliant,
*  we can directly use FM FREE_SELECTIONS_RANGE_2_WHERE

    TYPE-POOLS rsds.

    DATA lr_key_tab           LIKE REF TO et_key.
    DATA lx_root              TYPE REF TO cx_root.
    DATA lv_statement         TYPE string.
    DATA lv_where_clause      TYPE string.

*  in nativeSQL, the component separator is a dot
    lv_statement = |{ replace( val = mv_from_clause  sub = '~'  with = '.'  occ = 0 ) }|.

*  add SELECT clause
    lv_statement = |SELECT DISTINCT { co_alias_anchor_table }.{ /bobf/if_conf_c=>sc_attribute_name_db_key } FROM { lv_statement }|.

    IF mv_dbms EQ 'ORA'.
*    Oracle does not comply to the SQL-standard with respect to aliasing
*    of tables in a from clause - we need to remove the AS-statement,
*    but only in the FROM
      REPLACE ALL OCCURRENCES OF co_string_as IN lv_statement WITH ''.
*    REPLACE ALL OCCURRENCES OF 'OUTER' IN lv_statement WITH ''.
    ENDIF.

    DATA lo_statement TYPE REF TO cl_sql_statement.
    DATA lo_result_set TYPE REF TO cl_sql_result_set.

    lo_statement = mo_connection->create_statement( ).
    lv_where_clause = me->get_where_clause(
                        it_where        = it_where
                        is_range        = is_range
                        io_statement    = lo_statement
                    ).

    IF lv_where_clause IS NOT INITIAL.
      lv_statement = |{ lv_statement } WHERE { lv_where_clause } |.
    ENDIF.

    TRY.
        lo_result_set = lo_statement->execute_query( lv_statement ).

        GET REFERENCE OF et_key INTO lr_key_tab.

        lo_result_set->set_param_table(
          EXPORTING
            itab_ref             = lr_key_tab
*          corresponding_fields = corresponding_fields    " List of Columns of the Internal Table
*          lob_fields           = lob_fields    " List of LOB Fields
        ).

*      fetch data package
        ev_count = lo_result_set->next_package( upto = iv_max_rows ).

*      close the result set in order to free the DB resources
        lo_result_set->close( ).

        CLEAR ev_rc.

      CATCH cx_sql_exception
         cx_parameter_invalid INTO lx_root.    " Superclass for Parameter Error
        ASSERT FIELDS lx_root->get_text( ) CONDITION 1 = 0.
    ENDTRY.


  ENDMETHOD.


METHOD ESCAPE_ALIASES.

* --> Step: 5 Begin of Insert by Iyanknnu 14 Jan 2015
  DATA ls_field      TYPE ty_view_field.
* --> Step: 5 End of Insert by Iyanknnu 14 Jan 2015
* --> Step: 6 Begin of comment by Iyanknnu 14 Jan 2015
*  LOOP AT ct_select_ranges ASSIGNING FIELD-SYMBOL(<ls_select_range>).
*    LOOP AT <ls_select_range>-frange_t ASSIGNING FIELD-SYMBOL(<ls_range_per_table>).
*      REPLACE ALL OCCURRENCES OF '.' IN <ls_range_per_table>-fieldname WITH '~'.
*    ENDLOOP.
*  ENDLOOP.
* --> Step: 6 End of comment by Iyanknnu 14 Jan 2015

* --> Step: 7 Begin of Insert by Iyankannu 14 Jan 2015
  FIELD-SYMBOLS: <ls_select_range> TYPE rsds_range.
  FIELD-SYMBOLS: <ls_range_per_table> TYPE rsds_frange.
  FIELD-SYMBOLS: <ls_field> TYPE rsdsfields.
  FIELD-SYMBOLS: <ls_field_desc> TYPE fldconvert.

  DATA lv_field_name TYPE fieldname.

  LOOP AT ct_select_ranges ASSIGNING <ls_select_range>.
    LOOP AT <ls_select_range>-frange_t ASSIGNING <ls_range_per_table>.
      CLEAR lv_field_name.
      CLEAR ls_field.
      lv_field_name = <ls_range_per_table>-fieldname.
      READ TABLE mt_field INTO ls_field WITH KEY sql_field_alias
                              COMPONENTS sql_field_alias = <ls_range_per_table>-fieldname.
      IF sy-subrc EQ 0.
        <ls_range_per_table>-fieldname = ls_field-field_alias_name.
      ENDIF.


      IF ls_field-field_alias_name IS NOT INITIAL.
        LOOP AT ct_field ASSIGNING <ls_field> WHERE fieldname = lv_field_name.
          <ls_field>-fieldname = ls_field-field_alias_name.
        ENDLOOP.

        LOOP AT ct_field_desc ASSIGNING <ls_field_desc> WHERE fieldname = lv_field_name.
          <ls_field_desc>-fieldname = ls_field-field_alias_name.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
* --> Step: 7 end of Insert by Iyanknnu 14 Jan 2015
ENDMETHOD.


  METHOD EXTEND_FIELD_ROLLNAME.
    RETURN.
  ENDMETHOD.


  METHOD GET_SELECT_RANGE_FOR_TABLE.
*  within is_range, each attribute upon which a select-option is set
*  is represented as a component with type rangetab
*  In order to translate this into a where-clause, we need to collect metadata
*  for each of the table involved in the selection
    DATA lo_structdescr                 TYPE REF TO cl_abap_structdescr.
    DATA lt_component                   TYPE cl_abap_structdescr=>component_table.
    DATA lt_select_ranges               TYPE rsds_trange.
    DATA ls_select_ranges               LIKE LINE OF lt_select_ranges.
    DATA ls_select_frange               LIKE LINE OF ls_select_ranges-frange_t.
    DATA lt_field                       TYPE STANDARD TABLE OF rsdsfields.
    DATA ls_field                       LIKE LINE OF lt_field.
    DATA lt_field_desc                  TYPE STANDARD TABLE OF fldconvert.
    DATA ls_field_desc                  LIKE LINE OF lt_field_desc.
    DATA ls_select_selopt               LIKE LINE OF ls_select_frange-selopt_t.
    DATA ls_table                       LIKE LINE OF et_table.

    FIELD-SYMBOLS <ls_component>        LIKE LINE OF lt_component.
    FIELD-SYMBOLS <ls_field>            LIKE LINE OF mt_field.
    FIELD-SYMBOLS <ls_table>            LIKE LINE OF et_table.
    FIELD-SYMBOLS <lt_range>            TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_range>            TYPE any.

    CLEAR: et_table.

    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( is_range ).
    lt_component = lo_structdescr->get_components( ).
    CLEAR lt_select_ranges.

    LOOP AT lt_component ASSIGNING <ls_component>.

      CLEAR: ls_table, ls_select_frange.

*    first, we need to determine the meta data of the attribute
      READ TABLE mt_field ASSIGNING <ls_field> WITH KEY view_fieldname COMPONENTS view_fieldname = <ls_component>-name.
      ASSERT sy-subrc = 0.

*   collect for every table it's field informations
      READ TABLE et_table ASSIGNING <ls_table> WITH TABLE KEY tabname = <ls_field>-sql_table_alias.
      IF NOT sy-subrc EQ 0.
*        there has not been a criterion for this table
        IF <ls_field>-source_table = mv_anchor_table.
          ls_table-tabname = mv_anchor_table.
        ELSE.
          ls_table-tabname = <ls_field>-sql_table_alias.
        ENDIF.
        INSERT ls_table INTO TABLE et_table ASSIGNING <ls_table>.
      ENDIF.

*    create the criterion-entry
      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_range TO <lt_range>.
      ASSERT sy-subrc = 0.

*   Collect field informations for where clause generation
*   The function module needs metadata about the datatype
      CLEAR: ls_field, ls_field_desc.
*   ls_field-tablename = <ls_select_ranges>-tablename. "Has to be empty!!!
      ls_field-fieldname = <ls_field>-sql_field_alias.
      INSERT ls_field INTO TABLE <ls_table>-t_field.

      ls_field_desc-fieldname   = ls_field-fieldname.
      ls_field_desc-type        = <ls_field>-dd27p_ref->inttype.
      ls_field_desc-length      = <ls_field>-dd27p_ref->flength.
      ls_field_desc-clength     = <ls_field>-dd27p_ref->intlen.
      ls_field_desc-decimals    = <ls_field>-dd27p_ref->decimals.
      ls_field_desc-lower       = <ls_field>-dd27p_ref->lowercase.
      ls_field_desc-convexit    = <ls_field>-dd27p_ref->convexit.
      ls_field_desc-outputstyle = <ls_field>-dd27p_ref->outputstyle.

      INSERT ls_field_desc INTO TABLE <ls_table>-t_field_desc.

      ls_select_frange-fieldname = <ls_field>-sql_field_alias.
      LOOP AT <lt_range> ASSIGNING <ls_range>.
*      These are both select-option tables but moving the
*      complete table with simple casting is not supported
*      within ABAP, so we need to loop.

        DATA ls_selopt LIKE LINE OF ls_select_frange-selopt_t.
        MOVE-CORRESPONDING <ls_range> TO ls_selopt.
        APPEND ls_selopt TO ls_select_frange-selopt_t.

      ENDLOOP.
      INSERT ls_select_frange INTO TABLE <ls_table>-t_frange.

    ENDLOOP.
  ENDMETHOD.


  METHOD GET_WHERE_CLAUSE.

*  the where clause is tricky: The comfortable SELECT-OPTIONS
*  are not available in NativeSQL, so we need to translate it to a
*  normal where-clause

    DATA lt_table                     TYPE tt_table.
    FIELD-SYMBOLS <ls_table>          LIKE LINE OF lt_table.
    DATA lt_where_by_table            TYPE rsds_twhere.
    DATA ls_where_by_table            LIKE LINE OF lt_where_by_table.
    DATA lt_select_ranges             TYPE rsds_trange.
    DATA ls_select_ranges             LIKE LINE OF lt_select_ranges.
    DATA lv_and_open                  TYPE sap_bool.

    DATA lt_where TYPE STANDARD TABLE OF string.     "Total where clause / result set
    DATA lv_where LIKE LINE OF lt_where.
    DATA lt_field TYPE STANDARD TABLE OF rsdsfields.
    DATA lt_field_desc TYPE STANDARD TABLE OF fldconvert.

    IF it_where IS INITIAL.
*      even if no restriction is defined, we need to limit the query to the current client
      rv_where_clause = get_where_client( ).
      RETURN.">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    me->get_select_range_for_table(
      EXPORTING is_range = is_range
      IMPORTING et_table = lt_table ).

*   Create where clause for actual table
*   For all input parameters: there hasn't got to be a table name!
*   That's the reason why the FM is called one time for every table
    LOOP AT lt_table ASSIGNING <ls_table>.

      CLEAR:
        ls_select_ranges,
        lt_select_ranges,
        lt_field,
        lt_field_desc,
        lt_where_by_table.

      lv_and_open = abap_false.

      " check the current selection table has any quantity field in where condition
      " if we have the where class with quantity field an currency field
      " we should not have the open quotes and closing quotes on those value

*   Map input data
      ls_select_ranges-frange_t = <ls_table>-t_frange.
      INSERT ls_select_ranges INTO TABLE lt_select_ranges.
      lt_field = <ls_table>-t_field.
      lt_field_desc = <ls_table>-t_field_desc.

* --> Step: 4 Begin of Insert by Iyanknnu 14 Jan 2015
      me->escape_aliases( CHANGING ct_select_ranges = lt_select_ranges
                                   ct_field         = lt_field
                                   ct_field_desc    = lt_field_desc ).
* --> Step: 4 End of Insert by Iyanknnu 14 Jan 2015

      CALL FUNCTION 'FREE_SELECTIONS_INIT'
        EXPORTING
          kind                     = 'F'
          field_ranges_int         = lt_select_ranges
        IMPORTING
          where_clauses            = lt_where_by_table
        TABLES
          fields_tab               = lt_field
          field_desc               = lt_field_desc
        EXCEPTIONS
          fields_incomplete        = 1
          fields_no_join           = 2
          field_not_found          = 3
          no_tables                = 4
          table_not_found          = 5
          expression_not_supported = 6
          incorrect_expression     = 7
          illegal_kind             = 8
          area_not_found           = 9
          inconsistent_area        = 10
          kind_f_no_fields_left    = 11
          kind_f_no_fields         = 12
          too_many_fields          = 13
          dup_field                = 14
          field_no_type            = 15
          field_ill_type           = 16
          dup_event_field          = 17
          node_not_in_ldb          = 18
          area_no_field            = 19.
*   There may not be an error, otherwise our where clause would be wrong
      ASSERT sy-subrc IS INITIAL.

* --> Step: 2 Begin of Insert by Iyanknnu 21 Jan 2015
      me->unescape_aliases(
        CHANGING
          ct_select_ranges = lt_where_by_table ).
* --> Step: 2 Begin of Insert by Iyanknnu 21 Jan 2015

*   For all tables concatenate the where clauses by ANDs
      IF NOT lt_where IS INITIAL.
        lv_where = |AND (|.
        APPEND lv_where TO lt_where.
        lv_and_open = abap_true.
      ENDIF.

*   Add where clause for the actual table to our result set
      IF NOT lt_where_by_table IS INITIAL.
*     We create the where clause per table, so we expect one entry
        ASSERT lines( lt_where_by_table ) EQ 1.
        READ TABLE lt_where_by_table INTO ls_where_by_table INDEX 1.

*        the where-statements for the table may be split across multiple lines.
*        Reason for this is that criteria on character fields with long names might exceed the limit of SYCHAR72
*        this makes it tricky to adjust the different parts of the where-clause (e. g. add case-transformation, replace operators)
*        we'll parse the tokens in order to identify their role in the where-statement
        DATA lt_parsed_where TYPE tt_parsed_where.
        lt_parsed_where = me->parse_where_by_table( is_where_by_table = ls_where_by_table ).

        me->adjust_parsed_where(
            EXPORTING
                iv_tabname = <ls_table>-tabname
            CHANGING
                ct_parsed_where = lt_parsed_where ).

*                combine the tokens again and attach them to the sql statement
        lv_where = compile_parsed_where(
                    it_parsed_where = lt_parsed_where
                    io_statement    = io_statement "Todo: Prepared statement positions
                    ).
        APPEND lv_where TO lt_where.
      ENDIF.

*   close AND ( ... ) which may combine criteria on multiple tables
      IF lv_and_open EQ abap_true.
        lv_where = |)|.
        APPEND lv_where TO lt_where.
      ENDIF.

    ENDLOOP.

* We want to return a long string, but not a String[]
*  => concatenate it
    LOOP AT lt_where INTO lv_where.
      rv_where_clause = |{ rv_where_clause }  { lv_where }|.
    ENDLOOP.
    rv_where_clause = |{ condense( rv_where_clause ) }|.

*    finally, it's native-SQL, so we need to take care of client-handling ourselves
    IF rv_where_clause IS NOT INITIAL.
      rv_where_clause = |{ rv_where_clause } AND { get_where_client( ) }|.
    ELSE.
      rv_where_clause = get_where_client( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_WHERE_CLIENT.

    rv_where_client = |{ co_alias_anchor_table }.MANDT = { sy-mandt }| ##no_text.

  ENDMETHOD.


  METHOD INITIALIZE.

*  this method needs to be called only once as it only translates metadata
*  which is independent of the instances
    IF mv_from_clause IS INITIAL.
*  DDIC views are always inner joins. Therefore, we can only directly access it once
*  we want the query to be of an inner join behavior.
*  This will - in reality - rarely be the case.

      CASE mv_join_type .
        WHEN co_inner.
          mv_from_clause = mv_db_view_name.
        WHEN co_left_outer.
          analyze_ddic_definition( ).
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

    ENDIF.



  ENDMETHOD.


  METHOD PARSE_WHERE_BY_TABLE.
*    The where by table specifies the conditions on a particular ... well ... table.
*    In order to optimize data transport in ancient times, the statement has been split across multiple lines.
*    Let's combine them into a single string for easier parsing

    DATA lv_where_by_table TYPE string.
    DATA lv_string_part LIKE LINE OF is_where_by_table-where_tab.
    DATA ls_parsed_where LIKE LINE OF rt_parsed_where.

    CLEAR lv_where_by_table.
    LOOP AT is_where_by_table-where_tab INTO lv_string_part.
      lv_where_by_table = lv_where_by_table && | | && lv_string_part-line.
    ENDLOOP.

*    Next step is to identify the search strings. Those can be recognized as literals surrounded by hyphens
*    These search-phrases need to be preserved as a whole. This needs to be done in the beginning as they might contain spaces or other characters
*    which might be interpreted as separators if not placed between the hyphens
*    If the search phrase contains a hyphen itself, it will be escaped as double hyphen. In order to conserve this special character, we'll
*    replace it with a very unlikely sequence of characters

*    before we do that, we need to apply a special replacement: If the search term is a single hyphen, it is getting
*    propagated with lots of spaces in between. These have to be stripped (for this very special search string)
    REPLACE |'''                                           ' | IN lv_where_by_table WITH |'{ co_double_hyphen }'|.

*    when searching for special characters, ESCAPE is used to define an escaping token for sql-statement.
*    This needs to be preserved
    REPLACE ALL OCCURRENCES OF |' ESCAPE '| IN lv_where_by_table WITH |{ co_escape_token }|.

    REPLACE ALL OCCURRENCES OF |''| IN lv_where_by_table WITH co_double_hyphen.

    DATA lt_parts_per_hyphen TYPE tt_string.
    DATA lv_part_per_hyphen LIKE LINE OF lt_parts_per_hyphen.
    SPLIT lv_where_by_table AT |'| INTO TABLE lt_parts_per_hyphen.

*    now, every second item is a search phrase token. All the other (uneven) indices can be tokenized by spaces and interpreted.
    DEFINE mac_add_token.
      ls_parsed_where-token = &1.
      ls_parsed_where-token_type = &2.
      add 1 to ls_parsed_where-index.
      insert ls_parsed_where into table rt_parsed_where.
    END-OF-DEFINITION.
    DATA lt_parts_per_space TYPE tt_string.
    DATA lv_part_per_space LIKE LINE OF lt_parts_per_space.
    LOOP AT lt_parts_per_hyphen INTO lv_part_per_hyphen WHERE table_line IS NOT INITIAL.
      IF sy-tabix MOD 2 = 0.
*            It's a search term.
*        this term may include an escaping instruction
        IF contains( val = lv_part_per_hyphen  sub = co_escape_token ).
*            we don't need to escape the special character as we use a statement for the selection.
*            but now we need to remove the special characters which are used as escape-char from the search term
          DATA lv_escaping_token TYPE string.
          SPLIT lv_part_per_hyphen AT co_escape_token INTO lv_part_per_hyphen lv_escaping_token.
          ASSERT sy-subrc = 0. "There must not be multiple ESCAPEs for one search string.

          REPLACE ALL OCCURRENCES OF lv_escaping_token IN lv_part_per_hyphen WITH ''.
        ENDIF.
        REPLACE ALL OCCURRENCES OF co_double_hyphen IN lv_part_per_hyphen WITH |'|.
        mac_add_token lv_part_per_hyphen co_token_type-search_text.
      ELSE.
        SPLIT lv_part_per_hyphen AT space INTO TABLE lt_parts_per_space.
        LOOP AT lt_parts_per_space INTO lv_part_per_space WHERE table_line IS NOT INITIAL.
          IF  lv_part_per_space EQ '('
              OR lv_part_per_space EQ ')'
              OR lv_part_per_space EQ 'AND'
              OR lv_part_per_space EQ 'OR'
              OR lv_part_per_space EQ 'NOT'.
            mac_add_token lv_part_per_space co_token_type-logical_expression.
          ELSEIF lv_part_per_space EQ 'EQ'
                  OR lv_part_per_space EQ '='
                  OR lv_part_per_space EQ '<'
                  OR lv_part_per_space EQ '<='
                  OR lv_part_per_space EQ '>'
                  OR lv_part_per_space EQ '>='
                  OR lv_part_per_space EQ '<>'
                  OR lv_part_per_space EQ 'LIKE'
                  OR lv_part_per_space EQ 'BT'
                  OR lv_part_per_space EQ 'BETWEEN'
                  OR lv_part_per_space EQ 'CP'
                  OR lv_part_per_space EQ 'LE'
                  OR lv_part_per_space EQ 'GE'
                  OR lv_part_per_space EQ 'NE'
                  OR lv_part_per_space EQ 'NP'
                  OR lv_part_per_space EQ 'GT'
                  OR lv_part_per_space EQ 'LT'.
            mac_add_token lv_part_per_space co_token_type-operator.
          ELSE.
            ASSERT lv_part_per_space CA '.'.
            mac_add_token lv_part_per_space co_token_type-fieldname.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


METHOD UNESCAPE_ALIASES.
  DATA lt_string TYPE TABLE OF rsdswhere.
* --> Step: 3 Begin of Comment by Iyanknnu 21 Jan 2015
*  LOOP AT ct_select_ranges ASSIGNING FIELD-SYMBOL(<ls_select_range>).
*    LOOP AT <ls_select_range>-frange_t ASSIGNING FIELD-SYMBOL(<ls_range_per_table>).
*      REPLACE ALL OCCURRENCES OF '~' IN <ls_range_per_table>-fieldname WITH '.'.
*    ENDLOOP.
*  ENDLOOP.
* --> Step: 3 Begin of Comment by Iyanknnu 21 Jan 2015

* --> Step: 4 Begin of Insert by Iyanknnu 21 Jan 2015
  FIELD-SYMBOLS <ls_select_range> TYPE rsds_where.
  FIELD-SYMBOLS <lv_string> TYPE rsdswhere.
  DATA ls_field TYPE ty_view_field.

  LOOP AT ct_select_ranges ASSIGNING <ls_select_range>.
    CLEAR lt_string.
    LOOP AT mt_field INTO ls_field.
      LOOP AT <ls_select_range>-where_tab ASSIGNING <lv_string>.
        REPLACE ALL OCCURRENCES OF ls_field-field_alias_name IN <lv_string> WITH ls_field-sql_field_alias.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
* --> Step: 4 Begin of Insert by Iyanknnu 21 Jan 2015
ENDMETHOD.
ENDCLASS.
