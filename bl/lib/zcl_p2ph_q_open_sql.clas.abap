class ZCL_P2PH_Q_OPEN_SQL definition
  public
  abstract
  create public .

public section.

*"* public components of class ZCL_P2PH_Q_OPEN_SQL
*"* do not include other source files here!!!
  interfaces /BOBF/IF_FRW_QUERY .
  PROTECTED SECTION.

    TYPES:
      tt_string TYPE STANDARD TABLE OF string .

    DATA mv_anchor_table TYPE tabname .
*"* protected components of class ZCL_P2PH_Q_OPEN_SQL
*"* do not include other source files here!!!
    DATA mv_from_clause TYPE string .

    METHODS do_select
      IMPORTING
        !iv_select_clause TYPE string
        !it_where TYPE tt_string
        !it_order_by TYPE tt_string
        !iv_max_rows TYPE i
        !is_range TYPE any OPTIONAL
      EXPORTING
        !et_key TYPE /bobf/t_frw_key
        !ev_count TYPE syst-dbcnt
        !ev_rc TYPE subrc
      RAISING
        /bobf/cx_frw .

    METHODS adapt_selection_parameters
         CHANGING ct_selection_parameter TYPE /bobf/t_frw_query_selparam.
  PRIVATE SECTION.

    DATA mt_dedicated_join TYPE i .
*"* private components of class ZCL_P2PH_Q_OPEN_SQL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_P2PH_Q_OPEN_SQL IMPLEMENTATION.


  METHOD /bobf/if_frw_query~query.

* Code based upon method /BOBF/IF_BUF_DATA_ACCESS~QUERY.

    DATA:
      ls_key       TYPE /bobf/s_frw_key,
      lt_selection TYPE /bobf/t_frw_query_selparam,
      lr_selection TYPE REF TO /bobf/s_frw_query_selparam,
      ls_sorting   TYPE /bobf/s_frw_query_sorting,
      lv_index     TYPE i,
      lv_max_rows  TYPE i,
      lv_where     TYPE string,
      lv_condit    TYPE string,                            " iyankannu
      lt_where     TYPE tt_string,
      lv_order     TYPE string,
      lt_order     TYPE tt_string,
      lt_range_key TYPE RANGE OF /bobf/conf_key,
      ls_range_key LIKE LINE OF lt_range_key,
      ls_comp      TYPE cl_abap_structdescr=>component,
      lt_comp      TYPE cl_abap_structdescr=>component_table,
      lr_range_r   TYPE REF TO data,
      lo_range     TYPE REF TO cl_abap_structdescr,
      lt_range     TYPE RANGE OF string,                    "#EC NEEDED
      ls_range     LIKE LINE OF lt_range,
      lx_root      TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_range>   TYPE any,
      <lt_range>   LIKE lt_range.

    CLEAR:
      et_key,
      eo_message,
      et_data,
      es_query_info.

* build where condition for key filter
    IF it_filter_key IS NOT INITIAL.
      IF lt_where IS NOT INITIAL.
        lv_where = 'AND'.
        APPEND lv_where TO lt_where.
      ENDIF.
      LOOP AT it_filter_key INTO ls_key.
        ls_range_key-option = 'EQ'.
        ls_range_key-sign   = 'I'.
        ls_range_key-low    = ls_key-key.
        INSERT ls_range_key INTO TABLE lt_range_key.
      ENDLOOP.
      CONCATENATE /bobf/if_conf_c=>sc_attribute_name_db_key 'IN LT_RANGE_KEY' INTO lv_where SEPARATED BY space.
      APPEND lv_where TO lt_where.
    ENDIF.

* build where condition from selection
    IF it_selection_parameters IS NOT INITIAL.
      lt_selection = it_selection_parameters.

*    There are some parameters which implicitly include other parameters.
*    Add them to the selection before translating this range into a where-clause
      me->adapt_selection_parameters( CHANGING ct_selection_parameter = lt_selection ).

      SORT lt_selection BY attribute_name.
      LOOP AT lt_selection REFERENCE INTO lr_selection.
        AT NEW attribute_name.
          ls_comp-name = lr_selection->attribute_name.
          ls_comp-type ?= cl_abap_typedescr=>describe_by_data( lt_range ).
          APPEND ls_comp TO lt_comp.
        ENDAT.
      ENDLOOP.
      lo_range = cl_abap_structdescr=>create( lt_comp ).
      CREATE DATA lr_range_r TYPE HANDLE lo_range.
      ASSIGN lr_range_r->* TO <ls_range>.

*  prerequisite: All query parameters are named like the view-fields they shall be mapped to
      LOOP AT lt_selection REFERENCE INTO lr_selection.
        AT NEW attribute_name.
          IF lt_where IS NOT INITIAL.
            lv_where = 'AND'.
            APPEND lv_where TO lt_where.
          ENDIF.
          CONCATENATE '<LS_RANGE>-' lr_selection->attribute_name INTO lv_where.
          ASSIGN (lv_where) TO <lt_range>.
          CONCATENATE lr_selection->attribute_name 'IN' lv_where INTO lv_condit SEPARATED BY space.      " Iyankannu
          APPEND lv_condit TO lt_where.                                                                  " Iyankannu
        ENDAT.

        MOVE-CORRESPONDING lr_selection->* TO ls_range.
        INSERT ls_range INTO TABLE <lt_range>.
      ENDLOOP.
    ELSE.
*      no criteria specified => create a pseudo-wa in order to to assign the fieldsymbol.
      CREATE DATA lr_range_r TYPE string.
      ASSIGN lr_range_r->* TO <ls_range>.
    ENDIF.

* build order by table
    LOOP AT is_query_options-sorting_options INTO ls_sorting.
      IF ls_sorting-ascending = abap_true.
        CONCATENATE ls_sorting-attribute_name 'ASCENDING'  INTO lv_order SEPARATED BY space.
      ELSE.
        CONCATENATE ls_sorting-attribute_name 'DESCENDING' INTO lv_order SEPARATED BY space.
      ENDIF.
      APPEND lv_order TO lt_order.
    ENDLOOP.

* default order if paging is active
    IF sy-subrc <> 0 AND is_query_options-paging_options-paging_active = abap_true.
      CONCATENATE /bobf/if_conf_c=>sc_attribute_name_db_key 'ASCENDING'  INTO lv_order SEPARATED BY space.
      APPEND lv_order TO lt_order.
    ENDIF.

    IF is_query_options-paging_options-paging_active = abap_true AND
       is_query_options-paging_options-start_key IS NOT INITIAL.
      IF lt_where IS NOT INITIAL.
        lv_where = 'AND'.
        APPEND lv_where TO lt_where.
      ENDIF.
      CONCATENATE /bobf/if_conf_c=>sc_attribute_name_db_key 'GT IS_QUERY_OPTIONS-PAGING_OPTIONS-START_KEY' INTO lv_where SEPARATED BY space.
      APPEND lv_where TO lt_where.
    ENDIF.

*  disable the max rows if paging is active
    IF is_query_options-maximum_rows > 0.
      lv_max_rows = is_query_options-maximum_rows.
      IF is_query_options-paging_options-paging_active = abap_true AND
         is_query_options-paging_options-start_row IS NOT INITIAL.
        lv_max_rows = 0.
      ENDIF.
    ENDIF.

    TRY.
        DATA lv_select_clause TYPE string.
        lv_select_clause = |{ /bobf/if_conf_c=>sc_attribute_name_db_key } AS { /bobf/if_conf_c=>sc_attribute_name_key }|.

        me->do_select(
          EXPORTING
              iv_select_clause = lv_select_clause
              it_where         = lt_where
              it_order_by      = lt_order
              is_range         = <ls_range>
              iv_max_rows      = is_query_options-maximum_rows
          IMPORTING
              et_key           = et_key
              ev_count         = es_query_info-count
              ).
      CATCH cx_sy_sql_error INTO lx_root.
*        @todo: Proper error handling
        ASSERT 1 = 0.
*        RAISE EXCEPTION TYPE zcx_p2ph
*          EXPORTING
*            previous = lx_root.
    ENDTRY.

* fill export parameters
    IF is_query_options-paging_options-paging_active = abap_true AND
       is_query_options-paging_options-start_row IS NOT INITIAL.
      IF is_query_options-paging_options-start_row > 1.
        DELETE et_key TO is_query_options-paging_options-start_row - 1.
      ENDIF.
      IF is_query_options-maximum_rows > 0.
        lv_index = is_query_options-maximum_rows + 1.
        DELETE et_key FROM lv_index.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /BOBF/IF_FRW_QUERY~RETRIEVE_DEFAULT_PARAM.
                                                            "#EC needed
  ENDMETHOD.


  METHOD ADAPT_SELECTION_PARAMETERS.
    RETURN.
  ENDMETHOD.


  METHOD do_select.

    CLEAR: et_key, ev_count, ev_rc.

    DATA lx_root TYPE REF TO cx_root.

    FIELD-SYMBOLS <ls_range> TYPE any.

*  when selecting from any JOIN, the occurence of the anchor
*  node's attributes might increase as joined db tables can have a cardinality > 1.
*  therefore, we need the DISTINCT lines

    ASSIGN is_range TO <ls_range>.

    TRY.
        SELECT DISTINCT (iv_select_clause) FROM (mv_from_clause)
          UP TO iv_max_rows ROWS
          INTO TABLE et_key
          WHERE     (it_where)
          ORDER BY  (it_order_by)
        .                               "#EC CI_DYNTAB "#EC CI_DYNWHERE
      CATCH cx_root INTO lx_root.
*        @todo: Proper exception handling
        ASSERT 1 = 0.
*        RAISE EXCEPTION TYPE /bobf/cx_frw
*          EXPORTING
*            previous = lx_root.
    ENDTRY.
    ev_count = sy-dbcnt.
    ev_rc    = sy-subrc.

  ENDMETHOD.
ENDCLASS.
