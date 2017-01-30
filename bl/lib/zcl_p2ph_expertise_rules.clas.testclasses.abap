
CLASS ltcl_expert_area DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_Expert_Area
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_P2PH_EXPERTISE_RULES
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_p2ph_expertise_rules.  "class under test

    METHODS: setup.
    METHODS: teardown.
    METHODS: determine_expert_area FOR TESTING.
ENDCLASS.       "ltcl_Expert_Area


CLASS ltcl_expert_area IMPLEMENTATION.

  METHOD setup.
    f_cut = zcl_p2ph_expertise_rules=>get_instance( ).
  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.


  METHOD determine_expert_area.

    cl_aunit_assert=>assert_equals(
        exp                  = 'PURCHASING'
        act                  = f_cut->determine_expert_area( ir_environment = NEW #( application_component_id = 'MM-PUR' ) )
    ).

  ENDMETHOD.

ENDCLASS.
