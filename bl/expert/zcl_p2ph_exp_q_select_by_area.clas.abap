class ZCL_P2PH_EXP_Q_SELECT_BY_AREA definition
  public
  inheriting from ZCL_P2PH_Q_DB_VIEW
  final
  create public .

public section.

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_P2PH_EXP_Q_SELECT_BY_AREA IMPLEMENTATION.


METHOD constructor.
  super->constructor( ). "ABAP requires a not existing constructor of an ABSTRACT superclass to be called...

  mv_db_view_name = 'ZP2PH_EXP_AREA_V'.
  mv_join_type = co_left_outer.

*where clause need to filled before start the selection of data from the data base table based on the user input.
  initialize( ).
ENDMETHOD.
ENDCLASS.