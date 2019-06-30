CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      count_neighbours FOR TESTING,
      test_die_overpopulation FOR TESTING,
      test_die_loneliness FOR TESTING,
      test_born FOR TESTING,
      test_live FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD count_neighbours.
    cl_abap_unit_assert=>assert_equals(
     exp = 3
     act = NEW lcl_gameoflife( NEW lcl_test_initializer( ) )->count_neighbours( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

  METHOD test_born.
    DATA(lo_gol) = NEW lcl_gameoflife( NEW lcl_test_initializer( )  ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_true
     act = lo_gol->is_cell_alive( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

  METHOD test_die_loneliness.
    DATA(lo_gol) = NEW lcl_gameoflife( NEW lcl_test_initializer( )  ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_false
     act = lo_gol->is_cell_alive( iv_row = 1 iv_col = 4 )
    ).
  ENDMETHOD.

  METHOD test_die_overpopulation.
    DATA(lo_gol) = NEW lcl_gameoflife( NEW lcl_test_initializer( ) ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_false
     act = lo_gol->is_cell_alive( iv_row = 7 iv_col = 3 )
    ).
  ENDMETHOD.

  METHOD test_live.
    DATA(lo_gol) = NEW lcl_gameoflife( NEW lcl_test_initializer( )  ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_true
     act = lo_gol->is_cell_alive( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

ENDCLASS.
