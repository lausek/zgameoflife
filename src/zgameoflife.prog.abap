REPORT zgameoflife.

CLASS lcl_gameoflife DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gtys_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF gtys_cell.

    DATA:
            gt_playfield TYPE STANDARD TABLE OF gtys_cell WITH EMPTY KEY.

    METHODS:
      constructor,
      is_cell_alive
        IMPORTING
                  iv_row          TYPE i
                  iv_col          TYPE i
        RETURNING VALUE(rv_alive) TYPE abap_bool,
      count_neighbours
        IMPORTING
                  iv_row               TYPE i
                  iv_col               TYPE i
        RETURNING VALUE(rv_neighbours) TYPE i,
      do_iteration.

  PRIVATE SECTION.
    CONSTANTS:
      cv_rows TYPE i VALUE 10,
      cv_cols TYPE i VALUE 10.

ENDCLASS.

CLASS lcl_gameoflife IMPLEMENTATION.
  METHOD constructor.
    DO cv_rows TIMES.
      DATA(lv_row) = sy-index.

      DO cv_cols TIMES.
        DATA(lv_col) = sy-index.
        APPEND VALUE #(
            row = lv_row
            col = lv_col
        ) TO gt_playfield.
      ENDDO.
    ENDDO.

    DEFINE mac_set_alive.
      gt_playfield[ row = &1 col = &2 ]-alive = abap_true.
    END-OF-DEFINITION.

    mac_set_alive 2 1.
    mac_set_alive 1 4.
    mac_set_alive 3 2.
    mac_set_alive 4: 2, 3.
    mac_set_alive 5 1.
    mac_set_alive 6: 2, 3, 4.
    mac_set_alive 7: 2, 3, 4.
    mac_set_alive 8: 2, 3, 4.
  ENDMETHOD.

  METHOD count_neighbours.
    DATA(lv_upper_row) = iv_row - 1.
    DATA(lv_lower_row) = iv_row + 1.

    DEFINE mac_count_if_present.
      IF is_cell_alive( iv_row = &1 iv_col = &2 ).
        ADD 1 TO rv_neighbours.
      ENDIF.
    END-OF-DEFINITION.

    IF 1 <= lv_upper_row.
      DATA(lv_col) = iv_col - 1.
      mac_count_if_present lv_upper_row lv_col.
      ADD 1 TO lv_col.
      mac_count_if_present lv_upper_row lv_col.
      ADD 1 TO lv_col.
      mac_count_if_present lv_upper_row lv_col.
    ENDIF.

    lv_col = iv_col - 1.
    mac_count_if_present iv_row lv_col.
    ADD 2 TO lv_col.
    mac_count_if_present iv_row lv_col.

    IF lv_lower_row <= cv_rows.
      lv_col = iv_col - 1.
      mac_count_if_present lv_lower_row lv_col.
      ADD 1 TO lv_col.
      mac_count_if_present lv_lower_row lv_col.
      ADD 1 TO lv_col.
      mac_count_if_present lv_lower_row lv_col.
    ENDIF.
  ENDMETHOD.

  METHOD is_cell_alive.
    ASSIGN gt_playfield[ row = iv_row col = iv_col ] TO FIELD-SYMBOL(<ls_cell>).
    rv_alive = COND #(
        WHEN sy-subrc = 0 THEN <ls_cell>-alive
        ELSE abap_false
    ).
  ENDMETHOD.

  METHOD do_iteration.
    DATA(lt_new_playfield) = gt_playfield[].
    LOOP AT gt_playfield
        ASSIGNING FIELD-SYMBOL(<ls_cell>).
      lt_new_playfield[ row = <ls_cell>-row col = <ls_cell>-col ]-alive = SWITCH #(
          LET lv_neighbours = count_neighbours(
              iv_row = <ls_cell>-row
              iv_col = <ls_cell>-col
          ) IN
          <ls_cell>-alive
          " cell is alive; only stays alive if two or three neighbours
          WHEN abap_true THEN boolc( lv_neighbours = 2 OR lv_neighbours = 3 )
          " cell is dead; spawn new one if exactly three neighbours
          ELSE boolc( lv_neighbours = 3 )
      ).
    ENDLOOP.
    gt_playfield[] = lt_new_playfield[].
  ENDMETHOD.

ENDCLASS.

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
     exp = 3 " bei Zeile 3 Spalte 2 EXP = 3
     act = NEW lcl_gameoflife( )->count_neighbours( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

  METHOD test_born.
    DATA(lo_gol) = NEW lcl_gameoflife( ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_true
     act = lo_gol->is_cell_alive( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

  METHOD test_die_loneliness.
    DATA(lo_gol) = NEW lcl_gameoflife( ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_false
     act = lo_gol->is_cell_alive( iv_row = 1 iv_col = 4 )
    ).
  ENDMETHOD.

  METHOD test_die_overpopulation.
    DATA(lo_gol) = NEW lcl_gameoflife( ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_false
     act = lo_gol->is_cell_alive( iv_row = 7 iv_col = 3 )
    ).
  ENDMETHOD.

  METHOD test_live.
    DATA(lo_gol) = NEW lcl_gameoflife( ).
    lo_gol->do_iteration( ).
    cl_abap_unit_assert=>assert_equals(
     exp = abap_true
     act = lo_gol->is_cell_alive( iv_row = 3 iv_col = 2 )
    ).
  ENDMETHOD.

ENDCLASS.
