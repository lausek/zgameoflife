REPORT zgameoflife.

CLASS lcl_gameoflife DEFINITION.

  PUBLIC SECTION.
    CONSTANTS:
      cv_rows TYPE i VALUE 10,
      cv_cols TYPE i VALUE 10.

    TYPES:
      BEGIN OF gtys_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF gtys_cell,
      gtyt_playfield TYPE STANDARD TABLE OF gtys_cell WITH EMPTY KEY.

    DATA:
      gt_playfield TYPE gtyt_playfield READ-ONLY.

    CLASS-METHODS:
      main.

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

ENDCLASS.

CLASS lcl_output DEFINITION.
  PUBLIC SECTION.
    METHODS:
      draw
        IMPORTING
          it_playfield TYPE lcl_gameoflife=>gtys_cell.

ENDCLASS.

INCLUDE:
  zgameoflife_output,
  zgameoflife_cls,
  zgameoflife_test.

START-OF-SELECTION.
  lcl_gameoflife=>main( ).
