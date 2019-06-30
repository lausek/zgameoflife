REPORT zgameoflife NO STANDARD PAGE HEADING.

DEFINE mac_write_hr.
  WRITE '-------------------------------------------------------'. NEW-LINE.
END-OF-DEFINITION.

DEFINE mac_write_header.
  FORMAT COLOR COL_BACKGROUND.
  WRITE '   ______                     ____  ______    _ ____   '. NEW-LINE.
  WRITE '  / ____/___ _____ ___  ___  / __ \/ __/ /   (_) __/__ '. NEW-LINE.
  WRITE ' / / __/ __ `/ __ `__ \/ _ \/ / / / /_/ /   / / /_/ _ \'. NEW-LINE.
  WRITE '/ /_/ / /_/ / / / / / /  __/ /_/ / __/ /___/ / __/  __/'. NEW-LINE.
  WRITE '\____/\__,_/_/ /_/ /_/\___/\____/_/ /_____/_/_/  \___/ '. NEW-LINE.
  mac_write_hr.
  WRITE '~~ by lausek |'.
  DATA(lv_header_offset) = 8.
END-OF-DEFINITION.

CLASS lcl_initializer DEFINITION DEFERRED.
CLASS lcl_output DEFINITION DEFERRED.

CLASS lcl_gameoflife DEFINITION.

  PUBLIC SECTION.
    CONSTANTS:
      cv_rows TYPE i VALUE 22,
      cv_cols TYPE i VALUE 70.

    TYPES:
      BEGIN OF gtys_cell,
        row   TYPE i,
        col   TYPE i,
        alive TYPE abap_bool,
      END OF gtys_cell,
      gtyt_playfield TYPE STANDARD TABLE OF gtys_cell WITH EMPTY KEY.

    CLASS-DATA:
      go_gameoflife TYPE REF TO lcl_gameoflife READ-ONLY,
      go_output     TYPE REF TO lcl_output READ-ONLY,
      gv_auto_on    TYPE abap_bool.

    DATA:
      gt_playfield TYPE gtyt_playfield READ-ONLY.

    CLASS-METHODS:
      main,
      update.

    METHODS:
      constructor
        IMPORTING
          io_initializer TYPE REF TO lcl_initializer OPTIONAL,
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

CLASS lcl_output DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor,
      display ABSTRACT
        IMPORTING
          it_playfield TYPE lcl_gameoflife=>gtyt_playfield.

ENDCLASS.

CLASS lcl_output IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

DATA:
  gv_auto_on TYPE abap_bool.

INCLUDE:
  zgameoflife_output,
  zgameoflife_output_quick,
  zgameoflife_cls,
  zgameoflife_test.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'UPD'.
      PERFORM run_update USING 7.
  ENDCASE.

AT LINE-SELECTION.
  PERFORM run_update USING sy-lilli.

START-OF-SELECTION.
  lcl_gameoflife=>main( ).

FORM run_update
    USING
        iv_lilli TYPE i.
  CASE iv_lilli.
    WHEN 7.
      READ LINE 7 FIELD VALUE gv_auto_on.
      lcl_gameoflife=>gv_auto_on = gv_auto_on.
      lcl_gameoflife=>update( ).
  ENDCASE.
ENDFORM.

FORM trigger_update
  USING
    p_task TYPE clike.
  SET USER-COMMAND 'UPD'.
ENDFORM.
