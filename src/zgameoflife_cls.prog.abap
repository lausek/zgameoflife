CLASS lcl_initializer DEFINITION.
  PUBLIC SECTION.
    METHODS:
      initialize
        CHANGING
          ct_playfield TYPE lcl_gameoflife=>gtyt_playfield.

ENDCLASS.

CLASS lcl_initializer IMPLEMENTATION.
  METHOD initialize.
    DATA(lo_rand) = cl_abap_random=>create( seed = CONV #( sy-uzeit ) ).
    LOOP AT ct_playfield
      ASSIGNING FIELD-SYMBOL(<ls_cell>).
      <ls_cell>-alive = boolc( '0.5' < lo_rand->decfloat16( ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test_initializer DEFINITION
  INHERITING FROM lcl_initializer.
  PUBLIC SECTION.
    METHODS:
      initialize REDEFINITION.

ENDCLASS.

CLASS lcl_test_initializer IMPLEMENTATION.
  METHOD initialize.
    DEFINE mac_set_alive.
      ct_playfield[ row = &1 col = &2 ]-alive = abap_true.
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
ENDCLASS.

CLASS lcl_gameoflife IMPLEMENTATION.

  METHOD main.
    go_gameoflife = NEW lcl_gameoflife( NEW lcl_initializer( ) ).
    go_output = NEW lcl_output_quick( ).

    DO 4 TIMES.
      go_gameoflife->do_iteration( ).
    ENDDO.

    go_output->display( go_gameoflife->gt_playfield ).
  ENDMETHOD.

  METHOD update.
    ASSERT go_gameoflife IS BOUND AND go_output IS BOUND.
    go_gameoflife->do_iteration( ).
    go_output->display( go_gameoflife->gt_playfield ).
  ENDMETHOD.

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
    IF io_initializer IS SUPPLIED.
      io_initializer->initialize( CHANGING ct_playfield = gt_playfield ).
    ENDIF.
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
