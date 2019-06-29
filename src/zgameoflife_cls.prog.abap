CLASS lcl_gameoflife IMPLEMENTATION.

  METHOD main.
*    DATA(lo_gol) = NEW lcl_gameoflife( ).
*    DATA(lo_output) = NEW lcl_output( ).
*
*    DO 2 TIMES.
*      lo_gol->do_iteration( ).
*      lo_output->display( lo_gol->gt_playfield ).
*    ENDDO.
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
