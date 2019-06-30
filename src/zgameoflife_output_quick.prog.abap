CLASS lcl_output_quick DEFINITION
    INHERITING FROM lcl_output.
  PUBLIC SECTION.
    METHODS:
      display REDEFINITION.

ENDCLASS.

CLASS lcl_output_quick IMPLEMENTATION.

  METHOD display.
    " always use first site to avoid page stack overflow
    sy-lsind = 0.

    mac_write_header.
    FORMAT HOTSPOT ON.
    WRITE 'DO ITERATION' COLOR = col_total.
    FORMAT HOTSPOT OFF.
    WRITE '| AUTO'. WRITE gv_auto_on AS CHECKBOX.
    NEW-LINE.
    DATA(lv_prev_linno) = sy-linno.

    LOOP AT it_playfield
        ASSIGNING FIELD-SYMBOL(<ls_cell>).
      sy-linno = lv_header_offset + <ls_cell>-row.
      WRITE AT <ls_cell>-col(*) <ls_cell>-alive NO-GAP.
    ENDLOOP.

    IF gv_auto_on = abap_true.
      DATA lv_task TYPE string.
      CALL FUNCTION 'C14Z_WAIT_N_SECONDS' STARTING NEW TASK lv_task
        PERFORMING trigger_update ON END OF TASK.
    ENDIF.

    sy-linno = lv_prev_linno.
  ENDMETHOD.

ENDCLASS.
