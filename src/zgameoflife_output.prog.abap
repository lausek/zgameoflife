CLASS lcl_output_grid DEFINITION
    INHERITING FROM lcl_output.
  PUBLIC SECTION.
    METHODS:
      display REDEFINITION.

  PROTECTED SECTION.
    DATA:
      lo_salv             TYPE REF TO cl_salv_table,
      lr_output_playfield TYPE REF TO data.

ENDCLASS.

CLASS lcl_output_grid IMPLEMENTATION.

  METHOD display.
    DATA:
        lt_comps TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS:
        <lt_output_playfield> TYPE STANDARD TABLE.

    IF lo_salv IS NOT BOUND.
      DATA(lo_output_cety) = cl_abap_datadescr=>describe_by_name( 'ABAP_BOOL' ).
      DATA(lo_output_lnty) = cl_abap_structdescr=>create(
          VALUE #(
              FOR GROUPS lg_columns OF <ls_cell> IN it_playfield GROUP BY ( col = <ls_cell>-col )
              (
                  name = |_{ lg_columns-col }|
                  type = CAST cl_abap_datadescr( lo_output_cety )
              )
          )
      ).
      DATA(lo_output_ty) = cl_abap_tabledescr=>create( lo_output_lnty ).

      CREATE DATA lr_output_playfield TYPE HANDLE lo_output_ty.
      ASSIGN lr_output_playfield->* TO <lt_output_playfield>.

      LOOP AT it_playfield
          INTO DATA(ls_cell)
          GROUP BY ( row = ls_cell-row )
          REFERENCE INTO DATA(lg_rows).

        APPEND INITIAL LINE TO <lt_output_playfield>
            ASSIGNING FIELD-SYMBOL(<ls_output_line>).

        LOOP AT GROUP lg_rows
            ASSIGNING FIELD-SYMBOL(<ls_nothercell>).
          ASSIGN COMPONENT |_{ <ls_nothercell>-col }|
              OF STRUCTURE <ls_output_line>
              TO FIELD-SYMBOL(<lg_cell>).
          ASSERT sy-subrc = 0.
          <lg_cell> = <ls_nothercell>-alive.
        ENDLOOP.

      ENDLOOP.

      cl_salv_table=>factory(
          IMPORTING
              r_salv_table = lo_salv
          CHANGING
              t_table = <lt_output_playfield>
      ).

      lo_salv->display( ).
    ELSE.
      ASSIGN lr_output_playfield->* TO <lt_output_playfield>.

      LOOP AT it_playfield
          ASSIGNING FIELD-SYMBOL(<ls_mcell>).
        ASSIGN <lt_output_playfield>[ <ls_mcell>-row ]
            TO FIELD-SYMBOL(<ls_line>).
        ASSERT sy-subrc = 0.
        ASSIGN COMPONENT |_{ <ls_mcell>-col }|
            OF STRUCTURE <ls_line>
            TO <lg_cell>.
        ASSERT sy-subrc = 0.
        <lg_cell> = <ls_mcell>-alive.
      ENDLOOP.

      lo_salv->refresh( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
