*&---------------------------------------------------------------------*
*& Report ZREMAINDER_DATES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zremainder_dates.

TABLES : pa0019, pa0002, pa0041.
CLASS list DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS detail.

ENDCLASS.

DATA: gv_current_date TYPE d.


*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_user_command USING i_function TYPE salv_de_function
                              i_text     TYPE string.

*   DATA: l_string TYPE string.

  IF i_function EQ 'PREV'.
    IF gv_current_date+4(2) GT 1.
      gv_current_date+4(2) = gv_current_date+4(2) - 1.
    ELSE.
      gv_current_date+4(2) = 12.
      gv_current_date(4) = gv_current_date(4) - 1.
    ENDIF.
  ENDIF.

  IF i_function EQ 'NEXT'.
    IF gv_current_date+4(2) LT 12.
      gv_current_date+4(2) = gv_current_date+4(2) + 1.
    ELSE.
      gv_current_date+4(2) = 1.
      gv_current_date(4) = gv_current_date(4) + 1.
    ENDIF.
  ENDIF.

  list=>detail( ).

*  CONCATENATE i_text i_function INTO l_string SEPARATED BY space.
*
*  MESSAGE i000(0k) WITH l_string.

ENDFORM.                    " handle_user_command

*... §5 Definition is later
CLASS lcl_handle_events DEFINITION DEFERRED.


DATA: gt_outtab TYPE STANDARD TABLE OF alv_t_t2.

DATA: gr_table   TYPE REF TO cl_salv_table.

DATA: gr_container TYPE REF TO cl_gui_custom_container.

DATA: g_okcode TYPE syucomm.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function TEXT-i08.
  ENDMETHOD.                    "on_user_command

  METHOD on_before_user_command.
    PERFORM handle_user_command USING e_salv_function TEXT-i09.
  ENDMETHOD.                    "on_before_user_command

  METHOD on_after_user_command.
    PERFORM handle_user_command USING e_salv_function TEXT-i10.
  ENDMETHOD.                    "on_after_user_command

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

CLASS list IMPLEMENTATION.

  METHOD detail.
    TYPES: BEGIN OF alv_line,
             mndat TYPE pa0019-mndat,
             termn TYPE pa0019-termn,
             tmtxt TYPE t531s-tmtxt,
             ename TYPE pa0001-ename,
           END OF alv_line.
    DATA : wa_alv TYPE alv_line.
    DATA : alv_tab    TYPE TABLE OF alv_line,person_tab  TYPE TABLE OF rhldapp,wa_person TYPE  rhldapp.

    DATA : start_date TYPE d,end_date TYPE d,lv_pernr TYPE pa0001-pernr,lv_ordunit TYPE objec-objid.

    IF gv_current_date IS INITIAL.
      gv_current_date = sy-datum.
    ENDIF.

    CLEAR lv_pernr.
    SELECT SINGLE pernr INTO lv_pernr FROM pa0105 WHERE usrid EQ sy-uname AND usrty EQ '0001'.
    IF sy-subrc EQ 0.
      CLEAR lv_ordunit.
      SELECT SINGLE  orgeh INTO lv_ordunit FROM pa0001 WHERE pernr EQ lv_pernr AND endda GE sy-datum.

      CALL FUNCTION 'RH_DIR_ORG_STRUC_GET'
        EXPORTING
          act_orgunit = lv_ordunit
*         ACT_PLVAR   =
*         ACT_DATE    = SY-DATUM
*         SORT_FLAG   = 'X'
*         ADD_FLAG_PDATA        = 'X'
        TABLES
*         ORG_UNITS   =
          person_tab  = person_tab
*         ORG_PERS_REL          =
*     EXCEPTIONS
*         NO_ACTIVE_PLVAR       = 1
*         OTHERS      = 2
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      LOOP AT person_tab INTO wa_person.

        CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
          EXPORTING
            iv_date             = gv_current_date
          IMPORTING
            ev_month_begin_date = start_date
            ev_month_end_date   = end_date.

        SELECT * FROM pa0019 INTO pa0019 WHERE ( ( mndat GE start_date AND mndat LE end_date ) ) AND pernr EQ wa_person-objid.
          wa_alv-mndat = pa0019-mndat.
          wa_alv-termn = pa0019-termn.
          SELECT SINGLE tmtxt INTO wa_alv-tmtxt FROM t531s WHERE sprsl EQ sy-langu AND tmart EQ pa0019-tmart.
          SELECT SINGLE ename INTO wa_alv-ename FROM pa0001 WHERE pernr EQ pa0019-pernr.
          APPEND wa_alv TO alv_tab.
          CLEAR wa_alv.
        ENDSELECT.



        SELECT * FROM pa0002 INTO pa0002 WHERE begda LE end_date AND endda GE start_date AND pernr EQ wa_person-objid ORDER BY endda DESCENDING.

          DATA: lv_gbdat TYPE pa0002-gbdat.

          MOVE pa0002-gbdat TO lv_gbdat.
          lv_gbdat(4) = gv_current_date(4).

          IF lv_gbdat GE start_date AND lv_gbdat LE end_date.
            MOVE TEXT-t02 TO wa_alv-tmtxt.

            MOVE lv_gbdat TO wa_alv-mndat.
            MOVE lv_gbdat TO wa_alv-termn.
            SELECT SINGLE ename INTO wa_alv-ename FROM pa0001 WHERE pernr EQ pa0002-pernr.
            APPEND wa_alv TO alv_tab.
            CLEAR wa_alv.
          ENDIF.

          EXIT.
        ENDSELECT.

        SELECT * FROM pa0041 INTO pa0041 WHERE begda LE end_date AND endda GE start_date AND pernr EQ wa_person-objid ORDER BY endda DESCENDING.

          DATA: lv_index  TYPE n LENGTH 2,
                lv_dar    TYPE c LENGTH 12 VALUE 'PA0041-DAR01',
                lv_dat    TYPE c LENGTH 12 VALUE 'PA0041-DAt01',
                lv_years  TYPE i,
                lv_yearst TYPE c LENGTH 2,
                lv_anniv  TYPE d.

          FIELD-SYMBOLS: <dar> TYPE pa0041-dar01, <dat> TYPE pa0041-dat01.

          DO 12 TIMES.
            lv_index = lv_index + 1.
            lv_dar+10(2) = lv_index.
            lv_dat+10(2) = lv_index.
            ASSIGN (lv_dar) TO <dar>.
            ASSIGN (lv_dat) TO <dat>.
            IF <dar> EQ 'U5'. "Anniversary date
              lv_anniv = <dat>.
              lv_anniv(4) = gv_current_date(4).
              IF lv_anniv GE start_date AND lv_anniv LE end_date.
                lv_years = gv_current_date(4) - <dat>(4).
                IF lv_years MOD 5 EQ 0.
                  lv_yearst = lv_years.
                  CONCATENATE TEXT-t03 ' (' lv_yearst ')' INTO wa_alv-tmtxt.
                  MOVE lv_anniv TO wa_alv-mndat.
                  MOVE lv_anniv TO wa_alv-termn.
                  SELECT SINGLE ename INTO wa_alv-ename FROM pa0001 WHERE pernr EQ pa0041-pernr.
                  APPEND wa_alv TO alv_tab.
                  CLEAR wa_alv.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDDO.

          EXIT.
        ENDSELECT.
*

      ENDLOOP.
    ENDIF.
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = DATA(alv)
          CHANGING  t_table = alv_tab ).

        alv->set_screen_status(
          pfstatus      =  'MAIN'
          report        =  sy-repid
          set_functions = alv->c_functions_all ).




*... §6 register to the events of cl_salv_table
        DATA: lr_events TYPE REF TO cl_salv_events_table.
*      ... §5 object for handling the events of cl_salv_table
        DATA: gr_events TYPE REF TO lcl_handle_events.
        lr_events = alv->get_event( ).


        CREATE OBJECT gr_events.

*... §6.1 register to the event USER_COMMAND
        SET HANDLER gr_events->on_user_command FOR lr_events.

        SET HANDLER gr_events->on_before_user_command FOR lr_events.

        SET HANDLER gr_events->on_after_user_command FOR lr_events.


*... set list title
        DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
              l_title             TYPE lvc_title,
              l_ltx               TYPE t247-ltx.

        SELECT SINGLE ltx FROM t247 INTO l_ltx WHERE spras EQ sy-langu AND mnr EQ gv_current_date+4(2).


        CONCATENATE TEXT-t01 l_ltx  gv_current_date(4) INTO l_title SEPARATED BY space.
        lr_display_settings = alv->get_display_settings( ).
        lr_display_settings->set_list_header( l_title ).


*        DATA: o_footer_element TYPE REF TO cl_salv_form_layout_grid,
*              o_header_info    TYPE REF TO cl_salv_form_header_info,
*              o_text           TYPE REF TO cl_salv_form_text.
*
**   Create an instance of Footer objects
*        CREATE OBJECT o_footer_element
*          EXPORTING
*            columns = 1.
*
**   Create a Text with layout HEADER INFORMATION
*        o_header_info = o_footer_element->create_header_information(
*          row = 1
*          column = 1
*          text     = 'Text of Footer Info'
*          tooltip  = 'Tooltip of Footer Info' ).
*
*        alv->set_end_of_list( o_footer_element ).

        alv->display( ).
      CATCH cx_salv_msg.
        MESSAGE 'ALV display not possible' TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
*
ENDCLASS.



START-OF-SELECTION.

  list=>detail( ).
