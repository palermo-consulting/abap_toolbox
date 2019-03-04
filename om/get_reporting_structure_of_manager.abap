CLEAR lv_pernr.
SELECT SINGLE pernr INTO lv_pernr FROM pa0105 WHERE usrid EQ sy-uname AND usrty EQ '0001'.
IF sy-subrc EQ 0.
  CLEAR lv_ordunit.
  SELECT SINGLE  orgeh INTO lv_ordunit FROM pa0001 WHERE pernr EQ lv_pernr AND endda GE sy-datum.

  CALL FUNCTION 'RH_DIR_ORG_STRUC_GET'
    EXPORTING
      act_orgunit = lv_ordunit
    TABLES
      person_tab  = person_tab.
ENDIF.
