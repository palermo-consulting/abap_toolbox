%&%& RDIRZ_GVNA0352_PAYROLL_ROUTINES
Z_GVNA0352_PAYROLL_ROUTINES               X           1         GVNA0352    20111103GVNA0352    20120208000029     010EX 2014050806173420120208200611                    X
%&%& REPOZ_GVNA0352_PAYROLL_ROUTINES
*&---------------------------------------------------------------------*
*& Report  Z_GVNA0352_PAYROLL_ROUTINES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_gvna0352_payroll_routines.

TYPES: BEGIN OF gt_rt1,
        abart TYPE  abrar, "Employee subgroup grouping for personnel calculation rule
        lgart TYPE  lgart, "Wage Type
        v0typ TYPE  v0typ, "Variable assignment type
        v0znr TYPE  v0znr, "Variable assignment number
        betpe TYPE  betpe, "Payroll: Amount per unit
        anzhl TYPE  pranz, "HR payroll: Number
        betrg TYPE  maxbt, "HR Payroll: Amount
       END OF gt_rt1.

DATA: gt_rgdir            TYPE STANDARD TABLE OF pc261,
      gs_rgdir            TYPE pc261,
      gt_evpdir           TYPE STANDARD TABLE OF pc261,
      gs_evpdir           TYPE pc261,
      gt_previous_results TYPE STANDARD TABLE OF pc261,
      gs_previous_results TYPE pc261,
      gt_rt               TYPE STANDARD TABLE OF pc207,
      gs_rt               TYPE pc207,
      gv_calcd            TYPE c,
      gs_in_entry         TYPE pc261,
      gs_out_entry        TYPE pc261,
      gs_payresult        TYPE payus_result.


INCLUDE rpcccd09.

PARAMETERS p_pernr LIKE pernr-pernr.
PARAMETERS p_inper LIKE gs_rgdir-inper.
PARAMETERS p_lgart LIKE t512w-lgart.

CALL FUNCTION 'HR_PCLX_INIT_BUFFER'.

* Read RGDIR
CALL FUNCTION 'CU_READ_RGDIR'
  EXPORTING
    persnr          = p_pernr
  TABLES
    in_rgdir        = gt_rgdir
  EXCEPTIONS
    no_record_found = 1
    OTHERS          = 2.

SORT gt_rgdir BY seqnr DESCENDING.
LOOP AT gt_rgdir INTO gs_rgdir WHERE inper EQ p_inper AND fpper EQ p_inper.
  EXIT.
ENDLOOP.
SORT gt_rgdir BY seqnr ASCENDING.

CALL FUNCTION 'CD_EVALUATION_PERIODS'
  EXPORTING
    bonus_date      = '00000000'
    inper_modif     = gs_rgdir-iperm
    inper           = gs_rgdir-inper
    pay_type        = gs_rgdir-inpty
    pay_ident       = gs_rgdir-inpid
  TABLES
    rgdir           = gt_rgdir
    evpdir          = gt_evpdir
*   IABKRS          =
  EXCEPTIONS
    no_record_found = 1
    OTHERS          = 2.

* Read regular payroll results
* A results (original result plus retroactive calculations)
* and P results

LOOP AT gt_evpdir INTO gs_evpdir WHERE srtza = cd_c-actual.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      clusterid                    = 'RU'
      employeenumber               = p_pernr
      sequencenumber               = gs_evpdir-seqnr
    CHANGING
      payroll_result               = gs_payresult
    EXCEPTIONS
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      error_reading_archive        = 8
      error_reading_relid          = 9
      OTHERS                       = 10.

  gt_rt[] = gs_payresult-inter-rt[].
  SORT gt_rt BY lgart.
  DELETE gt_rt WHERE lgart NE '/101'.

  CALL FUNCTION 'CD_RETROCALC_PERIOD'
    EXPORTING
      entry  = gt_evpdir
    IMPORTING
      calcd  = gv_calcd
    EXCEPTIONS
      OTHERS = 1.
* Determine whether retro result
  CHECK gv_calcd = 'X'.
* Special processing: Only the retro period is processed.
  gs_in_entry = gs_evpdir.

  CALL FUNCTION 'CD_READ_PREVIOUS'
    EXPORTING
      in_record       = gs_in_entry
    TABLES
      rgdir           = gt_rgdir
      out_rgdir       = gt_previous_results
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

* Output structure is a table, since there can be
* several previous results: for example, if legal person
* changes, and is retroactively deleted
  LOOP AT gt_previous_results INTO gs_previous_results.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        clusterid                    = 'RU'
        employeenumber               = p_pernr
        sequencenumber               = gs_previous_results-seqnr
      CHANGING
        payroll_result               = gs_payresult
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        error_reading_archive        = 8
        error_reading_relid          = 9
        OTHERS                       = 10.


  ENDLOOP.

ENDLOOP.

BREAK-POINT.
%&%& TEXPZ_GVNA0352_PAYROLL_ROUTINES
RProgram Z_GVNA0352_PAYROLL_ROUTINES       35
%&%& HEADZ_GVNA0352_PAYROLL_ROUTINES
DOKU      ZI_C634_M07_RSPINT00                                                  RE  E                                                  S_DOCU_SHOW     S_DOCUS100005GVNA0352    702 20130220201527GVNA0352    702 2013022022143707200000  0
%&%& DOKLZ_GVNA0352_PAYROLL_ROUTINES
