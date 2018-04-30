class ZCL_ALV_SIMPLE definition
  public
  create public .

public section.

  type-pools SLIS .
  methods CONSTRUCTOR
    importing
      value(IM_TAB_NAME) type SLIS_TABNAME default 'GT_ALVTAB' .
  methods ADD_COLUMN
    importing
      value(IM_FIELDNAME) type SLIS_FIELDNAME
      value(IM_SELTEXT_M) type DD03P-SCRTEXT_M optional
      value(IM_SELTEXT_L) type DD03P-SCRTEXT_L optional.
  methods DISPLAY
    changing
      !IM_ALVDATA type TABLE .
protected section.

  type-pools SLIS .
  data T_FIELDCAT type SLIS_T_FIELDCAT_ALV .
  data S_FIELDCAT type SLIS_FIELDCAT_ALV .
  data S_LAYOUT type SLIS_LAYOUT_ALV .
private section.
ENDCLASS.



CLASS ZCL_ALV_SIMPLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_SIMPLE->ADD_COLUMN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FIELDNAME                   TYPE        SLIS_FIELDNAME
* | [--->] IM_SELTEXT_M                   TYPE        DD03P-SCRTEXT_M
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_column.

  s_fieldcat-fieldname = im_fieldname.
  IF im_seltext_m IS INITIAL AND im_seltext_l IS INITIAL.
	s_fieldcat-seltext_m = 'Column'.
	s_fieldcat-seltext_l = 'Column'.
  ELSE.
    s_fieldcat-seltext_m = im_seltext_m.
    s_fieldcat-seltext_l = im_seltext_l.
  ENDIF.
  APPEND s_fieldcat TO t_fieldcat.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_SIMPLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB_NAME                    TYPE        SLIS_TABNAME (default ='GT_ALVTAB')
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.
  s_layout-zebra   = 'X'.
  s_layout-colwidth_optimize = 'X'.
  s_layout-cell_merge  = 'N'.
  s_fieldcat-tabname = im_tab_name.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_SIMPLE->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [<-->] IM_ALVDATA                     TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program                = sy-cprog
     is_layout                         = s_layout
     it_fieldcat                       = t_fieldcat
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = im_alvdata
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.
ENDCLASS.