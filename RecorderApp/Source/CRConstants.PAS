{===============================================================================
  Unit:        ThamesConstants.pas

  Description: This unit contains constants used throughout the application.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 15 $
    $Date: 16/05/08 12:48 $
    $Author: Johndurman $
===============================================================================}
unit CRConstants;

interface

const
  AT_CAPTION = 'caption';
  AT_DATASIZE = 'size';
  AT_DATATYPE = 'datatype';
  AT_DEFAULT = 'default';
  AT_DESCRIPTION = 'description';
  AT_ENTRYCOUNT = 'entrycount';
  AT_FIELD = 'field';
  AT_FIELDNAME = 'name';
  AT_FILENAME = 'filename';
  AT_FILE = 'file';
  AT_INCLUDEPARTIALOVERLAP = 'includepartialoverlap';
  AT_KEYCOLUMN = 'keycolumn';
  AT_KEYTYPE = 'keytype';
  AT_MENUPATH = 'menupath';
  AT_NAME = 'name';
  AT_OPERATOR = 'operator';
  AT_POSITION = 'position';
  AT_SELECTONEOFGROUP = 'selectoneofgroup';
  AT_SQL = 'SQL';
  AT_SUBSTITUTEFOR = 'SubstituteFor';
  AT_TABLENAME = 'tablename';
  AT_TABLEFIELD = 'tablefield';
  AT_TEMPLATE = 'template';
  AT_TITLE = 'title';
  AT_TYPE = 'type';
  AT_USERUCKSACK = 'userucksack';
  AT_VALUE = 'value';
  AT_VISIBLE = 'visible';
  AT_WIDTH = 'width';
  AT_INPUTFILE = 'inputfile';

  EL_BATCHUPDATE = 'batchupdate';
  EL_CLAUSECONDITION = 'Condition';
  EL_CSVCOLUMN = 'CSVColumn';
  EL_CSVCOLUMNS = 'CSVColumns';
  EL_CSVFILE = 'CSVFile';
  EL_CSVTABLE = 'CSVTable';
  EL_CSVTABLES = 'CSVTables';
  EL_CONDITIONOPTION = 'Option';
  EL_CUSTOMREPORT = 'CustomReport';
  EL_OUTPUT = 'Output';
  EL_REPORTCOLUMN = 'Column';
  EL_REPORTCOLUMNS = 'Columns';
  EL_REPORTSQL = 'SQL';
  EL_TEMPLATE = 'Template';
  EL_WHERECLAUSE = 'Where';
  EL_ORDERBYCLAUSE = 'Orderby';
  EL_OUTPUTTABLEORDERBYCLAUSE = 'OutputTableOrderby';
  EL_ORDERBYCLAUSECOMPONENTS = 'OrderbyClauseComponents';
  EL_EXTERNAL_FILTER_FILE = 'externalfilterfile';
  EL_EXTERNAL_FILTER_FILES = 'externalfilterfiles';

  SQL_CREATETEMPTABLE = 'CREATE TABLE %s (%s)';
  SQL_DROPTEMPTABLE = 'IF OBJECT_ID(''tempdb.dbo.%s'') IS NOT NULL DROP TABLE %s';
  SQL_INSERTRECORD = 'INSERT %s (%s) VALUES (%s)';
  SQL_SELECT_TEMP_TABLE = 'SELECT * FROM #REPORT_OUTPUT';
  SQL_READUNCOMMITTED = 'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
  SQL_READCOMMITTED = 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
  BU_WHERECLAUSEMARKER = '{WHERE}';
  SQL_SAMPLES_FOR_POLYGON = ' SELECT RecordKey FROM #TempList ';

  CONST_RESULTS_GRID_DEFAULT_COLUMN_WIDTH = 200;

implementation

end.

