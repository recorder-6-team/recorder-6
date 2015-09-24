//==============================================================================
//  Unit:        SchemeManagerData
//
//  Implements:  TdmSchemeManager
//
//  Description:
//
//  Author:      John van Breda
//  Created:     27 February 2002
//
//  Last Revision Details:
//    $Revision: 10 $
//    $Date: 19/12/07 10:55 $
//    $Author: Rickyshrestha $
//
//  $History: SchemeManagerData.pas $
//  
//  *****************  Version 10  *****************
//  User: Rickyshrestha Date: 19/12/07   Time: 10:55
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  ResStr_NoSurveyExist
//  
//  *****************  Version 9  *****************
//  User: Ericsalmon   Date: 23/05/03   Time: 16:53
//  Updated in $/JNCC/Development/Build/Source
//  DFMs to Text.
//  
//  *****************  Version 8  *****************
//  User: Michaelbailey Date: 20/11/02   Time: 10:21
//  Updated in $/JNCC/Source
//  CCN No:  CEDaR10    Recording schemes changed to reference new Export
//  Filters instead of Surveys.
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 14/06/02   Time: 16:08
//  Updated in $/JNCC/Source
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 14/06/02   Time: 15:53
//  Updated in $/JNCC/Source
//  Bug fix: SQL for insert and update when no export format is specified.
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit SchemeManagerData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, ADODB, DatabaseAccessADO, DataClasses;

type
  TdmSchemeManager = class(TBaseDataModule)
    qrySchemes: TJNCCQuery;
    dsSchemes: TDataSource;
    qrySchemesRECORDING_SCHEME_KEY: TStringField;
    qrySchemesSCHEME_NAME: TStringField;
    qrySchemesSITEM_NAME: TStringField;
    qrySchemesEXPORT_FILTER_KEY: TStringField;
    qrySchemesLAST_CONTRIBUTION_DATE: TDateTimeField;
    qryExportFilterLookup: TJNCCQuery;
    qrySchemesEMAIL: TStringField;
    qrySchemesIndicator: TBooleanField;
    qrySchemesFORMAT_NAME: TStringField;
    qrySchemesEXPORT_FORMAT_ID: TIntegerField;
    qryExportFormatLookup: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    class function GetSurveyName(const key: TKeyString): string;
    constructor Create(AOwner : TComponent); override;
    procedure UpdateRow( var ioRowKey : string; iData : TStringList );
    procedure UpdateRowKnownInsert(var ioRowKey: string;
      iData: TStringList; const iInsert : boolean);
    procedure DeleteRow( iRowKey : String );
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, GeneralFunctions, ExceptionForm;

const
  SQL_INSERT_SCHEME = 'INSERT INTO Recording_Scheme ' +
                      '(Recording_Scheme_Key, Item_Name, Email, Export_Filter_Key, Export_Format_Id) ' +
                      'VALUES ' +
                      '(''%s'', ''%s'', ''%s'', %s, %s)';

  SQL_UPDATE_SCHEME = 'UPDATE Recording_Scheme ' +
                      'SET Item_Name = ''%s'', ' +
                      '    Email = ''%s'', ' +
                      '    Export_Filter_Key = %s, ' +
                      '    Export_Format_Id = %s ' +
                      'WHERE Recording_Scheme_Key = ''%s''';

  SQL_GET_SURVEY    = 'SELECT Item_Name FROM Survey WHERE Survey_Key = "%s"';

resourcestring
  ResStr_NoSurveyExist =  'Survey does not exist.';

//==============================================================================
{ TdmSchemeManager }
//==============================================================================
constructor TdmSchemeManager.Create(AOwner: TComponent);
begin
  inherited;
  qrySchemes.Open;
end;  // Create

//==============================================================================
{ Callback from the grid manager.  Deletes the row key in RECORDING_SCHEME }
procedure TdmSchemeManager.DeleteRow(iRowKey: String);
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'Delete from RECORDING_SCHEME where RECORDING_SCHEME_KEY=''' + iRowKey +  '''';
    ExecSQL;
  end;
end;  // DeleteRow

//==============================================================================
{ Callback from the grid manager.  Updates the row key in RECORDING_SCHEME
   according to the supplied data list.  If the key is null, then gets a new
   key and appends it }
class function TdmSchemeManager.GetSurveyName(const key: TKeyString): string;
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(SQL_GET_SURVEY, [key]);
    Open;
    try
      if RecordCount <> 1 then
        raise TExceptionPath.CreateNonCritical(ResStr_NoSurveyExist);
      Result := FieldByName('Item_Name').AsString;
    finally Close end;
  end;
end;

procedure TdmSchemeManager.UpdateRow(var ioRowKey: string; iData: TStringList);
begin
  UpdateRowKnownInsert( ioRowKey, iData, ioRowKey='' );
end;  // UpdateRow

//==============================================================================
{ As above, but where we know if the record exists.  This allows a new row to
    be created for a fixed key }
procedure TdmSchemeManager.UpdateRowKnownInsert(var ioRowKey: string;
  iData: TStringList; const iInsert : boolean);
var
  lstExportFilterKey : String;
  lstSQLName         : String;
  lstExportFormat    : String;
begin
  // Check for null export filter key so SQL is correct
  if iData.Values['Export_Filter_Key'] <> '' then
    lstExportFilterKey := '''' + iData.Values['Export_Filter_Key'] + ''''
  else
    lstExportFilterKey := 'Null';
  // Duplicate single quotes to make string SQL safe
  lstSQLName := DuplicateCharacters(iData.Values['Scheme_Name'], '''');
  // If not recording scheme was specified, defaults to XML
  lstExportFormat := iData.Values['EXPORT_FORMAT_ID'];
  if lstExportFormat = '' then lstExportFormat := '1';

  with dmGeneralData.qryAllPurpose do begin
    if iInsert then begin
      if ioRowKey = '' then // get new key if needed
        ioRowKey := dmGeneralData.IDGenerator.GetNextKey('RECORDING_SCHEME');
      SQL.Text := Format(SQL_INSERT_SCHEME,
                         [ioRowKey, lstSQLName, iData.Values['Email'], lstExportFilterKey, lstExportFormat]);
      ExecSQL;
    end else begin
      // existing record to update
      SQL.Text := Format(SQL_UPDATE_SCHEME,
                         [lstSQLName, iData.Values['Email'], lstExportFilterKey, lstExportFormat, ioRowKey]);
      ExecSQL;
    end;
  end; // with qryAllPurpose
end;  // UpdateRowKnownInsert

//==============================================================================
end.
