//==============================================================================
//  Unit:        ExternalFilter
//
//  Implements:  TExternalFilter, TFileFilter, TFilterFactory
//
//  Description: TExternalFilter defines the base class for an external filter,
//                and implements the common methods.
//                TFileFilter is an inherited class that loads an external filter from a file.
//                TFilterFactory is the factory calss that decides which inherited class of
//                TExternalFilter should be created.
//
//  Author:      David Kelly
//  Created:     4 January 2008
//
//  Last Revision Details:
//    $Revision: 27 $
//    $Date: 6/05/09 11:30 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit ExternalFilter;

interface

uses
  ApplicationSettings, Recorder2000_TLB, DataClasses, Classes, COMClasses, ExceptionForm,
  StrUtils, SysUtils, Dialogs, Forms, Maintbar, Controls, Constants, ADOInt,
  DatabaseAccessADO;

type
  EExternalFilterException = class(TExceptionPath);
  ELinkedRecordNotAvailable = class(EExternalFilterException);

  TExternalFilter = class(TObject)
  private
    FDataTypes: TStringList;
    FHints: TStringList;
    FForms: TStringList;
    FFilterTables: TStringList;
    function GetForms: TStringList;
    function TableNameToForm(const TableName: String): string;
    procedure CheckOpenForms;
    procedure LoadFilterTables;
    procedure LinkNonFilterTable(var ATableName, AKey, AHint: String);
    procedure ClearList(list: TStringList);
  protected
    function GetCorrectTableName(const tableName: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyFilter(ACheckForms: Boolean = True);
  end;

  TFileFilter = class(TExternalFilter)
  protected
    function SplitLine(const ALine: String): TStringList;
  public
    function LoadFilter(const Filter: String): Boolean;
  end;

  TListFilter = class(TExternalFilter)
  public
    function LoadFilter(const AKeyList: TKeyList; AValidationMessages: TStringList = nil): Boolean;
  end;

  TRecordsetFilter = class(TExternalFilter)
  public
    function LoadFilter(const ARecordset: _Recordset): Boolean;
  end;

{-------------------------------------------------------------------------------
}
implementation

uses
  ComObj, BaseADODataModule, GeneralFunctions;

resourcestring
  ResStr_NoHints = 'Internal error. No hints list has been created for table %s.';
  ResStr_ConfirmFilter = 'The filter contains data that applies to screens other than ' +
      'the one you are currently viewing. Do you want to filter these screens (select ' +
      'Yes), view the filter in just the current screen (select No), or cancel the filter?';
  ResStr_LinkedRecordInFilter = 'A %s record relating to this record is included in the filter.';
  ResStr_ExternalFilterInvalid = 'The filter file contains an invalid line and cannot be loaded: '#13#10'%s';
  ResStr_NoValidRecordsToFilter = 'No relevant records were found in the filter.';
  ResStr_MultiFieldKey =
      'The filter cannot be loaded because it contains references to the %s table '
      + 'which has more than one field in its primary key and therefore is not supported.';
  ResStr_LoadingFilter  = 'Loading filter...';
  ResStr_ApplyingFilter = 'Applying filter...';

  ResStr_ContinueLoadingLargeFilter =
      'There are a large number of items to filter on (1000+). This may take some time.'#13#10#13#10
      + 'Do you want to continue?';

const
  SQL_MASTER_DETAIL =
      'SELECT Master.%s FROM %s Master JOIN %s Detail ON Master.%s = Detail.%s WHERE Detail.%s = ''%s''';

  MAX_ITEMS_FOR_WARNING = 1000;

{===============================================================================
  TExternalFilter
}
{-------------------------------------------------------------------------------
  Base class constructor
}
constructor TExternalFilter.Create;
begin
  inherited;
  FDataTypes := TStringList.Create;
  FHints := TStringList.Create;
  FFilterTables := TStringList.Create;
  LoadFilterTables;
end;
    
{-------------------------------------------------------------------------------
  Base class destructor
}
destructor TExternalFilter.Destroy;
begin
  ClearList(FDataTypes);
  FDataTypes.Free;
  ClearList(FHints);
  FHints.Free;
  if Assigned(FForms) then
    FForms.Free;
  FFilterTables.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TExternalFilter.ClearList(list: TStringList);
var
  i: Integer;
begin
  for i := 0 to list.Count - 1 do
    TObject(list.Objects[i]).Free;
  list.Clear;
end;

{-------------------------------------------------------------------------------
  Checks if the forms the filter affects are open.
}
procedure TExternalFilter.CheckOpenForms;
var
  i: Integer;
  tableName: String;
begin
  FForms := GetForms;
  // If no forms are open, we do nothing here, which means all forms affected by
  // the filter will open when the filter is applied.
  // If active form is the import wizard, just finished import and user requested
  // to see the new records, so just go ahead regardless.
  if Assigned(frmMain.ActiveMDIChild)
     and not SameText(frmMain.ActiveMDIChild.Name, 'frmImportWizard') then
    // If the only affected form is the current one, we do nothing,
    // otherwise show a messagebox
    if (FForms.IndexOf(frmMain.ActiveMDIChild.Name) < 0) or (FForms.Count > 1) then
      case MessageDlg(ResStr_ConfirmFilter, mtConfirmation, mbYesNoCancel, 0) of
        // All discarded
        mrCancel:
          AppSettings.ClearAllFilteredRecords;
        // Discard all not related to current form
        mrNo:
          for i := AppSettings.ExternalFilters.Count - 1 downto 0 do begin
            tableName := AppSettings.ExternalFilters[i];
            if not SameText(TableNameToForm(tableName), frmMain.ActiveMDIChild.Name) then
              AppSettings.ClearFilteredRecords([tableName]);
          end;
      end;
end;
      
{-------------------------------------------------------------------------------
  Applies the filter, displaying all the appropriate screens.
}
procedure TExternalFilter.ApplyFilter(ACheckForms: Boolean = True);
var
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  frmMain.SetStatus(ResStr_ApplyingFilter);
  try
    if ACheckForms then
      CheckOpenForms;
    AppSettings.DisplayFilteredData;
  finally
    DefaultCursor(cursor);
    frmMain.SetStatus('');
  end;
  if AppSettings.ExternalFilters.Count = 0 then
    ShowInformation(ResStr_NoValidRecordsToFilter);
end;

{-------------------------------------------------------------------------------
}
function TExternalFilter.GetCorrectTableName(const tableName: String): String;
begin
  Result := tableName;

  // Some correction required for SOURCE.
  if SameText(tableName, TN_SOURCE) then Result := TN_REFERENCE;
end;

{-------------------------------------------------------------------------------
  Returns a list of the forms affected by the filter
}
function TExternalFilter.GetForms: TStringList;
var
  i: Integer;
  lForms: TStringList;
  formName: String;
begin
  lForms := TStringList.Create;
  for i := 0 to AppSettings.ExternalFilters.Count - 1 do begin
    formName := TableNameToForm(AppSettings.ExternalFilters[i]);
    if lForms.IndexOf(formName) < 0 then lForms.Add(formName);
  end;
  Result := lForms;
end;

{-------------------------------------------------------------------------------
  Returns the name of the form that displays the data from the given table
}
function TExternalFilter.TableNameToForm(const TableName: String): string;
begin
  if SameText(TableName, TN_SURVEY) or SameText(TableName, TN_SURVEY_EVENT) or
     SameText(TableName, TN_SAMPLE) or SameText(TableName, TN_TAXON_OCCURRENCE) or
     SameText(TableName, TN_BIOTOPE_OCCURRENCE) then
    Result := 'frmObservations'
  else if SameText(TableName, TN_LOCATION) or SameText(TableName, TN_LOCATION_FEATURE) then
    Result := 'frmLocations'
  else if SameText(TableName, TN_NAME) or
      SameText(TableName, TN_INDIVIDUAL) or
      SameText(TableName, TN_ORGANISATION) then
    Result := 'frmIndOrg'
  else if SameText(TableName, TN_REFERENCE) then
    Result := 'frmReferences'
  else if SameText(TableName, TN_TAXON_LIST_ITEM) then
    Result := 'frmTaxonDictBrowser'
  else if SameText(TableName, TN_BIOTOPE_LIST_ITEM) then
    Result := 'frmBiotopeDictBrowser'
  else if SameText(TableName, TN_ADMIN_AREA) then
    Result := 'frmAdminAreaDictBrowser';
end;

{-------------------------------------------------------------------------------
  Populates FFilterTables with a list of the tables we can filter by
}
procedure TExternalFilter.LoadFilterTables;
begin
  FFilterTables.Add(TN_SURVEY);
  FFilterTables.Add(TN_SURVEY_EVENT);
  FFilterTables.Add(TN_SAMPLE);
  FFilterTables.Add(TN_TAXON_OCCURRENCE);
  FFilterTables.Add(TN_BIOTOPE_OCCURRENCE);
  FFilterTables.Add(TN_LOCATION);
  FFilterTables.Add(TN_LOCATION_FEATURE);
  FFilterTables.Add(TN_NAME);
  FFilterTables.Add(TN_INDIVIDUAL);
  FFilterTables.Add(TN_ORGANISATION);
  FFilterTables.Add(TN_REFERENCE);
  FFilterTables.Add(TN_TAXON_LIST_ITEM);
  FFilterTables.Add(TN_BIOTOPE_LIST_ITEM);
  FFilterTables.Add(TN_ADMIN_AREA);
end;

{-------------------------------------------------------------------------------
  If a table in the filter is not one of FFilterTables, we find a linked
  record that is.
}
procedure TExternalFilter.LinkNonFilterTable(var ATableName, AKey, AHint: String);
var
  lRS: _Recordset;
  lMasterTable, lMasterField, lDetailTable, lDetailField: String;
  sql: String;
begin
  // retrieve the relationship to the master table which relates to the node that
  // must be shown
  lRS := dmDatabase.GetRecordset('usp_GetMainNodeTableForSubDetailTable', ['@DetailTable', ATableName]);

  // Retrieve the key of the linked record.
  if not lRS.Eof then begin
    with lRS do begin
      lMasterTable := Fields['Master_Table'].Value;
      lMasterField := Fields['Master_Field'].Value;
      lDetailTable := Fields['Detail_Table'].Value;
      lDetailField := Fields['Detail_Field'].Value;
      Close;
    end;

    try
      sql := Format(
          SQL_MASTER_DETAIL,
          [lMasterField, lMasterTable, lDetailTable, lMasterField, lDetailField,
           dmDatabase.GetPrimaryKey(lDetailTable, False), AKey]);
      lRS := dmDatabase.ExecuteSQL(sql, True);

      if lRS.Eof and lRS.Bof then
        raise ELinkedRecordNotAvailable.CreateNonCritical('');

      // Add a sentence informing the user of the original record to the hint
      AHint := Format(ResStr_LinkedRecordInFilter, [ReadableFormat(lDetailTable)]);
      
      // Set the table and key values to those of the linked record
      ATableName := lMasterTable;
      AKey := lRS.Fields[lMasterField].Value;
    except
      on EMultiFieldKey do
        raise EExternalFilterException.CreateNonCritical(
            Format(ResStr_MultiFieldKey, [lDetailTable]));
    end;
  end;
end;

{===============================================================================
  TFileFilter
}
{-------------------------------------------------------------------------------
  Loads an external filter from a .ref file
}
function TFileFilter.LoadFilter(const Filter: String): Boolean;
var
  lLines, lRecord: TStringList;
  i: Integer;
  lTable, lKey, lHint, lDetailRecordHint: String;
  cursor: TCursor;
begin
  Result := True;
  cursor := Screen.Cursor;

  lLines := TStringList.Create;
  try
    AppSettings.ClearAllFilteredRecords;
    lLines.LoadFromFile(Filter);

    // Confirm loading large filter. Give opportunity to cancel instead of waiting.
    if lLines.Count > MAX_ITEMS_FOR_WARNING then
      Result := ConfirmYesNo(ResStr_ContinueLoadingLargeFilter) = mrYes;
    if not Result then Exit;

    HourglassCursor;
    frmMain.SetStatus(ResStr_LoadingFilter);
    AppSettings.ClearAllFilteredRecords;
    for i := 0 to lLines.Count - 1 do begin
      // The first line of the file can be a description. If it is, ignore it.
      if not SameText(LeftStr(lLines[i], 11), 'Description') and (Trim(lLines[i]) <> '') then begin
        lRecord := SplitLine(lLines[i]);
        if (lRecord.Count < 2) or (lRecord.Count > 3) then
          raise EExternalFilterException.CreateNonCritical(
              Format(ResStr_ExternalFilterInvalid, [lLines[i]]));
        lTable  := GetCorrectTableName(lRecord.Strings[0]);
        lKey    := lRecord.Strings[1];
        lHint   := '';
        if lRecord.Count > 2 then
          lHint := lRecord.Strings[2];
        lDetailRecordHint := '';
        try
          if FFilterTables.IndexOf(lTable) < 0 then
            LinkNonFilterTable(lTable, lKey, lDetailRecordHint);
          // note adding a record twice just appends the hints
          if lDetailRecordHint<>'' then
            // add the record with the note about the detail record first
            AppSettings.AddFilteredRecord(lTable, lKey, lDetailRecordHint);
          // add the record with it's main hint, indenting this if it's a hint
          // for a detail record described by the lDetailRecordHint
          AppSettings.AddFilteredRecord(lTable, lKey,
              IfThen((lDetailRecordHint='') or (lHint=''), '', ' -> ') + lHint);
        except
          on ELinkedRecordNotAvailable do ; // ignore a detail record where the master record is not in db
        end;
      end;
      frmMain.SetProgress(i * 100 div lLines.Count);
    end;
  finally
    lLines.Free;
    DefaultCursor(cursor);
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  Splits a tab-separated line of the file into a stringlist
}
function TFileFilter.SplitLine(const ALine: String): TStringList;
var
  lRecord: TStringList;
  lTempStr, lToken: string;
const
  TAB = #9;
begin
  lRecord := TStringList.Create;
  lTempStr := ALine;
  // remove each token from start of the line one by one
  while lTempStr<>'' do begin
    if Pos(TAB, lTempStr)>0 then
      lToken := LeftStr(lTempStr, Pos(TAB, lTempStr)-1)
    else
      lToken := lTempStr;
    lRecord.Add(lToken);
    // take the remainder of the unprocessed line (+2 to skip past the tab)
    lTempStr := Copy(lTempStr, Length(lToken)+2, Length(lTempStr));
  end;
  Result := lRecord;
end;

{===============================================================================
  TListFilter
}
{-------------------------------------------------------------------------------
  Prepare filter from keylist. Applies a generic hint to all filtered items.
}
function TListFilter.LoadFilter(const AKeyList: TKeyList; AValidationMessages: TStringList): Boolean;
var
  i: Integer;
  tableName, key, hint: String;
  cursor: TCursor;
begin
  Result := True;

  // Confirm loading large filter. Give opportunity to cancel instead of waiting.
  if AKeyList.Header.ItemCount > MAX_ITEMS_FOR_WARNING then
    Result := ConfirmYesNo(ResStr_ContinueLoadingLargeFilter) = mrYes;
  if not Result then Exit;

  cursor := HourglassCursor;
  frmMain.SetStatus(ResStr_LoadingFilter);
  try
    AppSettings.ClearAllFilteredRecords;
    tableName := AKeyList.Header.TableName;
    for i := 0 to AKeyList.Header.ItemCount - 1 do
    begin
      tableName := GetCorrectTableName(AKeyList.ItemTable[i]);

      key := AKeyList.Items[i].KeyField1;
      if Assigned(AValidationMessages) then hint := AValidationMessages[i]
                                       else hint := '';
      try
        if FFilterTables.IndexOf(tableName) < 0 then
          LinkNonFilterTable(tableName, key, hint);

        AppSettings.AddFilteredRecord(tableName, key, hint);
      except
        on ELinkedRecordNotAvailable do ; // ignore a detail record where the master record is not in db
      end;
      frmMain.SetProgress(i * 100 div AKeyList.Header.ItemCount);
    end;
  finally
    DefaultCursor(cursor);
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;

{===============================================================================
  TRecordsetFilter
}
{-------------------------------------------------------------------------------
  Prepare filter from recordset.
}
function TRecordsetFilter.LoadFilter(const ARecordset: _Recordset): Boolean;
var
  lTableName, lKey, lHint: String;
  cursor:TCursor;
begin
  Result := True;

  // Confirm loading large filter. Give opportunity to cancel instead of waiting.
  if ARecordset.RecordCount > MAX_ITEMS_FOR_WARNING then
    Result := ConfirmYesNo(ResStr_ContinueLoadingLargeFilter) = mrYes;
  if not Result then Exit;

  cursor := HourglassCursor;
  frmMain.SetStatus(ResStr_LoadingFilter);
  try
    AppSettings.ClearAllFilteredRecords;
    with ARecordset do
      while not Eof do begin
        lTableName := GetCorrectTableName(Fields['Table'].Value);
        lKey := Fields['Key'].Value;
        if Fields.Count > 2 then
          // The hint field can have any name.
          lHint := Fields[2].Value
        else
          lHint := '';
        try
          if FFilterTables.IndexOf(lTableName) < 0 then
            LinkNonFilterTable(lTableName, lKey, lHint);
          AppSettings.AddFilteredRecord(lTableName, lKey, lHint);
        except
          on ELinkedRecordNotAvailable do ; // ignore a detail record where the master record is not in db
        end;
        MoveNext;
        frmMain.SetProgress(Integer(AbsolutePosition) * 100 div RecordCount);
      end;
  finally
    DefaultCursor(cursor);
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;

end.
