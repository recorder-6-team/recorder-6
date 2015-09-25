{===============================================================================
  Unit:        FastColumnTypes

  Defines:     TfraFastColumnTypes

  Description: Set the column types for identified columns in data file.

  Model:       ImportWizard

  Last revision information:
    $Revision: 15 $
    $Date: 18/11/09 11:33 $
    $Author: Andrewkemp $

===============================================================================}

unit FastColumnTypes;

interface

uses
  Windows, Messages, SysUtils, DB, Grids, DBGrids, ImportWizardDBGrid, StdCtrls,
  Buttons, ExtCtrls, Controls, Classes, Dialogs, ColumnTypes, IWResourceStrings,
  IWLargeImportFile, IWColumnMappingClasses, IWSettings, ADOInt, Forms;

resourcestring
  ResStr_RevalidateTermLists =
      'Recorder Term Lists have changed. There are currently columns mapped to term'
      + ' lists and will be revalidated.';

  ResStr_ValidationErrors =
      'Errors have been identified and need to be resolved.';

type
  TfraFastColumnTypes = class(TfraColumnTypes)
    btnParseData: TBitBtn;
    procedure ColumnComboChange(Sender: TObject);
    procedure btnParseDataClick(Sender: TObject);
  private
    FValidationExpected: Boolean;
    function GetAdditionalColumnType(column: TColumn; combo: TComboBox): Boolean;
    procedure ImportFileFastParseError(Sender: TObject; const fieldName: String;
        errors: _Recordset);
    procedure GetMemoText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure SetMemoText(Sender: TField; const Text: string);
  protected
    procedure CheckMappingsAgainstCombos; override;
    procedure ClientDataSetBeforeDelete(DataSet: TDataSet); override;
    procedure ClientDataSetBeforePost(DataSet: TDataSet); override;
    function GetHasNext: Boolean; override;
    procedure LoadContent; override;
    procedure PrepareTempTables; override;
    procedure ReinsertFirstRow; override;
    procedure RemoveErrors(AColumn: TColumn); override;
    procedure RemoveFirstRow; override;
  public
    procedure RefreshTermLists; override;
    procedure ValidateContent; override;
  end;

//==============================================================================
implementation

uses
  ColumnType, IWConstants, GeneralFunctions, DatabaseAccessADO;

{$R *.dfm}

{-==============================================================================
    TfraFastColumnTypes
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.btnParseDataClick(Sender: TObject);
var
  showSelected, showErrors: Boolean;
  cur: TCursor;
  msg: TMessage;
begin
  inherited;
  cur := HourglassCursor;
  try
    if Settings.ImportedData.State = dsEdit then
      Settings.ImportedData.Post;

    showSelected := chkShowSelected.Checked;
    showErrors   := chkShowErrors.Checked;
    try
      chkShowErrors.Checked := False;
      dbgData.ShowSelected  := False;
      // Force the grid to immediately contain all rows
      NeedsRefilter := True;
      WMRefilter(msg);

      PrepareTempTables;
      FValidationExpected := False;
    finally
      dbgData.ShowSelected  := showSelected;
      chkShowErrors.Checked := showErrors;
      Refilter;
    end;

    // Any errors will stop the process, until they are resolved.
    if lblErrors.Caption <> '0' then
    begin
      ShowInformation(ResStr_ValidationErrors);
    end;
    UpdateErrorBullets;
  finally
    DefaultCursor(cur);
    ChangedContent;
  end;
end;  // TfraFastColumnTypes.btnParseDataClick

{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.ClientDataSetBeforeDelete(DataSet: TDataSet);
begin
  // If some validation already done, might need to remove data from temp tables.
  if TLargeImportFile(Settings.ImportFile).ValidationInfo.Count > 0 then
    TLargeImportFile(Settings.ImportFile).RemoveRow(
        Settings.ImportedData.FieldByName(FLD_ROWId).AsInteger);
  inherited;
end;  // TfraFastColumnTypes.ClientDataSetBeforeDelete

{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.ClientDataSetBeforePost(DataSet: TDataSet);
var
  i: Integer;
begin
  // No validation if first time. Otherwise, validate record being posted.
  if TLargeImportFile(Settings.ImportFile).ValidationInfo.Count > 0 then
  begin
    for i := 1 to Dataset.Fields.Count - 1 do
      TLargeImportFile(Settings.ImportFile).UpdateRowField(Dataset.Fields[i].FieldName);
    UpdateErrorBullets;
    Dirty := True;
  end;
end;  // TfraFastColumnTypes.ClientDataSetBeforePost

{-------------------------------------------------------------------------------
}
function TfraFastColumnTypes.GetHasNext: Boolean;
begin
  Result := CanProceed and not FValidationExpected;
end;  // TfraFastColumnTypes.GetHasNext

{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.LoadContent;
var
  i: Integer;
begin
  inherited;
  TLargeImportFile(Settings.ImportFile).OnFastParseError := ImportFileFastParseError;

  // Validation expected by default only if first time round.
  FValidationExpected := TLargeImportFile(Settings.ImportFile).ValidationInfo.Count = 0;

  // Setup events to handle TEXT values properly instead of
  // having "(MEMO)" showing up in the grid... 
  with Settings.ImportedData do
    for i := 0 to Fields.Count - 1 do
      if Fields[i].DataType = ftMemo then
      begin
        Fields[i].OnGetText := GetMemoText;
        Fields[i].OnSetText := SetMemoText;
      end;
end;  // TfraFastColumnTypes.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.PrepareTempTables;

  procedure ResetErrors(ValidationInfo: TStringList);
  var
    I, J: Integer;
    CVInfo: TColumnValidationInfo;
    Column: TColumn;
  begin
    for I := 0 to dbgData.Columns.Count - 1 do
    begin
      Column := dbgData.Columns[I];
      J := ValidationInfo.IndexOf(Column.Field.FieldName);
      if J > -1 then
      begin
        CVInfo := TColumnValidationInfo(ValidationInfo.Objects[J]);
        if CVInfo.RequiresParsing then
        begin
          RemoveErrors(Column);
        end;
      end;
    end;
  end;

begin
  with TLargeImportFile(Settings.ImportFile) do begin
    ClearMatchRules;
    PrepareData;
    ResetErrors(ValidationInfo);
    ParseData;
  end;
end; // TfraFastColumnTypes.PrepareTempTables

{-------------------------------------------------------------------------------
  Handles the selection of a column type that is not part of the most common ones.
}
function TfraFastColumnTypes.GetAdditionalColumnType(column: TColumn; combo: TComboBox): Boolean;
var
  dlg: TdlgColumnType;
  columnType: TColumnType;
begin
  Result := False;
  dlg := TdlgColumnType.Create(nil, Settings.ImportFile.ColumnMapping);
  try
    columnType := Settings.ImportFile.ColumnMapping.MappedType(column.Field.FieldName);
    dlg.ColumnType := columnType;

    if dlg.ShowModal = mrOk then
    begin
      if Assigned(dlg.ColumnType) then
        with combo do
        begin
          ItemIndex := Items.IndexOfObject(dlg.ColumnType);
          if ItemIndex = -1 then begin
            Items.InsertObject(Items.Count - 1, dlg.ColumnType.Name, dlg.ColumnType);
            ItemIndex := Items.Count - 2;
          end;
        end;
      Result := True;
      FValidationExpected := True;
    end else
    // Nothing changed, revert to previous selection.
    if Assigned(columnType) then
      combo.ItemIndex := combo.Items.IndexOf(columnType.Name)
    else
      combo.ItemIndex := combo.Items.IndexOf(ResStr_SkipColumn);
  finally
    dlg.Free;
  end;
  ChangedContent;
end;  // TfraFastColumnTypes.GetAdditionalColumnType

{-------------------------------------------------------------------------------
  Changing column type will force revalidation of entire column when user moves
  to next screen.
}
procedure TfraFastColumnTypes.ColumnComboChange(Sender: TObject);
var
  combo: TComboBox;
  column: TColumn;
  currentType: TColumnType;
  mappedColumn: String;
  mappedType: TColumnType;
  importFile: TLargeImportFile;
begin
  importFile  := TLargeImportFile(Settings.ImportFile);
  combo       := TComboBox(Sender);
  column      := dbgData.GetColumnForCombo(combo);
  currentType := importFile.ColumnMapping.MappedType(column.Field.FieldName);

  if combo.Text = ResStr_MoreOptions then
    if not GetAdditionalColumnType(column, combo) then Exit;

  // Map the selected column, or clear the errors.
  mappedType := TColumnType(combo.Items.Objects[combo.ItemIndex]);
  if Assigned(mappedType) then
  begin
    // If already mapped, need to update the display before the mapping
    // is reassigned, so that we don't get duplicates showing.
    mappedColumn := Settings.ImportFile.ColumnMapping.MappedColumn(mappedType);
    if (mappedColumn <> '') and (mappedColumn <> column.Field.FieldName) then
    begin
      combo := dbgData.GetComboForColumn(mappedColumn);
      combo.ItemIndex := combo.Items.IndexOf(ResStr_SkipColumn);
      RemoveErrors(dbgData.GetColumnForCombo(combo));
    end;
    // Now apply the mapping properly.
    importFile.ColumnMapping.MapColumn(column.Field.FieldName, mappedType);
    RemoveErrors(column);
  end else
    with Settings.ImportFile.ColumnMapping do begin
      if Assigned(currentType) then
      begin
        UnmapColumn(column.Field.FieldName);
        RemoveErrors(column);
      end;
    end;
  UpdateErrorBullets;
  Dirty := True;
  FValidationExpected := True;
  ChangedContent;
end;  // TfraFastColumnTypes.ColumnComboChange

{-------------------------------------------------------------------------------
  Checks whether there are columns mapped to term lists that will need to be
  revalidated.
}
procedure TfraFastColumnTypes.RefreshTermLists;
var
  i, idx: Integer;
  colName: String;
  colType: TColumnType;
  importFile: TLargeImportFile;
  needReparse: Boolean;
begin
  importFile := TLargeImportFile(Settings.ImportFile);
  importFile.ColumnMapping.RefreshTermLists;
  needReparse := False;

  if importFile.ValidationInfo.Count > 0 then
    // Find out if any validation needs to be redone.
    for i := 0 to dbgData.Columns.Count - 1 do
    begin
      colName := dbgData.Columns[i].Field.FieldName;
      colType := importFile.ColumnMapping.MappedType(colName);
      if Assigned(colType) and (colType.TermListTable <> '') then
      begin
        // The mapping didn't change, but still must flag the need for revalidation.
        idx := importFile.ValidationInfo.IndexOf(colName);
        if idx > -1 then
        begin
          TColumnValidationInfo(importFile.ValidationInfo[idx]).RequiresParsing := True;
          // Since the column needs to be revalidated, clear any errors that were there.
          RemoveErrors(dbgData.Columns[i]);
          UpdateErrorBullets;
        end;
        needReparse := True;
      end;
    end;
  // Only show message once, even if multiple columns are mapped to term lists.
  if needReparse then begin
    ShowInformation(ResStr_RevalidateTermLists);
    FValidationExpected := True;
  end;
  ChangedContent;
end;  // TfraFastColumnTypes.RefreshTermLists

{-------------------------------------------------------------------------------
  Reinsert columns names into the source dataset. Also reinsert first row of data
  into any temp tables if passed validation.
}
procedure TfraFastColumnTypes.ReinsertFirstRow;
var
  i: Integer;
  lFields: String;
  lValues: String;
  importFile: TLargeImportFile;
begin
  lFields := '';
  lValues := '';
  with Settings.ImportedData do
    for i := 0 to FieldCount - 1 do
    begin
      if i > 0 then
      begin
        lFields := lFields + ', ';
        lValues := lValues + ', ';
      end;
      lFields := lFields + #10#9'"' + Fields[i].FieldName + '"';
      if Fields[i].FieldName = FLD_ROWID then
        lValues := lValues + '1'
      else
        lValues := lValues + #10#9'''' + SafeQuotes(Settings.FirstRow.ValueFromIndex[i - 1]) + '''';
    end;

  dmDatabase.ExecuteSql(
      'SET IDENTITY_INSERT ' + Settings.ImportedData.TableName + ' ON');
  try
    // we can't do this with ImportedData.Insert/Post because that
    // does not play nicely with 'SET IDENTITY_INSERT' for some reason.
    dmDatabase.ExecuteSql(
        'INSERT INTO ' + Settings.ImportedData.TableName + ' ('
        + lFields
        + #10') VALUES ('
        + lValues
        + #10')');
  finally
    dmDatabase.ExecuteSql(
        'SET IDENTITY_INSERT ' + Settings.ImportedData.TableName + ' OFF');
  end;

  with Settings.ImportedData do
  begin
    Requery;
    importFile := TLargeImportFile(Settings.ImportFile);
    // Validate fields only if not first time round.
    if importFile.ValidationInfo.Count > 0 then
    begin
      Locate(FLD_ROWID, 1, []);
      for i := 0 to FieldCount - 1 do
        if Fields[i].CurValue <> Fields[i].NewValue then  // No unnecessary updates.
          if not dbgData.CellHasError[dbgData.Columns[i]] then
            importFile.UpdateRowField(Fields[i].FieldName);
    end;
  end;
  ChangedContent;
end;  // TfraFastColumnTypes.ReinsertFirstRow

{-------------------------------------------------------------------------------
  Remove first row of data from any temp tables already created.
}
procedure TfraFastColumnTypes.RemoveFirstRow;
begin
  if TLargeImportFile(Settings.ImportFile).ValidationInfo.Count > 0 then
    TLargeImportFile(Settings.ImportFile).RemoveRow(1);
  CheckMappingsAgainstCombos;
end;  // TfraFastcolumnTypes.RemoveFirstRow

{-------------------------------------------------------------------------------
  Ensure the displayed type at the top of each column is up to date.
}
procedure TfraFastColumnTypes.CheckMappingsAgainstCombos;
var
  i, idx: integer;
  combo: TComboBox;
  mappedType: TColumnType;
begin
  // Copy mappings back into combo boxes.
  for i := 0 to dbgData.Columns.Count - 1 do
  begin
    combo := dbgData.GetComboForColumn(i);

    if Assigned(combo) then
    begin
      mappedType := Settings.ImportFile.ColumnMapping.MappedType(dbgData.Columns[i].Field.FieldName);

      if Assigned(mappedType) then
      begin
        idx := combo.Items.IndexOfObject(mappedType);

        if combo.ItemIndex <> idx then
        begin
          combo.ItemIndex := idx;
          // if not found, must be an advanced type so add to list.
          if combo.ItemIndex = -1 then
          begin
            combo.Items.InsertObject(combo.Items.Count - 1, mappedType.Name, mappedType);
            combo.ItemIndex := combo.Items.Count - 2;
          end;
          FValidationExpected := True;
        end;
      end else
      if not SameText(combo.Text, ResStr_SkipColumn) then
      begin
        combo.ItemIndex := combo.Items.IndexOf(ResStr_SkipColumn);
        RemoveErrors(dbgData.Columns[i]);
        FValidationExpected := True;
      end;
    end;
  end;
  ChangedContent;
end;  // TfraFastColumnTypes.CheckMappingsAgainstCombos

{-------------------------------------------------------------------------------
  Flags all relevant cells with an error. Note that this is called when a whole
  column has been validated.
  This happens when its mapping changed and therefore there were no errors for it.
  Which also means, increase number of errors, NO DECREASE.
}
procedure TfraFastColumnTypes.ImportFileFastParseError(Sender: TObject; const fieldName: String;
    errors: _Recordset);
var
  i: Integer;
  column: TColumn;
begin
  // Get the relevant column first.
  column := nil;
  for i := 0 to dbgData.Columns.Count - 1 do
    if dbgData.Columns[i].Field.FieldName = fieldName then
    begin
      column := dbgData.Columns[i];
      Break;
    end;

  Assert(Assigned(column), Format(ResStr_UnknownFieldParseError, [fieldName]));

  // Got to go through the whole recordset to mark the cells.
  // Hopefully there aren't too many... unless the whole column is wrong for the chosen type...
  while not errors.Eof do
  begin
    if Settings.ImportedData.Locate(FLD_ROWID, errors.Fields[FLD_ROWID].Value, []) then
      dbgData.CellHasError[column] := True;
    errors.MoveNext;
  end;

  IncErrors(errors.RecordCount);
end;  // TfraFastColumnTypes.ImportFileFastParseError

{-------------------------------------------------------------------------------
  Removes errors for validated columns. Checks there are errors to remove before
  proceeding with going through the whole source data.
  Occurs when remapping a column, just before "losing" the mapping.
}
procedure TfraFastColumnTypes.RemoveErrors(AColumn: TColumn);
var
  idx: Integer;
  cvInfo: TColumnValidationInfo;
  importFile: TLargeImportFile;
begin
  importFile := TLargeImportFile(Settings.ImportFile);

  if importFile.ValidationInfo.Count > 0 then
  begin
    idx := importFile.ValidationInfo.IndexOf(AColumn.Field.FieldName);

    // Remove errors only if there are any to remove. No point wasting time.
    if idx > -1 then
    begin
      cvInfo := TColumnValidationInfo(importFile.ValidationInfo.Objects[idx]);

      if cvInfo.ErrorCount > 0 then
      begin
        inherited;
        // Don't forget to reset counter.
        cvInfo.ErrorCount := 0;
      end;

      // Reset indicator.
      cvInfo.RequiresParsing := True;
    end;
  end;
end;  // TfraFastColumnTypes.RemoveErrors

{-------------------------------------------------------------------------------
}
procedure TfraFastColumnTypes.ValidateContent;
begin
  // No call to inherited.
  if Settings.ImportedData.State = dsEdit then
  begin
    Settings.ImportedData.Post;
    Application.ProcessMessages;
    if not CanProceed then Abort;
  end;

  // Data parsed and valid, get the matching tables loaded and ready now.
  TLargeImportFile(Settings.ImportFile).InitialiseMatching;
end;  // TfraFastColumnTypes.ValidateContent

{-------------------------------------------------------------------------------
 Returns the actual text of the field instead of having "(MEMO)" displayed in the grid.
}
procedure TfraFastColumnTypes.GetMemoText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  Text := Sender.AsString;
end;

{-------------------------------------------------------------------------------
  Saves any changes abck to the table.
}
procedure TfraFastColumnTypes.SetMemoText(Sender: TField; const Text: string);
begin
  Sender.AsString := Text;
end;

end.

