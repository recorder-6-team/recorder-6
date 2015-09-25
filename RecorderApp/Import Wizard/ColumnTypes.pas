{===============================================================================
  Unit:        ColumnTypes

  Defines:     TfraColumnTypes

  Description: Set the column types for identified columns in data file.

  Model:       ImportWizard

  Last revision information:
    $Revision: 62 $
    $Date: 06/03/13 09:20 $
    $Author: Michaelcaptain $

===============================================================================}

unit ColumnTypes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWSettings, IWBasePage, ExtCtrls, StdCtrls, ComCtrls, Contnrs,
  Grids, ImageListButton, Buttons, DssStringGrid, HTMLView, DB, DBClient,
  SMIBase, SMI2TXT, DBGrids, StrUtils, Math, IWParsers, IWColumnMappingClasses,
  ExceptionForm, ImportWizardDBGrid, ADODB, Constants, ADOInt, ApplicationSettings;

resourcestring
  ResStr_SkipColumn = '<skip this column>';
  ResStr_Type = 'Type';
  ResStr_ConfirmDeleteSelection = 'Are you sure you want to delete the selected rows?';
  ResStr_FirstRowMissing = 'First row not found in dataset';
  ResStr_AllErrorsCleared = 'The errors highlighted in the grid must be corrected.';
  ResStr_InvalidMethodCall = 'Invalid call to method %s.';
  ResStr_UnknownFieldParseError = 'Parse error occurred for unknown field %s';
  ResStr_UnlinkingColumn = 'Unlinking column...';
  ResStr_ErrorResolvedPleaseRevalidate = 'This error has been resolved. Please'
      + ' validate the data again.';

  ResStr_TooWideColumnSet =
      'The settings you have chosen would create '+
      'a column that is too wide (greater than 8060 characters).  Please ensure that '+
      'you have selected the correct settings for this import file.';

  ResStr_ImportFileEmpty = 'No data could be imported, either because the import file is empty or it was in use by another application.';

type
  EColumnTypes = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Frame displayed by the import wizard when the user is associating columns in the
    imported data with column types available for import in the Import Wizard.  This frame
    also validates that the format of items in a column can be parsed correctly against the
    specification of the associated column type, and allows the user to rectify errors
    directly in the grid.  In addition, the user is able to remove rows from the import
    file that they do not want to import.
    The following validation rules are applied when determining whether the Next button is
    enabled:
    At least a Species Name column type must be selected.  All other mandatory fields are
    prompted for if required on TfraMissingData.
    All the errors encountered when parsing the import data have been cleared.
    If a Specimen Type or Specimen Comment column is specified, then a Specimen Number
    column is also required.  For each row with a value in the Specimen Type or Specimen
    Comment column, there must also be a value in Specimen Number.
    When the Next button on the wizard is disabled, then text is appended to the HTML
    instructions panel: 'You cannot proceed until:' followed by a bullet list of the points
    that prevent the button being enabled.
    Validation rules specified in the section entitled Import Wizard Field Parsing details
    pass for each and every column.
  }
  TfraColumnTypes = class(TBasePage)
    btnRemove: TBitBtn;
    chkFirstRow: TCheckBox;
    chkShowErrors: TCheckBox;
    chkShowSelected: TCheckBox;
    DataSource: TDataSource;
    Label1: TLabel;
    Label4: TLabel;
    lblErrors: TLabel;
    pnlControls: TPanel;
    Shape1: TShape;
    dbgData: TImportWizardDBGrid;
    procedure btnRemoveClick(Sender: TObject);
    procedure chkFirstRowClick(Sender: TObject);
    procedure chkShowSelectedClick(Sender: TObject);
    procedure chkShowErrorsClick(Sender: TObject);
    procedure dbgDataPopulateCombo(Sender: TObject;
      ACombo: TComboBox);
    procedure ColumnComboChange(Sender:TObject);
    procedure ColumnComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
        TOwnerDrawState);
    procedure GridSelectedRowsChanged(Sender: TObject);
    procedure dbgDataCellClick(Column: TColumn);
    procedure dbgDataGetSelectedCellHighlight(Column: TColumn);
    procedure dbgDataKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FFieldSizes: array of integer;
    FReloadRequired: Boolean;
    FCanProceed: boolean;
    FOldDateFormat: string;
    FOldDateSep: char;
    FCheckingMappings: boolean;
    FNeedsRefilter: boolean;
    FDirty: boolean;
    FSelectedMatchColumns: TStringList;
    FHighlightMatch: Boolean;
    procedure DecErrors(count: Integer = 1);
    procedure GetCellError(Sender: TObject; ACol: integer; var AError: string);
    procedure ImportFileParseError(Sender: TObject; const FieldName: string; Error: boolean);
    function PopulateGrid: Boolean;
    procedure SelectedMatchValueCheck(Sender: TObject; Position: Integer; const Field: String;
      const Value: Variant);
    procedure SetDirty(const Value: Boolean);
    procedure SetGridColumnTitles;
    procedure ParseColumn(const AColumn: string);
  protected
    procedure CheckMappingsAgainstCombos; virtual;
    procedure ClientDataSetBeforeDelete(DataSet: TDataSet); virtual;
    procedure ClientDataSetBeforePost(DataSet: TDataSet); virtual;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetHTMLImageName: String; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    procedure IncErrors(count: Integer = 1);
    procedure LoadContent; override;
    procedure PrepareTempTables; virtual;
    procedure Refilter;
    procedure ReinsertFirstRow; virtual;
    procedure RemoveFirstRow; virtual;
    procedure RemoveErrors(AColumn: TColumn); virtual;
    procedure UpdateErrorBullets; virtual;
    procedure WMRefilter(var AMessage: TMessage); message WM_REFILTER;
    property CanProceed: Boolean read FCanProceed write FCanProceed;
    property Dirty: boolean read FDirty write SetDirty;
    property NeedsRefilter: Boolean read FNeedsRefilter write FNeedsRefilter;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    destructor Destroy; override;
    procedure SaveContent; override;
    procedure ValidateContent; override;
    procedure RefreshTermLists; override;
    procedure RegisterDragDropComponents; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FileSelect, FixedWidths, MissingData, MatchGeneric, ColumnType, GeneralFunctions,
  IWConstants, IWResourceStrings, DatabaseAccessADO, Maintbar, OnlineHelp;

{-==============================================================================
    TfraColumnTypes
===============================================================================}

{-------------------------------------------------------------------------------
  Constructor
}
constructor TfraColumnTypes.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited;
  HelpContext := IDH_IWCOLUMNTYPES;
end;

{-------------------------------------------------------------------------------
}
destructor TfraColumnTypes.Destroy;
begin
  if FOldDateFormat <> '' then ShortDateFormat := FOldDateFormat;
  if FOldDateSep <> '' then DateSeparator := FOldDateSep;
  if Assigned(Settings.ImportedData) then
  begin
    Settings.ImportedData.BeforePost := nil;
    Settings.ImportedData.BeforeDelete := nil;
  end;
  inherited;
end;  // TfraColumnTypes.Destroy

{-------------------------------------------------------------------------------
  CCN JNCC Code 6.10.3- 006. Must set FDirty := True after Delete
}
procedure TfraColumnTypes.btnRemoveClick(Sender: TObject);
var
  lOldID: Integer;
begin
  if ConfirmYesNoDefaultNo(ResStr_ConfirmDeleteSelection) = mrYes then begin
    // Dataset changes, can't use Return button now.
    if Settings.BackFromMatchPage then
      SendMessage(ContainerForm.Handle, UM_RETURN, 1, 0)
    else
      SendMessage(ContainerForm.Handle, UM_RETURN, 0, 0);

    with Settings.ImportedData do begin
      DisableControls;
      try
        First;
        while not Eof do
          if not dbgData.CurrentRowSelected then
            Next
          else
          begin
            lOldID := FieldByName(FLD_ROWID).Value;
            Delete;
            if not Eof then
              // this works around an apparent bug in TClientDataset
              // where it does not move to the next record after Delete
              // if Filtered is True
              if FieldByName(FLD_ROWID).Value = lOldID then Next;
          end;
        First;
      finally
        EnableControls;
        Refilter;
      end; // try
    end; // with
    // this updates the selected row count
    dbgData.CurrentRowSelected := False;
    UpdateErrorBullets;
    btnRemove.Enabled := False;
    Dirty := True;
  end;
end;  // TfraColumnTypes.btnRemoveClick

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.chkFirstRowClick(Sender: TObject);
var
  lShowErrors, lShowSelected: boolean;
  lMessage: TMessage;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // store original state of show errors/selected rows checkboxes
    lShowErrors           := chkShowErrors.Checked;
    lShowSelected         := dbgData.ShowSelected;
    chkShowErrors.Checked := false;
    dbgData.ShowSelected  := false;
    // Force the grid to immediately contain all rows
    FNeedsRefilter := True;
    WMRefilter(lMessage);
    try
      inherited;
      if not chkFirstRow.Checked then begin
        Settings.FieldNameOnFirstRow := chkFirstRow.Checked;
        ReinsertFirstRow;
      end else
      begin
        SetGridColumnTitles;
        Settings.FieldNameOnFirstRow := chkFirstRow.Checked;
        Settings.ImportFile.ColumnMapping.MapAutomatically;
        RemoveFirstRow;
      end;
      UpdateErrorBullets;
      Dirty := True;
    finally
      dbgData.ShowSelected  := lShowSelected;
      chkShowErrors.Checked := lShowErrors;
      Refilter;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfraColumnTypes.chkFirstRowClick

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.SetGridColumnTitles;
var
  i: integer;
begin
  with Settings do
    for i := 0 to ImportFile.ColumnMapping.ColumnCount - 1 do
      if FirstRow.IndexOfName(dbgData.Columns[i].Field.FieldName) > -1 then
        dbgData.Columns[i].Title.Caption := Settings.FirstRow.ValueFromIndex[i - 1];
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.CheckMappingsAgainstCombos;
var
  i: integer;
  lCombo: TComboBox;
  lMappedType: TColumnType;
  lIdx: integer;
  lColumnsToParse: TStringList;
begin
  // Check a flag, so this proc can't cause a recursion
  if not FCheckingMappings then begin
    FCheckingMappings := True;
    lColumnsToParse := TStringList.Create;
    try
      // Copy mappings back into combo boxes
      for i := 0 to dbgData.Columns.Count-1 do begin
        lCombo := dbgData.GetComboForColumn(i);
        if Assigned(lCombo) then begin
          lMappedType := Settings.ImportFile.ColumnMapping.MappedType(
              dbgData.Columns[i].Field.FieldName);
          if Assigned(lMappedType) then begin
            lIdx := lCombo.Items.IndexOfObject(lMappedType);
            if lCombo.ItemIndex<>lIdx then begin
              lCombo.ItemIndex := lIdx;
              // if not found, must be an advanced type so add to list
              if lCombo.ItemIndex=-1 then begin
                lCombo.Items.InsertObject(lCombo.Items.Count-1, lMappedType.Name, lMappedType);
                lCombo.ItemIndex := lCombo.Items.Count-2;
              end;
              lColumnsToParse.Add(dbgData.GetColumnForCombo(lCombo).Field.FieldName);
            end;
          end
          else if (lCombo.Text<>ResStr_SkipColumn) then begin
            lCombo.ItemIndex := lCombo.Items.IndexOf(ResStr_SkipColumn);
            RemoveErrors(dbgData.Columns[i]);
          end;
        end; // if
      end; // for
      // Now initiate parsing of all the columns that require it
      Settings.ImportFile.ParseColumnList(lColumnsToParse);
    finally
      FCheckingMappings := False;
      lColumnsToParse.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ClientDataSetBeforeDelete(DataSet: TDataSet);
var
  i: integer;
begin
  // About to delete a record, so reduce the error count
  for i := 1 to dbgData.Columns.Count-1 do
    if dbgData.CellHasError[dbgData.Columns[i]] then
    begin
      dbgData.CellHasError[dbgData.Columns[i]] := False;
      DecErrors;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ClientDataSetBeforePost(DataSet: TDataSet);
var
  i: Integer;
begin
  // reparse any changed fields for errors
  if Assigned(Settings.ImportFile) then begin
    with Dataset do begin
      for i := 1 to Fields.Count-1 do
        Settings.ImportFile.ParseField(Fields[i].FieldName);
    end;
    UpdateErrorBullets;
  end;
  Dirty := True;
end;  // TfraColumnTypes.ClientDataSetBeforePost

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ColumnComboDrawItem(Control: TWinControl; Index: Integer; Rect:
    TRect; State: TOwnerDrawState);
begin
  inherited;
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items[Index] = ResStr_MoreOptions then begin
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Style := [fsItalic];
    end;
    Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, Items[Index]);
  end;
end;  // TfraColumnTypes.ColumnComboDrawItem

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.dbgDataCellClick(Column: TColumn);
begin
  inherited;
  // Enforce readonly on rowid column.
  if Column.FieldName = FLD_ROWID then
    dbgData.Options := dbgData.Options - [dgEditing, dgAlwaysShowEditor]
  else
    dbgData.Options := dbgData.Options + [dgEditing, dgAlwaysShowEditor];
end;  // TfraColumnTypes.dbgDataCellClick

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.dbgDataGetSelectedCellHighlight(Column: TColumn);
var
  i: Integer;
  lColType: TColumnType;
  lGrid: TImportWizardDBGrid;
begin
  inherited;
  if Settings.BackFromMatchPage and Assigned(Column.Field) then begin
    if Column.Field.FieldName = FLD_ROWID then Exit;

    lGrid := TImportWizardDBGrid(Column.Grid);
    i := FSelectedMatchColumns.IndexOf(Column.Field.FieldName);
    if i = -1 then
      lGrid.Canvas.Brush.Color := MergeColours(clWindow, clHighlight, 90)
    else begin
      lColType := TColumnType(FSelectedMatchColumns.Objects[i]);
      FHighlightMatch := False;
      lColType.Parser.ParseField(Column.Field.AsString, SelectedMatchValueCheck);
      if not FHighlightMatch then
        lGrid.Canvas.Brush.Color := MergeColours(clWindow, clHighlight, 90);
    end;
  end;
end;  // TfraColumnTypes.dbgDataGetSelectedCellHighlight

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.DecErrors(count: Integer = 1);
begin
  with lblErrors do begin
    Caption := IntToStr(StrToInt(Caption) - count);
    if Caption = '0' then begin
      Font.Color := clWindowText;
      Font.Style := [];
      chkShowErrors.Checked := False;
    end;
  end;
end;  // TfraColumnTypes.DecErrors

{-------------------------------------------------------------------------------
}
function TfraColumnTypes.GetHasNext: Boolean;
begin
  Result := FCanProceed;
end;  // TfraColumnTypes.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraColumnTypes.GetHasPrevious: Boolean;
begin
  Result := not (Settings.ImportType in [itDBase, itParadox, itADO]);
end;  // TfraColumnTypes.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraColumnTypes.GetNext: TBasePageClass;
var
  lGotLocation : Boolean;
  lGotDate : Boolean;
  lGotObserver : Boolean;
  lGotDeterminer : Boolean;
  lTermFieldsRequired: Boolean;
begin
  with Settings.ImportFile.ColumnMapping do
  begin
    lGotLocation   := (IsMapped(ColumnTypeByKey(CT_KEY_GRID_REFERENCE))
        or IsMapped(ColumnTypeByKey(CT_KEY_LOCATION_NAME)));
    lGotDate       := IsMapped(ColumnTypeByKey(CT_KEY_DATE));
    lGotObserver   := IsMapped(ColumnTypeByKey(CT_KEY_OBSERVER));
    lGotDeterminer := not lGotObserver
        or (IsMapped(ColumnTypeByKey(CT_KEY_DETERMINER)));
    RefreshRequiredTerms;
    lTermFieldsRequired := TermFieldsRequiredCount > 0;
  end;
  if (lGotLocation and lGotDate and lGotObserver and lGotDeterminer and
      (not lTermFieldsRequired) and
      not Settings.UseOldImportWizard) then
    Result := TfraMatchGeneric
  else
    Result := TfraMissingData;
end;  // TfraColumnTypes.GetNext 

{-------------------------------------------------------------------------------
}
function TfraColumnTypes.GetPrevious: TBasePageClass;
begin
  if Settings.ImportType = itFixedWidth then
    Result := TfraFixedWidths
  else
    Result := TfraFileSelect;
end;  // TfraColumnTypes.GetPrevious

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ImportFileParseError(Sender: TObject;
    const FieldName: string; Error: boolean);
var
  lColumn: TColumn;
  i: integer;
  lCellHadError: boolean;
begin
  lColumn := nil;
  for i := 0 to dbgData.Columns.Count - 1 do
    if dbgData.Columns[i].Field.FieldName = FieldName then begin
      lColumn := dbgData.Columns[i];
      Break;
    end;
  Assert(Assigned(lColumn), Format(ResStr_UnknownFieldParseError, [FieldName]));
  lCellHadError := dbgData.CellHasError[lColumn];
  dbgData.CellHasError[lColumn] := Error;
  if Error and (not lCellHadError) then
    IncErrors
  else
  if (not Error) and lCellHadError then
  begin
    DecErrors;
    if chkShowErrors.Checked then
      Refilter;
  end;
end;  // TfraColumnTypes.ImportFileParseError

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.Refilter;
begin
  FNeedsRefilter := True;
  PostMessage(Self.Handle, WM_REFILTER, 0, 0);
end;  // TfraColumnTypes.Refilter

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.WMRefilter(var AMessage: TMessage);
begin
  // Filtering handled using messages, as we need to ensure all updates are
  // completed before refiltering, and we don't want unnecessary repeats
  if FNeedsRefilter then begin
    dbgData.ShowErrors := chkShowErrors.Checked;
    Settings.ImportedData.DisableControls;
    try
      Settings.ImportedData.Filtered := False;
      Settings.RecordCount := Settings.ImportedData.RecordCount;
      if chkShowErrors.Checked or chkShowSelected.Checked then
        Settings.ImportedData.Filtered := True;
    finally
      Settings.ImportedData.EnableControls;
    end; // try
    FNeedsRefilter := False;
  end;
end;  // TfraColumnTypes.WMRefilter

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.IncErrors(count: Integer = 1);
begin 
  with lblErrors do begin
    Caption    := IntToStr(StrToInt(Caption) + count);
    Font.Color := clRed;
    Font.Style := [fsBold];
  end;
  chkShowErrors.Enabled := True;
end;  // TfraColumnTypes.IncErrors

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.SelectedMatchValueCheck(Sender: TObject; Position: Integer;
  const Field: string; const Value: Variant);
begin
  if CompareText(VarToStr(Value), Settings.SelectedMatchValue) = 0 then
    FHighlightMatch := True;
end;  // TfraColumnTypes.SelectedMatchValueCheck

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.LoadContent;

  procedure SelectMatchValues;
  var
    i, j: Integer;
    lColType: TColumnType;
    lField: TField;
  begin
    with Settings do begin
      ImportedData.First;
      while not ImportedData.Eof do begin
        // Find out if match rule is associated with any column, should be at least one.
        for i := 0 to ImportedData.FieldCount - 1 do begin
          lField := ImportedData.Fields[i];
          lColType := ImportFile.ColumnMapping.MappedType(lField.FieldName);
          if Assigned(lColType) then
            if Assigned(lColType.Parser) then
              for j := 0 to lColType.Parser.FieldCount - 1 do
                if lColType.FieldMatchRuleKeys[j] = ImportFile.MatchRules[MatchRuleIndex].Key then
                begin
                  FSelectedMatchColumns.AddObject(lField.FieldName, lColType);
                  // Now see if the row has to be selected.
                  FHighlightMatch := False;
                  lColType.Parser.ParseField(lField.AsString, SelectedMatchValueCheck);
                  if FHighlightMatch then
                    dbgData.CurrentRowSelected := True;
                end;
        end;
        ImportedData.Next;
      end;
      ImportedData.First;
    end;
  end;

begin
  inherited;

  // User defined short date format applies whilst on this page
  FOldDateFormat := ShortDateFormat;
  FOldDateSep    := DateSeparator;
  if Settings.DateFormat <> '' then ShortDateFormat := Settings.DateFormat;
  if Settings.DateDelimiter <> '' then DateSeparator := Settings.DateDelimiter;
  if not Settings.DataFileLoaded then
    Settings.CreateImportFile;

  if PopulateGrid then begin
    Settings.ImportFile.OnParseErrorChanged := ImportFileParseError;
    dbgData.ShowCombos   := True;
    Settings.RecordCount := Settings.ImportedData.RecordCount;

    if Settings.ImportType in [itText, itCSV, itFixedWidth, itExcel, itLotus, itQuattro] then
      chkFirstRow.Enabled := True
    else begin
      chkFirstRow.Enabled := False;
      Settings.ImportFile.ColumnMapping.MapAutomatically;
    end;
    // load column mappings from the template if there is one
    Settings.LoadColumnsFromFile;
    // Check the first row box without automatically mapping everything
    chkFirstRow.OnClick := nil;
    chkFirstRow.Checked := Settings.FieldNameOnFirstRow;
    chkFirstRow.OnClick := chkFirstRowClick;
    if chkFirstRow.Checked then
      SetGridColumnTitles;
    FCheckingMappings := False;
    FNeedsRefilter    := False;
    Dirty             := False;
    CheckMappingsAgainstCombos;

    if Settings.BackFromMatchPage then begin
      FSelectedMatchColumns := TStringList.Create;
      // Ask for Return button to show up.
      SendMessage(TWinControl(Owner).Handle, UM_RETURN, 1, 1);
      // Find out which rows to select straightaway.
      SelectMatchValues;
      dbgData.ShowSelected    := True;
      chkShowSelected.Checked := True;
      chkShowSelected.Enabled := True;
      btnRemove.Enabled       := dbgData.SelectedCount > 0;
    end;
  end;
end;  // TfraColumnTypes.LoadContent

{-------------------------------------------------------------------------------
  Populate the grid and configure the various columns.
}
function TfraColumnTypes.PopulateGrid: Boolean;
var
  i: Integer;
  errors: ADOInt.Errors;
begin
  Result := False;
  SetLength(FFieldSizes, 0);
  try
    Settings.ImportedData.Requery;
    if Settings.ImportedData.RecordCount = 0 then begin
      MessageDlg(ResStr_ImportFileEmpty, mtInformation, [mbOk], 0);
      // Send message to cancel to wizard.
      PostMessage(TWinControl(Owner).Handle, UM_CANCEL_IMPORT, 0, 0);
    end else begin
      Settings.ImportedData.BeforePost   := ClientDatasetBeforePost;
      Settings.ImportedData.BeforeDelete := ClientDatasetBeforeDelete;
      FReloadRequired := False;
      Datasource.Dataset := Settings.ImportedData;
      // Row ID field leftmost
      Settings.ImportedData.FieldByName(FLD_ROWID).Index := 0;
      dbgData.KeyField := Settings.ImportedData.FieldByName(FLD_ROWID);
      for i := 0 to Settings.ImportedData.Fields.Count - 1 do
        Settings.ImportedData.Fields[i].DisplayWidth := 30;
      Settings.ImportedData.FieldByName(FLD_ROWID).DisplayWidth := 2;
      Settings.ImportedData.First;
      dbgData.Columns[0].Title.Caption := ResStr_Type + ':';
      dbgData.Columns[0].ReadOnly      := True;
      dbgData.OnGetCellError           := GetCellError;
      Result := True;
    end;
  except
    on EDatabaseError do begin
      errors := Settings.ImportedData.Connection.Errors;
      if errors.Count > 0 then
        if errors.Item[0].Number = -2147217900 then
          raise EColumnTypes.CreateNonCritical(ResStr_TooWideColumnSet);
      // throw the original error if not the above.
      raise;
    end;
  end;
end;  // TfraColumnTypes.PopulateGrid

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.SaveContent;
begin
  inherited;

  FSelectedMatchColumns.Free;
  Settings.FieldNameOnFirstRow := chkFirstRow.Checked;
  ShortDateFormat := FOldDateFormat;
  DateSeparator := FOldDateSep;
end;  // TfraColumnTypes.SaveContent

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.PrepareTempTables;
begin
  // Prepare the temp tables required by the rest of the import process,
  // unless nothing has changed
  if Dirty or (Settings.ImportFile.MatchRuleCount=0) then begin
    dbgData.ShowSelected := False;
    Settings.ImportFile.DropWorkingTables;
    Settings.ImportFile.ClearMatchRules;
    Settings.ImportFile.PrepareData;
    Settings.ImportFile.InitialiseMatching;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.UpdateErrorBullets;
var
  lBullets: TStringList;
begin
  lBullets := TStringList.Create;
  try
    if lblErrors.Caption<>'0' then
      lBullets.Add(ResStr_AllErrorsCleared);
    Settings.ImportFile.ColumnMapping.RefreshErrors;
    lBullets.AddStrings(Settings.ImportFile.ColumnMapping.Errors);
    ChangedHTML(lBullets);
    FCanProceed := lBullets.Count=0;
    ChangedContent;
  finally
    lBullets.Free;
  end; // try
end;  // TfraColumnTypes.UpdateErrorBullets

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.chkShowSelectedClick(Sender: TObject);
begin
  dbgData.ShowSelected := chkShowSelected.Checked;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.chkShowErrorsClick(Sender: TObject);
begin
  Refilter;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.dbgDataPopulateCombo(Sender: TObject; ACombo: TComboBox);
var
  i: integer;
begin
  ACombo.Items.Add(ResStr_SkipColumn);
  with Settings.ImportFile.ColumnMapping do
    for i := 0 to TypeCount-1 do begin
      if Types[i].CommonlyUsed then
        ACombo.Items.AddObject(Types[i].Name, Types[i]);
    end;
  ACombo.Items.Add(ResStr_MoreOptions);
  ACombo.ItemIndex := 0;
end;

{-------------------------------------------------------------------------------
  Changes column mapping based on selected option in list of column types.
}
procedure TfraColumnTypes.ColumnComboChange(Sender: TObject);
var
  lColumn: TColumn;
  lDlg: TdlgColumnType;
  lCombo: TComboBox;
  lOldType: TColumnType;
begin
  Assert(Sender is TComboBox, Format(ResStr_InvalidMethodCall,
       ['TfraColumnTypes.ColumnComboChange']));
  lCombo := TComboBox(Sender);
  lColumn := dbgData.GetColumnForCombo(lCombo);
  if lCombo.Text=ResStr_MoreOptions then begin
    // More options
    lDlg := TdlgColumnType.Create(nil, Settings.ImportFile.ColumnMapping);
    try
      with Settings.ImportFile.ColumnMapping do
        if Assigned(MappedType(lColumn.Field.FieldName)) then
          lDlg.ColumnType := MappedType(lColumn.Field.FieldName);
      if lDlg.ShowModal=mrOk then begin
        if Assigned(lDlg.ColumnType) then
          with lCombo do begin
            ItemIndex := Items.IndexOfObject(lDlg.ColumnType);
            if ItemIndex = -1 then
            begin
              Items.InsertObject(Items.Count - 1, lDlg.ColumnType.Name, lDlg.ColumnType);
              ItemIndex := Items.Count - 2;
            end;
            // Parse errors
          end;
      end
      else
        CheckMappingsAgainstCombos;
    finally
      lDlg.Free;
    end
  end;

  with Settings.ImportFile.ColumnMapping do
    lOldType := MappedType(lColumn.Field.FieldName);

  // Map the selected column, or clear the errors
  if Assigned(lCombo.Items.Objects[lCombo.ItemIndex]) then begin
    with Settings.ImportFile.ColumnMapping do
      MapColumn(lColumn.Field.FieldName, TColumnType(lCombo.Items.Objects[lCombo.ItemIndex]));
    if Assigned(lOldType) then
      Settings.ImportFile.ParseRelatedColumns(lOldType);
    ParseColumn(lColumn.Field.FieldName);
    // The following ensures that no duplicate columns are selected
    CheckMappingsAgainstCombos;
  end else
  with Settings.ImportFile.ColumnMapping do begin
    if Assigned(lOldType) then begin
      UnmapColumn(lColumn.Field.FieldName);
      if KeyIsMapped(CT_KEY_LOCATION) then
        ParseColumn(MappedColumn(ColumnTypeByKey(CT_KEY_LOCATION)));
      if KeyIsMapped(CT_KEY_GRID_REFERENCE) then
        ParseColumn(MappedColumn(ColumnTypeByKey(CT_KEY_GRID_REFERENCE)));
      if KeyIsMapped(CT_KEY_SPATIAL_SYSTEM) then
        ParseColumn(MappedColumn(ColumnTypeByKey(CT_KEY_SPATIAL_SYSTEM)));
      if KeyIsMapped(CT_KEY_LOCATION_NAME) then
        ParseColumn(MappedColumn(ColumnTypeByKey(CT_KEY_LOCATION_NAME)));
      RemoveErrors(lColumn);
    end;
  end;
  UpdateErrorBullets;
  Dirty := True;
end;

{-------------------------------------------------------------------------------
  Method to reparse a column.  This ensures that the entire column content
     is parsed by unchecking the 2 'Show ...' checkboxes, then checking them
     again afterwards.
}
procedure TfraColumnTypes.ParseColumn(const AColumn: string);
var
  lShowErrors, lShowSelected: boolean;
  lMessage: TMessage;
begin
  // store original state of show errors/selected rows checkboxes
  lShowErrors := chkShowErrors.Checked;
  lShowSelected := dbgData.ShowSelected;
  chkShowErrors.Checked := false;
  dbgData.ShowSelected := false;
  // Force the grid to immediately contain all rows
  FNeedsRefilter := True;
  WMRefilter(lMessage);
  try
    Settings.ImportFile.ParseColumn(AColumn);
  finally
    dbgData.ShowSelected := lShowSelected;
    chkShowErrors.Checked := lShowErrors;
    Refilter;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.RefreshTermLists;
var
  i: Integer;
  colName: String;
begin
  Settings.ImportFile.ColumnMapping.RefreshTermLists;
  for i := 0 to dbgData.Columns.Count - 1 do begin
    colName := dbgData.Columns[i].Field.FieldName;
    if Assigned(Settings.ImportFile.ColumnMapping.MappedType(colName)) then
      Settings.ImportFile.ParseColumn(colName);
  end;
end;  // TfraColumnTypes.RefreshTermLists

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.RemoveFirstRow;
begin
  CheckMappingsAgainstCombos;
end;  // TfraColumnTypes.RemoveFirstRow

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ReinsertFirstRow;
var
  I: Integer;
  lFields: String;
  lValues: String;
begin
  lFields := '';
  lValues := '';
  with Settings.ImportedData do
    for I := 0 to FieldCount - 1 do
    begin
      if I > 0 then
      begin
        lFields := lFields + ', ';
        lValues := lValues + ', ';
      end;
      lFields := lFields + '"' + Fields[I].FieldName + '"';
      if Fields[I].FieldName = FLD_ROWID then
        lValues := lValues + '1'
      else
        lValues := lValues + '''' +
            DuplicateCharacters(Settings.FirstRow.ValueFromIndex[i-1], '''') + '''';
    end;

  dmDatabase.ExecuteSql(
      'SET IDENTITY_INSERT ' + Settings.ImportedData.TableName + ' ON');
  try
    // we can't do this with ImportedData.Insert/Post because that
    // does not play nicely with 'SET IDENTITY_INSERT' for some reason
    dmDatabase.ExecuteSql(
        'INSERT INTO ' + Settings.ImportedData.TableName + ' ('
        + lFields + ')'
        + ' VALUES (' + lValues + ')');
  finally
    dmDatabase.ExecuteSql(
        'SET IDENTITY_INSERT ' + Settings.ImportedData.TableName + ' OFF');
  end;

  with Settings.ImportedData do
  begin
    Requery;
    Locate(FLD_ROWID, 1, []);
    for I := 0 to FieldCount - 1 do
      Settings.ImportFile.ParseField(Fields[i].FieldName);
  end;
end;  // TfraColumnTypes.ReinsertFirstRow

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.RemoveErrors(AColumn: TColumn);
var
  lBookmark: TBookmark;
  lCellHadError: boolean;
  lCursor: TCursor;
begin
  with Settings.ImportedData do begin
    lBookmark := GetBookmark;
    DisableControls;
    lCursor := HourglassCursor;
    frmMain.SetStatus(ResStr_UnlinkingColumn);
    try
      First;
      while not EOF do begin
        lCellHadError := dbgData.CellHasError[AColumn];
        dbgData.CellHasError[AColumn] := False;
        if lCellHadError then
          DecErrors;
        Next;
        frmMain.SetProgress(RecNo * 100 div RecordCount);
      end;
      if chkShowErrors.Checked then
        Refilter;
    finally
      try
        Gotobookmark(lBookmark);
      except
        on EDatabaseError do
          // ignore, record may no longer exist if error cleared
      end;
      EnableControls;
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
      DefaultCursor(lCursor);
    end; // try
  end;
end;

{-------------------------------------------------------------------------------
  Hint handler for the grid
}
procedure TfraColumnTypes.GetCellError(Sender: TObject; ACol: integer;
  var AError: string);
var
  I: Integer;
  lFieldName: string;
  lColumn: TColumn;
begin
  lFieldName := dbgData.Fields[ACol].FieldName;

  if Settings.ImportFile.ColumnMapping.IsMapped(lFieldName) then
    AError := Settings.ImportFile.GetParseError(lFieldName)
  else
    AError := '';

  if AError = '' then
    for I := 0 to dbgData.Columns.Count - 1 do
    begin
      lColumn := dbgData.Columns[I];
      if lColumn.FieldName = lFieldName then
      begin
        if dbgData.CellHasError[lColumn] then
        begin
          AError := ResStr_ErrorResolvedPleaseRevalidate;
        end;
        Break;
      end;
    end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.GridSelectedRowsChanged(Sender: TObject);
begin
  btnRemove.Enabled := dbgData.SelectedCount > 0;
  if not chkShowSelected.Checked then
    chkShowSelected.Enabled := btnRemove.Enabled;
end;  // TfraColumnTypes.GridSelectedRowsChanged

{-------------------------------------------------------------------------------
}
function TfraColumnTypes.GetHTMLImageName: String;
begin
  Result := 'Columns.jpg';
end;  // TfraColumnTypes.GetHTMLImageName

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.RegisterDragDropComponents;
begin
  // Use this method to set the initial state of the error help, as the
  // constructor and loadcontent methods are too early
  inherited;
  UpdateErrorBullets;
end;  // TfraColumnTypes.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.SetDirty(const Value: Boolean);
begin
  FDirty := Value;
  if FDirty and not LoadingPage then
    if Settings.BackFromMatchPage then
      SendMessage(ContainerForm.Handle, UM_RETURN, 1, 0)
    else
      SendMessage(ContainerForm.Handle, UM_RETURN, 0, 0);
end;  // TfraColumnTypes.SetDirty

{-------------------------------------------------------------------------------
}
procedure TfraColumnTypes.ValidateContent;
var
  lCursor: TCursor;
begin
  inherited;
  lCursor := HourglassCursor;
  try
    if Settings.ImportedData.State = dsEdit then
      Settings.ImportedData.Post;
    // use validate content to prepare tables, as this only happens when going
    // forwards in the wizard.
    PrepareTempTables;
  finally
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
  Implement Ctrl-A to select all
}
procedure TfraColumnTypes.dbgDataKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lBookmark: TBookmark;
  lCursor: TCursor;
begin
  inherited;
  if (chr(Key)='A') and (ssCtrl in Shift) then begin
    with Settings.ImportedData do begin
      lBookmark := GetBookmark;
      DisableControls;
      lCursor := HourglassCursor;
      try
        First;
        while not EOF do begin
          dbgData.CurrentRowSelected := true;
          frmMain.SetProgress(RecNo * 100 div RecordCount);
          Next;
        end;
      finally
        Gotobookmark(lBookmark);
        EnableControls;
        frmMain.SetProgress(0);
        DefaultCursor(lCursor);
      end; // try
    end;
    btnRemove.Enabled := dbgData.SelectedCount > 0;
  end; // if
end;

end.
