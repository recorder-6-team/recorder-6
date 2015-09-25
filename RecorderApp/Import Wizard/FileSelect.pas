{===============================================================================
  Unit:        FileSelect

  Defines:     TfraFileSelect

  Description: Data file selection frame. Also handles the parsing of the source
               file so that it can be loaded in the main data grid.

  Model:       ImportWizard

  Last revision information:
    $Revision: 54 $
    $Date: 21/03/13 16:50 $
    $Author: Michaelcaptain $

===============================================================================}

unit FileSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWBasePage, ExtCtrls, StdCtrls, ImageListButton, IWSettings, 
  Buttons, HTMLView, RestrictedEdits, DBClient, SMIBase, SMI2TXT, SMI2XLS,
  SMI2WKS, SMI2DB, SMI2DBF, SMI2ADO, SMI2WQ, DB, ADODB, Grids, DBGrids,
  SMI2OLE, ApplicationSettings, StrUtils, ExceptionForm, ComboListID;

resourcestring
  ResStr_RowsSepecifiedError =
      'The value specified as the %s row to import must be a positive whole number';
  ResStr_RowsSepecifiedRangeError =
      'The value specified as the first row to import must be '
      + 'lower than the value specified as the last row to import';
  ResStr_First             = 'first';
  ResStr_Last              = 'last';
  ResStr_DefaultColumnName = 'Column %d';
  ResStr_None              = '<none>';
  ResStr_LoadingIDs        = 'Loading record IDs...';

type
  {-----------------------------------------------------------------------------
    Wizard frame displayed for all file types except NBN XML files and Zipped Access Database
    files.  This page allows the user to specify the formatting options used when parsing the
    file.
  }
  TfraFileSelect = class (TBasePage)
    btnLess: TBitBtn;
    btnMore: TBitBtn;
    cmbDateFormat: TComboBox;
    cmbRecSeparator: TComboBox;
    cmbTemplate: TComboBox;
    cmbTextQualifier: TComboBox;
    eDateDelimiter: TEdit;
    eDecimalSymbol: TEdit;
    eRowsFrom: TNumberEdit;
    eRowsTo: TNumberEdit;
    eSymbol: TEdit;
    eThousandSymbol: TEdit;
    gbDateTime: TGroupBox;
    gbImportedRows: TGroupBox;
    gbNumbers: TGroupBox;
    lblDelimiterSection: TLabel;
    lblUseTemplate: TLabel;
    lblTemplateSection: TLabel;
    lblDataTableSection: TLabel;
    lblSelectTable: TLabel;
    lblRowsFrom: TLabel;
    lblRowsTo: TLabel;
    lblAdvanced: TLabel;
    lblDateDelimiter: TLabel;
    lblDateOrder: TLabel;
    lblDecimalSymbol: TLabel;
    lblQualifier: TLabel;
    lblRecSeparator: TLabel;
    lblThousandSymbol: TLabel;
    lbTables: TListBox;
    pnlDelimiters: TPanel;
    pnlFormatting: TPanel;
    pnlTableSelect: TPanel;
    pnlTemplate: TPanel;
    rbDelimited: TRadioButton;
    rbFixedWidth: TRadioButton;
    rbImportAllRows: TRadioButton;
    rbImportSelectedRows: TRadioButton;
    rgSeparator: TRadioGroup;
    shpDelimiterSeparator: TShape;
    shpTemplateSeparator: TShape;
    shpDataTableSeparator: TShape;
    shpAdvanced: TShape;
    pnlSurvey: TPanel;
    lblSurveyName: TLabel;
    shpSurveySeparator: TShape;
    lblSurvey: TLabel;
    lblSelectSurvey: TLabel;
    cmbSurvey: TIDComboBox;
    procedure btnLoadClick(Sender: TObject);
    procedure btnMoreLessClick(Sender: TObject);
    procedure rbImportRowsClick(Sender: TObject);
    procedure rbDelimitersClick(Sender: TObject);
    procedure rbFixedWidthClick(Sender: TObject);
    procedure DataChanged(Sender:TObject);
    procedure cmbTemplateChange(Sender: TObject);
    procedure cmbSurveyPopulate(Sender: TObject);
  private
    FSMImport: TSMImportBaseComponent;
    FErrors: TStringList;
    FInserts: TStringList;
    FFieldSizes: TStringList;
    FCancelled: Boolean;
    procedure ImportData;
    function CreateImportObject: TSMImportBaseComponent;
    function CreateTextImportObject: TSMImportFromText;
    function CreateXLSImportObject: TSMImportFromXLS;
    function CreateLotusImportObject: TSMImportFromWKS;
    function CreateParadoxImportObject: TSMImportFromParadox;
    function CreateDBFImportObject: TSMImportFromDBF;
    function CreateADOImportObject: TSMImportFromADO;
    function CreateQuattroImportObject: TSMImportFromQuattro;
    procedure SetImportObjRowContentIndices(
      const smImport: TSMImportBaseComponent;
      const defaultFirstDataRow: integer);
    procedure SetImportObjFormatOptions(
      const smImport: TSMImportBaseComponent);
    procedure SetImportObjSourceFile(
      const smImport: TSMImportBaseComponent);
    function GetImportObjSQL: string;
    procedure SMImportErrorEvent(Sender: TObject; Error: Exception;
      var Abort: Boolean);
    procedure SMImportBeforeRecordEvent(Sender: TObject; const Fields: string;
        var Values: Variant; var Accept: Boolean);
    procedure SMImportAfterRecordEvent(Sender: TObject; var Abort: Boolean);
    procedure SMImportCreateStructure(Sender: TObject; Columns: TSMIColumns);
    procedure SMImportGetCellParams(Sender: TObject; Field: TField;
      var Value: Variant);
    procedure LoadADOTables;
    function ValidateInput: boolean;
    function ValidateDelimiterInput: boolean;
    function ValidateFormattingInput: boolean;
    function ValidateDataTableInput: boolean;
    procedure CopyFieldNamesToRow(dsImport: TADOTable);
    procedure AddValidationFailureMessage(const AMessage: string);
    function SelectedImportType: TImportType;
    procedure SetDelimitersPanelVals;
    procedure SetFormattingPanelVals;
    procedure SetTableSelectVals;
    procedure SetSettingsDelimiterProperties;
    procedure SaveUserInput;
    procedure SetSettingsFormattingProperties;
    procedure SetSettingsADOProperties;
    procedure PopulateTemplates;
    procedure LoadTemplate;
    procedure PopulateFormFromSettings;
  protected
    function GetHasNext: Boolean; override;
    function GetNext: TBasePageClass; override;
    procedure LoadContent; override;
    function GetHTMLImageName: string; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    procedure RefreshContent; override;
    procedure SaveContent; override;
    procedure ValidateContent; override;
    procedure SaveCurrentSettings; override; 
    procedure Cancel; override;
    property Cancelled: boolean read FCancelled;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FixedWidths, ColumnTypes, FastColumnTypes, IWConstants, IWResourceStrings, Maintbar,
  SMCells, GeneralFunctions, DatabaseAccessADO, Sql, OnlineHelp;

const
  DEFAULT_FIELD_SIZE     = 100;
  DEFAULT_BIG_FIELD_SIZE = 500;
  BATCH_SIZE             = 200;

resourcestring
  ResStr_DataTableNotSelected =
    'A Data Table must be selected.';
  ResStr_FailedToImportData =
    'The data import failed. Please check that the properties are correctly set and that the import file is not empty.';
  ResStr_ValidationFailed =
    'Validation on the page failed - see the help in the left panel for details.';
  ResStr_CannotFindReqFields = 'Could not find required fields in recordset.';
  ResStr_UnrecognisedImport          = 'Unrecognised import type.';
  ResStr_UnrecognisedFieldDelimit    = 'Unrecognised field delimiter.';
  ResStr_UnrecognisedRecordSeparator = 'Unrecognised record seperator.';
  ResStr_DelimitSymbRequired  = 'A delimiter symbol is required';
  ResStr_SpecifyDateDelimiter = 'A date delimiter must be specified';
  ResStr_SpecifyDecimalSymbol = 'A decimal symbol must be specified';
  ResStr_Required = 'A value for Survey is required';

  //Status
  ResStr_ImportingData = 'Importing data...';

{-==============================================================================
    TfraFileSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.btnLoadClick(Sender: TObject);
begin
  ChangedContent;
end;  // TfraFileSelect.btnLoadClick 

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.btnMoreLessClick(Sender: TObject);
begin
  inherited;
  btnMore.Visible := not btnMore.Visible;
  btnLess.Visible := not btnMore.Visible;
  pnlFormatting.Visible := btnLess.Visible;
end;  // TfraFileSelect.btnMoreLessClick

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.cmbTemplateChange(Sender: TObject);
begin
  inherited;
  if cmbTemplate.ItemIndex > 0 then LoadTemplate;
end;  // TfraFileSelect.cmbTemplateChange

{-------------------------------------------------------------------------------
}
function TfraFileSelect.GetHasNext: Boolean;
var
  lBullets: TStringList;
begin
  lBullets := TStringList.Create;
  try
    // Check all combos have a selection.
    if ((cmbSurvey.ItemIndex = -1) and (not Settings.UseOldImportWizard)) then
      lBullets.Add(Format(ResStr_Required, [lblSurvey.Caption]));
  Result := ValidateInput and (lBullets.Count = 0);
  ChangedHtml(lBullets);
  finally
    lBullets.Free;
  end; // try
end;  // TfraFileSelect.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraFileSelect.GetNext: TBasePageClass;
begin
  if rbFixedWidth.Checked then
    Result := TfraFixedWidths
  else
  if Settings.UseOldImportWizard then
    Result := TfraColumnTypes
  else
    Result := TfraFastColumnTypes;
end;  // TfraFileSelect.GetNext 

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.cmbSurveyPopulate(Sender: TObject);
begin
  inherited;
  with dmDatabase.GetRecordset('usp_Surveys_Select', []) do
    try
      while not Eof do begin
        cmbSurvey.Add(Fields['Display_Name'].Value, String(Fields['Survey_Key'].Value));
        MoveNext;
      end;
    finally
      Close;
    end;
end;  // TfraFileSelect.cmbSurveyPopulate

{-------------------------------------------------------------------------------
}

procedure TfraFileSelect.LoadContent;
begin
  inherited;
  FErrors     := TStringList.Create;
  FInserts    := TStringList.Create;
  FFieldSizes := TStringList.Create;
  FCancelled  := False;
  PopulateTemplates;
  //nb must load ado tables b4 populating form as will try to set selected
  //ado table in list box
  if Settings.ImportType = itADO then
    LoadADOTables;
  PopulateFormFromSettings;
  if Settings.UseOldImportWizard then begin
    pnlSurvey.Hide;
    AppSettings.IWSurveyKey := '';
  end
  else
    cmbSurvey.Clear;
    cmbSurvey.PopulateContent;
    with Settings.UserSuppliedData do
    begin
      if SurveyKey <> '' then begin
        cmbSurvey.ItemIndex := cmbSurvey.IDIndexOf(SurveyKey);
        changedContent;
      end;
    end;
end;  // TfraFileSelect.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.LoadADOTables;
var
  lcnADODb: TADOConnection;
  ldsSchemaInfo: TADODataSet;
  i, lTypeFieldIndex, lNameFieldIndex: integer;
begin
  lbTables.Items.Clear;
  //nb connection string should be valid as was checked b4 opening wizard  
  lcnADODb := TADOConnection.Create(nil);
  try
    lcnADODb.LoginPrompt := false;
    lcnADODb.ConnectionString := Settings.SourceDataFile;
    ldsSchemaInfo := TADODataSet.Create(nil);
    try
      lcnADODb.OpenSchema(siTables, EmptyParam, EmptyParam, ldsSchemaInfo);
      with ldsSchemaInfo.Recordset do begin
        if not EOF then begin
          lTypeFieldIndex := -1;
          lNameFieldIndex := -1;
          for i := 0 to Fields.Count - 1 do begin
            if SameText(Fields.Item[i].Name, 'TABLE_TYPE') then
              lTypeFieldIndex := i
            else
            if SameText(Fields.Item[i].Name, 'TABLE_NAME') then
              lNameFieldIndex := i;
          end;
          if (lNameFieldIndex = - 1) or (lNameFieldIndex = -1) then
            raise Exception.Create(ResStr_CannotFindReqFields);
          while not EOF do begin
            if SameText(Fields.Item[lTypeFieldIndex].Value, 'TABLE') then
              lbTables.Items.Add(Fields.Item[lNameFieldIndex].Value);
            MoveNext;
          end;
        end;
      end;
    finally
      ldsSchemaInfo.Free;
    end;
  finally
    lcnADODb.Free;
  end;
end;  // TfraFileSelect.LoadADOTables

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.RefreshContent;
begin
  SaveContent;
  LoadContent;
end;  // TfraFileSelect.RefreshContent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SaveContent;
begin
  inherited;
  FreeAndNil(FErrors);
  FreeAndNil(FInserts);
  FreeAndNil(FFieldSizes);
  //save all user selections in case we come back to this page
  SaveUserInput;

  // In case some fields were extended, or changed to TEXT, field definitions need a refreh.
  if Settings.ImportedData.Active and (not Settings.UseOldImportWizard) then
  begin
    with Settings.UserSuppliedData do begin
      SurveyKey                 := cmbSurvey.CurrentStrID;
    end;
    AppSettings.IWSurveyKey := cmbSurvey.CurrentStrID;
    Settings.ImportedData.Close;
    Settings.ImportedData.FieldDefs.Update;
    Settings.ImportedData.Open;
  end;
end;  // TfraFileSelect.SaveContent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.DataChanged(Sender: TObject);
begin
  if Sender = eSymbol then begin
    rgSeparator.ItemIndex := rgSeparator.Items.Count - 1;
    rbDelimited.Checked := True;
  end;
  if Sender = cmbTextQualifier then rbDelimited.Checked := True;

  if (Sender = eRowsFrom) or (Sender = eRowsTo) then
     rbImportSelectedRows.Checked := True;

  ChangedContent;
end;  // TfraFileSelect.DataChanged

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.rbImportRowsClick(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraFileSelect.rbImportAllRowsClick

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.rbDelimitersClick(Sender: TObject);
begin
  inherited;
  rbDelimited.Checked := True;
  ChangedContent;
end;  // TfraFileSelect.rbDelimitersClick

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.rbFixedWidthClick(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraFileSelect.rbFixedWidthClick

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.ImportData;
var
  origCursor: TCursor;
begin
  if ValidateInput then begin
    Settings.ResetImportedData;
    origCursor := Screen.Cursor;
    try
      try
        Screen.Cursor := crHourglass;
        frmMain.SetStatus(ResStr_ImportingData);
        FSMImport := CreateImportObject;
        try
          //set generic import object properties and event handlers
          with FSMImport do begin
            DataSet             := Settings.ImportedData;
            AnimatedStatus      := False;
            AbortOnProblem      := False;
            Options             := [soExtendedStatistic, soSkipEmptyRow, soWaitCursor];
            OnErrorEvent        := SMImportErrorEvent;
            OnCreateStructure   := SMImportCreateStructure;
            OnBeforeRecordEvent := SMImportBeforeRecordEvent;
            OnAfterRecordEvent  := SMImportAfterRecordEvent;
            OnGetCellParams     := SMImportGetCellParams;
          end;

          FInserts.Clear;
          try
            FSMImport.Execute;
          except on E:EFOpenError do
            raise TExceptionPath.CreateNonCritical(E.message);
          end;
          if FInserts.Count > 0 then
            dmDatabase.ExecuteSQL(FInserts.Text);

          if not Settings.ImportedData.Active then 
          begin
            //import did not work, settings probably wrong - tell user
            //and don't go anywhere!
            Settings.ResetImportedData;
            ContainerForm.ValidateValue(false, ResStr_FailedToImportData);
          end;
        finally
          FreeAndNil(FSMImport);
        end;
      except
        on Exception do begin
          Settings.ResetImportedData;
          raise;
        end;
      end;
    finally
      Screen.Cursor := origCursor;
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    end;
  end else
    ContainerForm.ValidateValue(false, ResStr_ValidationFailed);
end;  // TfraFileSelect.ImportData

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateImportObject: TSMImportBaseComponent;
begin
  case SelectedImportType of
    itText, itCSV, itFixedWidth: Result := CreateTextImportObject;
    itExcel:                     Result := CreateXLSImportObject;
    itLotus:                     Result := CreateLotusImportObject;
    itQuattro:                   Result := CreateQuattroImportObject;
    itDBase:                     Result := CreateDBFImportObject;
    itParadox:                   Result := CreateParadoxImportObject;
    itADO:                       Result := CreateADOImportObject;
  else
    raise Exception.Create(ResStr_UnrecognisedImport);
  end;
end;  // TfraFileSelect.CreateImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateTextImportObject: TSMImportFromText;
var
  smImport: TSMImportFromText;
begin
  smImport := TSMImportFromText.Create(nil);
  Result := smImport;

  with smImport do begin
    SetImportObjSourceFile(smImport);
    SetImportObjRowContentIndices(smImport, 2);
    if rbFixedWidth.Checked then
      Fixed := true
    else begin
      Fixed := false;
      case rgSeparator.ItemIndex of
        0: FieldDelimiter := fdTab;
        1: FieldDelimiter := fdSemicolon;
        2: FieldDelimiter := fdComma;
        3: FieldDelimiter := fdSpace;
        4: begin
             FieldDelimiter := fdCustom;
             FieldDelimiterCustom := eSymbol.Text[1];
           end;
      else
        raise Exception.Create(ResStr_UnrecognisedFieldDelimit);
      end;
    end;
    case cmbRecSeparator.ItemIndex of
      0: RecordSeparator := rsCRLF;
      1: RecordSeparator := rsCR;
      2: RecordSeparator := rsLF;
    else
      raise Exception.Create(ResStr_UnrecognisedRecordSeparator);
    end;
    if cmbTextQualifier.Text = ' ' then
      TextQualifier := tqNone
    else if cmbTextQualifier.Text = '"' then
      TextQualifier := tqQuot
    else if cmbTextQualifier.Text = '''' then
      TextQualifier := tqApos
    else begin
      TextQualifier := tqCustom;
      TextQualifierCustom := cmbTextQualifier.Text[1];
    end;
  end;
end;  // TfraFileSelect.CreateTextImportObject

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetSettingsDelimiterProperties;
begin
  with Settings do begin
    ImportType := SelectedImportType;
    if ImportType = itText then
    begin
      case rgSeparator.ItemIndex of
        0: TextFieldDelimiter := #9;
        1: TextFieldDelimiter := ';';
        2: TextFieldDelimiter := ',';
        3: TextFieldDelimiter := ' ';
        4: begin
             if length(eSymbol.Text) > 0 then
               TextFieldDelimiter := eSymbol.Text[1]
             else
               TextFieldDelimiter := #0;
           end
      else
        raise Exception.Create(ResStr_UnrecognisedFieldDelimit);
      end;
    end;
    case cmbRecSeparator.ItemIndex of
      0: TextRecordSeparator := #13#10;
      1: TextRecordSeparator := #13;
      2: TextRecordSeparator := #10;
    else
      raise Exception.Create(ResStr_UnrecognisedRecordSeparator);
    end;
    if cmbTextQualifier.Text = ' ' then
      TextQualifier := #0
    else if length(cmbTextQualifier.Text) > 0 then
      TextQualifier := cmbTextQualifier.Text[1]
    else
      TextQualifier := #0;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateXLSImportObject: TSMImportFromXLS;
var
  smImport: TSMImportFromXLS;
begin
  smImport := TSMImportFromXLS.Create(nil);
  Result   := smImport;

  SetImportObjSourceFile(smImport);
  SetImportObjRowContentIndices(smImport, 1);
  SetImportObjFormatOptions(smImport);
end;  // TfraFileSelect.CreateXLSImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateLotusImportObject: TSMImportFromWKS;
var
  smImport : TSMImportFromWKS;
begin
  smImport := TSMImportFromWKS.Create(nil); 
  Result   := smImport;

  SetImportObjSourceFile(smImport);
  SetImportObjRowContentIndices(smImport, 1);
  SetImportObjFormatOptions(smImport);
end;  // TfraFileSelect.CreateLotusImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateQuattroImportObject: TSMImportFromQuattro;
var
  smImport: TSMImportFromQuattro;
begin
  smImport := TSMImportFromQuattro.Create(nil);   
  Result   := smImport;

  SetImportObjSourceFile(smImport);
  SetImportObjRowContentIndices(smImport, 1);
  SetImportObjFormatOptions(smImport);
end;  // TfraFileSelect.CreateQuattroImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateParadoxImportObject: TSMImportFromParadox;
var
  smImport: TSMImportFromParadox;
begin
  smImport := TSMImportFromParadox.Create(nil); 
  Result   := smImport;

  SetImportObjSourceFile(smImport);
end;  // TfraFileSelect.CreateParadoxImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateDBFImportObject: TSMImportFromDBF;
var
  smImport: TSMImportFromDBF;
begin
  smImport := TSMImportFromDBF.Create(nil);  
  Result   := smImport;
  
  SetImportObjSourceFile(smImport);
end;  // TfraFileSelect.CreateDBFImportObject

{-------------------------------------------------------------------------------
}
function TfraFileSelect.CreateADOImportObject: TSMImportFromADO;
var
  smImport: TSMImportFromADO;
begin
  smImport := TSMImportFromADO.Create(nil);
  Result   := smImport;

  SetImportObjSourceFile(smImport);
  smImport.SQL := GetImportObjSQL;
end;  // TfraFileSelect.CreateADOImportObject

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetImportObjSourceFile(
  const smImport: TSMImportBaseComponent);
begin
  smImport.SourceFileName := Settings.SourceDataFile;
end;  // TfraFileSelect.SetImportObjSourceFile

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetImportObjRowContentIndices(
  const smImport: TSMImportBaseComponent; const defaultFirstDataRow: integer);
begin
  with smImport do begin
    if rbImportSelectedRows.Checked then begin
      RowFieldNames := StrToInt(eRowsFrom.Text);
      RowFirst      := RowFieldNames;
      RowLast       := StrToInt(eRowsTo.Text);
    end else begin
      RowFieldNames := 1;
      RowFirst      := defaultFirstDataRow;
      RowLast       := 2147483647;
    end;
  end;
end;  // TfraFileSelect.SetRowContentIndices

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetImportObjFormatOptions(
  const smImport: TSMImportBaseComponent);
begin
  with smImport.DataFormats do begin
    CustomDateTimeFormat := StringReplace(
        cmbDateFormat.Items[cmbDateFormat.ItemIndex],
        '/',
        eDateDelimiter.Text,
        [rfReplaceAll]);
    DecimalSeparator := eDecimalSymbol.Text[1];
    if length(eThousandSymbol.Text) > 0 then
      ThousandSeparator := eThousandSymbol.Text[1]
    else
      ThousandSeparator := #0;
  end;
end;  // TfraFileSelect.SetImportObjFormatOptions

{-------------------------------------------------------------------------------
}
function TfraFileSelect.GetImportObjSQL: string;
var
  lTableName: string;

  function HasNonAlphaNumericChar(const AText: string): boolean;
  const
    ALPHANUMERICS = ['0'..'9','a'..'z','A'..'Z'];
  var
    i: integer;
  begin
    Result := false;
    for i := 1 to Length(AText) do
      if not (AText[i] in ALPHANUMERICS) then begin
        Result := true;
        break; // from loop
      end;
  end;

begin
  if HasNonAlphaNumericChar(lbTables.Items[lbTables.ItemIndex]) then
    lTableName := '[' + lbTables.Items[lbTables.ItemIndex] + ']'
  else
    lTableName := lbTables.Items[lbTables.ItemIndex];
  Result := 'SELECT * FROM ' + lTableName + ';'
end;  // TfraFileSelect.GetImportObjSQL

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SMImportErrorEvent(Sender: TObject;
  Error: Exception; var Abort: Boolean);
begin
  Abort := true;
  frmMain.SetStatus('');
  frmMain.SetProgress(0);
  // Display error, but only if the import is not being cancelled.
  if not Cancelled then
    Application.OnException(Sender, Error);
end;  // TfraFileSelect.SMImportErrorEvent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SMImportBeforeRecordEvent(Sender: TObject;
    const Fields: string; var Values: Variant; var Accept: Boolean);
var
  I: Integer;
  lValues: String;
begin
  lValues := '';
  for I := 0 to VarArrayHighBound(Values, 1) do
  begin
    if I > 0 then lValues := lValues + ', ';
    lValues := lValues + TransactSqlLiteral(VarToStr(Values[I][2]));
  end;

  FInserts.Add(
      'INSERT INTO ' + Settings.ImportedData.TableName
      + ' VALUES (' + lValues + ')');

  if FInserts.Count = BATCH_SIZE then
  begin
    dmDatabase.ExecuteSQL(FInserts.Text);
    FInserts.Clear;
  end;
  Accept := False;
end;  // TfraFileSelect.SMImportBeforeRecordEvent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SMImportAfterRecordEvent(Sender: TObject;
  var Abort: Boolean);
begin
  if Assigned(FSMImport) then
    if FSMImport.Statistic.TotalCount > 0 then
      if FSMImport.Statistic.TotalImported mod BATCH_SIZE = 0 then
      begin
        frmMain.SetProgress(
            Round(FSMImport.Statistic.TotalImported / FSMImport.Statistic.TotalCount * 100));
        Application.ProcessMessages;
      end;
end;  // TfraFileSelect.SMImportAfterRecordEvent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SMImportCreateStructure(Sender: TObject;
  Columns: TSMIColumns);
var
  i: integer;
  lFieldSize: integer;
  lFieldName: string;
  lSql: String;
begin
  if not Assigned(FSMImport) then
    Exit;
  Settings.ImportedData.Close;
  FFieldSizes.Clear;

  lSql := 'CREATE TABLE ' + Settings.ImportedData.TableName + ' (';

  for i := 0 to Columns.Count - 1 do begin
    if SelectedImportType in [itDBase, itParadox, itADO] then
      lFieldName := StringReplace(Columns[i].FieldName, ' ', '', [rfReplaceAll])
    else
      lFieldName := Format(ResStr_DefaultColumnName, [i + 1]);

    if Columns[i].DataType = itString then
    begin
      // Default used when we don't know,
      // Or when it's text/excel as SMImport is not reliable in those cases.
      if (Columns[i].Size = 0) or (SelectedImportType in [itText, itCSV, itExcel]) then
        lFieldSize := DEFAULT_FIELD_SIZE
      else
        lFieldSize := Columns[i].Size + 10;
      { Create a field of the required size. We add 10 to this - see incident
          12585 for details. This is because you can have a string column
          containing dates such as 12-01-05, which reports a size of 9. But
          when the cell is parsed, it returns a date, which converts to the
          system date format which may be more than 9 chars. }
      // Set text field if field won't fit into a varchar
      if lFieldSize > 8000 then
        lSql := lSql
           + #10#9'"' + lFieldName + '" TEXT  NULL, '
      else
        lSql := lSql
           + #10#9'"' + lFieldName + '" VARCHAR(' + IntToStr(lFieldSize) + ') NULL, ';
      // Set initial field size for possible alterations later, not for old wizard though.
      FFieldSizes.Values[lFieldName] := IntToStr(lFieldSize);
    end else begin
      lSql := lSql + #10#9'"' + lFieldName + '" VARCHAR(50) NULL, ';
    end;

    if SelectedImportType in [itDBase, itParadox, itADO] then
      FSMImport.Mappings.Add(lFieldName + '="' + Columns[i].FieldName + '"')
    else
    if SelectedImportType in [itExcel, itLotus, itQuattro] then
    begin
      Settings.FirstRow.Add(lFieldName + '=' + Columns[i].FieldName);
      FSMImport.Mappings.Add(lFieldName + '=' + GetSpreadSheetColByID(I + 1));
    end else begin
      Settings.FirstRow.Add(lFieldName + '=' + Columns[i].FieldName);
      FSMImport.Mappings.Add(lFieldName + '=' + Columns[i].Caption);
    end;
  end;
  lSql := lSql + #10#9'"' + FLD_ROWID + '" INT IDENTITY PRIMARY KEY)';
  dmDatabase.ExecuteSql(lSql);
  Settings.ImportedData.Open;

  if SelectedImportType in [itDBase, itParadox, itADO] then
    for I := 0 to Columns.Count - 1 do
      Settings.ImportedData.Fields[I].DisplayLabel := Columns[I].FieldName
  else
  if SelectedImportType in [itText, itCSV] then
    //copy the field names of the data set into a new row
    //for text files in case there are no column heading in file
    CopyFieldNamesToRow(Settings.ImportedData);
end;  // TfraFileSelect.SMImportCreateStructure

{-------------------------------------------------------------------------------
}
function TfraFileSelect.ValidateInput: boolean;
var
  lIsValid: boolean;
begin
  if Assigned(FErrors) then
    FErrors.Clear;

  lIsValid := true;
  if pnlDelimiters.Visible then
    lIsValid := ValidateDelimiterInput and lIsValid;
  //pnlFormatting is hidable with more/less button so check the file type
  //to determine if should be checked
  if SelectedImportType in [itText, itCSV, itFixedWidth, itExcel, itLotus, itQuattro] then
    lIsValid := ValidateFormattingInput and lIsValid;
  if pnlTableSelect.Visible then
    lIsValid :=  ValidateDataTableInput and lIsValid;
  Result := lIsValid;

  if Assigned(FErrors) then
  ChangedHTML(FErrors);
end;  // TfraFileSelect.ValidateInput

{-------------------------------------------------------------------------------
}
function TfraFileSelect.ValidateDelimiterInput: boolean;
var
  lIsValid: boolean;
begin
  lIsValid := true;
  if rbDelimited.Checked then
    if rgSeparator.ItemIndex = 4 then
      if length(eSymbol.Text) = 0 then begin
        lIsValid := false;
        AddValidationFailureMessage(ResStr_DelimitSymbRequired);
      end;
  Result := lIsValid;
end;  // TfraFileSelect.ValidateDelimiterInput

{-------------------------------------------------------------------------------
}
function TfraFileSelect.ValidateFormattingInput: boolean;
var
  lIsValid: boolean;
  lRowsFrom, lRowsTo: integer;
begin
  lIsValid := true;
  if rbImportSelectedRows.Checked then
  begin
    if TryStrToInt(eRowsFrom.Text, lRowsFrom) then begin
      if lRowsFrom <= 0 then begin
        AddValidationFailureMessage(Format(ResStr_RowsSepecifiedError, [ResStr_First]));
        lIsValid := false;
      end;
    end else begin
      AddValidationFailureMessage(Format(ResStr_RowsSepecifiedError, [ResStr_First]));
      lIsValid := false;
    end;

    if TryStrToInt(eRowsTo.Text, lRowsTo) then begin
      if lRowsTo <= 0 then begin
        AddValidationFailureMessage(Format(ResStr_RowsSepecifiedError, [ResStr_Last]));
        lIsValid := false;
      end;
    end else begin
      AddValidationFailureMessage(Format(ResStr_RowsSepecifiedError, [ResStr_Last]));
      lIsValid := false;
    end;

    if (lRowsFrom > lRowsTo) and (lRowsTo > 0) then begin
      lIsValid := false;
      AddValidationFailureMessage(ResStr_RowsSepecifiedRangeError);
    end;
  end;

  if Length(eDateDelimiter.Text) = 0 then begin
    lIsValid := false;
    AddValidationFailureMessage(ResStr_SpecifyDateDelimiter);
  end;

  if Length(eDecimalSymbol.Text) = 0 then begin
    lIsValid := false;
    AddValidationFailureMessage(ResStr_SpecifyDecimalSymbol);
  end;
  Result := lIsValid;
end;  // TfraFileSelect.ValidateFormattingInput

{-------------------------------------------------------------------------------
}
function TfraFileSelect.ValidateDataTableInput: boolean;
begin
  if lbTables.ItemIndex >= 0 then
    Result := true
  else begin
    AddValidationFailureMessage(ResStr_DataTableNotSelected);
    Result := false;
  end;
end;  // TfraFileSelect.ValidateDataTableInput

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.CopyFieldNamesToRow(dsImport: TADOTable);
var
  i: integer;
begin
  with dsImport do begin
    First;
    Insert;
    for i := 0 to Fields.Count - 1 do
      if Fields[i].FieldName<>FLD_ROWID then
        Fields[i].Value := Settings.FirstRow.Values[Fields[i].FieldName];
    Post;
  end;
end;  // TfraFileSelect.CopyFieldNamesToRow

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SMImportGetCellParams(Sender: TObject; Field: TField;
  var Value: Variant);
begin
  if not Assigned(FSMImport) then
    Exit;
  if (VarType(Value) = varOleStr)
     or (VarType(Value) = varStrArg)
     or (VarType(Value) = varString)
  then
    // Old wizard, proceed with using the LargeFields approach.
    if Settings.UseOldImportWizard then
    begin
      if Field.Size < Length(VarToStr(Value)) then
      begin
        // Field is too big to fit, so remember the actual value
        Settings.LargeFields.Add(
            Field.FieldName + ',' + IntToStr(FSMImport.Statistic.TotalImported)
            + '=' + VarToStr(Value));
        // Add event handlers to get the actual values when necessary
        Field.OnGetText := Settings.LargeFieldGetText;
        Field.OnSetText := Settings.LargeFieldSetText;
        // store a trimmed version in the dataset
        Value := StrUtils.LeftStr(VarToStr(Value), Field.Size - 3) + '...';
      end;
    end else
    if Length(VarToStr(Value)) > DEFAULT_FIELD_SIZE then
      // Might need to change to TEXT.
      if Length(VarToStr(Value)) > DEFAULT_BIG_FIELD_SIZE then
      begin
        // Change to TEXT, but only if not already done.
        if FFieldSizes.Values[Field.FieldName] <> 'TEXT' then
        begin
          dmDatabase.ExecuteSQL(Format(
              'ALTER TABLE "%s" ALTER COLUMN "%s" TEXT ',
              [Settings.ImportedData.TableName, Field.FieldName]));
          FFieldSizes.Values[Field.FieldName] := 'TEXT';
        end;
      end else
      // Change to BIG, if still on DEFAULT.
      if FFieldSizes.Values[Field.FieldName] = IntToStr(DEFAULT_FIELD_SIZE) then
      begin
        dmDatabase.ExecuteSQL(Format(
            'ALTER TABLE "%s" ALTER COLUMN "%s" VARCHAR(%d) ',
            [Settings.ImportedData.TableName, Field.FieldName, DEFAULT_BIG_FIELD_SIZE]));
        FFieldSizes.Values[Field.FieldName] := IntToStr(DEFAULT_BIG_FIELD_SIZE);
      end;
end;  // TfraFileSelect.SMImportGetCellParams

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.AddValidationFailureMessage(const AMessage: string);
begin
  if Assigned(FErrors) then
    FErrors.Add(AMessage);
end;  // TfraFileSelect.AddValidationFailureMessage

{-------------------------------------------------------------------------------
}
function TfraFileSelect.SelectedImportType: TImportType;
begin
  if not pnlDelimiters.Visible then
    Result := Settings.ImportType
  else if rbFixedWidth.Checked then
    Result := itFixedWidth
  else
    Result := itText;
end;  // TfraFileSelect.SelectedImportType

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetDelimitersPanelVals;
begin
  with Settings do begin
    rbFixedWidth.Checked := not (Settings.ImportType in [itText, itCSV]);
    rbDelimited.Checked := (Settings.ImportType in [itText, itCSV]);
    case TextFieldDelimiter of
      #9 : rgSeparator.ItemIndex := 0;
      ';': rgSeparator.ItemIndex := 1;
      ',': rgSeparator.ItemIndex := 2;
      ' ': rgSeparator.ItemIndex := 3;
    else
      rgSeparator.ItemIndex := 4;
      if TextFieldDelimiter <> #0 then
        eSymbol.Text := TextFieldDelimiter;
    end;

    if TextRecordSeparator = #10 then
      cmbRecSeparator.ItemIndex := 2
    else if TextRecordSeparator = #13 then
      cmbRecSeparator.ItemIndex := 1
    else //#13#10 or ''
      cmbRecSeparator.ItemIndex := 0;

    if TextQualifier = #0 then
      cmbTextQualifier.ItemIndex := 0
    else if TextQualifier = '"' then
      cmbTextQualifier.ItemIndex := 1
    else if TextQualifier = '''' then
      cmbTextQualifier.ItemIndex := 2
    else begin
      cmbTextQualifier.ItemIndex := -1;
      cmbTextQualifier.Text := TextQualifier;
    end;
  end;
end;  // TfraFileSelect.SetDelimitersPanelVals

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetFormattingPanelVals;
begin
  with Settings do begin
    if not ImportOnlySpecifiedRows then
      rbImportAllRows.Checked := true
    else begin
      rbImportSelectedRows.Checked := true;
      eRowsFrom.Text := IntToStr(ImportRowsFrom);
      eRowsTo.Text   := IntToStr(ImportRowsTo);
    end;
    cmbDateFormat.ItemIndex := -1;
    cmbDateFormat.Text      := DateFormat;

    eDateDelimiter.Text := DateDelimiter;
    eDecimalSymbol.Text := DecimalSymbol;
    if ThousandSeparator = #0 then
      eThousandSymbol.Text := ''
    else
      eThousandSymbol.Text := ThousandSeparator;
  end;
end;  // TfraFileSelect.SetFormattingPanelVals

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetTableSelectVals;
var
  i: integer;
begin
  if length(Settings.ADOImportTable) > 0 then begin
    for i := 0 to lbTables.Items.Count - 1 do begin
      if Settings.ADOImportTable = lbTables.Items[i] then begin
        lbTables.ItemIndex := i;
        break;
      end;
    end;
  end;
end;  // TfraFileSelect.SetTableSelectVals

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SaveUserInput;
begin
  case SelectedImportType of
    itText, itCSV, itFixedWidth: begin
        SetSettingsDelimiterProperties;
        SetSettingsFormattingProperties;
      end;
    itExcel, itLotus, itQuattro:       
      SetSettingsFormattingProperties;
    itADO:
      SetSettingsADOProperties;
  end;
end;  // TfraFileSelect.SaveUserInput

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetSettingsFormattingProperties;
begin
  with Settings do begin
    DateFormat := cmbDateFormat.Text;
    if length(eDateDelimiter.Text) > 0 then
      DateDelimiter := eDateDelimiter.Text[1]
    else
      DateDelimiter := #0;
    if length(eDecimalSymbol.Text) > 0 then
      DecimalSymbol := eDecimalSymbol.Text[1]
    else
      DecimalSymbol := #0;
    if length(eThousandSymbol.Text) > 0 then
      ThousandSeparator := eThousandSymbol.Text[1]
    else
      ThousandSeparator := #0;
    if rbImportSelectedRows.Checked then begin
      ImportOnlySpecifiedRows := true;
      try
        ImportRowsFrom := StrToInt(eRowsFrom.Text);
      except on Exception do
        ImportRowsFrom := 0
      end;
      try
        ImportRowsTo := StrToInt(eRowsTo.Text);
      except on Exception do
        ImportRowsTo := 0
      end;
    end else begin
      ImportOnlySpecifiedRows := false;
      ImportRowsFrom := 1;
      ImportRowsTo   := High(Integer);
    end;
  end;
end;  // TfraFileSelect.SetSettingsFomattingProperties

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SetSettingsADOProperties;
begin
  if lbTables.ItemIndex > -1 then
    Settings.ADOImportTable := lbTables.Items[lbTables.ItemIndex];
end;  // TfraFileSelect.SetSettingsADOProperties

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.ValidateContent;
begin
  //Use ValidateContent to perform import of data in dataset as is called
  //only when next is clicked (and not when previous is)

  //if fixed width we are not going to import data here otherwise we need to
  //fill a dataset first
  if (SelectedImportType <> itFixedWidth) and
     not ((SelectedImportType in [itText, itCSV]) and rbFixedWidth.Checked) then
    try
      ImportData;
    // Suppress errors if cancelled.
    except
      on Exception do
        if not Cancelled then
          raise;
    end;
end;  // TfraFileSelect.ValidateContent

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.PopulateTemplates;
var
  lSearchRec: TSearchRec;
  lSearchStr: string;
begin
  with cmbTemplate do begin
    Clear;
    (*TODO: remove this code and use a method in settings instead*)
    lSearchStr := AppSettings.ImportTemplatePath + '*.xml';
    try
      if FindFirst(lSearchStr, 0, lSearchRec) = 0 then
      begin
        Items.Add(LeftStr(lSearchRec.Name, Length(lSearchRec.Name) - 4));
        while FindNext(lSearchRec) = 0 do
          Items.Add(LeftStr(lSearchRec.Name, Length(lSearchRec.Name) - 4));
      end;

      Sorted := True;
      Sorted := False;
      Items.Insert(0, ResStr_None);
      ItemIndex := 0; 
    finally
      FindClose(lSearchRec);
    end;
  end;
end;  // TfraFileSelect.PopulateTemplates

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.LoadTemplate;
begin
  //user has chosen to load from template - need to populate settings and then
  //form from settings.  Column mappings load after data is loaded
  Settings.LoadFileSettingsFromFile(
      AppSettings.ImportTemplatePath + cmbTemplate.Text + '.xml');
  PopulateFormFromSettings;
end;  // TfraFileSelect.LoadTemplate

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.PopulateFormFromSettings;
begin
  if Settings.ImportType in [itText, itCSV, itFixedWidth] then
  begin
    cmbTextQualifier.Enabled := (Settings.ImportType in [itText, itCSV]);
    SetDelimitersPanelVals;
    SetFormattingPanelVals;
  end else
  if Settings.ImportType in [itExcel, itLotus, itQuattro] then
  begin
    pnlDelimiters.Visible := False;
    pnlTemplate.Height    := pnlTemplate.Height + btnMore.Height;
    with btnLess do begin
      Parent  := pnlTemplate;
      Top     := pnlTemplate.Height - Height - 4;
      Anchors := [akRight, akTop];
    end;
    with btnMore do begin
      Parent  := pnlTemplate;
      Top     := pnlTemplate.Height - Height - 4;
      Anchors := [akRight, akTop];
    end;
    SetFormattingPanelVals;
  end else
  if Settings.ImportType in [itDBase, itParadox, itADO] then begin
    pnlDelimiters.Visible := False;
    // Paradox & DBase: 1 file per table. No need for selection.
    pnlTableSelect.Visible := (Settings.ImportType = itADO);
    if Settings.ImportType = itADO then begin
      LoadADOTables;
      SetTableSelectVals;
    end;
  end;
end;  // TfraFileSelect.PopulateFormFromSettings

{-------------------------------------------------------------------------------
}
procedure TfraFileSelect.SaveCurrentSettings;
begin
  with Settings.UserSuppliedData do begin
      SurveyKey                 := cmbSurvey.CurrentStrID;
  end;
  Settings.HaveVisitedFileSelect := True;
  SaveUserInput;
end;  // TfraFileSelect.SaveCurrentSettings

{-------------------------------------------------------------------------------
}
function TfraFileSelect.GetHTMLImageName: string;
begin
  Result := 'Settings.jpg';
end;

{-------------------------------------------------------------------------------
  Constructor
}
constructor TfraFileSelect.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited;
  HelpContext := IDH_IWFILESELECT;
end;

{-------------------------------------------------------------------------------
  Cancels the import.
}
procedure TfraFileSelect.Cancel;
begin
  inherited;
  FCancelled := True;
end;  // TfraFileSelect.Cancel

end.
