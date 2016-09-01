//==============================================================================
//  Unit:        SurveyDetails
//
//  Implements:  TfrmSurveyDetails
//
//  Description:
//
//  Author:      Eric Salmon
//  Created:     4 Feb 2008
//
//  Last Revision Details:
//    $Revision: 11 $
//    $Date: 16/01/09 11:50 $
//    $Author: Pauldavies $
//
//==============================================================================

unit SurveyTagDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, StdCtrls, Grids, DBGrids, ExtCtrls, Buttons, Menus, DBCtrls,
  ComCtrls, Mask, VagueDateEdit, BaseChildUnit, CompositeComponent, Sources,
  SurveyData, DataClasses, Db, DropTarget, VagueDate, Constants, JNCCDatasets,
  ExceptionForm, BaseDockedForm, ValidationData, OnlineHelp, SpatialRefFuncs,
  GeneralFunctions, ImageListButton, DatabaseAccessADO, DataStringGrid,
  ControlStringGrid, ADODB, ConceptTerm;

type
  ESurveyTagDetailsError = class(TExceptionPath);

  TfrmSurveyTagDetails = class(TfrmBaseDockedForm)
    qrySurveys: TJNCCQuery;
    qrySurveysConcept_Key: TStringField;
    qrySurveysSurvey_Key: TStringField;
    qrySurveysItemName: TStringField;
    qrySurveysEntered_By: TStringField;
    qrySurveysCustodian: TStringField;
    qrySurveysSurvey_Tag_Key: TStringField;
    pnlDetails: TPanel;
    pnlInner: TPanel;
    Shape2: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eTagName: TEdit;
    lblSurveyTagDisp: TLabel;
    btnSave: TImageListButton;
    btnCancel: TImageListButton;
    sgSurveys: TControlStringGrid;
    btnDeleteSurvey: TImageListButton;
    btnAddSurvey: TImageListButton;
    mmDescription: TMemo;
    Bevel1: TBevel;
    eTagSortCode: TEdit;
    lblSortCode: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pnlDetailsResize(Sender: TObject);
    procedure eTagSortCodeExit(Sender: TObject);
    procedure eTagSortCodeKeyPress(Sender: TObject; var Key: Char);
  private
    FDrillForm:TBaseForm;
    FConceptKey: TKeyString;
    FClosingForm : Boolean;
    FSurveysGridManager: TDataStringGrid;
    FConceptTerm: TConceptTerm;
    FRefreshSurveysNeeded: Boolean;
    FFactKey: TKeyString;
    FFactName: String;
    FFactLanguage: String;
    FFactTimestamp: TSQLSvrTimestamp;
    procedure ClearFields;
    procedure EnableDetails(newMode:TEditMode);
    procedure SetDrillForm(Value: TBaseForm);
    procedure WMTransferDone(var Msg: TMessage); message WM_TRANSFER_DONE;
    procedure FreeObjects;
    procedure SetupObjects;
    procedure DropSurvey(const Sender: TObject; const format: Integer; const sourceData: TKeyList;
        const textStrings: TStringList; var handled: Boolean);
    procedure PopulateSurveys;
    procedure SurveyFindData(const initialText: String; var text, key: String);
    procedure SurveyGetData;
    procedure SurveyDeleteRow(rowKey: String);
    procedure SurveyUpdateRow(var rowKey: String; data: TStringList);
    procedure ValidateSurveys;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord;
    procedure EditRecord;
    procedure DisplayRecord(const AConceptKey:TKeyString);
    procedure DeleteRecord(const AConceptKey: TKeyString);
    procedure RefreshColours;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property ConceptKey: TKeyString read FConceptKey write FConceptKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, EventDetails, Maintbar,
  Find, Observations, Map, MetadataPopup, EnhancedTermLists, Variants,
  BaseADODataModule;

resourcestring
  ResStr_SurveyTagRequired  = 'A Survey Tag Name is required for every Survey Tag.';
  ResStr_ThisSurveyIsAlreadyInList = 'This survey is already present in the list.';
  ResStr_SpecifyValidSurvey = 'Please specify a valid survey.';
  ResStr_FactName           = 'Fact Description';
  ResStr_ThesaurusFactTypeName = 'Fact';
  ResStr_TagNameAlreadyUsed =
      'The specified Survey Tag is already in use. Please choose another.';

//==============================================================================
constructor TfrmSurveyTagDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DrillForm := nil;
  SetupObjects;
  EnableDetails(emView);

  Self.HelpContext := IDH_SURVEYTAGSGENERAL;
end;  // Create

//==============================================================================
procedure TfrmSurveyTagDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmSurveyTagDetails.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose := False;
  if EditMode <> emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                btnSaveClick(nil);
                CanClose := True;
              end;
      mrNo  : begin
                FClosingForm := True;
                btnCancelClick(nil);
                CanClose := True;
                FClosingForm := False;
              end;
    end;
  end else
    CanClose := True;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmSurveyTagDetails.SetupObjects;
begin
  FConceptTerm := TConceptTerm.Create;

  qrySurveys.Connection := dmDatabase.dbLocal;

  sgSurveys.Cells[0, 0] := ResStr_Surveys;
  FSurveysGridManager   := TDataStringGrid.Create(sgSurveys, qrySurveys, 'Survey_Tag_Key');
  with FSurveysGridManager do begin
    UserID               := AppSettings.UserID;
    RestrictFullEdit     := AppSettings.RestrictFullEdit;
    SiteID               := AppSettings.SiteID;
    AddOnly              := AppSettings.UserAccessLevel = ualAddOnly;
    btnAddSurvey.OnClick := AddToGrid;
    btnDeleteSurvey.OnClick := DeleteFromGrid;
    SetupLinkedEdit('Survey_Key', 0, SurveyFindData, SurveyGetData);
    ImageList            := dmFormActions.ilButtons;
    GetButtonImageIndex  := 5;
    OnUpdateRow          := SurveyUpdateRow;
    OnDeleteRow          := SurveyDeleteRow;
  end;
end;  // SetupObjects

//==============================================================================
procedure TfrmSurveyTagDetails.FreeObjects;
begin
  FConceptTerm.Free;
  FSurveysGridManager.Free;
end;  // FreeObjects

//==============================================================================
procedure TfrmSurveyTagDetails.DisplayRecord(const AConceptKey: TKeyString);
var
  cursor:TCursor;
begin
  cursor := HourglassCursor;
  ClearFields;
  FConceptKey := AConceptKey;
  try
    FConceptTerm.Load(ConceptKey);
    lblSurveyTagDisp.Caption := FConceptTerm.Term;
    eTagName.Text            := FConceptTerm.Term;
    eTagSortCode.Text     := FConceptTerm.SortCode;
    with dmDatabase.GetRecordset(
        'usp_ThesaurusFact_Select_ForSurveyTag',
        ['@ConceptKey', ConceptKey]) do
      if not Eof then begin
        FFactKey           := Fields['Thesaurus_Fact_Key'].Value;
        FFactName          := Fields['Item_Name'].Value;
        FFactLanguage      := Fields['Language_Key'].Value;
        FFactTimestamp     := Fields['Timestamp'].Value;
        mmDescription.Text := VarToStr(Fields['Data'].Value);
        Close;
      end;
    PopulateSurveys;
    FRefreshSurveysNeeded := False;
  finally
    DefaultCursor(cursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmSurveyTagDetails.AddRecord;
begin
  ConceptKey := '';
  ClearFields;
  PopulateSurveys;
  EnableDetails(emAdd);
end;  // AddRecord

//==============================================================================
procedure TfrmSurveyTagDetails.ClearFields;
begin
  lblSurveyTagDisp.Caption := '';
  eTagName.Text            := '';
  mmDescription.Text       := '';
  FConceptTerm.Clear;
  FFactKey                 := '';
  FFactName                := '';
  FFactLanguage            := '';
  FFactTimestamp           := Null;
end;  // ClearFields

//==============================================================================
procedure TfrmSurveyTagDetails.DeleteRecord(const AConceptKey: TKeyString);
begin
  dmDatabase.RunStoredProc('usp_SurveyTag_Delete_ForConcept', ['@ConceptKey', AConceptKey]);
end;  // DeleteRecord

//==============================================================================
{ Display a form to edit the selected survey }
procedure TfrmSurveyTagDetails.EditRecord;
begin
  DisplayRecord(ConceptKey);
  if dmGeneralData.SessionHasFullEditAccess(TN_CONCEPT, 'Concept_Key', ConceptKey) then
    EnableDetails(emEdit);
end;  // EditRecord

//==============================================================================
procedure TfrmSurveyTagDetails.btnSaveClick(Sender: TObject);
var
  cursor: TCursor;
  lang: String;
  params: Array of Variant;
begin
  inherited;

  cursor := HourglassCursor;
  try
    SetLength(params, 0);

    // If new tag, get the correct concept group
    if EditMode = emAdd then begin
      with dmDatabase.GetRecordset('usp_ConceptGroup_Select_ForApplication', ['@Key', ACG_SURVEY_TAGS]) do
        FConceptTerm.ConceptGroupKey := Fields['Concept_Group_Key'].Value;

      lang := Languages.NameFromLocaleID[GetUserDefaultLCID];
      if Pos('(', lang) > 0 then lang := Trim(Copy(lang, 1, Pos('(', lang) - 1));
      FConceptTerm.LanguageId := dmDatabase.GetStoredProcOutputParam(
          'usp_Language_Find',
          ['@Name', lang],
          '@Key');
    end;

    FConceptTerm.Term := eTagName.Text;
    FConceptTerm.SortCode := eTagSortCode.Text;
    ValidateValue(eTagName.Text <> '', ResStr_SurveyTagRequired, eTagName);
    if EditMode = emAdd then
      ValidateValue(not FConceptTerm.ReuseExistingConcept, ResStr_TagNameAlreadyUsed, eTagName);
    ValidateSurveys;

    FConceptTerm.Save;
    ConceptKey := FConceptTerm.ConceptKey;

    if Trim(mmDescription.Text) <> '' then begin
      if FFactKey = '' then begin
        FFactName := ResStr_FactName;
        FFactLanguage := FConceptTerm.LanguageId;
      end;

      params := VarArrayOf(
          ['@Key', FFactKey,
           '@ItemName', FFactName,
           '@Data', Trim(mmDescription.Text),
           '@MeaningKey', Null,
           '@ConceptKey', ConceptKey,
           '@TermVersionKey', Null,
           '@RelatedTermVersions', 0,
           '@Inherited', 0,
           '@LanguageKey', FFactLanguage,
           '@FactTypeMeaningKey', MEANING_KEY_FACT,
           '@FactTypeMeaningName', ResStr_ThesaurusFactTypeName,
           '@FactVagueDateStart', Date,
           '@FactVagueDateEnd', Date,
           '@FactVagueDateType', 'DD',
           '@Timestamp', FFactTimestamp
      ]);

      if FFactKey = '' then
        dmDatabase.RunInsertStoredProc(TN_THESAURUS_FACT, 'usp_ThesaurusFact_Insert', params, '@Key')
      else
        dmDatabase.RunUpdateStoredProc('usp_ThesaurusFact_Update', params);
    end else
    if FFactKey <> '' then
      dmDatabase.RunDeleteStoredProc(
          'usp_ThesaurusFact_Delete',
          ['@Key', FFactKey, '@Timestamp', FFactTimestamp, '@SyncTaxonDict', 0]);
    
    FSurveysGridManager.Save;

    if DrillForm <> nil then
      with TfrmObservations(DrillForm) do begin
        if FRefreshSurveysNeeded then begin
          SurveyTagToReselect := ConceptKey;
          PostMessage(Handle, WM_REFRESH_OBSERVATIONS, 0, 0);
        end else begin
          SetItemTextAndKey(eTagName.Text, ConceptKey);
          tvObservations.Selected := SelectedItem;
        end;
      end;

    EnableDetails(emView);
    DisplayRecord(ConceptKey);
  finally
    DefaultCursor(cursor);
  end;
end;  // btnSaveClick

//==============================================================================
procedure TfrmSurveyTagDetails.btnCancelClick(Sender: TObject);
var
  discardNew: Boolean;
begin
  inherited;
  discardNew := ConceptKey = '';
  EnableDetails(emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if discardNew and (DrillForm <> nil) then
      PostMessage(TfrmObservations(DrillForm).Handle, WM_DISCARD_OBSERVATION, 0, 0)
    else begin
      with TfrmObservations(DrillForm) do
        tvObservations.Selected := SelectedItem;
      DisplayRecord(ConceptKey);
    end;
end;  // btnCancelClick

//==============================================================================
procedure TfrmSurveyTagDetails.EnableDetails(newMode: TEditMode);
var
  editing: Boolean;
begin
  FEditMode := newMode;
  editing := EditMode <> emView;

  RefreshColours;
  eTagSortCode.ReadOnly := not editing;
  eTagName.ReadOnly := not editing;
  mmDescription.ReadOnly := not editing;

  FSurveysGridManager.Enabled := editing;
  sgSurveys.Refresh;
  btnAddSurvey.Enabled := editing;
  btnDeleteSurvey.Enabled := editing;

  btnSave.Enabled   := editing;
  btnCancel.Enabled := editing;

  if DrillForm <> nil then TfrmObservations(DrillForm).SetMenuState(not editing);
end;  // EnableDetails

//==============================================================================
procedure TfrmSurveyTagDetails.SetDrillForm(Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmSurveyTagDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm <> nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmSurveyTagDetails.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu((DrillForm.ActiveControl is TDBRichEdit) and (EditMode <> emView));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmSurveyTagDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(EditMode <> emView);
end;  // EnterRTF

//==============================================================================
procedure TfrmSurveyTagDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmformActions.UpdateRTFMenu(False);
end;  // ExitRTF

//==============================================================================
procedure TfrmSurveyTagDetails.SetupDestinationControls;
begin
  RegisterDropComponent(sgSurveys, DropSurvey, [TN_SURVEY], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmSurveyTagDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode <> emView, [eTagName]);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.DropSurvey(const Sender: TObject; const format: Integer;
    const sourceData: TKeyList; const textStrings: TStringList; var handled: Boolean);
var
  existingRow: Integer;
begin
  if FSurveysGridManager.Enabled then
    if SameText(sourceData.Header.TableName, TN_SURVEY) and
       (sourceData.Header.ItemCount > 0) then
    begin
      existingRow := FSurveysGridManager.FindRowByKeyInColumn(sourceData.Items[0].KeyField1, 0);
      if existingRow <> -1 then
        sgSurveys.Row := existingRow
      else begin
        FSurveysGridManager.AddToGrid(nil);
        with dmDatabase.GetRecordset('usp_Survey_Select', ['@Key', sourceData.Items[0].KeyField1]) do
          FSurveysGridManager.UpdateLinkedValue(
              Fields['Display_Name'].Value, sourceData.Items[0].KeyField1);
      end;
    end;
end;  // DropSurvey

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.PopulateSurveys;
begin
  qrySurveys.Close;
  qrySurveys.Parameters.ParamByName('Key').Value := ConceptKey;
  qrySurveys.Parameters.ParamByName('UserNameKey').Value := AppSettings.UserID;
  qrySurveys.Open;
  FSurveysGridManager.PopulateGrid;
  FSurveysGridManager.Refresh;
end;  // PopulateSurveys

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.SurveyFindData(const initialText: String; var text, key: String);
var
  tmpKey: TKeyString;
  duplicateRow: Integer;

  function CheckSurvey: Boolean;
  var
    find: TdlgFind;
  begin
    Result := true;
    find := TdlgFind.CreateDialog(nil, ResStr_FindSurvey, ftSurvey);
    with find do begin
      try
        if FindUnique(text) then begin
          tmpKey := ItemKey;
          text := ItemText;
        end
        else if not eSearchText.NoSourceItems then begin
          if ShowModal = mrOk then begin
            tmpKey := ItemKey;
            text := ItemText;
          end else
            Result := False;
        end else begin
          Result := False;
          ShowInformation(ResStr_NoSurveys);
        end;
      finally
        Free;
      end;
    end;
  end;

begin
  text := initialText;
  CheckSurvey;
  duplicateRow := FSurveysGridManager.FindRowByKeyInColumn(tmpKey, 0);
  if (duplicateRow = -1) or (FSurveysGridManager.CellLinkedKey[0, sgSurveys.Row] = tmpKey) then
    key := tmpKey
  else begin
    key := '';
    ShowInformation(ResStr_ThisSurveyIsAlreadyInList);
  end;
end;  // SurveyFindData

{-------------------------------------------------------------------------------
 Make the get behave like the find. There's only 1 observation screen.
}
procedure TfrmSurveyTagDetails.SurveyGetData;
var
  text, key: String;
begin
  SurveyFindData(sgSurveys.Cells[sgSurveys.Col, sgSurveys.Row], text, key);
  FSurveysGridManager.UpdateLinkedValue(text, key);
end;  // SurvyeGetData

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.SurveyDeleteRow(rowKey: String);
begin
  dmDatabase.RunDeleteStoredProc('usp_SurveyTag_Delete', ['@Key', rowKey]);
  FRefreshSurveysNeeded := True;
end;  // SurveyDeleteRow

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.SurveyUpdateRow(var rowKey: String; data: TStringList);
begin
  if rowKey <> '' then
    dmDatabase.RunUpdateStoredProc('usp_SurveyTag_Update',
        ['@Key', rowKey,
         '@SurveyKey', data.Values['Survey_Key'],
         '@ConceptKey', ConceptKey,
         '@SessionID', AppSettings.SessionID])
  else
    rowKey := dmDatabase.RunInsertStoredProc('Survey_Tag',
        'usp_SurveyTag_Insert',
        ['@SurveyKey', data.Values['Survey_Key'],
         '@ConceptKey', ConceptKey,
         '@SessionID', AppSettings.SessionID],
         '@Key');
  FRefreshSurveysNeeded := True;
end;  // SurveyUpdateRow

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyTagDetails.ValidateSurveys;
var
  i, duplicateRow: Integer;
  key: TKeyString;
  tag: String;

  // remove a row, either because its a duplicate or empty
  procedure DeleteRow(row: Integer);
  begin
    sgSurveys.Row := i;
    FSurveysGridManager.DeleteFromGrid(nil);
  end;

begin
  for i := sgSurveys.Rowcount - 1 downto 1 do
  begin
    if FSurveysGridManager.RowContainsData(i) then begin
      if FSurveysGridManager.CellLinkedKey[0, i] = '' then begin
        // Data present, but no key
        tag := sgSurveys.Cells[0, i];
        dmGeneralData.CheckConcept(ResStr_FindSurvey, ResStr_NoSurveys, ftSurvey, tag, key);
        if key = '' then begin
          // No matched item found
          raise ESurveyTagDetailsError.CreateValidation(ResStr_SpecifyValidSurvey, sgSurveys);
        end else begin
          // Matched a keyword, is it a duplicate?
          duplicateRow := FSurveysGridManager.FindRowByKeyInColumn(key, 0);
          if (duplicateRow = -1) or (FSurveysGridManager.CellLinkedKey[0, sgSurveys.Row] = key) then
            FSurveysGridManager.UpdateLinkedValue(tag, key, -1, i)
          else
            // Remove the duplicate row
            DeleteRow(i);
        end;
      end;
    end else
      // Remove the empty row
      DeleteRow(i);
  end;
  FSurveysGridManager.Validate;
end;  // ValidateSurveys

procedure TfrmSurveyTagDetails.pnlDetailsResize(Sender: TObject);
begin
  inherited;
  pnlInner.Width := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height;
end;

procedure TfrmSurveyTagDetails.eTagSortCodeExit(Sender: TObject);
var
lVal,lErr: Integer;
begin
  inherited;
  if eTagSortCode.Text = '' then eTagSortCode.Text := '0';
  Val(eTagSortCode.Text, lVal, lErr);
  if (lErr <> 0) or (lVal < 0) then
    raise TExceptionPath.CreateValidation(ResStr_Invalid_Sort_Code, eTagSortCode)
end;

procedure TfrmSurveyTagDetails.eTagSortCodeKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if not (Key in [#8, '0'..'9']) then Key := #0;
end;

end.





