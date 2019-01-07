//==============================================================================
//  Unit:        SurveyDetails
//
//  Implements:  TfrmSurveyDetails
//
//  Description: This form is docked on the Observations screen to allow details
//               of a selected survey to be viewed and/or edited.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 183 $
//    $Date: 8/04/10 17:21 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit SurveyDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, StdCtrls, Grids, DBGrids, ExtCtrls, Buttons, Menus, DBCtrls,
  ComCtrls, Mask, VagueDateEdit, BaseChildUnit, CompositeComponent, Sources,
  SurveyData, DataClasses, Db, DropTarget, VagueDate, Constants, JNCCDatasets,
  ExceptionForm, BaseDockedForm, ValidationData, OnlineHelp, SpatialRefFuncs,
  GeneralFunctions, ImageListButton, DatabaseAccessADO, DataStringGrid,
  ControlStringGrid, AddinCompositeComponent, AddinLinkedControls, Map,Variants;

type
  ESurveyDetailsError = class(TExceptionPath);

  TfrmSurveyDetails = class(TfrmBaseDockedForm)
    dlgOpen: TOpenDialog;
    pmMapWindow: TPopupMenu;
    pnlDetails: TPanel;
    pnlInner: TPanel;
    bbCancel: TImageListButton;
    bbSave: TImageListButton;
    pcSurveyDetails: TPageControl;
    tsGeneral: TTabSheet;
    bvlGeneral: TBevel;
    lblSurveyName: TLabel;
    lblType: TLabel;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblSurveyStatus: TLabel;
    lblSurveyRunBy: TLabel;
    lblSurveyMedia: TLabel;
    lblDescription: TLabel;
    Label4: TLabel;
    lblOperatingFrom: TLabel;
    lblOperatingTo: TLabel;
    dbcmbSurveyStatus: TDBLookupComboBox;
    dbcmbSurveyMedia: TDBLookupComboBox;
    dbcmbSurveyType: TDBLookupComboBox;
    dbeSurveyName: TDBEdit;
    dbePeriodicity: TDBEdit;
    dbreDescription: TDBRichEdit;
    eSurveyFrom: TVagueDateEdit;
    eSurveyTo: TVagueDateEdit;
    eSurveyOpFrom: TVagueDateEdit;
    eSurveyOpTo: TVagueDateEdit;
    eSurveyRunBy: TNameLinkedEdit;
    tsGeography: TTabSheet;
    Bevel2: TBevel;
    Label7: TLabel;
    gbSpatialRefCorners: TGroupBox;
    Label5: TLabel;
    Label30: TLabel;
    Label1: TLabel;
    Bevel3: TBevel;
    btnMap: TImageListButton;
    eNECorner: TEdit;
    eSWCorner: TEdit;
    btnMapDropDown: TButton;
    dbreGeoCoverage: TDBRichEdit;
    tsSurveyTags: TTabSheet;
    Shape2: TShape;
    btnAddTag: TImageListButton;
    btnRemoveTag: TImageListButton;
    sgSurveyTags: TControlStringGrid;
    tsSources: TTabSheet;
    Sources: TSources;
    lblRunbyPrompt: TLabel;
    lblRunbyDisp: TLabel;
    lblSurveyNameDisp: TLabel;
    Licence: TTabSheet;
    dbcmbLicence: TDBLookupComboBox;
    lblLicence: TLabel;
    lblAttribution: TLabel;
    dbreAttribution: TDBRichEdit;
    lblNotes: TLabel;
    dbreNotes: TDBRichEdit;
    Label2: TLabel;
    eImportDate: TEdit;
    dbcbTemporary: TDBCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSaveClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure btnMapClick(Sender: TObject);
    procedure EnterRTF(Sender: TObject);
    procedure ExitRTF(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pnlDetailsResize(Sender: TObject);
    procedure eSWCornerExit(Sender: TObject);
    procedure eNECornerExit(Sender: TObject);
    procedure pcSurveyDetailsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dbComboKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dbComboClick(Sender: TObject);
    procedure btnMapDropDownClick(Sender: TObject);
    procedure SurveyDateValidate(Sender: TObject);
    procedure eSurveyRunByFindData(Sender: TObject);
    procedure eSurveyRunByGetData(Sender: TObject);
    procedure tsGeneralResize(Sender: TObject);
    procedure tsSurveyTagsShow(Sender: TObject);
    procedure TagColumnResize(Sender: TObject);
    procedure btnAddTagClick(Sender: TObject);
    procedure btnRemoveTagClick(Sender: TObject);
    procedure eImportDateExit(Sender: TObject);
  private
    FValidEnteredSW : string;
    FValidEnteredNE : String;
    FDrillForm:TBaseForm;
    FdmSurvey :TdmSurvey;
    FSurveyKey: TKeyString;
    SurveyDates: array [1..4] of TVagueDateEdit;
    FKeyDown: Boolean; //says whether a key is currently down on a DBLookupCombo
    FClosingForm : Boolean;
    FSurveyTagsGridManager: TDataStringGrid;
    FRefreshSurveysNeeded: Boolean;
    procedure EnableDetails(const NewMode:Constants.TEditMode);
    procedure SetDrillForm(const Value: TBaseForm);
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure FreeObjects;
    procedure SetupObjects;
    procedure DropRunByName(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    function GetRunByDisp: string;
    procedure UpdateSurveyRunBy(KeyList: TKeyList);
    procedure UpdateBoundingBox(KeyList: TKeyList);
    procedure ValidateNECorner;
    procedure ValidateSWCorner;
    procedure MapForBoundingBoxClick(Sender: TObject);
    procedure SelectAreaOnMap(MapForm: TfrmMap);
    procedure DropSurveyTag(const Sender: TObject; const format: Integer; const sourceData: TKeyList;
        const textStrings: TStringList; var handled: Boolean);
    procedure PopulateSurveyTags;
    procedure SurveyTagFindData(const initialText: String; var text, key: String);
    procedure SurveyTagGetData;
    procedure SurveyTagDeleteRow(rowKey: String);
    procedure SurveyTagUpdateRow(var rowKey: String; data: TStringList);
    procedure UpdateSurveyTag(keylist: TKeylist);
    procedure ValidateSurveyTags;
    procedure ValidateImportDate;
  protected
    procedure SetupDestinationControls; override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetKeyList: TKeyList; override;
    procedure UpdateRTFMenu; override;
    procedure AddRecord;
    procedure EditRecord;
    procedure DisplayRecord(const ASurveyKey:TKeyString);
    procedure DeleteRecord(const ASurveyKey: TKeyString);
    procedure RefreshLists;
    procedure SetDisplaySystem;
    procedure RefreshNames;
    procedure RefreshColours;
    procedure ShowMetadata; override;
    procedure UpdateMapWindowSelector; override;
    property DrillForm:TBaseForm read FDrillForm write SetDrillForm;
    property SurveyKey:TKeyString read FSurveyKey write FSurveyKey;
//    property RunByKey: TKeyString read FRunByKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, EventDetails, Maintbar,
  Find, Observations, MetadataPopup, EnhancedTermLists;

resourcestring
  ResStr_SurveyNameRequired = 'A Survey Name is required for every Survey.';
  ResStr_SurveyorNameRequired = 'A Surveyor''s Name is required for every Survey.';
  ResStr_InvalidSurveyorsName = 'The Surveyor''s Name is invalid. Enter a valid name.';
  ResStr_TempSurveyRequired = 'The survey has unparsed observer names and must be set to temporary.';
  ResStr_InvalidLicence = 'Licence is not valid for a temporary survey';
  ResStr_SurveyTypeRequired = 'A Survey Type is required for every Survey. Select one from the list.';

  ResStr_AllowedToDateRequired =  'A Records Allowed To Date is required when the Survey is completed.' +
                                  ' Enter a date or change the Status value.';

  ResStr_OperatingToDateRequired =  'An Operating To Date is required when the Survey is completed.' +
                                    ' Enter a date or change the Status value.';

  ResStr_GeoInfoRequired =  'Some Geographic Information is required. Enter either both '+
                            'Bounding Box limits or the Geographic Coverage.';

  ResStr_SRNotRecognised =  'The spatial reference is not recognised.';

  ResStr_ThisSurveyTagIsAlreadyInList = 'This survey tag is already present in the list.';
  ResStr_SpecifyValidSurveyTag = 'Please specify a valid survey tag.';
  ResStr_InvalidSurveyTag='The term you have selected is not from the list of tags so cannot be used '+
      'as a survey tag.';
  ResStr_TheImportDate = 'The import date ';


//==============================================================================
constructor TfrmSurveyDetails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DrillForm:=nil;
  SetupObjects;
  EnableDetails(Constants.emView);
  pcSurveyDetails.ActivePage:=tsGeneral;

  //Help Setup
  Self.HelpContext            := IDH_SURVEYS;
  tsGeneral.HelpContext       := IDH_SURVEYSGENERAL;
  tsGeography.HelpContext     := IDH_SURVEYSGEOGRAPHY;
  tsSurveyTags.HelpContext    := IDH_SURVEYTAGS;
  tsSources.HelpContext       := IDH_SURVEYSSOURCES;
  pcSurveyDetails.HelpContext := IDH_SURVEYSGENERAL;

  FKeyDown:=false;
end;  // Create

//==============================================================================
procedure TfrmSurveyDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeObjects;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmSurveyDetails.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if EditMode<>emView then begin
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                FClosingForm:=true;
                bbCancelClick(nil);
                CanClose:=true;
                FClosingForm:=false;
              end;
    end;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmSurveyDetails.SetupObjects;
begin
  // Data Module
  FdmSurvey:=TdmSurvey.Create(nil);

  sgSurveyTags.Cells[0, 0] := ResStr_SurveyTags;
  FSurveyTagsGridManager := TDataStringGrid.Create(sgSurveyTags, FdmSurvey.qrySurveyTags, 'Survey_Tag_Key');
  with FSurveyTagsGridManager do begin
    UserID               := AppSettings.UserID;
    RestrictFullEdit     := AppSettings.RestrictFullEdit;
    SiteID               := AppSettings.SiteID;
    AddOnly              := AppSettings.UserAccessLevel = ualAddOnly;
    SetupLinkedEdit('Concept_Key', 0, SurveyTagFindData, SurveyTagGetData);
    ImageList            := dmFormActions.ilButtons;
    GetButtonImageIndex  := 5;
    OnUpdateRow          := SurveyTagUpdateRow;
    OnDeleteRow          := SurveyTagDeleteRow;
  end;

  // Sources
  Sources.Init(dmDatabase.LocalDatabase, 'Survey_Sources',
               AppSettings.UserAccessLevel, dmGeneralData.GetNextKey,
               RegisterDropComponent, AppSettings.SiteID, AppSettings.ExternalFilePath);
  Sources.OnFindInternal:=dmGeneralData.SourcesFindInternal;
  Sources.OnAddInternal :=dmGeneralData.SourcesAddInternal;
  Sources.OnShowSource :=dmGeneralData.SourcesShowInternal;
end;  // SetupObjects

//==============================================================================
procedure TfrmSurveyDetails.FreeObjects;
begin
  FSurveyTagsGridManager.Free;
  FdmSurvey.Free;
  FdmSurvey:=nil;
end;  // FreeObjects

//==============================================================================
function TfrmSurveyDetails.GetRunByDisp:string;
begin
  if Pos(', ',eSurveyRunBy.Text)>0 then
    Result:=Copy(eSurveyRunBy.Text,Pos(', ',eSurveyRunBy.Text)+2,255)
  else
    Result:=eSurveyRunBy.Text;
end;  // RefershRunByDisp

//==============================================================================
procedure TfrmSurveyDetails.DisplayRecord(const ASurveyKey: TKeyString);
var lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  FSurveyKey   :=ASurveyKey;
  try
    with FdmSurvey do
      with qrySurvey do begin
        Close;
        ResetDBRichEditControls([dbreDescription, dbreGeoCoverage]);
        Parameters.ParamByName('KeyParameter').Value:=ASurveyKey;
        Open;
        FCustodian := FieldByName('Custodian').AsString;
        eSurveyRunBy.Text :=dmGeneralData.GetName(FieldByName('Run_By').AsString);
        eSurveyRunBy.Key  := FieldByName('Run_By').AsString;
        lblSurveyNameDisp.Caption:=GetTextWithinLimit(Canvas,DuplicateCharacters(dbeSurveyName.Text, Char('&')),165);
        lblRunByDisp.Caption:=DuplicateCharacters(GetTextWithinLimit(Canvas,GetRunByDisp,
                                                 pnlDetails.Width-lblRunByDisp.Left-8), '&');

        eSurveyFrom.Text   := FieldByName('From_Vague_Date_Start').Text;
        eSurveyTo.Text     := FieldByName('To_Vague_Date_Start').Text;
        eSurveyOpFrom.Text := FieldByName('Op_From_Vague_Date_Start').Text;
        eSurveyOpTo.Text   := FieldByName('Op_To_Vague_Date_Start').Text;
        eImportDate.Text           := FieldByName('Import_Date').Text;
        FValidEnteredSW := FieldByName('SW_Spatial_Ref').AsString;
        eSWCorner.Text := LocaliseSpatialRef(GetDisplaySpatialRef(
            AppSettings.SpatialRefSystem,
            FValidEnteredSW,
            FieldByName('Spatial_Ref_System').AsString,
            FieldByName('SW_Lat').Value,
            FieldByName('SW_Long').Value,
            ''));
        FValidEnteredNE := FieldByName('NE_Spatial_Ref').AsString;
        eNECorner.Text := LocaliseSpatialRef(GetDisplaySpatialRef(
            AppSettings.SpatialRefSystem,
            FValidEnteredNE,
            FieldByName('Spatial_Ref_System').AsString,
            FieldByName('NE_Lat').Value,
            FieldByName('NE_Long').Value,
            ''));
      end;
    PopulateSurveyTags;
    FRefreshSurveysNeeded := False;
    // Sources
    Sources.SourcesFromKey:=ASurveyKey;
    Sources.RefreshLists;
    // Additional Pages
    ChangeAdditionalPage(pcSurveyDetails);
    // Notify COM Addins
    NotifyDataItemChange;

    if pcSurveyDetails.ActivePage = tsSurveyTags then
      with sgSurveyTags do
        if ClientWidth > 0 then
          ColWidths[0] := ClientWidth;
  finally
    DefaultCursor(lCursor);
  end;
end;  // DisplayRecord

//==============================================================================
procedure TfrmSurveyDetails.AddRecord;
begin
  pcSurveyDetails.ActivePage := tsGeneral;
  // Get next sequential key
  SurveyKey := dmGeneralData.GetNextKey('Survey','Survey_Key');
  FCustodian := AppSettings.SiteID;
  with FdmSurvey.qrySurvey do begin
    Append;
    dbcmbSurveyStatus.KeyValue:=NONE_RECORD_KEY;
    FieldByName('Survey_Status_Key').AsString:=NONE_RECORD_KEY;
    dbcmbSurveyMedia.KeyValue:=NONE_RECORD_KEY;
    FieldByName('Survey_Media_Key').AsString:=NONE_RECORD_KEY;
    dbcmbLicence.KeyValue:=NONE_RECORD_KEY;
    FieldByName('Licence_Key').AsString:=NONE_RECORD_KEY;
    FieldByName('Temporary_Survey').AsBoolean:= false;
  end;
  ResetDBRichEditControls([dbreDescription, dbreGeoCoverage]);
  dmGeneralData.SetNameIDAndDate(FdmSurvey.qrySurvey,'Entered_By','Entry_Date');
  eSurveyRunBy.Text   :='';
  lblRunByDisp.Caption:='';
  // Sources
  Sources.SourcesFromKey:=SurveyKey;
  Sources.RefreshLists;
  FValidEnteredSW := '';
  FValidEnteredNE := '';
  eSWCorner.Text := '';
  eNECorner.Text := '';
  { Add COM addin page }
  AddAdditionalPages;
  EnableDetails(Constants.emAdd);
end;  // AddRecord

//==============================================================================
procedure TfrmSurveyDetails.DeleteRecord(const ASurveyKey:TKeyString);
begin
  // Remove dependencies BEFORE the main record. Doh!
  dmDatabase.RunStoredProc('usp_SurveyTag_Delete_ForSurvey', ['@SurveyKey', ASurveyKey]);
  FdmSurvey.DeleteRecord(ASurveyKey);
  DeleteAdditionalPages;
end;  // DeleteRecord

//==============================================================================
{ Display a form to edit the selected survey }
procedure TfrmSurveyDetails.EditRecord;
begin
  DisplayRecord(SurveyKey);
  if dmGeneralData.HasFullEditAccess('Survey', 'Survey_Key', SurveyKey) and
      HaveCustody then
    try
      FdmSurvey.qrySurvey.Edit;
      dmGeneralData.SetNameIDAndDate(FdmSurvey.qrySurvey,'Changed_By','Changed_Date');
      EnableDetails(Constants.emEdit);
    except
      on E:Exception do
        if dmDatabase.CheckError(E, dbeRecordLocked) then begin
          MessageDlg(ResStr_CannotEditRecord + #13#13 +
                     dmDatabase.GetErrorMessage(E.Message, dbeRecordLocked),
                     mtInformation, [mbOk], 0);
          TfrmObservations(DrillForm).tvObservations.SetFocus;
        end else
          Raise;
    end  // Try...Except
  else
    EnableDetails(Constants.emEdit);
end;  // EditRecord

//==============================================================================
procedure TfrmSurveyDetails.bbSaveClick(Sender: TObject);
var
  lCurrentTab      : TTabSheet;
  lValidSR         : TValidBoundingBox;
  lCursor          : TCursor;
  lCustodian       : String;
 begin
  inherited;

  lCurrentTab:=pcSurveyDetails.ActivePage;
  // Sender is nil if called from CloseQuery method.
  if Sender=nil then begin
    if eNECorner.Focused or eSWCorner.Focused then begin
      ValidateNECorner;
      ValidateSWCorner;
    end;
    pcSurveyDetails.ActivePage:=tsGeneral;
  end else
    pcSurveyDetails.ActivePage:=tsGeneral;

  ValidateValue(dbeSurveyName.Text<>'',ResStr_SurveyNameRequired,dbeSurveyName);
  ValidateValue(eSurveyRunBy.Text<>'',ResStr_SurveyorNameRequired,eSurveyRunBy);
  ValidateValue(dmGeneralData.CheckName(eSurveyRunBy),ResStr_InvalidSurveyorsName,eSurveyRunBy);
  ValidateValue(dbcmbSurveyType.Text<>'',ResStr_SurveyTypeRequired,dbcmbSurveyType);
  if dbcbTemporary.Checked then begin
    ValidateValue(dmGeneralData.CheckLicence(vartostr(dbcmbLicence.KeyValue)),ResStr_InvalidLicence);
  end else
    ValidateValue(dmGeneralData.CheckTempSurvey(SurveyKey),ResStr_TempSurveyRequired);

  SurveyDateValidate(nil);

  ValidateValue(not ((Pos('COMPLETE',UpperCase(dbcmbSurveyStatus.Text))>0) and (eSurveyTo.Text='')),
                ResStr_AllowedToDateRequired,eSurveyTo);
  ValidateValue(not ((Pos('COMPLETE',UpperCase(dbcmbSurveyStatus.Text))>0) and (eSurveyOpTo.Text='')),
                ResStr_OperatingToDateRequired,eSurveyOpTo);
  pcSurveyDetails.ActivePage:=tsGeography;

  { Validate Bounding Box }
  if (FValidEnteredSW + FValidEnteredNE) <> '' then
  begin
    lValidSR := CheckBoundingBox(FValidEnteredSW, FValidEnteredNE,
                                  DetermineSpatialRefSystem(FValidEnteredSW));
    if not lValidSR.Valid then
      raise ESurveyDetailsError.CreateValidation(lValidSR.Error, eSWCorner);
    //  ValidateValue(lValidSR.Valid, lValidSR.Error);
  end;
  pcSurveyDetails.ActivePage:=tsSurveyTags;
  ValidateSurveyTags;

  { Call to validate COM addin page... }
  if not CheckAdditionalPageCanSave then
    Exit;
  pcSurveyDetails.ActivePage:=lCurrentTab;

  lCursor:=HourglassCursor;
  try
    lCustodian := dmGeneralData.Custodian('Survey', 'Survey_Key', SurveyKey);
    if FdmSurvey.qrySurvey.State in [dsEdit, dsInsert] then
    begin
      with FdmSurvey.qrySurvey do begin
        FieldByName('Run_By').AsString:=eSurveyRunBy.Key;
        FieldByName('From_Vague_Date_Start').Text    := eSurveyFrom.Text;
        FieldByName('To_Vague_Date_Start').Text      := eSurveyTo.Text;
        FieldByName('Op_From_Vague_Date_Start').Text := eSurveyOpFrom.Text;
        FieldByName('Op_To_Vague_Date_Start').Text   := eSurveyOpTo.Text;
        FieldByName('Import_Date').Text   := eImportDate.Text;
        FieldByName('Temporary_Survey').AsBoolean := dbcbTemporary.checked;
        if EditMode = emAdd then
        begin
          FieldByName('Survey_Key').AsString := SurveyKey;
          FieldByName('Spatial_Ref_System').AsString := AppSettings.SpatialRefSystem;
        end;
        FieldByName('NE_SPATIAL_REF').AsString := FValidEnteredNE;
        FieldByName('SW_SPATIAL_REF').AsString := FValidEnteredSW;
        Post;
      end;
    end else
    if (EditMode = Constants.emEdit) and (lCustodian <> FCustodian) then begin
      FdmSurvey.qrySurvey.Cancel;
      MessageDlg(Format(ResStr_CustodyChanged, ['Survey']), mtWarning, [mbOk], 0);
    end;

    FSurveyTagsGridManager.Save;

    // Sources
    Sources.Post;
    // Additional Pages
    SaveAdditionalPages;

    if DrillForm<>nil then
      with TfrmObservations(DrillForm) do begin
        if FRefreshSurveysNeeded and AppSettings.OrganiseSurveysByTag then begin
          SurveyToReselect := SurveyKey;
          PostMessage(Handle, WM_REFRESH_OBSERVATIONS, pcSurveyDetails.ActivePageIndex, 0);
        end else begin
          SetItemTextAndKey(dbeSurveyName.Text + ' - ' + GetRunByDisp, SurveyKey);
          tvObservations.Selected := SelectedItem;
        end;
      end;

    EnableDetails(Constants.emView);
    DisplayRecord(SurveyKey);
  finally
    //Send message to all open forms that current survey has changed
    frmMain.BroadcastMessage(WM_UPDATE_SURVEY_COMBO);
    DefaultCursor(lCursor);
  end;
end;  // bbSaveClick

//==============================================================================
procedure TfrmSurveyDetails.bbCancelClick(Sender: TObject);
var tfDiscardNew:boolean;
begin
  inherited;
  tfDiscardNew:=FdmSurvey.qrySurvey.State=dsInsert;
  FdmSurvey.qrySurvey.Cancel;
  // Additional Pages
  CancelAdditionalPages;
  EnableDetails(Constants.emView);

  // If closing form, no need to do anything further. Otherwise, refresh screens.
  if not FClosingForm then
    if tfDiscardNew and (DrillForm<>nil) then
      PostMessage(TfrmObservations(DrillForm).Handle,WM_Discard_Observation,0,0)
    else begin
      with TfrmObservations(DrillForm) do
        tvObservations.Selected:=SelectedItem;
      DisplayRecord(SurveyKey);
    end;
end;  // bbCancelClick

//==============================================================================
procedure TfrmSurveyDetails.EnableDetails(const NewMode:Constants.TEditMode);
var tfOn:boolean;
begin
  FEditMode:=NewMode;
  tfOn:=EditMode<>emView;

  RefreshColours;

  eSurveyFrom.ReadOnly   := not (FdmSurvey.qrySurvey.State in [dsEdit,dsInsert]);
  eSurveyTo.ReadOnly     := eSurveyFrom.ReadOnly;
  eSurveyOpFrom.ReadOnly := eSurveyFrom.ReadOnly;
  eSurveyOpTo.ReadOnly   := eSurveyFrom.ReadOnly;
  eNECorner.ReadOnly     := eSurveyFrom.ReadOnly;
  eSWCorner.ReadOnly     := eSurveyFrom.ReadOnly;
  eImportDate.ReadOnly   := eSurveyFrom.ReadOnly;
  if eSurveyFrom.ReadOnly then
    eSurveyRunBy.EditMode := emBrowse
  else
    eSurveyRunBy.EditMode := emEdit;

  UpdateMapWindowSelector;

  FSurveyTagsGridManager.Enabled := tfOn;
  btnAddTag.Enabled := tfOn;
  btnRemoveTag.Enabled := tfOn;
  sgSurveyTags.Refresh;

  Sources.EditMode       := NewMode;

  bbSave.Enabled  :=tfOn;
  bbCancel.Enabled:=tfOn;

  // Additional Pages
  SetAdditionalPagesState(tfOn);

  if DrillForm<>nil then TfrmObservations(DrillForm).SetMenuState(not tfOn);
end;  // EnableDetails

//==============================================================================
procedure TfrmSurveyDetails.UpdateSurveyRunBy(KeyList:TKeyList);
begin
  if (FdmSurvey.qrySurvey.State in [dsEdit,dsInsert]) then
    try
      if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then begin
        eSurveyRunBy.Text:=dmGeneralData.GetName(KeyList.Items[0].KeyField1);
        eSurveyRunBy.Key:=KeyList.Items[0].KeyField1;
      end;
    finally
      KeyList.Free
    end;
end;  // UpdateSurveyRunBy

//==============================================================================
procedure TfrmSurveyDetails.btnMapClick(Sender: TObject);
begin
  inherited;
  dmFormActions.actMapWindow.Execute;
  SelectAreaOnMap(TfrmMap(frmMain.ActiveMDIChild));
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.btnMapDropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  inherited;
  lPos := btnMap.ClientToScreen(Point(0, btnMap.Height));
  pmMapWindow.Popup(lPos.X, lPos.Y);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.MapForBoundingBoxClick(Sender: TObject);
begin
  dmFormActions.MapWindowMenuClick(Sender);
  SelectAreaOnMap(TfrmMap(frmMain.ActiveMDIChild));
end;

{-------------------------------------------------------------------------------
  Selects the area on the given map that is specified in the text boxes
  defining the bounding box, and sets up the return link so that the text boxes
  are updated if the bounding box is modified using the map.
}
procedure TfrmSurveyDetails.SelectAreaOnMap(MapForm: TfrmMap);

  function LatLong(Edit: TEdit): TLatLong;
  begin
    Result := ConvertToLatLong(
        DelocaliseSpatialRef(Edit.Text),
        AppSettings.SpatialRefSystem);
  end;

begin
  if (eSWCorner.Text = '') or (eNECorner.Text = '') then
    MapForm.SelectArea
  else
  begin
    try
      MapForm.SelectArea(LatLong(eSWCorner), LatLong(eNECorner));
    except
      MapForm.SelectArea;
    end;
  end;

  SetupLink(MapForm, Self, UpdateBoundingBox);
end;

//==============================================================================
procedure TfrmSurveyDetails.UpdateBoundingBox(KeyList:TKeyList);
begin
  try
    if (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
      if KeyList.Header.TableName = 'SPATIAL_REF' then
      begin
        eNECorner.Text  := LocaliseSpatialRef(KeyList.Items[0].KeyField2);
        FValidEnteredNE := KeyList.Items[0].KeyField2;
        eSWCorner.Text  := LocaliseSpatialRef(KeyList.Items[1].KeyField2);
        FValidEnteredSW := KeyList.Items[1].KeyField2;
      end;
  finally
    KeyList.Free;
  end;
end;  // UpdateBoundingBox

//==============================================================================
procedure TfrmSurveyDetails.SetDrillForm(const Value: TBaseForm);
begin
  FDrillForm := Value;
end;  // SetDrillForm

//==============================================================================
procedure TfrmSurveyDetails.WMTransferDone(var Msg: TMessage);
begin
  if DrillForm<>nil then TfrmObservations(DrillForm).Show;
end;  // WMTransferDone

//==============================================================================
procedure TfrmSurveyDetails.UpdateRTFMenu;
begin
  if Assigned(DrillForm) then
    dmFormActions.UpdateRTFMenu((DrillForm.ActiveControl is TDBRichEdit) and
                              (FdmSurvey.qrySurvey.State in [dsEdit,dsInsert]));
end;  // UpdateRTFMenu

//==============================================================================
procedure TfrmSurveyDetails.EnterRTF(Sender: TObject);
begin
  inherited;
  dmFormActions.UpdateRTFMenu(FdmSurvey.qrySurvey.State in [dsEdit,dsInsert]);
end;  // EnterRTF

//==============================================================================
procedure TfrmSurveyDetails.ExitRTF(Sender: TObject);
begin
  inherited;
  dmformActions.UpdateRTFMenu(false);
end;  // ExitRTF

//==============================================================================
procedure TfrmSurveyDetails.pnlDetailsResize(Sender: TObject);
var
  deltaWidth, deltaHeight: Integer;
begin
  inherited;
  lblRunByDisp.Caption := GetTextWithinLimit(
      Canvas,
      GetRunByDisp,
      pnlDetails.Width - lblRunByDisp.Left - 8);

  // Works out the change in height and width of the panel.
  deltaWidth  := pnlDetails.Width - pnlInner.Width;
  deltaHeight := pnlDetails.Height - pnlInner.Height;

  // For some reason, the automatic resizing doesn't work properly. Using a
  // second panel within the main panel and sizing it manually solves this, but
  // is a bit of a hack.
  pnlInner.Width  := pnlDetails.Width;
  pnlInner.Height := pnlDetails.Height;

  // Custom controls do not have anchor properties and must be resized manually.
  eSurveyRunBy.Width := eSurveyRunBy.Width + deltaWidth;
  Sources.Width      := Sources.Width + deltaWidth;
  Sources.Height     := Sources.Height + deltaHeight;
end;  // pnlSurveyDetailsResize

//==============================================================================
procedure TfrmSurveyDetails.SetupDestinationControls;
begin
  RegisterDropComponent(eSurveyRunBy,DropRunByName,
                        [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION],
                        [CF_JNCCDATA, CF_TEXT]);
  RegisterDropComponent(sgSurveyTags, DropSurveyTag, [TN_CONCEPT], [CF_JNCCDATA, CF_TEXT]);
end;  // SetupDestinationControls

//==============================================================================
procedure TfrmSurveyDetails.DropRunByName(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if (EditMode <> emView) and (FdmSurvey.qrySurvey.State in [dsEdit, dsInsert]) then
    ioHandled := dmGeneralData.DropLinkedEditText(
        eSurveyRunBy,
        iFormat,
        iSourceData,
        dmGeneralData.GetName,
        iTextStrings)
  else
    ioHandled := True;
end;  // DropRunByName

//==============================================================================
function TfrmSurveyDetails.GetKeyList: TKeyList;
var
  lNewKeyList: TEditableKeyList;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList := TEditableKeyList.Create;
  lNewKeyList.SetTable(TN_SURVEY);
 	lNewKeyList.AddItem(SurveyKey, '');
  Result := lNewKeyList;
end;  // GetKeyList

//==============================================================================
procedure TfrmSurveyDetails.RefreshLists;
begin
  // Have to call queries active true and false or else dbcomboboxes do not repopulate
  with FdmSurvey do begin
    qrySurveyType.Active   := False;
    qrySurveyType.Active   := True;
    qrySurveyStatus.Active := False;
    qrySurveyStatus.Active := True;
    qrySurveyMedia.Active  := False;
    qrySurveyMedia.Active  := True;
    qrySurveyLicence.Active  := False;
    qrySurveyLicence.Active  := True;
  end;
end;  // RefreshLists

//==============================================================================
//==============================================================================
// On Exit - if there is anything in the edit box then validate it
procedure TfrmSurveyDetails.eNECornerExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else if ((not FClosingForm) and (FEditMode<>emView)) then
    ValidateNECorner;
end;  // eNECornerExit

procedure TfrmSurveyDetails.ValidateNECorner;
begin
  ValidateValue(
      ValidSpatialRef(
          DelocaliseSpatialRef(eNECorner.Text),
          AppSettings.SpatialRefSystem).Valid,
      ResStr_SRNotRecognised,eNECorner);
  ValidateSpatialRefEntry(eNECorner, AppSettings.SpatialRefSystem, FValidEnteredNE);
end;  // ValidateNECorner

//==============================================================================
// On Exit
{ if there is anything in the edit box then validate it }
procedure TfrmSurveyDetails.eSWCornerExit(Sender: TObject);
var lCancelPos:TPoint;
begin
  inherited;
  lCancelPos:=bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lCancelPos.X in [0..bbCancel.Width]) and (lCancelPos.Y in [0..bbCancel.Height]) then
    bbCancelClick(nil)
  else if ((not FClosingForm) and (FEditMode<>emView)) then
    ValidateSWCorner;
end;  // eSWCornerExit

procedure TfrmSurveyDetails.ValidateSWCorner;
begin
  ValidateValue(
      ValidSpatialRef(
          DelocaliseSpatialRef(eSWCorner.Text),
          AppSettings.SpatialRefSystem).Valid,
      ResStr_SRNotRecognised,eSWCorner);
  ValidateSpatialRefEntry(eSWCorner, AppSettings.SpatialRefSystem, FValidEnteredSW);
end;  // ValidateSWCorner

//==============================================================================
procedure TfrmSurveyDetails.SetDisplaySystem;
begin
  ValidateSpatialRefEntry(eNECorner, AppSettings.SpatialRefSystem, FValidEnteredNE);
  ValidateSpatialRefEntry(eSWCorner, AppSettings.SpatialRefSystem, FValidEnteredSW);
end;

//==============================================================================
procedure TfrmSurveyDetails.pcSurveyDetailsChange(Sender: TObject);
begin
  inherited;
  pcSurveyDetails.HelpContext := pcSurveyDetails.ActivePage.HelpContext;
end;

//==============================================================================
procedure TfrmSurveyDetails.RefreshNames;
begin
  eSurveyRunBy.Text:=dmGeneralData.GetName(eSurveyRunBy.Key);
  with FdmSurvey.qrySurvey do
    lblRunByDisp.Caption:=GetTextWithinLimit(Canvas,GetRunByDisp,
                                             pnlDetails.Width-lblRunByDisp.Left-8);
end;

//==============================================================================
procedure TfrmSurveyDetails.RefreshColours;
begin
  SetRequiredFieldsColourState(EditMode<>emView,
                               [dbeSurveyName, eSurveyRunBy, dbcmbSurveyType, eSurveyFrom]);
end;

//==============================================================================
procedure TfrmSurveyDetails.ShowMetadata;
begin
  with TdlgMetaDataPopup.Create(nil) do
  try
    ShowStandard(ResStr_Survey, lblSurveyNameDisp.Caption,
                 FdmSurvey.qrySurvey.FieldByName('Survey_Key').AsString,
                 FdmSurvey.qrySurvey);
  finally
    Free;
  end;
end;

procedure TfrmSurveyDetails.FormCreate(Sender: TObject);
begin
  inherited;
  //Need this array for the validation procedures
  SurveyDates[1] := eSurveyFrom;
  SurveyDates[2] := eSurveyTo;
  SurveyDates[3] := eSurveyOpFrom;
  SurveyDates[4] := eSurveyOpTo;
end;

//==============================================================================
//These functions control whether BaseChild warns the user that the data
//in the combo boxes is not
//editable because the data is from another site.
procedure TfrmSurveyDetails.dbComboKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=true;
end;

procedure TfrmSurveyDetails.dbComboKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKeyDown :=false;
end;

procedure TfrmSurveyDetails.dbComboClick(Sender: TObject);
begin
  inherited;
  //If this came from a key then no need to send the message.
  //Otherwise pretend a key has been pressed.
  if not FKeyDown then
  begin
    PostMessage(TWinControl(Sender).Handle, WM_KEYDOWN, VK_Return,0);
    FKeyDown:=false;
  end;
end;

//==============================================================================
procedure TfrmSurveyDetails.UpdateMapWindowSelector;
begin
  AppSettings.UpdateMapMenu(Self, pmMapWindow.Items, True, MapForBoundingBoxClick);
  btnMap.Enabled         := (EditMode <> emView) and (AppSettings.AvailableMaps.Count > 0);
  btnMapDropDown.Enabled := btnMap.Enabled;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.SurveyDateValidate(Sender: TObject);
var
  lPos: TPoint;
  lResult: TValidationResult;
begin
  inherited;
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height])) or FClosingForm) then
  begin
    lResult := ValidateSurveyDates(eSurveyFrom.Text, eSurveyTo.Text,
                                   eSurveyOpFrom.Text, eSurveyOpTo.Text);
    ValidateValue(lResult.Success, lResult.Message, SurveyDates[lResult.FailParamIndx]);

    if Sender is TVagueDateEdit then
      TVagueDateEdit(Sender).Text := VagueDateToString(TVagueDateEdit(Sender).VagueDate);
  end;
end;
{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.DropSurveyTag(const Sender: TObject; const format: Integer;
    const sourceData: TKeyList; const textStrings: TStringList; var handled: Boolean);
var
  existingRow: Integer;
begin
  if FSurveyTagsGridManager.Enabled then
    if SameText(sourceData.Header.TableName, TN_CONCEPT) and (sourceData.Header.ItemCount > 0) then
    begin
      existingRow := FSurveyTagsGridManager.FindRowByKeyInColumn(sourceData.Items[0].KeyField1, 0);
      if existingRow <> -1 then
        sgSurveyTags.Row := existingRow
      else begin
        with dmDatabase.GetRecordset(
            'usp_Concept_Select',
            ['@ConceptKey', sourceData.Items[0].KeyField1]) do
        begin
          if Fields['Concept_Group_Key'].Value = ACG_SURVEY_TAGS then begin
            FSurveyTagsGridManager.AddToGrid(nil);
            FSurveyTagsGridManager.UpdateLinkedValue(
                Fields['Item_Name'].Value,
                sourceData.Items[0].KeyField1);
          end else
            //display warning message
            MessageDlg(ResStr_InvalidSurveyTag, mtWarning, [mbOk], 0);
        end;
      end;
    end;
end;  // DropSurveyTag

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.PopulateSurveyTags;
begin
  FdmSurvey.qrySurveyTags.Close;
  FdmSurvey.qrySurveyTags.Parameters.ParamByName('@Key').Value := SurveyKey;
  FdmSurvey.qrySurveyTags.Open;
  FSurveyTagsGridManager.PopulateGrid;
  FSurveyTagsGridManager.Refresh;
end;  // PopulateSurveyTags

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.SurveyTagFindData(const initialText: String; var text, key: String);
var
  tmpKey: TKeyString;
  duplicateRow: Integer;
begin
  text := initialText;
  dmGeneralData.CheckConcept(ResStr_FindSurveyTag, ResStr_NoSurveyTags, ftSurveyTag, text, tmpKey);
  duplicateRow := FSurveyTagsGridManager.FindRowByKeyInColumn(tmpKey, 0);
  if (duplicateRow = -1) or (FSurveyTagsGridManager.CellLinkedKey[0, sgSurveyTags.Row] = tmpKey) then
    key := tmpKey
  else begin
    key := '';
    text := '';
    ShowInformation(ResStr_ThisSurveyTagIsAlreadyInList);
  end;
end;  // SurveyTagFindData

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.SurveyTagGetData;
begin
  dmFormActions.actEnhancedTermlists.Execute;
  TfrmEnhancedTermLists(frmMain.ActiveMDIChild).SelectConceptGroup(ACG_SURVEY_TAGS, ResStr_SurveyTags);
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateSurveyTag);
end;  // SurveyTagGetData

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.SurveyTagDeleteRow(rowKey: String);
begin
  dmDatabase.RunDeleteStoredProc('usp_SurveyTag_Delete', ['@Key', rowKey]);
  FRefreshSurveysNeeded := True;
end;  // SurveyTagDeleteRow

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.SurveyTagUpdateRow(var rowKey: String; data: TStringList);
begin
  if rowKey<>'' then
    dmDatabase.RunUpdateStoredProc('usp_SurveyTag_Update',
        ['@Key', rowKey,
         '@SurveyKey', SurveyKey,
         '@ConceptKey', data.Values['Concept_Key'],
         '@SessionID', AppSettings.SessionID])
  else
    rowKey := dmDatabase.RunInsertStoredProc('Survey_Tag',
        'usp_SurveyTag_Insert',
        ['@SurveyKey', SurveyKey,
         '@ConceptKey', data.Values['Concept_Key'],
         '@SessionID', AppSettings.SessionID],
         '@Key');
  FRefreshSurveysNeeded := True;
end;  // SurveyTagUpdateRow

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.UpdateSurveyTag(keylist: TKeylist);
begin
  try
    if Keylist.Header.ItemCount = 1 then
      if SameText(Keylist.Header.TableName, TN_CONCEPT) then
        // No duplicates.
        if FSurveyTagsGridManager.FindRowByKeyInColumn(Keylist.Items[0].KeyField1, 0) = -1 then
          with dmDatabase.GetRecordset('usp_Concept_Select',['@ConceptKey', Keylist.Items[0].KeyField1]) do begin
            if Fields['Concept_Group_Key'].Value = ACG_SURVEY_TAGS then begin
              FSurveyTagsGridManager.AddToGrid(nil);
              FSurveyTagsGridManager.UpdateLinkedValue(Fields['Item_Name'].Value,Keylist.Items[0].KeyField1)
              end
            else
                //display warning message
                MessageDlg(ResStr_InvalidSurveyTag, mtWarning, [mbOk], 0);
            end
        else
          ShowInformation(ResStr_ThisSurveyTagIsAlreadyInList);
  finally
    Keylist.Free;
  end;
end;  // UpdateSurveyTag

{-------------------------------------------------------------------------------
}
procedure TfrmSurveyDetails.ValidateSurveyTags;
var
  i, duplicateRow: Integer;
  key: TKeyString;
  tag: String;

    // remove a row, either because its a duplicate or empty
    procedure DeleteRow(row: Integer);
    begin
      sgSurveyTags.Row := i;
      FSurveyTagsGridManager.DeleteFromGrid(nil);
    end;

begin
  for i := sgSurveyTags.Rowcount - 1 downto 1 do
  begin
    if FSurveyTagsGridManager.RowContainsData(i) then begin
      if FSurveyTagsGridManager.CellLinkedKey[0, i] = '' then begin
        // Data present, but no key
        tag := sgSurveyTags.Cells[0, i];
        dmGeneralData.CheckConcept(ResStr_FindSurveyTag, ResStr_NoSurveyTags, ftSurveyTag, tag, key);
        if key = '' then begin
          // No matched item found
         // pcSurveyDetails.ActivePage := tsSurveyTags;
          raise ESurveyDetailsError.CreateValidation(ResStr_SpecifyValidSurveyTag, sgSurveyTags);
        end else begin
          // Matched a keyword, is it a duplicate?
          duplicateRow := FSurveyTagsGridManager.FindRowByKeyInColumn(key, 0);
          if (duplicateRow = -1) or (FSurveyTagsGridManager.CellLinkedKey[0, sgSurveyTags.Row] = key) then
            FSurveyTagsGridManager.UpdateLinkedValue(tag, key, -1, i)
          else
            // Remove the duplicate row
            DeleteRow(i);
        end;
      end;
    end else
      // Remove the empty row
      DeleteRow(i);
  end;
  FSurveyTagsGridManager.Validate;
end;  // ValidateSurveyTags

{-------------------------------------------------------------------------------
  Find data setup for Survey Run By
}
procedure TfrmSurveyDetails.eSurveyRunByFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckName(eSurveyRunBy);
end;

{-------------------------------------------------------------------------------
    F9 return data setup for Survey Run By
}
procedure TfrmSurveyDetails.eSurveyRunByGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateSurveyRunBy);
end;

{-------------------------------------------------------------------------------
   When this tab page is resized, resizes its contents to match.
}
procedure TfrmSurveyDetails.tsGeneralResize(Sender: TObject);
var
  width: Integer;
  halfWidth: Integer;
begin
  inherited;
  width := bvlGeneral.Width
            - dbcmbSurveyStatus.Left + bvlGeneral.Left
            - lblSurveyMedia.Width
            - 4 * 2 // Internal Spacing
            - 8;    // Spacing within bevel;
  halfWidth := width div 2;
  dbcmbSurveyStatus.Width := halfWidth;
  lblSurveyMedia.Left := dbcmbSurveyStatus.Left + dbcmbSurveyStatus.Width + 4;
  dbcmbSurveyMedia.Left := lblSurveyMedia.Left + lblSurveyMedia.Width + 4;
  dbcmbSurveyMedia.Width := halfWidth;
end;

{-------------------------------------------------------------------------------
  When the survey tags page is shown, resize the column to fit.
}
procedure TfrmSurveyDetails.tsSurveyTagsShow(Sender: TObject);
begin
  inherited;
  with sgSurveyTags do
    ColWidths[0] := ClientWidth;
end;

{-------------------------------------------------------------------------------
  Resize the tag column to fit the grid.
}
procedure TfrmSurveyDetails.TagColumnResize(Sender: TObject);
begin
  inherited;

  if pcSurveyDetails.ActivePage = tsSurveyTags then
    with sgSurveyTags do
      ColWidths[0] := ClientWidth;
end;

{-------------------------------------------------------------------------------
  When the user clicks to add a tag, throw the event on the grid manager to add
  a new tag, and resize the grid column to fit (since it may need to make room for
  a scroll bar).
}
procedure TfrmSurveyDetails.btnAddTagClick(Sender: TObject);
begin
  inherited;
  FSurveyTagsGridManager.AddToGrid(nil);
  TagColumnResize(nil);
end;

{-------------------------------------------------------------------------------
  When the user clicks to delete a tag, throw the event on the grid manager to delete
  a tag, and resize the grid column to fit (since it may have more room if the scroll
  bar has gone).
}
procedure TfrmSurveyDetails.btnRemoveTagClick(Sender: TObject);
begin
  inherited;
  FSurveyTagsGridManager.DeleteFromGrid(nil);
  TagColumnResize(nil);
end;

procedure TfrmSurveyDetails.ValidateImportDate;
begin
  if eImportDate.Text<>'' then begin
    try
      // Use VagueDate functions to deal with other date separators, like "."
      // If conversion fails, not a valid date and trapped after handler
      eImportDate.Text := VagueDateToString(StringToVagueDate(eImportDate.Text));
    except
    end;
    // And validate
    ValidateValue(IsDate(eImportDate.Text) and (StrToDate(eImportDate.Text)<=Date),
                  InvalidDate(ResStr_TheImportDate,false,false),eImportDate);
    eImportDate.Text:=DateToStr(StrToDate(eImportDate.Text));
  end;
end;  // eImportDateExit


procedure TfrmSurveyDetails.eImportDateExit(Sender: TObject);
var
lPos: TPoint;
begin
  inherited;
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if not (((lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height])) or FClosingForm) then
    ValidateImportDate;
end;

end.
