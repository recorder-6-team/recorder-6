//==============================================================================
//  Unit: FilterManager
//
//  Description: Allows the user to manage Export Filters.  Includes
//  functionality for adding, editing, deleting, saving and loading.
//
//  Author: Michael Bailey
//  Created: Nov 2002
//
//  Last Revision Details:
//    $Revision: 25 $
//    $Date: 26/05/08 13:57 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit FilterManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FormActions, ImageListButton, Constants,
  ExportFilters, ComCtrls, Mask, BaseChildUnit, DataClasses, BaseFormUnit,
  GeneralFunctions, Map;

resourcestring
  ResStr_FilterOpenFailed = 'An error occurred while trying to open the Export Filter.  '+
      'Error is described as:'#13#10'%s';
  ResStr_TheExportFilter = 'the selected Export Filter';
  ResStr_SelectSurvey = 'Select Survey';
  ResStr_SelectTaxa = 'Select Taxa';
  ResStr_UnrecognisedSpatialRef = 'The spatial reference is not recognised.';
  ResStr_ExportFilterExists = 'The Export Filter already exists in the database.  Overwrite?';
  ResStr_CannotDeleteFilter = 'Export Filter cannot be deleted while there are related ' +
                              'Recording Schemes in the database.';
  ResStr_ProvideFilterName = 'Please provide a name for your filter.';
  ResStr_InvalidStartDate =  'Please Enter a valid Start Date.';
  ResStr_InvalidEndDate = 'Please Enter a valid End Date.';
  ResStr_SaveChanges =  'Save changes before closing?';
  ResStr_AllFilterSurvey =  'Filter on all Surveys';
  ResStr_AllFilterSurveyTag = 'Filter on all Survey Tags';
  ResStr_TaxonGroupFilter = 'Filter on all Taxa';

type
  { Information on the TListBox controls.
    Each List Box stores objects associated with each string in the items list.
    lbFilters stores a TExportFilter object, lbFilterSurveys stores a TKey
    object & lbFilterTaxa stores a TTaxonNames object.  It is important to note
    that in the latter two cases the objects are identical copies of the objects
    in the corresponding TExportFilter object.  They do not instead store a
    reference to the objects in the TExportFilter object because the two lists
    will only coincide when in View Mode.  Believe me it's easier that way. }
  TfrmFilterManager = class(TBaseChild)
    gbFilterList: TGroupBox;
    lbFilters: TListBox;
    btnAdd: TImageListButton;
    btnEdit: TImageListButton;
    btnDelete: TImageListButton;
    odFilter: TOpenDialog;
    sdFilter: TSaveDialog;
    pcFilterDetails: TPageControl;
    tsGeneral: TTabSheet;
    tsSamples: TTabSheet;
    tsOptions: TTabSheet;
    lblFilterName: TLabel;
    eFilterName: TEdit;
    lblFilterSurveys: TLabel;
    lbFilterSurveys: TListBox;
    btnAddSurvey: TImageListButton;
    btnDeleteSurvey: TImageListButton;
    lblFilterTaxa: TLabel;
    lbFilterTaxa: TListBox;
    btnAddTaxon: TImageListButton;
    btnDeleteTaxon: TImageListButton;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    gbSpatialRefCorners: TGroupBox;
    lblSWCorner: TLabel;
    lblNECorner: TLabel;
    lblInstructions: TLabel;
    bvlInstructions: TBevel;
    btnMap: TImageListButton;
    eNECorner: TEdit;
    eSWCorner: TEdit;
    bvlCheckBoxes: TBevel;
    lblObsDateStart: TLabel;
    lblObsDateEnd: TLabel;
    bvlObservationDates: TBevel;
    meObsDateStart: TMaskEdit;
    meObsDateEnd: TMaskEdit;
    cbIncludeOccurrences: TCheckBox;
    cbIncludeLocations: TCheckBox;
    cbIncludeNames: TCheckBox;
    pnlButtons: TPanel;
    btnOpen: TImageListButton;
    btnSave: TImageListButton;
    pnlClose: TPanel;
    btnClose: TButton;
    lblFilterSurveyTags: TLabel;
    lbFilterSurveyTags: TListBox;
    btnAddSurveyTag: TImageListButton;
    btnDeleteSurveyTag: TImageListButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddTaxonClick(Sender: TObject);
    procedure btnDeleteTaxonClick(Sender: TObject);
    procedure btnDeleteSurveyClick(Sender: TObject);
    procedure lbFiltersClick(Sender: TObject);
    procedure lbFiltersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbFilterTaxaDrawItem
      (Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnAddSurveyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pcFilterDetailsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnMapClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnAddSurveyTagClick(Sender: TObject);
    procedure btnDeleteSurveyTagClick(Sender: TObject);
    procedure eNECornerExit(Sender: TObject);
    procedure eSWCornerExit(Sender: TObject);
  private
    FEditMode: TEditMode;
    FSelectedFilter: TExportFilter;
    FNoSurveys: Boolean;
    FNoSurveyTags: Boolean;
    FNoTaxa: Boolean;
    FValidEnteredSW : string;
    FValidEnteredNE : String;
    FMapAlreadyPresent: Boolean;
    FMapWindow: TfrmMap;
    procedure Save;
    procedure Validate;
    procedure ValidateBoundingBox;
    function ValidDate(AEdit: TMaskEdit): Boolean;
    procedure SetEditMode(const Value: TEditMode);
    procedure ClearListBoxes;
    procedure SetSelectedFilter(AFilter: TExportFilter);
    procedure DisplayFilter;
    procedure SetNoSurveys;
    procedure SetSomeSurveys;
    procedure SetNoSurveyTags;
    procedure SetSomeSurveyTags;
    procedure SetNoTaxa;
    procedure SetSomeTaxa;
    function NoRelatedSchemes: Boolean;
    procedure UpdateBoundingBox(KeyList: TKeyList);
    procedure ValidateCorner(ctrl: TEdit; var storedValue: string);
  protected
    procedure SetupDestinationControls; override;
    procedure SurveyDataDropped(const Sender: TObject;
              const iFormat: integer; const iSourceData: TKeyList;
              const iTextStrings: TStringList; var ioHandled: boolean);
    procedure SurveyTagDataDropped(const Sender: TObject;
              const iFormat: integer; const iSourceData: TKeyList;
              const iTextStrings: TStringList; var ioHandled: boolean);
    procedure TaxonDataDropped(const Sender: TObject;
              const iFormat: integer; const iSourceData: TKeyList;
              const iTextStrings: TStringList; var ioHandled: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EditMode: TEditMode read FEditMode write SetEditMode;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Find, ExceptionForm, GeneralData, ApplicationSettings, Maintbar,
  SpatialRefFuncs, DropTarget, DatabaseAccessADO;

const
  RELATED_SCHEMES_SQL = 'SELECT Recording_Scheme_Key FROM Recording_Scheme ' +
                        'WHERE Export_Filter_Key = ''%s''';

{-------------------------------------------------------------------------------
 Constructs the dialogue box, loading in all Export Filters in the database.
}
constructor TfrmFilterManager.Create(AOwner: TComponent);
var lFilterList: TList;
    k: Integer;
begin
  inherited;
  lFilterList := TExportFilter.CreateAllFilters;
  try
    if lFilterList.Count = 0 then begin
      btnEdit.Enabled   := False;
      btnDelete.Enabled := False;
      FNoSurveys        := False;
      FNoSurveyTags     := False;
      FNoTaxa           := False;
    end else begin
      for k := 0 to lFilterList.Count - 1 do
        lbFilters.Items.AddObject(TExportFilter(lFilterList[k]).FilterName, lFilterList[k]);
      lbFiltersClick(nil);
    end;
  finally
    lFilterList.Free
  end;
  EditMode := emView;
end;

{-------------------------------------------------------------------------------
  Frees all objects in the list boxes.
}
destructor TfrmFilterManager.Destroy;
var k: Integer;
begin
  ClearListBoxes;
  with lbFilters.Items do begin
    for k := Count - 1 downto 0 do Objects[k].Free;
    Clear;
  end;

  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.FormCreate(Sender: TObject);
begin
  pcFilterDetails.ActivePage := tsGeneral;
  gbFilterList.Height := pnlButtons.Top - gbFilterList.Top;
  pcFilterDetails.Height := gbFilterList.Height;
  pcFilterDetails.Width := ClientWidth - pcFilterDetails.Left - 10;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TExportFilter.UpdateFilterList;
  Action := caFree;
end;

{-------------------------------------------------------------------------------
  Create a new Export Filter.
}
procedure TfrmFilterManager.btnAddClick(Sender: TObject);
begin
  EditMode := emAdd;

  eFilterName.Text := '';
  ClearListBoxes;
  SetNoSurveys;
  SetNoTaxa;
  eSWCorner.Text := '';
  eNECorner.Text := '';
  meObsDateStart.Text := '';
  meObsDateEnd.Text := '';
  FValidEnteredSW := '';
  FValidEnteredNE := '';
  cbIncludeOccurrences.Checked := True;
  cbIncludeLocations.Checked := False;
  cbIncludeNames.Checked := False;
end;

{-------------------------------------------------------------------------------
  Add a Survey to filter on.
}
procedure TfrmFilterManager.btnAddSurveyClick(Sender: TObject);
var k: Integer;
begin
  with TdlgFind.CreateDialog(nil, ResStr_SelectSurvey, ftSurvey) do
    try
      if ShowModal = mrOK then
        with lbFilterSurveys.Items do begin
          for k := 0 to Count - 1 do
            if Assigned(Objects[k]) and (TKey(Objects[k]).Key = ItemKey) then Exit;

          SetSomeSurveys;
          AddObject(ItemText, TKey.Create(ItemKey));
        end;
    finally
      Free
    end;
end;

{-------------------------------------------------------------------------------
  Add a Survey Tag to filter on.
}
procedure TfrmFilterManager.btnAddSurveyTagClick(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  with TdlgFind.CreateDialog(nil, ResStr_FindSurveyTag, ftSurveyTag) do
    try
      if ShowModal = mrOk then begin
        with lbFilterSurveyTags.Items do begin
          for i := 0 to Count - 1 do
            if Assigned(Objects[i]) and (TKey(Objects[i]).Key = ItemKey) then Exit;

          SetSomeSurveyTags;
          AddObject(ItemText, TKey.Create(ItemKey));
        end;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Add a Taxonomic Group to filter on.
}
procedure TfrmFilterManager.btnAddTaxonClick(Sender: TObject);
var k: Integer;
    lTaxonNames: TTaxonNames;
begin
  with TdlgFind.CreateDialog(nil, true, ResStr_SelectTaxa, ftTaxon) do
    try
      if ShowModal = mrOK then begin
        lTaxonNames := dmGeneralData.GetTaxonNamesObject(dmGeneralData.GetTaxonPreferredKey(ItemKey));

        with lbFilterTaxa.Items do begin
          for k := Count - 1 downto 0 do
            if Assigned(Objects[k]) and TTaxonNames(Objects[k]).IsIdentical(lTaxonNames) then Exit;

          SetSomeTaxa;
          AddObject('', lTaxonNames);
        end;
      end;
    finally
      Free
    end;
end;

{-------------------------------------------------------------------------------
  Cancel Adding or Editing.
}
procedure TfrmFilterManager.btnCancelClick(Sender: TObject);
begin
  DisplayFilter;
  EditMode := emView;
end;

{-------------------------------------------------------------------------------
  Close the dialogue box.
}
procedure TfrmFilterManager.btnCloseClick(Sender: TObject);
begin
  Close;
end;

{-------------------------------------------------------------------------------
  Delete the selected Export Filter.
}
procedure TfrmFilterManager.btnDeleteClick(Sender: TObject);
begin
  lbFiltersClick(nil);
  if (FSelectedFilter <> nil) and NoRelatedSchemes and
     (ConfirmDeletionYesNo(ResStr_TheExportFilter)=mrYes) then
  begin
    FSelectedFilter.Delete;
    FSelectedFilter.Free;
    lbFilters.Items.Delete(lbFilters.ItemIndex);
    lbFiltersClick(nil);
  end;
end;

{-------------------------------------------------------------------------------
  Deletes the selected Survey(s) from the list of Surveys to filter on.
}
procedure TfrmFilterManager.btnDeleteSurveyClick(Sender: TObject);
var k: Integer;
begin
  for k := lbFilterSurveys.Items.Count - 1 downto 0 do
    if lbFilterSurveys.Selected[k] then begin
      lbFilterSurveys.Items.Objects[k].Free;
      lbFilterSurveys.Items.Delete(k);
    end;
  if lbFilterSurveys.Items.Count = 0 then SetNoSurveys;
end;

{-------------------------------------------------------------------------------
  Deletes the selected Surevy Tag(s) from the list to filter on.
}
procedure TfrmFilterManager.btnDeleteSurveyTagClick(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  with lbFilterSurveyTags do begin
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then begin
        Items.Objects[i].Free;
        Items.Delete(i);
      end;
    if Items.Count = 0 then SetNoSurveyTags;
  end;
end;

{-------------------------------------------------------------------------------
  Deletes the selected Taxonomic Group(s) from the list to filter on.
}
procedure TfrmFilterManager.btnDeleteTaxonClick(Sender: TObject);
var k: Integer;
begin
  for k := lbFilterTaxa.Items.Count - 1 downto 0 do
    if lbFilterTaxa.Selected[k] then begin
      lbFilterTaxa.Items.Objects[k].Free;
      lbFilterTaxa.Items.Delete(k);
    end;
  if lbFilterTaxa.Items.Count = 0 then SetNoTaxa;
end;

{-------------------------------------------------------------------------------
  Edit the selected Export Filter.
}
procedure TfrmFilterManager.btnEditClick(Sender: TObject);
begin
  EditMode := emEdit;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.btnMapClick(Sender: TObject);
begin
  FMapAlreadyPresent := Assigned(frmMain.GetForm(TfrmMap));

  dmFormActions.actMapWindow.Execute;
  FMapWindow := frmMain.GetForm(TfrmMap) as TfrmMap;
  if Assigned(FMapWindow) then
  begin
    FMapWindow.BringToFront;
    FMapWindow.SelectArea;
    SetupLink(TBaseForm(frmMain.ActiveMDIChild), Self, UpdateBoundingBox);
  end;
end;

// Save the new/edited Export Filter to the database.
procedure TfrmFilterManager.btnOKClick(Sender: TObject);
begin
  Save;
  EditMode := emView;
end;

// Open an Export Filter that has been saved to file.
procedure TfrmFilterManager.btnOpenClick(Sender: TObject);
var lFilter: TExportFilter;
    i: Integer;
begin
  if odFilter.Execute then begin
    try
      lFilter := TExportFilter.LoadFromFile(odFilter.FileName);
    except on E:Exception do
      raise TExceptionPath.CreateNonCritical(Format(ResStr_FilterOpenFailed, [E.Message]));
    end;
    try
      i := lbFilters.Items.Count - 1;
      while (i >= 0) and (TExportFilter(lbFilters.Items.Objects[i]).FilterKey <> lFilter.FilterKey) do Dec(i);
      if i < 0 then begin
        lFilter.SaveFilter;
        lbFilters.ItemIndex := lbFilters.Items.AddObject(lFilter.FilterName, lFilter);
        lbFiltersClick(nil);
      end else
        if MessageDlg(ResStr_ExportFilterExists,
                      mtWarning, [mbYes, mbNo], 0) = idYes then begin
          lbFilters.Items.Objects[i].Free;
          lbFilters.Items.Objects[i] := lFilter;
          lbFilters.Items.Strings[i] := lFilter.FilterName;
          lbFilters.ItemIndex := i;
          lbFiltersClick(nil);
        end else
          lFilter.Free;
    except
      on E: Exception do begin
        lFilter.Free;
        raise E;
      end;
    end;
  end;
end;

// Save an Export Filter to file.
procedure TfrmFilterManager.btnSaveClick(Sender: TObject);
begin
  if FSelectedFilter <> nil then begin
    sdFilter.FileName := ValidateFileName(FSelectedFilter.FilterName + '.exf');
    if sdFilter.Execute then
      FSelectedFilter.SaveToFile(sdFilter.FileName);
  end;
end;

// Clears the Survey and Taxonomic Group list boxes, freeing the associated objects.
procedure TfrmFilterManager.ClearListBoxes;

  procedure ClearList(items: TStrings);
  var
    i: Integer;
  begin
    for i := 0 to items.Count - 1 do
      items.Objects[i].Free;
    items.Clear;
  end;

begin
  ClearList(lbFilterSurveys.Items);
  ClearList(lbFilterSurveyTags.Items);
  ClearList(lbFilterTaxa.Items);
end;

{-------------------------------------------------------------------------------
 Displays the currently selected Export Filter in the Filter Details GroupBox.
}
procedure TfrmFilterManager.DisplayFilter;
var k: Integer;
begin
  ClearListBoxes;
  if FSelectedFilter = nil then begin
    eFilterName.Text    := '';
    eNECorner.Text      := '';
    eSWCorner.Text      := '';
    meObsDateStart.Text := '';
    meObsDateEnd.Text   := '';
    cbIncludeOccurrences.Checked := False;
    cbIncludeLocations.Checked   := False;
    cbIncludeNames.Checked       := False;
  end else begin
    eFilterName.Text := FSelectedFilter.FilterName;
    if FSelectedFilter.SurveyCount > 0 then begin
      SetSomeSurveys;
      for k := 0 to FSelectedFilter.SurveyCount - 1 do
        lbFilterSurveys.Items.AddObject(
            FSelectedFilter.SurveyName[k],
            TKey.Create(FSelectedFilter.SurveyKey[k]));
    end else
      SetNoSurveys;

    if FSelectedFilter.SurveyTagCount > 0 then begin
      SetSomeSurveyTags;
      for k := 0 to FSelectedFilter.SurveyTagCount - 1 do
        lbFilterSurveyTags.Items.AddObject(
            FSelectedFilter.SurveyTagName[k],
            TKey.Create(FSelectedFilter.SurveyTagKey[k]));
    end else
      SetNoSurveyTags;

    if FSelectedFilter.TaxonCount > 0 then begin
      SetSomeTaxa;
      for k := 0 to FSelectedFilter.TaxonCount - 1 do
        lbFilterTaxa.Items.AddObject('', TExportFilter.CopyTaxonNames(FSelectedFilter.TaxonNames[k]));
    end else
      SetNoTaxa;

    if FSelectedFilter.HasBoundingBox then begin
      FValidEnteredSW := ConvertFromLatLong(FSelectedFilter.SWLatLong, AppSettings.SpatialRefSystem);
      FValidEnteredNE := ConvertFromLatLong(FSelectedFilter.NELatLong, AppSettings.SpatialRefSystem);
      eSWCorner.Text := LocaliseSpatialRef(FValidEnteredSW);
      eNECorner.Text := LocaliseSpatialRef(FValidEnteredNE);
    end else begin
      eSWCorner.Text := '';
      eNECorner.Text := '';
      FValidEnteredSW := '';
      FValidEnteredNE := '';
    end;
    if FSelectedFilter.HasStartDate then meObsDateStart.Text := DateToStr(FSelectedFilter.ObsDateStart)
                                    else meObsDateStart.Text := '';
    if FSelectedFilter.HasEndDate then meObsDateEnd.Text := DateToStr(FSelectedFilter.ObsDateEnd)
                                  else meObsDateEnd.Text := '';
    cbIncludeOccurrences.Checked := FSelectedFilter.IncludeOccurrences;
    cbIncludeLocations.Checked := FSelectedFilter.IncludeLocations;
    cbIncludeNames.Checked := FSelectedFilter.IncludeNames;
  end;
end;

// Checks whether currently adding/editing an Export Filter and asks for confirmation if so.
procedure TfrmFilterManager.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var lModalResult: TModalResult;
begin
  if EditMode in [emAdd, emEdit] then begin
    lModalResult := MessageDlg(ResStr_SaveChanges, mtWarning, mbYesNoCancel, 0);
    case lModalResult of
      idYes: Save;
      idCancel: CanClose := False;
    end;
  end;
end;

// Causes the newly selected filter to be displayed.
procedure TfrmFilterManager.lbFiltersClick(Sender: TObject);
begin
  if EditMode = emView then
    if lbFilters.Items.Count = 0 then SetSelectedFilter(nil)
    else begin
      if lbFilters.ItemIndex = -1 then lbFilters.ItemIndex := 0;
      SetSelectedFilter(TExportFilter(lbFilters.Items.Objects[lbFilters.ItemIndex]));
    end;
end;

// Needs to fire the click event.
procedure TfrmFilterManager.lbFiltersKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lbFiltersClick(Sender);
end;

// To ensure taxa are displayed in italic when necessary.
procedure TfrmFilterManager.lbFilterTaxaDrawItem
  (Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var lTaxonNames: TTaxonNames;
begin
  lbFilterTaxa.Canvas.FillRect(Rect);
  if not FNoTaxa then begin
    lTaxonNames := TTaxonNames(lbFilterTaxa.Items.Objects[Index]);
    GenericDrawTaxonNames(lbFilterTaxa.Canvas, lTaxonNames, Rect, False);
  end else
    lbFilterTaxa.Canvas.TextOut(Rect.Left, Rect.Top,lbFilterTaxa.Items[Index]);
end;

// Returns False iff there are recording schemes associated with the selected Filter.
function TfrmFilterManager.NoRelatedSchemes: Boolean;
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(RELATED_SCHEMES_SQL, [FSelectedFilter.FilterKey]);
    Open;
    try Result := RecordCount = 0;
    finally Close end;
  end;
 if not Result then
   MessageDlg(ResStr_CannotDeleteFilter, mtInformation, [mbOK], 0);
end;

procedure TfrmFilterManager.pcFilterDetailsChange(Sender: TObject);
begin
  btnOK.Parent := pcFilterDetails.ActivePage;
  btnCancel.Parent := pcFilterDetails.ActivePage;
end;

// Assigns the entered data to the TExportFilter object (creating one when necessary)
// and causes the object to commit itself to the database.
procedure TfrmFilterManager.Save;
var lFilter: TExportFilter;
    k: Integer;
begin
  Validate;
  if EditMode = emAdd then lFilter := TExportFilter.Create
  else begin
    lFilter := FSelectedFilter;
    lFilter.ClearSurveys;
    lFilter.ClearSurveyTags;
    lFilter.ClearTaxa;
  end;
  try
    lFilter.FilterName := eFilterName.Text;
    if not FNoSurveys then
      for k := 0 to lbFilterSurveys.Items.Count - 1 do
        lFilter.AddSurvey(lbFilterSurveys.Items[k], TKey(lbFilterSurveys.Items.Objects[k]).Key);
    if not FNoSurveyTags then
      for k := 0 to lbFilterSurveyTags.Items.Count - 1 do
        lFilter.AddSurveyTag(lbFilterSurveyTags.Items[k], TKey(lbFilterSurveyTags.Items.Objects[k]).Key);
    if not FNoTaxa then
      for k := 0 to lbFilterTaxa.Items.Count - 1 do
        lFilter.AddTaxonCopy(TTaxonNames(lbFilterTaxa.Items.Objects[k]));
    lFilter.IncludeOccurrences := cbIncludeOccurrences.Checked;
    lFilter.IncludeLocations   := cbIncludeLocations.Checked;
    lFilter.IncludeNames       := cbIncludeNames.Checked;

    lFilter.HasBoundingBox := (eSWCorner.Text <> '') or (eNECorner.Text <> '');
    if lFilter.HasBoundingBox then begin
      lFilter.SWLatLong := ConvertToLatLong(FValidEnteredSW, AppSettings.SpatialRefSystem);
      lFilter.NELatLong := ConvertToLatLong(FValidEnteredNE, AppSettings.SpatialRefSystem);
    end;

    lFilter.HasStartDate :=
        StringReplace(meObsDateStart.Text, ' ', '', [rfReplaceAll]) <> DateSeparator + DateSeparator;
    if lFilter.HasStartDate then lFilter.ObsDateStart := StrToDate(meObsDateStart.Text);

    lFilter.HasEndDate :=
        StringReplace(meObsDateEnd.Text, ' ', '', [rfReplaceAll]) <> DateSeparator + DateSeparator;
    if lFilter.HasEndDate then lFilter.ObsDateEnd := StrToDate(meObsDateEnd.Text);

    lFilter.SaveFilter;
  except
    on E: Exception do begin
      if EditMode = emAdd then lFilter.Free;
      raise E;
    end;
  end;
  if EditMode = emAdd then begin
    lbFilters.ItemIndex := lbFilters.Items.AddObject(lFilter.FilterName, lFilter);
    FSelectedFilter := lFilter;
  end else
    lbFilters.Items.Strings[lbFilters.ItemIndex] := lFilter.FilterName;
end;

// Enables/disables controls according to the new EditMode.
procedure TfrmFilterManager.SetEditMode(const Value: TEditMode);
var viewing: Boolean;
begin
  FEditMode := Value;
  viewing := Value = emView;

  lbFilters.Enabled            := viewing;
  btnAdd.Enabled               := viewing;
  btnEdit.Enabled              := viewing and (FSelectedFilter <> nil);
  btnDelete.Enabled            := viewing and (FSelectedFilter <> nil);
  btnSave.Enabled              := viewing and (FSelectedFilter <> nil);
  btnOpen.Enabled              := viewing;
  eFilterName.Enabled          := not viewing;
  lbFilterSurveys.Enabled      := not viewing;
  btnAddSurvey.Enabled         := not viewing;
  btnDeleteSurvey.Enabled      := not viewing and not FNoSurveys;
  lbFilterSurveyTags.Enabled   := not viewing;
  btnAddSurveyTag.Enabled      := not viewing;
  btnDeleteSurveyTag.Enabled   := not viewing and not FNoSurveyTags;
  lbFilterTaxa.Enabled         := not viewing;
  btnAddTaxon.Enabled          := not viewing;
  btnDeleteTaxon.Enabled       := not viewing and not FNoTaxa;
  cbIncludeOccurrences.Enabled := not viewing;
  cbIncludeLocations.Enabled   := not viewing;
  cbIncludeNames.Enabled       := not viewing;
  btnCancel.Enabled            := not viewing;
  btnOK.Enabled                := not viewing;
  eNECorner.Enabled            := not viewing;
  eSWCorner.Enabled            := not viewing;
  btnMap.Enabled               := not viewing;
  meObsDateStart.Enabled       := not viewing;
  meObsDateEnd.Enabled         := not viewing;
  SetRequiredFieldsColourState(btnOK.Enabled, [eFilterName]);
end;

// Displays the "Filter on all Surveys" message.  The FNoSurveys value is used
// to indicate that the item in the list box does not represent a Survey.
procedure TfrmFilterManager.SetNoSurveys;
begin
  lbFilterSurveys.Items.Add(ResStr_AllFilterSurvey);
  lbFilterSurveys.Font.Color := clGrayText;
  btnDeleteSurvey.Enabled := False;
  FNoSurveys := True;
end;

// Displays the "Filter on all Survey Tags" message.  The FNoSurveyTags value is used
// to indicate that the item in the list box does not represent a Survey Tag.
procedure TfrmFilterManager.SetNoSurveyTags;
begin
  lbFilterSurveyTags.Items.Add(ResStr_AllFilterSurveyTag);
  lbFilterSurveyTags.Font.Color := clGrayText;
  btnDeleteSurveyTag.Enabled := False;
  FNoSurveyTags := True;
end;

// Displays the "Filter on all Taxonomic Groups" message.  The FNoTaxa value is used
// to indicate that the item in the list box does not represent a Taxonomic Group.
procedure TfrmFilterManager.SetNoTaxa;
begin
  lbFilterTaxa.Items.Add(ResStr_TaxonGroupFilter);
  lbFilterTaxa.Font.Color := clGrayText;
  btnDeleteTaxon.Enabled := False;
  FNoTaxa := True;
end;

// Notes the newly selected Filter and acts accordingly.
procedure TfrmFilterManager.SetSelectedFilter(AFilter: TExportFilter);
begin
  if AFilter <> FSelectedFilter then begin
    FSelectedFilter := AFilter;
    DisplayFilter;
    if AFilter = nil then begin
      btnEdit.Enabled := False;
      btnDelete.Enabled := False;
      btnSave.Enabled := False;
    end;
  end;
end;

// Undoes the effects of SetNoSurveys.
procedure TfrmFilterManager.SetSomeSurveys;
begin
  if FNoSurveys then begin
    lbFilterSurveys.Enabled := EditMode in [emAdd, emEdit];
    lbFilterSurveys.Items.Delete(0);
  end;
  lbFilterSurveys.Font.Color := clWindowText;
  btnDeleteSurvey.Enabled := EditMode in [emAdd, emEdit];
  FNoSurveys := False;
end;

procedure TfrmFilterManager.SetSomeSurveyTags;
begin
  if FNoSurveyTags then begin
    lbFilterSurveyTags.Enabled := EditMode in [emAdd, emEdit];
    lbFilterSurveyTags.Items.Delete(0);
  end;
  lbFilterSurveyTags.Font.Color := clWindowText;
  btnDeleteSurveyTag.Enabled := EditMode in [emAdd, emEdit];
  FNoSurveyTags := False;
end;

// Undoes the effects of SetNoTaxa.
procedure TfrmFilterManager.SetSomeTaxa;
begin
  if FNoTaxa then begin
    lbFilterTaxa.Enabled := EditMode in [emAdd, emEdit];
    lbFilterTaxa.Items.Delete(0);
  end;
  lbFilterTaxa.Font.Color := clWindowText;
  btnDeleteTaxon.Enabled := EditMode in [emAdd, emEdit];
  FNoTaxa := False;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.SetupDestinationControls;
begin
  RegisterDropComponent(lbFilterSurveys, SurveyDataDropped, [TN_SURVEY], [CF_JNCCDATA]);
  RegisterDropComponent(lbFilterSurveyTags, SurveyTagDataDropped, [TN_CONCEPT], [CF_JNCCDATA]);
  RegisterDropComponent(lbFilterTaxa, TaxonDataDropped, [TN_TAXON_LIST_ITEM], [CF_JNCCDATA]);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.SurveyDataDropped(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var
  i, j: Integer;
  dup: Boolean;
begin
  if iFormat = CF_JNCCDATA then
  begin
    if SameText(iSourceData.Header.TableName, TN_SURVEY) then
      for i := 0 to iSourceData.Header.ItemCount - 1 do begin
        dup := False;
        with lbFilterSurveys.Items do begin
          for j := 0 to Count - 1 do
            if Assigned(Objects[j]) and (TKey(Objects[j]).Key = iSourceData.Items[i].KeyField1) then
            begin
              dup := True;
              Break;
            end;

          if dup then Continue;

          with dmDatabase.GetRecordset('usp_Survey_Select', ['@Key', iSourceData.Items[i].KeyField1]) do
          begin
            if not Eof then begin
              SetSomeSurveys;
              AddObject(Fields['Display_Name'].Value, TKey.Create(iSourceData.Items[i].KeyField1));
            end;
            Close;
          end;
        end;
      end;
    ioHandled := true;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.SurveyTagDataDropped(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var
  i, j: Integer;
  dup: Boolean;
begin
  if iFormat = CF_JNCCDATA then
  begin
    if SameText(iSourceData.Header.TableName, TN_CONCEPT) then
      for i := 0 to iSourceData.Header.ItemCount - 1 do begin
        dup := False;
        with lbFilterSurveyTags.Items do begin
          for j := 0 to Count - 1 do
            if Assigned(Objects[j]) and (TKey(Objects[j]).Key = iSourceData.Items[i].KeyField1) then
            begin
              dup := True;
              Break;
            end;

          if dup then Continue;

          with dmDatabase.GetRecordset('usp_Concept_Select', ['@ConceptKey', iSourceData.Items[i].KeyField1]) do
          begin
            if not Eof then begin
              SetSomeSurveyTags;
              AddObject(Fields['Item_Name'].Value, TKey.Create(iSourceData.Items[i].KeyField1));
            end;
            Close;
          end;
        end;
      end;
    ioHandled := true;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.TaxonDataDropped(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
var
  i, j: Integer;
  txNames: TTaxonNames;
  dup: Boolean;
begin
  if iFormat = CF_JNCCDATA then
  begin
    if iSourceData.Header.TableName = TN_TAXON_LIST_ITEM then
      for i := 0 to iSourceData.Header.ItemCount - 1 do begin
        txNames := dmGeneralData.GetTaxonNamesObject(
            dmGeneralData.GetTaxonPreferredKey(iSourceData.Items[i].KeyField1));

        dup := False;
        with lbFilterTaxa.Items do begin
          for j := 0 to Count - 1 do
            if Assigned(Objects[j]) and TTaxonNames(Objects[j]).IsIdentical(txNames) then begin
              dup := True;
              Break;
            end;

          if dup then Continue;

          SetSomeTaxa;
          AddObject('', txNames);
        end;
      end;
    ioHandled := true;
  end;
end;


{-------------------------------------------------------------------------------
 Lifted from SurveyDetails
}
procedure TfrmFilterManager.UpdateBoundingBox(KeyList:TKeyList);
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
  If FMapAlreadyPresent then
    BringToFront
  else begin
    FMapWindow.Release;
    frmMain.Repaint;
  end;
end;  // UpdateBoundingBox

// All Export Filters must have a name!
procedure TfrmFilterManager.Validate;
begin
  if eFilterName.Text = '' then begin
    if pcFilterDetails.ActivePage = tsGeneral then eFilterName.SetFocus;
    raise TExceptionPath.CreateNonCritical(ResStr_ProvideFilterName);
  end;
  ValidateBoundingBox;
  if not ValidDate(meObsDateStart) then begin
     if pcFilterDetails.ActivePage = tsSamples then meObsDateStart.SetFocus;
     raise TExceptionPath.CreateNonCritical(ResStr_InvalidStartDate);
  end;
  if not ValidDate(meObsDateEnd) then begin
    if pcFilterDetails.ActivePage = tsSamples then meObsDateEnd.SetFocus;
    raise TExceptionPath.CreateNonCritical(ResStr_InvalidEndDate);
  end;
end;

// Validate the entry in the specified Date Edit box - can be null.
procedure TfrmFilterManager.ValidateBoundingBox;
var
  lValidSR: TValidBoundingBox;
begin
  if eNECorner.Focused or eSWCorner.Focused then begin
    ValidateCorner(eSWCorner, FValidEnteredSW);
    ValidateCorner(eNECorner, FValidEnteredNE);
  end;
  if (eSWCorner.Text <> '') or (eNECorner.Text <> '') then begin
    if ( FValidEnteredSW + FValidEnteredNE ) <> '' then
    begin
      lValidSR := CheckBoundingBox(FValidEnteredSW, FValidEnteredNE, DetermineSpatialRefSystem(FValidEnteredSW));
      if not lValidSR.Valid then begin
        if pcFilterDetails.ActivePage = tsSamples then eSWCorner.SetFocus;
        raise TExceptionPath.CreateNonCritical(lValidSR.Error);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmFilterManager.ValidDate(AEdit: TMaskEdit): Boolean;
var
  value: String;
begin
  value  := StringReplace(AEdit.Text, ' ', '', [rfReplaceAll]);
  Result := value = DateSeparator + DateSeparator;
  if not Result then begin
    try
      AEdit.Text := DateToStr(StrToDate(value));
      Result := True;
    except
      on EConvertError do { nothing }
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmFilterManager.FormActivate(Sender: TObject);
begin
  inherited;
  if not (csDestroying in ComponentState) then // safety
     with frmMain do
     begin
       dmFormActions.actPrint.Enabled := false;
       ClearContextToolbar(false);// locks the main window
       mnuFileSave.Enabled   := False;
       mnuFileSaveAs.Enabled := False;
     end;
  LockWindowUpdate(0);//unlocks the main window.
end;

procedure TfrmFilterManager.eNECornerExit(Sender: TObject);
begin
  if FEditMode<>emView then
    ValidateCorner(eNECorner, FValidEnteredNE);
end;

procedure TfrmFilterManager.eSWCornerExit(Sender: TObject);
begin
  if FEditMode<>emView then
    ValidateCorner(eSWCorner, FValidEnteredSW);
end;


procedure TfrmFilterManager.ValidateCorner(ctrl: TEdit; var storedValue: string);
begin
  ValidateValue(
      ValidSpatialRef(
          DelocaliseSpatialRef(ctrl.Text),
          AppSettings.SpatialRefSystem).Valid,
      ResStr_InvalidSpRefFormat,ctrl);
  ValidateSpatialRefEntry(ctrl, AppSettings.SpatialRefSystem, storedValue);
end;  // ValidateNECorner

end.
