{===============================================================================
  Unit:        ParametersForm.pas

  Description: The Parameter Entry form.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 43 $
    $Date: 15/04/09 15:57 $
    $Author: Pauldavies $
===============================================================================}

unit CRFrmParametersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, Grids, ExtCtrls, CRReportSQL,
  ComboListID, AddinIDComboBox, Contnrs, VagueDateEdit, CRCommonClasses,
  RestrictedEdits, AddinCompositeComponent, AddinLinkedControls, ImgList,
  FormActions, ExceptionForm, BaseChildUnit, adodb, BaseFormUnit, Menus,
  SpatialRef, DataClasses, AddinSearchManager, ComCtrls, exgrid, RapTree,
  KeyboardRapidTree, MapServerLink, Map, Wizard, MapPolygonScanner;

resourcestring
  ResStr_ValueNotRecognised='Please enter a value for this parameter.';
  ResStr_LinkedEditNotRecognised='Please enter an existing item for this parameter.';
  ResStr_SelectGroupParamYouRequire='Select the %s parameter type you require:';
  ResStr_UseParameter='Use %s as a parameter';
  ResStr_GridSquareRangeDiffSystems = 'A grid square range cannot be generated when '+
      'the spatial reference systems are different.';
  ResStr_GridSquareRangeDiffSizes = 'A grid square range cannot be generated when '+
      'the defining grid squares are not the same precision.';
  ResStr_GridSquareRange = 'Grid Square Range';
  ResStr_BoundingBox = 'Bounding Box';
  ResStr_SWCorner = 'SW Corner';
  ResStr_NECorner = 'NE Corner';
  ResStr_SpatialRefNotRecognised = 'The spatial reference system cannot be recognised';
  ResStr_SelectARadioButton = 'You need to select one option for the %s group of parameters.';
  ResStr_SelectAtLeastOne = 'You need to enter a value for at least one of the %s parameters.';
  ResStr_WrongParameterType = 'Internal error - Parameter type %s not handled';
  ResStr_InvalidLinkedControl = 'Internal error - invalid linked control update type.';
  ResStr_Text = 'Text';
  ResStr_Number = 'Number';
  ResStr_Date = 'Date';
  ResStr_VagueDate = 'Vague Date';
  ResStr_FilePath = 'File Path';
  ResStr_Location = 'Location';
  ResStr_Taxon = 'Taxon';
  ResStr_Name = 'Name';
  ResStr_Individual = 'Individual';
  ResStr_Organisation = 'Organisation';
  ResStr_SpatialRef = 'Spatial Ref.';
  ResStr_Polygon = 'Polygon';
  ResStr_InvalidContrRequested = 'Internal error - invalid control requested data from the map.';
  ResStr_FromRucksack = 'From Rucksack';
  ResStr_BadUseRucksackValue = 'Unexpected value for UseRucksack property.';
  ResStr_ManualParamValuesCaption = 'Enter parameter values manually:';
  ResStr_RucksackParamValuesCaption = 'Select rucksack to obtain parameter values:';
  ResStr_CannotUseOptionalCurrentRucksack = 'Internal error - cannot specify Optional for FCurrentUseRucksack field when creating grouped controls for a parameter that optionally gets its values from a rucksack.';
  ResStr_SortOrder = 'Sort Order';
  ResStr_InvalidPolygonRef =  'Invalid Data. Polygon reference expected.';
  ResStr_InvalidParameterTypeForRucksack = 'This parameter type cannot be set from a rucksack.';
  ResStr_AllPolygonsDeleted = 'All the polygons selected for this report have since been deleted, so the report will be cancelled.';
  ResStr_SomePolygonsDeleted = 'Some of the polygons selected for this report have since been deleted and will be excluded from the report results.';

type
  EParametersException = class(TExceptionPath);

  // radio button that tracks the parameter & label in a SelectOneOfGroup scenario
  TGroupingRadioButton = class(TRadioButton)
  private
    FControl: TWinControl;
    procedure SetControl(const Value: TWinControl);
  protected
    procedure Click; override;
  public
    procedure EnableControls(AControl: TControl);
    property Control: TWinControl read FControl write SetControl;
  end;

  // custom control to accept a grid square range parameter entry
  TGridSquareRange = class(TPanel)
  private
    FNeRef: TEdit;
    FSwRef: TEdit;
    FtoPanel: TPanel;
    function GetRange: string;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Validate;
    property Range: string read GetRange;
  end;

  // custom control to accept a bounding box parameter entry
  TBoundingBox = class(TPanel)
  private
    FNeRef: TEdit;
    FSwRef: TEdit;
    FDropDownButton: TButton;
    FDropDownMenu: TPopupMenu;
    FGetButton: TImageListButton;
    procedure DropDownClick(Sender: TObject);
    function GetValues: string;
    function GetNeRef: string;
    function GetOnGetFromMap: TNotifyEvent;
    function GetSwRef: string;
    procedure SetNeRef(const Value: string);
    procedure SetOnGetFromMap(const Value: TNotifyEvent);
    procedure SetSwRef(const Value: string);
    procedure SetupButtons;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateMapWindowSelector;
    procedure Validate;
    property NeRef: string read GetNeRef write SetNeRef;
    property OnGetFromMap: TNotifyEvent read GetOnGetFromMap write SetOnGetFromMap;
    property Values: string read GetValues;
    property SwRef: string read GetSwRef write SetSwRef;
  published
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
  end;

  //custom control for buttons of polygon selection screen
  TPolygonSelectionBtn = class(TPanel)
  private
    FFindPolygonButton: TImageListButton;
    FFindPolygonDropDown: TButton;
    procedure SetFindPolygonButton;
    procedure SetFindPolygonDropDown;
    procedure btnFindPolygonClick(Sender: TObject);
    procedure btnPolyDropDownClick (Sender: TObject);
    procedure MapForPolygonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //custom control to select the polygon to report on
  TPolygonSelection = class(TPanel)
  private
    FPolygonLayers: TKeyboardRapidTree;
    FButtonPanel: TPolygonSelectionBtn;
    FMapAlreadyPresent: Boolean;
    FMapWindow: TfrmMap;
    FIsForLocations: boolean;
    procedure AddPolygons(AMapServerLink: TMapServerLink; AParentNode: TFlyNode;
        const AKey: TKeyData);
    procedure SetPolygonSelectionControl(imageList: TImageList);
    procedure ClearPolygonLayers;
    procedure RequestMapForReturnData(ABaseMapKey: TKeyString;
        ACallBackFunction: TRequestorUpdateFunction);
    procedure UpdatePolygonList(KeyList: TKeyList);
    procedure PolygonLayersImageClicked(Sender: TObject; ANode: TFlyNode);
    procedure PopulatePolygonLayers(AMapServerLink: TMapServerLink);
    procedure SelectPolygon(ANode: TFlyNode; const ASelect: Boolean; AUpdateParent: Boolean = True);
    function ReadItemsInPolygon(AConnection: TADOConnection; includePartialOverlap:
        Boolean): string;
    procedure SetIsForLocations(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsForLocations: boolean read FIsForLocations write SetIsForLocations;
  end;

  TfrmParameters = class(TBaseChild)
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    FileOpenDialog: TOpenDialog;
    imgList: TImageList;
    pnlButtons: TPanel;
    pnlSpacer: TPanel;
    pnlTitle: TPanel;
    pnlInstructions: TPanel;
    mmInstructions: TMemo;
    pnlParamBackground: TPanel;
    pnlDecorative: TPanel;
    pnlParams: TPanel;
    ilPolygonSelection: TImageList;
    pmMapForPolygons: TPopupMenu;
    procedure btnFileOpenClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure RestrictedEditCheckIsFloat(Sender: TObject; const iText: String; var ioAccept:
        Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDescription: String;
    FDupParams: TList;
    FParameters: TObjectList;
    FGroupBoxes: TStringList;
    FLastAddedControl: TWinControl;
    FLinkedControls: TObjectList;
    FMapPopupMenus: TObjectList;
    FRadioContainers: TStringList;
    FReportFile: TBaseXmlQueryFile;
    FReportTitle: String;
    FReturnDataControl: TControl;
    FCurrentUseRucksack: TUseRucksack;
    FPolygonChosen: boolean;
    FNotCancelled: Boolean;
    FOrderByList: TOrderByClauses;
    function AddCheckBox(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddComboBox(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddControl(AInputParam: TInputParam; AParent: TWinControl;
        AWidthRatio: integer=60): TWinControl;
    function AddEdit(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    procedure AddEditButtonPanel(AControl: TWinControl; ARowContainer: TScrollbox;
        AInputParam: TInputParam);
    function AddFileOpenButton(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddGridSquareRange(AInputParam: TInputParam; AParent: TWinControl): TGridSquareRange;
    function AddBoundingBox(AInputParam: TInputParam; AParent: TWinControl): TBoundingBox;
    function AddDatePicker(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    procedure AddGroupedControl(AInputParam: TInputParam; AParent: TWinControl;
        AChecked: Boolean);
    function AddLinkedEdit(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddGroupingRadioButton(AInputParam: TInputParam; AParent:
        TWinControl): TGroupingRadioButton;
    function AddLabel(AInputParam: TInputParam; AParent, ALinkedControl: TWinControl): TLabel;
    function AddMultipleControls(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddMultipleControlsCheckUseRucksack(AInputParam: TInputParam;
        AParent: TWinControl): TWinControl;
    function AddRucksackComboBox(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    procedure AddMultipleEntryCountRow(AInputParam: TInputParam; AParent: TWinControl;
        AResizeContainer: boolean);
    function AddSingleOrMultipleControl(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    procedure AddOrderByOptionControls;
    function AddPanel(AParent: TWinControl): TPanel;
    procedure AddParam(AInputParam: TInputParam);
    procedure AddParamControls;
    function AddScrollbox(AInputParam: TInputParam; AParent: TWinControl;
        ATop: integer): TScrollbox;
    function AddRestrictedEdit(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddSingleControl(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    procedure AddSpacer(AParent: TWinControl);
    function AddSpatialRefEdit(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function AddVagueDateEdit(AInputParam: TInputParam; AParent: TWinControl): TWinControl;
    function CreateMapPopupMenu(AOwnerControl: TWinControl): TPopupMenu;
    procedure DropOntoLinkedControl(const Sender: TObject; const AFormat: Integer;
        const ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropSpatialRef(const Sender: TObject; const AFormat: Integer; const
        ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure FocusFirstControl;
    procedure GetDataSourceForm(ASearchType: AddinSearchManager.TSearchType); virtual;
    function GetParamContainer(AInputParam: TInputParam): TWinControl;
    procedure LinkedControlFindData(Sender: TObject);
    procedure LinkedControlGetData(Sender: TObject);
    procedure MapForSpatialRefClick(Sender: TObject);
    procedure MapForBoundingBoxClick(Sender: TObject);
    procedure PutMultipleInputValuesIntoParam(AControl: TWinControl; AParam: TInputParam);
    function ParamNeedsInput(AInputParam: TInputParam): Boolean;
    procedure PrepLinkedControl(AControl: TAddinLinkedEdit);
    procedure PrepMapGetData(Sender: TObject);
    procedure ProcessDupParams(AParams: TList);
    procedure ProcessParams;
    procedure PutInputValuesIntoParam(AParam: TInputParam);
    procedure ResizeParentToAccommodate(AControl: TWinControl);
    procedure SetDescription(const Value: String);
    procedure SetDupParams(const Value: TList);
    procedure SetReportTitle(const Value: String);
    procedure PutSingleInputValuesIntoParam(AControl: TWinControl; AParam: TInputParam);
    procedure PutValuesFromRucksackIntoParam(RucksackList: TComboBox; AParam: TInputParam);
    procedure SpatialRefGetFromMap(Sender: TObject);
    procedure UpdateLinkedControl(KeyList: TKeyList);
    procedure UpdateSpatialRef(KeyList: TKeyList);
    function ValidateContent: Boolean;
    procedure ValidateControls;
    procedure ValidateFilename(AFileName: String);
    procedure ValidateVagueDate(AVagueDateString: String);
    procedure ValidateDate(ADate: String);
    function ValidateLinkedEdit(AControl: TAddinLinkedEdit): Boolean;
    procedure ValidateMultipleControls(AInputParam: TInputParam);
    procedure ValidateSingleControl(AControl: TControl);
    procedure ValidateSpatialRef(const ARef: string);
    procedure AddRucksack(const FullPath: string; const ComboBox: TCustomComboBox);
    procedure AddGroupedControlsForSameParameter(AInputParam: TInputParam; AParent: TWinControl);
    procedure PutGroupedInputValuesIntoParam(AControl: TWinControl; AParam: TInputParam);
    function AddPolygonSelection(AInputParam: TInputParam; AParent:TWinControl): TPolygonSelection;
    function SetupPolygonResults(APolygonSelection: TPolygonSelection;
        includePartialOverlap: Boolean): String;
    procedure SetupDestinationControl(AControl: TWinControl);
  protected
    procedure SetupDestinationControls; override;
  public
    destructor Destroy; override;
    procedure DoUpdateLinkedControl(AControl: TAddinLinkedEdit; const AKey: string);
    procedure EnableDeleteButton(Sender: TObject; AEnable: boolean);
    function GetParamEnabled(AParam: TInputParam): Boolean;
    procedure MultiControlPanelEnter(Sender: TObject);
    procedure MultiControlPanelExit(Sender: TObject);
    procedure RememberRadioContainer(AControl: TWinControl; const AGroupName: string);
    procedure SetReportFile(AReportFile: TBaseXmlQueryFile);
    procedure ValidateRadioGroups;
    property Description: String read FDescription write SetDescription;
    property DupParams: TList read FDupParams write SetDupParams;
    property ReportTitle: String read FReportTitle write SetReportTitle;
  end;

  TFileLinkedEdit = class(TAddinLinkedEdit);

  TAddDeleteButton = class(TImageListButton)
  private
    FInputParam: TInputParam;
    FRowContainer: TScrollbox;
    procedure SetInputParam(const Value: TInputParam);
    procedure SetRowContainer(const Value: TScrollbox);
  public
    property InputParam: TInputParam read FInputParam write SetInputParam;
    property RowContainer: TScrollbox read FRowContainer write SetRowContainer;
  end;

  TAddRowMethod = procedure(AInputParam: TInputParam; AParent: TWinControl;
      AResizeContainer: boolean) of object;

  TAddButton = class(TAddDeleteButton)
  private
    FAddRowMethod: TAddRowMethod;
    procedure SetAddRowMethod(const Value: TAddRowMethod);
  public
    procedure Click; override;
    property AddRowMethod: TAddRowMethod read FAddRowMethod write SetAddRowMethod;
  end;

  TDeleteButton = class(TAddDeleteButton)
  public
    procedure Click; override;
  end;

implementation

uses
  GeneralFunctions, ResourceStrings, CRConstants, VagueDate, DropTarget, DropStruct,
  StrUtils, Maintbar, SpatialRefFuncs, ApplicationSettings, GeneralData, constants,
  IWValidation, Rucksack, DatabaseAccessADO, WizardData, Treecoll;

{$R *.dfm}

type
  TCustomEditAccessor = class(TCustomEdit)
  end;

const
  EDIT_BUTTON_SIZE   = 20;
  BUTTON_PANEL_WIDTH = EDIT_BUTTON_SIZE + 3;
  EDIT_BUTTON_TOP    = 23;
  PANEL_PADDING      =  4;
  TO_PANEL_WIDTH     = 12;

  // Images indexes for the tree nodes
  CHECKED_IMG_IDX      = 0;
  UNCHECKED_IMG_IDX    = 1;
  CHECKED_GREY_IMG_IDX = 2;
  WORLD_IMG_IDX        = 3;


{-==============================================================================
    TFrmParameters
===============================================================================}
{-------------------------------------------------------------------------------
  Destructor.
}
destructor TfrmParameters.Destroy;
begin
  FParameters.Free;
  FGroupBoxes.Free;
  FDupParams.Free;
  FMapPopupMenus.Free;
  FLinkedControls.Free;
  FRadioContainers.Free;
  inherited;
end;  // TFrmParameters.Destroy

{-------------------------------------------------------------------------------
  Add a checkbox to the grid.
}
function TfrmParameters.AddCheckBox(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TCheckBox.Create(Self);
  FLastAddedControl := Result;
  if AInputParam.default = 'Y' then
    TCheckBox(Result).Checked := True;
end;  // TFrmParameters.AddCheckBox

{-------------------------------------------------------------------------------
  Add a combobox to the grid. 
}
function TfrmParameters.AddComboBox(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
var
  i: Integer;
  j: integer;
begin
  Result := TAddinIDComboBox.Create(Self);
  FLastAddedControl := Result;
  with TAddinIDComboBox(Result) do begin
    BevelKind := bkFlat;
    Parent := AParent;
  end;

  with AInputParam do
    if Assigned(Options) then begin
        TAddinIDComboBox(Result).PopulateContent;
        for i := 0 to Options.Count - 1 do begin
          TAddinIDComboBox(Result).Add
                            (Options.Items[i].Name, Options.Items[i].Value);
          If Options.Items[i].Name = AInputParam.default then j := i;
        end;
        TAddinIDComboBox(Result).ItemIndex := j;
    end;

end;  // TFrmParameters.AddComboBox

{-------------------------------------------------------------------------------
  Method that decides what controls to place on the panel.
}
function TfrmParameters.AddControl(AInputParam: TInputParam; AParent:
    TWinControl; AWidthRatio: integer=60): TWinControl;
var
  lEffectiveWidth: integer;
begin
  with AInputParam do
    if      DataType = dtOptionSet then Result := AddComboBox(AInputParam, AParent)
    else if DataType = dtCSVFile then Result := AddFileOpenButton(AInputParam, AParent)
    else if DataType = dtVagueDate then Result := AddVagueDateEdit(AInputParam, AParent)
    else if DataType = dtNumber then Result := AddRestrictedEdit(AInputParam, AParent)
    else if DataType = dtTrueFalse then Result := AddCheckBox(AInputParam, AParent)
    else if DataType in [dtLocation, dtTaxon, dtName, dtIndividual, dtOrganisation, dtTaxon] then
      // Multiple controls have a TScrollBox as their grandparent
      if (AParent.Parent is TScrollBox) or (AInputParam.UseRucksack = urNo) or (FCurrentUseRucksack = urNo) then
        Result := AddLinkedEdit(AInputParam, AParent)
      else begin
        Result := AddRucksackComboBox(AInputParam, AParent);
        AWidthRatio := AWidthRatio * 2 div 3;   // Reduce the width for longer label
      end
    else if DataType = dtGridSquareRange then Result := AddGridSquareRange(AInputParam, AParent)
    else if DataType = dtBoundingBox then Result := AddBoundingBox(AInputParam, AParent)
    else if DataType = dtSpatialRef then Result := AddSpatialRefEdit(AInputParam, AParent)
    else if DataType = dtDate then Result := AddDatePicker(AInputParam, AParent)
    else if DataType = dtSamplesInPolygon then Result := AddPolygonSelection(AInputParam, AParent)
    else if DataType = dtLocationsInPolygon then Result := AddPolygonSelection(AInputParam, AParent)
    else Result := AddEdit(AInputParam, AParent);
  with Result do begin
    Parent := AParent;
    Ctl3D := false;
    Top := 0;
    // calculate the width allowing for the panel border
    lEffectiveWidth := AParent.ClientWidth;
    if AParent is TPanel then
      lEffectiveWidth := lEffectiveWidth - (2 * TPanel(AParent).BorderWidth);
    Width := lEffectiveWidth * AWidthRatio div 100; // 3/5 width
    Left := lEffectiveWidth - Width;
    // tweak the sizing for single controls
    if (AInputParam.EntryCount=1) then
      Width := Width + 7;
    Anchors := [akLeft, akTop, akRight];
  end;
end;  // TFrmParameters.AddControl

{-------------------------------------------------------------------------------
  Add an editbox to the grid.
}
function TfrmParameters.AddEdit(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TEdit.Create(Self);
  FLastAddedControl := Result;
  TCustomEdit(Result).text := AInputParam.default;
end;  // TFrmParameters.AddEdit

{-------------------------------------------------------------------------------
  Creates a panel aligned into the right of the current panel with add and edit
     buttons, used for flexible number of entry controls.
}
procedure TfrmParameters.AddEditButtonPanel(AControl: TWinControl;
    ARowContainer: TScrollbox; AInputParam: TInputParam);
var
  lButtonPanel: TPanel;
  lAddButton: TAddButton;
  lDeleteButton: TDeleteButton;

    procedure CommonButtonSetup(AButton: TAddDeleteButton);
    begin
      with AButton do begin
        Parent := lButtonPanel;
        RowContainer := ARowContainer;
        InputParam := AInputParam;
      end;
    end;

begin
  lButtonPanel := TPanel.Create(self);
  with lButtonPanel do begin
    Parent := AControl;
    SetBounds(Parent.Width-BUTTON_PANEL_WIDTH-1, 0, BUTTON_PANEL_WIDTH,
      EDIT_BUTTON_TOP+EDIT_BUTTON_SIZE*2+2);
    Anchors := [akTop, akRight];
    Width := BUTTON_PANEL_WIDTH;
    BorderStyle := bsNone;
    BevelOuter := bvNone;
  end;
  ResizeParentToAccommodate(lButtonPanel);
  lAddButton := TAddButton.Create(self);
  CommonButtonSetup(lAddButton);
  with lAddButton do begin
    SetBounds(0, EDIT_BUTTON_TOP, EDIT_BUTTON_SIZE, EDIT_BUTTON_SIZE);
    ImageList := dmFormActions.ilButtons;
    ImageIndex := 2;
    AddRowMethod := AddMultipleEntryCountRow;
  end;
  lDeleteButton := TDeleteButton.Create(self);
  CommonButtonSetup(lDeleteButton);
  with lDeleteButton do begin
    SetBounds(0, EDIT_BUTTON_TOP+EDIT_BUTTON_SIZE, EDIT_BUTTON_SIZE, EDIT_BUTTON_SIZE);
    ImageList := dmFormActions.ilButtons;
    ImageIndex := 4;
    Enabled := False;
  end;
  // store the delete button against the scrollbox tag, so that it can be enabled
  // and disabled
  ARowContainer.Tag := Integer(lDeleteButton);
end;

{-------------------------------------------------------------------------------
  Add a linked edit to the grid. The linked edit is used because it contains a text field and
      a button. The button is used so the the File Open dialog can be brought up and the user
      can select the CSV file they want to use.
}
function TfrmParameters.AddFileOpenButton(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TFileLinkedEdit.Create(Self);
  FLastAddedControl := Result;
  with TFileLinkedEdit(Result) do begin
    OnGetData := btnFileOpenClick;
    Height := 19;
    ImageList := imgList;
    ImageIndex := 0;
  end;
end;  // TFrmParameters.AddFileOpenButton

{-------------------------------------------------------------------------------
  Add a control which is one of a group (i.e. only one active at a time)
}
procedure TfrmParameters.AddGroupedControl(AInputParam: TInputParam; AParent:
    TWinControl; AChecked: Boolean);
var
  lRadioButton: TGroupingRadioButton;
  lParent: TWinControl;
begin
  lRadioButton := AddGroupingRadioButton(AInputParam, AParent);
  lRadioButton.Parent.Height := lRadioButton.Height + 3; // because autosize doesn't work here
  ResizeParentToAccommodate(lRadioButton.Parent);
  lParent := AddSingleOrMultipleControl(AInputParam, AParent);
  ResizeParentToAccommodate(lParent);
  // link radiobuttons to the controls
  if Assigned(lRadioButton) then begin
    lRadioButton.Control := lParent;
    lRadioButton.Checked := AChecked;
    lRadioButton.EnableControls(lParent);
  end;
end;

{-------------------------------------------------------------------------------
  Add a linked edit control for location parameters
}
function TfrmParameters.AddLinkedEdit(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TAddinLinkedEdit.Create(Self);
  case AInputParam.DataType of
    dtLocation: Result.Tag := ST_LOCATION;
    dtName: Result.Tag := ST_NAME;
    dtIndividual: Result.Tag := ST_INDIVIDUAL;
    dtOrganisation: Result.Tag := ST_ORGANISATION;
    dtTaxon: Result.Tag := ST_SPECIES;
  end;
  PrepLinkedControl(TAddinLinkedEdit(Result));
end;

{-------------------------------------------------------------------------------
  Feed in parameters to this Parameter Entry form using this public method.
}
procedure TfrmParameters.AddParam(AInputParam: TInputParam);
var
  lParentControl: TWinControl;
begin
  if Assigned(AInputParam) then begin
    if AInputParam.DataType = dtCurrentUserID then
      FParameters.Add(AInputParam)
    else if ParamNeedsInput(AInputParam) then begin
      lParentControl := GetParamContainer(AInputParam);
      if AInputParam.SelectOneOfGroup='' then
        if AInputParam.UseRucksack = urOptional then
          AddGroupedControlsForSameParameter(AInputParam, lParentControl)
        else
          AddSingleOrMultipleControl(AInputParam, lParentControl)
      else
        AddGroupedControl(AInputParam, lParentControl, False);

      AInputParam.InputControl := FLastAddedControl;
      FParameters.Add(AInputParam);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Add a control which is one of a group (i.e. only one active at a time),
    where all the controls in the group relate to the same parameter
}
procedure TfrmParameters.AddGroupedControlsForSameParameter(
    AInputParam: TInputParam; AParent: TWinControl);
begin
  try
    FCurrentUseRucksack := urNo;
    AddGroupedControl(AInputParam, AParent, True);
    FCurrentUseRucksack := urYes;
    AddGroupedControl(AInputParam, AParent, False);
  finally
    FCurrentUseRucksack := urOptional;
    FLastAddedControl := AParent;
  end;
end;

{-------------------------------------------------------------------------------
  Adds a radio button for a grouped control
}
function TfrmParameters.AddGroupingRadioButton(AInputParam: TInputParam;
    AParent: TWinControl): TGroupingRadioButton;
var
  lCaption: string;
begin
  Result := TGroupingRadioButton.Create(self);
  with Result do begin
    Parent := AParent;
    if (AInputParam.SelectOneOfGroup = '') then
      RememberRadioContainer(AParent, AInputParam.Description)
    else
      RememberRadioContainer(AParent, AInputParam.SelectOneOfGroup);
    Top := 1000;
    Align := alTop;
    Width := Parent.Width - Left * 2;
    case FCurrentUseRucksack of
      urNo:
        Caption := ResStr_ManualParamValuesCaption;
      urYes:
        Caption := ResStr_RucksackParamValuesCaption;
      urOptional: begin
        lCaption := AInputParam.Description;
        if Length(lCaption) <= 30 then
          Caption := Format(ResStr_UseParameter, [lCaption])
        else
          Caption := Format(ResStr_UseParameter, [LeftStr(lCaption, 30)+'...']);
      end;
    end;    // case FCurrentUseRucksack of
    Checked := False;
  end;
end;

{-------------------------------------------------------------------------------
  Add a label to the left of a control
}
function TfrmParameters.AddLabel(AInputParam: TInputParam; AParent, ALinkedControl:
    TWinControl): TLabel;
begin
  Result := TLabel.Create(Self);
  with Result do begin
    Parent := AParent;
    if AParent.Parent is TGroupBox then
      Left := 16
    else
      Left := 0;
    Caption := AInputParam.Description;
    case AInputParam.DataType of
      dtText: Caption := Caption + ' (' + ResStr_Text +')';
      dtNumber: Caption := Caption + ' (' + ResStr_Number + ')';
      dtDate: Caption := Caption + ' (' + ResStr_Date + ')';
      dtVagueDate: Caption := Caption + ' (' + ResStr_VagueDate + ')';
      dtCSVFile: Caption := Caption + ' (' + ResStr_FilePath + ')';
      dtLocation: Caption := Caption + ' (' + ResStr_Location + ')';
      dtTaxon: Caption := Caption + ' (' + ResStr_Taxon + ')';
      dtName: Caption := Caption + ' (' + ResStr_Name + ')';
      dtIndividual: Caption := Caption + ' (' + ResStr_Individual + ')';
      dtOrganisation: Caption := Caption + ' (' + ResStr_Organisation + ')';
      dtGridSquareRange: Caption := Caption + ' (' + ResStr_GridSquareRange + ')';
      dtBoundingBox: Caption := Caption + ' (' + ResStr_BoundingBox + ')';
      dtSpatialRef: Caption := Caption + ' (' + ResStr_SpatialRef + ')';
      dtSamplesInPolygon: Caption := Caption + ' (' + ResStr_Polygon + ')';
      dtLocationsInPolygon: Caption := Caption + '(' + ResStr_Polygon + ')';
    end;

    // Add indication that parameter values are supplied by a rucksack
    if (ALinkedControl is TComboBox) and (AInputParam.UseRucksack = urYes) then
      if (RightStr(Caption, 1) = ')') then
        Caption := LeftStr(Caption, Length(Caption) - 1) + ' - ' + ResStr_FromRucksack + ')'
      else
        Caption := Caption + '(' + ResStr_FromRucksack + ')';
        
    WordWrap := True;
    if AInputParam.EntryCount=1 then
      Width := AParent.Width * 2 div 5 - PANEL_PADDING*2 - Left
    else
      Width := AParent.Width - Left - 8;
    // force number of lines to recalculate
    Autosize := False;
    Autosize := True;
    // Vertically centre control on the label, or top align if too high then
    // centre control on label
    if Assigned(ALinkedControl) then begin
      if ALinkedControl.Height >= Height then
        Top := ALinkedControl.Top + (ALinkedControl.Height - Height) div 2
      else begin
        Top := 0; // might be padded to 4 - just go as high as we can
        ALinkedControl.Top := Top + (Height - ALinkedControl.Height) div 2;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Adds a parameter entry set for a control which has entrycount<>1,
    taking into account the value of the userucksack attribute (if set)
}
function TfrmParameters.AddMultipleControlsCheckUseRucksack(
  AInputParam: TInputParam; AParent: TWinControl): TWinControl;
begin
  case AInputParam.UseRucksack of
    urNo:       Result := AddMultipleControls(AInputParam, AParent);
    urYes:      Result := AddSingleControl(AInputParam, AParent);
    urOptional:
      case FCurrentUseRucksack of
        urNo:       Result := AddMultipleControls(AInputParam, AParent);
        urYes:      Result := AddSingleControl(AInputParam, AParent);
        urOptional:
          raise EParametersException.Create(ResStr_CannotUseOptionalCurrentRucksack);
      else
        raise EParametersException.Create(ResStr_BadUseRucksackValue);
      end;    // case FCurrentUseRucksack of
  else
    raise EParametersException.Create(ResStr_BadUseRucksackValue);
  end;    // case AInputParam.UseRucksack of
end;

{-------------------------------------------------------------------------------
  Adds a parameter entry set for a control which has entrycount<>1
}
function TfrmParameters.AddMultipleControls(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
var
  lParent: TScrollbox;
  lCount : integer;
  i: integer;
  lOuterPanel: TPanel;
begin
  lOuterPanel := AddPanel(AParent);
  with AddLabel(AInputParam, lOuterPanel, nil) do begin
    Top := 8;
    lParent := AddScrollbox(AInputParam, lOuterPanel, Top + Height + 6);
  end;
  if AInputParam.EntryCount=-1 then
    AddEditButtonPanel(lOuterPanel, lParent, AInputParam);
  // if -1 used, this means any number of controls, so start with 1
  lCount := Max(1, AInputParam.EntryCount);
  for i := 1 to lCount do
    AddMultipleEntryCountRow(AInputParam, lParent, True);
  if lCount=1 then begin
    // if only 1 visible, bear in mind more can be added so allow a bit more space
    lParent.Height := lParent.Height * 3;
    ResizeParentToAccommodate(lParent);
  end;
  // record the scrollbox as the control linked to the param, so it can find all
  // the content
  FLastAddedControl := lParent;
  Result := lOuterPanel;
end;

{-------------------------------------------------------------------------------
  Add a combobox containing a list of all the users rucksacks
}
function TfrmParameters.AddRucksackComboBox(AInputParam: TInputParam;
  AParent: TWinControl): TWinControl;
var
  lPath: string;
  lSearchRec: TSearchRec;
begin
  Result := TComboBox.Create(Self);
  FLastAddedControl := Result;
  with TComboBox(Result) do begin
    BevelKind := bkFlat;
    Parent := AParent;
    Style := csDropDownList;
    Width := 121;
  end;

  // Determine the rucksack path
  if (AppSettings.RucksackPath = '') then
    lPath := ExtractFilePath(Application.ExeName)
  else
    lPath := AppSettings.RucksackPath;

  // Add all the rucksacks in the folder to the dropdown list
  if FindFirst(lPath + '*.ruk', 0, lSearchRec) = 0 then
  begin
    //Add the file to the menu
    AddRucksack(lPath + lSearchRec.Name, TComboBox(Result));
    //Add a new menu item for each remaining file
    while FindNext(lSearchRec) = 0 do
      AddRucksack(lPath + lSearchRec.Name, TComboBox(Result));
    //Free the search results
    FindClose(lSearchRec);
  end;
end;

{-------------------------------------------------------------------------------
  Adds a rucksack to the dropdown list of rucksacks
}
procedure TfrmParameters.AddRucksack(const FullPath: string;
  const ComboBox: TCustomComboBox);
var lRucksackListItem: TRucksackListItem;
    lFileName: String;
begin
  //Create object to hold full name
  lRucksackListItem := TRucksackListItem.Create;
  lRucksackListItem.FileName := FullPath;

  //Get filename without path
  lFileName := ExtractFileName(FullPath);

  //Get filename without extension
  lFileName := Copy(lFileName, 1, Length(lFileName) - 4);

  //Add filename without extension and path to combo
  ComboBox.Items.AddObject(lFileName, lRucksackListItem);
end;  // AddRucksack

{-------------------------------------------------------------------------------
  If required, creates a control to allow the user to select the Order By
}
procedure TfrmParameters.AddOrderByOptionControls;
var
  i: integer;
  lOrderByParam: TInputParam;
  lOrderByItem: TOrderByOption;
  lOrderByList: TOrderByOptions;
begin
  //  If there are any orderby clauses
  if FOrderByList.Count>1 then
  begin
    //  Get a list of OrderBy Clauses
    lOrderByList := TOrderByOptions.Create;
    // Read all the OrderBy clauses into the options of a single parameter
    for i := 0 to FOrderByList.Count-1 do begin
      lOrderByItem := TOrderByOption.Create;
      lOrderByItem.Name := FOrderByList[i].Name;
      lOrderByItem.Value :=FOrderByList[i].Field;
      lOrderByList.Add(lOrderByItem);
    end;
    //  Now Create a TInputParam parameter instance
    lOrderByParam := TInputParam.Create( dtOptionSet,
                                    'Select Sort Order',
                                    TConditionOptions(lOrderByList));
    //  And add this to the Forms Parameters list
    AddParam(lOrderByParam);
  end;  //  IF we have some order by clauses
end;

{-------------------------------------------------------------------------------
  Add a new container parent to the top of the current container.
}
function TfrmParameters.AddPanel(AParent: TWinControl): TPanel;
begin
  Result := TPanel.Create(self);
  with Result do begin
    Parent := AParent;
    Width := AParent.ClientWidth; // because aligning to top doesn't occur quick enough for our layout code
    // ensure that when aligned to top, it goes under any previous controls
    Top := 3000;
    Align := alTop;
    BorderWidth := PANEL_PADDING; // padding
    BorderStyle := bsNone;
    BevelOuter := bvNone;
    Caption := '';
    Autosize := true;
  end;
end;

{-------------------------------------------------------------------------------
  Adds the list of parameter controls required to the dialog
}
procedure TfrmParameters.AddParamControls;
var
  lParamNames: TStringList;
  i: integer;
begin
  // create a string list to track param names we have used, as we don't
  // display duplicates
  lParamNames := TStringList.Create;
  try
    lParamNames.Sorted := True;
    with FReportFile do begin
      for i := 0 to InputParamCount-1 do begin
        if lParamNames.IndexOf(InputParam[i].Description)=-1 then begin
          AddParam(InputParam[i]);
          lParamNames.Add(InputParam[i].Description);
        end
        else
          FDupParams.Add(InputParam[i]);
      end; // for
    end;
  finally
    lParamNames.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Add a new container parent to the top of the current container, using a scroll
     box so that if many controls in a group it is displayed with a scrollbar
}
function TfrmParameters.AddScrollbox(AInputParam: TInputParam; AParent:
    TWinControl; ATop: integer): TScrollbox;
begin
  Result := TScrollbox.Create(self);
  with Result do begin
    Parent := AParent;
    if AInputParam.SelectOneOfGroup<>'' then // this is a nested multiple control
      Left := 16
    else
      Left := 0;
    if AInputParam.EntryCount=-1 then
      // extra space allowed for button panel
      Width := AParent.ClientWidth-BUTTON_PANEL_WIDTH-1-Left
    else
      Width := AParent.ClientWidth-4-Left;
    Top := ATop;
    Height := 10; // arbitrary - it will be resized later
    Anchors := [akLeft, akTop, akRight];
    BevelOuter := bvNone;
    ctl3D := false;
  end;
  AParent.ClientHeight := Max(AParent.ClientHeight, Result.Height + Result.Top);
end;

{-------------------------------------------------------------------------------
  Add a restricted edit to the grid. A restricted edit is used rather than a number edit,
      because number edits have difficulty with negative numbers.
}
function TfrmParameters.AddRestrictedEdit(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TRestrictedEdit.Create(Self);
  FLastAddedControl := Result;
  with TRestrictedEdit(Result) do
    OnCheckText := RestrictedEditCheckIsFloat;

end;  // TFrmParameters.AddRestrictedEdit

{-------------------------------------------------------------------------------
  Adds a normal parameter to the list
}
function TfrmParameters.AddSingleControl(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
var
  lParent: TPanel;
begin
  lParent := AddPanel(AParent);
  AddControl(AInputParam, lParent);
  AddLabel(AInputParam, lParent, FLastAddedControl);
  // refresh to ensure the panel height is accurate
  lParent.Autosize := false;
  lParent.Autosize := true;
  Result := lParent;
end;

{-------------------------------------------------------------------------------
  Add in a spacer panel for appearances
}
procedure TfrmParameters.AddSpacer(AParent: TWinControl);
begin
  with AddPanel(AParent) do begin
    Autosize := false;
    Height := PANEL_PADDING*2;
  end;
end;

{-------------------------------------------------------------------------------
  Add a vague date edit to the grid.
}
function TfrmParameters.AddVagueDateEdit(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TVagueDateEdit.Create(Self);
  FLastAddedControl := Result;
end;  // TFrmParameters.AddVagueDateEdit

{-------------------------------------------------------------------------------
  Handle a click on the button of the linked edit.
}
procedure TfrmParameters.btnFileOpenClick(Sender: TObject);
begin
  FileOpenDialog.Execute;
  if Sender is TFileLinkedEdit then
    TFileLinkedEdit(Sender).Text := FileOpenDialog.Filename;
end;  // TFrmParameters.btnFileOpenClick

{-------------------------------------------------------------------------------
  Close dialog if validation succeeds.
}
procedure TfrmParameters.btnOKClick(Sender: TObject);
begin
  ProcessParams;
  ValidateContent;
  FReportFile.BuildReport;
  FNotCancelled := True;
  Close;
end;  // TFrmParameters.btnOKClick

{-------------------------------------------------------------------------------
  Ensure the first control in the grid gets focus.
}
procedure TfrmParameters.FocusFirstControl;
var                               
  lEntryPanelHeight: integer;
  index: integer;

  {-----------------------------------------------------------------------------
    Checks whether there are any input parameters with assigned controls, and if
    so sets index to the index of the first one.
  }
  function HasInputControls: Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FParameters.Count - 1 do
      if Assigned(TInputParam(FParameters.Items[i]).InputControl) then begin
        Result := True;
        index := i;
        Exit;
      end;
  end;

begin
  if HasInputControls then begin
    if TWinControl(TInputParam(FParameters.Items[index]).InputControl).CanFocus then
      TWinControl(TInputParam(FParameters.Items[index]).InputControl).SetFocus;
    // set params panel height
    lEntryPanelHeight := pnlParams.Controls[pnlParams.ControlCount-1].Top +
        pnlParams.Controls[pnlParams.ControlCount-1].Height + 16;
    ClientHeight := lEntryPanelHeight + pnlSpacer.Height + mmInstructions.Height +
        pnlButtons.Height;
    if pnlTitle.Visible then
      Height := Height + pnlTitle.Height;
    pnlDecorative.Height := pnlParamBackground.Height;
  end;
end;  // TFrmParameters.FocusFirstControl

{-------------------------------------------------------------------------------
  A public method used by other objects to get the values entered by the user
      back out of this Parameter Entry form.
}
procedure TfrmParameters.PutInputValuesIntoParam(AParam: TInputParam);
var
  lControl: TWinControl;
begin
  AParam.Values.Clear;
  if AParam.Active then begin
    lControl := AParam.InputControl;
    if (lControl is TAddinLinkedEdit) then
      if TAddinLinkedEdit(lControl).Key='' then
        DoCheck(TAddinLinkedEdit(lControl), TAddinLinkedEdit(lControl).Tag);
    // note a control will not be available if a duplicate param is present
    if Assigned(lControl) then begin
      if lControl is TScrollBox then
        PutMultipleInputValuesIntoParam(lControl, AParam)
      else if (lControl is TGroupBox) then
        PutGroupedInputValuesIntoParam(lControl, AParam)
      else
        PutSingleInputValuesIntoParam(lControl, AParam);
    end
    else if (AParam.DataType=dtOptionSet) and (AParam.Options.Count=1) then
      AParam.Values.Add(AParam.Options[0].Value);
  end;
end;  // TFrmParameters.PutInputValuesIntoParam

{-------------------------------------------------------------------------------
  Puts the values entered by a user into a parameter that can accept direct
    entry or via specifying a rucksack to indicate its values. }
procedure TfrmParameters.PutGroupedInputValuesIntoParam(AControl: TWinControl;
  AParam: TInputParam);
var
  i, j: integer;
  RadioButtonControl: TWinControl;
  ChildControl: TControl;
begin
  for i := 0 to Pred(AControl.ControlCount) do begin
    if AControl.Controls[i] is TGroupingRadioButton then
      if TGroupingRadioButton(AControl.Controls[i]).Checked then begin
        RadioButtonControl := TGroupingRadioButton(AControl.Controls[i]).Control;
        for j := 0 to Pred(RadioButtonControl.ControlCount) do begin
          ChildControl := RadioButtonControl.Controls[j];
          if ChildControl is TScrollBox then begin
            PutMultipleInputValuesIntoParam(TScrollBox(ChildControl), AParam);
            Break;
          end else if ChildControl is TWinControl then begin
            PutSingleInputValuesIntoParam(TWinControl(ChildControl), AParam);
            Break;
          end;    // if ChildControl is TScrollBox
        end;    // for j := 0 to Pred(RadioButtonControl.ControlCount)
      end;    // if TGroupingRadioButton(AControl.Controls[i]).Checked
  end;    // for i := 0 to Pred(AControl.ControlCount)
end;

{-------------------------------------------------------------------------------
  Returns true if a param is active.  Can be false if one of a select group
    which has not been selected.
}
function TfrmParameters.GetParamEnabled(AParam: TInputParam): Boolean;
begin
  Result := true; // default - used when an optionset has only one option so no control
  if Assigned(AParam.InputControl) then
      Result := AParam.InputControl.Enabled;
end;

{-------------------------------------------------------------------------------
  Input parameters go straight onto the form, unless they are grouped, in which
     case a group box is created (one for each group name)
}
function TfrmParameters.GetParamContainer(AInputParam: TInputParam):
    TWinControl;
var
  lBoxIndex: integer;
  lNewBox: TGroupBox;
  lGroupBoxName: string;
begin
  if (AInputParam.SelectOneOfGroup = '') and (AInputParam.UseRucksack <> urOptional) then
    // normal parameter
    Result := pnlParams
  else begin
    // select one of group parameter needs a group box which might already exist
    if (AInputParam.UseRucksack = urOptional) then
      lGroupBoxName := AInputParam.Description
    else
      lGroupBoxName := AInputParam.SelectOneOfGroup;

    lBoxIndex := FGroupBoxes.IndexOf(lGroupBoxName);
    if lBoxIndex>-1 then
      Result := TWinControl(FGroupBoxes.Objects[lBoxIndex])
    else begin
      AddSpacer(pnlParams);            
      lNewBox := TGroupBox.Create(self);
      lNewBox.Parent := pnlParams;
      lNewBox.Top := 1000;
      lNewBox.Align := alTop;
      lNewBox.Height := 20; // arbitrary - gets adjusted as controls are added
      lNewBox.Caption := Format(ResStr_SelectGroupParamYouRequire,
          [lGroupBoxName]);
      AddSpacer(lNewBox);
      Result := lNewBox;
      FGroupBoxes.AddObject(lGroupBoxName, lNewBox);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  This method is called to check that the values entered in the restricted edit a floating
      point values.
}
procedure TfrmParameters.RestrictedEditCheckIsFloat(Sender: TObject; const iText: String; var
    ioAccept: Boolean);
begin
  inherited;
  // Check valid characters
  ioAccept := IsFloat(iText) or (iText = '') or (iText = '-') or (iText = '.');
end;  // TFrmParameters.RestrictedEditCheckIsFloat

{-------------------------------------------------------------------------------
  Provide a description/instructions for the report dialog.
}
procedure TfrmParameters.SetDescription(const Value: String);
begin
  mmInstructions.Lines.Text := Value;
  mmInstructions.Height := mmInstructions.Lines.Count*13+16;
  pnlInstructions.Height := mmInstructions.Height;
end;  // TFrmParameters.SetDescription

{-------------------------------------------------------------------------------
  Any parameter that is a combo box with only 1 entry doesn't need to be
       displayed
}
function TfrmParameters.ParamNeedsInput(AInputParam: TInputParam): Boolean;
begin
  Result := (AInputParam.DataType <> dtOptionSet) or (AInputParam.Options.Count > 1);
end;

{-------------------------------------------------------------------------------
  The list of parameters supplied to this method are those parameters that were not 
  displayed on the input parameters form because they were repeats of other parameters.  
  Therefore they pick up the same value as the parameter that was used. 
}
procedure TfrmParameters.ProcessDupParams(AParams: TList);
var
  i: Integer;
  j: Integer;
begin
  // loop through params that are duplicates
  for i := 0 to AParams.Count-1 do begin
    // loop through to find the first param with same name - the one that was
    // displayed on the input params form
    for j := 0 to FReportFile.InputParamCount-1 do begin
      if CompareText(TInputParam(AParams[i]).Description,
          FReportFile.InputParam[j].Description)=0 then begin
        // reusing a param name only works if param data type is the same
        if TInputParam(AParams[i]).DataType <> FReportFile.InputParam[j].DataType then
          raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
              Format(ResStr_DupParamTypeProblem, [FReportFile.InputParam[j].Description]));
        TInputParam(AParams[i]).Values.Assign(FReportFile.InputParam[j].Values);
        Break; // from inner loop
      end;
    end;
  end;
end;  // TReportFile.ProcessDupParams

{-------------------------------------------------------------------------------
  Read the values back from the controls into the input param instances
}
procedure TfrmParameters.ProcessParams;
var
  i: integer;
  lCursor: TCursor;
begin
  lCursor := Screen.Cursor;
  try
    for i := 0 to FReportFile.InputParamCount-1 do begin
      FReportFile.InputParam[i].Active := GetParamEnabled(FReportFile.InputParam[i]);
      PutInputValuesIntoParam(FReportFile.InputParam[i]);
    end;
    //  Get the OrderBy Value also and add to the list
    if FOrderByList.count > 1 then
    begin
      PutInputValuesIntoParam(FParameters[FReportFile.InputParamCount] as TInputParam);
      if (FParameters[FReportFile.InputParamCount] as TInputParam).Values.Count > 0 then begin
        i := FOrderByList.FindByValue(
            (FParameters[FReportFile.InputParamCount] as TInputParam).Values[0]);
        FOrderByList.Selected := i;
      end else raise EParametersException.CreateNonCritical(
        Format(ResStr_ParameterRequired, [ResStr_SortOrder]));
    end
    else
      //  We have only one clause so select it
      FOrderByList.Selected := 0;
    ProcessDupParams(FDupParams);
  finally
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
  Incrrease the height of a container box to accomodate an added control
}
procedure TfrmParameters.ResizeParentToAccommodate(AControl: TWinControl);
var
  lMaxHeight: integer;
  lResizeParent: TWinControl;
begin
  // we are trying to resize the parent, but if its aligned to client then
  // we need to resize the parent's container instead
  if AControl.Parent.Align=alClient then
    lResizeParent := AControl.Parent.Parent
  else
    lResizeParent := AControl.Parent;
  if AControl.Parent is TScrollBox then
    // use scrollbars for large scrollboxes
    lMaxHeight := 150
  else
    lMaxHeight := 1000;
  if AControl is TPanel then
    if TPanel(AControl).Autosize then begin
      // force autosize controls to update their height properly
      TPanel(AControl).Autosize := false;
      TPanel(AControl).Autosize := true;
    end;

  lResizeParent.Height := Min(lMaxHeight,
      Max(lResizeParent.Height, AControl.Height + AControl.Top+2));
end;

procedure TfrmParameters.SetDupParams(const Value: TList);
begin
  FDupParams := Value;
end;

{-------------------------------------------------------------------------------
  Initialise the dialog according to the report file parameters and details
}
procedure TfrmParameters.SetReportFile(AReportFile: TBaseXmlQueryFile);
begin
  FReportFile := AReportFile;
  ReportTitle := FReportFile.Title;
  Description := FReportFile.Description;
  AddParamControls;
  if FReportFile.ReportSQL.OrderByClauses.Count > 0 then
    FOrderByList := FReportFile.ReportSQL.OrderByClauses
  else
    FOrderByList := FReportFile.ReportSQL.OutputTableOrderByClauses;
  AddOrderByOptionControls;
  FocusFirstControl;
  Top := (frmMain.ClientHeight - Height - frmMain.CoolbarMain.height) div 2;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmParameters.SetReportTitle(const Value: String);
begin
  pnlTitle.Caption := Value;
  pnlTitle.Visible := pnlTitle.Caption <> '';
end;  // TFrmParameters.SetReportTitle

{-------------------------------------------------------------------------------
  Ensure that all of the controls on the grid have data in them.
}
function TfrmParameters.ValidateContent: Boolean;
var
  i,lValIdx: Integer;
  lFoundVal: boolean;
begin
  // This routine checks first if there is an entry for all parameters
  Result := True;
  //  For each paramter in the forms' list
  for i := 0 to FParameters.Count-1 do begin
    //  Check if it has a value assigned
    lFoundVal := false;
    for lValIdx := 0 to TInputParam(FParameters[i]).Values.Count-1 do begin
      if (TInputParam(FParameters[i]).Values[lValIdx] <> '') and {numbers}
          (TInputParam(FParameters[i]).Values[lValIdx] <> '''''') {text} then begin
        lFoundVal := true;
        break; // from loop
      end;
    end;
    if not lFoundVal then
      //  If not, see if it is an optionset which has not been shown as it only
      //  has one entry
      if assigned(FParameters[i]) then
        if Assigned(TInputParam(FParameters[i]).InputControl) then
          if TInputParam(FParameters[i]).InputControl.Enabled then
            raise EParametersException.CreateNonCritical(Format(ResStr_ParameterRequired,
              [TInputParam(FParameters[i]).Description]))
  end;  //  of the For loop
  ValidateRadioGroups;
  //  Then does specific checks on certain types
  //  i.e. Vague Date and Filename
  ValidateControls;
end;  // TFrmParameters.ValidateContent

{-------------------------------------------------------------------------------
  Validation specific for each control type
}
procedure TfrmParameters.ValidateControls;
var
  i: Integer;
begin
  for i := 0 to FParameters.Count - 1 do
    with TInputParam(FParameters[i]) do
      if InputControl is TScrollBox then
        ValidateMultipleControls(TInputParam(FParameters[i]))
      else
        ValidateSingleControl(InputControl);
end;  // TFrmParameters.ValidateControls

{-------------------------------------------------------------------------------
}
procedure TfrmParameters.ValidateFilename(AFileName: String);
begin
  if not FileExists(AFileName) then
    raise EParametersException.CreateNonCritical(Format(ResStr_FileDoesNotExist,
                                                                  [AFileName]));
end;  // TFrmParameters.ValidateFilename

{-------------------------------------------------------------------------------
  Raises exception if a vague date can't be parsed
}
procedure TfrmParameters.ValidateVagueDate(AVagueDateString: String);
begin
  try
    StringToVagueDate(AVagueDateString);
  except
    on EConvertError do
      raise EParametersException.CreateNonCritical(Format(ResStr_InvalidVagueDate, [AVagueDateString]));
    on EVagueDateError do
      raise EParametersException.CreateNonCritical(Format(ResStr_InvalidVagueDate, [AVagueDateString]));
  end;
end;  // TFrmParameters.ValidateVagueDate

{-------------------------------------------------------------------------------
  Raises exception if a date can't be parsed
}
procedure TfrmParameters.ValidateDate(ADate: String);
begin
  try
    StrToDate(ADate);
  except
    on EConvertError do
      raise EParametersException.CreateNonCritical(Format(ResStr_InvalidDate, [ADate]));
  end;
end;  // TFrmParameters.ValidateFilename

{-------------------------------------------------------------------------------
  Accessor
}
procedure TGroupingRadioButton.Click;
var
  i: integer;
begin
  inherited;
  // cause all the radio buttons to enable / disable their controls
  for i := 0 to Parent.ControlCount-1 do
    if (Parent.Controls[i] is TGroupingRadioButton) then
      TGroupingRadioButton(Parent.Controls[i]).EnableControls(
          TGroupingRadioButton(Parent.Controls[i]).Control);
end;

{-------------------------------------------------------------------------------
  Enable or disable the controls associated with a radio button depending
     on if the radio button is checked.  Recurses into the controls.
}
procedure TGroupingRadioButton.EnableControls(AControl: TControl);
var
  i: integer;
begin
  // don't enable delete buttons until a row is selected
  AControl.Enabled := Checked and (not (AControl is TDeleteButton));
  if AControl is TWinControl then
    for i := 0 to TWinControl(AControl).ControlCount-1 do
      EnableControls(TWinControl(AControl).Controls[i]);
  if (AControl is TSpatialRef) and Checked then
    TSpatialRef(AControl).UpdateMapwindowSelector
  else if (AControl is TBoundingBox) and Checked then
    TBoundingBox(AControl).UpdateMapwindowSelector;
end;

{-------------------------------------------------------------------------------
  Set the control that is enabled or disabled by this radio button
}
procedure TGroupingRadioButton.SetControl(const Value: TWinControl);
begin
  FControl := Value;
end;

{-------------------------------------------------------------------------------
  Add a grid square range parameter entry control
}
function TfrmParameters.AddGridSquareRange(AInputParam: TInputParam; AParent:
    TWinControl): TGridSquareRange;
begin
  Result := TGridSquareRange.Create(Self);
  FLastAddedControl := Result;
end;

{-------------------------------------------------------------------------------
  Adds a single entry control row to a multiple entry count parameter.
}
procedure TfrmParameters.AddMultipleEntryCountRow(AInputParam: TInputParam;
    AParent: TWinControl; AResizeContainer: boolean);
var
  lPanel: TPanel;
  lWinControl: TWinControl;
begin
  lPanel := AddPanel(AParent);
  lPanel.BorderWidth := 1;
  lPanel.OnEnter := MultiControlPanelEnter;
  lPanel.OnExit := MultiControlPanelExit;
  lWinControl := AddControl(AInputParam, lPanel, 100);
  lWinControl.Left := 1;
  if AResizeContainer then begin
    ResizeParentToAccommodate(lPanel);
    ResizeParentToAccommodate(AParent);
  end else begin
    SetupDestinationControl(lWinControl);
  end;
end;

{-------------------------------------------------------------------------------
  Add a control or control group depending on entrycount value. Returns the
     created control panel
}
function TfrmParameters.AddSingleOrMultipleControl(AInputParam: TInputParam;
    AParent: TWinControl): TWinControl;
begin
  if AInputParam.EntryCount=1 then
    Result := AddSingleControl(AInputParam, AParent)
  else
    Result := AddMultipleControlsCheckUseRucksack(AInputParam, AParent);
end;

{-------------------------------------------------------------------------------
  Add a spatial reference entry control
}
function TfrmParameters.AddSpatialRefEdit(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TSpatialRef.Create(Self);
  FLastAddedControl := Result;
  with TSpatialRef(Result) do begin
    SimpleMode := true;
    ImageList := dmFormActions.ilButtons;
    ImageIndex := 5;
    DisplaySystem := AppSettings.SpatialRefSystem;
    OnGetFromMap := SpatialRefGetFromMap;
  end;
  CreateMapPopupMenu(Result);
  // add to list so we can register for drag and drop when ready
  FLinkedControls.Add(Result);
end;

{-------------------------------------------------------------------------------
  Dialog is non modal so make sure it cleans up
}
procedure TfrmParameters.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if not FNotCancelled then FReportFile.CancelReport;
  Action := caFree;
end;

{-------------------------------------------------------------------------------
  Cancel button - cleanup report
}
procedure TfrmParameters.btnCancelClick(Sender: TObject);
begin
  inherited;
  Close;
end;

{-------------------------------------------------------------------------------
  Creates a popup menu to attach to a spatial ref control
}
function TfrmParameters.CreateMapPopupMenu(AOwnerControl: TWinControl):
    TPopupMenu;
begin
  Result := TPopupMenu.Create(AOwnerControl);
  FMapPopupMenus.Add(Result);
  if AOwnerControl is TSpatialRef then begin
    AppSettings.UpdateMapMenu(self, Result.Items, True, MapForSpatialRefClick);
    TSpatialRef(AOwnerControl).DropDownMenu := Result;
    TSpatialRef(AOwnerControl).UpdateMapWindowSelector;
  end
  else if AOwnerControl is TBoundingBox then begin
    AppSettings.UpdateMapMenu(self, Result.Items, True, MapForBoundingBoxClick);
    TBoundingBox(AOwnerControl).DropDownMenu := Result;
    TBoundingBox(AOwnerControl).UpdateMapWindowSelector;
  end;
end;

{-------------------------------------------------------------------------------
  Click handler for map submenu items on the spatial ref control
}
procedure TfrmParameters.MapForSpatialRefClick(Sender: TObject);
begin
  PrepMapGetData(Sender);
  SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                        Self, UpdateSpatialRef);
end;

{-------------------------------------------------------------------------------
  Retrieve the SQL In clause data content from a multiple entry count parameter.
    The control passed in is the scroll box.
}
procedure TfrmParameters.PutMultipleInputValuesIntoParam(AControl: TWinControl;
    AParam: TInputParam);
var
  i: integer;
begin
  for i := 0 to AControl.ControlCount-1 do begin
    // navigate through layout panel down to input control
    Assert(AControl.Controls[i] is TPanel);
    Assert(TPanel(AControl.Controls[i]).Controls[0] is TWinControl);
    PutSingleInputValuesIntoParam(TWinControl(
          TPanel(AControl.Controls[i]).Controls[0]), AParam)
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve the SQL data value from an input control
}
procedure TfrmParameters.PutSingleInputValuesIntoParam(AControl: TWinControl;
    AParam: TInputParam);
var
  lValue: string;
begin
  if (AControl is TComboBox) and (AParam.UseRucksack <> urNo) then begin
    PutValuesFromRucksackIntoParam(TComboBox(AControl), AParam);
  end else begin
    lValue := '';
    if AControl is TCustomEdit then
      lValue := TCustomEdit(AControl).Text
    else if AControl is TDateTimePicker then
      lValue := IntToStr(Trunc(TDateTimePicker(AControl).Date))
    else if AControl is TFileLinkedEdit then
      lValue := TFileLinkedEdit(AControl).Text
    else if AControl is TAddinLinkedEdit then
      lValue := TAddinLinkedEdit(AControl).Key
    else if AControl is TCheckBox then begin
      if TCheckBox(AControl).Checked then
        lValue := '1'
      else
        lValue := '0';
    end else if AControl is TAddinIDComboBox then
      lValue := TAddinIDComboBox(AControl).CurrentStrId
    else if AControl is TGridSquareRange then
      lValue := TGridSquareRange(AControl).Range
    else if AControl is TBoundingBox then
      lValue := TBoundingBox(AControl).Values
    else if AControl is TSpatialRef then
      lValue := TSpatialRef(AControl).DisplayRef
    else if AControl is TPolygonSelection then
      lValue := SetupPolygonResults(TPolygonSelection(AControl), AParam.PartialOverlap)
    else
      raise EParametersException.Create(Format(ResStr_WrongParameterType, [AControl.ClassName]));
    if (AParam.DataType=dtText) or (AControl is TAddinLinkedEdit) then begin
      // ensure text is SQL safe
      lValue := DuplicateCharacters(lValue, '''');
      // wrap text param values in quotes
      lValue := '''' + lValue + '''';
    end; // case
    if lValue<>'' then begin
      AParam.Values.Add(lValue);
    end;
  end;    // if (AControl is TComboBox) and (AParam.UseRucksack <> urNo)
end;

{-------------------------------------------------------------------------------
  Retrieve the SQL data values from a rucksack
}
procedure TfrmParameters.PutValuesFromRucksackIntoParam(
  RucksackList: TComboBox; AParam: TInputParam);
var
  lFileName, lSectionName, lCurrent: string;
  lRucksack: TextFile;
begin
  if (RucksackList.ItemIndex >= 0) then
    if (RucksackList.Items.Objects[RucksackList.ItemIndex] is TRucksackListItem) then begin
      lFileName := TRucksackListItem(RucksackList.Items.Objects[RucksackList.ItemIndex]).FileName;

      case AParam.DataType of
        dtTaxon:        lSectionName := 'TAXON';
        dtLocation:     lSectionName := 'LOCATION';
        dtIndividual:   lSectionName := 'PEOPLE';
        dtName:         lSectionName := 'PEOPLE';
        dtOrganisation: lSectionName := 'PEOPLE';
      else
        raise EParametersException.CreateNonCritical(ResStr_InvalidParameterTypeForRucksack);
      end;    // case AParam.DataType of

      AssignFile(lRucksack, lFileName);
      Reset(lRucksack);
      try
        lCurrent := '';
        // Find start of relevant section
        while CompareText(lCurrent, '<' + lSectionName + '>') <> 0 do
          Readln(lRucksack, lCurrent);

        // Read data from rucksack
        Readln(lRucksack, lCurrent);
        while CompareText(lCurrent, '</' + lSectionName + '>') <> 0 do begin
          // ensure text is SQL safe
          lCurrent := DuplicateCharacters(lCurrent, '''');
          // wrap text param values in quotes
          lCurrent := '''' + lCurrent + '''';
          AParam.Values.Add(lCurrent);
          Readln(lRucksack, lCurrent);
        end;    // while CompareText(lCurrent, '</' + lSectionName + '>') <> 0
      finally
        CloseFile(lRucksack);
      end;
    end;    // if (RucksackList.Items.Objects[RucksackList.ItemIndex] is TRucksackListItem)
end;

{-------------------------------------------------------------------------------
  Ensure an entered spatial reference parameter can be parsed.
}
procedure TfrmParameters.ValidateSpatialRef(const ARef: string);
var
  lValidSpatialRef: TValidSpatialRef;
begin
  lValidSpatialRef := ValidSpatialRef(ARef, AppSettings.SpatialRefSystem);
  if not lValidSpatialRef.Valid then
    raise EParametersException.CreateNonCritical(lValidSpatialRef.Error);
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TAddDeleteButton.SetRowContainer(const Value: TScrollbox);
begin
  FRowContainer := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TAddDeleteButton.SetInputParam(const Value: TInputParam);
begin
  FInputParam := Value;
end;

{-------------------------------------------------------------------------------
  Click the add button will add a row to the scrollbox
}
procedure TAddButton.Click;
begin
  inherited;
  if Assigned(AddRowMethod) then
    AddRowMethod(FInputParam, FRowContainer, False);
end;

{-------------------------------------------------------------------------------
  Accessor method for the method to allow new rows to be added
}
procedure TAddButton.SetAddRowMethod(const Value: TAddRowMethod);
begin
  FAddRowMethod := Value;
end;

{-------------------------------------------------------------------------------
  Construct a grid square range control - 2 edit boxes (sw & ne corner)
}
constructor TGridSquareRange.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  BevelOuter := bvNone;
  FSwRef := TEdit.Create(self);
  with FSwRef do begin
    Parent := self;
    Ctl3d := false;
    Parent.Height := Height + 2;
    Align := alLeft;
    Width := self.Width div 2 - TO_PANEL_WIDTH div 2;
  end;
  FToPanel := TPanel.Create(self);
  with FtoPanel do begin
    Parent := self;
    Caption := '->';
    Width := TO_PANEL_WIDTH;
    Left := 10;
    Align := alLeft;
    BorderStyle := bsNone;
    BevelOuter := bvNone;
  end;
  FNeRef := TEdit.Create(self);
  with FNeRef do begin
    Parent := self;
    Ctl3d := false;
    Align := alClient;
  end;
end;

{-------------------------------------------------------------------------------
  Build the grid square range for the entered controls
}
function TGridSquareRange.GetRange: string;
var
  lRange: TStringList;
  lNERef: string;
  lSystem: string;
begin
  Validate;
  lRange := TStringList.Create;
  try
    // if user only enters sw corner, then use this as entire range (ie a single square)
    if Trim(FNERef.Text)='' then
      lNERef := FSWRef.Text
    else
      lNERef := FNERef.Text;
    lSystem := DetermineSpatialRefSystem(FSWRef.Text);
    SpatialRefFuncs.BuildGridSquareRange(FSWRef.Text, lNERef, lSystem, lRange, True);
    result := lRange.Commatext;
  finally
    lRange.Free;
  end;

end;

{-------------------------------------------------------------------------------
  Keep grid square range controls equal width
}
procedure TGridSquareRange.Resize;
begin
  inherited;
  FswRef.Width := self.Width div 2 - TO_PANEL_WIDTH div 2;
end;

{-------------------------------------------------------------------------------
  Ensure content is valid
}
procedure TGridSquareRange.Validate;
var
  lSystem: string;
begin
  if FSwRef.Text='' then
    raise EParametersException.CreateNonCritical(Format(ResStr_ParameterRequired,
              [ResStr_GridSquareRange]));
  lSystem := DetermineSpatialRefSystem(FSwRef.Text);
  try
    CheckIsGridSystem(lSystem);
  except
    on E:ESpatialRefError do
      raise EParametersException.CreateValidation(E.Message, FSwRef);
  end;
  if Trim(FNeRef.Text)<>'' then begin
    if lSystem <> DetermineSpatialRefSystem(FNeRef.Text) then
      raise EParametersException.CreateValidation(ResStr_GridSquareRangeDiffSystems, FSwRef);
    if Length(Trim(FSwRef.Text))<>Length(Trim(FNeRef.Text)) then
      raise EParametersException.CreateValidation(ResStr_GridSquareRangeDiffSizes, FSwRef);
  end;
end;

{-------------------------------------------------------------------------------
  Click a delete button on a multiple entry count param to remove the cel
      with the input focus.
}
procedure TDeleteButton.Click;
begin
  inherited;
  if Tag<>0 then begin
    TObject(Tag).Free;
    Tag := 0;
    Enabled := False;
  end;
end;

{-------------------------------------------------------------------------------
  Add a grid square range parameter entry control
}
function TfrmParameters.AddBoundingBox(AInputParam: TInputParam; AParent:
    TWinControl): TBoundingBox;
begin
  Result := TBoundingBox.Create(Self);
  CreateMapPopupMenu(Result);
  Result.OnGetFromMap := SpatialRefGetFromMap;
  FLastAddedControl := Result;
end;

{-------------------------------------------------------------------------------
  Add a date control to the grid.
}
function TfrmParameters.AddDatePicker(AInputParam: TInputParam; AParent:
    TWinControl): TWinControl;
begin
  Result := TDateTimePicker.Create(Self);
  FLastAddedControl := Result;
end;  // TFrmParameters.AddEdit

{-------------------------------------------------------------------------------
  Updates a linked control with the label from the supplied key
}
procedure TfrmParameters.DoUpdateLinkedControl(AControl: TAddinLinkedEdit;
    const AKey: string);
begin
  with AControl do begin
    // reject individuals on organisation controls and vice versa
    if Tag in [ST_INDIVIDUAL, ST_ORGANISATION] then
      if dmGeneralData.IsNameKeyForOrganisation(AKey) then begin
        if Tag=ST_INDIVIDUAL then exit
      end
      else begin
        if Tag=ST_ORGANISATION then exit;
      end;
    if AKey <> '' then begin
      case Tag of
        ST_NAME, ST_INDIVIDUAL, ST_ORGANISATION:
          Text := dmGeneralData.GetName(AKey);
        ST_LOCATION:
          Text := dmGeneralData.GetLocationName(AKey);
        ST_SPECIES :
          Text := dmGeneralData.GetTaxonName(AKey);
      else
        raise EParametersException.Create(ResStr_InvalidLinkedControl);
      end; // case
      Key := AKey;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Drag drop handler for names being dropped onto frame.
}
procedure TfrmParameters.DropOntoLinkedControl(const Sender: TObject; const
    AFormat: Integer; const ASourceData: TKeyList; const ATextStrings:
    TStringList; var AHandled: Boolean);

      function IsValidTable(const ASearchType: AddinSearchManager.TSearchType; const ATable: string): boolean;
      begin
        case ASearchType of
          ST_LOCATION: Result := SameText(ATable, TN_LOCATION);
          ST_SPECIES: Result := SameText(ATable, TN_TAXON_LIST_ITEM);
          ST_NAME: Result := SameText(ATable, TN_NAME)
                   or SameText(ATable, TN_INDIVIDUAL)
                   or SameText(ATable, TN_ORGANISATION);
          ST_INDIVIDUAL: Result := SameText(ATable, TN_NAME)
                   or SameText(ATable, TN_INDIVIDUAL);
          ST_ORGANISATION: Result := SameText(ATable, TN_NAME)
                   or SameText(ATable, TN_ORGANISATION);
        else
          Result := false;
        end; // case
      end;
begin
  if (ASourceData <> nil) and (ASourceData.Header.ItemCount > 0)
     and (AFormat = CF_JNCCDATA) then
    if TClipboardCapability(Sender).DropControl is TAddinLinkedEdit then begin
      if IsValidTable(TAddinLinkedEdit(TClipboardCapability(Sender).DropControl).Tag,
          ASourceData.Header.TableName) then begin
        AHandled := true;
        DoUpdateLinkedControl(TAddinLinkedEdit(TClipboardCapability(Sender).DropControl),
             ASourceData.Items[0].KeyField1);
      end;
    end;
end;  // TFrmParameters.DropOntoLinkedControl

{-------------------------------------------------------------------------------
  Drag drop handler for spatial references being dropped onto frame.
}
procedure TfrmParameters.DropSpatialRef(const Sender: TObject; const AFormat:
    Integer; const ASourceData: TKeyList; const ATextStrings: TStringList; var
    AHandled: Boolean);
begin
  if (ASourceData <> nil) and (ASourceData.Header.ItemCount > 0) then
    if (AFormat = CF_JNCCDATA) and
       (CompareText(ASourceData.Header.TableName, 'Spatial_Ref') = 0) then
    begin
      AHandled := True;
      if TClipboardCapability(Sender).DropControl is TSpatialRef then
        with TSpatialRef(TClipboardCapability(Sender).DropControl) do
          if ASourceData.Items[0].KeyField1 <> '' then begin
            Values := dmGeneralData.SetSpatialRef(
                ASourceData.Items[0].KeyField2, '', AppSettings.SpatialRefSystem);
          end;
    end
end;  // TFrmParameters.DropSpatialRef

{-------------------------------------------------------------------------------
  Creates the object list that the controls on the grid are placed into.
  Cannot use constructor to do this as this is a MDI form, it cannot be created
  hidden. Create goes to FormCreate goes to FormActivate.
}
procedure TfrmParameters.FormCreate(Sender: TObject);
begin
  inherited;
  FParameters := TObjectList.Create;
  FParameters.OwnsObjects := False;
  // a list of params not shown because they were duplicates (so inherit their
  // value from the first instance).
  FDupParams := TList.Create;
  // list of grouped parameter groups, name + group box on the data
  FGroupBoxes := TStringList.Create;
  FMapPopupMenus := TObjectList.Create;
  FMapPopupMenus.OwnsObjects := False;
  FLinkedControls := TObjectList.Create;
  FLinkedControls.OwnsObjects := False;
  FRadioContainers := TStringList.Create;
  FRadioContainers.Sorted := true;
  FRadioContainers.Duplicates := dupIgnore;
  FCurrentUseRucksack := urOptional;
end;

{-------------------------------------------------------------------------------
  Ensure context toolbar cleared.  Also reinitialise the map menus
}
procedure TfrmParameters.FormActivate(Sender: TObject);
var
  i: integer;
begin
  inherited;
  frmMain.ClearContextToolbar(True);
  for i := 0 to FMapPopupMenus.Count-1 do
    if FMapPopupMenus[i] is TPopupMenu then begin
      if TPopupMenu(FMapPopupMenus[i]).Owner is TSpatialRef then begin
        AppSettings.UpdateMapMenu(self, TPopupMenu(FMapPopupMenus[i]).Items, True, MapForSpatialRefClick);
        TSpatialRef(TPopupMenu(FMapPopupMenus[i]).Owner).UpdateMapWindowSelector;
      end
      else if TPopupMenu(FMapPopupMenus[i]).Owner is TBoundingBox then begin
        AppSettings.UpdateMapMenu(self, TPopupMenu(FMapPopupMenus[i]).Items, True, MapForBoundingBoxClick);
        TBoundingBox(TPopupMenu(FMapPopupMenus[i]).Owner).UpdateMapWindowSelector;
      end; // if
    end;
  LockWindowUpdate(0);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmParameters.GetDataSourceForm(ASearchType: AddinSearchManager.TSearchType);
begin
  case ASearchType of
    ST_INDIVIDUAL, ST_ORGANISATION, ST_NAME: dmFormActions.actNames.Execute;
    ST_LOCATION:   dmFormActions.actLocations.Execute;
    ST_SPECIES:    dmFormActions.actTaxonDiction.Execute;
  end;
end;  // TBaseMatch.GetDataSourceForm

{-------------------------------------------------------------------------------
  When the user presses return on a linked control, display the find dialog.
    The linked control's tag identifies what is being searched.
}
procedure TfrmParameters.LinkedControlFindData(Sender: TObject);
begin
  DoCheck(TAddinLinkedEdit(Sender), TAddinLinkedEdit(Sender).Tag,'', '', True);
end;

{-------------------------------------------------------------------------------
  When the user presses the button on a linked control, initiate return data.
    The linked control's tag identifies what is being linked.
}
procedure TfrmParameters.LinkedControlGetData(Sender: TObject);
begin
  if Sender is TAddinLinkedEdit then begin
    FReturnDataControl := TControl(Sender);
    GetDataSourceForm(TAddinLinkedEdit(Sender).Tag);
    SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                          Self, UpdateLinkedControl);
    Application.MainForm.BringToFront;
  end;
end;

{-------------------------------------------------------------------------------
  Click handler for map submenu items on the spatial ref control
}
procedure TfrmParameters.MapForBoundingBoxClick(Sender: TObject);
begin
  PrepMapGetData(Sender);
  TfrmMap(frmMain.ActiveMDIChild).SelectArea;
  SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                        Self, UpdateSpatialRef);
end;

{-------------------------------------------------------------------------------
  Common handling when creating a parameter entry control of any linked type
}
procedure TfrmParameters.PrepLinkedControl(AControl: TAddinLinkedEdit);
begin
  FLastAddedControl := AControl;
  with TAddinLinkedEdit(AControl) do begin
    BevelKind := bkFlat;
    ImageList := dmFormActions.ilButtons;
    ImageIndex := 5;
    OnGetData := LinkedControlGetData;
    OnFindData := LinkedControlFindData;
  end;
  // add to list so we can register for drag and drop when ready
  FLinkedControls.Add(AControl);
end;

{-------------------------------------------------------------------------------
  Do the things that need to be done before getting data from a map, either for
    a spatial ref or a bounding box
}
procedure TfrmParameters.PrepMapGetData(Sender: TObject);
begin
  dmFormActions.MapWindowMenuClick(Sender);
  Assert(Sender is TMenuItem);
  FReturnDataControl := TControl(TMenuItem(Sender).GetParentMenu.Owner);
end;

{-------------------------------------------------------------------------------
  So that we can validate that at least one option is selected in a radio group
    remember the controls that contain radio buttons.
}
procedure TfrmParameters.RememberRadioContainer(AControl: TWinControl; const
    AGroupName: string);
begin
  FRadioContainers.AddObject(AGroupName, AControl);
end;

{-------------------------------------------------------------------------------
  Initialise drop capability for linked controls
}
procedure TfrmParameters.SetupDestinationControls;
var
  i: integer;
begin
  for i := 0 to FLinkedControls.Count-1 do begin
    SetupDestinationControl(TWinControl(FLinkedControls[i]));
  end;
end;

{-------------------------------------------------------------------------------
  Initialise drop capability for a single linked control
}
procedure TfrmParameters.SetupDestinationControl(AControl: TWinControl);
begin
  if AControl is TAddinLinkedEdit then
    case TAddinLinkedEdit(AControl).Tag of
      ST_LOCATION:
        RegisterDropComponent(TAddinLinkedEdit(AControl), DropOntoLinkedControl,
                       [TN_LOCATION], [CF_JNCCDATA, CF_TEXT]);
      ST_SPECIES:
        RegisterDropComponent(TAddinLinkedEdit(AControl), DropOntoLinkedControl,
                       [TN_TAXON_LIST_ITEM], [CF_JNCCDATA, CF_TEXT]);
      ST_NAME:
        RegisterDropComponent(TAddinLinkedEdit(AControl), DropOntoLinkedControl,
                       [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA, CF_TEXT]);
      ST_INDIVIDUAL:
        RegisterDropComponent(TAddinLinkedEdit(AControl), DropOntoLinkedControl,
                       [TN_NAME, TN_INDIVIDUAL], [CF_JNCCDATA, CF_TEXT]);
      ST_ORGANISATION:
        RegisterDropComponent(TAddinLinkedEdit(AControl), DropOntoLinkedControl,
                       [TN_NAME, TN_ORGANISATION], [CF_JNCCDATA, CF_TEXT]);
    end // case
  else if AControl is TSpatialRef then
    RegisterDropComponent(TSpatialRef(AControl).ControlSpatialRef, DropSpatialRef,
                        ['SPATIAL_REF'], [CF_JNCCDATA, CF_TEXT]);
end;

{-------------------------------------------------------------------------------
  Click on the map button on a spatial ref control to initiate return data
}
procedure TfrmParameters.SpatialRefGetFromMap(Sender: TObject);
begin
  if dmFormActions.DefaultMapWindow <> nil then begin
    SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                            self, UpdateSpatialRef);
    Assert(Sender is TWinControl);
    Assert((TWinControl(Sender).Parent is TSpatialRef) or (TWinControl(Sender).Parent is TBoundingBox));
    if TWinControl(Sender).Parent is TBoundingBox then
      TfrmMap(frmMain.ActiveMDIChild).SelectArea;
    FReturnDataControl := TWinControl(Sender).Parent;
  end;
end;

{-------------------------------------------------------------------------------
  Method receiving feedback from Return data operations for linked controls.
}
procedure TfrmParameters.UpdateLinkedControl(KeyList: TKeyList);
begin
  if Assigned(FReturnDataControl) then
    if (FReturnDataControl is TAddinLinkedEdit) and (Keylist.Header.ItemCount>0) then
      DoUpdateLinkedControl(TAddinLinkedEdit(FReturnDataControl),
          Keylist.Items[0].KeyField1);
  BringToFront;
end;

{-------------------------------------------------------------------------------
  Update when a spatial reference is returned from the map
}
procedure TfrmParameters.UpdateSpatialRef(KeyList: TKeyList);
var
  lRequiredKeys: integer;
begin
  Assert(Assigned(FReturnDataControl));
  if FReturnDataControl is TSpatialRef then
    lRequiredKeys := 1
  else if FReturnDataControl is TBoundingBox then
    lRequiredKeys := 2
  else
    raise EParametersException.Create(ResStr_InvalidContrRequested);
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount >= lRequiredKeys) then
    begin
      if (CompareText(KeyList.Header.TableName, 'Spatial_Ref') = 0) then
      begin
        if FReturnDataControl is TSpatialRef then begin
          if KeyList.Items[0].KeyField2 <> '' then
            TSpatialRef(FReturnDataControl).Values :=
                dmGeneralData.SetSpatialRef(KeyList.Items[0].KeyField2, '', AppSettings.SpatialRefSystem);
        end
        else begin
          TBoundingBox(FReturnDataControl).NeRef  := KeyList.Items[0].KeyField2;
          TBoundingBox(FReturnDataControl).SwRef  := KeyList.Items[1].KeyField2;
        end;
      end;
    end;
  finally
    KeyList.Free;
    FReturnDataControl := nil;
  end;
  BringToFront;
end;  // TFrmParameters.UpdateSpatialRef

{-------------------------------------------------------------------------------
  Check a linked edit control has been populated.  If not, call the Check
      operation on it.
}
function TfrmParameters.ValidateLinkedEdit(AControl: TAddinLinkedEdit): Boolean;
begin
  Result := AControl.Key<>'';
end;

{-------------------------------------------------------------------------------
  Loop through the controls in a scroll box, validate each one in turn
}
procedure TfrmParameters.ValidateMultipleControls(AInputParam: TInputParam);
var
  i: integer;
  lValidEntries: integer;
begin
  lValidEntries := 0;
  for i := 0 to AInputParam.InputControl.ControlCount-1 do begin
    // navigate through layout panel down to input control
    Assert(AInputParam.InputControl.Controls[i] is TPanel);
    Assert(TPanel(AInputParam.InputControl.Controls[i]).Controls[0] is TWinControl);
    try
      ValidateSingleControl(TWinControl(
          TPanel(AInputParam.InputControl.Controls[i]).Controls[0]));
      Inc(lValidEntries);
    except on TExceptionPath do ;
    end;
  end;
  if lValidEntries=0 then
    raise EParametersException.CreateValidation(Format(ResStr_SelectAtLeastOne, [AInputParam.Description]),
        TWinControl(TPanel(AInputParam.InputControl.Controls[0]).Controls[0]));
end;

{-------------------------------------------------------------------------------
  Ensure that one option is selected in each radio group
}
procedure TfrmParameters.ValidateRadioGroups;
var
  i, j: integer;
  lSelectionFound: boolean;
begin
  for i := 0 to FRadioContainers.Count-1 do begin
    lSelectionFound := False;
    for j := 0 to TWinControl(FRadioContainers.Objects[i]).ControlCount-1 do
      if TWinControl(FRadioContainers.Objects[i]).Controls[j] is TRadioButton then
        if TRadioButton(TWinControl(FRadioContainers.Objects[i]).Controls[j]).Checked then begin
          lSelectionFound := true;
          break;
        end;
    if not lSelectionFound then
      raise EParametersException.CreateNonCritical(
          Format(ResStr_SelectARadioButton, [FRadioContainers[i]]));
  end;
end;

{-------------------------------------------------------------------------------
  Validates an input param's control (or scrollbox of controls)
}
procedure TfrmParameters.ValidateSingleControl(AControl: TControl);
begin
  if Assigned(AControl) and AControl.Enabled then begin
    if AControl is TFileLinkedEdit then
      ValidateFilename(TAddinLinkedEdit(AControl).Text)
    else if AControl is TAddinLinkedEdit then
      ValidateValue(ValidateLinkedEdit(TAddinLinkedEdit(AControl)),
           ResStr_LinkedEditNotRecognised, TAddinLinkedEdit(AControl))
    else if AControl is TVagueDateEdit then
      ValidateVagueDate(TVagueDateEdit(AControl).Text)
    else if TInputParam(AControl).DataType = dtDate then
      ValidateDate(TEdit(AControl).Text)
    else if TInputParam(AControl).DataType = dtSpatialRef then
      ValidateSpatialRef(TSpatialRef(AControl).DisplayRef)
    else if AControl is TGridSquareRange then
      TGridSquareRange(AControl).Validate
    else if AControl is TBoundingBox then
      TBoundingBox(AControl).Validate;
  end;
end;

{-------------------------------------------------------------------------------
  Construct a grid square range control - 2 edit boxes (sw & ne corner)
}
constructor TBoundingBox.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsSingle;
  BevelOuter := bvNone;
  FSwRef := TEdit.Create(self);
  with FSwRef do begin
    Parent := self;
    Ctl3d := false;
    Self.Height := Height*3;
    Top := Height*2-4;
    Left := 4;
    Width := 100;
  end;
  FNeRef := TEdit.Create(self);
  with FNeRef do begin
    Parent := self;
    Ctl3d := false;
    Width := 100;
    Left := Parent.Width - Width-4;
    Anchors := [akTop, akRight];
    Top := 4;
  end;
  with TLabel.Create(self) do begin
    Parent := self;
    Caption := '<- ' + ResStr_SWCorner;
    Left := FSwRef.Width + 4;
    Top := FSWRef.Top + 2;
  end;
  with TLabel.Create(self) do begin
    Parent := self;
    Caption := ResStr_NECorner + ' ->';
    Left := FNeRef.Left - Width - 4;
    Anchors := [akTop, akRight];
    Top := FNeRef.Top + 2;
  end;
  SetupButtons;
end;

{-------------------------------------------------------------------------------
}
procedure TBoundingBox.DropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  if Assigned(FDropDownMenu) then begin
    lPos := FGetButton.ClientToScreen(Point(0, FGetButton.Height));
    FDropDownMenu.Popup(lPos.X, lPos.Y);
  end;
end;  // TSpatialRef.DropDownClick

{-------------------------------------------------------------------------------
  Returns an set of values for the bounding box - SWLat, SWLong, NELat, NELong
}
function TBoundingBox.GetValues: string;
var
  lSWLatLong, LNELatLong: TLatLong;
  lSWSystem, lNESystem: string;
  lFormatSettings: TFormatSettings;
begin
  lSWSystem := DetermineSpatialRefSystem(SwRef);
  if (lSWSystem = ResStr_SystemUnknown) then
    raise EParametersException.CreateValidation(ResStr_SpatialRefNotRecognised, FSwRef);
  lNESystem := DetermineSpatialRefSystem(NeRef);
  if (lNESystem = ResStr_SystemUnknown) then
    raise EParametersException.CreateValidation(ResStr_SpatialRefNotRecognised, FNeRef);
  lSWLatLong := ConvertToLatLong(SwRef, lSWSystem);
  lNeLatLong := ConvertToLatLong(NeRef, lNESystem);

  // Internationalisation glitch with decimal separator, so force the dot.
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, lFormatSettings);
  lFormatSettings.DecimalSeparator := '.';
  Result := 'SWLat=' + FloatToStr(lSwLatLong.Lat, lFormatSettings) + ',' +
            'SWLong=' + FloatToStr(lSwLatLong.Long, lFormatSettings) + ',' +
            'NELat=' + FloatToStr(lNeLatLong.Lat, lFormatSettings) + ',' +
            'NELong=' + FloatToStr(lNeLatLong.Long, lFormatSettings);
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TBoundingBox.GetNeRef: string;
begin
  Result := FNeRef.Text;
end;

{-------------------------------------------------------------------------------
  Accessor.  OnGetFromMap is the get button's click event
}
function TBoundingBox.GetOnGetFromMap: TNotifyEvent;
begin
  Result := FGetButton.OnClick;
end;

{-------------------------------------------------------------------------------
  Accessor
}
function TBoundingBox.GetSwRef: string;
begin
  Result := FSwRef.Text;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TBoundingBox.SetNeRef(const Value: string);
begin
  FNeRef.Text := Value;
end;

{-------------------------------------------------------------------------------
  Accessor.  OnGetFromMap is the get button's click event
}
procedure TBoundingBox.SetOnGetFromMap(const Value: TNotifyEvent);
begin
  FGetButton.OnClick := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TBoundingBox.SetSwRef(const Value: string);
begin
  FSwRef.Text := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TBoundingBox.SetupButtons;

  procedure SharedControlSetup(AControl: TWinControl);
  begin
    with AControl do begin
      Parent   := Self;
      Height   := 23;
      Visible    := True;
      Top        := Self.Height - Height - 4;
      Anchors  := [akTop, akRight];
    end;
  end;

begin
  FDropDownButton := TButton.Create(Self);
  SharedControlSetup(FDropDownButton);
  with FDropDownButton do begin
    Width      := 14;
    ParentFont := False;
    Font.Name  := 'Marlett';  // MS font.
    Font.Style := [];
    Caption    := '6';  // Dropdown arrow
    TabOrder   := 2;
    Left       := Self.Width - Width - 4;
    OnClick    := DropDownClick;
  end;
  FGetButton := TImageListButton.Create(Self);
  SharedControlSetup(FGetButton);
  with FGetButton do begin
    Width    := 22;
    Caption  := '';
    Hint     := ResStr_FindSpatialRef;
    TabOrder := 2;  // Will "insert" it in front of arrow-down button.
    Left     := FDropDownButton.Left - Width;
    ImageList := dmFormActions.ilButtons;
    ImageIndex := 5;
  end;
end;  // TBoundingBox.SetupButtons

{-------------------------------------------------------------------------------
  Enabled/disable the map link buttons
}
procedure TBoundingBox.UpdateMapWindowSelector;
begin
  if Assigned(FDropDownMenu) and Assigned(Parent)
      and Assigned(FGetButton) and Assigned(FDropDownButton) then begin
    FGetButton.Enabled := (FDropDownMenu.Items.Count > 0) and (Parent.Enabled);
    FDropDownButton.Enabled  := FGetButton.Enabled;
  end;
end;

{-------------------------------------------------------------------------------
  Ensure content is valid
}
procedure TBoundingBox.Validate;

    procedure CheckCorner(AControl: TEdit);
    var
      lSystem: string;
    begin
      if AControl.Text='' then
        raise EParametersException.CreateValidation(Format(ResStr_ParameterRequired,
                  [ResStr_BoundingBox]), AControl);
      lSystem := DetermineSpatialRefSystem(AControl.Text);
      if lSystem=ResStr_SystemUnknown then
        raise EParametersException.CreateNonCritical(ResStr_SystemUnknown);
    end;
begin
  CheckCorner(FSwRef);
  CheckCorner(FNeRef);
end;

{-------------------------------------------------------------------------------
  Enable or disable the delete button associated with a multi entry control
}
procedure TfrmParameters.EnableDeleteButton(Sender: TObject; AEnable: boolean);
begin
  if TPanel(Sender).Parent is TScrollbox then
    if TScrollbox(TPanel(Sender).Parent).Tag <> 0 then
      if TObject(TScrollbox(TPanel(Sender).Parent).Tag) is TImageListButton then begin
        TImageListButton(TScrollbox(TPanel(Sender).Parent).Tag).Enabled := AEnable;
        // tag the delete button with the current row
        TImageListButton(TScrollbox(TPanel(Sender).Parent).Tag).Tag := Integer(Sender);
      end;
end;

{-------------------------------------------------------------------------------
  Enter a control that is part of a multi control list - select the control
    so the delete button knows what to do.
}
procedure TfrmParameters.MultiControlPanelEnter(Sender: TObject);
begin
  TPanel(Sender).Color := MergeColours(clBtnFace, clHighlight, 60);
  EnableDeleteButton(Sender, true);
end;

{-------------------------------------------------------------------------------
  Exit a control that is part of a multi control list - deselect the control
}
procedure TfrmParameters.MultiControlPanelExit(Sender: TObject);
begin
  TPanel(Sender).Color := clBtnFace;
  // don't disable delete button if the user has just clicked it
  if not (ActiveControl is TDeleteButton) then
    EnableDeleteButton(Sender, False);
end;

{-------------------------------------------------------------------------------
}
function TfrmParameters.AddPolygonSelection(AInputParam: TInputParam;
          AParent: TWinControl) : TPolygonSelection;
begin
  Result := TPolygonSelection.Create(self);
  Result.IsForLocations := AInputParam.DataType = dtLocationsInPolygon;
  FLastAddedControl := Result;
end;

{-------------------------------------------------------------------------------
}
constructor TPolygonSelection.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  BevelOuter := bvNone;
  Height := 300;
  Width := 250;

  FPolygonLayers := TKeyboardRapidTree.Create(Self);
  //Setting up FPolygonLayers (tree view)
  SetPolygonSelectionControl(TFrmParameters(AOwner).ilPolygonSelection);

  //Setting the buttons panel
  FButtonPanel := TPolygonSelectionBtn.Create(Self);
  FButtonPanel.Parent := Self;

  AppSettings.UpdateMapMenu(
      TForm(Owner),
      TFrmParameters(Owner).pmMapForPolygons.Items,
      True,
      FButtonPanel.MapForPolygonClick);
end;

{-------------------------------------------------------------------------------
  Destructor
}
destructor TPolygonSelection.Destroy;
begin
  if Assigned(FMapWindow) and (not FMapAlreadyPresent) then begin
    FMapWindow.Release;
    FMapWindow := nil;
  end;
  inherited;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TPolygonSelection.SetIsForLocations(const Value: boolean);
begin
  FIsForLocations := Value;
end;

//----------------------------------------------------------------------------
procedure TPolygonSelection.SetPolygonSelectionControl(imageList: TImageList);
var
  lCursor : TCursor;
  lMapServerLink : TMapServerLink;

begin
  //Setting up FPolygonLayers
  with FPolygonLayers do begin
    Parent                 := Self;
    Align                  := alTop;
    Height                 := 250;
    ColCount               := 1;
    DoubleBuffered         := True;
    DefaultRowHeight       := 17;
    FitColumnToClientWidth := True;
    FixedRows              := 0;
    GridLineWidth          := 0;
    HideSelection          := False;
    Images                 := imageList;
    RowCount               := 1;
    ShowImages             := True;
    ShowLogic              := True;
    SmoothExpandCollapse   := False;
    StateImages            := imageList;
    TransparentMode        := True;
    OnImageClicked         := PolygonLayersImageClicked;
  end;

  lCursor := HourglassCursor;
  try
    lMapServerLink := TMapServerLink.Create(nil);
    try
      ClearPolygonLayers;
      PopulatePolygonLayers(lMapServerLink);
    finally
      lMapServerLink.Free
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;

//==============================================================================
// Clears FPolygonLayers to refresh
procedure TPolygonSelection.ClearPolygonLayers;
var loNode: TFlyNode;
begin
  loNode := FPolygonLayers.Items.GetFirstNode;
  while loNode <> nil do begin
    TKeyData(loNode.Data).Free;
    loNode := loNode.GetNextSibling;
  end;
  FPolygonLayers.Items.Clear;
end;

//==============================================================================
procedure TPolygonSelectionBtn.btnFindPolygonClick(Sender: TObject);
begin
  inherited;
  TPolygonSelection(Owner).RequestMapForReturnData(
      AppSettings.AvailableMaps.DefaultMap.BaseMapKey,
      TPolygonSelection(Owner).UpdatePolygonList);
end;

{-------------------------------------------------------------------------------
}
procedure TPolygonSelectionBtn.MapForPolygonClick(Sender: TObject);
begin
  TPolygonSelection(Owner).RequestMapForReturnData(
      AppSettings.AvailableMaps[TMenuItem(Sender).Tag].BaseMapKey,
      TPolygonSelection(Owner).UpdatePolygonList);
end;

//==============================================================================
//Brings up the map window to allow the user to choose a polygon
//This polygon can be returned to this screen
procedure TPolygonSelection.RequestMapForReturnData(ABaseMapKey: TKeyString;
          ACallBackFunction: TRequestorUpdateFunction);
begin
  FMapAlreadyPresent := False;
  FMapWindow := dmFormActions.MapWindow(ABaseMapKey);

  if not Assigned(FMapWindow) then
  begin
    FMapWindow := dmFormActions.MapWindow(ABaseMapKey, True);
    FMapWindow.CalledFromCustom := True;
    FMapWindow.actPointerExecute(nil);
  end else
    FMapAlreadyPresent := True;
  //Setting up the link between parameter screen and map window so that chosen
  //polygon value can be returned
  TBaseForm(Owner).SetupLink(TBaseForm(FMapWindow), TBaseForm(Owner), ACallBackFunction);
  FMapWindow.BringToFront;
end;

//==============================================================================
//Takes the retunred value from the map window and selects the corresponding
//polygons in FPolygonLayers
procedure TPolygonSelection.UpdatePolygonList(KeyList: TKeyList);
var
  lMapServerLink: TMapServerLink;
  lMapSheetKey: String;
  lSheetNode, lNode: TFlyNode;
  lObjectID: Integer;
  lKey: TKeyData;
  lMapSheetFileName: string;

    // retrieves the Node in the hierarchy associated with the map sheet, or
    // nil if not found
    procedure LocateMapSheetNode;
    begin
      lSheetNode := FPolygonLayers.Items.GetFirstNode;
      // Locate BaseMap, but only if there are several to choose from.
      if AppSettings.AvailableMaps.Count > 1 then begin
        while Assigned(lSheetNode) do begin
          if TKeyData(lSheetNode.Data).ItemKey = FMapWindow.BaseMapKey then Break;
          lSheetNode := lSheetNode.GetNextSibling;
        end;
        if Assigned(lSheetNode) then
          lSheetNode := lSheetNode.GetFirstChild;
      end;
      while Assigned(lSheetNode) do begin
        if TKeyData(lSheetNode.Data).ItemKey = lMapSheetKey then Break;
        lSheetNode := lSheetNode.GetNextSibling;
      end;
    end;

    // finds the polygon node associate with the polygon, or nil if not found
    procedure LocatePolygonNode;
    begin
      lNode := lSheetNode.GetFirstChild;
      lObjectID := StrToInt(KeyList.Items[0].KeyField2);
      while Assigned(lNode) do begin
        // If found, select it.
        if Integer(lNode.Data) = lObjectID then begin
          lNode.Expand(True);
          lNode.Tree.Selected := lNode;
          SelectPolygon(lNode, True);
          Break;
        end;
        lNode := lNode.GetNextSibling;
      end;
    end;

begin
  // Find the selected polygon from the keylist in the tree
  if KeyList.Header.TableName <> 'POLYGON' then
    MessageDlg(ResStr_InvalidPolygonRef, mtWarning, [mbOk], 0)
  else begin
    lMapServerLink := TMapServerLink.Create(nil);
    try
      // Use MapServerLink to work out the MapSheet from the given SheetID
      with lMapServerLink do begin
        ActiveDataset := FMapWindow.BaseMapKey + '.gds';
        lMapSheetKey := SheetMapSheetKey(StrToInt(KeyList.Items[0].KeyField1));
        lMapSheetFileName := ExtractFileName(SheetFileName(StrToInt(KeyList.Items[0].KeyField1)));
      end;
      LocateMapSheetNode;
      if not Assigned(lSheetNode) then begin
        // user may have added a sheet during return data operation so refresh
        PopulatePolygonLayers(lMapServerLink);
        LocateMapSheetNode;
      end;
      // Locate Polygon
      if Assigned(lSheetNode) then begin
        LocatePolygonNode;
        if not Assigned(lNode) then begin
          lKey := TKeyData.Create;
          lKey.ItemKey        := lMapSheetKey;
          lKey.ItemAdditional := lMapSheetFileName;
          AddPolygons(lMapServerLink, lSheetNode, lKey);
          LocatePolygonNode;
        end;
      end;
    finally
      lMapServerLink.Free;
    end;
  end;
  if FMapAlreadyPresent then
    FMapWindow.RefreshMap
  else begin
    FMapWindow.Release;  // Get rid of the map window
    FMapWindow := nil;
    frmMain.Repaint;
  end;
  //Bring the parameter entry screen to the front
  TFrmParameters(Owner).BringToFront;
end;

//==============================================================================
procedure TPolygonSelection.PolygonLayersImageClicked(Sender: TObject; ANode: TFlyNode);
begin
  inherited;
  SelectPolygon(FPolygonLayers.Selected, FPolygonLayers.Selected.ImageIndex = UNCHECKED_IMG_IDX);
end;

//==============================================================================
{ Select a polygon in the tree view.  If iUpdateParent is False, then the parent
     node is not scanned to set grey ticks for partial update- useful for
     optimising the load process }
procedure TPolygonSelection.SelectPolygon(ANode: TFlyNode; const ASelect: Boolean;
  AUpdateParent: Boolean = True);
var
  lNode, lChildNode: TFlyNode;
  lAllSelected, lSomeSelected: Boolean;
begin
  if Assigned(ANode) then begin
    // If no change in state, ignore.
    if ((ANode.ImageIndex = CHECKED_IMG_IDX) and ASelect) or
       ((ANode.ImageIndex = UNCHECKED_IMG_IDX) and not ASelect) then Exit;

    // Otherwise, process.
    if ASelect then ANode.ImageIndex := CHECKED_IMG_IDX
               else ANode.ImageIndex := UNCHECKED_IMG_IDX;
    ANode.SelectedIndex := ANode.ImageIndex;

    // Update all sub-nodes, but don't get back to parent.
    lNode := ANode.GetFirstChild;
    while Assigned(lNode) do begin
      SelectPolygon(lNode, ASelect, False);  // Use recursion for child nodes.
      lNode := lNode.GetNextSibling;
    end;

    // Now deal with parent nodes.
    lNode := ANode.Parent;
    while Assigned(lNode) do begin
      lChildNode := lNode.GetFirstChild;
      lAllSelected  := True;
      lSomeSelected := False;
      // Check state of all child nodes
      while Assigned(lChildNode) do begin
        // If some unchecked or greyed, not all nodes are selected.
        if lChildNode.ImageIndex in [UNCHECKED_IMG_IDX, CHECKED_GREY_IMG_IDX] then
          lAllSelected := False;
        // If some checked or greyed, some nodes are selected.
        if lChildNode.ImageIndex in [CHECKED_IMG_IDX, CHECKED_GREY_IMG_IDX] then
          lSomeSelected := True;
        lChildNode := lChildNode.GetNextSibling;
      end;
      // Update parent node state image accordingly.
      if lAllSelected then lNode.ImageIndex := CHECKED_IMG_IDX else
      if lSomeSelected then lNode.ImageIndex := CHECKED_GREY_IMG_IDX
                       else lNode.ImageIndex := UNCHECKED_IMG_IDX;
      lNode.SelectedIndex := lNode.ImageIndex;
      // Go up again.
      lNode := lNode.Parent;
    end;
  end;
end;  // SelectPolygon

//==============================================================================
// Sets up the Find Polygon Button
procedure TPolygonSelectionBtn.SetFindPolygonButton;
begin
  with FFindPolygonButton do
  begin
    Caption    := '&Find Polygon On Map';
    Height     := 25;
    Width      := 133;
    Left       := 5;
    ImageList  := dmFormActions.ilButtons;
    ImageIndex := 5;
    OnClick    := btnFindPolygonClick;
  end;
end;

//==============================================================================
//Sets up the find polygon drop down button
procedure TPolygonSelectionBtn.SetFindPolygonDropDown;
begin
  with FFindPolygonDropDown do
  begin
    Left    := FFindPolygonButton.Left + FFindPolygonButton.Width - 1;
    Width   := 16;
    Height  := 25;
    Font.Charset := SYMBOL_CHARSET;
    Font.Size    := 8;
    Font.Name    := 'Marlett';
    ParentFont   := False;
    Caption := '6';
    OnClick := btnPolyDropDownClick;
  end;
end;

//==============================================================================
constructor TPolygonSelectionBtn.Create(AOwner: TComponent);
begin
  inherited;
  Align := alBottom;
  BevelOuter := bvNone;
  BevelInner := bvNone;

  //Adding the button
  FFindPolygonButton := TImageListButton.Create(Self);
  FFindPolygonButton.Parent := Self;
  SetFindPolygonButton;

  //Adding the drop down button
  FFindPolygonDropDown := TButton.Create(Self);
  FFindPolygonDropDown.Parent := Self;
  SetFindPolygonDropDown;
end;

//==============================================================================
//Brings up the map window to allow the user to choose a polygon
//This polygon can be returned to this screen
procedure TPolygonSelectionBtn.btnPolyDropDownClick (Sender: TObject);
var
  lPos: TPoint;
begin
  inherited;
  //Getting the position to show the pop up menu
  lPos := FFindPolygonButton.ClientToScreen(Point(0, FFindPolygonButton.Height));
  if (Owner.Owner is TFrmParameters) and Assigned(Owner.Owner) then
    TFrmParameters(Owner.Owner).pmMapForPolygons.Popup(lPos.X, lPos.Y);
end;

{-------------------------------------------------------------------------------
  Populate the list of polygons for a single sheet in the hierarchy. If the
      parent node is already populated, this acts as just a refresh of the
      missing items.
}
procedure TPolygonSelection.AddPolygons(AMapServerLink: TMapServerLink;
    AParentNode: TFlyNode; const AKey: TKeyData);
var
  lSheetID: Integer;
  lIdx: Integer;
  lStaticId: Integer;
  lLinked: Byte;
  lLocationKey: TKeyString;
  lLocationName: String;
  lMaxDisplayLength: Integer;
  lDisplayID: String;
  lNode : TFlyNode;

    // search the list of nodes to see if the next node is already on the list,
    // using the static ID to check
    function FoundNode: boolean;
    var
      i: integer;
    begin
      Result := false; // default
      for i := 0 to AParentNode.Count-1 do
        if Integer(AParentNode.Items[i].Data) = lStaticId then begin
          Result := true;
          break;
        end;
    end;

begin
  // Get the Sheet Id, from the filename in map dataset.
  lSheetID := AMapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath +
                                               AKey.ItemAdditional);

  // Get all objects in the sheet and find out if it is linked to
  // Location Boundary, Admin Area Boundary, or is just Unlinked
  lMaxDisplayLength := Length(IntToStr(AMapServerLink.ObjectTotal[lSheetID] - 1));
  for lIdx := 0 to AMapServerLink.ObjectTotal[lSheetID] - 1 do begin
    lStaticId := AMapServerLink.StaticIDforObject(lSheetID, lIdx);
    if not FoundNode then begin
      // Assume unlinked
      lLinked := 0;

      // Find out if polygon linked to location.
      with dmDatabase.ExecuteSQL(Format('SELECT Location_Key FROM Location_Boundary ' +
                                        'WHERE Map_Sheet_Key = ''%s'' AND Object_Id = %d',
                                        [AKey.ItemKey, lStaticId]), True) do
      begin
        // Check if polygon linked to Location Boundary, only need the Location_Key
        if not Eof then begin
          lLocationKey  := Fields['Location_Key'].Value;
          lLocationName := dmGeneralData.GetLocationName(lLocationKey);
          lLinked := 1;
        end;
        Close;
      end;

      // Check if polygon linked to Admin Area Boundary, if not already linked
      if lLinked = 0 then
        // Only need the Admin Area name
        with dmDatabase.ExecuteSQL(Format('SELECT AA.Item_Name FROM Admin_Boundary AB ' +
                   'INNER JOIN Admin_Area AA ON AA.Admin_Area_Key = AB.Admin_Area_Key ' +
                   'WHERE AB.Map_Sheet_Key = ''%s'' AND AB.Object_Id = %d',
                   [AKey.ItemKey, lStaticId]), True) do
        begin
          if not Eof then begin
            lLocationName := Fields['Item_Name'].Value;
            lLinked := 2;
          end;
          Close;
        end;

      case lLinked of
        // Polygon linked to a location
        1: lNode := FPolygonLayers.Items.AddChildObject(AParentNode,
                                                         lLocationName,
                                                         Pointer(lStaticId));
        // Polygon linked to an admin area
        2: lNode := FPolygonLayers.Items.AddChildObject(AParentNode,
                                                         lLocationName,
                                                         Pointer(lStaticId));
        // Unlinked polygon
        else begin
          // This will get the unlinked polygons ordered properly.
          lDisplayID := IntToStr(lStaticID);
          while Length(lDisplayID) < lMaxDisplayLength do lDisplayID := ' ' + lDisplayID;
          lNode := FPolygonLayers.Items.AddChildObject(AParentNode,
                                                        'Unlinked Polygon [ID: ' + lDisplayID + ']',
                                                        Pointer(lStaticId));
        end;
      end;
      lNode.ImageIndex    := UNCHECKED_IMG_IDX;
      lNode.SelectedIndex := UNCHECKED_IMG_IDX;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Populate the Polygons hierarchy
}
procedure TPolygonSelection.PopulatePolygonLayers(AMapServerLink:
    TMapServerLink);
var
  lNode : TFlyNode;
  i : Integer;
  lKey: TKeyData;

  // Add the list of layers to the hierarchy at the top level. If the
  // list is already populated, this is just a refresh for new layers
  procedure AddLayers(AParentNode: TFlyNode; const ABaseMapKey: TKeyString);
  var
    lExistingLayers: TStringList;
    lLayerIdx: integer;
  begin
    lExistingLayers := TStringList.Create;
    try
      for lLayerIdx := 0 to FPolygonLayers.Items.Count-1 do
        lExistingLayers.Add(TKeyData(FPolygonLayers.Items[lLayerIdx].Data).ItemKey);
      // Need to activate correct map dataset before accessing the sheets.
      AMapServerLink.ActiveDataset := ABaseMapKey + '.gds';
      with dmDatabase.ExecuteSQL('SELECT * FROM Map_Sheet WHERE Sheet_Type=3 ' +
                                 'AND Base_Map_Key=''' + ABaseMapKey + '''', True) do
      begin
        while not Eof do begin
          // skip existing layers
          if lExistingLayers.IndexOf(Fields['Map_Sheet_Key'].Value)=-1 then begin
            lKey := TKeyData.Create;
            lKey.ItemKey        := Fields['Map_Sheet_Key'].Value;
            lKey.ItemAdditional := VarToStr(Fields['Dataset_Sheet_FileName'].Value);
            lNode := FPolygonLayers.Items.AddChildObject(AParentNode, Fields['Sheet_Name'].Value, lKey);
            lNode.ImageIndex    := UNCHECKED_IMG_IDX;
            lNode.SelectedIndex := UNCHECKED_IMG_IDX;
            if lKey.ItemAdditional <> '' then
              AddPolygons(AMapServerLink, lNode, lKey);
          end;
          MoveNext;
        end;
        Close;
      end;
    finally
      lExistingLayers.Free;
    end;
  end;

begin
  with AppSettings.AvailableMaps do
    for i := 0 to Count - 1 do
    begin
      if Count > 1 then
      begin
        lKey := TKeyData.Create;
        lKey.ItemKey := Items[i].BaseMapKey;
        lNode := FPolygonLayers.Items.AddObjectFirst(nil, Items[i].DisplayName, lKey);
        lNode.ImageIndex    := UNCHECKED_IMG_IDX;
        lNode.SelectedIndex := UNCHECKED_IMG_IDX;
        lNode.StateIndex    := WORLD_IMG_IDX;
      end else
        lNode := nil;
      AddLayers(lNode, Items[i].BaseMapKey);
    end;
  FPolygonLayers.SortType := ComCtrls.stNone;
  FPolygonLayers.SortType := ComCtrls.stText;
end;

{-------------------------------------------------------------------------------
Finds the samples in the chosen polygon
}
function TPolygonSelection.ReadItemsInPolygon(AConnection: TADOConnection;
    includePartialOverlap: Boolean): string;
var
  lMapNode: TFlyNode;
  lMapServerLink: TMapServerLink;

  //----------------------------------------------------------------------------
  procedure GetItemsFromPolygons(AMapNode: TFLyNode; const BaseMapSpatialSystem: String);
  var lSheetNode, lPolygonNode: TFlyNode;
      lScanner: TPolygonScanner;
      lPolygonsCount, lMissingPolygonsCount: integer;
  begin
    if Assigned(AMapNode) then lSheetNode := AMapNode.GetFirstChild
                          else lSheetNode := FPolygonLayers.Items.GetFirstNode;
    lPolygonsCount := 0;
    lMissingPolygonsCount := 0;
    while Assigned(lSheetNode) do begin
      if lSheetNode.ImageIndex <> UNCHECKED_IMG_IDX then begin
        // At least one polygon on this layer, so create the PolygonScanner, we'll need it.
        lScanner := TPolygonScanner.Create(lMapServerLink.MapHandle, BaseMapSpatialSystem);
        try
          lPolygonNode := lSheetNode.GetFirstChild;
          while Assigned(lPolygonNode) do begin
            if lPolygonNode.ImageIndex = CHECKED_IMG_IDX then begin
              Inc(lPolygonsCount);
              try
                if IsForLocations then
                  lScanner.GetLocationsForPolygon(
                        lMapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath +
                                                         TKeyData(lSheetNode.Data).ItemAdditional),
                        Integer(lPolygonNode.Data),
                        AConnection,
                        includePartialOverlap)
                else
                  lScanner.GetSamplesForPolygon(
                        lMapServerLink.SheetIDByFileName(AppSettings.ObjectSheetFilePath +
                                                         TKeyData(lSheetNode.Data).ItemAdditional),
                        Integer(lPolygonNode.Data),
                        AConnection,
                        includePartialOverlap);
                Result := 'Polygon Selected';
                TFrmParameters(Owner).FPolygonChosen := True;
              except
                on E:EMapPolygonScanner do
                  if E.Message = ResStr_PolygonMissing then
                    Inc(lMissingPolygonsCount)
                  else
                    raise;
              end; // try
            end;
            // Next polygon node.
            lPolygonNode := lPolygonNode.GetNextSibling;
          end;
        finally
          lScanner.Free;
        end;
      end;
      // Next map sheet node.
      lSheetNode := lSheetNode.GetNextSibling;
    end;
    if lMissingPolygonsCount=lPolygonsCount then
      raise EParametersException.CreateNonCritical(ResStr_AllPolygonsDeleted)
    else if lMissingPolygonsCount<>0 then
      ShowInformation(ResStr_SomePolygonsDeleted);
  end;
  //----------------------------------------------------------------------------

begin
  lMapServerLink := TMapServerLink.Create(nil);
  try
    // If one map, top node is NOT a base map.
    if AppSettings.AvailableMaps.Count = 1 then
      with AppSettings.AvailableMaps[0] do begin
        lMapServerLink.ActiveDataset := BaseMapKey + '.gds';
        GetItemsFromPolygons(nil, SpatialSystem);
      end
    else begin
      lMapNode := FPolygonLayers.Items.GetFirstNode;
      while Assigned(lMapNode) do begin
        if lMapNode.ImageIndex <> UNCHECKED_IMG_IDX then begin
          lMapServerLink.ActiveDataset := TKeyData(lMapNode.Data).ItemKey + '.gds';
          GetItemsFromPolygons(
              lMapNode,
              AppSettings.AvailableMaps.ItemsByKey[TKeyData(lMapNode.Data).ItemKey].SpatialSystem);
        end;
        lMapNode := lMapNode.GetNextSibling;
      end;
    end;
  finally
    lMapServerLink.Free;
  end;
end;

//==============================================================================
//Calls the TPolygonSelection method to find the samples in the polygon
function TfrmParameters.SetupPolygonResults(APolygonSelection: TPolygonSelection;
    includePartialOverlap: Boolean): String;
begin
  Result := APolygonSelection.ReadItemsInPolygon(FReportFile.Connection, includePartialOverlap);
end;




end.

