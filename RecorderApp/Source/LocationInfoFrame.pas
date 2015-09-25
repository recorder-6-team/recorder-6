unit LocationInfoFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, BaseDragFormUnit, AddinCompositeComponent, AddinLinkedControls,
  StdCtrls, CompositeComponent, SpatialRef, DataClasses, BaseFormUnit,
  Menus, LocOnFly;

resourcestring
  ResStr_LocationNotRecognised = 'The location specified is not recognised.  Before specifying '+
      'a location, it must first be added to the list of locations in the database using the '+
      'Locations data entry screen.';
  ResStr_SpatialRefQualifierRequired = 'A qualifier must be entered to support the spatial Reference. '+
      'This is important to help determine the quality and accuracy of the spatial reference.';
  ResStr_SpatialRefNotRecognised =  'The spatial reference is not recognised.';

  ResStr_InvalidLocationName =  'The Location Name is either missing or invalid. ' +
                                ' Please enter a Location, a Spatial Reference or a Location Name.';
  ResStr_UpdateSrefToSiteCentroid = 'Do you want to update the spatial reference to the centroid of the selected location?';

type
  TfraLocationInfo = class(TFrame)
    Label6: TLabel;
    Label7: TLabel;
    eSpatialRef: TSpatialRef;
    Label8: TLabel;
    eLocationName: TEdit;
    eLocation: TAddinLinkedEdit;
    pmMaps: TPopupMenu;
    procedure eLocationFindData(Sender: TObject);
    procedure eLocationGetData(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure eSpatialRefGetFromMap(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FContainerForm: TBaseForm;
    FOnChangedContent: TNotifyEvent;
    FReadOnly: Boolean;
    FOnUpdateLocation: TNotifyEvent;
    procedure ChangedContent;
    procedure DropLocation(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure UpdateLocation(KeyList: TKeyList);
    procedure SetOnChangedContent(Value: TNotifyEvent);
    function GetHasSpatialInfo: Boolean;
    function GetLocalityLabel: String;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateSpatialRef(KeyList: TKeyList);
    procedure MapForSpatialRefClick(Sender: TObject);
    procedure SetOnUpdateLocation(Value: TNotifyEvent);
    function CheckLocationEdit: Boolean;
    procedure SetLocationAndSpatialRef(const ANewKey: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure RegisterDragDrop(AParentForm: TBaseForm);
    procedure Validate(const ASurveyKey: String = '');
    procedure FindOnMap;
    procedure UpdateMapWindowSelector;
    procedure Refresh;
    procedure SetColour(AColour: TColor);
    property OnChangedContent: TNotifyEvent read FOnChangedContent write SetOnChangedContent;
    property HasSpatialInfo: Boolean read GetHasSpatialInfo;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property OnUpdateLocation: TNotifyEvent read FOnUpdateLocation write SetOnUpdateLocation;
    property LocalityLabel: String read GetLocalityLabel;
  end;

implementation

{$R *.dfm}

uses
  GeneralData, Constants, ApplicationSettings, DropTarget, FormActions,
  SpatialRefFuncs, GeneralFunctions, ValidationData, Map, Find,
  DatabaseAccessADO;

{-------------------------------------------------------------------------------
}
constructor TfraLocationInfo.Create(AOwner: TComponent);
begin
  inherited;
  eSpatialRef.Modified := false;
end;

{-------------------------------------------------------------------------------
  Drag drop handler for locations being dropped onto frame.
}
procedure TfraLocationInfo.DropLocation(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  if (ASourceData <> nil) and (ASourceData.Header.ItemCount > 0) and (not Readonly) then begin
    if (AFormat = CF_JNCCDATA) and
       (SameText(ASourceData.Header.TableName, TN_LOCATION) or
        SameText(ASourceData.Header.TableName, 'Spatial_Ref')) then
    begin
      AHandled := True;
      if ASourceData.Items[0].KeyField1 <> '' then
        SetLocationAndSpatialRef(ASourceData.Items[0].KeyField1);

      if SameText(ASourceData.Header.TableName, 'Spatial_Ref') then
      begin
        { Overwrite the Spatial ref returned from the database with the one
          passed from the map. }
        eSpatialRef.Values := dmGeneralData.SetSpatialRef(
            ASourceData.Items[0].KeyField2, '', AppSettings.SpatialRefSystem);
      end else
        AHandled := False
    end
  end else
    AHandled := True;
end;  // TfraLocationInfo.DropLocation

{-------------------------------------------------------------------------------
  Registration of drag and drop methods
}
procedure TfraLocationInfo.RegisterDragDrop(AParentForm: TBaseForm);
begin
  AParentForm.RegisterDropComponent(eLocation, DropLocation,
                         [TN_LOCATION, 'SPATIAL_REF'], [CF_JNCCDATA, CF_TEXT]);
  AParentForm.RegisterDropComponent(eSpatialRef.ControlSpatialRef, DropLocation,
                         [TN_LOCATION, 'SPATIAL_REF'], [CF_JNCCDATA, CF_TEXT]);
  FContainerForm := AParentForm;
end;

{-------------------------------------------------------------------------------
  Click Return data button - display locations list and setup return data link
}
procedure TfraLocationInfo.eLocationGetData(Sender: TObject);
begin
  dmFormActions.actLocations.Execute;
  FContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                          FContainerForm, UpdateLocation);
end;

{-------------------------------------------------------------------------------
  Click return whilst typing to start up Find dialog
}
procedure TfraLocationInfo.eLocationFindData(Sender: TObject);
begin
  inherited;
  CheckLocationEdit;
  ChangedContent;
end;  // TfraLocationInfo.eLocationFindData

{-------------------------------------------------------------------------------
}
procedure TfraLocationInfo.UpdateLocation(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil) and (KeyList.Header.ItemCount > 0) then
      SetLocationAndSpatialRef(KeyList.Items[0].KeyField1);
  finally
    KeyList.Free;
  end;
  if Assigned(FOnUpdateLocation) then
    FOnUpdateLocation(self);
  ChangedContent;
end;  // TfraLocationInfo.UpdateLocation

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfraLocationInfo.SetOnChangedContent(Value: TNotifyEvent);
begin
  FOnChangedContent := Value;
end;

{-------------------------------------------------------------------------------
  Fire event if content changes
}
procedure TfraLocationInfo.ChangedContent;
begin
  if Assigned(FOnChangedContent) then
    FOnChangedContent(Self);
end;

{-------------------------------------------------------------------------------
  Return true if there is (or should be) spatial reference info available
    on the frame
}
function TfraLocationInfo.GetHasSpatialInfo: Boolean;
begin
  Result := (eLocation.Text <> '') or (eSpatialRef.SpatialRef <> '') or (eLocationName.Text = '');
end;

{-------------------------------------------------------------------------------
  Return the location info as a string. Result depends on which controls are populated
}
function TfraLocationInfo.GetLocalityLabel: String;
begin    
  if eLocation.Text <> '' then
    Result := eLocation.Text
  else
  if eSpatialRef.DisplayRef <> '' then
    Result := LocaliseSpatialRef(eSpatialRef.DisplayRef)
  else
    Result := eLocationName.Text;
end;

{-------------------------------------------------------------------------------
  Perform basic validation of the data.  Raises errors if anything fails.
     If a survey key is specified, then checks that it falls inside the
     bounding box.
}
procedure TfraLocationInfo.Validate(const ASurveyKey: String = '');
var
  lValidRef   : TValidSpatialRef;
  lValidResult: TValidationResult;
begin
  if HasSpatialInfo then
  begin
    lValidRef := ValidSpatialRef(eSpatialRef.SpatialRef, eSpatialRef.DisplaySystem);
    FContainerForm.ValidateValue(
        lValidRef.Valid,
        ResStr_SpatialRefNotRecognised,
        TWinControl(eSpatialRef.ControlSpatialRef));
  end;
  { If there is a Spatial Ref then validate presence of Qualifier }
  FContainerForm.ValidateValue(
      (eSpatialRef.DisplayRef = '') or (eSpatialRef.Qualifier <> ''),
      ResStr_SpatialRefQualifierRequired,
      TWinControl(eSpatialRef.ControlQualifier));

  { Check the specified location is valid }
  if eLocation.Text <> '' then
    FContainerForm.ValidateValue(CheckLocationEdit, ResStr_LocationNotRecognised, eLocation);

  { Check that at least something has been specified }
  FContainerForm.ValidateValue(
      (eSpatialRef.DisplayRef <> '') or (eLocation.Text<>'') or (eLocationName.Text<>''),
      ResStr_InvalidLocationName,
      eLocation);

  { Set up Location key and spatial reference field }
  if eLocation.Text = '' then
    eLocation.Key := '';
  if eSpatialRef.EnteredSystem = '' then
    eSpatialRef.EnteredSystem := DetermineSpatialRefSystem(eSpatialRef.EnteredRef);

  { Check the event falls within the bounding box for the survey }
  if HasSpatialInfo and (ASurveyKey<>'') then
  begin
    lValidResult := dmValidation.CheckEventInSurvey(
        ASurveyKey,
        eSpatialRef.EnteredRef,
        eSpatialRef.EnteredSystem,
        eLocation.Key);
    FContainerForm.ValidateValue(lValidResult.Success, lValidResult.Message, eLocation);
  end;
end;

{-------------------------------------------------------------------------------
  Accessor.  Propogates read only state to the contained controls
}
procedure TfraLocationInfo.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  if FReadOnly then
    eLocation.EditMode := AddinCompositeComponent.emBrowse
  else
    eLocation.EditMode := AddinCompositeComponent.emEdit;

  eLocationName.ReadOnly  := FReadOnly;

  if FReadOnly then
    eSpatialRef.EditMode := Constants.emView
  else
    eSpatialRef.EditMode := Constants.emEdit;
end;

{-------------------------------------------------------------------------------
  Centre the map onto the location specified on this frame
}
procedure TfraLocationInfo.FindOnMap;
begin
  TfrmMap.CentreOnPlace(eLocation.Key, eLocation.Text,
                        eSpatialRef.DisplayRef,
                        eSpatialRef.DisplaySystem);
end;

{-------------------------------------------------------------------------------
  Track if any content has changed
}
procedure TfraLocationInfo.DataChange(Sender: TObject);
begin
  ChangedContent;
end;

{-------------------------------------------------------------------------------
  When clicking on magnifying glass on spatial ref, go to the map to retrieve
      the reference
}
procedure TfraLocationInfo.eSpatialRefGetFromMap(Sender: TObject);
begin
  if dmFormActions.DefaultMapWindow <> nil then
    FContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                            FContainerForm, UpdateSpatialRef);
end;

{-------------------------------------------------------------------------------
  Update when a spatial reference is returned from the map
}
procedure TfraLocationInfo.UpdateSpatialRef(KeyList: TKeyList);
begin
  try
    if (KeyList <> nil)  and (KeyList.Header.ItemCount > 0) then
    begin
      if (CompareText(KeyList.Header.TableName, 'Spatial_Ref') = 0) then
      begin
        if KeyList.Items[0].KeyField1 <> '' then
          SetLocationAndSpatialRef(KeyList.Items[0].KeyField1);
        if KeyList.Items[0].KeyField2 <> '' then begin
          eSpatialRef.Values := dmGeneralData.SetSpatialRef(
              KeyList.Items[0].KeyField2, '', AppSettings.SpatialRefSystem);
        end;
      end;
    end;
  finally
    KeyList.Free;
  end;
  if Assigned(FOnUpdateLocation) then
    FOnUpdateLocation(self);
  ChangedContent;
end;  // TfraLocationInfo.UpdateSpatialRef

{-------------------------------------------------------------------------------
  Click handler for map submenu items on the spatial ref control
}
procedure TfraLocationInfo.MapForSpatialRefClick(Sender: TObject);
begin
  dmFormActions.MapWindowMenuClick(Sender);
  FContainerForm.SetupLink(
      TBaseForm(Application.MainForm.ActiveMDIChild),
      FContainerForm, UpdateSpatialRef);
end;

{-------------------------------------------------------------------------------
  Update the list of map menu items when the user adds a new map
}
procedure TfraLocationInfo.UpdateMapWindowSelector;
begin
  AppSettings.UpdateMapMenu(FContainerForm, pmMaps.Items, True, MapForSpatialRefClick);
  eSpatialRef.UpdateMapWindowSelector;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TfraLocationInfo.SetOnUpdateLocation(Value: TNotifyEvent);
begin
  FOnUpdateLocation := Value;
end;

{-------------------------------------------------------------------------------
  Checks if the supplied location name edit box contains a valid location
  name that matches the location key provided.  If so, then fills in correct
  case details.  If not, then looks for a unique hit on the name, fills in
  correct case details and updates the key value.  Else, displays the find
  dialog and returns the chosen key.  Returns false if user cancels this,
  or if there were no partial matches at all in the find dialog. When a location
  is found, if the spatial reference is not filled in then sets it to the
  centroid of the location.  Returns true if location populated Ok. }
function TfraLocationInfo.CheckLocationEdit: Boolean;
var
  lFind: TdlgFind;
begin
  Result := false;
  if eLocation.Key <> '' then begin
    with dmDatabase.GetRecordset('usp_Location_Select', ['@Key', eLocation.Key]) do
      if not (EOF or BOF) then
        if SameText(Fields['Item_Name'].Value, eLocation.Text) then begin
          SetLocationAndSpatialRef(eLocation.Key);
          Result := true;
        end;
  end;

  if not Result then begin
    lFind := TdlgFind.CreateDialog(nil,ResStr_FindLocationName,ftLocation);
    with lFind do begin
      try
        if FindUnique(eLocation.Text) then begin
          SetLocationAndSpatialRef(dmGeneralData.GetLocKeyFromLocNameKey(lFind.ItemKey));
          Result := true;
        end else
        if not eSearchText.NoSourceItems then begin
          if ShowModal=mrOk then begin
            SetLocationAndSpatialRef(dmGeneralData.GetLocKeyFromLocNameKey(lFind.ItemKey));
            Result := true;
          end;
        end else begin // No source items
          MessageDlg(ResStr_NoLocationItems, mtInformation, [mbOK], 0);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Use the new key to identify the location name and place it in the edit
      control.  If the previous location's spatial ref had not been manually
      modified, overwrite it with the new location's.
}
procedure TfraLocationInfo.SetLocationAndSpatialRef(const ANewKey: String);
var
  doUpdateSref: boolean;
begin
  // force spatial ref control to pick up new entered ref
  eSpatialRef.DoExit;
  doUpdateSref := false;
  // Are we updating the location to a new loc key?
  if (ANewKey<>'') and (eLocation.Key<>ANewKey) then begin
    // If so, do we already have an sref? If so need to prompt to confirm
    if (eSpatialRef.SpatialRef<>'') then
      doUpdateSref := ConfirmYesNo(ResStr_UpdateSrefToSiteCentroid)=mrYes
    else
      doUpdateSref := true;
  end;
  eLocation.Text := dmGeneralData.GetLocationName(ANewKey);
  if doUpdateSref then
    // Passing aNewKey to SetSpatialRef will force it to put the selected location's sref into the
    // eSpatialRef control.
    eSpatialRef.Values :=
      dmGeneralData.SetSpatialRef(eSpatialRef.SpatialRef, ANewKey, AppSettings.SpatialRefSystem);
  eLocation.Key := ANewKey;
end;

{-------------------------------------------------------------------------------
  Reload the location details using the existing key
}
procedure TfraLocationInfo.Refresh;
begin
  eSpatialRef.DoExit;
  SetLocationAndSpatialRef(eLocation.Key);
end;

{-------------------------------------------------------------------------------
  Set the background colours of the location and spatial ref controls.
}
procedure TfraLocationInfo.SetColour(AColour: TColor);
begin
  eLocation.Color                     := AColour;
  eLocationName.Color                 := AColour;
  eSpatialRef.ControlSpatialRef.Color := AColour;
  eSpatialRef.ControlQualifier.Color  := AColour;
end;

procedure TfraLocationInfo.FrameResize(Sender: TObject);
var
  adjust: integer;
begin
  eLocation.Width := Width - eLocation.Left;
  // calculate any adjustment made to the width of the spatial ref control applied by translation,
  // and respect that adjustment in the new width.
  adjust := StrToInt(LocalizerOnFly.GetTranslatedProperty('fraLocationInfo', 'eSpatialRef.Width'))
      - StrToInt(LocalizerOnFly.PropValueByName(LocalizerOnFly.NativeLocale, 'fraLocationInfo', 'eSpatialRef.Width'));
  eSpatialRef.Width := adjust + Width - eSpatialRef.Left;
end;

end.
