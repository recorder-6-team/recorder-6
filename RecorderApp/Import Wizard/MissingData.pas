{===============================================================================
  Unit:        MissingData

  Defines:     TfraMissingData

  Description: Allow user to type in any values that are required but were
               not specified in a column.

  Model:       ImportWizard

  Last revision information:
    $Revision: 30 $
    $Date: 06/03/13 09:13 $
    $Author: Michaelcaptain $

===============================================================================}
unit MissingData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  IWBasePage, StdCtrls, Menus, ComboListID, CompositeComponent, SpatialRef, Constants,
  AddinCompositeComponent, AddinLinkedControls, ExtCtrls, HTMLView, VagueDate,
  VagueDateEdit, FormActions, DataClasses, BaseFormUnit, DropTarget,
  LocationInfoFrame, IWSettings;

resourcestring
  ResStr_Required = 'A value for %s is required';

type
  {-----------------------------------------------------------------------------
    Frame requesting the user to enter data for any database attributes that are required
    but not specified in the import file.  The values specified then apply to every record
    in the import file.  This always includes the survey, but also includes other fields
    such as observers, term lists and location details depending on the columns specified
    in the imported data.  All combo boxes on this page are repopulated if the user selects
    another screen then returns to the import wizard, since they may have created new
    entries in the underlying list.
    This wizard page is always displayed.
  }
  TfraMissingData = class(TBasePage)
    cmbSurvey: TIDComboBox;
    eDate: TVagueDateEdit;
    eDeterminer: TNameLinkedEdit;
    eObserver: TNameLinkedEdit;
    lblObserverName: TLabel;
    lblSurvey: TLabel;
    lblSelectSurvey: TLabel;
    lblOtherValues: TLabel;
    lblSelectValues: TLabel;
    lblDate: TLabel;
    lblSelectDate: TLabel;
    lblSingleObserver: TLabel;
    lblDateValue: TLabel;
    lblDeterminerName: TLabel;
    lblDeterminer: TLabel;
    lblNoDeterminerColumn: TLabel;
    lblNoObserverColumn: TLabel;
    lblSingleLocation: TLabel;
    lblSelectLocation: TLabel;
    lblSurveyName: TLabel;
    pnlDate: TPanel;
    pnlDeterminer: TPanel;
    pnlLocation: TPanel;
    pnlObserver: TPanel;
    pnlSurvey: TPanel;
    pnlTermLists: TPanel;
    rbFirstObserverAsDeterminer: TRadioButton;
    rbSpecifyDeterminer: TRadioButton;
    shpLocationSeparator: TShape;
    shpObserverSeparator: TShape;
    shpSurveySeparator: TShape;
    shpOtherValuesSeparator: TShape;
    shpDateSeparator: TShape;
    shpDeterminerSeparator: TShape;
    fraLocationInfo: TfraLocationInfo;
    procedure cmbSurveyPopulate(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure eDateExit(Sender: TObject);
    procedure eSpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
    procedure NameFindData(Sender: TObject);
    procedure NameGetData(Sender: TObject);
  private
    FTermListCombos: TList;
    procedure DropDeterminer(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure DropObserver(const Sender: TObject; const AFormat: Integer; const ASourceData:
        TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
    procedure PopulateTermListPanel;
    procedure UpdateDeterminer(KeyList: TKeyList);
    procedure UpdateName(ATarget: TAddinLinkedEdit; KeyList: TKeyList);
    procedure UpdateObserver(KeyList: TKeyList);
  protected
    function GetBorderWidth: Integer; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetHTMLImageName: String; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    procedure LoadContent; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    procedure RefreshContent; override;
    procedure RegisterDragDropComponents; override;
    procedure SaveContent; override;
    procedure UnRegisterDragDropComponents; override;
    procedure UpdateMapWindowSelector; override;
    procedure ValidateContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  IWConstants, MatchGeneric, ColumnTypes, FastColumnTypes, ApplicationSettings, Find,
  GeneralData, DatabaseAccessADO, ValidationData, SpatialRefFuncs, IWResourceStrings,
  IWColumnMappingClasses, ADOInt, GeneralFunctions, OnlineHelp;

{-==============================================================================
    TfraMissingData
===============================================================================}

{-------------------------------------------------------------------------------
  Constructor
}
constructor TfraMissingData.Create(AOwner: TComponent; ASettings:
    TdmIWSettings);
begin
  inherited;
  HelpContext := IDH_IWMISSINGDATA;
end;

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.cmbSurveyPopulate(Sender: TObject);
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
end;  // TfraMissingData.cmbSurveyPopulate 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.DataChange(Sender: TObject);
begin
  inherited;
  if Sender = eDeterminer then rbSpecifyDeterminer.Checked := True;
  ChangedContent;
end;  // TfraMissingData.DataChange

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.DropDeterminer(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  AHandled := dmGeneralData.DropLinkedEditText(
      eDeterminer,
      AFormat,
      ASourceData,
      dmGeneralData.GetIndividualName,
      ATextStrings);
  if AHandled then begin
    rbSpecifyDeterminer.Checked := True;
    ChangedContent;
  end;
end;  // TfraMissingData.DropDeterminer

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.DropObserver(const Sender: TObject; const AFormat: Integer; const
    ASourceData: TKeyList; const ATextStrings: TStringList; var AHandled: Boolean);
begin
  AHandled := dmGeneralData.DropLinkedEditText(
      eObserver,
      AFormat,
      ASourceData,
      dmGeneralData.GetIndividualName,
      ATextStrings);
end;  // TfraMissingData.DropObserver 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.eDateExit(Sender: TObject);
begin
  inherited;
  if not DoValidateContent then Exit;
  
  ContainerForm.ValidateValue(
      IsVagueDate(eDate.Text),
      ResStr_InvalidVagueDate, eDate);
  ContainerForm.ValidateValue(
      CheckVagueDate(eDate.Text),
      InvalidDate(ResStr_TheDate, True, False), eDate);

  eDate.Text := VagueDateToString(eDate.VagueDate);
end;  // TfraMissingData.eDateExit 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.eSpatialRefInvalidSpatialRef(Sender: TObject; var Handled: Boolean);
begin
  inherited;
  Handled := not DoValidateContent;
end;  // TfraMissingData.eSpatialRefInvalidSpatialRef 

{-------------------------------------------------------------------------------
  Page has own scrollbar, so no need for extra gap around it. 
}
function TfraMissingData.GetBorderWidth: Integer;
begin
  Result := 0;
end;  // TfraMissingData.GetBorderWidth 

{-------------------------------------------------------------------------------
}
function TfraMissingData.GetHasNext: Boolean;
var
  i: Integer;
  lBullets: TStringList;
begin
  lBullets := TStringList.Create;
  try
    // Check all combos have a selection.
    if ((cmbSurvey.ItemIndex = -1) and pnlSurvey.Visible) then
      lBullets.Add(Format(ResStr_Required, [lblSurvey.Caption]));
    if not (not pnlLocation.Visible or
           ((Trim(fraLocationInfo.eSpatialRef.SpatialRef) <> '')
            and (Trim(fraLocationInfo.eSpatialRef.Qualifier) <> '')) or
           (Trim(fraLocationInfo.eLocation.Text) <> '') or
           (Trim(fraLocationInfo.eLocationName.Text) <> '')) then
      lBullets.Add(Format(ResStr_Required, [lblSingleLocation.Caption]));

    if pnlDate.Visible and (eDate.Text = '') then
      lBullets.Add(Format(ResStr_Required, [lblDate.Caption]));

    if pnlObserver.Visible and (eObserver.Text = '') then
      lBullets.Add(Format(ResStr_Required, [lblSingleObserver.Caption]));

    if pnlDeterminer.Visible and
       (not rbFirstObserverAsDeterminer.Checked) and (eDeterminer.Text = '') then
      lBullets.Add(Format(ResStr_Required, [lblDeterminer.Caption]));

    for i := 0 to FTermListCombos.Count - 1 do
      if TIDComboBox(FTermListCombos[i]).ItemIndex = -1 then
        lBullets.Add(Format(
            ResStr_Required,
            [ReadableFormat(TIDComboBox(FTermListCombos[i]).Name)]));

    Result := lBullets.Count = 0;
    ChangedHtml(lBullets);
  finally
    lBullets.Free;
  end; // try
end;  // TfraMissingData.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraMissingData.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraMissingData.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraMissingData.GetHTMLImageName: String;
begin
  Result := 'Data.jpg';
end;  // TfraMissingData.GetHTMLImageName

{-------------------------------------------------------------------------------
}
function TfraMissingData.GetNext: TBasePageClass;
begin
  Result := TfraMatchGeneric;
end;  // TfraMissingData.GetNext 

{-------------------------------------------------------------------------------
}
function TfraMissingData.GetPrevious: TBasePageClass;
begin
  if Settings.UseOldImportWizard then
    Result := TfraColumnTypes
  else
    Result := TfraFastColumnTypes;
end;  // TfraMissingData.GetPrevious 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.LoadContent;
var
  lKey: TKeyString;
  lColour: TColor;
begin
  inherited;
  FTermListCombos := TList.Create;
  
  UpdateMapWindowSelector;
  // Survey already selected in the new import wizard at FileSelect
  if (not Settings.UseOldImportWizard) then
    pnlSurvey.Hide
  else begin
    // Just to get the right survey in there.
    cmbSurvey.Clear;
    cmbSurvey.PopulateContent;
    with Settings.UserSuppliedData do
    begin
      if SurveyKey <> '' then
        cmbSurvey.ItemIndex := cmbSurvey.IDIndexOf(SurveyKey);
    end;
  end;
  with Settings.UserSuppliedData do
  begin
    with Settings.ImportFile.ColumnMapping do
    begin
      pnlLocation.Visible   := (not IsMapped(ColumnTypeByKey(CT_KEY_GRID_REFERENCE)))
          and (not IsMapped(ColumnTypeByKey(CT_KEY_LOCATION_NAME)));
      pnlDate.Visible       := not IsMapped(ColumnTypeByKey(CT_KEY_DATE));
      pnlObserver.Visible   := not IsMapped(ColumnTypeByKey(CT_KEY_OBSERVER));
      pnlDeterminer.Visible := not pnlObserver.Visible
          and (not IsMapped(ColumnTypeByKey(CT_KEY_DETERMINER)));
    end;

    lKey := SingleLocationKey;
    // Don't overwrite location text that hasn't been linked yet, if return data used.
    if lKey <> '' then
      fraLocationInfo.eLocation.Text := dmGeneralData.GetLocationName(lKey);
    fraLocationInfo.eLocation.Key := lKey;
    fraLocationInfo.eSpatialRef.Qualifier     := SingleSpatialRefQualifier;
    fraLocationInfo.eSpatialRef.EnteredRef    := SingleSpatialRef;
    fraLocationInfo.eSpatialRef.EnteredSystem := SingleSpatialRefSystem;
    fraLocationInfo.eSpatialRef.DisplaySystem := AppSettings.SpatialRefSystem;
    fraLocationInfo.eLocationName.Text        := SingleLocationName;
    eDate.Text       := SingleDate;
    eObserver.Key    := SingleObserverKey;
    eObserver.Text   := dmGeneralData.GetName(eObserver.Key);
    eDeterminer.Key  := SingleDeterminerKey;
    eDeterminer.Text := dmGeneralData.GetName(eDeterminer.Key);
    // Do radio buttons AFTER name set, due to events on the way.
    rbFirstObserverAsDeterminer.Checked := UseObserverAsDeterminer;
    rbSpecifyDeterminer.Checked         := UseSpecificDeterminer;

    PopulateTermListPanel;
  end;

  ContainerForm.SetRequiredFieldsColourState(True, [cmbSurvey, eDate, eObserver.EditBox]);
  fraLocationInfo.SetColour(MergeColours(AppSettings.MandatoryColour, clWindow, 25));
  if AppSettings.DisableDragDropFrames then lColour := clWindowFrame
                                       else lColour := AppSettings.DragDestColour;
  fraLocationInfo.eLocation.DragDestinationColour := lColour;
  fraLocationInfo.eSpatialRef.DestCol             := lColour;
  eObserver.DragDestinationColour   := lColour;
  eDeterminer.DragDestinationColour := lColour;
  ChangedContent;  
end;  // TfraMissingData.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.NameFindData(Sender: TObject);
begin
  inherited;
  dmGeneralData.CheckIndividual(TAddinLinkedEdit(Sender));
end;  // TfraMissingData.NameFindData

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.NameGetData(Sender: TObject);
begin
  inherited;
  dmFormActions.actNames.Execute;
  if Sender = eObserver then
    ContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                            ContainerForm, UpdateObserver)
  else
    ContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                            ContainerForm, UpdateDeterminer);
end;  // TfraMissingData.NameGetData 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.PopulateTermListPanel;
var
  lTopPos, lMaxWidth, i: Integer;

  procedure SetupControlsForTermList(const AColumnType: TColumnType);
  var
    lCombo: TIDComboBox;
  begin
    // If component found, it's refresh in progress, otherwise, it's first load.
    lCombo := TIDComboBox(FindComponent(AColumnType.TermListTable));
    if lCombo = nil then
      with TLabel.Create(Self) do begin
        Parent  := pnlTermLists;
        Top     := lTopPos;
        Left    := 32;
        Caption := AColumnType.Name + ':';
        if lMaxWidth < Canvas.TextWidth(Caption) then
          lMaxWidth := Canvas.TextWidth(Caption);
      end;

    if lCombo = nil then
      lCombo := TIDComboBox.Create(Self);
    // Load or refresh, the list was cleared, so add combo in any case.
    FTermListCombos.Add(lCombo);
  
    with lCombo do begin
      Name      := AColumnType.TermListTable;
      Parent    := pnlTermLists;
      Top       := lTopPos - 4;
      Width     := 220;
      Style     := csDropDownList;
      Ctl3D     := False;
      BevelKind := bkFlat;
      OnChange  := DataChange;
  
      { Force Populated to be set to True, so all items will stay even after first dropdown.}
      PopulateContent;
      with dmDatabase.ExecuteSQL('SELECT * FROM ' + AColumnType.TermListTable, True) do
        try
          while not Eof do begin
            Add(VarToStr(Fields['Short_Name'].Value),
                VarToStr(Fields[AColumnType.TermListTable + '_Key'].Value));
            MoveNext;
          end;
        finally
          Close;
        end;

      // There will always be en entry in the list, see above, so go for it.
      ItemIndex := IDIndexOf(Settings.UserSuppliedData.TermKey[Name]);
    end;
    ContainerForm.SetRequiredFieldsColourState(True, [lCombo]);
    Inc(lTopPos, 28);
    pnlTermLists.Height := lTopPos + 4;
  end;
  
begin
  with Settings.ImportFile.ColumnMapping do begin
    pnlTermLists.Visible := False;
    lTopPos   := 52;
    lMaxWidth := 84;  // Vertically aligned with other controls in other panels.
  
    for i := 0 to TermFieldsRequiredCount - 1 do
      SetupControlsForTermList(TermFieldsRequired[i]);
    pnlTermLists.Visible := TermFieldsRequiredCount > 0;
  
    // Now align all combos, looks better.
    for i := 0 to FTermListCombos.Count - 1 do
      TIDComboBox(FTermListCombos[i]).Left := lMaxWidth + 36;
  end;
end;  // TfraMissingData.PopulateTermListPanel 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.RefreshContent;
begin
  SaveContent;
  LoadContent;
end;  // TfraMissingData.RefreshContent 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.RegisterDragDropComponents;
begin
  fraLocationInfo.RegisterDragDrop(ContainerForm);
  ContainerForm.RegisterDropComponent(eObserver, DropObserver, [TN_NAME, TN_INDIVIDUAL],
                                      [CF_JNCCDATA]);
  ContainerForm.RegisterDropComponent(eDeterminer, DropDeterminer, [TN_NAME, TN_INDIVIDUAL],
                                      [CF_JNCCDATA]);
  // Setup F11 shortcut
  eObserver.NameKey := AppSettings.UserId;
  eObserver.NameText := AppSettings.UserName;
  eDeterminer.NameKey := AppSettings.UserId;
  eDeterminer.NameText := AppSettings.UserName;
end;  // TfraMissingData.RegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.SaveContent;
var
  i: Integer;
begin
  inherited;
  with Settings.UserSuppliedData do begin
    if Settings.UseOldImportWizard then
      SurveyKey                 := cmbSurvey.CurrentStrID;
    SingleLocationKey         := fraLocationInfo.eLocation.Key;
    SingleSpatialRef          := fraLocationInfo.eSpatialRef.EnteredRef;
    SingleSpatialRefSystem    := fraLocationInfo.eSpatialRef.EnteredSystem;
    SingleSpatialRefQualifier := fraLocationInfo.eSpatialRef.Qualifier;
    SingleLocationName        := fraLocationInfo.eLocationName.Text;
    SingleDate                := eDate.Text;
    SingleObserverKey         := eObserver.Key;
    UseObserverAsDeterminer   := rbFirstObserverAsDeterminer.Checked;
    UseSpecificDeterminer     := rbSpecifyDeterminer.Checked;
    SingleDeterminerKey       := eDeterminer.Key;
  
    for i := 0 to FTermListCombos.Count - 1 do
      with TIDComboBox(FTermListCombos[i]) do
        TermKey[Name] := CurrentStrID;
  end;
  FTermListCombos.Free;
end;  // TfraMissingData.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.UnRegisterDragDropComponents;
begin
  with fraLocationInfo do
    ContainerForm.UnRegisterDragDropComponents([eLocation, eSpatialRef.ControlSpatialRef]);
  ContainerForm.UnRegisterDragDropComponents([eObserver, eDeterminer]);
end;  // TfraMissingData.UnRegisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.UpdateDeterminer(KeyList: TKeyList);
begin
  UpdateName(eDeterminer, KeyList);
  rbSpecifyDeterminer.Checked := True;
  ChangedContent;
end;  // TfraMissingData.UpdateDeterminer

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.UpdateMapWindowSelector;
begin
  fraLocationInfo.UpdateMapWindowSelector;
end;  // TfraMissingData.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.UpdateName(ATarget: TAddinLinkedEdit; KeyList: TKeyList);
begin
  try
    if Assigned(KeyList) then begin
      if (KeyList.Header.ItemCount > 0) and
         SameText(KeyList.Items[0].KeyField2, 'Individual') then
      begin
        ATarget.Key := KeyList.Items[0].KeyField1;
        ATarget.Text:= dmGeneralData.GetIndividualName(ATarget.Key);
      end;
    end;
  finally
    KeyList.Free;
  end;
  ChangedContent;
end;  // TfraMissingData.UpdateName

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.UpdateObserver(KeyList: TKeyList);
begin
  UpdateName(eObserver, KeyList);
end;  // TfraMissingData.UpdateObserver 

{-------------------------------------------------------------------------------
}
procedure TfraMissingData.ValidateContent;
var
  SurveyKey: String;
begin
  inherited;
  if pnlLocation.Visible then
  begin
    if Settings.UseOldImportWizard then
      SurveyKey := cmbSurvey.CurrentStrID
    else
      SurveyKey := Settings.UserSuppliedData.SurveyKey;
    fraLocationInfo.Validate(SurveyKey);
  end;

  if pnlDate.Visible then
    eDateExit(nil);
  
  if pnlObserver.Visible then
    ContainerForm.ValidateValue(
        dmGeneralData.CheckIndividual(eObserver),
        ResStr_ObserverNameRequired,
        eObserver);

  if pnlDeterminer.Visible and rbSpecifyDeterminer.Checked then
    ContainerForm.ValidateValue(
        dmGeneralData.CheckIndividual(eDeterminer),
        ResStr_DeterminerNameRequired,
        eDeterminer);
end;  // TfraMissingData.ValidateContent 

end.
