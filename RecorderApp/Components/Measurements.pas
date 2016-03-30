//==============================================================================
//  Unit:        Measurements
//
//  Implements:  TMeasurements
//               TMeasurementGrid
//
//  Description: Composite component dealing with the measurements.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 27/03/2002
//               Changed parameter list of the Init function to accept a
//               database object instead of the database name and session name.
//
//  Last Revision Details:
//    $Revision: 74 $
//    $Date: 4/06/10 15:46 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 1999
//
//==============================================================================

unit Measurements;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, CompositeComponent, Grids, StdCtrls, MeasurementsData,
  DBListCombo, Db, JNCCDatasets, DataClasses, Displayfuncs, Constants, ADODB;

const
  MAX_LENGTH_DATA       = 20;
  MAX_LENGTH_QUALIFIER  = 40;
  MAX_LENGTH_ACCURACY   = 20;

resourcestring
  ResStr_MeasurementMissingOrInvalid =
      'Data missing or invalid in Measurements. Please enter a valid value.';
  ResStr_NoDatabase = 'Connection uninitialised in TMeasurements component.';
  ResStr_ConDeleteMeasurement = 'Are you sure you want to delete this Measurement?';
  ResStr_Measurement = 'Measurement';
  ResStr_Qualifier   = 'Qualifier';
  ResStr_Data        = 'Data';
  ResStr_Unit        = 'Unit';
  ResStr_Accuracy    = 'Accuracy';
  ResStr_Exact       = 'Exact';
  ResStr_Percentage  = '+/- 5%';
  ResStr_Estimate    = 'Estimate';

type
  EMeasurementsError = class(Exception);

  // Override this method to get the comboboxes to move with the columns when
  // they are resized.
  TMeasurementGrid = class(TStringGrid)
  protected
    procedure ColWidthsChanged; override;
  end;

  TMeasurements = class(TCompositeComponent)
  private
    FsgMeasurements     : TMeasurementGrid;
    FbbAdd              : TBitBtn;
    FbbDel              : TBitBtn;
    FcmbMeasureType     : TDBListCombo;
    FcmbMeasureUnit     : TDBListCombo;
    FcmbMeasureQualifier: TDBListCombo;
    FcmbMeasureAccuracy : TComboBox;
    FcmbMeasureData     : TDBListCombo;
    FdmMeasurements     : TdmMeasurements;
    FMeasureList        : TMeasureList;
    FKeyValue           : TKeyString;
    FUserAccessLevel    : TUserAccessLevel;
    FLastCell           : TPoint;
    FSiteID: string;
    FRestrictFullEdit: Boolean;
    FRestrictedData: Boolean;
    procedure SetDataTableName(const Value: string);
    procedure SetKeyValue(const Value: TKeyString);
    procedure RefreshUnitsAndQualifiers(const TypeKey: TKeyString);
    procedure RefreshDataValues(const UnitKey: TKeyString);
    procedure SetControlOntoCell(iControl : TControl; iiCol, iiRow : integer);
    procedure HideComboboxes;
    procedure ApplyEditChanges;
    function InvalidRestrictedData(item: TMeasureItem): Boolean;
    procedure cmbKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmbKeyPress(Sender: TObject; var Key: Char);
    procedure MoveGridCell(const iToTheRight: Boolean);
  protected
    procedure SetEnabled(Value: boolean); override;
    procedure SetupMeasurementsGrid;
    procedure SetupAddButton;
    procedure SetupDeleteButton;
    procedure SetupMeasureTypeComboBox;
    procedure SetupMeasureUnitComboBox;
    procedure SetupMeasureQualiferComboBox;
    procedure SetupDataComboBox;
    procedure SetupAccuracyComboBox;
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure SetupGridCellEditing(iiCol, iiRow: integer; itfCreateIfRequired: boolean);
    procedure cmbMeasureTypeChange(Sender: TObject);
    procedure cmbMeasureUnitChange(Sender: TObject);
    procedure cmbMeasureDataChange(Sender: TObject);
    procedure cmbMeasureQualifierChange(Sender: TObject);
    procedure cmbMeasureAccuracyChange(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbDelClick(Sender: TObject);
    procedure DoExit; override;
    procedure SetEditMode(const Value: TEditMode); override;
    function RowEditable(ARow: Integer = -1): Boolean;
 public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure Init(const ADatabase: TADOConnection;
      const iTableName, iDataTableKeyField, iParentTableKeyField: String;
      const iUserID: TKeyString; const iUAL: TUserAccessLevel; iNextKey: TGetNextKey;
      const ASiteID: string; ARestrictFullEdit: boolean);
    procedure UpdateList;
    function CheckGrid: boolean;
    procedure RefreshLists;
    property Grid: TMeasurementGrid read FsgMeasurements;
    property KeyValue:TKeyString read FKeyValue write SetKeyValue;
  published
    property Enabled write SetEnabled;
    property OnClick;
    property OnEnter;
    property OnExit;
  end;

//==============================================================================
implementation

{$R *.res}

//==============================================================================
// TMeasurementGrid
//------------------------------------------------------------------------------
procedure TMeasurementGrid.ColWidthsChanged;
begin
  inherited;
  if Assigned(OnTopLeftChanged) then OnTopLeftChanged(nil);
end;

//==============================================================================
{ TMeasurements }
//------------------------------------------------------------------------------
constructor TMeasurements.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastCell := Point(0, 0); // last cell edited;
  Width     := 355;
  Height    := 265;
  FdmMeasurements := TdmMeasurements.Create(Self);
  FMeasureList    := nil;
  SetupMeasurementsGrid;
  SetupAddButton;
  SetupDeleteButton;
  SetupMeasureTypeComboBox;
  SetupMeasureUnitComboBox;
  SetupMeasureQualiferComboBox;
  SetupAccuracyComboBox;
  SetupDataComboBox;
  EditMode := emView;
end;  // Create

{===============================================================================
 Description : Destructor.  Note controls are owned by self so automatically
      destroyed
 Created : 06/02/2003 }
destructor TMeasurements.Destroy;
begin
  FMeasureList.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TMeasurements.SetupAddButton;
begin
  FbbAdd := TBitBtn.Create(Self);
  with FbbAdd do
  begin
    Parent := Self;
    SetBounds(Self.Width - 24, 0, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_MADD');
    NumGlyphs := 2;
    OnClick   := bbAddClick;
    Anchors   := [akRight, akTop];
  end;
end;  // SetupAddButton

//==============================================================================
procedure TMeasurements.SetupDeleteButton;
begin
  FbbDel := TBitBtn.Create(Self);
  with FbbDel do
  begin
    Parent := Self;
    SetBounds(Self.Width - 24, 23, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_MDEL');
    NumGlyphs := 2;
    OnClick   := bbDelClick;
    Anchors   := [akRight, akTop];
  end;
end;  // SetupDeleteButton

//==============================================================================
procedure TMeasurements.SetupMeasureTypeComboBox;
begin
  FcmbMeasureType:= TDBListCombo.Create(Self);
  with FcmbMeasureType do begin
    Parent      := Self;
    Visible     := False;
    SetBounds(2, 40, 70, 20);
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    Style       := csDropDownList;
    Datasource  := FdmMeasurements.dsMeasureType;
    KeyField    := 'Measurement_Type_Key';
    ListField   := 'Short_Name';
    OnChange    := cmbMeasureTypeChange;
    OnKeyDown   := cmbKeyDown;
    OnKeyPress  := cmbKeyPress;    
  end;
end;  // SetupMeasureTypeComboBox

//==============================================================================
procedure TMeasurements.SetupMeasureUnitComboBox;
begin
  FcmbMeasureUnit := TDBListCombo.Create(Self);
  with FcmbMeasureUnit do begin
    Parent      := Self;
    Visible     := False;
    SetBounds(230, 40, 50, 20);
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    Style       := csDropDownList;
    DataSource  := FdmMeasurements.dsMeasureUnit;
    KeyField    := 'Measurement_Unit_Key';
    Listfield   := 'Short_Name';
    OnChange    := cmbMeasureUnitChange;
    OnKeyDown   := cmbKeyDown;
    OnKeyPress  := cmbKeyPress;
  end;
end;  // SetupMeasureUnitComboBox

//==============================================================================
procedure TMeasurements.SetupMeasureQualiferComboBox;
begin
  FcmbMeasureQualifier := TDBListCombo.Create(Self);
  with FcmbMeasureQualifier do begin
    Parent      := Self;
    Visible     := False;
    SetBounds(73, 40, 114, 20);
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    Style       := csDropDownList;
    DataSource  := FdmMeasurements.dsMeasureQualifier;
    KeyField    := 'Measurement_Qualifier_Key';
    Listfield   := 'Short_Name';
    OnChange    := cmbMeasureQualifierChange;
    OnKeyDown   := cmbKeyDown;
    OnKeyPress  := cmbKeyPress;    
  end;
end;

procedure TMeasurements.cmbKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key in [#13, #10, #9] then begin// Return or shift return
    MoveGridCell(Key<>#10);
    Key := #0;
  end;
end;  // cmbKeyPress


//==============================================================================
{ MoveGridCell - navigates the current cell to the right or left in the grid.
     If you hit the end, goes to the next/previous row where one is available }
procedure TMeasurements.MoveGridCell(const iToTheRight: Boolean);
begin
  if iToTheRight then begin  // Return
    FsgMeasurements.SetFocus;
    with FsgMeasurements do
      if Col < ColCount - 1 then
        Col := Col + 1
      else
      if Row < RowCount - 1 then begin
        Row := Row + 1;
        Col := 2;
      end;
  end else begin  // Shift-Return
    FsgMeasurements.SetFocus;
    with FsgMeasurements do
      if Col > 0 then
        Col := Col - 1
      else
      if Row > FixedRows then begin // don't move into title row
        Row := Row - 1;
        Col := ColCount-1;
      end;
  end;
end;  // MoveGridCell

//==============================================================================
{ Trap left and right arrow, return and ctrl-return, to provide navigation in
    the combo boxes on the measurements grid }
procedure TMeasurements.cmbKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lCellMove: Integer;
begin
  inherited;
  lCellMove := 0; // No movement by default
  with TComboBox(Sender) do
    case Key of
      VK_RIGHT:
        if (Style = csDropDownList) or        // Cannot edit, so always navigate within grid.
           (SelStart = Length(Text)) or       // Editable, but caret at right limit of text.
           (SelLength = Length(Text)) then    // Editable, all text selected.
          lCellMove := 1; // Cell to the right.
      VK_LEFT:
        if (Style = csDropDownList) or        // Cannot edit, so always navigate within grid.
           (SelStart = 0) or                  // Editable, but caret at left limit of text.
           (SelLength = Length(Text)) then    // Editable, all text selected.
          lCellMove := -1; // Cell to the left.
      VK_DELETE, VK_BACK: begin
        FsgMeasurements.Cells[FsgMeasurements.Col, FsgMeasurements.Row] := ''; // blank the cell
        if (Style = csDropDownList) then
          ItemIndex := -1; // blank the combo by clearing selection
      end;
    end;
  if lCellMove <> 0 then begin
    MoveGridCell(lCellMove = 1);
    Key := 0; // no further action on this keypress
  end; // if
end;  // cmbKeyDown

//==============================================================================
procedure TMeasurements.SetupAccuracyComboBox;
begin
  FcmbMeasureAccuracy := TCombobox.Create(Self);
  with FcmbMeasureAccuracy do begin
    Parent      := Self;
    Visible     := False;
    SetBounds(281, 40, 49, 20);
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    MaxLength   := MAX_LENGTH_ACCURACY;
    OnChange    := cmbMeasureAccuracyChange;
  end;
end;  // SetupAccuracyComboBox

//==============================================================================
procedure TMeasurements.SetupDataComboBox;
begin
  FcmbMeasureData := TDBListCombo.Create(Self);
  with FcmbMeasureData do begin
    Parent      := Self;
    Visible     := False;
    Style       := csDropDownList;
    SetBounds(189, 40, 40, 20);
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    DataSource  := FdmMeasurements.dsMeasureData;
    KeyField    := 'Data';
    Listfield   := 'Data';
    OnChange    := cmbMeasureDataChange;
  end;
end;

//==============================================================================
procedure TMeasurements.SetupMeasurementsGrid;
begin
  FsgMeasurements := TMeasurementGrid.Create(Self); //TStringGrid.Create(Self);
  with FsgMeasurements do
  begin
    Parent           := Self;
    Align            := alLeft;
    Width            := Self.Width - 23;
    FixedRows        :=   1;
    FixedCols        :=   0;
    ColCount         :=   5;
    RowCount         :=   2;
    DefaultRowHeight :=  19;
    ColWidths[0]     :=  70;
    ColWidths[1]     := 115;
    ColWidths[2]     :=  40;
    ColWidths[3]     :=  50;
    ColWidths[4]     :=  49;
    OnDrawCell       := GridDrawCell;
    OnKeyPress       := GridKeyPress;
    OnSelectCell     := GridSelectCell;
    OnTopLeftChanged := GridTopLeftChanged;
    Options          := [goFixedVertLine, goFixedHorzLine, goVertLine,
                         goHorzLine, goColSizing];
    Cells[0, 0]      := ResStr_Measurement;
    Cells[1, 0]      := ResStr_Qualifier;
    Cells[2, 0]      := ResStr_Data;
    Cells[3, 0]      := ResStr_Unit;
    Cells[4, 0]      := ResStr_Accuracy;
    Anchors          := [akRight, akTop, akBottom, akLeft];
  end;
end;  // SetupMeasurementsGrid

//==============================================================================
// itfCreateIfRequired - if in edit mode and you click on empty line in grid,
//                       it will Create a measurement
procedure TMeasurements.SetupGridCellEditing(iiCol, iiRow: integer;
  itfCreateIfRequired: boolean);
var lDataItem:TMeasureItem;
    lPrevKey :TKeyString;
begin
  if (EditMode = emView) or (FMeasureList.ItemCount = 0) then
  begin
    FbbDel.Enabled := false;
    HideComboboxes;
  end else begin
    if (FMeasureList.ItemCount=0) and itfCreateIfRequired then
      // Check Item exists in list, create one if needed
      lDataItem:=FMeasureList.CheckItem(FsgMeasurements.Rows[iiRow].Objects[0])
    else
      lDataItem:=TMeasureItem(FsgMeasurements.Rows[iiRow].Objects[0]);

    if Assigned(lDataItem) then
    begin
      RefreshDataValues(lDataItem.MeasureUnitKey);
      if FRestrictedData then FcmbMeasureData.KeyValue := lDataItem.Data;

      FbbDel.Enabled := (EditMode = emAdd) or
         ((EditMode = emEdit) and (FUserAccessLevel > ualAddOnly)
         and ((lDataItem.EnteredBy=FMeasureList.UserID) or (not FRestrictFullEdit))
         and ((lDataItem.Custodian = '') or (lDataItem.Custodian = FSiteID)));

      if FbbDel.Enabled or (lDataItem.ItemKey = '') then
      begin
        if iiCol in [0, 1, 3] then
          FsgMeasurements.Options := FsgMeasurements.Options - [goEditing]
        else
          FsgMeasurements.Options := FsgMeasurements.Options + [goEditing];

        lPrevKey :=FcmbMeasureType.KeyValue;
        // If no Type selected, setup a new one.
        // This could happen if new item and unit column clicked w/o any type already selected
        if lDataItem.MeasureTypeKey = '' then begin
          FcmbMeasureType.ItemIndex:= 0;
          lDataItem.MeasureType    := FcmbMeasureType.Text;
          lDataItem.MeasureTypeKey := FcmbMeasureType.KeyValue;
        end else
          FcmbMeasureType.KeyValue := lDataItem.MeasureTypeKey;

        // If new node, the unit is not set.
        if lDataItem.MeasureUnitKey = '' then begin
          RefreshUnitsAndQualifiers(lDataItem.MeasureTypeKey);
          lDataItem.MeasureUnit   := FcmbMeasureUnit.Text;
          lDataItem.MeasureUnitKey:= FcmbMeasureUnit.KeyValue;
          lDataItem.Qualifier     := FcmbMeasureQualifier.Text;
          lDataItem.QualifierKey  := FcmbMeasureQualifier.KeyValue;
          RefreshDataValues(FcmbMeasureUnit.KeyValue);
          if FRestrictedData then lDataItem.Data := FcmbMeasureData.Text;
        end else begin
        // else, check if we need to refresh the unit list
          if lPrevKey<>FcmbMeasureType.KeyValue then
            RefreshUnitsAndQualifiers(lDataItem.MeasureTypeKey);
          FcmbMeasureUnit.KeyValue     := lDataItem.MeasureUnitKey;
          FcmbMeasureQualifier.KeyValue:= lDataItem.QualifierKey;
          RefreshDataValues(FcmbMeasureUnit.KeyValue);
          if FRestrictedData then FcmbMeasureData.KeyValue := lDataItem.Data;
        end;
      end else
        FsgMeasurements.Options:=FsgMeasurements.Options - [goEditing];
    end;
  end;
end;  // SetupGridCellEditing

//==============================================================================
procedure TMeasurements.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with FsgMeasurements do begin
    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2);
  end;
end;  // GridDrawCell

//==============================================================================
procedure TMeasurements.HideComboboxes;
begin
  FcmbMeasureType.Visible      := false;
  FcmbMeasureQualifier.Visible := false;
  FcmbMeasureData.Visible      := False;
  FcmbMeasureUnit.Visible      := false;
  FcmbMeasureAccuracy.Visible  := false;
end;

//==============================================================================
{ Description : Display combo boxes when cells are selected
  Created : 15/11/2002 }
procedure TMeasurements.GridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
var cmbTemp  :TComboBox;
    lDataItem:TMeasureItem;
    tfShowComboBox:Boolean;
begin
  if goEditing in FsgMeasurements.Options then
    ApplyEditChanges;
  SetupGridCellEditing(ACol, ARow, True);
  if (EditMode <> emView) and (FMeasureList.ItemCount > 0) then
  begin
    if ARow > 0 then begin
      lDataItem := TMeasureItem(FsgMeasurements.Rows[ARow].Objects[0]);
      tfShowComboBox := FbbDel.Enabled or (lDataItem.ItemKey = '');
    end else
      tfShowComboBox := false;

    if tfShowComboBox and RowEditable(ARow) and
       ((ACol in [0, 1, 3, 4]) or ((ACol = 2) and FRestrictedData)) then
    begin
      cmbTemp := nil;
      case ACol of
        0 : cmbTemp := FcmbMeasureType;
        1 : cmbTemp := FcmbMeasureQualifier;
        2 : cmbTemp := FcmbMeasureData;
        3 : cmbTemp := FcmbMeasureUnit;
        4 : cmbTemp := FcmbMeasureAccuracy;
      end;
      SetControlOntoCell(cmbTemp, ACol, ARow);
      if ACol = 4 then
        cmbTemp.Text := FsgMeasurements.Cells[ACol, ARow]
      else begin
        if ACol = 3 then  // Refresh units
          RefreshUnitsAndQualifiers(FcmbMeasureType.KeyValue);
        if ACol = 2 then
          RefreshDataValues(FcmbMeasureUnit.KeyValue);
        cmbTemp.ItemIndex := cmbTemp.Items.IndexOf(FsgMeasurements.Cells[ACol, ARow]);
      end;
      cmbTemp.Visible := True;
      if cmbTemp.CanFocus then cmbTemp.SetFocus;
    end else
      HideComboBoxes;

    // Hide the unwanted combo boxes
    if (ACol <> 0) or (not tfShowComboBox) then FcmbMeasureType.Visible     := False;
    if (ACol <> 1) or (not tfShowComboBox) then FcmbMeasureQualifier.Visible:= False;
    if (ACol <> 2) or (not tfShowComboBox) then FcmbMeasureData.Visible     := False;
    if (ACol <> 3) or (not tfShowComboBox) then FcmbMeasureUnit.Visible     := False;
    if (ACol <> 4) or (not tfShowComboBox) then FcmbMeasureAccuracy.Visible := False;
  end;
  FLastCell := Point(ACol, ARow);
end;  // GridSelectCell

//==============================================================================
procedure TMeasurements.ApplyEditChanges;
var
  lDataItem : TMeasureItem;
begin
  if FLastCell.Y > 0 then
    lDataItem := TMeasureItem(FsgMeasurements.Rows[FLastCell.Y].Objects[0])
  else
    lDataItem := nil;

  if Assigned(lDataItem) then
  begin
    // Update the item properties with new values in the grid.
    if (FLastCell.Y > 0) and Assigned(lDataItem) then begin
      if FsgMeasurements.Cells[2, FLastCell.Y] <> lDataItem.Data then
        lDataItem.Data     := FsgMeasurements.Cells[2, FLastCell.Y];
      if FsgMeasurements.Cells[4, FLastCell.Y] <> lDataItem.Accuracy then
        lDataItem.Accuracy := FsgMeasurements.Cells[4, FLastCell.Y];
    end;
  end;
end;

//==============================================================================
{ Description : Moves the supplied control so it overlays a given cell
  Created : 15/11/2002 }
procedure TMeasurements.SetControlOntoCell(iControl : TControl; iiCol, iiRow : integer);
var
  lRect : TRect; // position of cell
begin
  lRect := FsgMeasurements.CellRect(iiCol, iiRow);
  iControl.Left   := lRect.Left + 1;
  iControl.Top    := lRect.Top + 1;
  iControl.Width  := lRect.Right - lRect.Left + 2;
  iControl.Height := lRect.Bottom - lRect.Top - 2;
end;

//==============================================================================
// Needed to handle Data item value change properly.
procedure TMeasurements.GridKeyPress(Sender: TObject; var Key: Char);
begin
  with FsgMeasurements do
    // Cancel changes, reset value in grid
    if (Key = #27) or (not RowEditable) then begin
      Cells[2, Row] := TMeasureItem(Rows[Row].Objects[0]).Data;
      EditorMode    := False;  // Hide editor
      Options       := Options - [goEditing];
      GridTopLeftChanged(nil);
    end else
    // Save value in measurement object
    if Key = #13 then begin
      if Col = 2 then
        TMeasureItem(Rows[Row].Objects[0]).Data := Cells[2, Row];
      GridTopLeftChanged(nil);
    end else
    // Setup text limit for Data column.
    if Col = 2 then
      if Assigned(InplaceEditor) then begin
        if SendMessage(InplaceEditor.Handle, EM_GETLIMITTEXT, 0, 0) <> MAX_LENGTH_DATA then
          SendMessage(InplaceEditor.Handle, EM_LIMITTEXT, MAX_LENGTH_DATA, 0);
      end;
end;  // GridKeyPress

//==============================================================================
// Move and resize comboboxes with the grid.
procedure TMeasurements.GridTopLeftChanged(Sender: TObject);
var lDummy : boolean; // this is an unused var parameter
begin
  inherited;
  GridSelectCell(nil, FsgMeasurements.Col, FsgMeasurements.Row, lDummy);
end;  // GridTopLeftChanged

//==============================================================================
procedure TMeasurements.cmbMeasureTypeChange(Sender:TObject);
begin
  inherited;
  if (FMeasureList <> nil) and (FMeasureList.ItemCount > 0) then
    with TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]) do
      if MeasureType <> FcmbMeasureType.Text then begin
        MeasureType    := FcmbMeasureType.Text;
        MeasureTypeKey := FcmbMeasureType.KeyValue;

        // Reset Units and Qualifiers to match the selected Type
        RefreshUnitsAndQualifiers(MeasureTypeKey);
        MeasureUnit    := FcmbMeasureUnit.Text;
        MeasureUnitKey := FcmbMeasureUnit.KeyValue;
        Qualifier      := FcmbMeasureQualifier.Text;
        QualifierKey   := FcmbMeasureQualifier.KeyValue;
        RefreshDataValues(FcmbMeasureUnit.KeyValue);
        if FRestrictedData then Data := FcmbMeasureData.Text;
      end;
end;  // cmbMeasureTypeChange

//==============================================================================
procedure TMeasurements.RefreshUnitsAndQualifiers(const TypeKey: TKeyString);
begin
  if FcmbMeasureType.Active then begin
    // Refresh Units
    with FdmMeasurements.qryMeasureUnit do begin
      FcmbMeasureUnit.Active := False;
      Parameters.ParamByName('KeyParameter').Value := TypeKey;
      FcmbMeasureUnit.Active := True;
    end;
    FcmbMeasureUnit.ItemIndex:=0;
    // Refresh Qualifiers
    with FdmMeasurements.qryMeasureQualifier do begin
      FcmbMeasureQualifier.Active := False;
      Parameters.ParamByName('KeyParameter').Value := TypeKey;
      FcmbMeasureQualifier.Active := True;
    end;
    FcmbMeasureQualifier.ItemIndex:=0;
  end;
end;  // RefreshUnitsAndQualifiers

//==============================================================================
procedure TMeasurements.RefreshDataValues(const UnitKey: TKeyString);
begin
  // Refresh Data values, if any
  with FdmMeasurements.qryMeasureData do begin
    FcmbMeasureData.Active := False;
    Parameters.ParamByName('KeyParameter').Value := UnitKey;
    FcmbMeasureData.Active := True;
    FRestrictedData := FcmbMeasureData.Items.Count > 0;
    if FRestrictedData then FcmbMeasureData.ItemIndex := 0;
  end;
end;  // RefreshDataValues

//==============================================================================
procedure TMeasurements.cmbMeasureUnitChange(Sender: TObject);
begin
  inherited;
  if (FMeasureList<>nil) and (FMeasureList.ItemCount>0) then
    with TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]) do begin
      MeasureUnit   :=FcmbMeasureUnit.Text;
      MeasureUnitKey:=FcmbMeasureUnit.KeyValue;
      RefreshDataValues(FcmbMeasureUnit.KeyValue);
      if FRestrictedData then 
        if FcmbMeasureData.Items.IndexOf(Data) = -1 then
           Data := FcmbMeasureData.Text
        else
          FcmbMeasureData.keyValue := Data;
    end;
end;  // cmbMeasureUnitChange

//==============================================================================
procedure TMeasurements.cmbMeasureQualifierChange(Sender: TObject);
begin
  inherited;
  if (FMeasureList<>nil) and (FMeasureList.ItemCount>0) then
    with TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]) do begin
      Qualifier   :=FcmbMeasureQualifier.Text;
      QualifierKey:=FcmbMeasureQualifier.KeyValue;
    end;
end;  // cmbMeasureQualifierChange

//==============================================================================
procedure TMeasurements.cmbMeasureDataChange(Sender: TObject);
begin
  inherited;
  if (FMeasureList <> nil) and (FMeasureList.ItemCount > 0) then
    with TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]) do
      Data := FcmbMeasureData.Text;
end;  // cmbMeasureDataChange

//==============================================================================
procedure TMeasurements.cmbMeasureAccuracyChange(Sender: TObject);
begin
  inherited;
  if (FMeasureList<>nil) and (FMeasureList.ItemCount>0) then
    with TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]) do
      Accuracy:=FcmbMeasureAccuracy.Text;
end;  // cmbMeasureAccuracyChange

//==============================================================================
procedure TMeasurements.bbAddClick(Sender: TObject);
var lDataItem:TMeasureItem;
  canSelect: boolean;
begin
  lDataItem := TMeasureItem.CreateNew(FMeasureList);
  FcmbMeasureType.ItemIndex := 0;
  with lDataItem do begin
    MeasureType    := FcmbMeasureType.Text;
    MeasureTypeKey := FcmbMeasureType.KeyValue;
    RefreshUnitsAndQualifiers(MeasureTypeKey);  // Refresh list and set ItemIndex to 0
    MeasureUnit    := FcmbMeasureUnit.Text;
    MeasureUnitKey := FcmbMeasureUnit.KeyValue;
    Qualifier      := FcmbMeasureQualifier.Text;
    QualifierKey   := FcmbMeasureQualifier.KeyValue;
    RefreshDataValues(FcmbMeasureUnit.KeyValue);
    if FRestrictedData then Data := FcmbMeasureData.Text;
  end;
  FMeasureList.AddNew(lDataItem);
  FsgMeasurements.Row := FsgMeasurements.RowCount - 1;
  GridSelectCell(FsgMeasurements, FsgMeasurements.Col, FsgMeasurements.Row, canSelect);
  FbbDel.Enabled := true;
end;  // bbAddClick

//==============================================================================
procedure TMeasurements.bbDelClick(Sender: TObject);
var Wallaby: Boolean;
begin
  if MessageDlg(ResStr_ConDeleteMeasurement,mtConfirmation, [mbNo, mbYes], 0)=mrYes then begin
    FMeasureList.DeleteItem(FsgMeasurements.Row);

    FsgMeasurements.Options := FsgMeasurements.Options - [goEditing];
    GridSelectCell(nil, FsgMeasurements.Col, FsgMeasurements.Row, Wallaby);
  end;
end;  // bbDelClick

//==============================================================================
procedure TMeasurements.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  FsgMeasurements.Enabled := Value;
  FbbAdd.Enabled := Value and (EditMode <> emView);
  FbbDel.Enabled := Value and (EditMode <> emView);
end;  // SetEnabled

//==============================================================================
procedure TMeasurements.SetEditMode(const Value: TEditMode);
var lDataItem:TMeasureItem;
begin
  inherited SetEditMode(Value);
  with FsgMeasurements do
    if (Value<>emView) then Options:=Options - [goRowSelect]
                       else Options:=Options + [goRowSelect];

  if FMeasureList <> nil then begin
    lDataItem := nil;
    // Make sure there is an object to play with
    if FMeasureList.ItemCount > 0 then
      lDataItem := TMeasureItem(FsgMeasurements.Rows[FsgMeasurements.Row].Objects[0]);
    if (Value <> emView) then begin
      // Edit mode on, activate the comboboxes
      FcmbMeasureType.Active := true;
      if lDataItem <> nil then begin
        FcmbMeasureType.KeyValue := lDataItem.MeasureTypeKey;
        RefreshUnitsAndQualifiers(lDataItem.MeasureTypeKey);
        RefreshDataValues(lDataItem.MeasureUnitKey);
      end;
    end else begin
      // Deactivate the comboboxes
      FcmbMeasureType.Active     := false;
      FcmbMeasureUnit.Active     := false;
      FcmbMeasureQualifier.Active:= false;
      FcmbMeasureData.Active     := False;
    end;
  end;
  // Set buttons state
  FbbAdd.Enabled := Value <> emView;
  SetupGridCellEditing(FsgMeasurements.Col, FsgMeasurements.Row, False);
end;  // SetEditMode

//==============================================================================
procedure TMeasurements.DoExit;
begin
  FcmbMeasureType.Visible     := false;
  FcmbMeasureUnit.Visible     := false;
  FcmbMeasureData.Visible     := False;
  FcmbMeasureQualifier.Visible:= false;
  FcmbMeasureAccuracy.Visible := false;
  inherited DoExit;
end;  // DoExit

//==============================================================================
procedure TMeasurements.SetDataTableName(const Value: string);
begin
  // Set the SQL text with the new table name
  if FdmMeasurements <> nil then begin
    // This query returns a read-only set of records
    with FdmMeasurements.qryMeasure do begin
      SQL[ 0] := 'SELECT DT.' + Value + '_Key AS Data_Key, ';
      SQL[10] := '  INNER JOIN ' + Value + ' AS DT ';
      SQL[13] := 'WHERE DT.' + Copy(Value, 1, Pos('_data', LowerCase(Value))) + 'Key ';
      Parameters.ParamByName('KeyParameter').Value := '';
    end;
    // Prepare the SQL for the update query. This one is an updatable SELECT
    with FdmMeasurements.qryUpdate do begin
      SQL.Text := 'SELECT * FROM ' + Value
          + ' WHERE ' + Copy(Value, 1, Pos('_data', LowerCase(Value)))
          + 'Key = :KeyParameter';
    end;
    // Filter the Measurement Types according to screen used
    with FdmMeasurements.qryMeasureType do begin
      Parameters.ParamByName('DataTableName').Value := Value;
    end;
  end;
end;  // SetDataTableName

//==============================================================================
procedure TMeasurements.Refresh;
begin
  FMeasureList.Refresh;
end;  // Refresh

//==============================================================================
procedure TMeasurements.SetKeyValue(const Value: TKeyString);
begin
  FKeyValue := Value;
  FdmMeasurements.qryMeasure.Parameters.ParamByName('KeyParameter').Value := Value;
  // Keep in sync
  FdmMeasurements.qryUpdate.Parameters.ParamByName('KeyParameter').Value := Value;
end;  // SetKeyValue

//==============================================================================
procedure TMeasurements.Init(const ADatabase: TADOConnection;
  const iTableName, iDataTableKeyField, iParentTableKeyField: String;
  const iUserID: TKeyString; const iUAL: TUserAccessLevel; iNextKey: TGetNextKey;
  const ASiteID: String; ARestrictFullEdit: boolean);
begin
  // Initialize Measurements properties in one go.
  if ADatabase  = nil then
    raise EMeasurementsError.Create(ResStr_NoDatabase);

  FUserAccessLevel := iUAL;
  FdmMeasurements.UpdateDatasets(ADatabase);
  FSiteID := ASiteID;
  FRestrictFullEdit := ARestrictFullEdit;
  // Set DataTableName
  SetDataTableName(iTableName);

  // Set MeasureList
  FMeasureList:=TMeasureList.Create(FdmMeasurements.qryMeasure,'Data_Key',
                                    FsgMeasurements, TMeasureItem, FdmMeasurements);
  FMeasureList.KeyValue           := KeyValue;
  FMeasureList.DataTableName      := iTableName;
  FMeasureList.DataTableKeyField  := iDataTableKeyField;
  FMeasureList.ParentTableKeyField:= iParentTableKeyField;
  FMeasureList.UserID             := iUserID;
  FMeasureList.NextKey            := iNextKey;

  FcmbMeasureAccuracy.Items.Add(ResStr_Exact);
  FcmbMeasureAccuracy.Items.Add(ResStr_Percentage);
  FcmbMeasureAccuracy.Items.Add(ResStr_Estimate);
end;  // Init

//==============================================================================
procedure TMeasurements.UpdateList;
begin
  // Save to database
  FMeasureList.KeyValue:=KeyValue;
  FMeasureList.Update;
end;  // UpdateList

//==============================================================================
function TMeasurements.CheckGrid: Boolean;
var
  i   : Integer;
  item: TMeasureItem;
  dummy: Boolean;     // throw away var value
begin
  // force any current changes into the data item first
  GridSelectCell(FsgMeasurements, 0, 0, dummy);
  Result := True;
  if FMeasureList.ItemCount > 0 then  // If there are some measurements around...
    for i := 1 to FsgMeasurements.RowCount - 1 do begin
      item := TMeasureItem(FsgMeasurements.Objects[0, i]);
      with item do
        if (MeasureType = '') or (MeasureUnit = '') or (Qualifier = '') or (Data = '') or
           InvalidRestrictedData(item) then
        begin
          FsgMeasurements.Row := i;
          FsgMeasurements.Col := 0;
          Result := False;
          Exit;
        end;
    end;
end;  // CheckGrid

//==============================================================================
function TMeasurements.InvalidRestrictedData(item: TMeasureItem): Boolean;
begin
  RefreshDataValues(item.MeasureUnitKey);
  Result := FRestrictedData and (FcmbMeasureData.Items.IndexOf(item.Data) = -1);
end;  // InvalidRestrictedData

//==============================================================================
procedure TMeasurements.RefreshLists;
begin
  // Refresh only if in edit mode
  if EditMode<>emView then begin
    FcmbMeasureType.Active     := false;
    FcmbMeasureUnit.Active     := false;
    FcmbMeasureQualifier.Active:= false;
    FcmbMeasureData.Active     := False;
    // Reset controls and refresh the combo boxes too.
    EditMode:=EditMode;
  end;
end;  // RefreshLists

{-------------------------------------------------------------------------------
  Check if a row is editable.  If row not supplied, use current row.
}
function TMeasurements.RowEditable(ARow: Integer = -1): Boolean;
var
  lDataItem: TMeasureItem;
  lRow: integer;
begin
  Result := True;
  if ARow = -1 then
    lRow := FsgMeasurements.Row
  else
    lRow := ARow;

  lDataItem := FMeasureList.CheckItem(FsgMeasurements.Rows[lRow].Objects[0]);
  if Assigned(lDataItem) then
    Result := (FSiteID = lDataItem.Custodian) or (lDataItem.Custodian = '');

  if Result then
    Result := (FUserAccessLevel >= ualAddOnly) and
        ((lDataItem.EnteredBy = FMeasureList.UserID) or (not FRestrictFullEdit)
         or (lDataItem.EnteredBy = ''));
end;

end.
