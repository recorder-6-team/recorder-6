{===============================================================================
  Unit:        SpatialRef

  Defines:     TSpatialRef

  Description:

  Model:

  Created:

  Last revision information:
    $Revision: 64 $
    $Date: 7.04.10 14:32 $
    $Author: Andrewkemp $

===============================================================================}

unit SpatialRef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, IDComboBox, CompositeComponent, SpatialRefFuncs,
  Registry, Constants, ImgList, ImageListButton, Menus;
  
resourcestring
  ResStr_FindSpatialRef = 'Find spatial reference from the map';

type
  // Used to send a set of spatial ref properties to results of functions
  TSpatialRefValues = record
    DisplayRef: String;
    DisplaySystem: String;
    EnteredRef: String;
    EnteredSystem: String;
    Qualifier: String;
  end;

  TInvalidSpatialRefEvent = procedure(Sender: TObject; var Handled: Boolean) of object;

  TSpatialRef = class (TCompositeComponent)
  private
    FBevel: TBevel;
    FDisplayRef: String;
    FDisplaySystem: String;
    FDragDropShape: TShape;
    FDropDownButton: TButton;
    FDropDownMenu: TPopupMenu;
    FEnteredRef: String;
    FEnteredSystem: String;
    FGetButton: TImageListButton;
    FListAdded: Boolean;
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FOnFind: TNotifyEvent;
    FOnInvalidSpatialRef: TInvalidSpatialRefEvent;
    FQualifierCombo: TIDComboBox;
    FSimpleMode: Boolean;
    FSpatialRefEdit: TEdit;
    function CanUseMaps: Boolean;
    procedure DataChange(Sender: TObject);
    procedure DropDownClick(Sender: TObject);
    function GetColor: TColor;
    function GetDisplayRef: String;
    function GetDisplaySystem: String;
    function GetEnteredRef: String;
    function GetEnteredSystem: String;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetOnGetFromMap: TNotifyEvent;
    function GetQualifier: String;
    function GetSpatialRef: String;
    function GetValues: TSpatialRefValues;
    procedure SetColor(Value: TColor);
    procedure SetDisplayRef(const Value: String);
    procedure SetDisplaySystem(const Value: String);
    procedure SetEnteredRef(const Value: String);
    procedure SetEnteredSystem(const Value: String);
    procedure SetFont(const Value: TFont);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetOnFind(const Value: TNotifyEvent);
    procedure SetOnGetFromMap(const Value: TNotifyEvent);
    procedure SetQualifier(const Value: String);
    procedure SetSimpleMode(const Value: Boolean);
    procedure SetSpatialRef(const Value: String);
    procedure SetupButtons;
    procedure SetupCombo;
    procedure SetupControl;
    procedure SetupEdit;
    procedure SetupShape;
    procedure SetValues(const Value: TSpatialRefValues);
    procedure SpatialRefEditKeyPressed(Sender: TObject; var Key: Char);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure PrepareSRQualifiers;
    procedure SetupQualifierCombo;
  protected
    procedure DoCtl3DChanged; override;
    procedure SetEditMode(const Value: TEditMode); override;
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure UpdateMapWindowSelector;
    procedure DoExit; override;
    property ControlQualifier: TIDComboBox read FQualifierCombo;
    property ControlSpatialRef: TEdit read FSpatialRefEdit;
    property DisplayRef: String read GetDisplayRef write SetDisplayRef;
    property DisplaySystem: String read GetDisplaySystem write SetDisplaySystem;
    property EnteredRef: String read GetEnteredRef write SetEnteredRef;
    property EnteredSystem: String read GetEnteredSystem write SetEnteredSystem;
    property Modified: Boolean read FModified write FModified;
    property Qualifier: String read GetQualifier write SetQualifier;
    property Values: TSpatialRefValues read GetValues write SetValues;
  published
    property Color read GetColor write SetColor default clWindow;
    property DestCol default clRed;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property EditMode default emEdit;
    property Font write SetFont;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnFind: TNotifyEvent read FOnFind write SetOnFind;
    property OnGetFromMap: TNotifyEvent read GetOnGetFromMap write SetOnGetFromMap;
    property OnInvalidSpatialRef: TInvalidSpatialRefEvent read FOnInvalidSpatialRef write FOnInvalidSpatialRef;
    property SimpleMode: Boolean read FSimpleMode write SetSimpleMode default false;
    property SourceCol default clBlue;
    property SpatialRef: String read GetSpatialRef write SetSpatialRef;
  end;
  
//==============================================================================
implementation

resourcestring
  ResStr_Qualifier  = 'Qualifier';
  ResStr_SpatialRef = 'Spatial reference';

  //These are the options of Spatial Reference Qualifier. Any addition to this list in '||' adds that option
  ResStr_SRQualifiers = '|Internal Map| |GPS| |Original Recorder| |Estimated from Map| |Site Centroid| |From GIS|';

var
  slSRQualifiers : TStringList;

{-==============================================================================
    TSpatialRef
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpatialRef.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrepareSRQualifiers; //Sets up slSRQualifiers to feed into FQualifierCombo

  // All controls are parented to the main one, so don't need to free each individually.
  FSimpleMode := False;
  SetupControl;
  SetupEdit;
  SetupShape;
  SetupButtons;
  SetupCombo;
  FListAdded := False;
  
  // Set the defaults.
  DestCol    := clRed;
  SourceCol  := clBlue;
  EditMode   := emEdit;
  ImageIndex := -1;
end;  // TSpatialRef.Create

{-------------------------------------------------------------------------------
  Determine whether the buttons for the maps can be enabled or not. If the dropdown
  menu is empty, there are no maps to choose from, therefore the buttons should be
  disabled.
}
function TSpatialRef.CanUseMaps: Boolean;
begin
  Result := False;
  if Assigned(FDropDownMenu) then Result := FDropDownMenu.Items.Count > 0;
end;  // TSpatialRef.CanUseMaps: Boolean

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.Clear;
begin
  FSpatialRefEdit.Text      := '';
  FQualifierCombo.ItemIndex := -1;
  Modified                  := True;
end;  // TSpatialRef.Clear

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DataChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;  // TSpatialRef.DataChange

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DoExit;
var
  lValidSpatialRef: TValidSpatialRef;
  lHandled: Boolean;
begin
  if FSpatialRefEdit.Modified then begin
    Modified := True;
    lValidSpatialRef := ValidSpatialRef(SpatialRef, DisplaySystem);
    if lValidSpatialRef.Valid then begin
      SetDisplayRef(lValidSpatialRef.FormattedSR);
      SetEnteredRef(lValidSpatialRef.EnteredFormattedSR);
      SetEnteredSystem(DetermineSpatialRefSystem(EnteredRef));
    end else begin
      lHandled := False;
      // Give opportunity to intercept and handle the problem.
      if Assigned(FOnInvalidSpatialRef) then
        FOnInvalidSpatialRef(Self, lHandled);
      // If the error has been handled, don't raise, otherwise do.
      if not lHandled then
        raise ESpatialRefError.CreateValidation(lValidSpatialRef.Error, FSpatialRefEdit);
    end;
  end; // if modified

  inherited;
end;  // TSpatialRef.DoExit

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DropDownClick(Sender: TObject);
var
  lPos: TPoint;
begin
  if Assigned(FDropDownMenu) then begin
    lPos := FGetButton.ClientToScreen(Point(0, FGetButton.Height));
    FDropDownMenu.Popup(lPos.X, lPos.Y);
  end;
end;  // TSpatialRef.DropDownClick

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetColor: TColor;
begin
  Result := FSpatialRefEdit.Color;
end;  // TSpatialRef.GetColor

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetDisplayRef: String;
begin
  Result := FDisplayRef;
end;  // TSpatialRef.GetDisplayRef 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetDisplaySystem: String;
begin
  if FDisplaySystem = '' then
    Result := DetermineSpatialRefSystem(SpatialRef)
  else
    Result := FDisplaySystem;
end;  // TSpatialRef.GetDisplaySystem 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetEnteredRef: String;
begin
  // The checks are necessary because FEnteredRef is updated only when focus
  // leaves the component, not just the Edit box. And if the Spatial Ref is
  // cleared after FEnteredRef has been set, FEnteredRef might not reflect that
  // when this function is called, as the focus might still be on the Edit Box.
  if (FEnteredRef<>'') and (FSpatialRefEdit.Text<>'') then
    Result := FEnteredRef
  else
    Result := SpatialRef;
end;  // TSpatialRef.GetEnteredRef 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetEnteredSystem: String;
begin
  if FEnteredSystem = '' then
    Result := DetermineSpatialRefSystem(SpatialRef)
  else
    Result := FEnteredSystem;
end;  // TSpatialRef.GetEnteredSystem 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetImageIndex: Integer;
begin
  Result := FGetButton.ImageIndex;
end;  // TSpatialRef.GetImageIndex

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetImageList: TCustomImageList;
begin
  Result := FGetButton.ImageList;
end;  // TSpatialRef.GetImageList

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetOnGetFromMap: TNotifyEvent;
begin
  Result := FGetButton.OnClick;
end;  // TSpatialRef.GetOnGetFromMap 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetQualifier: String;
begin
  Result := FQualifierCombo.Text;
end;  // TSpatialRef.GetQualifier 

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetSpatialRef: String;
begin
  Result := DelocaliseSpatialRef(FSpatialRefEdit.Text);
end;  // TSpatialRef.GetSpatialRef

{-------------------------------------------------------------------------------
}
function TSpatialRef.GetValues: TSpatialRefValues;
begin
  Result.EnteredRef    := GetEnteredRef;
  Result.EnteredSystem := GetEnteredSystem;
  Result.Qualifier     := GetQualifier;
  Result.DisplayRef    := GetDisplayRef;
  Result.DisplaySystem := GetDisplaySystem;
end;  // TSpatialRef.GetValues

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetColor(Value: TColor);
begin
  FSpatialRefEdit.Color := Value;
  FQualifierCombo.Color := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetDisplayRef(const Value: String);
begin
  FDisplayRef          := Value;
  FSpatialRefEdit.Text := LocaliseSpatialRef(FDisplayRef);
  Modified             := True;
end;  // TSpatialRef.SetDisplayRef

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetDisplaySystem(const Value: String);
begin
  FDisplaySystem := Value;
  if EnteredRef <> '' then begin
    if EnteredSystem = DisplaySystem then
      DisplayRef := EnteredRef
    else
      DisplayRef := ConvertSystems(EnteredRef, EnteredSystem, DisplaySystem);
  end;
end;  // TSpatialRef.SetDisplaySystem

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.DoCtl3DChanged;
begin
  inherited;
  FSpatialRefEdit.Ctl3D := Ctl3D;
  FQualifierCombo.Ctl3D := Ctl3D;
  if Ctl3D then begin
    FSpatialRefEdit.BorderStyle := bsSingle;
    FQualifierCombo.BevelKind := bkNone;
  end else begin
    FSpatialRefEdit.BorderStyle := bsNone;
    FQualifierCombo.BevelKind := bkFlat;
  end;
  RecreateWnd;
end;  // TSpatialRef.DoCtl3DChanged;

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetEditMode(const Value: TEditMode);
begin
  inherited SetEditMode(Value);
  FSpatialRefEdit.ReadOnly := Value = emView;
  FGetButton.Enabled       := (Value <> emView) and CanUseMaps;
  FDropDownButton.Enabled  := FGetButton.Enabled and CanUseMaps;
  FQualifierCombo.Enabled  := Value <> emView;
end;  // TSpatialRef.SetEditMode

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetEnteredRef(const Value: String);
begin
  FEnteredRef := Value;
  Modified    := True;
  if DisplayRef = '' then DisplayRef := FEnteredRef;
end;  // TSpatialRef.SetEnteredRef

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetEnteredSystem(const Value: String);
begin
  FEnteredSystem := Value;
  Modified := True;
end;  // TSpatialRef.SetEnteredSystem 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetFont(const Value: TFont);
begin
  inherited Font := Value;
  FSpatialRefEdit.Font.Assign(Value);
  FQualifierCombo.Font.Assign(Value);
  SendMessage(Self.Handle, WM_Size, 0, 0);
end;  // TSpatialRef.SetFont

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetImageIndex(const Value: Integer);
begin
  FGetButton.ImageIndex := Value;
end;  // TSpatialRef.SetImageIndex

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetImageList(Value: TCustomImageList);
begin
  FGetButton.ImageList := Value;
end;  // TSpatialRef.SetImageList

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetName(const NewName: TComponentName);
begin
  inherited;
  if Assigned(FSpatialRefEdit) then FSpatialRefEdit.Name := NewName + 'Edit';
  if Assigned(FQualifierCombo) then FQualifierCombo.Name := NewName + 'Combo';
end;  // TSpatialRef.SetName

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetOnFind(const Value: TNotifyEvent);
begin
  FOnFind := Value;
end;  // TSpatialRef.SetOnFind

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetOnGetFromMap(const Value: TNotifyEvent);
begin
  FGetButton.OnClick := Value;
end;  // TSpatialRef.SetOnGetFromMap

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetQualifier(const Value: String);
begin
  FQualifierCombo.Text := Value;
  Modified := True;
end;  // TSpatialRef.SetQualifier 

{-------------------------------------------------------------------------------
  Accessor.  Disables the panel border and the qualifier combo
}
procedure TSpatialRef.SetSimpleMode(const Value: Boolean);
begin
  FSimpleMode             := Value;
  FQualifierCombo.Visible := not FSimpleMode;
  FBevel.Visible          := not FSimpleMode;
end;

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetSpatialRef(const Value: String);
begin
  FSpatialRefEdit.Text := LocaliseSpatialRef(Value);
  Modified             := True;
end;  // TSpatialRef.SetSpatialRef 

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupButtons;
begin
  FGetButton := TImageListButton.Create(Self);
  with FGetButton do begin
    Parent   := Self;
    Height   := 23;
    Width    := 22;
    Caption  := '';
    Hint     := ResStr_FindSpatialRef;
    Visible  := True;
    TabOrder := 1;
  end;
  FDropDownButton := TButton.Create(Self);
  with FDropDownButton do begin
    Parent     := Self;
    Height     := 23;
    Width      := 14;
    ParentFont := False;
    Font.Name  := 'Marlett';  // MS font.
    Font.Style := [];
    Caption    := '6';  // Dropdown arrow
    Visible    := True;
    TabOrder   := 2;
    OnClick    := DropDownClick;
  end;
end;  // TSpatialRef.SetupButtons

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupCombo;
begin
  FQualifierCombo := TIDComboBox.Create(Self);
  with FQualifierCombo do begin
    Parent   := Self;
    Top      := 6;
    Width    := 116;
    Sorted   := True;
    Hint     := ResStr_Qualifier;
    Visible  := True;
    TabOrder := 3;
    OnChange := DataChange;
  end;
end;  // TSpatialRef.SetupCombo

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupControl;
begin
  Caption       := '';
  Width         := 277;
  Height        := 33;
  FBevel        := TBevel.Create(Self);
  FBevel.Parent := Self;
  FBevel.Align  := alClient;
  FBevel.Shape  := bsFrame;
end;  // TSpatialRef.SetupControl

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupEdit;
begin
  FSpatialRefEdit := TEdit.Create(Self);
  with FSpatialRefEdit do begin
    Parent     := Self;
    BevelInner := bvSpace;
    BevelKind  := bkFlat;
    BevelOuter := bvSpace;
    Left       := 4;
    Top        := 6;
    Charcase   := ecUppercase;
    Hint       := ResStr_SpatialRef;
    Visible    := True;
    TabOrder   := 0;
    OnChange   := DataChange;
    OnKeyPress := SpatialRefEditKeyPressed;
  end;
end;  // TSpatialRef.SetupEdit

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupShape;
begin
  FDragDropShape := TShape.Create(Self);
  with FDragDropShape do begin
    Parent  := Self;
    Tag     := DEST_ONLY;
    Visible := True;
  end;
end;  // TSpatialRef.SetupShape

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetValues(const Value: TSpatialRefValues);
begin
  SetEnteredRef(Value.EnteredRef);
  SetEnteredSystem(Value.EnteredSystem);
  SetQualifier(Value.Qualifier);
  SetDisplayRef(Value.DisplayRef);
  SetDisplaySystem(Value.DisplaySystem);
  Modified := True;
end;  // TSpatialRef.SetValues

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SpatialRefEditKeyPressed(Sender: TObject; var Key: Char);
begin
  Modified := True;
  if Key = #13 then
    if Assigned(FOnFind) then FOnFind(Self);
end;  // TSpatialRef.SpatialRefEditKeyPressed

{-------------------------------------------------------------------------------
  Will force the state of the buttons to be refreshed, without duplicating any code.
}
procedure TSpatialRef.UpdateMapWindowSelector;
begin
  SetEditMode(EditMode);
end;  // TSpatialRef.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
  Resize and realign all controls to fit the available space.
  Changed to resize combobox too, when visible.
}
procedure TSpatialRef.WMSize(var Message: TMessage);
var
  halfWidth: Integer;
begin
  // Resize control to have edit box vertically centered, unless below minimum height.
  if FSpatialRefEdit.Height + 12 < FGetButton.Height then
    Height := FGetButton.Height + 10
  else
    Height := FSpatialRefEdit.Height + 12;

  FBevel.SetBounds(0, 0, Width, Height);
  if not FSimpleMode then begin
    // The 11 here is for the 4/3/4 gaps around the controls.
    halfWidth := (Width - 11 - FGetButton.Width - FDropDownButton.Width) div 2;
    if FQualifierCombo.Ctl3D then
      FQualifierCombo.Top := FSpatialRefEdit.Top
    else
      FQualifierCombo.Top := FSpatialRefEdit.Top - 1;

    FSpatialRefEdit.Width := halfWidth;
    FGetButton.Left       := halfWidth + 5;  // Includes a +1 for FDragDropShape.
    FGetButton.Top        := (Height - FGetButton.Height) div 2;
    FDropDownButton.Left  := FGetButton.Left + FGetButton.Width - 1;
    FDropDownButton.Top   := FGetButton.Top;
    FQualifierCombo.Left  := Self.Width - halfWidth - 4;
    FQualifierCombo.Width := halfWidth;
  end else begin
    FDropDownButton.Left  := Width - FDropDownButton.Width - 3;
    FDropDownButton.Top   := (Self.Height - FGetButton.Height) div 2;
    FGetButton.Left       := FDropDownButton.Left - FGetButton.Width + 1;
    FGetButton.Top        := FDropDownButton.Top;
    FSpatialRefEdit.Width := FGetButton.Left - 5;
  end;
  FDragDropShape.SetBounds(
      FSpatialRefEdit.Left - 1, FSpatialRefEdit.Top - 1,
      FSpatialRefEdit.Width + 2, FSpatialRefEdit.Height + 2);

  if not FListAdded then begin
    FListAdded := True;
    SetupQualifierCombo;
  end;
end;  // TSpatialRef.WMSize

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.PrepareSRQualifiers;
begin
  slSRQualifiers := TStringList.Create;
  with slSRQualifiers do
  begin
    Delimiter := ' ';
    QuoteChar := '|';
    DelimitedText := ResStr_SRQualifiers;
  end
end;

{-------------------------------------------------------------------------------
}
procedure TSpatialRef.SetupQualifierCombo;
var
  i: Integer;
begin
  for i := 0 to slSRQualifiers.Count - 1 do
    FQualifierCombo.AddWithID(slSRQualifiers[i], i);
end;

end.
