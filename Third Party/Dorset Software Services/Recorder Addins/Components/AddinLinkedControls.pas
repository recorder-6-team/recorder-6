{===============================================================================
  Unit:        AddinLinkedControl

  Defines:     TAddinLinkedEdit

  Description: A composite component to display linked data, which require the
               key of the displayed data to be remembered.

  Created:     August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 11/02/08 16:32 $
    $Author: Johnvanbreda $

===============================================================================}

unit AddinLinkedControls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  ImageListButton, AddinCompositeComponent, ImgList, Forms;

type
  {-----------------------------------------------------------------------------
    Composite component comprising an ImageListButton and an Edit box surrounded by a
    Shape. The shape can be turned on or off using the DisplayDragDropBorder property.
  }
  TAddinLinkedEdit = class(TAddinCompositeComponent)
  private
    FButton: TImageListButton;
    FEdit: TEdit;
    FKey: String;
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FOnFindData: TNotifyEvent;
    FOnGetData: TNotifyEvent;
    FShape: TShape;
    FOnEditMouseDown: TMouseEvent;
    FClearedKey: string;
    FInsertNameShortcutEnabled: Boolean;
    procedure ButtonClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var AKey: Word; Shift: TShiftState); virtual;
    procedure EditKeyPressed(Sender: TObject; var AKey: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
    function GetBorderStyle: TBorderStyle;
    function GetButtonHint: String;
    function GetButtonWidth: Integer;
    function GetCaption: String;
    function GetEditBox: TEdit;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetShowButton: Boolean;
    function GetShowShape: Boolean;
    function GetText: String;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetButtonHint(const Value: String);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetCaption(const Value: String);
    procedure SetImageIndex(Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetInsertNameShortcutEnabled(const Value: Boolean);
    procedure SetKey(const Value: String);
    procedure SetOnEditMouseDown(const Value: TMouseEvent);
    procedure SetSelStart(Value: Integer);
    procedure SetShowButton(Value: Boolean);
    procedure SetShowShape(const Value: Boolean);
    procedure SetText(const Value: String);
    procedure SetupButton;
    procedure SetupEdit;
    procedure SetupShape;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure SetOnFindData(const Value: TNotifyEvent);
    procedure SetOnGetData(const Value: TNotifyEvent);
  protected
    procedure SetColor(const Value: TColor); override;
    procedure DoCtl3DChanged; override;
    procedure SetEditMode(const Value: TEditMode); override;
    procedure SetFont(Value: TFont); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditSetFocus;
    property EditBox: TEdit read GetEditBox;
    property Key: String read FKey write SetKey;
    property Modified: Boolean read FModified write FModified;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property ClearedKey: string read FClearedKey;
    property InsertNameShortcutEnabled: Boolean read FInsertNameShortcutEnabled
        write SetInsertNameShortcutEnabled;
  published
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle;
    property ButtonHint: String read GetButtonHint write SetButtonHint;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 23;
    property Caption: String read GetCaption write SetCaption;
    property Color;
    property EditMode;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnFindData: TNotifyEvent read FOnFindData write SetOnFindData;
    property OnGetData: TNotifyEvent read FOnGetData write SetOnGetData;
    property OnKeyDown;
    property PopupMenu;
    property ShowButton: Boolean read GetShowButton write SetShowButton default True;
    property ShowDragDropBorder: Boolean read GetShowShape write SetShowShape default
        True;
    property Text: String read GetText write SetText;
    property OnEditMouseDown: TMouseEvent read FOnEditMouseDown write SetOnEditMouseDown;
  end;

  {-----------------------------------------------------------------------------
    Subclass of Addinlinkededit that allows F11 to insert current logged in
      user
  }
  TNameLinkedEdit = class(TAddinLinkedEdit)
  private
    FNameKey: string;
    FNameText: string;
    procedure SetNameKey(const Value: string);
    procedure SetNameText(const Value: string);
  protected
    procedure EditKeyDown(Sender: TObject; var AKey: Word; Shift: TShiftState); override;
  public
    property NameKey: string read FNameKey write SetNameKey;
    property NameText: string read FNameText write SetNameText;
  end;


//==============================================================================
implementation

{-==============================================================================
    TAddinLinkedEdit
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinLinkedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 152;
  Height :=  23;

  SetupShape;
  SetupEdit;
  SetupButton;
  InsertNameShortcutEnabled := False;
end;  // TLinkedEdit.Create 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnGetData) then
    FOnGetData(Self);
end;  // TLinkedEdit.ButtonClick

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.EditChange(Sender: TObject);
begin
  if FEdit.Modified then begin
    if Key<>'' then
      FClearedKey := Key;
    FKey := '';
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;  // TLinkedEdit.EditChange

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.EditKeyDown(Sender: TObject; var AKey: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then OnKeyDown(Sender, AKey, Shift);
  if (AKey=VK_F2) and Assigned(FOnGetData) then
    FOnGetData(Self);
end;  // TLinkedEdit.EditKeyDown

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.EditKeyPressed(Sender: TObject; var AKey: Char);
begin
  if not Visible then AKey := #0;
  if AKey in [#13, #27] then begin
    if (AKey = #13) and Assigned(FOnFindData) then FOnFindData(Self);
    // Remember to clear it, or we get an annoying beep.
    AKey := #0;
  end;
end;  // TLinkedEdit.EditKeyPressed

{-------------------------------------------------------------------------------
  Marshall mouse down on the edit box to the wrapper control's event
}
procedure TAddinLinkedEdit.EditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnEditMouseDown) then OnEditMouseDown(Sender, Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
  Public method allowing the developer to focus the edit control.
}
procedure TAddinLinkedEdit.EditSetFocus;
begin
  if FEdit.CanFocus then FEdit.SetFocus;
end;  // TLinkedEdit.EditSetFocus 

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetButtonHint: String;
begin
  Result := FButton.Hint;
end;  // TLinkedEdit.GetButtonHint

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetCaption: String;
begin
  if Assigned(FButton) then Result := FButton.Caption;
end;  // TAddinLinkedEdit.GetCaption

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetEditBox: TEdit;
begin
  Result := FEdit;
end;  // TLinkedEdit.GetEditBox

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetImageIndex: Integer;
begin
  Result := FButton.ImageIndex;
end;  // TLinkedEdit.GetImageIndex

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetImageList: TCustomImageList;
begin
  Result := FButton.ImageList;
end;  // TLinkedEdit.GetImageList

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;  // TLinkedEdit.GetSelLength 

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;  // TLinkedEdit.GetSelStart 

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetShowButton: Boolean;
begin
  Result := FButton.Visible;
end;  // TLinkedEdit.GetShowButton 

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetShowShape: Boolean;
begin
  Result := FShape.Visible;
end;  // TLinkedEdit.GetShowShape 

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetText: String;
begin
  Result := FEdit.Text;
end;  // TLinkedEdit.GetText 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetButtonHint(const Value: String);
begin
  FButton.Hint := Value;
end;  // TLinkedEdit.SetButtonHint 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetColor(const Value: TColor);
begin
  inherited SetColor(Value);

  FEdit.Color := Value;
end;  // TLinkedEdit.SetColor

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.DoCtl3DChanged;
begin
  FEdit.Ctl3D := Ctl3D;
end;  // TLinkedEdit.DoCtl3DChanged

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetEditMode(const Value: TEditMode);
begin
  inherited SetEditMode(Value);
  FEdit.ReadOnly  := Value = emBrowse;
  FButton.Enabled := not FEdit.ReadOnly;
end;  // TLinkedEdit.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetFont(Value: TFont);
begin
  inherited;
  FEdit.Font.Assign(Value);
end;  // TLinkedEdit.SetFont

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetImageIndex(Value: Integer);
begin
  FButton.ImageIndex := Value;
end;  // TLinkedEdit.SetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetImageList(Value: TCustomImageList);
begin
  FButton.ImageList := Value;
end;  // TLinkedEdit.SetImageList 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetKey(const Value: String);
begin
  if FKey <> Value then begin
    FKey := Value;
    Modified := True;
    // As the key was set explicitly (i.e. not by just typing in the box)
    // record it so that Find functionality can use this as the old key value
    FClearedKey := FKey;
  end;
end;  // TLinkedEdit.SetKey

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetSelStart(Value: Integer);
begin
  FEdit.SelStart := Value;
end;  // TLinkedEdit.SetSelStart

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetShowButton(Value: Boolean);
begin
  FButton.Visible := Value;
  AdjustSize;
end;  // TLinkedEdit.SetShowButton

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetShowShape(const Value: Boolean);
begin
  FShape.Visible := Value;
  AdjustSize;
end;  // TLinkedEdit.SetShowShape

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetText(const Value: String);
begin
  if FEdit.Text <> Value then begin
    FEdit.Text := Value;
    Modified := True;
  end;
end;  // TLinkedEdit.SetText 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetupButton;
begin
  FButton := TImageListButton.Create(Self);
  with FButton do begin
    Parent := Self;
    Height := 23;
    Width  := 23;
    Caption    := '';
    ImageIndex := -1;
    ImageList  := nil;
    Visible    := True;
    OnClick    := ButtonClick;
    SendToBack;
  end; // with
end;  // TLinkedEdit.SetupButton 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetupEdit;
begin
  FEdit := TEdit.Create(Self);
  with FEdit do begin
    Parent := Self;
    ParentFont := True;
    Visible    := True;
    OnChange   := EditChange;
    OnKeyDown  := EditKeyDown;
    OnKeyPress := EditKeyPressed;
    OnMouseDown:= EditMouseDown;
  end;
end;  // TLinkedEdit.SetupEdit

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetupShape;
begin
  FShape := TShape.Create(Self);
  with FShape do begin
    Parent := Self;
    Pen.Color := clRed;
    Visible := True;
    Tag     := DEST_ONLY;
  end;
end;  // TLinkedEdit.SetupShape 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if CanFocus and not FEdit.Focused and not FButton.Focused then
    EditSetFocus;
end;  // TLinkedEdit.WMSetFocus 

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.WMSize(var Message: TMessage);
var
  lBtnWidth: Integer;
begin
  // If Button not shown, shape and edit will be the length of control.
  if FButton.Visible then lBtnWidth := FButton.Width
                     else lBtnWidth := 0;
  
  if FShape.Visible then begin
    Self.Height := FEdit.Height + 2;
    FShape.SetBounds(0, 0, Self.Width - lBtnWidth, Self.Height);
    FEdit.SetBounds(1, 1, FShape.Width - 2, FEdit.Height);
    FButton.Height := FShape.Height;
  end else begin
    Self.Height := FEdit.Height;
    FEdit.SetBounds(0, 0, Self.Width - lBtnWidth, FEdit.Height);
    FButton.Height := FEdit.Height;
  end;

  FButton.Top  := 0;
  FButton.Left := Self.Width - FButton.Width;

  // Hide the button at design-time.
  if not FButton.Visible then begin
    FButton.Height := FButton.Height - 2;
    FButton.Top := 1;
    FButton.Left := FButton.Left - 1;
  end;

  Invalidate;
end;  // TLinkedEdit.WMSize

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetOnEditMouseDown(const Value: TMouseEvent);
begin
  FOnEditMouseDown := Value;
end;  // TLinkedEdit.SetOnEditMouseDown

{-------------------------------------------------------------------------------
  Accessor for edit box BorderStyle property
}
function TAddinLinkedEdit.GetBorderStyle: TBorderStyle;
begin
  Result := FEdit.BorderStyle;
end;  // TLinkedEdit.GetBorderStyle

{-------------------------------------------------------------------------------
}
function TAddinLinkedEdit.GetButtonWidth: Integer;
begin
  if Assigned(FButton) then 
    Result := FButton.Width
  else
    Result := 0;
end;  // TAddinLinkedEdit.GetButtonWidth

{-------------------------------------------------------------------------------
  Accessor for edit box BorderStyle property.  Also controls the bevel to
      provide appropriate spacing.
}
procedure TAddinLinkedEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  FEdit.BorderStyle := Value;
  // Set the bevel so that the text stays in roughly the correct place.
  if Value = bsNone then begin
    FEdit.BevelKind := bkFlat;
    FEdit.BevelOuter := bvSpace;
    FEdit.BevelInner := bvSpace;
  end else
    FEdit.BevelKind := bkNone;
  AdjustSize;
end;  // TLinkedEdit.SetBorderStyle

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetButtonWidth(const Value: Integer);
begin
  if Assigned(FButton) then begin
    FButton.Width := Value;
    AdjustSize;
  end;
end;  // TAddinLinkedEdit.SetButtonWidth

{-------------------------------------------------------------------------------
}
procedure TAddinLinkedEdit.SetCaption(const Value: String);
begin
  if Assigned(FButton) then
    FButton.Caption := Value;
end;  // TAddinLinkedEdit.SetCaption

{-------------------------------------------------------------------------------
  Accessor
}
procedure TAddinLinkedEdit.SetInsertNameShortcutEnabled(const Value: Boolean);
begin
  FInsertNameShortcutEnabled := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TAddinLinkedEdit.SetOnFindData(const Value: TNotifyEvent);
begin
  FOnFindData := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TAddinLinkedEdit.SetOnGetData(const Value: TNotifyEvent);
begin
  FOnGetData := Value;
end;

{===============================================================================
  TNameLinkedEdit
===============================================================================}

{-------------------------------------------------------------------------------
  Trap F11 key presses and insert logged in user name
}
procedure TNameLinkedEdit.EditKeyDown(Sender: TObject; var AKey: Word;
  Shift: TShiftState);
begin
  inherited;
  if (AKey=VK_F11) and Enabled and (not EditBox.ReadOnly) and (NameKey <> '') then begin
    // F11 pressed in an individual field, so put in logged in user
    Text := NameText;
    Key := NameKey;    
  end;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TNameLinkedEdit.SetNameKey(const Value: string);
begin
  FNameKey := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TNameLinkedEdit.SetNameText(const Value: string);
begin
  FNameText := Value;
end;

end.
