{===============================================================================
  Unit:        LinkedControl

  Defines:     TLinkedControl

  Description: A composite component to display linked data, which require the
               key of the displayed data to be remembered.

  Created:     August 2003

  Last revision information:
    $Revision: 33 $
    $Date: 13/03/08 12:06 $
    $Author: Ericsalmon $

===============================================================================}

unit LinkedControls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  ImageListButton, BaseCompositeComponent, ImgList, DSSDataTypes, Forms;

type
  {-----------------------------------------------------------------------------
    Composite component comprising an ImageListButton and an Edit box surrounded by a
    Shape. The shape can be turned on or off using the DisplayDragDropBorder property.
  }
  TLinkedEdit = class (TBaseCompositeComponent)
  private
    FKey: String;
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FOnFindData: TNotifyEvent;
    FOnGetData: TNotifyEvent;
    FOnEditMouseDown: TMouseEvent;
    procedure ButtonClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPressed(Sender: TObject; var Key: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
    function GetBorderStyle: TBorderStyle;
    function GetButtonHint: String;
    function GetEditBox: TEdit;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetMaxLength: Integer;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetShowButton: Boolean;
    function GetShowShape: Boolean;
    function GetText: String;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetButtonHint(const Value: String);
    procedure SetImageIndex(Value: Integer);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetKey(const Value: String);
    procedure SetMaxLength(Value: Integer);
    procedure SetOnEditMouseDown(const Value: TMouseEvent);
    procedure SetSelStart(Value: Integer);
    procedure SetShowShape(const Value: Boolean);
    procedure SetupButton;
    procedure SetupEdit;
    procedure SetupShape;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    FButton: TImageListButton;
    FEdit: TEdit;
    FShape: TShape;
    procedure DoResize; virtual;
    procedure SetColor(const Value: TColor); override;
    procedure DoCtl3DChanged; override;
    procedure SetEditMode(const Value: TEditMode); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetFont(Value: TFont); override;
    procedure SetShowButton(Value: Boolean); virtual;
    procedure SetText(const Value: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditSetFocus;
    property EditBox: TEdit read GetEditBox;
    property Key: String read FKey write SetKey;
    property Modified: Boolean read FModified write FModified;
    property SelLength: Integer read GetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
  published
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle;
    property ButtonHint: String read GetButtonHint write SetButtonHint;
    property Color;
    property EditMode;   
    property Enabled write SetEnabled;
    property Font write SetFont;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnFindData: TNotifyEvent read FOnFindData write FOnFindData;
    property OnGetData: TNotifyEvent read FOnGetData write FOnGetData;
    property OnKeyDown;
    property PopupMenu;
    property ShowButton: Boolean read GetShowButton write SetShowButton default True;
    property ShowDragDropBorder: Boolean read GetShowShape write SetShowShape default
        True;
    property Text: String read GetText write SetText;
    property OnEditMouseDown: TMouseEvent read FOnEditMouseDown write SetOnEditMouseDown;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TLinkedEdit
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TLinkedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width  := 152;
  Height :=  23;

  SetupShape;
  SetupEdit;
  SetupButton;
end;  // TLinkedEdit.Create 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.ButtonClick(Sender: TObject);
begin
  if Assigned(FOnGetData) then
    FOnGetData(Self);
end;  // TLinkedEdit.ButtonClick 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.EditChange(Sender: TObject);
begin
  if FEdit.Modified then
    Key := '';
  if Assigned(FOnChange) then FOnChange(Self);
end;  // TLinkedEdit.EditChange

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then OnKeyDown(Sender, Key, Shift);
  if (Key=VK_F2) and Assigned(FOnGetData) then
    FOnGetData(Self);
end;  // TLinkedEdit.EditKeyDown

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.EditKeyPressed(Sender: TObject; var Key: Char);
begin
  if Key in [#13, #27] then begin
    if (Key = #13) and Assigned(FOnFindData) then FOnFindData(Self);
    // Remember to clear it, or we get an annoying beep.
    Key := #0;
  end;
end;  // TLinkedEdit.EditKeyPressed

{-------------------------------------------------------------------------------
  Marshall mouse down on the edit box to the wrapper control's event
}
procedure TLinkedEdit.EditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnEditMouseDown) then OnEditMouseDown(Sender, Button, Shift, X, Y);
end;

{-------------------------------------------------------------------------------
  Public method allowing the developer to focus the edit control.
}
procedure TLinkedEdit.EditSetFocus;
begin
  if FEdit.CanFocus then FEdit.SetFocus;
end;  // TLinkedEdit.EditSetFocus 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetButtonHint: String;
begin
  Result := FButton.Hint;
end;  // TLinkedEdit.GetButtonHint 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetEditBox: TEdit;
begin
  Result := FEdit;
end;  // TLinkedEdit.GetEditBox

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetImageIndex: Integer;
begin
  Result := FButton.ImageIndex;
end;  // TLinkedEdit.GetImageIndex 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetImageList: TCustomImageList;
begin
  Result := FButton.ImageList;
end;  // TLinkedEdit.GetImageList

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;  // TLinkedEdit.GetMaxLength

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;  // TLinkedEdit.GetSelLength

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;  // TLinkedEdit.GetSelStart 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetShowButton: Boolean;
begin
  Result := FButton.Visible;
end;  // TLinkedEdit.GetShowButton 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetShowShape: Boolean;
begin
  Result := FShape.Visible;
end;  // TLinkedEdit.GetShowShape 

{-------------------------------------------------------------------------------
}
function TLinkedEdit.GetText: String;
begin
  Result := FEdit.Text;
end;  // TLinkedEdit.GetText 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetButtonHint(const Value: String);
begin
  FButton.Hint := Value;
end;  // TLinkedEdit.SetButtonHint 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetColor(const Value: TColor);
begin
  inherited SetColor(Value);

  FEdit.Color := Value;
end;  // TLinkedEdit.SetColor

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.DoCtl3DChanged;
begin
  FEdit.Ctl3D := Ctl3D;
end;  // TLinkedEdit.DoCtl3DChanged

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetEditMode(const Value: TEditMode);
begin
  inherited SetEditMode(Value);
  FEdit.ReadOnly  := Value = emBrowse;
  FButton.Enabled := not FEdit.ReadOnly;
end;  // TLinkedEdit.SetEditMode
 
{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FEdit.Enabled := Value;
  FButton.Enabled := Value;
end; // TLinkedEdit.SetEnabled

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetFont(Value: TFont);
begin
  inherited;
  FEdit.Font.Assign(Value);
end;  // TLinkedEdit.SetFont 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetImageIndex(Value: Integer);
begin
  FButton.ImageIndex := Value;
end;  // TLinkedEdit.SetImageIndex 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetImageList(Value: TCustomImageList);
begin
  FButton.ImageList := Value;
end;  // TLinkedEdit.SetImageList 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetKey(const Value: String);
begin
  if FKey <> Value then begin
    FKey := Value;
    Modified := True;
  end;
end;  // TLinkedEdit.SetKey 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetMaxLength(Value: Integer);
begin
  FEdit.MaxLength := Value;
end;  // TLinkedEdit.SetMaxLength

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetSelStart(Value: Integer);
begin
  FEdit.SelStart := Value;
end;  // TLinkedEdit.SetSelStart 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetShowButton(Value: Boolean);
begin
  FButton.Visible := Value;
  AdjustSize;
end;  // TLinkedEdit.SetShowButton

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetShowShape(const Value: Boolean);
begin
  FShape.Visible := Value;
  AdjustSize;
end;  // TLinkedEdit.SetShowShape

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetText(const Value: String);
begin
  if FEdit.Text <> Value then begin
    FEdit.Text := Value;
    Modified := True;
  end;
end;  // TLinkedEdit.SetText 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetupButton;
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
procedure TLinkedEdit.SetupEdit;
begin
  FEdit := TEdit.Create(Self);
  with FEdit do begin
    Parent := Self;
    Visible    := True;
    OnChange   := EditChange;
    OnKeyDown  := EditKeyDown;
    OnKeyPress := EditKeyPressed;
    OnMouseDown:= EditMouseDown;
  end;
end;  // TLinkedEdit.SetupEdit 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetupShape;
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
procedure TLinkedEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Visible and not FEdit.Focused and not FButton.Focused then
    FEdit.SetFocus;
end;  // TLinkedEdit.WMSetFocus 

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.WMSize(var Message: TMessage);
begin
  DoResize;
end;  // TLinkedEdit.WMSize

{-------------------------------------------------------------------------------
}
procedure TLinkedEdit.SetOnEditMouseDown(const Value: TMouseEvent);
begin
  FOnEditMouseDown := Value;
end;  // TLinkedEdit.SetOnEditMouseDown

{-------------------------------------------------------------------------------
  Accessor for edit box BorderStyle property
}
function TLinkedEdit.GetBorderStyle: TBorderStyle;
begin
  Result := FEdit.BorderStyle;
end;  // TLinkedEdit.GetBorderStyle

{-------------------------------------------------------------------------------
  Accessor for edit box BorderStyle property.  Also controls the bevel to
      provide appropriate spacing.
}
procedure TLinkedEdit.SetBorderStyle(const Value: TBorderStyle);
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
procedure TLinkedEdit.DoResize;
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
end;  // TLinkedEdit.DoResize;

end.
