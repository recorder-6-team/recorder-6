{===============================================================================

  Copyright © Dorset Software Services Ltd, 1999

  Component:
    TColorButton - Eric Salmon 21/01/1999

  Updates:

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.
    InHouse7, Delphi 7 package for in house components.

  Description:
    Reproduces the colour button available in Win95/NT on the Appearance page of
    the Display Propreties settings.
    15/06/2009 - Now theme-aware.

  Additional information:
    This component uses the ColPal unit and form to display the default set of
    available colours, with an extra button to call the standard Colour
    Selection dialog.

===============================================================================}

unit ColorBtn;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Forms, Themes;

type
  TColorButton = class(TCustomControl)
  private
    FActiveColor: TColor;
    FFocused: Boolean;
    FdlgColors: TForm;
    FColorsOn: Boolean;
    FIsHot: Boolean;
    FIsPressed: Boolean;
    FOnChange: TNotifyEvent;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetActiveColor(Value: TColor);
  protected
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure GetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
    procedure LoseFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure Change; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property ActiveColor:TColor read FActiveColor write SetActiveColor;
    property Enabled;
    property TabOrder;
    property TabStop;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

{==============================================================================}
implementation

uses
  ColPal;

{-------------------------------------------------------------------------------
  Initialises a default size and colour. The actual dialog for the default
  colours is only instantiated at run-time.
}
constructor TColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height       := 20;
  Width        := 41;
  FActiveColor := clWhite;
  TabStop      := True;
  if not (csDesigning in ComponentState) then
    FdlgColors := TdlgColorPalette.Create(nil);
end;  // TColorButton.Create

{-------------------------------------------------------------------------------
  Handles the cleanup of the colours dialog.
}
destructor TColorButton.Destroy;
begin
  if not (csDesigning in ComponentState) then
    FdlgColors.Free;
  inherited Destroy;
end;  // TColorButton.Destroy

{-------------------------------------------------------------------------------
  Catches the MouseEnter notification to display themed button as hot.
}
procedure TColorButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then FIsHot := True;
  Repaint;
end;  // TColorButton.CMMouseEnter

{-------------------------------------------------------------------------------
  Catches the MouseLeave notification to end the themed button hot status.
}
procedure TColorButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then FIsHot := False;
  Repaint;
end;  // TColorButton.CMMouseEnter

{-------------------------------------------------------------------------------
  Draw the button with the colour section and side arrow to bring up the
  colour dialog. Theme-aware.
}
procedure TColorButton.Paint;
var
  shift: Byte;
  lineColour: TColor;
  themeDetails: TThemedElementDetails;
begin
  shift := 0;

  if Enabled then
    lineColour := clBtnText
  else
    lineColour := clBtnShadow;  // For the disabled version

  if ThemeServices.ThemesEnabled then
  begin
    // Handle XP themes (Vista too?)
    if not Enabled then
      themeDetails := ThemeServices.GetElementDetails(tbPushButtonDisabled)
    else
    if (Focused or FIsHot) and not FIsPressed then
      themeDetails := ThemeServices.GetElementDetails(tbPushButtonHot)
    else
    if FIsPressed then
      themeDetails := ThemeServices.GetElementDetails(tbPushButtonPressed)
    else
      themeDetails := ThemeServices.GetElementDetails(tbPushButtonNormal);
    ThemeServices.DrawElement(Canvas.Handle, themeDetails, ClientRect);
  end else
    // Normal (non-themed) painting.
    with Canvas do
    begin
      if FIsPressed then shift := 1;
      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, Width, Height));
      Pen.Color := cl3DDkShadow;
      Rectangle(0, 0, Width, Height);
      if FIsPressed then
      begin
        Pen.Color := clBtnHighLight;
        MoveTo(1, Height - 1);
        LineTo(Width - 1, Height - 1);
        LineTo(Width - 1, 0);

        Pen.Color := clBtnShadow;
        MoveTo(1, Height - 2);
        LineTo(1, 1);
        LineTo(Width - 1, 1);
      end else begin
        Pen.Color := clBtnHighLight;
        MoveTo(0, Height - 2);
        LineTo(0, 0);
        LineTo(Width - 1, 0);

        Pen.Color:= clBtnShadow;
        MoveTo(1, Height - 2);
        LineTo(Width - 2, Height - 2);
        LineTo(Width - 2, 0);
      end;
    end;

  // Draw colour box and arrow, same in all cases.
  with Canvas do
  begin
    Pen.Color := lineColour;
    // Colour boundary.
    Rectangle(4 + shift, 4 + shift, Width - 15 + shift, Height - 4 + shift);

    // Selected colour.
    Brush.Color := FActiveColor;
    FillRect(Rect(5 + shift, 5 + shift, Width - 16 + shift, Height - 5 + shift));

    // Arrow.
    MoveTo(Width - 9 + shift, Height div 2 - 2 + shift);  LineTo(Width - 4 + shift, Height div 2 - 2 + shift);
    MoveTo(Width - 8 + shift, Height div 2 - 1 + shift);  LineTo(Width - 5 + shift, Height div 2 - 1 + shift);
    MoveTo(Width - 7 + shift, Height div 2     + shift);  LineTo(Width - 6 + shift, Height div 2     + shift);

    // Separator between colour and arrow.
    Pen.Color := clBtnShadow;
    MoveTo(Width - 12 + shift, 4 + shift);  LineTo(Width - 12 + shift, Height - 4 + shift);
    Pen.Color := clBtnHighLight;
    MoveTo(Width - 11 + shift, 4 + shift);  LineTo(Width - 11 + shift, Height - 4 + shift);

    if FFocused then
      DrawFocusRect(Rect(2 + shift, 3 + shift, Width - 3 + shift, Height - 3 + shift));
  end;
end;  // TColorButton.Paint

{-------------------------------------------------------------------------------
  Accessor. Sets the colour to display on the button.
}
procedure TColorButton.SetActiveColor(Value: TColor);
begin
  FActiveColor := Value;
  Repaint;
  Change;
end;  // TColorButton.SetActiveColor

{-------------------------------------------------------------------------------
  Handles MouseDown notification. Sets the button's IsPressed status.
}
procedure TColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
  SetFocus;
  FIsPressed := True;
  Repaint;
end;  // TColorButton.MouseDown

{-------------------------------------------------------------------------------
  Handles MouseUp notification. unset the button's IsPressed status and display
  the colour dialog.
}
procedure TColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  dialogPos, buttonPos: TPoint;
begin
  inherited MouseUp(Button,Shift,X,Y);
  FIsPressed := False;
  Repaint;

  with TdlgColorPalette(FdlgColors) do
  begin
    SelectedColor := ActiveColor;
    Show;
  end;
  FColorsOn := True;

  buttonPos.X := 0;
  buttonPos.Y := Height;
  dialogPos   := ClientToScreen(buttonPos);

  with TdlgColorPalette(FdlgColors) do
  begin
    CLientWidth   := DLG_WIDTH;
    ClientHeight  := DLG_HEIGHT;
    Left          := dialogPos.X;
    Top           := dialogPos.Y;
    SelectedColor := ActiveColor;
    Show;
  end;
end;  // TColorButton.MouseUp

{-------------------------------------------------------------------------------
}
procedure TColorButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift,X,Y);
end;  // MouseMove

{-------------------------------------------------------------------------------
  Updates the button with the selected colour.
}
procedure TColorButton.GetFocus(var Msg: TWMSetFocus);
begin
  if FColorsOn then
  begin
    // Update active color, if new one selected
    with TdlgColorPalette(FdlgColors) do begin
      Hide;
      if SelectedColor <> -1 then ActiveColor := SelectedColor;
    end;
    FColorsOn := False;
    Change;
  end;
  FFocused := True;
  Repaint;
end;  // TColorButton.GetFocus

{-------------------------------------------------------------------------------
}
procedure TColorButton.LoseFocus(var Msg: TWMKillFocus);
begin
  FFocused := False;
  Repaint;
end;  // TColorButton.LoseFocus

{-------------------------------------------------------------------------------
}
procedure TColorButton.Change;
begin
  Changed;
  if Assigned(FOnChange) then FOnChange(Self);
end;  // TColorButton.Change

{-------------------------------------------------------------------------------
}
procedure TColorButton.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;  // TColorButton.WMGetDlgCode

end.
