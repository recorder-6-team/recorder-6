{===============================================================================

  Copyright © Dorset Software Services Ltd, 1999

  Component:
    Supporting TColorButton

  Updates:

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.

  Description:
    Support unit for TColorButton component. Diaplays the default set of
    available colours, with an extra button to call the standard Colour
    Selection dialog.

===============================================================================}

unit ColPal;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls;

type
  TSingleColorButton = class (TCustomControl)
  private
    FActiveColor:TColor;
    procedure SetColor (AColor:TColor);
    procedure LoseFocus (var Msg:TWMKillFocus); message WM_KILLFOCUS;
  public
    constructor Create (AOwner:TComponent); override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    property ActiveColor:TColor read FActiveColor write SetColor;
    property OnClick;
    property OnKeyPress;
  end;  // TSingleColorButton

  TdlgColorPalette = class(TForm)
    dlgCustomColors: TColorDialog;
    Bevel1: TBevel;
    pbOther: TButton;
    procedure FormClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure pbOtherClick(Sender: TObject);
    procedure pbOtherKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    clExtra       :TColor;
    FSelectedColor:TColor;
    FButtons      :array [0..20] of TSingleColorButton;
    procedure ColorButtonClick (Sender:TObject);
    procedure ColorButtonKeyPress (Sender:TObject; var Key:char);
    procedure FocusColorButton;
  public
    { Public declarations }
    property SelectedColor:TColor read FSelectedColor write FSelectedColor;
    constructor Create (AOwner:TComponent); override;
    destructor Destroy; override;
  end;

const
  DLG_WIDTH = 78;
  DLG_HEIGHT=121;

  AllColors : array[0..20] of TColor=(clBLACK,clBlack,clSilver,clGray,
                                      clRed,clMaroon,clYellow, clOlive,
                                      clLime,clGreen,clAqua,clTeal,
                                      clBlue,clNavy,clFuchsia,clPurple,
                                      $CCFFCC, $FFBBBB,
                                      $99FFEE, $EEEEEE,  // Extra colours
                                      clWhite); // Other colour

{
                     Left - Top    Heigth - Width
  Button 'Other...':   4     99       18     52
  Bevel TopLine    :   4     95        2     70

}

{==============================================================================}
implementation

{$R *.DFM}

{==============================================================================}
constructor TSingleColorButton.Create (AOwner:TComponent);
begin
  inherited Create (AOwner);
  Width        :=18;
  Height       :=18;
  Visible      :=true;
  FActiveColor :=clWhite;
end;  // TSingleColorButton.Create

{==============================================================================}
procedure TSingleColorButton.SetColor (AColor:TColor);
begin
  if AColor<>FActiveColor then
  begin
    FActiveColor:=AColor;
    Repaint;
  end;
end;  // TSingleColorButton.SetColor

{==============================================================================}
procedure TSingleColorButton.Paint;
begin
  with Canvas do
  begin
    Brush.Style:=bsClear;
    if Focused then
    begin
      Pen.Color:=clBlack;
      Rectangle (0,0,Width,Height);       {black/white/black surrounding the color}
      Rectangle (2,2,Width-2,Height-2);
      Pen.Color:=clWhite;
      Rectangle (1,1,Width-1,Height-1);
    end else begin
      Pen.Color:=clBtnFace;
      Rectangle (0,0,Width,Height);
      Rectangle (2,2,Width-2,Height-2);

      Pen.Color:=clBtnHighLight;
      Rectangle (1,1,Width-1,Height-1);
      Pen.Color:=clBtnShadow;
      MoveTo (1,Height-3);
      LineTo (1,1);
      LineTo (Width-2,1);

      Pen.Color:=clBlack;
      MoveTo (2,Height-4);
      LineTo (2,2);
      LineTo (Width-3,2);
    end;
    Pen.Color  :=clBlack;
    Brush.Color:=FActiveColor;
    FillRect (Rect (3,3,Width-3,Height-3));
  end;
end;  // TSingleColorButton.Paint

{==============================================================================}
procedure TSingleColorButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove (Shift,X,Y);
  if not Focused then begin
    SetFocus;    {repaint only if not already focused, avoid flickering}
    Repaint;
  end;
end;  // TSingleColorButton.MouseMove

{==============================================================================}
procedure TSingleColorButton.LoseFocus (var Msg:TWMKillFocus);
begin
  Repaint;
end;  // TSingleColorButton.LoseFocus

{==============================================================================}
{==============================================================================}
constructor TdlgColorPalette.Create (AOwner:TComponent);
var lIdx:integer;
begin
  inherited Create (AOwner);
  // Declare and set general properties of all colour buttons
  for lIdx:=0 to 20 do begin
    FButtons[lIdx]:=TSingleColorButton.Create (Self);
    with FButtons[lIdx] do begin
      Left       :=(lIdx mod 4)*18+3;
      Top        :=(lIdx div 4)*18+3;
      Parent     :=Self;
      ActiveColor:=AllColors[lIdx];
      OnClick    :=ColorButtonClick;
      OnKeyPress :=ColorButtonKeyPress;
    end;
  end;
  // Set buttons with special requirements
  // Extra colour button
  clExtra:=RGB (160,160,164);
  FSelectedColor:=clWhite;
  // Other colour button, position different
  FButtons[19].ActiveColor:=clExtra;
  with FButtons[20] do begin
    Left       :=57;
    Top        :=99;
    ActiveColor:=SelectedColor;
  end;
//  Application.OnDeactivate:=FormClick;
end;  // Create

{==============================================================================}
destructor TdlgColorPalette.Destroy;
var lIdx:integer;
begin
  for lIdx:=0 to 20 do
    FButtons[lIdx].Free;
  inherited Destroy;
end;  // Destroy

{==============================================================================}
procedure TdlgColorPalette.FormClick(Sender: TObject);
begin
  Hide;
end;  // FormClick

{==============================================================================}
procedure TdlgColorPalette.FormPaint(Sender: TObject);
begin
  inherited;
  with Canvas do
  begin
    Pen.Color:=clBlack;
    MoveTo (          0,DLG_HEIGHT-1);
    LineTo (DLG_WIDTH-1,DLG_HEIGHT-1);
    LineTo (DLG_WIDTH-1,          -1);
    Pen.Color:=clBtnHighLight;
    MoveTo (          1,DLG_HEIGHT-2);
    LineTo (          1,           1);
    LineTo (DLG_WIDTH-1,           1);
    Pen.Color:=clBtnShadow;
    MoveTo (          1,DLG_HEIGHT-2);
    LineTo (DLG_WIDTH-2,DLG_HEIGHT-2);
    LineTo (DLG_WIDTH-2,           0);
  end;
end;  // FormPaint

{==============================================================================}
procedure TdlgColorPalette.FocusColorButton;
var lIdx    :integer;
    ltfFound:boolean;
begin
  // Hide the 'Other' colour by default
  FButtons[20].Visible:=false;
  ltfFound:=false;
  // Look for SelectedColor in standard buttons first.
  for lIdx:=0 to 18 do  // Stop at 18, as Extra and Other colours not in constant array
    if SelectedColor=AllColors[lIdx] then begin
      FButtons[lIdx].SetFocus;
      ltfFound:=true;
      Break;
    end;

  if not ltfFound then
    // Not standard colour, checked against extra colour first
    if SelectedColor=clExtra then
      FButtons[19].SetFocus
    else begin
      // Custom colour selected
      FButtons[20].ActiveColor:=SelectedColor;
      FButtons[20].Visible:=true;
      FButtons[20].SetFocus;
    end;
end;  // FocusColorButton

{==============================================================================}
procedure TdlgColorPalette.FormShow(Sender: TObject);
begin
  inherited;
  FocusColorButton;
end;  // FormShow

{==============================================================================}
procedure TdlgColorPalette.pbOtherClick(Sender: TObject);
begin
  inherited;
  dlgCustomColors.Color:=SelectedColor;
  if dlgCustomColors.Execute then
    FSelectedColor:=dlgCustomColors.Color;
  Hide;
end;  // pbOtherClick

{==============================================================================}
procedure TdlgColorPalette.pbOtherKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then begin
    FSelectedColor:=-1;
    Hide;
  end;
end;  // pbotherKeyPress

{==============================================================================}
procedure TdlgColorPalette.ColorButtonClick (Sender:TObject);
begin
  with TSingleColorButton(Sender) do
    if Focused then begin
      if SelectedColor<>ActiveColor then
        FSelectedColor:=ActiveColor
      else
        FSelectedColor:=-1;
      Self.Hide;
    end;
end;  // ColorButtonClick

{==============================================================================}
procedure TdlgColorPalette.ColorButtonKeyPress(Sender: TObject; var Key: Char);
begin
  pbOtherKeyPress(Sender,Key);
end;  // ColorButtonKeyPress

{==============================================================================}
end.
