{Version 9.03}     
{*********************************************************}
{*                     HTMLSBS1.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

unit Htmlsbs1;

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, HTMLUn2, HTMLGif2, HTMLSubs, StyleUn;

Type

  TPage = class(TSectionBase)
  public
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

  THorzLine = class(TSectionBase)          {a horizontal line, <hr>}
    VSize: integer;
    Color: TColor;
    Align: JustifyType;
    NoShade: boolean;
    BkGnd: boolean;
    Width, Indent: integer;   

    constructor Create(AMasterList: TSectionList; L: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    procedure CopyToClipboard; override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

   TPreFormated = class(TSection)
  {section for preformated, <pre>}
  public
    procedure ProcessText; override;     
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    end;

  TOptionObj = class(TObject)   {used by TListBoxFormControlObj}
  public
    Value: String;
    Selected: boolean;
  end;

  TListBoxFormControlObj = class(TFormControlObj)
  private
    LBSize, Longest: integer;
    TheFont: TFont;
  private
    EnterItems: integer;
    EnterSelected: array[0..50] of boolean;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    TheOptions: TStringList;
    {TheOptions is the original Options list for reseting.  It does not reflect
     the current selected state.  The Strings part is the Items in the list/combobox.
     The Options part is TOptionObj defined above}

    constructor Create(AMasterList: TSectionList; Position: integer;
                     L: TAttributeList; Prop: TProperties);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure AddStr(const WS: WideString; Value: string; Selected: boolean;
                 CodePage: integer);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;     
  end;

  TComboFormControlObj = class(TListBoxFormControlObj)
  private
    EnterIndex: integer;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    constructor Create(AMasterList: TSectionList; Position: integer;
                  L: TAttributeList; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ResetToValue; override;
    procedure SetHeightWidth(ACanvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;     
  end;

  TTextAreaFormControlObj = class(TFormControlObj)
  private
    EnterContents: string;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    Wrap: (wrOff, wrSoft, wrHard);
    Rows, Cols: integer;
    TheText: string;   
    constructor Create(AMasterList: TSectionList; Position: integer;
                   L: TAttributeList; Prop: TProperties);
    destructor Destroy; override;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure AddStr(const S: string);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
  end;

  TFormControlList = class(TList)  {a list of TFormControlObj's}  {not TFreeList}
  Public
    function FindControl(Posn: integer): TFormControlObj;
    function GetHeightAt(Posn: integer; var FormAlign: AlignmentType) : Integer;
    function GetWidthAt(Posn: integer; var HSpcL, HSpcR: integer) : integer;
    function GetControlCountAt(Posn: integer): integer;
    procedure Decrement(N: integer);
    end;

Implementation

uses
  {$ifdef Delphi6_Plus}
  Variants,
  {$endif}
  ReadHTML, HTMLView;

{----------------TPage.Draw1}
function TPage.Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  YOffset, Y: integer;
begin
Result := inherited Draw1(Canvas, ARect, Imgr, X, XRef, YRef);
Y := YDraw;

with ParentSectionList do
  if Printing then
    begin
    YOffset := YOff;
    if (Y-YOffset > ARect.Top+5) and (Y-YOffset < ARect.Bottom) and
         (Y < PageBottom) then
      PageBottom := Y;
    end;
end;

{----------------THorzLine.Create}
constructor THorzLine.Create(AMasterList: TSectionList; L: TAttributeList;
                Prop: TProperties);
var
  LwName: string[10];
  I: integer;
  TmpColor: TColor;
begin
inherited Create(AMasterList);
VSize := 2;
Align := Centered;
Color := clNone;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      SizeSy: if (Value > 0) and (Value <= 20) then
        begin
        VSize := Value;
        end;
      WidthSy:
        if Value > 0 then
          if Pos('%', Name) > 0 then
            begin
            if (Value <= 100) then
              Prop.Assign(IntToStr(Value)+'%', StyleUn.Width);
            end
          else
            Prop.Assign(Value, StyleUn.Width);
      ColorSy: if ColorFromString(Name, False, Color) then
                 Prop.Assign(Color, StyleUn.Color);
      AlignSy:
        begin
        LwName := Lowercase(Name);
        if LwName = 'left' then Align := Left
        else if LwName = 'right' then Align := Right;
        end;
      NoShadeSy: NoShade := True;
      end;

Prop.Assign(VSize, StyleUn.Height); {assigns if no property exists yet}
TmpColor := Prop.GetOriginalForegroundColor;
if TmpColor <> clNone then
  Color := TmpColor;
with Prop do
  if (varType(Props[TextAlign]) = VarString) and Originals[TextAlign] then
    if Props[TextAlign] = 'left' then
      Align := Left
    else if Props[TextAlign] = 'right' then
      Align := Right
    else if Props[TextAlign] = 'center' then
      Align := Centered;
end;

constructor THorzLine.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
begin
inherited Create(AMasterList);
System.Move((T as THorzline).VSize, VSize, DWord(@BkGnd)-DWord(@VSize)+Sizeof(BkGnd));
end;

procedure THorzLine.CopyToClipboard;
begin
ParentSectionList.CB.AddTextCR('', 0);
end;

function THorzLine.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
YDraw := Y;
StartCurs := Curs;
{Note: VSize gets updated in THRBlock.FindWidth}
ContentTop := Y;
DrawTop := Y;
Indent := IntMax(X, IMgr.LeftIndent(Y));
Width := IntMin(X + AWidth - Indent, IMgr.RightSide(Y)-Indent);
MaxWidth := Width;
SectionHeight := VSize;  
DrawHeight := SectionHeight;
ContentBot := Y+SectionHeight;
DrawBot := Y+DrawHeight;
Result := SectionHeight;
end;

{----------------THorzLine.Draw}
function THorzLine.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  XR: integer;
  YT, YO, Y: integer;
  White, BlackBorder: boolean;
begin
Y := YDraw;  
Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
YO := Y - ParentSectionList.YOff;
if (YO+SectionHeight >= ARect.Top) and (YO < ARect.Bottom) and
       (not ParentSectionList.Printing or (Y < ParentSectionList.PageBottom)) then
  with Canvas do
    begin
    YT := YO;
    X := IMgr.LfEdge+Indent;
    XR := X+Width-1;
    if Color <> clNone then
      begin
      Brush.Color := Color or $2000000;
      Brush.Style := bsSolid;
      FillRect(Rect(X, YT, XR+1, YT+VSize));
      end
    else
      begin
      with ParentSectionList do
        begin
        White := Printing or ((Background and $FFFFFF = clWhite) or
            ((Background = clWindow) and (GetSysColor(Color_Window) = $FFFFFF)));
        BlackBorder := NoShade or (Printing and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and
            (GetDeviceCaps(Handle, PLANES) = 1));
        end;
      if BlackBorder then Pen.Color := clBlack
        else Pen.Color := clBtnShadow;
      MoveTo(X, YT+VSize-1);
      LineTo(X, YT);
      LineTo(XR, YT);
      if BlackBorder then
        Pen.Color := clBlack
      else if White then
        Pen.Color := clSilver
      else Pen.Color := clBtnHighLight;
      LineTo(XR, YT+VSize-1);
      LineTo(X, YT+VSize-1);
      end;
    ParentSectionList.FirstPageItem := False;     {items after this will not be first on page}
    end;
end;

procedure TPreformated.ProcessText;     
var
  I: integer;
begin
I := Pos(nbsp, BuffS);
while I > 0 do
  begin
  BuffS[I] := ' ';   {subst space}
  Brk[I] := 'n';     {no break}
  I := Pos(nbsp, BuffS);
  end;
  
Finish;
end;

procedure TPreformated.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
if Len = 0 then
  begin
  Max := 0;
  Min := 0;
  end
else
  begin
  if StoredMax = 0 then  
    begin
    Max := FindTextWidth(Canvas, Buff, Len, False);
    StoredMax := Max;
    end
  else Max := StoredMax;  
  Min := IntMin(2000, Max);   {arbitrary selection}   
  end;
end;

function TPreFormated.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
var
  Dummy: integer;
  Save: integer;
begin
if Len = 0 then
  begin
  ContentTop := Y;
  Result := Fonts.GetFontAt(0, Dummy).Size;
  SectionHeight := Result;
  MaxWidth := 0;
  YDraw := Y;   
  DrawHeight := Result;   
  ContentBot := Y+Result;  
  DrawBot := ContentBot;    
  end
else
  begin
  {call with large width to prevent wrapping}
  Save := IMgr.Width;
  IMgr.Width := 32000;
  Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, IMgr, Dummy, Curs);
  IMgr.Width := Save;
  MinMaxWidth(Canvas, Dummy, MaxWidth);   {return MaxWidth}
  end;
end;

{----------------TListBoxFormControlObj.Create}
constructor TListBoxFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  Multiple: boolean;
  PntPanel: TPaintPanel;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
TheOptions := TStringList.Create;
Multiple := L.Find(MultipleSy, T);
if L.Find(SizeSy, T) then
  LBSize := T.Value
else LBSize := -1;
Longest := 3;   {the minimum size}
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TListBox.Create(PntPanel);
with TListBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Tmp := Prop.GetFont; 
  Font.Assign(Tmp);
  TheFont := Font;
  Tmp.Free;
  MultiSelect := Multiple;
  ExtendedSelect := Multiple;
  OnEnter := EnterEvent;    
  OnExit := ExitEvent;
  OnClick := FormControlClick;   
  end;
FControl.Parent := PntPanel;
end;

destructor TListBoxFormControlObj.Destroy;
var
  I: integer;
begin
for I := 0 to TheOptions.Count-1 do
  with TOptionObj(TheOptions.Objects[I]) do
    Free;
TheOptions.Free;
inherited Destroy;
end;

procedure TListBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I: integer;
  LB: TListBox;
begin
LB := FControl as TListBox;  {watch it, TListBox has a canvas too}
FormControlRect(Canvas, X1, Y1, X1+LB.Width, Y1+LB.Height, False, MasterList.PrintMonoBlack); 
Canvas.Brush.Style := bsClear;
Canvas.Font := LB.Font;
H2 := Abs(Canvas.Font.Height);
SetTextAlign(Canvas.handle, TA_Left+TA_Top);
for I := LB.TopIndex to IntMin(LB.Items.Count-1, LB.TopIndex+LBSize-1) do
  Canvas.TextRect(Rect(X1+4, Y1+4, X1+LB.Width-8, Y1+LB.Height-8), X1+4,
           Y1+4+(I-LB.TopIndex)*H2, LB.Items[I]);
end;

procedure TListBoxFormControlObj.AddStr(const WS: WideString; Value: string;
              Selected: boolean; CodePage: integer); 
var
  Opt: TOptionObj;
  DC: HDC;
  OldFont: THandle;
  ExtS: TSize;
  S1: string;
  N: integer;
begin
SetLength(S1, 2*Length(WS));
N := WideCharToMultibyte(CodePage, 0, PWideChar(WS), Length(WS), PChar(S1), Length(S1), Nil, Nil); 
SetLength(S1, N);
if Value = '' then
  Value := S1;
Opt := TOptionObj.Create;
Opt.Value := Value;
Opt.Selected := Selected;
TheOptions.AddObject(S1, Opt);
DC := GetDC(0);
OldFont := SelectObject(DC, TheFont.Handle);
GetTextExtentPoint32(DC, PChar(S1), Length(S1), ExtS);
SelectObject(DC, OldFont);
ReleaseDC(0, DC);
Longest := IntMax(Longest, ExtS.cx);
end;

procedure TListBoxFormControlObj.ResetToValue;
var
  I: Integer;
  Tmp: boolean;
begin
with (FControl as TListBox) do
  begin
  Clear;
  for I := 0 to TheOptions.Count-1 do
    begin
    Items.Add(TheOptions[I]);
    Tmp := (TheOptions.Objects[I] as TOptionObj).Selected;
    if MultiSelect then
      Selected[I] := Tmp
    else if Tmp then
      ItemIndex := I;
    end;
  if ItemIndex < 0 then
    ItemIndex := 0;
  TopIndex := 0;
  end;
end;

procedure TListBoxFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with TListBox(FControl) do
  begin
  Canvas.Font := Font;
  if LBSize = -1 then LBSize := IntMax(1, IntMin(8, TheOptions.Count));
  if FHeight >= 10 then
    ClientHeight := FHeight
  else
    ClientHeight := Canvas.TextHeight('A')*LBSize;
  if FWidth >= 10 then
    Width := FWidth
  else Width := Longest + GetSystemMetrics(sm_cxvscroll) + 10;  
  end;
end;

function TListBoxFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
begin
with (FControl as TListBox) do
  if (Index < Items.Count) then
      begin
      Result := True;
      S := '';
      if MultiSelect and Selected[Index] or
                     not MultiSelect and (ItemIndex = Index) then
        begin
        S := Self.FName+'=';
        with TheOptions.Objects[Index] as TOptionObj do
          S := S + Value;
        end;
    end
  else Result := False;
end;

procedure TListBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
var
  I: integer;
begin
with TListBox(FControl) do
  begin
  EnterItems := Items.Count;
  for I := 0 to IntMin(Items.Count-1, 50) do
    EnterSelected[I] := Selected[I];
  end;
end;

procedure TListBoxFormControlObj.DoOnChange;
var
  I: integer;
  Changed: boolean;
begin
Changed := False;
with TListBox(FControl) do
  begin
  if Items.Count <> EnterItems then
    Changed := True
  else
    for I := 0 to IntMin(Items.Count-1, 50) do
      if EnterSelected[I] <> Selected[I] then
        begin
        Changed := True;
        Break;
        end;
  end;
if Changed then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

procedure TListBoxFormControlObj.SetData(Index: integer; const V: String);
var
  I: integer;
  Option: TOptionObj;
  LB: TListbox;
begin
LB := FControl as TListBox;
if Index = 0 then
  LB.ItemIndex := 0;
for I := 0 to TheOptions.Count-1 do
  begin
  Option := TOptionObj(TheOptions.Objects[I]);
  if Index = 0 then
    begin
    Option.Selected := False;  {first time thru set to all False}
    with LB do
      if MultiSelect then
        Selected[I] := False;
    end;
  if CompareText(V, Option.Value) = 0 then
    begin
    Option.Selected := True;
    with LB do
      if MultiSelect then
        Selected[I] := True
      else ItemIndex := I;
    end;
  end;
LB.TopIndex := 0;  
end;

{----------------TComboFormControlObj.Create}
constructor TComboFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; Prop: TProperties);  
var
  PntPanel: TPaintPanel;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L, Prop);
PntPanel := TPaintPanel(AMasterList.PPanel);
PntPanel.RemoveControl(FControl);
FControl.Free;   {don't want the inherited one}
FControl := TComboBox.Create(PntPanel);
with TComboBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Tmp := Prop.GetFont; 
  Font.Assign(Tmp);
  TheFont := Font;
  Tmp.Free;
  Style := csDropDownList;    
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := FormControlClick;
  end;
FControl.Parent := PntPanel;
end;

procedure TComboFormControlObj.ResetToValue;
var
  I: Integer;
begin
with (FControl as TComboBox) do
  begin
  Clear;
  for I := 0 to TheOptions.Count-1 do
    begin
    Items.Add(TheOptions[I]);
    if (TheOptions.Objects[I] as TOptionObj).Selected then
      ItemIndex := I;
    end;
  if ItemIndex < 0 then
    ItemIndex := 0;
  end;
end;

procedure TComboFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  CB: TComboBox;
begin
CB := FControl as TComboBox;  {watch it, TComboBox has a canvas too}
FormControlRect(Canvas, X1, Y1, X1+CB.Width, Y1+CB.Height, False, MasterList.PrintMonoBlack);
Canvas.Brush.Style := bsClear;
Canvas.Font := CB.Font;
SetTextAlign(Canvas.handle, TA_Left+TA_Top);
Canvas.TextRect(Rect(X1+4, Y1+4, X1+CB.Width-8, Y1+CB.Height-3), X1+4,  
           Y1+4, CB.Items[CB.ItemIndex]);
end;

procedure TComboFormControlObj.SetHeightWidth(ACanvas: TCanvas);
begin
with TComboBox(FControl) do
  begin
  if FHeight >= 10 then
    Height := FHeight;
  if FWidth >= 10 then
    Width := FWidth
  else
    ClientWidth := Longest + GetSystemMetrics(sm_cxvscroll) + 10;  
  end;
end;

function TComboFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
begin
if Index = 0 then
  begin
  Result := True;
  with (FControl as TComboBox) do
    if (ItemIndex >= 0) and (ItemIndex <= Items.Count) then
      with TheOptions.Objects[ItemIndex] as TOptionObj do
        S := Self.FName+'='+Value;
  end
else Result := False;
end;

procedure TComboFormControlObj.SetData(Index: integer; const V: String);
var
  CB: TComboBox;
  I: integer;
  Option: TOptionObj;
begin
CB := FControl as TComboBox;
for I := 0 to TheOptions.Count-1 do
  begin
  Option := TOptionObj(TheOptions.Objects[I]);
  if CompareText(V, Option.Value) = 0 then
    begin
    Option.Selected := True;
    CB.ItemIndex := I;
    end;
  end;
end;

procedure TComboFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
EnterIndex := TComboBox(FControl).ItemIndex;
end;

procedure TComboFormControlObj.DoOnChange;
begin
if TComboBox(FControl).ItemIndex <> EnterIndex then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TTextAreaFormControlObj.Create}
constructor TTextAreaFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TPaintPanel;
  I: integer;
  SB: TScrollStyle;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
Rows := 5;
Cols := 30;
Wrap := wrSoft;    
SB := StdCtrls.ssVertical;

for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      RowsSy: Rows := Value;
      ColsSy: Cols := Value;
      WrapSy:
        if (Lowercase(Name) = 'off') then
          begin
          SB := StdCtrls.ssBoth;
          Wrap := wrOff;
          end
        else if (Lowercase(Name) = 'hard') then
          begin
          Wrap := wrHard;
          end;
      end;

PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TMemo.Create(PntPanel);
with TMemo(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Tmp := Prop.GetFont;
  Font.Assign(Tmp);
  Tmp.Free;
  ScrollBars := SB;
  Wordwrap := Wrap in [wrSoft, wrHard];
  OnKeyPress := MyForm.ControlKeyPress;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := FormControlClick;
  end;
FControl.Parent := PntPanel;
end;

destructor TTextAreaFormControlObj.Destroy;
begin
inherited Destroy;
end;

procedure TTextAreaFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I: integer;
begin
with TMemo(FControl) do
  begin
  FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack);
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Font;
  H2 := Canvas.TextHeight('A');
  SetTextAlign(Canvas.handle, TA_Left+TA_Top);
  for I := 0 to IntMin(Lines.Count-1, Rows-1) do
    Canvas.TextRect(Rect(X1+4, Y1+4, X1+Width-8, Y1+Height-8), X1+4,
             Y1+4+I*H2, Lines[I]);
  end;
end;

procedure TTextAreaFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with TMemo(FControl) do
  begin
  Canvas.Font := Font;
  if FHeight >= 10 then
    Height := FHeight
  else ClientHeight := Canvas.TextHeight('A')*Rows + 5;
  if FWidth >= 10 then
    Width := FWidth
  else ClientWidth := Canvas.TextWidth('s')*Cols + 5;
  end;
end;

procedure TTextAreaFormControlObj.AddStr(const S: string);
begin
TheText := TheText+S;   
end;

procedure TTextAreaFormControlObj.ResetToValue;
begin
with (FControl as TMemo) do
  begin
  Text := TheText;
  SelStart := 0;
  SelLength := 0;
  end;
end;

function TTextAreaFormControlObj.GetSubmission(Index: integer;
              var S: string): boolean;
var
  I: integer;
begin
if Index = 0 then
  begin
  Result := True;
  S := FName+'=';
  if Wrap in [wrOff, wrSoft] then
    S := S+(FControl as TMemo).Text  
  else
    with (FControl as TMemo) do
      for I := 0 to Lines.Count-1 do
        begin
        S := S + Lines[I];
        if (I < Lines.Count-1) then
          S := S + ^M^J;
        end;
  end
else Result := False;
end;

procedure TTextAreaFormControlObj.SetData(Index: integer; const V: String);
begin
(FControl as TMemo).Text := V;
end;

procedure TTextAreaFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
EnterContents := TMemo(FControl).Text;
end;

procedure TTextAreaFormControlObj.DoOnChange;
begin
if TMemo(FControl).Text <> EnterContents then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

function TFormControlList.FindControl(Posn: integer): TFormControlObj;
{find the control at a given character position}
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TFormControlObj(Items[I]).Pos = Posn then
    begin
    Result := Items[I];
    Exit;
    end;
Result := Nil;
end;

function TFormControlList.GetHeightAt(Posn: integer;
              var FormAlign: AlignmentType) : Integer;
var
  Ctrl: TFormControlObj;
begin
Ctrl := FindControl(Posn);
if Assigned(Ctrl) then
  begin
  Result := Ctrl.FControl.Height;
  FormAlign := Ctrl.FormAlign;
  end
else Result := -1;
end;

function TFormControlList.GetWidthAt(Posn: integer; var HSpcL, HSpcR: integer) : integer;
var
  Ctrl: TFormControlObj;
begin
Ctrl := FindControl(Posn);
if Assigned(Ctrl) then
  Begin
  Result := Ctrl.FControl.Width;
  HSpcL := Ctrl.HSpaceL;    
  HSpcR := Ctrl.HSpaceR;
  end
else Result := -1;
end;

function TFormControlList.GetControlCountAt(Posn: integer): integer;
{Return count of chars before the next form control.  0 if at the control,
 9999 if no controls after Posn}
var
  I, Pos: integer;
begin
if Count = 0 then
  begin
  Result := 9999;
  Exit;
  end;
I := 0;
while I < count do
  begin
  Pos := TFormControlObj(Items[I]).Pos;
  if Pos >= Posn then break;
  Inc(I);
  end;
if I = Count then Result := 9999
else
  Result := TFormControlObj(Items[I]).Pos - Posn;
end;

procedure TFormControlList.Decrement(N: integer);   
{called when a character is removed to change the Position figure}
var
  I: integer;
begin
for I := 0 to Count-1 do
  with TFormControlObj(Items[I]) do
    if Pos > N then
      Dec(Pos);
end;

end.
