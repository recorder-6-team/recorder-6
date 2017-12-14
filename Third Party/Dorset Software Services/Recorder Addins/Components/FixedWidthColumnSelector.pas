{===============================================================================
  Unit:        FixedWidthColumnSelector.pas

  Defines:     TFixedWidthColumnSelector

  Description: Control allowing user to specify column positions within text.

  Model:       -

  Created:     August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 7/09/04 16:20 $
    $Author: Andrewkemp $

===============================================================================}
unit FixedWidthColumnSelector;

interface

uses
  Controls, Classes, StdCtrls, Windows;

resourcestring
  ResStr_InvalidBreakPosition = '%0:s: Invalid break position (%1:d)';
  ResStr_NoSuchBreak = '%0:s: No such break (%1:d)';

type
  TFixedWidthColumnSelector = class(TCustomControl)
  protected
    FBreaks: TList;
    FChanged: Boolean;
    FCurrentBreakIndex: Integer;
    FHorizontalScroll: TScrollBar;
    FMaxLineLength: Integer;
    FMouseDown: Boolean;
    FOnChange: TNotifyEvent;
    FScrollPosX: Integer;
    FScrollPosY: Integer;
    FSorted: Boolean;
    FText: TStringList;
    FVerticalScroll: TScrollBar;
    function BreakIndex(Position: Integer): Integer; virtual;
    function CharacterPosition(X: Integer): Integer; virtual;
    procedure CreateWnd; override;
    procedure CreateScrollBars; virtual;
    procedure DoChange; virtual;
    function GetBreakCount: Integer; virtual;
    function GetBreaks(Index: Integer): Integer; virtual;
    procedure HorizontalScroll(Sender: TObject; ScrollCode: TScrollCode; var
        ScrollPos: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); 
        override;
    procedure MoveBreak(Index: Integer; Position: Integer); virtual;
    procedure Paint; override;
    function TextRectangle: TRect; virtual;
    procedure VerticalScroll(Sender: TObject; ScrollCode: TScrollCode; var
        ScrollPos: Integer); virtual;
    function VisibleColumns: Integer; virtual;
    function VisibleRows: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddBreak(Position: Integer); virtual;
    function BreakExists(Position: Integer): Boolean; virtual;
    procedure DeleteBreak(Position: Integer); virtual;
    procedure LoadText(Source: TStrings); virtual;
    function ValidBreakPosition(Position: Integer): Boolean; virtual;
    property BreakCount: Integer read GetBreakCount;
    property Breaks[Index: Integer]: Integer read GetBreaks;
    property LineLength: Integer read FMaxLineLength;
  published
    property Anchors;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;




implementation

uses
  SysUtils, Forms, Graphics, Math;

const
  SCROLL_BAR_SIZE = 14;
  RULER_SIZE = 29;

function CompareBreaks(B1, B2: Pointer): Integer;
begin 
  Result := Integer(B1) - Integer(B2);
end;
  
{-------------------------------------------------------------------------------
}
constructor TFixedWidthColumnSelector.Create(AOwner: TComponent);
begin
  inherited;
  FBreaks := TList.Create;
  FText := TStringList.Create;
  FCurrentBreakIndex := -1;
  Canvas.Font.Name := 'Courier';
  Canvas.Font.Height := -13;
end;  // TFixedWidthColumnSelector.Create 

{-------------------------------------------------------------------------------
}
destructor TFixedWidthColumnSelector.Destroy;
begin
  FText.Free;
  FBreaks.Free;
  inherited;
end;  // TFixedWidthColumnSelector.Destroy

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.AddBreak(Position: Integer);
begin
  Assert(
      ValidBreakPosition(Position),
      Format(ResStr_InvalidBreakPosition, ['AddBreak', Position]));
      
  FBreaks.Add(Pointer(Position));
  FSorted := False;
end;  // TFixedWidthColumnSelector.AddBreak 

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.BreakExists(Position: Integer): Boolean;
begin
  Result := BreakIndex(Position) <> -1;
end;  // TFixedWidthColumnSelector.BreakExists 

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.BreakIndex(Position: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to BreakCount - 1 do
    if Breaks[I] = Position then
    begin
      Result := I;
      Break
    end;
end;  // TFixedWidthColumnSelector.BreakIndex

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.CharacterPosition(X: Integer): Integer;
begin
  Result := FScrollPosX + (X div Canvas.TextWidth('W'));
end;  // TFixedWidthColumnSelector.CharacterPosition 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.CreateWnd;
begin
  inherited;
  CreateScrollBars;
end;  // TFixedWidthColumnSelector.CreateWnd;

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.CreateScrollBars;

  function CreateScrollBar(Kind: TScrollBarKind): TScrollBar;
  begin
    Result := TScrollBar.Create(Self);
    Result.Parent := Self;
    Result.Kind := Kind;
    Result.TabStop := False;
  end;
  
begin
  FHorizontalScroll := CreateScrollBar(sbHorizontal);
  with FHorizontalScroll do
  begin
    Left := 1;
    Top := Self.ClientHeight - SCROLL_BAR_SIZE;
    Width := Self.ClientWidth - SCROLL_BAR_SIZE - 1;
    Height := SCROLL_BAR_SIZE;
    Anchors := [akLeft, akRight, akBottom];
    OnScroll := HorizontalScroll;
  end;
  
  FVerticalScroll := CreateScrollBar(sbVertical);
  with FVerticalScroll do
  begin
    Left := Self.ClientWidth - SCROLL_BAR_SIZE;
    Top := RULER_SIZE;
    Width := SCROLL_BAR_SIZE;
    Height := Self.ClientHeight - RULER_SIZE - SCROLL_BAR_SIZE;
    Anchors := [akTop, akRight, akBottom];
    OnScroll := VerticalScroll;
  end;
end;  // TFixedWidthColumnSelector.CreateScrollBars

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.DeleteBreak(Position: Integer);
begin
  Assert(
      BreakExists(Position),
      Format(ResStr_NoSuchBreak, ['DeleteBreak', Position]));
  
  if (FCurrentBreakIndex <> -1) 
      and (Breaks[FCurrentBreakIndex] = Position) then
    FCurrentBreakIndex := -1;
    
  FBreaks.Delete(BreakIndex(Position));
end;  // TFixedWidthColumnSelector.DeleteBreak 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.DoChange;
begin
  if Assigned(OnChange) then OnChange(Self);
end;  // TFixedWidthColumnSelector.DoChange 

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.GetBreakCount: Integer;
begin
  Result := FBreaks.Count;
end;  // TFixedWidthColumnSelector.GetBreakCount 

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.GetBreaks(Index: Integer): Integer;
begin
  if not FSorted then
  begin
    FBreaks.Sort(CompareBreaks);
    FSorted := True;
  end;
  Result := Integer(FBreaks[Index]);
end;  // TFixedWidthColumnSelector.GetBreaks 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.HorizontalScroll(Sender: TObject;
    ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ScrollPos := Min(ScrollPos, Max(0, FMaxLineLength - VisibleColumns));

  if ScrollPos <> FScrollPosX then
  begin
    FScrollPosX := ScrollPos;
    Repaint;
  end;
end;  // TFixedWidthColumnSelector.HorizontalScroll

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.LoadText(Source: TStrings);
var
  I: Integer;
begin
  FText.Assign(Source);

  FMaxLineLength := 0;
  for I := 0 to FText.Count - 1 do 
    if FMaxLineLength < Length(FText[I]) then 
      FMaxLineLength := Length(FText[I]);

  FBreaks.Clear;        
  FCurrentBreakIndex := -1;
end;  // TFixedWidthColumnSelector.LoadText 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.MoveBreak(Index: Integer; Position:
    Integer);
begin
  Assert(
      ValidBreakPosition(Position), 
      Format(ResStr_InvalidBreakPosition, ['MoveBreak', Position]));
      
  FBreaks[Index] := Pointer(Position);
  FSorted := False;
end;  // TFixedWidthColumnSelector.MoveBreak 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.MouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  lPos: Integer;
begin
  if X < ClientWidth - SCROLL_BAR_SIZE then
  begin
    lPos := CharacterPosition(X);
    FCurrentBreakIndex := BreakIndex(lPos);
    FMouseDown := True;
  
    if ssDouble in Shift then 
    begin
      if FCurrentBreakIndex <> -1 then 
      begin
        DeleteBreak(lPos);
        FChanged := True;
        Repaint;
      end
    end 
    else if ValidBreakPosition(lPos) then
      begin
        AddBreak(lPos);
        FChanged := True;
        Repaint;
      end;
  end;
  inherited;
end;  // TFixedWidthColumnSelector.MouseDown 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.MouseMove(Shift: TShiftState; X, Y:
    Integer);
var
  lPos: Integer;
begin
  if FMouseDown 
      and (FCurrentBreakIndex <> -1) 
      and (X > 0)
      and (X < ClientWidth - SCROLL_BAR_SIZE) then
  begin
    lPos := CharacterPosition(X);    
  
    if ValidBreakPosition(lPos) then
    begin
      MoveBreak(FCurrentBreakIndex, lPos);
      FChanged := True;
      Repaint;
    end;
  end;
  inherited;
end;  // TFixedWidthColumnSelector.MouseMove 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.MouseUp(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
  if FChanged then
  begin
    FChanged := False;
    DoChange;
  end;
  inherited;
end;  // TFixedWidthColumnSelector.MouseUp 

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.Paint;
var
  lRect: TRect;
  i, j, k, lCharWidth, lCharHeight, lSampleWidth, lSampleHeight: Integer;
  lBreak: Integer;
  lString: String;
begin
  inherited;
  with Canvas do
  begin
    lCharWidth := TextWidth('W');
    lCharHeight := TextHeight('Wg');
  
    lRect := TextRectangle;
    Brush.Color := clBlack;
    FrameRect(lRect);
  
    InflateRect(lRect, -1, -1);  
    
    lSampleWidth := VisibleColumns;
    FHorizontalScroll.Max := Max(0, FMaxLineLength - 1);
    FHorizontalScroll.PageSize := Min(lSampleWidth, FHorizontalScroll.Max);
    FHorizontalScroll.LargeChange := Max(FHorizontalScroll.PageSize, 1);
    FHorizontalScroll.Enabled := FMaxLineLength > lSampleWidth;
    
    lSampleHeight := VisibleRows;
    FVerticalScroll.Max := Max(0, FText.Count - 1);
    FVerticalScroll.PageSize := Min(lSampleHeight, FVerticalScroll.Max);
    FVerticalScroll.LargeChange := Max(FVerticalScroll.PageSize, 1);
    FVerticalScroll.Enabled := FText.Count > lSampleHeight;
            
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    FillRect(lRect);

    {draw a text sample}
    for i := 0 to lSampleHeight - 1 do
    begin
      if i + FScrollPosY < FText.Count then
        lString := Copy(
            FText[i + FScrollPosY], 
            FScrollPosX + 1, 
            lSampleWidth + 1)
      else
        lString := '';
      TextOut(lRect.Left, lRect.Top + i * lCharHeight, lString);
    end;

    {draw the ruler}
    Brush.Color := Color;
    MoveTo(lRect.Left, lRect.Top - 8);
    LineTo(lRect.Right, lRect.Top - 8);
    for i := 0 to lSampleWidth do
    begin
      j := i * lCharWidth;
      k := i + FScrollPosX;
      if k mod 5 = 0 then
      begin
        MoveTo(j, lRect.Top - 13);
        LineTo(j, lRect.Top - 8);

        if (k <> 0) and (k mod 10 = 0) then begin
          lString := IntToStr(k);
          k := TextWidth(lString);
          TextOut(j - k div 2, lRect.Top - lCharHeight - 15, lString);
        end
      end else begin
        MoveTo(j, lRect.Top - 10);
        LineTo(j, lRect.Top - 8);
      end
    end;

    {draw the arrows for breaks}
    for i := 0 to BreakCount - 1 do
    begin
      lBreak := Breaks[i];
      if (FScrollPosX <= lBreak) and
         (lBreak < FScrollPosX + lSampleWidth + 1) then
      begin
        j := ((lBreak - FScrollPosX) mod (lSampleWidth + 1))
            * lCharWidth;
        if FMouseDown and (i = FCurrentBreakIndex) then
          Pen.Style := psDot
        else
          Pen.Style := psSolid;

        {arrow above}
        Brush.Color := clRed;
        Pen.Color := clRed;
        Polygon([Point(j, lRect.Top - 7),
                 Point(j - 3, lRect.Top - 3),
                 Point(j + 3, lRect.Top - 3)]);
        {line}
        MoveTo(j, lRect.Top - 3);
        LineTo(j, lRect.Bottom);
  
        Pen.Style := psSolid;
      end;
    end;
  end;
end;  // TFixedWidthColumnSelector.Paint

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.TextRectangle: TRect;
begin
  Result := ClientRect;
  Result.Top := Result.Top + Canvas.TextHeight('Wg') + 15;
  Result.Right := Result.Right - SCROLL_BAR_SIZE;
  Result.Bottom := Result.Bottom - SCROLL_BAR_SIZE;
  if not Ctl3D then
    InflateRect(Result, -1, -1);
end;  // TFixedWidthColumnSelector.TextRectangle 

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.ValidBreakPosition(Position: Integer):
    Boolean;
begin
  Result := (Position > 0) 
      and (Position <= FMaxLineLength - 1) 
      and not BreakExists(Position);
end;  // TFixedWidthColumnSelector.ValidBreakPosition

{-------------------------------------------------------------------------------
}
procedure TFixedWidthColumnSelector.VerticalScroll(Sender: TObject;
    ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ScrollPos := Min(ScrollPos, Max(0, FText.Count - VisibleRows));

  if ScrollPos <> FScrollPosY then
  begin
    FScrollPosY := ScrollPos;
    Repaint;
  end;
end;  // TFixedWidthColumnSelector.VerticalScroll

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.VisibleColumns: Integer;
begin
  with TextRectangle do
    Result := (Right - Left) div Canvas.TextWidth('W');
end;  // TFixedWidthColumnSelector.VisibleColumns

{-------------------------------------------------------------------------------
}
function TFixedWidthColumnSelector.VisibleRows: Integer;
begin
  with TextRectangle do
    Result := (Bottom - Top) div Canvas.TextHeight('Wg');
end;  // TFixedWidthColumnSelector.VisibleRows

end.
