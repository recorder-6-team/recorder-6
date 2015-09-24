//==============================================================================
//  Unit:        MapOptionsGrid
//
//  Implements:  TMapOptionsGrid
//               TMapItemSet
//               TDistributionData
//
//  Description: TMapItemSet
//               Base class for any object that can be stored against the map
//               options grid.
//
//  Author:      Robert Kinsey
//  Created:     1 November 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 2/01/08 10:26 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MapOptionsGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, menus;

type
  EMapOptionsGridError = class(Exception);

  TGridDisplayType = (dtMapLayers, dtDatasets);

  { Base class for any object that can be stored against the map options grid }
  TMapItemSet = class(TObject)
  private
    FVisible: boolean;
    FTitle  : string;
  protected
    procedure SetTitle(const Value: String); virtual;
    procedure SetVisible(const Value: boolean); virtual;
  public
    property Visible: boolean read FVisible write SetVisible;
    property Title: String read FTitle write SetTitle;
  end;

  TDistributionData = class(TMapItemSet)
  private
    FKeys        : TStringList;
    FTable       : String;
    FDatasetList : TList;
    FColour: TColor;
    procedure SetColour(const Value: TColor);
  protected
    procedure SetDatasetList(const Value: TList);
    procedure SetTable(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Keys : TStringList read FKeys;
    property Table : String read FTable write SetTable;
    property DatasetList: TList read FDatasetList;
    property Colour : TColor read FColour write SetColour;
  end;

  TRowChangedEvent     = procedure(Sender: TObject; const ARow: integer) of Object;
  TPopUpMenuBuiltEvent = procedure(Sender: TObject; var APopUpMenu: TPopupMenu) of Object;

  TMapOptionsGrid = class(TStringGrid)
  private
    { Private declarations }
    FDisplayType        : TGridDisplayType;
    FOnChange           : TRowChangedEvent;
    FItemsList          : TList;
    FdlgColor           : TColorDialog;
    FeDatasetName       : TEdit;
    FCurrentDataset     : Integer;
    FCurrentCol         : Integer;

    { Popup MenuItems }
    FpmMain             : TPopupMenu;
    FmuRenameRow        : TMenuItem;
    FmuRowProperties    : TMenuItem;
    FmuAddBaseMapRow    : TMenuItem;
    FmuAddPolygonRow    : TMenuItem;

    FOnRowChanged       : TRowChangedEvent;
    FOnAddItemsToPopup  : TPopUpMenuBuiltEvent;
    { For event handlers in response to map layer manipulations }
    FOnSetRowProperties : TRowChangedEvent;
    FOnRenameLayer      : TRowChangedEvent;
    FCanAddBaseSheet    : Boolean;
    FImages: TImageList;

    procedure SetOnChange(const Value: TRowChangedEvent);
    procedure DrawCheckBox(const ACanvas: TCanvas; const xPos,
      yPos: integer; const DrawTick: boolean);
    function GetColumnClicked(out oRow, oCol: integer; const iXMousePos,
      iYMousePos: integer): boolean;
    procedure SetCurrentDataset(const Value: integer);
    procedure eDataSetNameExit(Sender: TObject);
    procedure eDataSetNameKeyDown(Sender: TObject; var Key: Word;
              Shift: TShiftState);
    procedure eDatasetNameEnter(Sender: TObject);
    procedure SetCurrentCol(const Value: integer);
    procedure SetOnRowChanged(const Value: TRowChangedEvent);
    procedure ShowEdit;
    procedure SetCanAddBaseSheet(const Value: Boolean);
    procedure SetDisplayType(const Value: TGridDisplayType);
    procedure SetItemsList(const Value: TList);
    procedure SetImages(const Value: TImageList);
  protected
    procedure DrawCell(ACol : Integer; ARow : Integer; ARect : TRect; AState : TGridDrawState); override;
(*    procedure Paint; override;*)
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure KeyPress(var Key: Char); override;
    procedure BuildPopupMenu; virtual;
    procedure RenameSelectedRow(Sender: TObject); virtual;
    procedure SetSelectedRowProperties(Sender: TObject); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    property CurrentDataset : integer read FCurrentDataset write SetCurrentDataset;
    property CurrentCol : integer read FCurrentCol write SetCurrentCol;
    property CanAddBaseSheet: boolean read FCanAddBaseSheet write SetCanAddBaseSheet;
  published
    { Properties }
    property GridType : TGridDisplayType read FDisplayType write SetDisplayType;
    property ItemsList : TList read FItemsList write SetItemsList;
    property Images : TImageList read FImages write SetImages;
    { Event Handlers }
    property OnChange : TRowChangedEvent read FOnChange write SetOnChange;
    property OnRowChanged : TRowChangedEvent read FOnRowChanged write SetOnRowChanged;
    property OnAddItemsToPopup : TPopUpMenuBuiltEvent read FOnAddItemsToPopup write FOnAddItemsToPopup;
    property OnSetRowProperties : TRowChangedEvent read FOnSetRowProperties write FOnSetRowProperties;
    property OnRenameRow : TRowChangedEvent read FOnRenameLayer write FOnRenameLayer;
  end;

procedure Register;

//==============================================================================
implementation

resourcestring
  ResStr_MenuItemRenameCaption          = 'Rename Layer';
  ResStr_MenuItemRenameHint             = 'Change the name for the selected layer';
  ResStr_MenuItemPropertiesCaption      = 'Selected Layer &Properties';
  ResStr_MenuItemPropertiesHint         = 'Change the properties for selected map layer.';
  ResStr_MenuItemAddBasemapRowCaption   = 'Add Base Map Layer';
  ResStr_MenuItemAddBasemapRowHint      = 'Add a new base map layer to the map';
  ResStr_MenuItemAddPolygonRowCaption   = 'Add Polygon Layer';
  ResStr_MenuItemAddPolygonRowHint      = 'Add a new polygon layer to the map';

  ResStr_CannotBuildPopupMenu = 'Unable to build popup menu. Error message produced: ';
  ResStr_CannotAccessPopupMenu = 'Unable to access popup menu';

procedure Register;
begin
  RegisterComponents('JNCC', [TMapOptionsGrid]);
end;

//==============================================================================
//==============================================================================
{ TMapOptionsGrid }
//==============================================================================
constructor TMapOptionsGrid.Create(AOwner: TComponent);
begin
  inherited;

  { Setup basic draw state }
  FixedCols := 0;
  FixedRows := 0;
  RowCount  := 10;
  ColCount  := 3;
  Options := Options - [goHorzLine, goVertLine, goRangeSelect];
  If not (goRowSelect in Options) then
    Options := Options + [goRowSelect];
  ColWidths[0] := 19;
  ColWidths[1] := 20;
  GridLineWidth:=0;
  FItemsList := nil;
  //Enabled := False;

  FeDatasetName:=TEdit.Create(AOwner);
  with FeDatasetName do begin
    Parent:=Self;// will be giving it dimensions relative
                 // to the string grid
    Visible:=true;
    Text:='';
    BorderStyle := bsNone;
    OnKeyDown   := eDatasetNameKeyDown;
    OnExit      := eDatasetNameExit;
    OnEnter     := eDatasetNameEnter;
    SetBounds(0, 0, 0, 0);
  end;
  FdlgColor:=TColorDialog.Create(Self);
  CurrentCol:=-1;
  CurrentDataset:=-1;

  // Create the popup menu
  BuildPopupMenu;

end;  // Create

//==============================================================================
procedure TMapOptionsGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  x,y:integer;
begin
  inherited;
  if Assigned(FItemsList) then
  begin
    Canvas.Font.Color := clWindowText;
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(ARect);
    if (FItemsList.Count>0) and (ARow<FItemsList.Count) then
    begin
      if (ARow = CurrentDataset) and (ACol = CurrentCol) then
        Canvas.DrawFocusRect( Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom) );
      case ACol of
      0:
        begin
          x := ARect.Left+(ColWidths[0]-13) div 2;
          y := ARect.Top+(RowHeights[ARow]-13) div 2;
          DrawCheckBox(Canvas, x, y, TMapItemSet(FItemsList[ARow]).Visible);
        end; // if (ACol=0) and (ARow>0)
      1:
        begin
          Canvas.Brush.Color := TdistributionData(FItemsList[ARow]).Colour;
          if TObject(FItemsList[ARow]) is TDistributionData then begin
            Canvas.Brush.Color := TDistributionData(FItemsList[ARow]).Colour;
            Canvas.Brush.Style := bsSolid;
            Canvas.Pen.Color := clWindow;
            Canvas.Ellipse(ARect.Left + 2, ARect.Top+4, ARect.Left + 17, ARect.Top+19);
          end;
          Canvas.Brush.Style := bsClear;
        end;
      2:
        begin
          Canvas.TextOut( ARect.Left + 2, ARect.Top + 4, TMapItemSet(FItemsList[ARow]).Title );
        end;
      end; // case
    end; // if (FItemsList.Count>0) and (ARow<FItemsList.Count)
  end; // if assigned(FItemsList)
end;  // DrawCell


//==============================================================================
// Draws a 3D Checkbox on ACanvas at the xPos,yPos position.
// DrawTick indicates if the box is checked or not
procedure TMapOptionsGrid.DrawCheckBox(const ACanvas:TCanvas; const xPos,yPos:integer;
                       const DrawTick:boolean);
var lBColor,lPColor:TColor;
begin
  with ACanvas do begin
    // Save Brush and Pen colours
    lBColor:=Brush.Color;
    lPColor:=Pen.Color;

    Brush.Color:=clWindow;  // Use Windows colours
    Pen.Style  :=psSolid;
    Pen.Color  :=clBtnHighlight;  Rectangle(xPos,yPos,xPos+13,yPos+13);
    Pen.Color  :=clBtnFace;       Rectangle(xPos,yPos,xPos+12,yPos+12);
    Pen.Color  :=clBtnShadow;     MoveTo(xPos,yPos+11);    LineTo(xPos,yPos);
                                                           LineTo(xPos+12,yPos);
    Pen.Color  :=clBlack;         MoveTo(xPos+1,yPos+10);  LineTo(xPos+ 1,yPos+ 1);
                                                           LineTo(xPos+11,yPos+1);

    if DrawTick then begin  // Use black colour to draw tick
      Pen.Color:=clBlack;
      MoveTo(xPos+3,yPos+5);  LineTo(xPos+5,yPos+7);  LineTo(xPos+10,yPos+2);
      MoveTo(xPos+3,yPos+6);  LineTo(xPos+5,yPos+8);  LineTo(xPos+10,yPos+3);
      MoveTo(xPos+3,yPos+7);  LineTo(xPos+5,yPos+9);  LineTo(xPos+10,yPos+4);
    end;
    // Restore Brush and Pen colours
    Brush.Color:=lBColor;
    Pen.color  :=lPColor;
  end;
end;  // DrawCheckBox

//==============================================================================
procedure TMapOptionsGrid.SetItemsList(const Value: TList);
begin
  FItemsList := Value;
  Enabled := Assigned(FItemsList);
end;

//==============================================================================
procedure TMapOptionsGrid.SetOnChange(const Value: TRowChangedEvent);
begin
  FOnChange := Value;
end;

//==============================================================================
//==============================================================================
{ TDistributionData }
//==============================================================================
constructor TDistributionData.Create;
begin
  inherited;
  Visible:=true;
  FKeys:=TStringList.Create;
  FDatasetList := TList.Create; // start with an empty list
end;

//==============================================================================
destructor TDistributionData.Destroy;
var
  i : integer;
begin
  FKeys.Free;
  for i := 0 to FDatasetList.Count-1 do
    TObject(FDatasetList[i]).Free;
  FDatasetList.Free;
  inherited;
end;

function TMapOptionsGrid.GetColumnClicked(out oRow, oCol: integer;
  const iXMousePos, iYMousePos: integer): boolean;
var
  lPoint : TPoint;
  lWidth, lDepth : integer;
begin
  { Get the x and y pixel coordinates of the cursor for the window }
  lPoint.x := ixMousePos ;
  lPoint.y := iyMousePos ;
  oCol:=-1;
  oRow:=-1;
  lWidth:=0;
  lDepth:=0;
  Result:=False;
  While (lPoint.x>lWidth) and (oCol<ColCount-1) do
  begin
    inc(oCol);
  //find out if click is on a grid line
    if (lPoint.x>=lWidth) and (lPoint.x<(lWidth+ColWidths[oCol])) then
      Result:=True;
    lWidth := lWidth+ColWidths[oCol]+GridLineWidth;
  end;//while lPoint.x;
  if lPoint.Y<0 then
    Result:=false
  else
    While (lPoint.y>lDepth) and (oRow<RowCount-1) do
    begin
      inc(oRow);
    //find out if click is on a grid line
      if (lPoint.y>=lDepth) and (lPoint.y<lDepth+RowHeights[Row]) then
        Result:=True and Result;
      lDepth := lDepth+RowHeights[oRow]+GridLineWidth;
    end;//while lPoint.y
end;//getColumnClicked

//==============================================================================
procedure TDistributionData.SetColour(const Value: TColor);
begin
  FColour := Value;
end;

//==============================================================================
procedure TDistributionData.SetDatasetList(const Value: TList);
begin
  FDatasetList := Value;
end;


//==============================================================================
procedure TDistributionData.SetTable(const Value: String);
begin
  FTable := Value;
end;

//==============================================================================
procedure TMapOptionsGrid.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{
var
  lRow,lCol : integer;
  lValidCol : boolean;
  lEditRect : TRect;
}
begin
  inherited;
(*  if FItemsList.Count>0 then
  begin
    lValidCol:=GetColumnClicked(lRow,lCol,X,Y);
    lEditRect := CellRect(2,lRow);
    if lValidCol and (lRow<FItemsList.Count)then
    begin
      CurrentDataset:=lRow;
      CurrentCol:=lCol;
      Case lCol of
        0: {change the visibility checkbox}
          begin
            TDistributionData(FItemsList[lRow]).Visible:=not TDistributionData(FItemsList[lRow]).Visible;
            If Assigned(OnChange) then OnChange(Self, lRow);
          end;//case lcol=0
        2:
          ShowEdit;
      end;//case;
    end;   //if lValidCol
  end;    // if FItemsList.Count>=0;
  //Make sure the row is highlighted and the tick is right
 Invalidate;    *)
end;

{
Name:      SetCurrentDataset

Purpose:   To set the dataset which is up for editing or
           deletion and to put the edit in the right row.

Parameters:  Value: integer

Returns:     none

Side-Effects: position of eDatasetName; the text therein and
              of course FCurrentDataset.

Other:

}

procedure TMapOptionsGrid.SetCurrentDataset(const Value: integer);
begin
  if FCurrentDataset<> value then
  begin
    invalidate;
    If Assigned(FOnRowChanged) then
      FOnRowChanged(Self, Value);
  end;
  FCurrentDataset := Value;
  if Assigned(FItemsList) then
    if (CurrentDataset>=0) and (CurrentDataset<FItemsList.Count) then
      ShowEdit;
end;

{
Name:       eDatasetNameExit

Purpose:    An event hander for exiting FeDatasetName

Parameters: Sender

Returns:    none

Side-Effects: Updates contents of the string grid, makes
              FeDatasetname invisible

Other:

}

procedure TMapOptionsGrid.eDataSetNameExit(Sender: TObject);
begin
  if FeDatasetName.Visible and
    (CurrentDataset>=0) and (CurrentDataset<FItemsList.Count) then
  begin
    TDistributionData(FItemsList[CurrentDataset]).Title:=FeDatasetName.Text;
    SetFocus;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self, CurrentDataset);
  end;
end;

{==============================================================================
Procedure Name:         eDatsetNameKeyDown
Purpose:                To change the currentdataset if
                         enter is pressed. It is an event handler
                         for FeDatsetName
===============================================================================}
procedure TMapOptionsGrid.eDataSetNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=13 then
  begin
    eDatasetNameExit(nil);
    Key:=0;
  end;
end;
{==============================================================================
Procedure Name: DblClick
Purpose:        Opens a colour dialog if second column is clicked
                and changes the colour of the current dataset to the chosen
                colour.
===============================================================================}
procedure TMapOptionsGrid.DblClick;
var
  lRow, lCol: Integer;
  lPoint : TPoint;
  lRect : TRect;
begin
  inherited;
  GetCursorPos(lPoint);
  GetWindowRect(Handle, lRect );
  lPoint.x := lPoint.x - lRect.left;
  lPoint.y := lPoint.y - lRect.top;

  if GetColumnClicked(lRow, lCol, lPoint.X, lPoint.Y) then
    if (lCol=1) and (lRow<FItemsList.Count) then
    begin
      FdlgColor.Color:=TDistributionData(FItemsList[lRow]).Colour;
      //undo the colour changes which are a result of the double click
      Invalidate;
      if FdlgColor.Execute then
      begin
         TDistributionData(FItemsList[lRow]).Colour:=FdlgColor.Color;
         If Assigned(OnChange) then FOnChange(Self, lRow);
      end;
    end;
end;

{==============================================================================
Procedure Name: Invalidate
Purpose:        Inherits invalidate from TString grid and
               also makes sure that Row = CurrentDataset
===============================================================================}
procedure TMapOptionsGrid.Invalidate;
begin
  inherited;
  if Assigned(FItemsList) then
  begin
    if (CurrentDataset>=0) and (CurrentDataset<FItemsList.Count) then
      FeDatasetName.Text:=TDistributionData(FItemsList[CurrentDataset]).Title
    else
      FeDatasetName.Text:='';
  end else
    FeDatasetName.Text:='';
end;  // Invalidate

//This procedure changes the selected row if the keys are up or down
{==============================================================================
Procedure Name:     KeyDown
Purpose:            If the up or down keys are pressed then
                    the current dataset moves up and down.
                    If enter is pressed then the distribution
                    display is edited
===============================================================================}
procedure TMapOptionsGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    37: {left}
      if CurrentCol>0 then
         CurrentCol:=CurrentCol-1;
    38: {up Arrow}
      if (CurrentDataset>0) then
        CurrentDataset:=CurrentDataset-1;
    39: {right}
      if (CurrentCol<2) then
        CurrentCol:=CurrentCol+1;
    40: {down arrow}
      if Assigned(FItemsList) then
         if (CurrentDataset<FItemsList.Count-1) then
          CurrentDataset:=CurrentDataset+1;
    32,13: {enter or space: want to be equivalent to changing the
          entry in the chosen cell}
    begin
      if Assigned(FItemsList) then
         if (CurrentDataset>=0) and (CurrentDataset<FItemsList.Count) then
         begin
           Case CurrentCol of
             0:// visibility checkbox: change visibility
             begin
               TDistributionData(FItemsList[CurrentDataset]).Visible:=not TDistributionData(FItemsList[CurrentDataset]).Visible;
               if Assigned(FOnChange) then
                 FOnChange(Self, CurrentDataset);
             end;
             1://colour: make colour dialog appear
             begin
               FdlgColor.Color:=TDistributionData(FItemsList[CurrentDataset]).Colour;
               if FdlgColor.Execute then
               begin
                 TDistributionData(FItemsList[CurrentDataset]).Colour:=FdlgColor.Color;
                 if Assigned(FOnChange) then
                   FOnChange(Self, CurrentDataset);
               end;
             end;//case 2
             2:// name : focus edit
               FeDatasetName.SetFocus;
           end;// case col
           invalidate;
         end;// if CurrentDataset<>0 etc
    end// case 32 or 13(Space or enter)
  end;// case Key
end;

//==============================================================================
procedure TMapOptionsGrid.eDatasetNameEnter(Sender: TObject);
begin
  CurrentCol:=2;
  with FeDatasetName do
  begin
    selStart:=0;
    selLength:=length(Text);
  end;
end;

(*procedure TMapOptionsGrid.Paint;
var
  LineColor: TColor;
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect: TRect;
  AFocRect, FocRect: TRect;
  PointsList: PIntArray;
  StrokeList: PIntArray;
  MaxStroke: Integer;
  FrameFlags1, FrameFlags2: DWORD;

  procedure DrawLines(DoHorz, DoVert: Boolean; Col, Row: Longint;
    const CellBounds: array of Integer; OnColor, OffColor: TColor);

  { Cellbounds is 4 integers: StartX, StartY, StopX, StopY
    Horizontal lines:  MajorIndex = 0
    Vertical lines:    MajorIndex = 1 }

  const
    FlatPenStyle = PS_Geometric or PS_Solid or PS_EndCap_Flat or PS_Join_Miter;

    procedure DrawAxisLines(const AxisInfo: TGridAxisDrawInfo;
      Cell, MajorIndex: Integer; UseOnColor: Boolean);
    var
      Line: Integer;
      LogBrush: TLOGBRUSH;
      Index: Integer;
      Points: PIntArray;
      StopMajor, StartMinor, StopMinor: Integer;
    begin
      with Canvas, AxisInfo do
      begin
        if EffectiveLineWidth <> 0 then
        begin
          Pen.Width := GridLineWidth;
          if UseOnColor then
            Pen.Color := OnColor
          else
            Pen.Color := OffColor;
          if Pen.Width > 1 then
          begin
            LogBrush.lbStyle := BS_Solid;
            LogBrush.lbColor := Pen.Color;
            LogBrush.lbHatch := 0;
            Pen.Handle := ExtCreatePen(FlatPenStyle, Pen.Width, LogBrush, 0, nil);
          end;
          Points := PointsList;
          Line := CellBounds[MajorIndex] + EffectiveLineWidth shr 1 +
            GetExtent(Cell);
          //!!! ??? Line needs to be incremented for RightToLeftAlignment ???
          if UseRightToLeftAlignment and (MajorIndex = 0) then Inc(Line);
          StartMinor := CellBounds[MajorIndex xor 1];
          StopMinor := CellBounds[2 + (MajorIndex xor 1)];
          StopMajor := CellBounds[2 + MajorIndex] + EffectiveLineWidth;
          Index := 0;
          repeat
            Points^[Index + MajorIndex] := Line;         { MoveTo }
            Points^[Index + (MajorIndex xor 1)] := StartMinor;
            Inc(Index, 2);
            Points^[Index + MajorIndex] := Line;         { LineTo }
            Points^[Index + (MajorIndex xor 1)] := StopMinor;
            Inc(Index, 2);
            Inc(Cell);
            Inc(Line, GetExtent(Cell) + EffectiveLineWidth);
          until Line > StopMajor;
           { 2 integers per point, 2 points per line -> Index div 4 }
          PolyPolyLine(Canvas.Handle, Points^, StrokeList^, Index shr 2);
        end;
      end;
    end;

  begin
    if (CellBounds[0] = CellBounds[2]) or (CellBounds[1] = CellBounds[3]) then Exit;
    if not DoHorz then
    begin
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
    end
    else
    begin
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
    end;
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    Color: TColor; IncludeDrawState: TGridDrawState);
  var
    CurCol, CurRow: Longint;
    AWhere, Where, TempRect: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[CurRow];
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if (Where.Right > Where.Left) and RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col)  then
            Include(DrawState, gdFocused);
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          if not (gdFocused in DrawState) or not (goEditing in Options) or
            not FEditorMode or (csDesigning in ComponentState) then
          begin
            if DefaultDrawing or (csDesigning in ComponentState) then
              with Canvas do
              begin
                Font := Self.Font;
                if (gdSelected in DrawState) and
                  (not (gdFocused in DrawState) or
                  ([goDrawFocusSelected, goRowSelect] * Options <> [])) then
                begin
                  Brush.Color := clHighlight;
                  Font.Color := clHighlightText;
                end
                else
                  Brush.Color := Color;
                FillRect(Where);
              end;
            DrawCell(CurCol, CurRow, Where, DrawState);
            if DefaultDrawing and (gdFixed in DrawState) and Ctl3D and
              ((FrameFlags1 or FrameFlags2) <> 0) then
            begin
              TempRect := Where;
              if (FrameFlags1 and BF_RIGHT) = 0 then
                Inc(TempRect.Right, DrawInfo.Horz.EffectiveLineWidth)
              else if (FrameFlags1 and BF_BOTTOM) = 0 then
                Inc(TempRect.Bottom, DrawInfo.Vert.EffectiveLineWidth);
              DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags1);
              DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags2);
            end;
            if DefaultDrawing and not (csDesigning in ComponentState) and
              (gdFocused in DrawState) and
              ([goEditing, goAlwaysShowEditor] * Options <>
              [goEditing, goAlwaysShowEditor])
              and not (goRowSelect in Options) then
            begin
              if not UseRightToLeftAlignment then
                DrawFocusRect(Canvas.Handle, Where)
              else
              begin
                AWhere := Where;
                AWhere.Left := Where.Right;
                AWhere.Right := Where.Left;
                DrawFocusRect(Canvas.Handle, AWhere);
              end;
            end;
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  if UseRightToLeftAlignment then ChangeGridOrientation(True);

  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if (Horz.EffectiveLineWidth > 0) or (Vert.EffectiveLineWidth > 0) then
    begin
      { Draw the grid line in the four areas (fixed, fixed), (variable, fixed),
        (fixed, variable) and (variable, variable) }
      LineColor := clSilver;
      MaxStroke := Max(Horz.LastFullVisibleCell - LeftCol + FixedCols,
                        Vert.LastFullVisibleCell - TopRow + FixedRows) + 3;
      PointsList := StackAlloc(MaxStroke * sizeof(TPoint) * 2);
      StrokeList := StackAlloc(MaxStroke * sizeof(Integer));
      FillDWord(StrokeList^, MaxStroke, 2);

      if ColorToRGB(Color) = clSilver then LineColor := clGray;
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, 0, [0, 0, Horz.FixedBoundary, Vert.FixedBoundary], clBlack, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        LeftCol, 0, [Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary], clBlack, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, TopRow, [0, Vert.FixedBoundary, Horz.FixedBoundary,
        Vert.GridBoundary], clBlack, FixedColor);
      DrawLines(goHorzLine in Options, goVertLine in Options, LeftCol,
        TopRow, [Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary,
        Vert.GridBoundary], LineColor, Color);

      StackFree(StrokeList);
      StackFree(PointsList);
    end;

    { Draw the cells in the four areas }
    Sel := Selection;
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then
    begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then
    begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, FixedColor,
      [gdFixed]);
    DrawCells(LeftCol, 0, Horz.FixedBoundary - FColOffset, 0, Horz.GridBoundary,  //!! clip
      Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary - FColOffset,                   //!! clip
      Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, Color, []);

    if not (csDesigning in ComponentState) and
      (goRowSelect in Options) and DefaultDrawing and Focused then
    begin
      GridRectToScreenRect(GetSelection, FocRect, False);
      if not UseRightToLeftAlignment then
        Canvas.DrawFocusRect(FocRect)
      else
      begin
        AFocRect := FocRect;
        AFocRect.Left := FocRect.Right;
        AFocRect.Right := FocRect.Left;
        DrawFocusRect(Canvas.Handle, AFocRect);
      end;
    end;

    { Fill in area not occupied by cells }
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary));
    end;
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
    end;
  end;

  if UseRightToLeftAlignment then ChangeGridOrientation(False);
end;*)

procedure TMapOptionsGrid.SetCurrentCol(const Value: integer);
begin
  if FCurrentCol<> value then
  begin
    FCurrentCol := Value;
    invalidate;
  end;
  if Assigned(FItemsList) then
  if (CurrentDataset>=0) and (CurrentDataset<FItemsList.Count) then
    ShowEdit;
end;

procedure TMapOptionsGrid.SetOnRowChanged(const Value: TRowChangedEvent);
begin
  FOnRowChanged := Value;
end;

procedure TMapOptionsGrid.KeyPress(var Key: Char);
begin
  if CurrentCol=2 then
    if not(key in [Char(0),Char(13),Char(32)]) then // these have already been dealt with in keydown
      With FeDatasetName do
      begin
        SetFocus;
        Text:=Key;
        selStart:=2;
      end;
end;

//==============================================================================
procedure TMapOptionsGrid.ShowEdit;
var
  lEditRect: TRect;
begin
  lEditRect:=CellRect(2, CurrentDataset);
  with FeDatasetName do begin
    SetBounds( lEditRect.Left + 2, lEditRect.Top + 4,
               (lEditRect.Right - lEditRect.Left - 4),
               lEditRect.Bottom - lEditRect.Top - 5 );
    Text    := TDistributionData(FItemsList[CurrentDataset]).Title;
    Visible := True;
  end; // with
end;  // ShowEdit

//==============================================================================
{ Creates a simple generic popup menu for the component. Also fires off a
  event handler, allowing applications to add in their own menu items,
  or what have you }
procedure TMapOptionsGrid.BuildPopupMenu;
var lCount : integer;
begin
  { Build a popup menu for the map options grid }
  try

    { Create the popup menu }
    FpmMain := TPopUpMenu.Create(Self);
    FpmMain.AutoPopUp := true;

    { Set up menu item for renaming rows }
    FmuRenameRow := TMenuItem.Create(Self);
    with FmuRenameRow do begin
      Caption := ResStr_MenuItemRenameCaption;
      Hint    := ResStr_MenuItemRenameHint;
  //    FmuRenameRow.OnClick := RenameSelectedRow'
    end;  // with FmuRenameRow
    FpmMain.Items.Add(FmuRenameRow);

    { Set up menu item for setting row properties }
    FmuRowProperties := TMenuItem.Create(Self);
    with FmuRowProperties do begin
      Caption := STR_MENU_ITEM_PROPERTIES_CAPTION;
      Hint    := STR_MENU_ITEM_PROPERTIES_HINT;
 //     FmuRowProperties.OnClick := SetSelectedRowProperties;
    end; // with FmuRowProperties
    FpmMain.Items.Add(FmuRowProperties);

    if (FDisplayType = dtMapLayers) then begin

      { Set up menu item for adding new polgyon layers }
      FmuAddPolygonRow := TMenuItem.Create(Self);
      with FmuAddPolygonRow do begin
        Caption := ResStr_MenuItemAddPolygonRowCaption;
        Hint    := ResStr_MenuItemAddPolygonRowHint;
      end; // with
      FpmMain.Items.Add(FmuAddPolygonRow);

      if FCanAddBaseSheet then begin
        { Set up menu item for adding new base map layers }
        FmuAddBaseMapRow := TMenuItem.Create(Self);
        with FmuAddBaseMapRow do begin
          Caption := ResStr_MenuItemAddBasemapRowCaption;
          Hint    := ResStr_MenuItemAddBasemapRowHint;
        end; // with FmuAddBaseMapRow
        FpmMain.Items.Add(FmuAddBaseMapRow); // Add this row to the popup
      end; // if FCanAddBaseMap

    end; // if FDisplayType = dtMapLayers

    { Set the MapOptionsGrid's popup menu to be that just created }
    Self.PopUpMenu := FpmMain;

  except
    on E:Exception do begin
      { Free absolutely everything and notify of error }
      FmuRenameRow.Free;
      FmuRowProperties.Free;
      FmuAddBaseMapRow.Free;
      FmuAddPolygonRow.Free;
      FpmMain.Free;
      raise EMapOptionsGridError.Create(ResStr_CannotBuildPopupMenu +#13#10+ E.Message );
    end; // on exception
  end; // try..except

  { Notify that we have created the Popup Menu,
    hence allowing applications to add in custom items }
  if Assigned(FOnAddItemsToPopup) then
  try
    FOnAddItemsToPopup(Self, FpmMain);
    // if they've done something drastic
    if not Assigned(FpmMain) then
      EMapOptionsGridError.Create(ResStr_CannotAccessPopupMenu);
  except
    on E:Exception do begin
      if Assigned(FpmMain) then begin
        for lCount := FpmMain.Items.Count-1 downto 0 do
          if Assigned(FpmMain.Items[lCount]) then
            FpmMain.Items[lCount].Free;
      end else
        raise;
    end; // on Exception
  end; // try..except
end; // BuildPopupMenu

//==============================================================================
procedure TMapOptionsGrid.RenameSelectedRow(Sender: TObject);
begin

end;

procedure TMapOptionsGrid.SetSelectedRowProperties(Sender: TObject);
begin

end;

//==============================================================================
destructor TMapOptionsGrid.Destroy;
var
  lCount: integer;
begin
  FeDatasetName.Free;

  { Free popup menu and associated menu items }
  if FpmMain <> nil then begin
    for lCount := FpmMain.Items.Count-1 downto 0 do
      if FpmMain.Items[lCount]<>nil then
        TMenuItem(FpmMain.Items[lCount]).Free;
    FpmMain.Free;
  end; // if FpmMain<>nil

  inherited Destroy;
end; // Destroy

//==============================================================================
procedure TMapOptionsGrid.SetCanAddBaseSheet(const Value: Boolean);
begin
  FCanAddBaseSheet := Value;
end; // SetCanAddBaseSheet

//==============================================================================
procedure TMapOptionsGrid.SetDisplayType(const Value: TGridDisplayType);
begin
  FDisplayType := Value;
end;  // SetDisplayType

//==============================================================================
procedure TMapOptionsGrid.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

//==============================================================================
//==============================================================================
{ TMapItemSet }
//==============================================================================
procedure TMapItemSet.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

//==============================================================================
procedure TMapItemSet.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

end.
