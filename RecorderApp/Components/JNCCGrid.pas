{ TJNCCGrid component and accessory classes
     Provides a DBGrid style descendant of TRTFGrid which adds vague date font
     coloration and required field colours whilst editing.  Uses a new In Place
     Editor to achieve this.  As this is descended from TInPlaceEdit, not
     TDBGridInplaceEdit which is private to TDBGrid, some functionality in the
     editor may be lost.  This does not affect normal cell editing .
     By John van Breda
     08/04/99 }

unit JNCCGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Db, DBGrids, RTFGrid, VagueDateEdit, VagueDate;

type
  TGridMode = (gmReadOnly, gmEditing);

  TGetCellColorEvent = procedure (Sender: TObject; DataCol: Integer; Column: TColumn; State: TGridDrawState) of object;

  { This is a hack to allow exposure of the protected
    OnMouseDown property within this unit }
  TDummyControl = class(TControl)
  end;

  TJNCCInPlaceEdit = class(TInplaceEdit)
  protected
    procedure DoKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create(AOwner : TComponent); override;
  end;

  TDBJNCCGrid = class(TDBRTFGrid)
  private
    FRequiredColour: TColor;
    FGridMode: TGridMode;
    FOnGetCellColor: TGetCellColorEvent;
    procedure SetRequiredcolour(const Value: TColor);
    procedure SetGridMode(const Value: TGridMode);
    procedure SetOnGetCellColor(const Value: TGetCellColorEvent);
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MouseWheel;
  protected
    function CreateEditor: TInplaceEdit; override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
               Column: TColumn; State: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                X, Y: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property RequiredColour : TColor read FRequiredColour
                                     write SetRequiredcolour
                                     default clYellow;
    property GridMode : TGridMode read FGridMode write SetGridMode default gmReadOnly;
    property OnGetCellColor: TGetCellColorEvent read FOnGetCellColor write
        SetOnGetCellColor;
    property OnMouseDown;
    property OnMouseUp;
  end;


implementation

{ TDBJNCCGrid }

{ Overriden function to create the inplace editor for the grid.  The
     replacement editor handles coloration for the vague date fields }
constructor TDBJNCCGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequiredColour := clYellow;
  FGridmode := gmReadOnly;
  ReadOnly := True;
end;


//==============================================================================
function TDBJNCCGrid.CreateEditor: TInplaceEdit;
begin
  Result := TJNCCInPlaceEdit.Create(Self);
end;

{-------------------------------------------------------------------------------
  Set the brush colour for required fields and allow the inherited Draw
     ColumnCell to occur
}
procedure TDBJNCCGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  if (GridMode = gmEditing) and (Column.Field.Required) then
    Canvas.Brush.Color := RequiredColour
  else if gdSelected in State then
    Canvas.Brush.Color := clHighlight
  else if Assigned(OnGetCellColor) then
    OnGetCellColor(Self, DataCol, Column, State)
  else
    Canvas.Brush.Color := Color;
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  // restore defaults
  Canvas.Brush.Color := Color;
end;


//==============================================================================
procedure TDBJNCCGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(TDummyControl(Self).OnMouseDown) then
    TDummyControl(Self).OnMouseDown(Self, Button, Shift, X, Y);
end;


//==============================================================================
procedure TDBJNCCGrid.SetGridMode(const Value: TGridMode);
begin
  FGridMode := Value;
  { Set the grid read-only if applicable }
  ReadOnly := (Value = gmReadOnly);
  Invalidate;
end;


//==============================================================================
procedure TDBJNCCGrid.SetRequiredcolour(const Value: TColor);
begin
  FRequiredColour := Value;
  Invalidate;
end;


{ TJNCCInPlaceEditor }


//==============================================================================
constructor TJNCCInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Assign a handler to check vague text as it is typed }
  OnKeyPress := DoKeyPress;
  Font.Assign(TDBJNCCGrid(AOwner).Font);
end;



//==============================================================================
procedure TJNCCInPlaceEdit.DoKeyPress(Sender: TObject; var Key: Char);
begin
  { Need to pick up correct required colour }
  if (TDBJNCCGrid(Owner).Columns[TDBJNCCGrid(Owner).SelectedIndex].Field.Required)
            and (TDBJNCCGrid(Owner).GridMode = gmEditing) then
    Color := TDBJNCCGrid(Owner).RequiredColour
  else
    Color := clWindow;
  {Is it a vague date field we are editing?}
  if IsVagueDateField(TDBJNCCGrid(Owner).Columns[TDBJNCCGrid(Owner).
                          SelectedIndex].Field.FieldName) and modified then
  begin
    try
      { If the date is OK - set the font colour - ignore the return result }
      StringToVagueDate(Text + Key);
      Font.Color := clGreen;
    except // if an exception occurs- date is not valid so set font black
      on Exception do
        Font.Color := TDBJNCCGrid(Owner).Font.Color;
    end;
  end
  else
    { not a vague date control so display normally }
    Font.color := TDBJNCCGrid(Owner).Font.Color;
  Invalidate;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TDBJNCCGrid.SetOnGetCellColor(const Value: TGetCellColorEvent);
begin
  FOnGetCellColor := Value;
end;

{-------------------------------------------------------------------------------
  Enable scrolling with the mouse wheel
}
procedure TDBJNCCGrid.WMMouseWheel(var Message: TWMMouseWheel);
 var lDataset : TDataset;
begin
  if DataSource<>nil then
    lDataset := DataSource.Dataset
  else
    lDataset := nil;
  if (lDataset=nil) or not lDataset.Active then
    inherited
  else begin
    if Message.WheelDelta>0 then
      lDataset.Prior
    else
      lDataset.Next;
  end;

end;

end.
