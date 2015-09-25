unit CompositeComponent;

interface

uses
  Windows, Messages, Controls, ExtCtrls, Graphics, Classes, Constants;

type
  TCompositeComponent = class(TWinControl)
  private
    FSourceCol : TColor;
    FDestCol : TColor;
    FEditMode: TEditMode;
    procedure SetDestCol(const Value: TColor);
    procedure SetSourceCol(const Value: TColor);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentCtl3DChanged(var Message: TMessage); message CM_PARENTCTL3DCHANGED;
  protected
    procedure DoCtl3DChanged; virtual;
    procedure SetDragBorderColour;
    procedure SetEditMode(const Value: TEditMode); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    property EditMode : TEditMode read FEditMode write SetEditMode;
  published
    property Ctl3D;
    property ParentCtl3D;
    property SourceCol : TColor read FSourceCol write SetSourceCol;
    property DestCol : TColor read FDestCol write SetDestCol;
    property TabOrder;
  end;

const
  SOURCE_ONLY=1;
  DEST_ONLY  =2;
  SOURCE_DEST=3;

//==============================================================================
implementation

//==============================================================================
constructor TCompositeComponent.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
end;  // Create

//==============================================================================
procedure TCompositeComponent.SetDestCol(const Value: TColor);
begin
  FDestCol := Value;
  SetDragBorderColour;
end;  // SetDestCol

//==============================================================================
procedure TCompositeComponent.SetSourceCol(const Value: TColor);
begin
  FSourceCol := Value;
  SetDragBorderColour;
end;  // SetSourceCol

//==============================================================================
procedure TCompositeComponent.SetDragBorderColour;
var iCount:integer;
begin
  for iCount:= 0 to ComponentCount-1 do begin
    if Components[iCount] is TPanel then
      with TPanel(Components[iCount]) do
        case Tag of
          SOURCE_ONLY : Color:=FSourceCol;
          DEST_ONLY   : Color:=FDestCol;
          SOURCE_DEST : ; // Shouldn't happen because can't have dashed border on Panel
        end  // case
    else if Components[iCount] is TShape then
      with TShape(Components[iCount]) do
        case Tag of
          SOURCE_ONLY : Pen.Color:=FSourceCol;
          DEST_ONLY   : Pen.Color:=FDestCol;
          SOURCE_DEST : begin
                          Brush.Color:=FSourceCol;
                          Pen.Color  :=FDestCol;
                        end;
        end;  // case
  end;  // for
end;  // SetDrafBorderColour

//==============================================================================
procedure TCompositeComponent.SetEditMode(const Value: TEditMode);
begin
  FEditMode := Value;
end;  // SetEditMode

{-------------------------------------------------------------------------------
}
procedure TCompositeComponent.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustSize;
end;  // TBaseCompositeComponent.CMFontChanged

//==============================================================================
procedure TCompositeComponent.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;  // TCompositeComponent.CMCtl3DChanged

procedure TCompositeComponent.CMParentCtl3DChanged(var Message: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;  // TCompositeComponent.CMParentCtl3DChanged

procedure TCompositeComponent.DoCtl3DChanged;
begin
  // Do nothing here. Override to propagate the change to contained controls in descendents.
end;  // TCompositeComponent.DoCtl3DChanged

end.
