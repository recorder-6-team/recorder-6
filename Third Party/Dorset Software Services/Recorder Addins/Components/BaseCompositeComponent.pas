{===============================================================================
  Unit:        CompositeComponent

  Defines:     TCompositeComponent

  Description: Base class for components made up of several other ones.

  Created:     August 2003

  Last revision information:
    $Revision: 9 $
    $Date: 8/12/05 13:19 $
    $Author: Johnvanbreda $

===============================================================================}

unit BaseCompositeComponent;

interface

uses
  Windows, Messages, Controls, ExtCtrls, Graphics, Classes, DSSDataTypes;

const
  SOURCE_ONLY = 1;
  DEST_ONLY   = 2;
  SOURCE_DEST = 3;

type
  TBaseCompositeComponent = class (TWinControl)
  private
    FDragDestinationColour: TColor;
    FDragSourceColour: TColor;
    FEditMode: TEditMode;
    procedure SetDragDestinationColour(const Value: TColor);
    procedure SetDragSourceColour(const Value: TColor);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentCtl3DChanged(var Message: TMessage); message CM_PARENTCTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure DoCtl3DChanged; virtual;
    procedure SetColor(const Value: TColor); virtual;
    procedure SetDragBorderColour;
    procedure SetEditMode(const Value: TEditMode); virtual;
    procedure SetFont(Value: TFont); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Color write SetColor;
  published
    property Ctl3D;
    property Font write SetFont;
    property ParentCtl3D;
    property ParentFont;
    property DragDestinationColour: TColor read FDragDestinationColour write
        SetDragDestinationColour default clRed;
    property DragSourceColour: TColor read FDragSourceColour write SetDragSourceColour 
        default clBlue;
    property EditMode: TEditMode read FEditMode write SetEditMode default emEdit;
    property OnEnter;
    property OnExit;
    property TabOrder;
    property Visible;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TBaseCompositeComponent
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;  // TBaseCompositeComponent.CMCtl3DChanged

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.CMParentCtl3DChanged(var Message: TMessage);
begin
  inherited;
  DoCtl3DChanged;
end;  // TBaseCompositeComponent.CMParentCtl3DChanged

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustSize;
end;  // TBaseCompositeComponent.CMFontChanged

{-------------------------------------------------------------------------------
}
constructor TBaseCompositeComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  inherited Color := clWindow;
  FDragSourceColour := clBlue;
  FDragDestinationColour := clRed;
  FEditMode := emEdit;
  Tag := 1;
end;  // TBaseCompositeComponent.Create

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.DoCtl3DChanged;
begin
  // Nothing in base class.
end;  // TBaseCompositeComponent.DoCtl3DChanged

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetColor(const Value: TColor);
begin
  inherited Color := Value;
end;  // TBaseCompositeComponent.SetColor

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetDragBorderColour;
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do begin
    if Components[i] is TPanel then
      with TPanel(Components[i]) do
        case Tag of
          SOURCE_ONLY: Color := FDragSourceColour;
          DEST_ONLY  : Color := FDragDestinationColour;
          SOURCE_DEST: ;
              // Shouldn't happen because can't have dashed border on Panel
        end  // case
    else if Components[i] is TShape then
      with TShape(Components[i]) do
        case Tag of
          SOURCE_ONLY: Pen.Color := FDragSourceColour;
          DEST_ONLY  : Pen.Color := FDragDestinationColour;
          SOURCE_DEST: begin
                         Brush.Color := FDragSourceColour;
                         Pen.Color   := FDragDestinationColour;
                       end;
        end;  // case
  end;  // for
end;  // TBaseCompositeComponent.SetDragBorderColour

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetDragDestinationColour(const Value: TColor);
begin
  FDragDestinationColour := Value;
  SetDragBorderColour;
end;  // TBaseCompositeComponent.SetDragDestinationColour 

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetDragSourceColour(const Value: TColor);
begin
  FDragSourceColour := Value;
  SetDragBorderColour;
end;  // TBaseCompositeComponent.SetDragSourceColour 

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetEditMode(const Value: TEditMode);
begin
  FEditMode := Value;
end;  // TBaseCompositeComponent.SetEditMode

{-------------------------------------------------------------------------------
}
procedure TBaseCompositeComponent.SetFont(Value: TFont);
begin
  inherited Font := Value;
  AdjustSize;
end;  // TBaseCompositeComponent.SetFont

end.
