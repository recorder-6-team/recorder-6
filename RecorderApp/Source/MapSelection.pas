{===============================================================================
  Unit:        MapSelection

  Defines:     TdlgMapSelection

  Description: Let user select a map.

  Model:       Map.mpb

  Created:     January 2004

  Last revision information:
    $Revision: 6 $
    $Date: 5/12/05 15:42 $
    $Author: Johnvanbreda $

===============================================================================}

unit MapSelection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormActions, StdCtrls, ImageListButton, DataClasses, ExtCtrls, LayerLegend;

type
  TdlgMapSelection = class (TForm)
    Bevel1: TBevel;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    Panel1: TPanel;
    lblInstruct: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    lbMaps: TListBox;
    Label2: TLabel;
    lbLayers: TListBox;
    procedure lbLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State:
        TOwnerDrawState);
    procedure lbMapsClick(Sender: TObject);
    procedure lbMapsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State:
        TOwnerDrawState);
  private
    FLayerLegend: TLayerLegend;
    function GetBaseMapKey: TKeyString;
    procedure GetBaseMaps;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BaseMapKey: TKeyString read GetBaseMapKey;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings;

{-==============================================================================
    TdlgMapSelection
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgMapSelection.Create(AOwner: TComponent);
begin
  inherited;

  // Use TLayerLegend class to get all layers for each Base Maps.
  FLayerLegend := TLayerLegend.Create(nil, nil);
  GetBaseMaps;
  // Force label to resize, for internationalisation or different pixels per inch
  lblInstruct.AutoSize := False;
  lblInstruct.AutoSize := True;
end;  // TdlgMapSelection.Create

{-------------------------------------------------------------------------------
}
destructor TdlgMapSelection.Destroy;
begin
  FLayerLegend.Free;
  FLayerLegend := nil;

  inherited;
end;  // TdlgMapSelection.Destroy

{-------------------------------------------------------------------------------
}
function TdlgMapSelection.GetBaseMapKey: TKeyString;
begin
  Result := AppSettings.AvailableMaps[lbMaps.ItemIndex].BaseMapKey;
end;  // TdlgMapSelection.GetBaseMapKey

{-------------------------------------------------------------------------------
}
procedure TdlgMapSelection.GetBaseMaps;
var
  i: Integer;
begin
  for i := 0 to AppSettings.AvailableMaps.Count - 1 do
    lbMaps.Items.Add(AppSettings.AvailableMaps[i].DisplayName);
  // Select first one.
  lbMaps.ItemIndex := 0;
  lbMapsClick(nil);
end;  // TdlgMapSelection.GetBaseMaps

{-------------------------------------------------------------------------------
}
procedure TdlgMapSelection.lbLayersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
    State: TOwnerDrawState);
begin
  lbLayers.Canvas.FillRect(ARect);
  with FLayerLegend[Index] do begin
    // Symbol is drawn in centre of specified rectangle.
    DrawSymbol(lbLayers.Canvas,
               Rect(ARect.Left + 2, ARect.Top, ARect.Left + 18, ARect.Bottom));
    lbLayers.Canvas.TextOut(ARect.Left + 22, ARect.Top + 2, Title);
  end;
end;  // TdlgMapSelection.lbLayersDrawItem

{-------------------------------------------------------------------------------
}
procedure TdlgMapSelection.lbMapsClick(Sender: TObject);
var
  i: Integer;
begin
  FLayerLegend.BaseMapKey := AppSettings.AvailableMaps[lbMaps.ItemIndex].BaseMapKey;
  lbLayers.Clear;
  for i := 0 to FLayerLegend.Count - 1 do lbLayers.Items.Add('');
end;  // TdlgMapSelection.lbMapsClick

{-------------------------------------------------------------------------------
}
procedure TdlgMapSelection.lbMapsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
    State: TOwnerDrawState);
begin
  with lbMaps do begin
    Canvas.FillRect(Rect);
    dmFormActions.ilMenuOn.Draw(Canvas, Rect.Left + 2, Rect.Top, 21);
    Canvas.TextOut(Rect.Left + 22, Rect.Top + 2, Items[Index]);
  end;
end;  // TdlgMapSelection.lbMapsDrawItem

end.
