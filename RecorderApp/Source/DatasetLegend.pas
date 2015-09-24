{===============================================================================
  Unit:        DatasetLegend

  Defines:     TDatasetLegend
               TDatasetItem

  Description:

  Model:       Map.mpb

  Created:     December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 21/06/04 10:42 $
    $Author: Ericsalmon $

===============================================================================}

unit DatasetLegend;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs, BaseLegend;

type
  TDatasetItem = class;

  TDatasetLegend = class (TBaseLegend)
  private
    FColourIndex: Integer;
    function GetLegendItems(Index: Integer): TDatasetItem;
  public
    function AddItem(AItem: TBaseLegendItem): Boolean; override;
    procedure SetUnique;
    property LegendItems[Index: Integer]: TDatasetItem read GetLegendItems; default;
  end;
  
  TDatasetItem = class (TBaseLegendItem)
  private
    FColour: TColor;
    FColourChanged: Boolean;
    FDistributionPoints: TObjectList;
    FTableName: String;
    procedure SetColour(const Value: TColor);
  protected
    procedure DoAssign(ASource: TBaseLegendItem); override;
    function DoEditProperties: Boolean; override;
    procedure ResetFlags; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawSymbol(ACanvas: TCanvas; ARect: TRect); override;
    property Colour: TColor read FColour write SetColour;
    property ColourChanged: Boolean read FColourChanged;
    property DistributionPoints: TObjectList read FDistributionPoints;
    property TableName: String read FTableName write FTableName;
  end;
  
//==============================================================================
implementation

uses
  DeleteDatasets;

const
  MAX_DATASETS = 10;

  DATASET_COLOURS: Array[0..9] of TColor = ($FF0000, $00FF00, $0000FF, $000000, $888888,
                                            $FF8888, $88FFFF, $CC66FF, $CCCC00, $CC00CC);

{-==============================================================================
    TDatasetLegend
===============================================================================}
{-------------------------------------------------------------------------------
  Checks the number of datasets already present. If the maximum is reached, a dialog comes up 
      to delete a dataset before adding the new one.  If user cancels this, return false to 
      let calling process the dataset was not added. 
}
function TDatasetLegend.AddItem(AItem: TBaseLegendItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Grid.RowCount = MAX_DATASETS then begin
    with TdlgDeleteDatasets.Create(nil) do
      try
        for i := 0 to Grid.RowCount - 1 do
          lbDatasets.Items.Add(LegendItems[i].Title);
        if ShowModal = mrOk then
          DeleteItem(lbDatasets.ItemIndex)
        else
          Exit;
      finally
        Free;
      end;
  end;
  
  Result := inherited AddItem(AItem);
  
  // Apply default colour to new dataset item.
  if Result then begin
    TDatasetItem(AItem).Colour := DATASET_COLOURS[FColourIndex];
    FColourIndex := (FColourIndex + 1) mod MAX_DATASETS;
  end;
end;  // TDatasetLegend.AddItem 

{-------------------------------------------------------------------------------
}
function TDatasetLegend.GetLegendItems(Index: Integer): TDatasetItem;
begin
  Result := TDatasetItem(inherited LegendItems[Index]);
end;  // TDatasetLegend.GetLegendItems 

{-------------------------------------------------------------------------------
}
procedure TDatasetLegend.SetUnique;
begin
  // If nothing in first row, grid is empty, so nothing to do.
  if LegendItems[0] <> nil then
    // While there is still more then 1 row in grid, delete last row.
    while Grid.RowCount > 1 do
      DeleteItem(Grid.RowCount - 1);
  if Assigned(OnChange) then OnChange(Self);
end;  // TDatasetLegend.SetUnique 

{-==============================================================================
    TDatasetItem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDatasetItem.Create;
begin
  inherited;

  FDistributionPoints := TObjectList.Create;
end;  // TDatasetItem.Create

{-------------------------------------------------------------------------------
}
destructor TDatasetItem.Destroy;
begin
  FDistributionPoints.Free;

  inherited;
end;  // TDatasetItem.Destroy

{-------------------------------------------------------------------------------
}
procedure TDatasetItem.DoAssign(ASource: TBaseLegendItem);
begin
  if ASource is TDatasetItem then
    Colour := TDatasetItem(ASource).Colour;

  inherited;
end;  // TDatasetItem.DoAssign

{-------------------------------------------------------------------------------
}
function TDatasetItem.DoEditProperties: Boolean;
begin
  with TColorDialog.Create(nil) do
    try
      Color := Colour;
      if Execute then begin
        Colour := Color;
        Result := True;
      end else
        Result := False;
    finally
      Free;
    end;
end;  // TDatasetItem.DoEditProperties 

{-------------------------------------------------------------------------------
}
procedure TDatasetItem.DrawSymbol(ACanvas: TCanvas; ARect: TRect);
var
  lPColor, lBColor: TColor;
  lBStyle: TBrushStyle;
  lTop, lLeft: Integer;
begin
  with ACanvas do begin
    // Save values.
    lPColor := Pen.Color;
    lBColor := Brush.Color;
    lBStyle := Brush.Style;
  
    // Center the symbol
    lLeft := ARect.Left + (ARect.Right - ARect.Left - 10) div 2;
    lTop  := ARect.Top  + (ARect.Bottom - ARect.Top - 10) div 2;
    // Draw symbol
    Brush.Color := clWindow;
    Pen.Color   := clWindow;
    Ellipse(lLeft - 2, lTop - 2, lLeft + 12, lTop + 12);
    Brush.Color := Colour;
    Pen.Color   := Colour;
    Ellipse(lLeft, lTop, lLeft + 10, lTop + 10);

    // Retore values.
    Pen.Color   := lPColor;
    Brush.Color := lBColor;
    Brush.Style := lBStyle;
  end;
end;  // TDatasetItem.DrawSymbol

{-------------------------------------------------------------------------------
}
procedure TDatasetItem.ResetFlags;
begin
  inherited ResetFlags;
  
  FColourChanged := False;
end;  // TDatasetItem.ResetFlags 

{-------------------------------------------------------------------------------
}
procedure TDatasetItem.SetColour(const Value: TColor);
begin
  if FColour <> Value then begin
    FColour := Value;
    FColourChanged := True;
    if not EditingProperties then
      if Assigned(OnNeedRefresh) then OnNeedRefresh(Self);
  end;
  if not EditingProperties then ResetFlags;
end;  // TDatasetItem.SetColour 

end.
