{===============================================================================
  Unit:        BaseMatchPage

  Defines:     TBaseMatch

  Description: Base class for page requesting of the user to match imported data
               against existing Recorder data.

  Model:       ImportWizard

  Last revision information:
    $Revision: 5 $
    $Date: 29/04/09 9:20 $
    $Author: Ericsalmon $

===============================================================================}

unit ColumnType;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, ComCtrls, FormActions,
  RapTree, exgrid, IWColumnMappingClasses;

resourcestring
  ResStr_TaxonOccurrenceData = 'Taxon Occurrence Data';

type
  {-----------------------------------------------------------------------------
    Screen displayed when the user selects <More options> when assigning columns to column
    types in the import wizard.  This allows the user to select from any supported import
    column type and assign it to the current column in the import data grid.
  }
  TdlgColumnType = class (TForm)
    pnlButtons: TPanel;
    tvTypes: TRapidTree;
    pnlButtonsAlign: TPanel;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    procedure tvTypesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
        State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvTypesDblClick(Sender: TObject);
  private
    function GetColumnType: TColumnType;
    procedure SetColumnType(const Value: TColumnType);
  public
    constructor Create(AOwner: TComponent; AColumnMapping: TColumnMapping); reintroduce; overload;
    property ColumnType: TColumnType read GetColumnType write SetColumnType;
  end;

  TColumnTypeNode = class(TFlyNode)
  private
    FColumnType: TColumnType;
    procedure SetColumnType(const Value: TColumnType);
  public
    property ColumnType: TColumnType read FColumnType write SetColumnType;
  end;

implementation

{$R *.dfm}

{-==============================================================================
    TdlgColumnType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgColumnType.Create(AOwner: TComponent; AColumnMapping: TColumnMapping);
var
  i: integer;
  lNewNode: TColumnTypeNode;
  lDataNode: TFlyNode;
begin
  inherited Create(AOwner);
  lDataNode := tvTypes.Items.AddChild(nil, ResStr_TaxonOccurrenceData);

  with AColumnMapping do
    for i := 0 to TypeCount - 1 do
    begin
      if Types[i] is TTaxonOccurrenceDataColumnType then
        lNewNode := tvTypes.Items.AddTypedChild(lDataNode, TColumnTypeNode) as TColumnTypeNode
      else
        lNewNode := tvTypes.Items.AddTypedChild(nil, TColumnTypeNode) as TColumnTypeNode;
      lNewNode.Text       := Types[i].Name;
      lNewNode.ColumnType := Types[i];
    end;
  tvTypes.Items.AlphaSort;
end;

{-------------------------------------------------------------------------------
}
function TdlgColumnType.GetColumnType: TColumnType;
begin
  Result := nil;
  if Assigned(tvTypes.Selected) then
    if tvTypes.Selected is TColumnTypeNode then
      Result := TColumnTypeNode(tvTypes.Selected).ColumnType;
end;  // TdlgColumnType.GetColumnType

{-------------------------------------------------------------------------------
}
procedure TdlgColumnType.tvTypesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with tvTypes.Canvas.Font do
    if Node.Text = ResStr_TaxonOccurrenceData then
    begin
      Name  := 'Arial';
      Style := [fsItalic];
    end;
end;  // TdlgColumnType.tvTypesCustomDrawItem

{-------------------------------------------------------------------------------
}
procedure TdlgColumnType.tvTypesDblClick(Sender: TObject);
begin
  if tvTypes.Selected.Text <> ResStr_TaxonOccurrenceData then ModalResult := mrOk;
end;  // TdlgColumnType.tvTypesDblClick

{-------------------------------------------------------------------------------
}
procedure TdlgColumnType.SetColumnType(const Value: TColumnType);
var
  node: TFlyNode;
begin
  with tvTypes do
  begin
    Selected := Items.GetFirstNode;
    node := Items.GetFirstNode;
    if Assigned(Value) then
    begin
      while Assigned(node) do
        if (node is TColumnTypeNode)
           and (TColumnTypeNode(node).ColumnType = Value) then
        begin
          Selected := node;
          Break;
        end else
          node := node.GetNext;
    end;

    // Show the selected item.
    TopItem := Selected;
  end;
end;

//------------------------------------------------------------------------------
{ TColumnTypeNode }
{-------------------------------------------------------------------------------
}
procedure TColumnTypeNode.SetColumnType(const Value: TColumnType);
begin
  FColumnType := Value;
end;

end.
