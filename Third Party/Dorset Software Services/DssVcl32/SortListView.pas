{ SortListView - a component that provides default sorting behaviour aka the
    list view in explorer
    Copyrigh 2000 Dorset Software Services Ltd }

unit SortListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TSortListView = class(TCustomListView)
  private
    { Private declarations }
    FSortColumnIndex : integer;
    FReverseSort : boolean;
  protected
    { Protected declarations }
    procedure ColClick(Column: TListColumn); override;
    procedure ListViewSort(Sender: TObject; Item1, Item2: TListItem;
                Data: Integer; var Compare: Integer);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
  published
    { Publish the ListView stuff we want }
    property Align;
    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation



{ TSortListView }

{ When you click on a column, do a sort on the contents }
procedure TSortListView.ColClick(Column: TListColumn);
begin
  inherited;
  { Check for reversal of sort }
  FReverseSort := (FSortColumnIndex=Column.Index) and (not FReverseSort);
  { Select the chosen column and sort it }
  FSortColumnIndex := Column.Index;
  AlphaSort;
end;



constructor TSortListView.Create(AOwner: TComponent);
begin
  inherited;
  OnCompare := ListViewSort;
  FSortColumnIndex := 0;
  FReverseSort := False;
  ViewStyle := vsReport;
end;


{ OnCompare handler to provide the standard sort code in TCustomListView with
    a means of comparison.  Column compared depends on FSortColumnIndex }
procedure TSortListView.ListViewSort(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  Caption1, Caption2 : string;
begin
  if FSortColumnIndex = 0 then
  begin
    Caption1 := UpperCase(Item1.Caption);
    Caption2 := UpperCase(Item2.Caption);
  end else
  begin
    Caption1 := UpperCase(Item1.SubItems[FSortColumnIndex-1]);
    Caption2 := UpperCase(Item2.SubItems[FSortColumnIndex-1]);
  end;
  if Caption1 < Caption2 then
    Compare:=-1
  else if Caption1 > Caption2 then
    Compare:=1
  else
    Compare:=0;
  if FReverseSort then
    Compare := 1-Compare;
end;


end.
