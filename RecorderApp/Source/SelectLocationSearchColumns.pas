{===============================================================================
 Unit:        SelectLocationSearchColumns
 
 Defines:     TdlgSelectLocationSearchColumns
 
 Description: Dialog that allows the user to select which columns should be
              displayed (in addition to the name) when searching for locations.

 Created:     July 2009

 Last revision information:
   $Revision: 1 $
   $Date: 17/07/09 12:42 $
   $Author: Andrewkemp $

===============================================================================}
unit SelectLocationSearchColumns;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, CheckLst, ApplicationSettings;

resourcestring
  ResStr_LocationType = 'Location Type';
  ResStr_SpatialReference = 'Spatial Reference';
  ResStr_FileCode = 'File Code';
  
type
  TdlgSelectLocationSearchColumns = class(TForm)
    chklbColumns: TCheckListBox;
    bbCancel: TImageListButton;
    bbOK: TImageListButton;
    Label1: TLabel;
    procedure chklbColumnsClickCheck(Sender: TObject);
  private
    FSelectedColumns: TLocationSearchColumns;
  protected
    procedure SetSelectedColumns(Selection: TLocationSearchColumns);
    procedure UpdateSelectedColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedColumns: TLocationSearchColumns
        read FSelectedColumns write SetSelectedColumns;
  end;

var
  dlgSelectLocationSearchColumns: TdlgSelectLocationSearchColumns;


implementation

{$R *.dfm}

type
  TColumn = class
  public
    Column: TLocationSearchColumn;
    constructor Create(Column: TLocationSearchColumn);
  end;

constructor TColumn.Create(Column: TLocationSearchColumn);
begin
  Self.Column := Column;
end;  

{-------------------------------------------------------------------------------
  Initialize a new instance of the dialog.
}
constructor TdlgSelectLocationSearchColumns.Create(AOwner: TComponent);

  procedure AddColumn(const Name: String; Column: TLocationSearchColumn);
  begin
    chklbColumns.Items.AddObject(Name, TColumn.Create(Column));
  end;

begin
  inherited;
  AddColumn(ResStr_LocationType, lscLocationType);
  AddColumn(ResStr_SpatialReference, lscSpatialReference);
  AddColumn(ResStr_FileCode, lscFileCode);
end;

{-------------------------------------------------------------------------------
  Release resources used by the dialog.
}
destructor TdlgSelectLocationSearchColumns.Destroy;
var
  I: Integer;
begin
  for I := 0 to chklbColumns.Count - 1 do
  begin
    chklbColumns.Items.Objects[I].Free;
    chklbColumns.Items.Objects[I] := Nil;
  end;
  inherited;
end;

{-------------------------------------------------------------------------------
  Set the selection of columns.
}
procedure TdlgSelectLocationSearchColumns.SetSelectedColumns(
  Selection: TLocationSearchColumns);
var
  I: Integer;
  lItem: TColumn;
begin
  for I := 0 to chklbColumns.Count - 1 do
  begin
    lItem := TColumn(chklbColumns.Items.Objects[I]);
    chklbColumns.Checked[I] := lItem.Column in Selection;
  end;
  FSelectedColumns := Selection;
end;

{-------------------------------------------------------------------------------
  Update the selection of columns from the choices made by the user.
}
procedure TdlgSelectLocationSearchColumns.UpdateSelectedColumns;
var
  I: Integer;
  lItem: TColumn;
begin
  for I := 0 to chklbColumns.Count - 1 do
  begin
    lItem := TColumn(chklbColumns.Items.Objects[I]);
    if chklbColumns.Checked[I] then
      Include(FSelectedColumns, lItem.Column)
    else
      Exclude(FSelectedColumns, lItem.Column);
  end;
end;

{-------------------------------------------------------------------------------
  Update the selection of columns.
}
procedure TdlgSelectLocationSearchColumns.chklbColumnsClickCheck(
  Sender: TObject);
begin
  UpdateSelectedColumns;
end;

end.
