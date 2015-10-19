unit ConstraintsDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OffBtn, ExtCtrls, Grids, StdCtrls, AdvGrid, ADODB, BaseGrid, Variants;

type
  TStoreID = class(TObject)
  private
    FID: string;
  public
    property ID: string read FID write FID;
  end;

  TformConstraints = class(TForm)
    cbApply: TCheckBox;
    rgIncludeExclude: TRadioGroup;
    bClear: TOffice97Button;
    bOK: TOffice97Button;
    bCancel: TOffice97Button;
    sgConstraints: TAdvStringGrid;
    Shape: TShape;
    procedure sgConstraintsGetEditorType(Sender: TObject; ACol,
      ARow: Integer; var AEditor: TEditorType);
    procedure sgConstraintsComboChange(Sender: TObject; ACol, ARow,
      AItemIndex: Integer; ASelection: String);
    procedure sgConstraintsCanAddRow(Sender: TObject; var CanAdd: Boolean);
    procedure bClearClick(Sender: TObject);
  private
    { Private declarations }
    FTypes: TStrings;   // Store measurement types
    FQuery: TADOQuery;  // We need access to the database to get Types and Qualifiers

    procedure Set_Apply(const Value: boolean);
    function Get_Apply: boolean;
    procedure GetTypes;
    procedure GetQualfiers(const TypeKey: string);
    function Get_IsConstrained: boolean;
    function Get_SQLClause: string;
    function Get_Exclude: boolean;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent; aQuery: TADOQuery);
    destructor Destroy; override;
    property Apply: boolean read Get_Apply write Set_Apply;
    property IsConstrained: boolean read Get_IsConstrained;
    property SQLClause: string read Get_SQLClause;
    property Exclude: boolean read Get_Exclude;
  end;

var
  formConstraints: TformConstraints;

implementation

Const Quote = Char(39);

{$R *.DFM}

//--------------------------------------------------------------------------------
// We need access to the database query to get measurement types and qualifiers
//--------------------------------------------------------------------------------
constructor TformConstraints.Create(aOwner: TComponent; aQuery: TADOQuery);
begin
     inherited Create(aOwner);
     FQuery := aQuery;
     FTypes := TStringList.Create;
end;

//--------------------------------------------------------------------------------
// Make sure we free objects in lists and grids
//--------------------------------------------------------------------------------
destructor TformConstraints.Destroy;
var iRow, iCol: integer;
begin
     with sgConstraints do
          for iCol := 0 to ColCount-1 do
              for iRow := 0 to RowCount - 1 do
                  if Objects[iCol, iRow] <> nil then
                     TStoreID(Objects[iCol, iRow]).Free;
     FTypes.Free;
     inherited;
end;

//--------------------------------------------------------------------------------
// Implement Apply property
//--------------------------------------------------------------------------------
procedure TformConstraints.Set_Apply(const Value: boolean);
begin
  cbApply.Checked := Value;
end;

function TformConstraints.Get_Apply: boolean;
begin
   Result := cbApply.Checked;
end;

//--------------------------------------------------------------------------------
// Implement IsConstrained property.
// Check that we have at least 1 qualifier key in the grid.
//--------------------------------------------------------------------------------
function TformConstraints.Get_IsConstrained: boolean;
begin
     Result := False;
     with sgConstraints do
          If RowCount > 1 then // there is at least one row of constraints
             If Objects[1, 1] <> nil then // and there is an object attached to it
                If TStoreID(Objects[1, 1]).ID <> '' then // and the key is there
                   Result := True;
end;

//--------------------------------------------------------------------------------
// Implement SQLClause property which generates the necessary extra part of the
// WHERE clause which impliments the selected constraints
//--------------------------------------------------------------------------------
function TformConstraints.Get_SQLClause: string;
var iRow: integer;
begin
     // we only do anything if constraints are set
     If IsConstrained then
     begin
          Result := ' AND (';
          // if we are excluding, we don't want to exclude observations without measurements
          if Exclude then
             Result := Result + '(TAXON_OCCURRENCE_DATA.MEASUREMENT_QUALIFIER_KEY) Is Null OR NOT (';
          // because we have already tested that there are constraints, we already know that RowCount is >1
          with sgConstraints do
               For iRow := 1 to RowCount-1 do
               begin
                   Result := Result + '(TAXON_OCCURRENCE_DATA.MEASUREMENT_QUALIFIER_KEY)=' + Quote
                             + TStoreID(Objects[1, iRow]).ID + Quote;
                   // make sure we only add an OR if there is another row
                   if iRow < RowCount-1 then
                      Result := Result + ' OR '
               end;
          If Exclude then Result := Result + ')'; // terminate the OR NOT bracket
          Result := Result + ') '; // terminate the AND bracket round the whole cluase
     // if there are not constraints - just return an empty string
     end else Result := '';
end;

//--------------------------------------------------------------------------------
// Implement Exclude property
//--------------------------------------------------------------------------------
function TformConstraints.Get_Exclude: boolean;
begin
     Result := (rgIncludeExclude.ItemIndex = 1);
end;

//--------------------------------------------------------------------------------
// Put combobox in grid and populate its list
//--------------------------------------------------------------------------------
procedure TformConstraints.sgConstraintsGetEditorType(Sender: TObject;
  ACol, ARow: Integer; var AEditor: TEditorType);
begin
     aEditor := edComboEdit;
     with sgConstraints do
     case aCol of
          // Measurement types
          0:   begin
                    If FTypes.Count = 0 then GetTypes;
                    If FTypes.Count > 0 then Combobox.Items.Assign(FTypes);
                end;
          // Measurement qualifiers for the chosen type in Col 0
          1:    if Objects[0, aRow] <> nil then
                   GetQualfiers(TStoreID(Objects[0, aRow]).ID);
     end; // case
end;

//--------------------------------------------------------------------------------
// Get measurement type keys and labels from the database
// These are stored in FTypes so we don't need to keep doing it
//--------------------------------------------------------------------------------
procedure TformConstraints.GetTypes;
var lName, lSQL: string;
    lKey: TStoreID;
begin
  FTypes.Clear;
  lSQL := 'SELECT MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY, MEASUREMENT_TYPE.SHORT_NAME ' +
          'FROM MEASUREMENT_TYPE ' +
          'ORDER BY MEASUREMENT_TYPE.SHORT_NAME;';
  FQuery.SQL.Clear;
  FQuery.SQL.Add(lSQL);
  try
    FQuery.Open;
    If FQuery.RecordCount > 0 then
      while not FQuery.eof do
      begin
        lKey := TStoreID.Create;
        lName := FQuery.FieldByName('SHORT_NAME').AsString;
        lKey.ID := FQuery.FieldByName('MEASUREMENT_TYPE_KEY').AsString;;
        FTypes.AddObject(lName, lKey);
        FQuery.Next;
      end;
  finally
    FQuery.Close;
  end;
end;

//--------------------------------------------------------------------------------
// Get keys and labels for Measurement qualifiers for the given measurement type
//--------------------------------------------------------------------------------
procedure TformConstraints.GetQualfiers(const TypeKey: string);
var lName, lSQL: string;
    lKey: TStoreID;
begin
  sgConstraints.Combobox.Items.Clear;
  lSQL := 'SELECT MEASUREMENT_QUALIFIER.MEASUREMENT_QUALIFIER_KEY, MEASUREMENT_QUALIFIER.SHORT_NAME ' +
          'FROM MEASUREMENT_QUALIFIER ' +
          'WHERE (((MEASUREMENT_QUALIFIER.MEASUREMENT_TYPE_KEY)=' + Quote + TypeKey + Quote + ')) ' +
          'ORDER BY MEASUREMENT_QUALIFIER.SHORT_NAME;';
  FQuery.SQL.Clear;
  FQuery.SQL.Add(lSQL);
  try
    FQuery.Open;
    If FQuery.RecordCount > 0 then
      while not FQuery.eof do
      begin
        lKey := TStoreID.Create;
        lName := FQuery.FieldByName('SHORT_NAME').AsString;
        lKey.ID := FQuery.FieldByName('MEASUREMENT_QUALIFIER_KEY').AsString;
        sgConstraints.AddComboStringObject(lName, lKey);
        FQuery.Next;
      end;
  finally
    FQuery.Close;
  end;
end;

//--------------------------------------------------------------------------------
// Item selected in the combobox. Put the label into the cell and store its key
// in the grid's Objects
//--------------------------------------------------------------------------------
procedure TformConstraints.sgConstraintsComboChange(Sender: TObject; ACol,
  ARow, AItemIndex: Integer; ASelection: String);
var lKey: TStoreID;
begin
     // This event fires even if the combo is only searching incrementally
     // e.g. user has pressed a key and list has positioned at first item starting
     // with that character without an item having been retrieved into the edit.
     // But in these circumstances, aItemIndex is -1.
     If aItemIndex >= 0 then
        with sgConstraints do
        begin
             Cells[aCol, aRow] := aSelection;
             lKey := TStoreID.Create;
             lKey.ID := TStoreID(Combobox.Items.Objects[aItemIndex]).ID;
             Objects[aCol, aRow] := lKey;
             // if we are selecting a type, then clear any qualifier
             // already selected in that row
             If aCol = 0 then
             begin
                  Cells[1, aRow] := '';
                  If Objects[1, aRow] <> nil then
                  begin
                       TStoreID(Objects[1, aRow]).Free;
                       Objects[1, aRow] := nil;
                  end;
             end;
        end;
end;

//--------------------------------------------------------------------------------
// Don't allow row to be added if the last existing row is empty
//--------------------------------------------------------------------------------
procedure TformConstraints.sgConstraintsCanAddRow(Sender: TObject;
  var CanAdd: Boolean);
var iCol: integer;
begin
     CanAdd := False;
     with sgConstraints do
          for iCol := 0 to ColCount-1 do
              If Cells[iCol, RowCount-1] <> '' then
                 CanAdd := True;
end;

//--------------------------------------------------------------------------------
// Clear button - clear all constraints
//--------------------------------------------------------------------------------
procedure TformConstraints.bClearClick(Sender: TObject);
begin
     with sgConstraints do
     begin
          BeginUpdate;
          If RowCount > 1 then
             RemoveRows(1, RowCount-1);
          RowCount := 2;
          Cells[0,1] := '';
          Cells[1,1] := '';
          EndUpdate;
     end;
end;

end.
