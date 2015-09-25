//==============================================================================
//  Unit:        JNCCDatasets
//
//  Implements:  TDatasetAssistor
//               TJnccQuery
//               TJnccTable
//
//  Description: TJnccQuery and TJnccTable
//               Two identical components apart from the class they are derived
//               from.  Each traps fields that are VAGUE_DATE_START fields and
//               assigns OnSetText and OnGetText handlers for them. Other vague
//               date fields are hidden from view.
//
//               TDatasetAssistor
//               Rather than duplicating all the code between queries and
//               tables, we use an assistor class to perform the translations.
//               There is only one instance of this class required for all
//               datasets, so we use intialization and finalization to create
//               and free it.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     John van Breda 07/04/99
//               Now handle boolean field display so that they appear as Yes/No.
//
//               John van Breda 07/04/99
//               Now handles grid_square_type conversion eg from hectare to 100.
//
//  Last Revision Details:
//    $Revision: 112 $
//    $Date: 8.04.10 16:42 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit JNCCDatasets;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  VagueDate, ExceptionForm, SpatialRefFuncs, SQLConverter, ADODB;

type
  EJNCCDatasetError = class(TExceptionPath);

  TRequestNameEvent = procedure (Sender: TObject; var Taxon: string) of object;

  // Identifies each of the four fields used to define a spatial reference
  TSpatialReferenceField = (srfDefault, srfSystem, srfLat, srfLong);

  { Rather than duplicating all the code between queries and tables, we use an
       assistor class to perform the translations.  There is only one instance
       of this class required for all datasets, so we use intialization and
       finalization to create and free it. }
  TDatasetAssistor = class(TObject)
  private
    FLocalDBPath: string;
    FDictTables : TStringList;
    function GetRefSystemField(ADataset: TDataset; const APrefix: string):
        TField;
    function LatOrLongFieldName(ADataset: TDataset; const ALatOrLong: string):
        string;
    procedure SetSystem(const Value: string);
  protected
    FSystem : string;
    function GetVagueDateFromData(const iDataset: TDataset;
                                  const istField: string): TVagueDate;
    procedure SetVagueDateFields( iField: TField; iVagueDate: TVagueDate);
    function IsSpatialField(const iName: string): boolean;
    function GetSpatialName(const iFieldName: string): string;
    procedure SetLocalDBPath( const Value : String );
    function IsInDictFile(const iTableName : string) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoInternalOpen( iDataset : TDataset );
    procedure GetVagueText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure SetVagueText(Sender: TField; const Text: string);
    procedure GetBooleanText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure SetBooleanText(Sender: TField; const Text: string);
    procedure GetSpatialText(Sender: TField; var Text: string; DisplayText: Boolean);
    function HasSpatialFields(ADataset: TDataset): Boolean;
    function LongitudeFieldName(ADataset: TDataset): string;
    function LatitudeFieldName(ADataset: TDataset): string;
    procedure SetSpatialText(Sender: TField; const Text: string);
    procedure SpatialReferenceChanged(Sender: TField);
    function GetSpatialFieldName(ADataset: TDataset;
        const FieldType: TSpatialReferenceField): string;
    property System : string read FSystem write SetSystem;
    property LocalDBPath : string read FLocalDBPath write SetLocalDBPath;
  end;  // TDataSetAssistor

  //----------------------------------------------------------------------------
  TJNCCTable = class(TADOTable)
  protected
    procedure InternalOpen; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;  // TJnccTable

  //----------------------------------------------------------------------------
  TJNCCQuery = class(TADOQuery)
  private
    FOriginalSQL: String;
    FParseSQL: Boolean;
    procedure ReadCollection(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure InternalOpen; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteSQL;
  published
    property ParseSQL: Boolean read FParseSQL write FParseSQL;
  end;  // TJnccQuery

  //----------------------------------------------------------------------------
  TJNCCNamesQuery = class(TJNCCQuery)
  private
    FDisplayCommonNames : boolean;
    FOnRequestNameByOccurrence : TRequestNameEvent;
    FOnRequestNameByListItem   : TRequestNameEvent;
  protected
    procedure SetDisplayCommonNames(Value: boolean);
    procedure InternalOpen; override;
    procedure GetCommonNameForTaxonListItemKey(Sender: TField;
      var Text: string; DisplayText: Boolean);
    procedure GetCommonNameForOccurrenceKey(Sender: TField;
      var Text: string; DisplayText: Boolean);
  published
    property DisplayCommonNames: boolean read FDisplayCommonNames write SetDisplayCommonNames;
    property OnRequestNameByOccurrence: TRequestNameEvent read FOnRequestNameByOccurrence write FOnRequestNameByOccurrence;
    property OnRequestNameByListItem: TRequestNameEvent read FOnRequestNameByListItem write FOnRequestNameByListItem;
  end;  // TJNCCNamesQuery

var
  { global assistor class instance }
  gAssistor : TDatasetAssistor;

//==============================================================================
implementation

uses Registry, FileCtrl, StrUtils, Variants;

const
  VAGUE_DATE_VALID_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', ' ', '/', '.', '=', '_', ',',
                            '#', '@', ':', ';', '#', '!', '$', '%', '^', '&', '*', '<',
                            '>', '@', '{', '}', '[', ']', '(', ')', '+', '\', '|'];

  TRUE_DISPLAY      = 'Yes';
  FALSE_DISPLAY     = 'No';
  SPATIAL_REF       = 'SPATIAL_REF';
  SPATIAL_REF_SYSTEM= 'SPATIAL_REF_SYSTEM';
  GRID_SQUARE_SIZE  = 'SIZE';
  HECTARE           = 'Hectare';
  TETRAD            = 'Tetrad';

  // tables that are linked in the nbndict.mdb file
  DICT_TABLES : array[0..27] of string =
              ('ADMIN_AREA',
               'ADMIN_AREA_SOURCES',
               'ADMIN_RELATION',
               'ADMIN_TYPE',
               'BIOTOPE',
               'BIOTOPE_CLASSIFICATION',
               'BIOTOPE_CLASSIFICATION_VERSION',
               'BIOTOPE_CLASSIFICATION_TYPE',               
               'BIOTOPE_DESIGNATION',
               'BIOTOPE_DESIGNATION_TYPE',
               'BIOTOPE_FACT',
               'BIOTOPE_LIST_ITEM',
               'BIOTOPE_SOURCES',
               'TAXON',
               'TAXON_BIOTOPE_ASSOCIATION',
               'TAXON_COMMON_NAME',
               'TAXON_DESIGNATION',
               'TAXON_DESIGNATION_TYPE',
               'TAXON_FACT',
               'TAXON_LIST',
               'TAXON_LIST_ITEM',
               'TAXON_LIST_TYPE',
               'TAXON_LIST_VERSION',
               'TAXON_NAME_TYPE',
               'TAXON_RANK',
               'TAXON_SOURCES',
               'TAXON_TAXON_ASSOCIATION',
               'TAXON_VERSION');

resourcestring
  ResStr_InputInvalid = ' is not valid text for a yes/no field.';
  ResStr_InvalidVagueDate = 'The vague date you have entered is not valid';

{$R *.RES}

//==============================================================================
//==============================================================================
{ TJNCCTable }
//==============================================================================
{ Override this to trap vague date start and spatial ref fields.  The OnSetText
     and OnGetText handlers are assigned so the display/data entry can be
     managed }
constructor TJNCCTable.Create(AOwner: TComponent);
begin
  inherited;
  CommandTimeout := 0;
end;

procedure TJNCCTable.InternalOpen;
begin
  inherited InternalOpen;
  gAssistor.DoInternalOpen(Self);
end;

//==============================================================================
{ TJNCCQuery }
//==============================================================================
// Dummy class to be able to stream stuff in and out of data modules in D4/D5
// without the "Property does not exist" message type thing.
type
  TJNCCDummyParam = class(TCollectionItem)
  private
    FDataType: TFieldType;
    FName: String;
    FSize: Integer;
    FValue: Variant;
    FParamType: TParamType;  // Defined in unit DB
  published
    property DataType: TFieldType read FDataType write FDataType default ftUnknown;
    property Name: String read FName write FName;
    property Size: Integer read FSize write FSize;
    property Value: Variant read FValue write FValue;
    property ParamType: TParamType read FParamType write FParamType default ptUnknown;
  end;

constructor TJNCCQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommandTimeout := 0;
  FParseSQL := true;     // Set to false before open/exec if converter not needed.
end;  // Create

procedure TJNCCQuery.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadCollection, nil, true);
end;

procedure TJNCCQuery.ReadCollection(Reader: TReader);
var lColl: TCollection;
begin
  lColl := TCollection.Create(TJNCCDummyParam);
  try
    if Reader.ReadValue = vaCollection then
      Reader.ReadCollection(lColl);
  finally
    lColl.Free;
  end;
end;

{ Override this to trap vague date start fields.  The OnSetText and OnGetText
     handlers are assigned so the display/data entry can be managed }
procedure TJNCCQuery.InternalOpen;
begin
  inherited InternalOpen;
  gAssistor.DoInternalOpen(Self);
end;

procedure TJNCCQuery.ExecuteSQL;
begin
  // Don't do any conversion on Access queries going to Access database!!!!!
  if (Pos('Provider=Microsoft.Jet', Connection.ConnectionString) = 0) and FParseSQL then begin
    FOriginalSQL := Self.SQL.Text;
    Self.SQL.Text := ConvertSQL(Self.SQL.Text, ctSQLServer);
  end;
  // Now execute the query.
  ExecSQL;
  // Restore Original SQL text
  if (FOriginalSQL <> '') and FParseSQL then
    Self.SQL.Text := FOriginalSQL;
  FOriginalSQL := '';
end;

//------------------------------------------------------------------------------
procedure TJNCCQuery.DoBeforeOpen;
begin
  FOriginalSQL := '';
  // Don't do any conversion on Access queries going to Access database!!!!!
  if (Pos('Provider=Microsoft.Jet', Connection.ConnectionString) = 0) and FParseSQL then begin
    // Save original SQL
    FOriginalSQL := Self.SQL.Text;
    // Parse SQL
    Self.SQL.Text := ConvertSQL(Self.SQL.Text, ctSQLServer);
    // Carry on.
  end;
  inherited DoBeforeOpen;
end;

procedure TJNCCQuery.DoAfterClose;
begin
  // Restore original SQL
  if (FOriginalSQL <> '') and FParseSQL then
    Self.SQL.Text := FOriginalSQL;
  FOriginalSQL := '';
  // Carry on
  inherited DoAfterClose;
end;

//==============================================================================
//==============================================================================
{ TDatasetAssistor }
//==============================================================================
{ Read the list of dictionary tables }
constructor TDatasetAssistor.Create;
var
  i : integer;
begin
  inherited;
  FDictTables := TStringList.Create;
  FDictTables.Sorted := True; // for faster lookups
  for i := 0 to High(DICT_TABLES) do
    FDictTables.Add(DICT_TABLES[i]);
end;

//==============================================================================
destructor TDatasetAssistor.Destroy;
begin
  FDictTables.Free;
  inherited;
end;

//==============================================================================
{ Manage the opening of a dataset.  Looks for special spatial ref or date fields
    and sets up appropriate setText and getText methods }
procedure TDatasetAssistor.DoInternalOpen(iDataset: TDataset);
var
  i: integer;
  lFieldName: string;
  lSystemField: TField;
begin
  for i := 0 to iDataset.FieldCount-1 do
  begin
    if IsVagueDateField(iDataset.Fields[i].FieldName) then
    begin
      with iDataset.Fields[i] do begin
        lFieldName := GetVagueDateName(FieldName);
        // check other required vague date fields are specified
        if (iDataset.FieldList.IndexOf(lFieldName + 'VAGUE_DATE_END')>-1) and
            (iDataset.FieldList.IndexOf(lFieldName + 'VAGUE_DATE_TYPE')>-1) then begin
          ValidChars := VAGUE_DATE_VALID_CHARS;
          Alignment  := taLeftJustify;
          OnGetText  := GetVagueText;
          OnSetText  := SetVagueText;
          lFieldName := GetVagueDateName(FieldName);
          iDataset.FieldByName(lFieldName + 'VAGUE_DATE_END').Visible := False;
          iDataset.FieldByName(lFieldName + 'VAGUE_DATE_TYPE').Visible := False;
        end;
      end;
    end else
    if iDataset.Fields[i].DataType = ftBoolean then // setup boolean display
    begin
      iDataset.Fields[i].OnGetText := GetBooleanText;
      iDataset.Fields[i].OnSetText := SetBooleanText;
    end else
    if IsSpatialField(iDataset.Fields[i].FieldName) then
    begin
      lFieldName:= GetSpatialName(iDataset.Fields[i].FieldName);
      if (iDataset.FieldList.IndexOf(lFieldName + 'LAT')>-1) and
          (iDataset.FieldList.IndexOf(lFieldName + 'LONG')>-1) then
      begin
        iDataset.Fields[i].OnGetText := GetSpatialText;
        iDataset.Fields[i].OnSetText := SetSpatialText;
        iDataset.Fields[i].OnChange  := SpatialReferenceChanged;
        { Hide the lat/long fields }
        iDataset.FieldByName(lFieldName + 'LAT').Visible := False;
        iDataset.FieldByName(lFieldName + 'LONG').Visible := False;
        lSystemField := GetRefSystemField(iDataset, lFieldName);
        if Assigned(lSystemField) then lSystemField.Visible := False;
      end;
    end;
  end; // for
end;  // DoInternalOpen

//==============================================================================
{ Retrieve Yes or No from a field according to boolean value }
procedure TDatasetAssistor.GetBooleanText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  if (Sender.Value = Null) then
    Text := ''
  else
  begin
    if Sender.AsBoolean = True then
      Text := TRUE_DISPLAY
    else if Sender.AsBoolean = False then
      Text := FALSE_DISPLAY;
  end;
end;  // GetBooleanText

{-------------------------------------------------------------------------------
  Find the field for spatial ref system. This field can exist with or without
     the prefix
}
function TDatasetAssistor.GetRefSystemField(ADataset: TDataset;
  const APrefix: string): TField;
begin
  Result := ADataset.FindField(SPATIAL_REF_SYSTEM);
  if not Assigned(Result) then
    Result := ADataset.FindField(APrefix + SPATIAL_REF_SYSTEM);
end;

//------------------------------------------------------------------------------
{ Writes the appropriate value into a boolean field according to the text
     input }
procedure TDatasetAssistor.SetBooleanText(Sender: TField;
  const Text: string);
begin
  if (Uppercase(Text)='TRUE') or (Uppercase(Text[1])='Y') then
    Sender.AsBoolean := True
  else if (Uppercase(Text)='FALSE') or (Uppercase(Text[1])='N') then
      Sender.AsBoolean := False
    else
      raise EJNCCDatasetError.Create(Text + ResStr_InputInvalid);
end;  // SetBooleanText

//==============================================================================
// Spatial Ref
function TDatasetAssistor.IsSpatialField(const iName: string): boolean;
begin
  { Look for 'SPATIAL_REF' on the end }
  Result := CompareText( Copy(iName, Length(iName) - Length(SPATIAL_REF)+1,
                  Length(SPATIAL_REF)), SPATIAL_REF)=0;
end;  // IsSpatialField

//------------------------------------------------------------------------------
function TDatasetAssistor.GetSpatialName(const iFieldName: string): string;
var
  lDotPos                                     : integer;
  lJustFieldName, lTable, lResult : string;
begin
  // Firstly check that if iFieldName contains the table name as well that this
  // is stripped off;
  lDotPos := pos('.', iFieldName);
  if lDotPos > 0 then
  begin
    lJustFieldName:=Copy(iFieldName,lDotPos+1,Length(iFieldName)-lDotPos);
    lTable:=Copy(iFieldName,1,lDotPos-1);
  end
  else
  begin
    lJustFieldName:=iFieldName;
    lTable := '';
  end;
  // Next remove the 'SPATIAL_REF' bit of the fieldname
  lResult := Copy(lJustFieldName, 1, Length(lJustFieldName)-Length(SPATIAL_REF));
  if lTable <> '' then
    // Add table bit back on
    Result := lTable + '.' + lResult
  else
    Result := lResult;
end;  // GetSpatialName


// Accessor method
procedure TDatasetAssistor.SetLocalDBPath( const Value : String );
begin
  FLocalDBPath := Value;
end;

//------------------------------------------------------------------------------
procedure TDatasetAssistor.GetSpatialText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  lRefPrefix: string;
  lSystemField: TField;
begin
  { If preferred system not supplied, return it as we know it }
  if gAssistor.System = '' then
    Text := Sender.AsString
  else
  begin
    lRefPrefix := GetSpatialName(Sender.FieldName);
    lSystemField := GetRefSystemField(Sender.Dataset, lRefPrefix);
    if not Assigned(lSystemField) then
      Text := Sender.AsString
    else
    begin
      Text := GetDisplaySpatialRef(gAssistor.System,
          Sender.Dataset.FieldByName(lRefPrefix + 'SPATIAL_REF').AsString,
          lSystemField.AsString,
          Sender.Dataset.FieldByName(lRefPrefix + 'LAT').Value,
          Sender.Dataset.FieldByName(lRefPrefix + 'LONG').Value,
          Sender.AsString)
    end;
  end;
  Text := LocaliseSpatialRef(Text);
end;  // GetSpatialText

//------------------------------------------------------------------------------
{ Writes a spatial ref string back into the dataset }
procedure TDatasetAssistor.SetSpatialText(Sender: TField;
  const Text: string);
begin
  Sender.Value := DelocaliseSpatialRef(Text);
end;

{-------------------------------------------------------------------------------
  Updates dependent fields (latitude, longitude, spatial reference system) to
  reflect a change in a spatial reference value.
}
procedure TDatasetAssistor.SpatialReferenceChanged(Sender: TField);
var
  lLatLong: TLatLong;
  lSpatialRef: string;
  lRefPrefix: string;
  lSystemField: TField;
  lSystem: string;

  function GetSystem: string;
  var
    lSystem: string;
  begin
    Result := '';
    if Assigned(lSystemField) then
    begin
      lSystem := lSystemField.AsString;
      if TestSpatialRefAgainstSystem(lSpatialRef, lSystem) then
        Result := lSystem;
    end;
  end;

begin
  lSpatialRef := Sender.AsString;
  lRefPrefix := GetSpatialName(Sender.FieldName);
  lSystemField := GetRefSystemField(Sender.Dataset, lRefPrefix);
  
  if lSpatialRef = '' then
    lLatLong := LatLong(NULL_LATLONG, NULL_LATLONG)
  else
  begin
    lSystem := GetSystem;
    if lSystem = '' then lSystem := DetermineSpatialRefSystem(lSpatialRef);
    lLatLong := ConvertToLatLong(lSpatialRef, lSystem);
  end;

  if lLatLong.LAT = NULL_LATLONG then
    Sender.Dataset.FieldByName(lRefPrefix + 'LAT').Clear
  else
    Sender.Dataset.FieldByName(lRefPrefix + 'LAT').AsFloat := lLatLong.LAT;

  if lLatLong.LONG = NULL_LATLONG then
    Sender.Dataset.FieldByName(lRefPrefix + 'LONG').Clear
  else
    Sender.Dataset.FieldByName(lRefPrefix + 'LONG').AsFloat := lLatLong.LONG;

  if Assigned(lSystemField) then
  begin
    if lSystem = '' then
      lSystemField.Clear
    else
      lSystemField.AsString := lSystem;
  end;
end;

//==============================================================================
{ Set the preferred reference system. Blank value- data is displayed as it was
     entered }
procedure TDatasetAssistor.SetSystem(const Value: string);
begin
  FSystem := Value;
end;  // SetSystem

//==============================================================================
{ Return the vague date construct for the current record in a dataset.  The
     field supplied is used to extract the name of each vague date field }
function TDatasetAssistor.GetVagueDateFromData(const iDataset: TDataset;
  const istField: string): TVagueDate;
var
  lstFieldStart : string;
begin
  lstFieldStart := Copy(istField, 1, Length(istField)-16);
  if not iDataset.FieldByName(lstFieldStart + 'VAGUE_DATE_START').IsNull then
    Result.StartDate := iDataset.FieldByName
                              (lstFieldStart + 'VAGUE_DATE_START').Value;
  if not iDataset.FieldByName(lstFieldStart + 'VAGUE_DATE_END').IsNull then
    Result.EndDate := iDataset.FieldByName
                              (lstFieldStart + 'VAGUE_DATE_END').Value;
  Result.DateTypeString := iDataset.FieldByName
                      (lstFieldStart + 'VAGUE_DATE_TYPE').AsString;
end;  // GetVagueDateFromData

//------------------------------------------------------------------------------
{ Looks for the respective vague date fields and translates them into a display
     text string }
procedure TDatasetAssistor.GetVagueText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  lVagueDate : TVagueDate;
begin
  try
    lVagueDate := GetVagueDateFromData(Sender.Dataset, Sender.FieldName);
    Text := VagueDateToString(lVagueDate);
  except
    on EConvertError do
    Text := '';
  end;
end;  // GetVagueText

{-------------------------------------------------------------------------------
  Return true if the dataset has any spatial reference fields
}
function TDatasetAssistor.HasSpatialFields(ADataset: TDataset): Boolean;
var
  i: integer;
  lPrefix: string;
  lHasSpatialRef, lHasSystem, lHasLat, lHasLong: boolean;
begin
  lHasSpatialRef := false;
  lHasSystem := false;
  lHasLat := false;
  lHasLong := false;
  for i := 0 to ADataset.Fields.Count-1 do
    if IsSpatialField(ADataset.Fields[i].FieldName) then begin
      lPrefix := GetSpatialName(ADataset.Fields[i].FieldName);
      lHasSpatialRef := true;
      break; // from loop
    end;
  // double check we also have a system, lat and long
  if lHasSpatialRef then
    for i := 0 to ADataset.Fields.Count-1 do begin
      lHasSystem := lHasSystem or SameText(ADataset.Fields[i].FieldName, lPrefix + 'SPATIAL_REF_SYSTEM')
          or SameText(ADataset.Fields[i].FieldName, lPrefix + 'SPATIAL_REF_SYSTEM');
      lHasLat := lHasLat or SameText(ADataset.Fields[i].FieldName, lPrefix + 'LAT');
      lHasLong := lHasLong or SameText(ADataset.Fields[i].FieldName, lPrefix + 'LONG');
    end;
  Result := lHasSpatialRef and lHasSystem and lHasLat and lHasLong;
end;

{-------------------------------------------------------------------------------
  Returns the name of the specified spatial reference field
}
function TDatasetAssistor.GetSpatialFieldName(ADataset: TDataset;
  const FieldType: TSpatialReferenceField): string;
var
  lSuffix: string;
  i: integer;
begin
  Result := '';

  case FieldType of
    srfDefault: lSuffix := SPATIAL_REF;
    srfSystem:  lSuffix := SPATIAL_REF_SYSTEM;
    srfLat:     lSuffix := 'LAT';
    srfLong:    lSuffix := 'LONG';
  end;    // case FieldType

  for i := 0 to Pred(ADataset.Fields.Count) do begin
    if SameText(AnsiRightStr(ADataset.Fields[i].FieldName, Length(lSuffix)), lSuffix) then begin
      Result := ADataset.Fields[i].FieldName;
      Break;
    end;    // if
  end;    // for i := 0 to Pred(ADataset.Fields.Count)
end;    // TDatasetAssistor.GetSpatialFieldName

//------------------------------------------------------------------------------
procedure TDatasetAssistor.SetVagueDateFields(iField: TField;
  iVagueDate: TVagueDate);
var
  lstFieldStart : string;
begin
  lstFieldStart := GetVagueDateName(iField.FieldName);
  iField.Dataset.FieldByName(lstFieldStart + 'VAGUE_DATE_START').AsInteger :=
                     Trunc(iVagueDate.StartDate);
  iField.Dataset.FieldByName(lstFieldStart + 'VAGUE_DATE_END').AsInteger :=
                     Trunc(iVagueDate.EndDate);
  iField.Dataset.FieldByName(lstFieldStart + 'VAGUE_DATE_TYPE').AsString :=
                     iVagueDate.DateTypeString;
end;  // SetVagueDateFields

//------------------------------------------------------------------------------
{ Handler for SetField for vague date fields.  Reads the vague date string as
    input, then uses SetVagueDateFields to update the dataset }
procedure TDatasetAssistor.SetVagueText(Sender: TField;
  const Text: string);
var
  lVagueDate : TVagueDate;
begin
  if not IsVagueDate(Text) then
    raise TExceptionPath.CreateNonCritical(ResStr_InvalidVagueDate)
  else
  begin
    lVagueDate := StringToVagueDate(Text);
    SetVagueDateFields(Sender, lVagueDate);
  end;
end;  // SetVagueText

//------------------------------------------------------------------------------
{ Returns true if the table is a dictionary table, and therefore should be in
     nbndict.mdb }
function TDatasetAssistor.IsInDictFile(const iTableName : string) : Boolean;
begin
  Result := FDictTables.IndexOf(iTableName)<>-1;
end;

{-------------------------------------------------------------------------------
  Returns the name of the longitude field in the dataset. If there are 2 then
    only returns 1 }
function TDatasetAssistor.LatOrLongFieldName(ADataset: TDataset; const
    ALatOrLong: string): string;
var
  i: integer;
  lPrefix: string;
  lHasSpatialRef: boolean;
begin
  lHasSpatialRef := false;
  for i := 0 to ADataset.Fields.Count-1 do
    if IsSpatialField(ADataset.Fields[i].FieldName) then begin
      lPrefix := GetSpatialName(ADataset.Fields[i].FieldName);
      lHasSpatialRef := true;
      break; // from loop
    end;
  if (ADataset.FieldList.IndexOf(lPrefix + ALatOrLong)>-1) and lHasSpatialRef then
    Result := lPrefix + ALatOrLong
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  Returns the name of the longitude field in the dataset. If there are 2 then
    only returns 1 }
function TDatasetAssistor.LongitudeFieldName(ADataset: TDataset): string;
begin
  Result := LatOrLongFieldName(ADataset, 'LONG');
end;

{-------------------------------------------------------------------------------
  Returns the name of the longitude field in the dataset. If there are 2 then
    only returns 1 }
function TDatasetAssistor.LatitudeFieldName(ADataset: TDataset): string;
begin
  Result := LatOrLongFieldName(ADataset, 'LAT');
end;

//==============================================================================
//==============================================================================
{ TJNCCNamesQuery }
//==============================================================================
{ Calls the method that the GetCommonNameForOccurrenceKey event handler has been
  set to.
  - If the method returns a non-empty string, then "Text" is set to this value.
  - If the method returns an empty string, or if the method pointer is not assigned,
    then the value for Text is left as its initial value }
procedure TJNCCNamesQuery.GetCommonNameForOccurrenceKey(Sender: TField;
  var Text: string; DisplayText: Boolean);
var lKey: string;
begin
  if Self.Active and Assigned(FOnRequestNameByOccurrence) then begin
    lKey := Self.FieldByName('OCCURRENCE KEY').AsString;
    FOnRequestNameByOccurrence(Self, lKey);
    if (lKey <> '') then
      Text := lKey;
  end; // if query active and method pointer assigned
end;  // GetCommonNameForOccurrenceKey

//------------------------------------------------------------------------------
{ Calls the method that the OnRequestNameByListItem event handler has been set
  to.
  - If the method returns a non-empty string, then "Text" is set to this value.
  - If the method returns an empty string, or if the method pointer is not assigned,
    then the value for Text is left as its initial value }
procedure TJNCCNamesQuery.GetCommonNameForTaxonListItemKey(Sender: TField;
  var Text: string; DisplayText: Boolean);
var lKey: string;
begin
  if Self.Active and Assigned(FOnRequestNameByListItem) then begin
    lKey := Self.FieldByName('TAXON LIST ITEM KEY').AsString;
    FOnRequestNameByListItem(Self, lKey);
    if (lKey <> '') then
      Text := lKey;
  end; // if query active and method pointer assigned
end;  // GetCommonNameForTaxonListItemKey

//------------------------------------------------------------------------------
{ Handles the display of Taxon Common Names in the following manner:
          1) If the query is set to display common names for taxa, then a check
             is made to see if a field for the common names is returned by the
             query;
          2) Then (if a common name field is present), then the OnGetText event
             handler for the commmon name field is set, depending upon the field
             to act as the source for the common name;
          3) - If the Taxon List Item Key is present, then its value will be used
             preferentially to find the common name. In this case the OnGetText
             event handler is set as GetCommonNameForTaxonListItemKey.
             - Alternatively, if a Taxon Occurrence Key field is present, then the
             event handler for the common name field is set to GetCommonNameForOccurrenceKey.
             - If neither of the above fields are present, then the event handler
               is set to nil and thus the original contents of the common name
               field shall be returned. }
procedure TJNCCNamesQuery.InternalOpen;
var i, j: integer;
begin
  inherited InternalOpen;
  if FDisplayCommonNames then begin

    { Look for common name field }
    for i := 0 to Self.FieldCount-1 do begin
      if (UpperCase(Self.Fields[i].FieldName) = 'TAXON COMMON NAME') then begin

        { Now look for a field to act as the source for the common names }
        // Use TAXON_LIST_ITEM_KEY preferentially
        for j := 0 to Self.FieldCount-1 do begin
          if UpperCase(Self.Fields[j].FieldName) = 'TAXON LIST ITEM KEY' then begin
            FieldByName('TAXON COMMON NAME').OnGetText := GetCommonNameForTaxonListItemKey;
            Exit;
          end; // if Field found
        end; // for

        // Use TAXON_OCCURRENCE_KEY if we can't get the Taxon List Item Key directly
        for j := 0 to Self.FieldCount-1 do begin
          if UpperCase(Self.Fields[j].FieldName) = 'OCCURRENCE KEY' then begin
            FieldByName('TAXON COMMON NAME').OnGetText := GetCommonNameForOccurrenceKey;
            Exit;
          end; // if field found
        end; // for

        { If neither Taxon_List_Item_Key or Taxon_Occurrence_Key are available,
          then set the method for the OnGetText handler to nil }
        FieldByName('TAXON COMMON NAME').OnGetText := nil;

      end; // if common name field found
    end; // for
  end; // if FDisplayCommonNames
end;  // TJNCCNamesQuery.InternalOpen

//------------------------------------------------------------------------------
procedure TJNCCNamesQuery.SetDisplayCommonNames(Value: boolean);
begin
  FDisplayCommonNames := Value;
end;  // SetDisplayCommonNames

initialization
  gAssistor := TDatasetAssistor.Create;

finalization
  gAssistor.Free;

//==============================================================================
end.

