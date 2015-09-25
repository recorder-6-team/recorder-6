//==============================================================================
//  Unit: ExportFilters
//
//  Description: Defines an object to represent an Export Filter, allowing
//    data to be exported filtered by a number of Surveys and Taxonomic
//    Groups specified by the User.
//
//  Author: Michael Bailey
//  Created: Nov 2002
//
//  Last Revision Details:
//    $Revision: 39 $
//    $Date: 8/04/09 15:43 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit ExportFilters;

interface

uses
  SysUtils, Classes, Menus, DataClasses, ExceptionForm, SpatialRefFuncs, Constants;

type
  EExportFilterError = class(TExceptionPath);

  TExportFilter = class
  private
    FIncludeLocations: Boolean;
    FPersisted: Boolean;
    FIncludeOccurrences: Boolean;
    FIncludeNames: Boolean;
    FFilterName: string;
    FFilterKey: TKeyString;
    FFilterSurveys: TStringList;
    FFilterSurveyTags: TStringList;
    FFilterTaxa: TList;
    FKeyList: TEditableKeyList;
    FFilterDate: TDateTime;
    FHasEndDate: Boolean;
    FHasStartDate: Boolean;
    FHasBoundingBox: Boolean;
    FNELong: Double;
    FSWLong: Double;
    FNELat: Double;
    FSWLat: Double;
    FObsDateStart: TDateTime;
    FObsDateEnd: TDateTime;
    FHasLastExportDate: Boolean;
    FLastExportDate: Tdatetime;

    FUserAccessLevel: TUserAccessLevel;
    FExportConfidentialOccurrences: boolean;
    procedure SetFilterName(const Value: string);
    procedure SetIncludeLocations(const Value: Boolean);
    procedure SetIncludeNames(const Value: Boolean);
    procedure SetIncludeOccurrences(const Value: Boolean);
    function GetSurveyKey(Index: Integer): TKeyString;
    function GetSurveyName(Index: Integer): string;
    function GetSurveyCount: Integer;
    function GetSurveyTagCount: Integer;
    function GetSurveyTagName(Index: Integer): String;
    function GetSurveyTagKey(Index: Integer): TKeyString;
    function GetTaxonCount: Integer;
    function GetTaxonKey(Index: Integer): TKeyString;
    function GetTaxonNames(Index: Integer): TTaxonNames;
    procedure LoadSurveys;
    procedure LoadTags;
    procedure LoadTaxa;
    procedure InsertFilter;
    procedure UpdateFilter;
    procedure SaveRelations;
    procedure PopulateKeyList(AKeyList: TEditableKeyList; ADate: TDateTime);
    function ListSurveyKeys: string;
    function ListTaxonKeys: string;
    procedure GetRelevantLists(out ASurveyList, ATaxonList : string);
    function GetTaxonOccurrencesSQL(const ASurveyList, ATaxonList: string): string;
    procedure PopulateWithLocations(const ASurveyList : string);
    procedure PopulateWithIndividuals;
    procedure PopulateWithOrganisations;
    procedure AddToKeyList(const ASQLText: string);
    function ReplaceDates(const SQLText: string): string;
    procedure SetHasBoundingBox(const Value: Boolean);
    procedure SetHasEndDate(const Value: Boolean);
    procedure SetHasStartDate(const Value: Boolean);
    procedure SetNELat(const Value: Double);
    procedure SetNELong(const Value: Double);
    procedure SetObsDateEnd(const Value: TDateTime);
    procedure SetObsDateStart(const Value: TDateTime);
    procedure SetSWLat(const Value: Double);
    procedure SetSWLong(const Value: Double);
    function GetNELat: Double;
    function GetNELong: Double;
    function GetObsDateEnd: TDateTime;
    function GetObsDateStart: TDateTime;
    function GetSWLat: Double;
    function GetSWLong: Double;
    function GetNELatString: string;
    function GetNELongString: string;
    function GetObsDateEndString: string;
    function GetObsDateStartString: string;
    function GetSWLatString: string;
    function GetSWLongString: string;
    function GetNELatLong: TLatLong;
    function GetSWLatLong: TLatLong;
    procedure SetNELatLong(const Value: TLatLong);
    procedure SetSWLatLong(const Value: TLatLong);
    function GetBiotopeOccurrencesSQL(const ASurveyList: string): string;
  public
    class function CreateFilterList: TStringList;
    class function CreateAllFilters: TList;
    class procedure SetExportFilterMenu(AMenuItem: TMenuItem; ANotifyEvent: TNotifyEvent);
    class procedure UpdateFilterList;
    class function CopyTaxonNames(ATaxonNames: TTaxonNames): TTaxonNames;
    class function LoadFromFile(const AFileName: TFileName): TExportFilter;
    class function FilterExists(const AFilterKey: TKeyString): Boolean;
    constructor Create;
    constructor CreateFromDatabase(const AKey: TKeyString);
    constructor CreateNew(const AName: string; AIncludeOccurrences: Boolean = True;
      AIncludeLocations: Boolean = True; AIncludeNames: Boolean = True);
    destructor Destroy; override;
    function AddSurvey(const ASurveyName: string; const ASurveyKey: TKeyString): Integer;
    function AddLoadSurvey(const ASurveyKey: TKeyString): Integer;
    function AddSurveyTag(const tagName: String; const conceptKey: TKeyString): Integer;
    function AddLoadSurveyTag(const conceptKey: TKeyString): Integer;
    function AddTaxon(const ATLIKey: TKeyString): Integer;
    function AddTaxonCopy(ATaxonNames: TTaxonNames): Integer;
    procedure DeleteSurvey(Index: Integer); overload;
    procedure DeleteSurvey(const ASurveyKey: TKeyString); overload;
    procedure DeleteTaxon(Index: Integer); overload;
    procedure DeleteTaxon(const ATaxonKey: TKeyString); overload;
    function SurveyIndexOf(const ASurveyKey: TKeyString): Integer;
    function TaxonIndexOf(const ATaxonKey: TKeyString): Integer;
    procedure ClearSurveys;
    procedure ClearSurveyTags;
    procedure ClearTaxa;
    procedure SaveFilter;
    procedure Delete;
    procedure SaveToFile(const AFileName: TFileName);
    procedure PopulateFromStrings(AStrings: TStrings);
    procedure SaveToStrings(AStrings: TStrings);
    procedure UpdateLastExportDate(idtDateTime : TDateTime);
    function KeyList(ADate: TDateTime): TEditableKeyList;
    property FilterName: string read FFilterName write SetFilterName;
    property FilterKey: TKeyString read FFilterKey;
    property IncludeOccurrences: Boolean read FIncludeOccurrences write SetIncludeOccurrences;
    property IncludeLocations: Boolean read FIncludeLocations write SetIncludeLocations;
    property IncludeNames: Boolean read FIncludeNames write SetIncludeNames;
    property Persisted: Boolean read FPersisted;
    property SurveyName[Index: Integer]: string read GetSurveyName;
    property SurveyKey[Index: Integer]: TKeyString read GetSurveyKey;
    property SurveyCount: Integer read GetSurveyCount;
    property SurveyTagCount: Integer read GetSurveyTagCount;
    property SurveyTagName[Index: Integer]: String read GetSurveyTagName;
    property SurveyTagKey[Index: Integer]: TKeyString read GetSurveyTagKey;
    property TaxonNames[Index: Integer]: TTaxonNames read GetTaxonNames;
    property TaxonKey[Index: Integer]: TKeyString read GetTaxonKey;
    property TaxonCount: Integer read GetTaxonCount;
    property HasBoundingBox: Boolean read FHasBoundingBox write SetHasBoundingBox;
    property SWLat: Double read GetSWLat write SetSWLat;
    property SWLong: Double read GetSWLong write SetSWLong;
    property SWLatLong: TLatLong read GetSWLatLong write SetSWLatLong;
    property NELat: Double read GetNELat write SetNELat;
    property NELong: Double read GetNELong write SetNELong;
    property NELatLong: TLatLong read GetNELatLong write SetNELatLong;
    property HasStartDate: Boolean read FHasStartDate write SetHasStartDate;
    property ObsDateStart: TDateTime read GetObsDateStart write SetObsDateStart;
    property HasEndDate: Boolean read FHasEndDate write SetHasEndDate;
    property ObsDateEnd: TDateTime read GetObsDateEnd write SetObsDateEnd;
    property HasLastExportDate : Boolean read FHasLastExportDate;
    property LastExportDate : TDatetime read FLastExportDate;

    property UserAccessLevel: TUserAccessLevel read FUserAccessLevel write FUserAccessLevel;
    property ExportConfidentialOccurrences: Boolean read FExportConfidentialOccurrences
        write FExportConfidentialOccurrences;
  end;

  // All instances to appear mnuToolsExportUsingFilter defined in Maintbar.
  // Each item corresponds to a filter indicated by the Key property.
  TExportFilterMenuItem = class(TMenuItem)
  private
    FKey: TKey;
  public
    constructor Create(AOwner: TComponent; const AKey: TKeyString); reintroduce;
    destructor Destroy; override;
    property Key: TKey read FKey;
  end;

//==============================================================================
implementation

uses
  DatabaseAccessADO, GeneralData, ApplicationSettings, ExportFilterSQL, Variants, GeneralFunctions,
  Windows, ADOInt;

const
  FIlTER_LIST    = 'SELECT Export_Filter_Key, Item_Name FROM Export_Filter';

  LOAD_SQL       = 'SELECT Item_Name, Include_Occurrences, ' +
                    'Include_Locations, Include_Names, ' +
                    'SW_Lat, SW_Long, NE_Lat, NE_Long, ' +
                    'Obs_Date_Start, Obs_Date_End, Last_Export_Date ' +
                   'FROM Export_Filter WHERE Export_Filter_Key = ''%s''';

  LOAD_TAXA      = 'SELECT * ' +
                   'FROM Index_Taxon_Name I INNER JOIN Export_Filter_Taxon E ' +
                   '  ON E.Taxon_List_Item_Key = I.Taxon_List_Item_Key ' +
                   'WHERE E.Export_Filter_Key = ''%s''';

  INSERT_FILTER  = 'INSERT INTO Export_Filter ' +
                   '  (Export_Filter_Key, Item_Name, ' +
                   '   Include_Occurrences, Include_Locations, Include_Names, ' +
                   '   SW_Lat, SW_Long, NE_Lat, NE_Long, ' +
                   '   Obs_Date_Start, Obs_Date_End) ' +
                   'VALUES (''%s'', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)';

  UPDATE_FILTER  = 'UPDATE Export_Filter ' +
                   'SET Item_Name = %s, ' +
                   '    Include_Occurrences = %s, Include_Locations = %s, Include_Names = %s, ' +
                   '    SW_Lat = %s, SW_Long = %s, NE_Lat = %s, NE_Long = %s, ' +
                   '    Obs_Date_Start = %s, Obs_Date_End = %s ' +
                   'WHERE Export_Filter_Key = ''%s''';

  DELETE_FILTER  = 'DELETE FROM Export_Filter WHERE Export_Filter_Key = ''%s''';

  DELETE_SURVEYS = 'DELETE FROM Export_Filter_Survey WHERE Export_Filter_Key = ''%s''';

  DELETE_TAXA    = 'DELETE FROM Export_Filter_Taxon WHERE Export_Filter_Key = ''%s''';

  INSERT_SURVEY  = 'INSERT INTO Export_Filter_Survey ' +
                   '  (Export_Filter_Key, Survey_Key) ' +
                   'VALUES (''%s'', ''%s'')';

  INSERT_TAXON   = 'INSERT INTO Export_Filter_Taxon ' +
                   '  (Export_Filter_Key, Taxon_List_Item_Key) ' +
                   'VALUES (''%s'', ''%s'')';

  SQL_UPDATE_LAST_EXPORT_DATE = 'UPDATE Export_Filter '+
                                'SET Last_Export_Date=''%s'' '+
                                'WHERE Export_Filter_Key=''%s''';

resourcestring
  ResStr_InvalidBooleanStr = 'Not a valid boolean string.';
  ResStr_SurveyDoesntExist = 'Survey does not exist.';
  ResStr_ConceptNotFound   = 'Survey Tag does not exist.';
  ResStr_CannotFindExportFilter = 'Export Filter cannot be found in the database.';
  ResStr_InvalidFilterFile =  '"%s" is not a valid filter file.';
  ResStr_InvalidFilterKey = 'Invalid Filter Key.';


// Variables hold the MenuItem under which the TExportFilterMenuItem objects appear
// and the TNotifyEvent which they should call when clicked.
var mMenuItem: TMenuItem;
    mNotifyEvent: TNotifyEvent;

//==============================================================================
function StrToBool(AString: string): Boolean;
begin
  AString := UpperCase(AString);
  Result := (AString = 'T') or (AString = 'TRUE');
  if not Result then
    if not ((AString = 'F') or (AString = 'FALSE')) then
      raise EConvertError.Create(ResStr_InvalidBooleanStr);
end;

//==============================================================================
//==============================================================================
{ TExportFilter }
//==============================================================================
// Adds a Survey to the Filter by key alone - database must be searched for the Survey Name.
function TExportFilter.AddLoadSurvey(const ASurveyKey: TKeyString): Integer;
begin
  with dmDatabase.GetRecordset('usp_Survey_Select', ['@Key', ASurveyKey]) do
  begin
    if Eof then
      raise TExceptionPath.Create(ResStr_SurveyDoesntExist)
    else
      Result := AddSurvey(Fields['Display_Name'].Value, ASurveyKey);
    Close;
  end;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.AddLoadSurveyTag(const conceptKey: TKeyString): Integer;
begin
  with dmDatabase.GetRecordset('usp_Concept_Select', ['@ConceptKey', conceptKey]) do
  begin
    if Eof then
      raise TExceptionPath.Create(ResStr_ConceptNotFound)
    else
      Result := AddSurveyTag(Fields['Item_Name'].Value, conceptKey);
    Close;
  end;
end;

{-------------------------------------------------------------------------------
 Adds a Survey to the Filter - Surveys are stored in a TStringList with the
 Survey Name as the string and the surveykey stored as a TKey in the
 associated objects.
}
function TExportFilter.AddSurvey(const ASurveyName: string; const ASurveyKey: TKeyString): Integer;
begin
  Result := SurveyIndexOf(ASurveyKey);
  if Result = -1 then Result := FFilterSurveys.AddObject(ASurveyName, TKey.Create(ASurveyKey));
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.AddSurveyTag(const tagName: string; const conceptKey: TKeyString): Integer;
begin
  Result := FFilterSurveyTags.IndexOfName(conceptKey);
  if Result = -1 then Result := FFilterSurveyTags.Add(conceptKey + '=' + tagName);
end;

{-------------------------------------------------------------------------------
 Adds a Taxonomic Group to the Filter - Taxa are stored in a TList using
 the TTaxonNames property.
}
function TExportFilter.AddTaxon(const ATLIKey: TKeyString): Integer;
begin
  if TaxonIndexOf(ATLIKey) = -1 then
    Result := FFilterTaxa.Add(dmGeneralData.GetTaxonNamesObject(
            dmGeneralData.GetTaxonPreferredKey(ATLIKey)))
  else
    Result := -1;
end;

{-------------------------------------------------------------------------------
 Creates a copy of TTaxonNames and adds it to the Filter.
}
function TExportFilter.AddTaxonCopy(ATaxonNames: TTaxonNames): Integer;
begin
  if TaxonIndexOf(ATaxonNames.TaxonListItemKey) = -1 then
    Result := FFilterTaxa.Add(CopyTaxonNames(ATaxonNames))
  else
    Result := -1;
end;

{-------------------------------------------------------------------------------
  Description : Fills the keylist with the results of the supplied query.  The
       query must contain an ItemKey and TableName parameter.
  Created : 05/05/2003 }
procedure TExportFilter.AddToKeyList(const ASQLText: string);
begin
  with dmDatabase.ExecuteSQL(ASQLText, True) do
  begin
    FKeyList.Capacity := FKeyList.Capacity + RecordCount;
    while not Eof do
    begin
      FKeyList.AddItem(
          VarToStr(Fields['ItemKey'].Value),
          VarToStr(Fields['TableName'].Value));
      MoveNext;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.ClearSurveys;
begin
  while FFilterSurveys.Count > 0 do DeleteSurvey(0);
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.ClearSurveyTags;
begin
  FFilterSurveyTags.Clear;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.ClearTaxa;
begin
  while FFilterTaxa.Count > 0 do DeleteTaxon(0);
end;

{-------------------------------------------------------------------------------
 Returns a copy of the passed in TTaxonNames object.
}
class function TExportFilter.CopyTaxonNames(ATaxonNames: TTaxonNames): TTaxonNames;
begin
  Result := TTaxonNames.Create;
  try
    Result.TaxonListItemKey := ATaxonNames.TaxonListItemKey;
    Result.TaxonName        := ATaxonNames.TaxonName;
    Result.CommonName       := ATaxonNames.CommonName;
    Result.EnteredName      := ATaxonNames.EnteredName;
    Result.TNItalic         := ATaxonNames.TNItalic;
    Result.CNItalic         := ATaxonNames.CNItalic;
    Result.ENItalic         := ATaxonNames.ENItalic;
    Result.TNAttribute      := ATaxonNames.TNAttribute;
    Result.CNAttribute      := ATaxonNames.CNAttribute;
    Result.ENAttribute      := ATaxonNames.ENAttribute;
    Result.TNAuthor         := ATaxonNames.TNAuthor;
    Result.ENAuthor         := ATaxonNames.ENAuthor;
  except
    on E: Exception do begin
      Result.Free;
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
constructor TExportFilter.Create;
begin
  FPersisted := False;
  FFilterSurveys := TStringList.Create;
  FFilterSurveyTags := TStringList.Create;
  FFilterTaxa := TList.Create;
end;

{-------------------------------------------------------------------------------
 Returns a complete list of all Export Filters in the database.
}
class function TExportFilter.CreateAllFilters: TList;
var k: Integer;
    lFilterList: TStringList;
begin
  lFilterList := CreateFilterList;
  try
    Result := TList.Create;
    try
      for k := 0 to lFilterList.Count - 1 do
        Result.Add(CreateFromDatabase(TKey(lFilterList.Objects[k]).Key));
    except
      on E: Exception do begin
        Result.Free;
        raise;
      end;
    end;
  finally
    for k := 0 to lFilterList.Count - 1 do
      lFilterList.Objects[k].Free;
    lFilterList.Free
  end;
end;

{-------------------------------------------------------------------------------
 Returns a TStringList of Filters with the Filter Name as the string and the
 Filter Key stored as a TKey in the associated object.
}
class function TExportFilter.CreateFilterList: TStringList;
begin
  Result := TStringList.Create;
  try
    with dmDatabase.ExecuteSQL(FILTER_LIST, True) do
    begin
      Result.Capacity := RecordCount;
      while not EOF do
      begin
        Result.AddObject(
            VarToStr(Fields['Item_Name'].Value),
            TKey.Create(VarToStr(Fields['Export_Filter_Key'].Value)));
        MoveNext;
      end;
    end;
    Result.Sort;
  except
    on E: Exception do begin
      Result.Free;
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
 Creates a Filter already extant in the database.
}
constructor TExportFilter.CreateFromDatabase(const AKey: TKeyString);
var
  ltfNullConvert : boolean;
begin
  with dmGeneralData.qryAllPurpose do
  begin
    ParseSQL := false;
    SQL.Text := Format(LOAD_SQL, [AKey]);
    Open;
    ltfNullConvert := NullStrictConvert;
    NullStrictConvert := false;
    try
      if RecordCount = 1 then
      begin
        FFilterKey := AKey;
        FFilterName := FieldValues['Item_Name'];
        FIncludeOccurrences := FieldValues['Include_Occurrences'];
        FIncludeLocations := FieldValues['Include_Locations'];
        FIncludeNames := FieldValues['Include_Names'];
        FHasBoundingBox := not FieldByName('SW_Lat').IsNull;
        if FHasBoundingBox then
        begin
          FSWLat  := FieldValues['SW_Lat'];
          FSWLong := FieldValues['SW_Long'];
          FNELat  := FieldValues['NE_Lat'];
          FNELong := FieldValues['NE_Long'];
        end;
        FHasStartDate := not FieldByName('Obs_Date_Start').IsNull;
        if HasStartDate then FObsDateStart := FieldValues['Obs_Date_Start'];
        FHasEndDate := not FieldByName('Obs_Date_End').IsNull;
        if HasEndDate then FObsDateEnd := FieldValues['Obs_Date_End'];
        FHasLastExportDate := not FieldByName('Last_Export_Date').IsNull;
        if HasLastExportDate then FLastExportDate := FieldValues['Last_Export_Date'];
      end else
        raise EExportFilterError.Create(ResStr_CannotFindExportFilter);
    finally
      NullStrictConvert := ltfNullConvert;
      Close;
      ParseSQL := true;
    end;
  end;
  FFilterSurveys := TStringList.Create;
  LoadSurveys;
  FFilterSurveyTags := TStringList.Create;
  LoadTags;
  FFilterTaxa := TList.Create;
  LoadTaxa;
  FPersisted := True;
end;

{-------------------------------------------------------------------------------
}
constructor TExportFilter.CreateNew (const AName: string; AIncludeOccurrences,
  AIncludeLocations, AIncludeNames: Boolean);
begin
  FFilterName := AName;
  FIncludeOccurrences := AIncludeOccurrences;
  FIncludeLocations := AIncludeLocations;
  FIncludeNames := AIncludeNames;
  FPersisted := False;
  FFilterSurveys := TStringList.Create;
  FFilterSurveyTags := TStringList.Create;
  FFilterTaxa := TList.Create;
end;

{-------------------------------------------------------------------------------
 This removes the Filter from the Database.
}
procedure TExportFilter.Delete;
begin
  if FPersisted then begin
    with dmGeneralData.qryAllPurpose do begin
      ParseSQL := false;
      SQL.Text := Format(DELETE_SURVEYS, [FFilterKey]);
      ExecSQL;
      dmDatabase.RunDeleteStoredProc('usp_ExportFilterTag_Delete_ForExportFilter', ['@Key', FFilterKey]);
      SQL.Text := Format(DELETE_TAXA, [FFilterKey]);
      ExecSQL;
      SQL.Text := Format(DELETE_FILTER, [FFilterKey]);
      ExecSQL;
      ParseSQL := true;
    end;
    FFilterKey := '';
    FPersisted := False;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.DeleteSurvey(const ASurveyKey: TKeyString);
begin
  DeleteSurvey(SurveyIndexOf(ASurveyKey));
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.DeleteSurvey(Index: Integer);
begin
  FFilterSurveys.Objects[Index].Free;
  FFilterSurveys.Delete(Index);
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.DeleteTaxon(Index: Integer);
begin
  TTaxonNames(FFilterTaxa[Index]).Free;
  FFilterTaxa.Delete(Index);
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.DeleteTaxon(const ATaxonKey: TKeyString);
begin
  DeleteTaxon(TaxonIndexOf(ATaxonKey));
end;

{-------------------------------------------------------------------------------
}
destructor TExportFilter.Destroy;
var k: Integer;
begin
  if Assigned (FFilterSurveys) then
  begin
    for k := FFilterSurveys.Count - 1 downto 0 do
      FFilterSurveys.Objects[k].Free;
    FFilterSurveys.Free;
  end;
  FreeAndNil(FFilterSurveyTags);
  if Assigned (FFilterTaxa) then
  begin
    for k := FFilterTaxa.Count - 1 downto 0 do
      TTaxonNames(FFilterTaxa[k]).Free;
    FFilterTaxa.Free;
  end;
  inherited;
end;

{-------------------------------------------------------------------------------
 Returns True if there is a Filter for the specified key in the database.
}
class function TExportFilter.FilterExists(const AFilterKey: TKeyString): Boolean;
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(LOAD_SQL, [AFilterKey]);
    Open;
    try
      Result := RecordCount = 1;
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetNELat: Double;
begin
  if HasBoundingBox then Result := FNELat else Result := 0;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetNELatLong: TLatLong;
begin
  Result.Lat := NELat;
  Result.Long := NELong;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetNELatString: string;
begin
  if HasBoundingBox then Result := FloatToStr(FNELat) else Result := 'Null';
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetNELong: Double;
begin
  if HasBoundingBox then Result := FNELong else Result := 0;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetNELongString: string;
begin
  if HasBoundingBox then Result := FloatToStr(FNELong) else Result := 'Null';
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetObsDateEnd: TDateTime;
begin
  if HasEndDate then Result := FObsDateEnd else Result := 0;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetObsDateEndString: string;
begin
  if HasEndDate then
    Result := IntToStr(Trunc(ObsDateEnd))
  else
    Result := 'Null';
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetObsDateStart: TDateTime;
begin
  if HasStartDate then Result := FObsDateStart else Result := 0;
end;

{-------------------------------------------------------------------------------
}
function TExportFilter.GetObsDateStartString: string;
begin
  if HasStartDate then
    Result := IntToStr(Trunc(ObsDateStart))
  else
    Result := 'Null';
end;

//==============================================================================
// Used in the export procedure.
// Creates a list of Survey keys, Survey_Event keys, Sample keys and
// Taxon_List_Items keys that are relevant to the given export filter.
// These keys represent what might be included but have not yet considered
// the Filter Date.
procedure TExportFilter.GetRelevantLists(out ASurveyList, ATaxonList: String);
begin
  if (SurveyCount > 0) or (SurveyTagCount > 0) then
    ASurveyList := ListSurveyKeys
  else
    ASurveyList := '';
  if TaxonCount > 0 then
    ATaxonList := ListTaxonKeys
  else
    ATaxonList := '';
end;

//------------------------------------------------------------------------------
function TExportFilter.GetTaxonOccurrencesSQL(const ASurveyList, ATaxonList:
    string): string;
var lstSQLJoin, lstSQLWhere: string;
    lIncludeSampleJoin,
    lIncludeEventJoin,
    lIncludeDeterminationJoin,
    lIncludeTaxaJoin : boolean;
    lFormatSettings: TFormatSettings;
begin
  // initialisation
  lIncludeSampleJoin :=
      HasBoundingBox or HasStartDate or HasEndDate or (ASurveyList <> '') or (FFilterDate <> 0);
  lIncludeEventJoin         := (ASurveyList <> '') or (FFilterDate <> 0);
  lIncludeDeterminationJoin := ATaxonList <> '';
  lIncludeTaxaJoin          := ATaxonList <> '';
  lstSQLWhere := '';
  lstSQLJoin := '';
  // first find extra joins and where clauses we need
  if ASurveyList <> '' then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_SURVEY_WHERE, [ASurveyList]));
  if FFilterDate <> 0 then
    lIncludeDeterminationJoin := True;
  if ATaxonList <> '' then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_TAXA_WHERE, [ATaxonList]));
  if HasBoundingBox then begin
    // VI 16851 - Must use '.' as decimal separator in SQL, not the current locale's value
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, lFormatSettings);
    lFormatSettings.DecimalSeparator := '.';
    AddToSQLWhereClause(lstSQLWhere, Format(
        SQL_LAT_LONG_WHERE,
        [FloatToStr(SWLat, lFormatSettings), FloatToStr(NELat, lFormatSettings),
        FloatToStr(SWLong, lFormatSettings), FloatToStr(NELong, lFormatSettings)]));
  end;    // if HasBoundingBox
  if HasStartDate then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_OBS_DATE_START_WHERE, [GetObsDateStartString]));
  if HasEndDate then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_OBS_DATE_END_WHERE, [GetObsDateEndString]));

  // now build the query
  if lIncludeSampleJoin        then lstSQLJoin := lstSQLJoin + SQL_SAMPLE_JOIN;
  if lIncludeEventJoin         then lstSQLJoin := lstSQLJoin + SQL_EVENT_JOIN;
  if lIncludeDeterminationJoin then lstSQLJoin := lstSQLJoin + SQL_TOCC_DETERMINATION_JOIN;
  if lIncludeTaxaJoin then begin
    if AppSettings.UseRecommendedTaxaNames then
      lstSQLJoin := lstSQLJoin + SQL_TOCC_TAXA_JOIN_NAMESERVER
    else
      lstSQLJoin := lstSQLJoin + SQL_TOCC_TAXA_JOIN;
  end;
  if FFilterDate <> 0 then begin
    lstSQLJoin := lstSQLJoin + SQL_TOCC_DATE_JOIN + SQL_SAMPLES_DATE_JOIN + SQL_EVENTS_DATE_JOIN;
    AddToSQLWhereClause(lstSQLWhere, '(' + ReplaceDates(SQL_TOCC_DATE_WHERE + ' OR ' +
        SQL_SAMPLES_DATE_WHERE + ' OR ' + SQL_EVENTS_DATE_WHERE) + ')');
  end;

  if not ((UserAccessLevel = ualAdmin) and ExportConfidentialOccurrences) then
    AddToSQLWhereClause(lstSQLWhere, 'xocc.Confidential=0');
  Result := SQL_TOCC_SELECT + lstSQLJoin + lstSQLWhere;
end;  // GetTaxonOccurrencesSQL

//------------------------------------------------------------------------------
function TExportFilter.GetBiotopeOccurrencesSQL(const ASurveyList: string):
    string;
var lstSQLJoin, lstSQLWhere: string;
    lIncludeSampleJoin,
    lIncludeEventJoin,
    lIncludeDeterminationJoin : boolean;
    lFormatSettings: TFormatSettings;
begin
  // initialisation
  lIncludeSampleJoin :=
      HasBoundingBox or HasStartDate or HasEndDate or (ASurveyList <> '') or (FFilterDate <> 0);
  lIncludeEventJoin := (ASurveyList <> '') or (FFilterDate <> 0);
  lstSQLWhere := '';
  lstSQLJoin := '';
  // first find the extra joins and where clauses we need
  if ASurveyList <> '' then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_SURVEY_WHERE, [ASurveyList]));
  if HasBoundingBox then begin
    // VI 16851 - Must use '.' as decimal separator in SQL, not the current locale's value
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, lFormatSettings);
    lFormatSettings.DecimalSeparator := '.';
    AddToSQLWhereClause(lstSQLWhere, Format(
        SQL_LAT_LONG_WHERE,
        [FloatToStr(SWLat, lFormatSettings), FloatToStr(NELat, lFormatSettings),
        FloatToStr(SWLong, lFormatSettings), FloatToStr(NELong, lFormatSettings)]));
  end;    // if HasBoundingBox
  if HasStartDate then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_OBS_DATE_START_WHERE, [GetObsDateStartString]));
  if HasEndDate then
    AddToSQLWhereClause(lstSQLWhere, Format(SQL_OBS_DATE_END_WHERE, [GetObsDateEndString]));
  lIncludeDeterminationJoin := (FFilterDate <> 0);

  // now build the query
  if lIncludeSampleJoin        then lstSQLJoin := lstSQLJoin + SQL_SAMPLE_JOIN;
  if lIncludeEventJoin         then lstSQLJoin := lstSQLJoin + SQL_EVENT_JOIN;
  if lIncludeDeterminationJoin then lstSQLJoin := lstSQLJoin + SQL_BOCC_DETERMINATION_JOIN;
  if FFilterDate <> 0 then begin
    lstSQLJoin := lstSQLJoin + SQL_BOCC_DATE_JOIN + SQL_SAMPLES_DATE_JOIN + SQL_EVENTS_DATE_JOIN;
    AddToSQLWhereClause(lstSQLWhere, '(' + ReplaceDates(SQL_BOCC_DATE_WHERE + ' OR ' +
        SQL_SAMPLES_DATE_WHERE + ' OR ' + SQL_EVENTS_DATE_WHERE) + ')');
  end;
  Result := SQL_BOCC_SELECT + lstSQLJoin + lstSQLWhere;
end;  // GetBiotopeOccurrencesSQL

//==============================================================================
function TExportFilter.GetSurveyCount: Integer;
begin
  Result := FFilterSurveys.Count;
end;

function TExportFilter.GetSurveyKey(Index: Integer): TKeyString;
begin
  Result := TKey(FFilterSurveys.Objects[Index]).Key;
end;

function TExportFilter.GetSurveyName(Index: Integer): string;
begin
  Result := FFilterSurveys.Strings[Index];
end;

function TExportFilter.GetSurveyTagCount: Integer;
begin
  Result := FFilterSurveyTags.Count;
end;

function TExportFilter.GetSurveyTagName(Index: Integer): String;
begin
  Result := FFilterSurveyTags.ValueFromIndex[Index];
end;

function TExportFilter.GetSurveyTagKey(Index: Integer): TKeyString;
begin
  Result := FFilterSurveyTags.Names[Index];
end;

function TExportFilter.GetSWLat: Double;
begin
  if HasBoundingBox then Result := FSWLat else Result := 0;
end;

function TExportFilter.GetSWLatLong: TLatLong;
begin
  Result.Lat := SWLat;
  Result.Long := SWLong;
end;

function TExportFilter.GetSWLatString: string;
begin
  if HasBoundingBox then Result := FloatToStr(FSWLat) else Result := 'Null';
end;

function TExportFilter.GetSWLong: Double;
begin
  if HasBoundingBox then Result := FSWLong else Result := 0;
end;

function TExportFilter.GetSWLongString: string;
begin
  if HasBoundingBox then Result := FloatToStr(FSWLong) else Result := 'Null';
end;

function TExportFilter.GetTaxonCount: Integer;
begin
  Result := FFilterTaxa.Count;
end;

function TExportFilter.GetTaxonKey(Index: Integer): TKeyString;
begin
  Result := GetTaxonNames(Index).TaxonListItemKey;
end;

function TExportFilter.GetTaxonNames(Index: Integer): TTaxonNames;
begin
  Result := TTaxonNames(FFilterTaxa[Index]);
end;

//==============================================================================
// Commits a new filter to the database.
procedure TExportFilter.InsertFilter;
begin
  if FFilterKey = '' then FFilterKey := dmGeneralData.GetNextKey('EXPORT_FILTER', 'EXPORT_FILTER_KEY');
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(INSERT_FILTER,
                       [FFilterKey, QuotedStr(FFilterName), SQLBoolString[FIncludeOccurrences],
                        SQLBoolString[FIncludeLocations], SQLBoolString[FIncludeNames],
                        GetSWLatString, GetSWLongString, GetNELatString, GetNELongString,
                        GetObsDateStartString, GetObsDateEndString]);
    ExecSQL;
    ParseSQL := true;
  end;
  SaveRelations;
  FPersisted := True;
end;

//==============================================================================
// Returns a TKeyList for exporting.
function TExportFilter.KeyList(ADate: TDateTime): TEditableKeyList;

  {$IFDEF DEBUG_EF}
  procedure TestSaveKeyList(AKeyList: TKeyList);
  { Allows easy examining of the KeyList being produced.  Debug purposes only. }
  var lStringList: TStringList;
      k: Integer;
  begin
    lStringList := TStringList.Create;
    try
      for k := 0 to AKeyList.Header.ItemCount - 1 do
        lStringList.Add(AKeyList.Items[k].KeyField1 + ' : ' + AKeyList.Items[k].KeyField2);
      lStringList.SaveToFile('C:\LastKeyList.txt');
    finally
      lStringList.Free;
    end;
  end;
  {$ENDIF}

begin
  Result := TEditableKeyList.Create;
  try
    Result.ConvertToMixedData;
    PopulateKeyList(Result, ADate);
    {$IFDEF DEBUG_EF}
    TestSaveKeyList(Result);
    {$ENDIF}
  except
    on E: Exception do begin
      Result.Free;
      raise;
    end;
  end;
end;  // KeyList

//==============================================================================
// Used in the export procedure.
// Creates a string representing a List of Survey keys.
function TExportFilter.ListSurveyKeys: string;
var
  k: Integer;
  fromTags: String;
begin
  if SurveyCount > 0 then
    Result := '''' + SurveyKey[0] + '''';
  for k := 1 to SurveyCount - 1 do
    Result := Result + ',''' + SurveyKey[k] + '''';

  // Also get all surveys for the tags.
  fromTags := '';
  if SurveyTagCount > 0 then begin
    for k := 0 to SurveyTagCount - 1 do
      with dmDatabase.GetRecordset(
          'usp_Surveys_Select_ForSurveyTag',
          ['@Key', SurveyTagKey[k], '@UserNameKey', AppSettings.UserID]) do
      begin
        while not Eof do begin
          fromTags := fromTags + ',''' + Fields['Survey_Key'].Value + '''';
          MoveNext;
        end;
        Close;
      end;
  end;
  if fromTags <> '' then
    if Result <> '' then Result := Result + fromTags
                    else Result := Copy(fromTags, 2, Length(fromTags));
end;

// Creates a string representing a List of Taxon_List_Item keys.
function TExportFilter.ListTaxonKeys: string;
var k: Integer;
begin
  if TaxonCount > 0 then
    Result := '''' + TaxonKey[0] + '''';
  for k := 1 to TaxonCount - 1 do
    Result := Result + ',''' + TaxonKey[k] + '''';
end;

// Loads an Export Filter from a file.
class function TExportFilter.LoadFromFile(const AFileName: TFileName): TExportFilter;
var f: TStringList;
begin
  Result := TExportFilter.Create;
  try
    f := TStringList.Create;
    try
      f.LoadFromFile(AFileName);
      if f.Strings[0] <> '[Export Filter]' then
        raise TExceptionPath.Create(Format(ResStr_InvalidFilterFile, [AFileName]));
      Result.PopulateFromStrings(f);
    finally
      f.Free;
    end;
    
    if Length(Result.FFilterKey) <> 16 then
      raise TExceptionPath.Create(ResStr_InvalidFilterKey);
  except
    on E: Exception do begin
      Result.Free;
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.LoadSurveys;
begin
  with dmDatabase.GetRecordset('usp_Surveys_Select_ForExportFilter', ['@Key', FFilterKey]) do
  begin
    while not Eof do begin
      FFilterSurveys.AddObject(
          Fields['Display_Name'].Value,
          TKey.Create(Fields['Survey_Key'].Value));
      MoveNext;
    end;
    Close;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.LoadTags;
begin
  with dmDatabase.GetRecordset('usp_SurveyTags_Select_ForExportFilter', ['@Key', FFilterKey]) do
  begin
    while not Eof do begin
      FFilterSurveyTags.Add(Fields['Concept_Key'].Value + '=' + Fields['PlainText'].Value);
      MoveNext;
    end;
    Close;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TExportFilter.LoadTaxa;
var lTaxonNames: TTaxonNames;
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(LOAD_TAXA, [FFilterKey]);
    Open;
    try
      while not EOF do begin
        lTaxonNames := TTaxonNames.Create;
        try
          lTaxonNames.TaxonListItemKey := FieldByName('Taxon_List_Item_Key').AsString;
          lTaxonNames.TaxonName        := FieldByName('Preferred_Name').AsString;
          lTaxonNames.TNItalic         := FieldByName('Preferred_Name_Italic').AsBoolean;
          lTaxonNames.TNAttribute      := FieldByName('Preferred_Name_Attribute').AsString;
          lTaxonNames.TNAuthor         := FieldByName('Preferred_Name_Authority').AsString;
          lTaxonNames.CommonName       := FieldByName('Common_Name').AsString;
          lTaxonNames.CNItalic         := FieldByName('Common_Name_Italic').AsBoolean;
          lTaxonNames.CNAttribute      := FieldByName('Common_Name_Attribute').AsString;
          lTaxonNames.EnteredName      := FieldByName('Actual_Name').AsString;
          lTaxonNames.ENItalic         := FieldByName('Actual_Name_Italic').AsBoolean;
          lTaxonNames.ENAttribute      := FieldByName('Actual_Name_Attribute').AsString;
          lTaxonNames.ENAuthor         := FieldByName('Authority').AsString;
          FFilterTaxa.Add(lTaxonNames);
        except
          on E: Exception do begin
            lTaxonNames.Free;
            raise;
          end;
        end;
        Next;
      end;
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;

// The TStrings object will be loaded from a file.
procedure TExportFilter.PopulateFromStrings(AStrings: TStrings);
var lHighNum, i: Integer;
begin
  with AStrings do begin
    FFilterKey         := Values['Export_Filter_Key'];
    FilterName         := Values['Filter_Name'];
    IncludeOccurrences := StrToBool(Values['Include_Occurrences']);
    IncludeLocations   := StrToBool(Values['Include_Locations']);
    IncludeNames       := StrToBool(Values['Include_Names']);
    HasBoundingBox     := StrToBool(Values['Has_Bounding_Box']);
    if HasBoundingBox then begin
      SWLat  := StrToFloat(Values['SW_Lat']);
      SWLong := StrToFloat(Values['SW_Long']);
      NELat  := StrToFloat(Values['NE_Lat']);
      NELong := StrToFloat(Values['NE_Long']);
    end;
    HasStartDate := StrToBool(Values['Has_Start_Date']);
    if HasStartDate then ObsDateStart := StrToInt(Values['Obs_Date_Start']);
    HasEndDate := StrToBool(Values['Has_End_Date']);
    if HasEndDate then ObsDateEnd := StrToInt(Values['Obs_Date_End']);
    if Values['Survey_Count'] <> '' then begin
      lHighNum := StrToInt(Values['Survey_Count']) - 1;
      for i := 0 to lHighNum do
        AddLoadSurvey(Values['Survey' + IntToStr(i)]);
    end;
    if Values['Taxon_Count'] <> '' then begin
      lHighNum := StrToInt(Values['Taxon_Count']) - 1;
      for i := 0 to lHighNum do
        AddTaxon(Values['Taxon' + IntToStr(i)]);
    end;
    if Values['Tag_Count'] <> '' then begin
      lHighNum := StrToInt(Values['Tag_Count']) - 1;
      for i := 0 to lHighNum do
        AddLoadSurveyTag(Values['Tag' + IntToStr(i)]);
    end;
  end;
end;  // PopulateFromStrings

//==============================================================================
// Manages the export process
procedure TExportFilter.PopulateKeyList(AKeyList: TEditableKeyList; ADate: TDateTime);
var lSurveyList, lOccurrencesSQL, lTaxonList: string;
begin
  FKeyList := AKeyList;
  FFilterDate := ADate;
  GetRelevantLists(lSurveyList, lTaxonList);
  if IncludeOccurrences then begin
    lOccurrencesSQL := GetTaxonOccurrencesSQL(lSurveyList, lTaxonList);
    AddToKeyList(lOccurrencesSQL);
    //Biotopes are included only if not filtering on taxonomic groups
    if FFilterTaxa.Count = 0 then begin
      lOccurrencesSQL := GetBiotopeOccurrencesSQL(lSurveyList);
      AddToKeyList(lOccurrencesSQL);
    end;
  end;
  if IncludeLocations then
    PopulateWithLocations(lSurveyList);
  if IncludeNames then begin
    PopulateWithIndividuals;
    PopulateWithOrganisations;
  end;
  FKeyList := nil;
end;  // PopulateKeyList

//==============================================================================
procedure TExportFilter.PopulateWithIndividuals;
var
  lstSQL : string;
begin
  lstSQL := SQL_INDIVIDUALS_SELECT;
  if FFilterDate <> 0 then begin
    lstSQL := lstSQL + SQL_INDIVIDUALS_DATE_JOIN;
    lstSQL := lstSQL + 'WHERE ' + ReplaceDates(SQL_INDIVIDUALS_DATE_WHERE);
  end;
  AddToKeyList(lstSQL);
end;

//------------------------------------------------------------------------------
{ Description : grab all location keys if the include locations option is on,
     and filter by bounding box if relevant
  Created : 02/12/2002 }
procedure TExportFilter.PopulateWithLocations(const ASurveyList : string);
var
  lstSQLSelect, lstSQLWhere, lstSQLtoRun, lstSQLSelectLocs : string;
begin
  lstSQLSelect := SQL_LOCATIONS_SELECT;
  lstSQLWhere := '';
  if HasBoundingBox then
    AddToSQLWhereClause(lstSQLWhere,
            Format(SQL_LOCATIONS_BOUNDING_BOX_WHERE, [FloatToStr(SWLat),
            FloatToStr(NELat), FloatToStr(SWLong), FloatToStr(NELong)]));
  if FFilterDate <> 0 then begin
    lstSQLSelect := lstSQLSelect + SQL_LOCATIONS_DATE_JOIN;
    AddToSQLWhereClause(lstSQLWhere, ReplaceDates(SQL_LOCATIONS_DATE_WHERE));
  end;
  if SurveyCount>0 then begin
    // use a union query for locations to get them through samples and events -
    // this seems to be quickest way
    AddToSQLWhereClause(lstSQLWhere,
            Format(SQL_LOCATIONS_BY_SURVEY_WHERE, [ASurveyList]));
    lstSQLSelectLocs := lstSQLSelect + SQL_LOCATIONS_BY_SAMPLE_SURVEY_JOIN;
    lstSQLtoRun := lstSQLSelectLocs+lstSQLWhere;
    lstSQLSelectLocs := lstSQLSelect + SQL_LOCATIONS_BY_EVENT_SURVEY_JOIN;
    lstSQLtoRun := lstSQLtoRun+#13#10'UNION'#13#10+lstSQLSelectLocs+lstSQLWhere;
  end else
    lstSQLtoRun := lstSQLSelect+lstSQLWhere;
  AddToKeyList(lstSQLtoRun);
end;

//------------------------------------------------------------------------------
procedure TExportFilter.PopulateWithOrganisations;
var
  lstSQL : string;
begin
  lstSQL := SQL_ORGANISATIONS_SELECT;
  if FFilterDate <> 0 then begin
    lstSQL := lstSQL + SQL_ORGANISATIONS_DATE_JOIN;
    lstSQL := lstSQL + 'WHERE ' + ReplaceDates(SQL_ORGANISATIONS_DATE_WHERE);
  end;
  AddToKeyList(lstSQL);
end;

//------------------------------------------------------------------------------
//==============================================================================
// Saves the filter to the database.
function TExportFilter.ReplaceDates(const SQLText: string): string;

  function DateIndex(const SQLText: string): Integer;
  begin
    Result := Length(SQLText);
    repeat
      Dec(Result);
      while (Result >= 1) and (SQLText[Result] <> ':') do Dec(Result);
    until (Result = 0) or (Copy(SQLText, Result, 11) = ':Date_Param');
  end;
  
var i: Integer;
begin
  Result := SQLText;
  i := DateIndex(Result);
  repeat
    Result := Copy(Result, 1, i - 1) +
              '''' + FormatDateTime('yyyymmdd', FFilterDate) + '''' +
              Copy(Result, i + 11, Length(Result) - i - 10);
    i := DateIndex(Result);
  until i = 0;
end;

//==============================================================================
procedure TExportFilter.SaveFilter;
var
  lDecSeparator: char;
begin
  // Change the decimal separator to . when sending floating point data to SQL
  lDecSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    if FPersisted then UpdateFilter else InsertFilter;
  finally
    DecimalSeparator := lDecSeparator;
  end;
end;

//==============================================================================
procedure TExportFilter.SaveRelations;
var k: Integer;
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    for k := FFilterSurveys.Count - 1 downto 0 do begin
      SQL.Text := Format(INSERT_SURVEY, [FFilterKey, TKey(FFilterSurveys.Objects[k]).Key]);
      ExecSQL;
    end;

    for k := 0 to FFilterSurveyTags.Count - 1 do
      dmDatabase.RunStoredProc(
          'usp_ExportFilterTag_Insert',
          ['@ExportFilterKey', FFilterKey,
           '@ConceptKey', FFilterSurveyTags.Names[k]]);

    for k := FFilterTaxa.Count - 1 downto 0 do begin
      SQL.Text := Format(INSERT_TAXON, [FFilterKey, TTaxonNames(FFilterTaxa[k]).TaxonListItemKey]);
      ExecSQL;
    end;
    ParseSQL := true;
  end;
end;

//==============================================================================
// Saves the Filter to a file.
procedure TExportFilter.SaveToFile(const AFileName: TFileName);
var f: TStringList;
begin
  f := TStringList.Create;
  try
    f.Add('[Export Filter]');
    SaveToStrings(f);
    f.SaveToFile(AFileName);
  finally f.Free end;
end;

//==============================================================================
// Ready to save the TStrings object to a file.
procedure TExportFilter.SaveToStrings(AStrings: TStrings);
var i: Integer;
begin
  with AStrings do begin
    Add('Export_Filter_Key=' + FilterKey);
    Add('Filter_Name=' + FilterName);
    Add('Include_Occurrences=' + BoolString[FIncludeOccurrences]);
    Add('Include_Locations=' + BoolString[FIncludeLocations]);
    Add('Include_Names=' + BoolString[FIncludeNames]);
    Add('Survey_Count=' + IntToStr(SurveyCount));
    Add('Taxon_Count=' + IntToStr(TaxonCount));
    Add('Tag_Count=' + IntToStr(SurveyTagCount));
    Add('Has_Bounding_Box=' + BoolString[FHasBoundingBox]);
    if FHasBoundingBox then begin
      Add('SW_Lat=' + FloatToStr(FSWLat));
      Add('SW_Long=' + FloatToStr(FSWLong));
      Add('NE_Lat=' + FloatToStr(FNELat));
      Add('NE_Long=' + FloatToStr(FNELong));
    end;
    Add('Has_Start_Date=' + BoolString[FHasStartDate]);
    if FHasStartDate then Add('Obs_Date_Start=' + GetObsDateStartString);
    Add('Has_End_Date=' + BoolString[FHasEndDate]);
    if FHasEndDate then Add('Obs_Date_End=' + GetObsDateEndString);
    for i := 0 to SurveyCount - 1 do
      Add(Format('Survey%d=%s', [i, SurveyKey[i]]));
    for i := 0 to TaxonCount - 1 do
      Add(Format('Taxon%d=%s', [i, TaxonKey[i]]));
    for i := 0 to SurveyTagCount - 1 do
      Add(Format('Tag%d=%s', [i, SurveyTagKey[i]]));
  end;
end;  // SaveToStrings

//==============================================================================
// Called during the starting processes of the Application.  Sets up the
// 'Export Using Filter' menu on the main screen.
class procedure TExportFilter.SetExportFilterMenu(AMenuItem: TMenuItem; ANotifyEvent: TNotifyEvent);
begin
  mMenuItem := AMenuItem;
  mNotifyEvent := ANotifyEvent;
  UpdateFilterList;
end;

procedure TExportFilter.SetFilterName(const Value: string);
begin
  FFilterName := Value;
end;

procedure TExportFilter.SetHasBoundingBox(const Value: Boolean);
begin
  FHasBoundingBox := Value;
end;

procedure TExportFilter.SetHasEndDate(const Value: Boolean);
begin
  FHasEndDate := Value;
end;


procedure TExportFilter.SetHasStartDate(const Value: Boolean);
begin
  FHasStartDate := Value;
end;

procedure TExportFilter.SetIncludeLocations(const Value: Boolean);
begin
  FIncludeLocations := Value;
end;

procedure TExportFilter.SetIncludeNames(const Value: Boolean);
begin
  FIncludeNames := Value;
end;

procedure TExportFilter.SetIncludeOccurrences(const Value: Boolean);
begin
  FIncludeOccurrences := Value;
end;

procedure TExportFilter.SetNELat(const Value: Double);
begin
  FNELat := Value;
end;

procedure TExportFilter.SetNELatLong(const Value: TLatLong);
begin
  NELat := Value.Lat;
  NELong := Value.Long;
end;

procedure TExportFilter.SetNELong(const Value: Double);
begin
  FNELong := Value;
end;

procedure TExportFilter.SetObsDateEnd(const Value: TDateTime);
begin
  FObsDateEnd := Value;
end;

procedure TExportFilter.SetObsDateStart(const Value: TDateTime);
begin
  FObsDateStart := Value;
end;

procedure TExportFilter.SetSWLat(const Value: Double);
begin
  FSWLat := Value;
end;

procedure TExportFilter.SetSWLatLong(const Value: TLatLong);
begin
  SWLat := Value.Lat;
  SWLong := Value.Long;
end;

procedure TExportFilter.SetSWLong(const Value: Double);
begin
  FSWLong := Value;
end;

function TExportFilter.SurveyIndexOf(const ASurveyKey: TKeyString): Integer;
begin
  Result := FFilterSurveys.Count - 1;
  while (Result >= 0) and (TKey(FFilterSurveys.Objects[Result]).Key <> ASurveyKey) do Dec(Result);
end;

function TExportFilter.TaxonIndexOf(const ATaxonKey: TKeyString): Integer;
begin
  Result := FFilterTaxa.Count - 1;
  while (Result >= 0) and (TTaxonNames(FFilterTaxa[Result]).TaxonListItemKey <> ATaxonKey) do Dec(Result);
end;

// Saves an existing filter to the database.
procedure TExportFilter.UpdateFilter;
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(UPDATE_FILTER,
                       [QuotedStr(FFilterName), SQLBoolString[FIncludeOccurrences],
                        SQLBoolString[FIncludeLocations], SQLBoolString[FIncludeNames],
                        GetSWLatString, GetSWLongString, GetNELatString, GetNELongString,
                        GetObsDateStartString, GetObsDateEndString, FFilterKey]);
    ExecSQL;
    SQL.Text := Format(DELETE_SURVEYS, [FFilterKey]);
    ExecSQL;
    dmDatabase.RunDeleteStoredProc('usp_ExportFilterTag_Delete_ForExportFilter', ['@Key', FFilterKey]);
    SQL.Text := Format(DELETE_TAXA, [FFilterKey]);
    ExecSQL;
    ParseSQL := true;
  end;
  SaveRelations;
end;

// Called when the 'Export Using Filter' menu on the main screen needs updating.
class procedure TExportFilter.UpdateFilterList;
var lFilterList: TStringList;
    k: Integer;
    lMenuItem: TMenuItem;
begin
  for k := mMenuItem.Count - 1 downto 0 do
    mMenuItem.Items[k].Free;
  lFilterList := CreateFilterList;
  try
    mMenuItem.Visible := lFilterList.Count > 0;
    for k := 0 to lFilterList.Count - 1 do begin
      lMenuItem := TExportFilterMenuItem.Create(mMenuItem, TKey(lFilterList.Objects[k]).Key);
      lMenuItem.Caption := lFilterList.Strings[k];
      lMenuItem.OnClick := mNotifyEvent;
      mMenuItem.Add(lMenuItem);
    end;
  finally
    for k := 0 to lFilterList.Count - 1 do
      lFilterList.Objects[k].Free;
    lFilterList.Free;
  end;
end;

//==============================================================================
{ TExportFilterMenuItem }

constructor TExportFilterMenuItem.Create(AOwner: TComponent; const AKey: TKeyString);
begin
  inherited Create(AOwner);
  FKey := TKey.Create(AKey);
end;

destructor TExportFilterMenuItem.Destroy;
begin
  FKey.Free;
  inherited;
end;

{ Description : updates LAST_EXPORT_DATE for the export filter.
  Created : 02/12/2002 }
procedure TExportFilter.UpdateLastExportDate(idtDateTime: TDateTime);
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(SQL_UPDATE_LAST_EXPORT_DATE, [FormatDateTime('yyyymmdd',
                       idtDateTime), FFilterKey]);
    ExecSQL;
    ParseSQL := true;
  end;
end;

end.
