//==============================================================================
//  Unit:        Search
//
//  Implements:  TdmSearch
//
//  Description: Implements data access functionality for various search options
//               available across the application.
//
//  Author:      Eric Salmon
//  Created:     22 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 139 $
//    $Date: 17/07/09 12:47 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Search;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, StdCtrls, VagueDate, BaseData, ComCtrls, DataClasses, Finder, Constants,
  SpatialRefFuncs, GeneralFunctions, JNCCDatasets, SQLConstants, ADODB, DatabaseAccessADO,strUtils;

type
  TAddToListCallback = procedure( iQuery : TJNCCQuery; AFinder:TFinder ) of Object;

  TTaxonSearchMode = (stList, stUnrestricted,stPreferredTaxa,stRecommendedFull,stPreferredLists, stRucksack);

  TdmSearch = class(TBaseDataModule)
    qrySource: TJNCCQuery;
    qryAdmin: TJNCCQuery;
  private
    { Private declarations }
    FTerminated : boolean;
    FTopKeys : TStringList;  // For use in Admin query, for now.
    FOrderByAbbreviation : boolean; // To order text returned to finder
    FTaxonSearchType: TTaxonSearchMode;
    FiSearchedChars : integer;
    procedure OpenSourceQuery;
    procedure InitGenericQuery(AFinder: TFinder; const SourceParameter: string;
      const iSQL: string; iAddToListMethod: TAddToListCallback;
      const iBiotope: Boolean; iEnterListKey: Boolean);
    procedure AddTaxonToList(iQuery: TJNCCQuery; AFinder: TFinder);
    procedure AddBiotopeToList(iQuery: TJNCCQuery; AFinder: TFinder);
    function SetupFilterText(const AText: String): String;
    procedure GetBiotopesFromList(AFinder: TFinder; const Keys: String;
      GetChildren: Boolean);
    procedure AddReferencesToFinder(AFinder: TFinder; ASQL: String);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetDatabaseName(const ADatabaseLocation:string);
    procedure InitFromTaxonQuery(AFinder: TFinder; const SourceParameter: string;
      iSearchUserCommonNames: boolean; iSearchBy: TTaxonSearchType; iSearchRestriction: string);
    procedure InitFromTaxonList(AFinder:TFinder; const AList:TEditableKeyList;
      const iForceLocal : boolean = false; GetChildren: boolean = True);

    procedure InitFromBiotopeQuery(AFinder:TFinder; const ASourceParameter: string);
    procedure InitFromBiotopeList(AFinder:TFinder; const AList:TStrings); overload;
    procedure InitFromBiotopeList(AFinder:TFinder; const AList:TEditableKeyList;
      GetChildren: boolean = True); overload;

    procedure InitFromLocationQuery(AFinder:TFinder); overload;
    procedure InitFromLocationQuery(AFinder:TFinder; const Query: String); overload;
    procedure InitFromLocationList(AFinder:TFinder; const AList:TList);

    procedure InitFromNameQuery(AFinder:TFinder);
    procedure InitFromNameList(AFinder:TFinder; const AList:TStrings; const AKeyList:TKeyList);
    procedure InitFromIndividualQuery(AFinder:TFinder);
    procedure InitFromOrganisationQuery(AFinder:TFinder; const tfClearAll:boolean=true);

    procedure InitFromReferenceQuery(AFinder:TFinder);
    procedure InitFromReferenceKeywordQuery(AFinder:TFinder);
    procedure InitFromReferenceList(AFinder:TFinder; const AList:TStrings; const AKeyList:TKeyList);

    procedure InitFromSurveyQuery(AFinder:TFinder);
    procedure InitFromSampleQuery(AFinder:TFinder);
    procedure InitFromTaxOccQuery(AFinder:TFinder);
    procedure InitFromConceptQuery(const applicationConceptGroup: TKeyString; AFinder:TFinder);
    procedure InitFromAdminAreaQuery(AFinder:TFinder; const SourceParameter:string);
    procedure InitFromAdminType(AFinder: TFinder; const typekey: TKeyString);
    procedure InitFromFeatureQuery(AFinder:TFinder);

    procedure Terminate;
  end;

var
  dmSearch: TdmSearch;

//==============================================================================
implementation

uses
  GeneralData, ApplicationSettings;

{$R *.DFM}

//==============================================================================
const
  SQL_TAXON_FROM_LIST =
          'SELECT ITNout.Taxon_List_Item_Key, ITNout.Actual_Name AS ItemName, ' +
          'ITNout.Authority, ITNout.Abbreviation ' +
          'FROM Index_Taxon_Name ITNin ';

  //============================================================================
  // one of three optional join paths depending on nameserver and whether group contents included
  //============================================================================

  // first join is a 'dummy' because we need no translation
  SQL_TAXON_JOIN =
          'INNER JOIN Index_Taxon_Name ITNout ON ITNout.Taxon_List_Item_Key = ITNin.Taxon_List_Item_Key';

  SQL_TAXON_JOIN_GROUP =
          'INNER JOIN Index_Taxon_Synonym ITS1 ON ITS1.Taxon_List_Item_Key=ITNin.Taxon_List_Item_Key '+
          'INNER JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key=ITS1.Synonym_List_Item_Key '+
          'INNER JOIN Index_Taxon_Synonym ITS2 ON ITS2.Taxon_List_Item_Key=ITG.Contained_List_Item_Key '+
          'INNER JOIN Index_Taxon_Name ITNout ON ITNout.Taxon_List_Item_Key=ITS2.Synonym_List_Item_Key';

  SQL_TAXON_JOIN_GROUP_NS =
          'INNER JOIN Index_Taxon_Name ITN1 ON ITN1.Recommended_Taxon_List_Item_Key = ITNin.Recommended_Taxon_List_Item_Key '+
          'INNER JOIN Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key=ITN1.Taxon_List_Item_Key '+
          'INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Taxon_List_Item_Key=ITG.Contained_List_Item_Key '+
          'INNER JOIN Index_Taxon_Name ITNout ON ITNout.Recommended_Taxon_List_Item_Key=ITN2.Recommended_Taxon_List_Item_Key';

  //============================================================================

  SQL_TAXON_FROM_LIST_WHERE =
          ' WHERE ITNin.Taxon_List_Item_Key IN (%s)';

  SQL_TAXON_FROM_LIST_RESTRICTED =
          '  AND (ITNout.Actual_Name '  + ST_LIKE_PATTERN +
          '   OR  ITNout.Authority '    + ST_LIKE_PATTERN +
          '   OR  ITNout.Abbreviation ' + ST_LIKE_PATTERN + ')';

  //----------------------------------------------------------------------------
  SQL_TOP_ADMINAREA =
          'SELECT Admin_Area_Key FROM Admin_Area '+
          'WHERE Admin_Type_Key = :AdminType AND Parent IS NULL';

  SQL_FILTERED_ADMINAREA =
          'SELECT Admin_Area_Key, Item_Name, Short_Code, Parent '+
          'FROM Admin_Area '+
          'WHERE (Item_Name ' + ST_LIKE_PATTERN +
          ' OR Short_Code ' + ST_LIKE_PATTERN + ')';

  SQL_TYPE_FILTERED_ADMINAREA =
          'SELECT Admin_Area_Key, Item_Name, Short_Code, Parent '+
          'FROM Admin_Area '+
          'WHERE (Item_Name ' + ST_LIKE_PATTERN +
          ' OR Short_Code ' + ST_LIKE_PATTERN + ') ' +
          'AND Admin_Type_Key = :AdminType';

  //----------------------------------------------------------------------------
  SQL_BIOTOPE_SELECT =
          'SELECT BLI.Biotope_List_Item_Key, B.Original_Code, B.Short_Term, '+
          '  CASE WHEN B.Original_Code IS NULL THEN B.Short_Term ' +
          '  ELSE ' +
          '    CASE WHEN B.Short_Term IS NULL THEN B.Original_Code ' +
          '    ELSE B.Original_Code + '', '' + B.Short_Term ' +
          '    END ' +
          '  END AS "BiotopeName" ';

  SQL_BIOTOPE_FROM_QUERY =
          SQL_BIOTOPE_SELECT +
          'FROM   Biotope B ' +
          'INNER JOIN Biotope_List_Item BLI ON B.Biotope_Key = BLI.Biotope_Key ' +
          'INNER JOIN Biotope_Classification_Version BCV ON BLI.BT_CL_Version_Key = BCV.BT_CL_Version_Key ' +
          'WHERE  BLI.BT_CL_Version_Key IN () ' +
          '  AND  BLI.BT_CL_Version_To Is Null ' +
          '  AND  (B.Original_Code ' + ST_LIKE_PATTERN +
          '   OR   B.Short_Term ' + ST_LIKE_PATTERN + ')' +
          'ORDER BY B.Original_Code, B.Short_Term';

  SQL_BIOTOPE_PRIMARY_FROM_LIST =
          SQL_BIOTOPE_SELECT +
          'FROM   Biotope_List_Item BLI, Biotope B ' +
          'WHERE  BLI.Biotope_List_Item_Key IN (%s) ' +
          '  AND  B.Biotope_Key = BLI.Biotope_Key';

  SQL_BIOTOPE_FROM_LIST =
          SQL_BIOTOPE_SELECT +
          'FROM   Biotope_List_Item BLI, Biotope B '+
          'WHERE  BLI.Parent = :ParentKey '+
          '  AND  B.Biotope_Key = BLI.Biotope_Key';

  //----------------------------------------------------------------------------
  SQL_LOCATION_SELECT =
          'SELECT N.Location_Name_Key, N.Item_Name, '+
          '  T.Short_Name AS Location_Type,'+
          '  L.Spatial_Ref, L.Spatial_Ref_System, L.LAT, L.LONG, '+
          '  L.File_Code '+
          ' FROM Location_Name N'+
          ' INNER JOIN Location L ON L.Location_Key = N.Location_Key'+
          ' INNER JOIN Location_Type T'+
          '  ON T.Location_Type_Key = L.Location_Type_Key';
          
  SQL_LOCATION_FROM_QUERY =
          SQL_LOCATION_SELECT +
          ' WHERE Item_Name ' + ST_LIKE_PATTERN +
          ' ORDER BY Item_Name';

  SQL_LOCATION_FROM_LIST =
          SQL_LOCATION_SELECT +
          ' WHERE N.Location_Key IN (%s)' +
          ' ORDER BY Item_Name';

  //----------------------------------------------------------------------------
  SQL_INDIVIDUAL_NAME =
        '[dbo].[FormatIndividualFull](Title,Initials,ForeName,Surname) AS INDIVIDUALNAME ';

  SQL_INDIVIDUAL_NAME_ADDRESS =
        '[dbo].[FormatIndividualFull](Title,Initials,ForeName,Surname) + '' <'' + [dbo].[ufn_GetAddress](Name_Key) + ''> '' AS INDIVIDUALNAME ' ;

  SQL_ORGANISATION_NAME =
          '  CASE WHEN Acronym IS NOT NULL THEN Acronym + '', '' + Full_Name ' +
          '  ELSE Full_Name END AS OrganisationName ';

  SQL_INDIVIDUAL_FROM_QUERY =
          'SELECT Name_Key, Forename, Initials, Surname, ' + SQL_INDIVIDUAL_NAME_ADDRESS  +
          'FROM Individual ' +
          'WHERE Forename ' + ST_LIKE_PATTERN +
          '   OR Initials ' + ST_LIKE_PATTERN +
          '   OR Surname ' + ST_LIKE_PATTERN +
          '   OR Title ' + ST_LIKE_PATTERN +
          ' ORDER BY Forename, Initials, Surname';

  SQL_ORGANISATION_FROM_QUERY =
          'SELECT Name_Key, Acronym, Full_Name, ' + SQL_ORGANISATION_NAME +
          'FROM Organisation '+
          'WHERE Acronym ' + ST_LIKE_PATTERN +
          '   OR Full_Name ' + ST_LIKE_PATTERN +
          ' ORDER BY Acronym, Full_Name';

  SQL_NAME_FROM_LIST =
          'SELECT N.Name_Key, Organisation, Forename, Initials, Surname, Acronym, Full_Name, ' +
          SQL_INDIVIDUAL_NAME + ', ' + SQL_ORGANISATION_NAME +
          'FROM [Name] N ' +
          'LEFT JOIN Individual I ON I.Name_Key = N.Name_Key ' +
          'LEFT JOIN Organisation O ON O.Name_Key = N.Name_Key ' +
          'WHERE N.Name_Key IN (%s) ' +
          'ORDER BY Forename, Initials, Surname, Acronym, Full_Name';

  //----------------------------------------------------------------------------
  SQL_REFERENCE_FROM_QUERY =
          'SELECT R.Source_Key, %s, R.Year_Vague_Date_Start, '+
          '       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, R.Title '+
          'FROM (Reference AS R ' +
          'INNER JOIN %s AS A ON R.Source_Key = A.Source_Key) '+
          'WHERE  %s ' + ST_LIKE_PATTERN +
          ' OR %s ' + ST_LIKE_PATTERN;

  SQL_REFERENCE_FROM_KEYWORDS_QUERY =
          'EXEC usp_References_Select_ForSearchByKeyword ''%s''';

  SQL_REFERENCE_FROM_LIST =
          'SELECT R.Source_Key, %s, R.Year_Vague_Date_Start, '+
          '       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, R.Title '+
          'FROM (Reference AS R ' +
          'INNER JOIN %s AS A ON R.Source_Key = A.Source_Key) '+
          'WHERE  R.Source_Key IN (%s)';

  //----------------------------------------------------------------------------
//PT - changed so that the location is returned and the sample reference is not

  SQL_SAMPLE_FROM_QUERY =
          'SELECT S.SAMPLE_KEY, S.Vague_Date_Start, S.Vague_Date_End, '+
          'S.Vague_Date_Type, S.Spatial_Ref, S.Lat, S.Long, '+
          'S.Spatial_Ref_System, LN.Item_Name, ST.Short_Name '+
          'FROM (Sample AS S INNER JOIN Sample_Type AS ST ON '+
          'S.Sample_Type_Key = ST.Sample_Type_Key) LEFT JOIN '+
          'Location_Name AS LN ON S.Location_Key = LN.Location_Key '+
          'INNER JOIN Survey_Event SE ' +
            'ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
          'LEFT JOIN User_Survey_Restriction USR ' +
            'ON USR.Survey_Key = SE.Survey_Key ' +
            'AND USR.Name_Key = ''%s'' ' +
          'WHERE LN.Preferred=1 AND NOT (S.Location_Key IS Null) '+
            'AND USR.Name_Key IS NULL ' +
          'UNION '+
          'SELECT S.SAMPLE_KEY, S.VAGUE_DATE_START, S.VAGUE_DATE_END, '+
          'S.VAGUE_DATE_TYPE, S.SPATIAL_REF, S.LAT, S.LONG, '+
          'S.SPATIAL_REF_SYSTEM, '''' AS ITEM_NAME, ST.Short_Name '+
          'FROM Sample AS S INNER JOIN Sample_Type AS ST ON '+
          'S.Sample_Type_Key = ST.Sample_Type_Key '+
          'INNER JOIN Survey_Event SE ' +
            'ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
          'LEFT JOIN User_Survey_Restriction USR ' +
            'ON USR.Survey_Key = SE.Survey_Key ' +
            'AND USR.Name_Key = ''%s'' ' +
          'WHERE S.Location_Key IS Null '+
            'AND USR.Name_Key IS NULL ';

// PT - added query for selecting related taxon occurrences

  SQL_TAXOCC_FROM_QUERY =
          'SELECT XO.TAXON_OCCURRENCE_KEY, S.VAGUE_DATE_START, '+
          'S.VAGUE_DATE_END, S.VAGUE_DATE_TYPE, S.SPATIAL_REF, '+
          'S.LAT, S.LONG, S.SPATIAL_REF_SYSTEM, LN.ITEM_NAME As [Location Name], '+
          'ST.Short_Name, T.Item_Name As [Taxon Name] '+
          'FROM ((((((Sample AS S INNER JOIN Sample_Type AS ST ON '+
          'S.Sample_Type_Key = ST.Sample_Type_Key) INNER JOIN TAXON_OCCURRENCE AS XO ON S.SAMPLE_KEY = XO.SAMPLE_KEY) '+
          'INNER JOIN TAXON_DETERMINATION AS TD ON XO.TAXON_OCCURRENCE_KEY = TD.TAXON_OCCURRENCE_KEY) '+
          'INNER JOIN TAXON_LIST_ITEM AS TLI ON TD.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY) '+
          'INNER JOIN TAXON_VERSION AS TV ON TLI.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY) '+
          'INNER JOIN TAXON AS T ON TV.TAXON_KEY = T.TAXON_KEY) '+
          'LEFT JOIN Location_Name AS LN ON S.LOCATION_KEY = LN.LOCATION_KEY '+
          'WHERE LN.Preferred=1 AND (NOT S.Location_Key IS Null) AND TD.Preferred=1 '+
          'AND T.ITEM_NAME LIKE ''%s%%'' ' +
          'UNION '+
          'SELECT XO.TAXON_OCCURRENCE_KEY, S.VAGUE_DATE_START, '+
          'S.VAGUE_DATE_END, S.VAGUE_DATE_TYPE, S.SPATIAL_REF, '+
          'S.LAT, S.LONG, S.SPATIAL_REF_SYSTEM, '''' As [Location Name], '+
          'ST.Short_Name, T.Item_Name As [Taxon Name] '+
          'FROM (((((Sample AS S INNER JOIN Sample_Type AS ST ON '+
          'S.Sample_Type_Key = ST.Sample_Type_Key) INNER JOIN TAXON_OCCURRENCE AS XO ON S.SAMPLE_KEY = XO.SAMPLE_KEY) '+
          'INNER JOIN TAXON_DETERMINATION AS TD ON XO.TAXON_OCCURRENCE_KEY = TD.TAXON_OCCURRENCE_KEY) '+
          'INNER JOIN TAXON_LIST_ITEM AS TLI ON TD.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY) '+
          'INNER JOIN TAXON_VERSION AS TV ON TLI.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY) '+
          'INNER JOIN TAXON AS T ON TV.TAXON_KEY = T.TAXON_KEY '+
          'WHERE S.Location_Key IS Null AND TD.Preferred=1 '+
          'AND T.ITEM_NAME LIKE ''%s%%''';

  //----------------------------------------------------------------------------
  // PT - added query for selecting location features

  SQL_FEATURE_FROM_QUERY =
          'SELECT LF.LOCATION_FEATURE_KEY, LF.ITEM_NAME, LFT.SHORT_NAME ' +
          'FROM (LOCATION_FEATURE AS LF ' +
          'INNER JOIN LOCATION_FEATURE_GRADING AS LFG ON LF.FEATURE_GRADING_KEY = LFG.FEATURE_GRADING_KEY) ' +
          'INNER JOIN LOCATION_FEATURE_TYPE AS LFT ON LFG.LOCATION_FEATURE_TYPE_KEY = LFT.LOCATION_FEATURE_TYPE_KEY ' +
          'ORDER BY LF.ITEM_NAME';

  SQL_CONCEPT_FROM_QUERY =
          'SELECT Concept_Key, Plaintext '+
          'FROM VW_ConceptTerm '+
          'WHERE Concept_Group_Key=''%s'' '+
          'AND Is_Current=1'+
          'AND Plaintext LIKE ''%s%%''';

  //----------------------------------------------------------------------------
  SQL_SURVEY_FROM_QUERY = 'SELECT Survey_Key, Item_Name FROM Survey ' +
          'LEFT JOIN User_Survey_Restriction USR ' +
            'ON USR.Survey_Key = SE.Survey_Key ' +
            'AND USR.Name_Key = ''%s'' ' +
          'WHERE USR.Name_Key IS NULL';

//==============================================================================
constructor TdmSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTerminated := False;
  FTopKeys:=TStringList.Create;
  FTopKeys.Sorted:=true;
  qrySource.ParseSQL := false;
  qryAdmin.ParseSQL  := false;
end;  // Create

//==============================================================================
destructor TdmSearch.Destroy;
begin
  FTopKeys.Free;
  FTopKeys := nil;
  inherited Destroy;
end;  // Destroy;

//==============================================================================
function TdmSearch.SetupFilterText(const AText: String): String;
begin
  Result := AText;
  if (Result <> '') and (Result[1] = '*') then Result := Copy(Result, 1, MIN_CHARS + 1)
                                          else Result := Copy(Result, 1, MIN_CHARS);
  Result := Trim(ConvertSearchString(Result));
end;  // SetupFilterText

//==============================================================================
procedure TdmSearch.InitFromTaxonQuery(AFinder: TFinder; const SourceParameter: string;
  iSearchUserCommonNames: boolean; iSearchBy: TTaxonSearchType; iSearchRestriction: string);
var
  lText, lSQL: String;
  lRucksackName: String;
  lCurrentListKey: String;
begin
  lText := SetupFilterText(AFinder.Text);
  { Build filter string.  Filter on abbreviation, names, or names including user common name }
  FOrderByAbbreviation := (iSearchBy = stAbbreviation);
  lCurrentListKey := SourceParameter;

  if iSearchRestriction = ResStr_Unrestricted then begin
    FTaxonSearchType := stUnrestricted;
    lSQL := Format(SQL_TAXON_FROM_QUERY_ALL_LISTS, [ResStr_Taxon_Can_Not_Expand,lText, lText, lText]);
  end else if iSearchRestriction = ResStr_Preferred_Taxa then begin
    FTaxonSearchType := stPreferredTaxa;
    lSQL := Format(SQL_TAXON_FROM_QUERY_PREFERRED_TAXA, [ResStr_Taxon_Can_Not_Expand,lText, lText, lText]);
  end else if  iSearchRestriction = ResStr_Recommended_Full then begin
    FTaxonSearchType := stRecommendedFull;
    lSQL := Format(SQL_TAXON_FROM_QUERY_RECOMMENDED_FULL, [ResStr_Taxon_Can_Not_Expand,lText, lText, lText]);
  end else if iSearchRestriction = ResStr_PreferredLists then begin
    FTaxonSearchType := stPreferredLists;
    // use generalData to get list version key for filter
    lSQL := Format(SQL_TAXON_FROM_QUERY_PREF_LISTS, [ResStr_Taxon_Can_Not_Expand,lText, lText, lText]);
  end else if iSearchRestriction = ResStr_CurrentChecklist then begin
    FTaxonSearchType := stList;
    if leftstr(lCurrentListKey,8)  <> 'VIRTUAL_' then begin
      // use generalData to get list version key for filter
      lSQL := Format(SQL_TAXON_FROM_QUERY_CURR_LIST,
                     [ResStr_Taxon_Can_Not_Expand,SourceParameter,lText, lText, lText,ltext]);
    end else begin
       lSQL := Format(SQL_TAXON_FROM_QUERY_VIRTUAL,
              [ResStr_Taxon_Can_Not_Expand,lCurrentListKey,lText, lText, lText,ltext]);

     end;
  end else begin
    { Search a rucksack }
    FTaxonSearchType := stRucksack;
    { get name without the Rucksack on the end }
    lRucksackName := Copy( iSearchRestriction, 1, Length(iSearchRestriction) - Length(ResStr_Rucksack));
    AppSettings.LastSearchedRucksack.RucksackName := lRucksackName;
    AppSettings.LastSearchedRucksack.BuildTemporaryTable;
    lSQL := Format(SQL_TAXON_FROM_QUERY_RUCKSACK, [ResStr_Taxon_Can_Not_Expand,lText, lText, lText, lText, lText]);
  end;

  { Initiate the search }
  InitGenericQuery(AFinder, SourceParameter, lSQL, AddTaxonToList, False, False);
end;  //  InitFromTaxonQuery

//==============================================================================
procedure TdmSearch.InitFromTaxonList(AFinder:TFinder; const AList:TEditableKeyList;
  const iForceLocal : boolean = false; GetChildren: boolean = True);
var iIndex                    : integer;
    lCursor                   : TCursor;
    lItemKeys                 : string;
    stkey, stName, stAuthority: string;
    lqryGetTaxa               : TJNCCQuery;
    lText                     : string;
    lFoundNames               : TStringList;
begin
  lCursor:=HourglassCursor;
  // Set session and database for Finder component to be the same
  AFinder.SetDatabase(dmDatabase.LocalDatabase);
  AFinder.ClearSourceList;
  lText := SetupFilterText(AFinder.Text);

  with AList do
    for iIndex:=0 to Header.ItemCount-1 do
      lItemKeys := lItemKeys + '''' + TStructItem(Items[iIndex]).KeyField1 + ''',';
  lItemKeys := Copy(lItemKeys, 1, Length(lItemKeys)-1); // strip last comma

  lqryGetTaxa:=TJNCCQuery.Create(nil);  // Need to use locale query.
  with lqryGetTaxa do
    try
      ParseSQL := false;
      dmDatabase.SetDatabaseLocal([lqryGetTaxa]);
      SQL.Add(SQL_TAXON_FROM_LIST);
      if GetChildren and AppSettings.UseRecommendedTaxaNames then
        SQL.Add(SQL_TAXON_JOIN_GROUP_NS)
      else if GetChildren and (not AppSettings.UseRecommendedTaxaNames) then
        SQL.Add(SQL_TAXON_JOIN_GROUP)
      else
        SQL.Add(SQL_TAXON_JOIN);
      SQL.Add(Format(SQL_TAXON_FROM_LIST_WHERE, [lItemKeys]));
      if (lText <> '*') and (lText <> '') then
        SQL.Add(Format(SQL_TAXON_FROM_LIST_RESTRICTED,[lText,lText,lText]));
      Open;
      if RecordCount>0 then begin
        lFoundNames:=TStringList.Create;
        lFoundNames.Sorted := true;
        try
          while not Eof do begin
            if lFoundNames.IndexOf(
                FieldByName('ItemName').AsString +
                FieldByName('Authority').AsString)=-1 then
            begin
              stKey       := FieldByName('Taxon_List_Item_Key').AsString;
              stName      := FieldByName('ItemName').AsString;
              stAuthority := FieldByName('Authority').AsString;
              if stAuthority<>'' then stName := stName + ', ' + stAuthority;
              AFinder.AddToSourceList(stKey, stName,
                                      [FieldByName('ItemName').AsString,
                                       FieldByName('Authority').AsString,
                                       FieldByName('Abbreviation').AsString]);
              lFoundNames.Add(FieldByName('ItemName').AsString +
                              FieldByName('Authority').AsString);
            end;
            Next;
          end;
        finally
          lFoundNames.Free;
        end;
      end;
    finally
      Close;
      Free;
    end;
  DefaultCursor(lCursor);
end;  // InitFromTaxonList

//==============================================================================
procedure TdmSearch.InitFromBiotopeQuery(AFinder:TFinder; const ASourceParameter: string);
var lText, lCode: string;
begin
  lText := SetupFilterText(AFinder.Text);
  // Code can be only 1 char long, so if a comma is provided, remove it
  // but only for the biotope code!
  if lText[Length(lText)] = ',' then lCode := Copy(lText, 1, Length(lText) - 1)
                                else lCode := lText;

  lText := Format(SQL_BIOTOPE_FROM_QUERY, [lCode, lText]);
  InitGenericQuery(AFinder, ASourceParameter, lText, AddBiotopeToList, True, True {FTaxonSearchType = stList});
end;  // InitFromBiotopeQuery

//==============================================================================
procedure TdmSearch.GetBiotopesFromList(AFinder: TFinder; const Keys: String;
  GetChildren: Boolean);
var lqryPrimary: TJNCCQuery;
  //----------------------------------------------------------------------------
  // Recursive procedure to go down the taxon hierarchy
  procedure GetBiotopeChildren(const ParentKey:string);
  var //stKey,stCode,stShortTerm:string;
      lqryChildren            :TJNCCQuery;
  begin
    lqryChildren := TJNCCQuery.Create(nil);  // Need to use locale query.
    dmDatabase.SetDatabaseLocal([lqryChildren]);
    with lqryChildren do
      try
        ParseSQL := false;
        SQL.Text := SQL_BIOTOPE_FROM_LIST;
        Parameters.ParamByName('ParentKey').Value := ParentKey;
        Open;
        while not Eof do begin
          AddBiotopeToList(lqryChildren, AFinder);
          GetBiotopeChildren(FieldByName('Biotope_List_Item_Key').AsString);
          Application.ProcessMessages;
          if FTerminated then Break; // from while loop
          Next;
        end;
      finally
        Close;
        Free;
      end;
  end;  // GetBiotopeChildren
  //----------------------------------------------------------------------------
begin
  AFinder.ClearSourceList;
  AFinder.SetDatabase(dmDatabase.LocalDatabase);
  lqryPrimary := TJNCCQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lqryPrimary]);
  FTerminated := false;
  with lqryPrimary do
    try
      ParseSQL := false;
      SQL.Text := Format(SQL_BIOTOPE_PRIMARY_FROM_LIST, [Keys]);
      Open;
      while not Eof do begin
        AddBiotopeToList(lqryPrimary, AFinder);
        if GetChildren then GetBiotopeChildren(FieldByName('Biotope_List_Item_Key').AsString);
        Application.ProcessMessages;
        if FTerminated then Break; // from while loop
        Next;
      end;
    finally
      Close;
      Free;
      FTerminated := true;
    end;
end;  // GetBiotopesFromList

//==============================================================================
procedure TdmSearch.InitFromBiotopeList(AFinder: TFinder; const AList: TStrings);
var iIndex   : Integer;
    lItemKeys: string;
    lCursor  : TCursor;
begin
  lCursor:=HourglassCursor;
  try
    lItemKeys := '';
    with AList do
      for iIndex := 0 to Count-1 do
        lItemKeys := lItemKeys + ',''' + TKeydata(Objects[iIndex]).ItemKey + '''';
    lItemKeys[1] := ' '; // strip first comma
    GetBiotopesFromList(AFinder, lItemKeys, true);
  finally
    DefaultCursor(lCursor);
  end;
end;  // InitFromBiotopeList

//==============================================================================
procedure TdmSearch.InitFromBiotopeList(AFinder: TFinder; const AList: TEditableKeyList;
  GetChildren: boolean = True);
var iIndex   : Integer;
    lItemKeys: String;
    lCursor  : TCursor;
begin
  lCursor:=HourglassCursor;
  try
    lItemKeys := '';
    with AList do
      for iIndex:=0 to header.ItemCount-1 do// begin
        lItemKeys := lItemKeys + ',''' + TStructItem(Items[iIndex]).KeyField1 + '''';
    lItemKeys[1] := ' '; // strip first comma
    GetBiotopesFromList(AFinder, lItemKeys, GetChildren);
  finally
    DefaultCursor(lCursor);
  end;
end;  // InitFromBiotopeList

//==============================================================================
procedure TdmSearch.InitFromLocationQuery(AFinder: TFinder);
begin
  InitFromLocationQuery(
      AFinder,
      Format(SQL_LOCATION_FROM_QUERY, [SetupFilterText(AFinder.Text)]));
end;

{-------------------------------------------------------------------------------
  Execute the given query, and add the resulting locations to the source list.
}
procedure TdmSearch.InitFromLocationQuery(AFinder: TFinder;
  const Query: String);
var lCursor:TCursor;

  {-----------------------------------------------------------------------------
    Text that should be displayed for the current location.
  }
  function LocationText: String;
  var
    lExtras: String;

    {---------------------------------------------------------------------------
      Add an extra term to the text for the location.
    }
    procedure AddTerm(Term: String);
    begin
      if Term <> '' then
      begin
        if lExtras <> '' then lExtras := lExtras + '; ';
        lExtras := lExtras + Term;
      end;
    end;
    
  begin
    lExtras := '';
    if lscLocationType in AppSettings.ExtraLocationSearchColumns then
      AddTerm(qrySource.FieldByName('Location_Type').AsString);
    if lscSpatialReference in AppSettings.ExtraLocationSearchColumns then
      AddTerm(qrySource.FieldByName('Spatial_Ref').Text);
    if lscFileCode in AppSettings.ExtraLocationSearchColumns then
      AddTerm(qrySource.FieldByName('File_Code').AsString);

    Result := qrySource.FieldByName('Item_Name').AsString;
    if lExtras <> '' then Result := Result + ' (' + lExtras + ')';
  end;

begin
  FTerminated := False;
  lCursor:=HourglassCursor;
  try
    AFinder.ClearSourceList;
    with qrySource do
      try
        if Active then Close;
        SQL.Text := Query;
        try
          OpenSourceQuery;
          First;
          while not Eof do begin
            AFinder.AddToSourceList(FieldByName('Location_Name_Key').AsString,
                                    LocationText, []);
            Application.ProcessMessages;
            if FTerminated then Break; // from while loop
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;  // InitFromLocationQuery

//==============================================================================
procedure TdmSearch.InitFromLocationList(AFinder:TFinder; const AList:TList);
var
  I: Integer;
  lItemKeys: String;
begin
  lItemKeys := '';
  for I := 0 to AList.Count - 1 do
  begin
    if I > 0 then lItemKeys := lItemKeys + ',';
    lItemKeys := lItemKeys + '''' + TKeyData(AList.Items[I]).ItemKey + '''';
  end;
  InitFromLocationQuery(AFinder, Format(SQL_LOCATION_FROM_LIST, [lItemKeys]));
end;  // InitFromLocationList

//==============================================================================
procedure TdmSearch.InitFromIndividualQuery(AFinder: TFinder);
var lText  : String;
    lCursor: TCursor;
begin
  FTerminated := False;
  lCursor:=HourglassCursor;
  try
    lText := SetupFilterText(AFinder.Text);
    AFinder.ClearSourceList;
    with qrySource do
      try
        if Active then Close;
        SQL.Text := Format(SQL_INDIVIDUAL_FROM_QUERY, [lText, lText, lText, lText]);
        try
          Open;
          First;
          while not Eof do begin
            AFinder.AddToSourceListWithAddition(FieldByName('Name_Key').AsString, FieldByName('IndividualName').AsString,
                                                [FieldByName('Forename').AsString, FieldByName('Initials').AsString,
                                                 FieldByName('Surname').AsString], 'INDIVIDUAL');
            Application.ProcessMessages;
            if FTerminated then Break;  // from while loop
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;  // InitFromIndividualQuery

//==============================================================================
procedure TdmSearch.InitFromOrganisationQuery(AFinder: TFinder;
  const tfClearAll: boolean = true);
var lText  : String;
    lCursor: TCursor;
begin
  FTerminated := false;
  lCursor := HourglassCursor;
  try
    if tfClearAll then AFinder.ClearSourceList;
    lText := SetupFilterText(AFinder.Text);
    with qrySource do
      try
        if Active then Close;
        SQL.Text := Format(SQL_ORGANISATION_FROM_QUERY, [lText, lText]);
        try
          Open;
          First;
          while not Eof do begin
            AFinder.AddToSourceListWithAddition(FieldByName('Name_Key').AsString,
                                                FieldByName('OrganisationName').AsString,
                                                [FieldByName('Acronym').AsString,
                                                 FieldByName('Full_Name').AsString],
                                                'ORGANISATION');
            Application.ProcessMessages;
            if FTerminated then Break;  // from while loop
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated := true;
  end;
end;  // InitFromOrganisationQuery

//==============================================================================
procedure TdmSearch.InitFromNameQuery(AFinder: TFinder);
begin
  InitFromIndividualQuery(AFinder);
  InitFromOrganisationQuery(AFinder, false);
end;  // InitFromNameQuery

//==============================================================================
procedure TdmSearch.InitFromNameList(AFinder: TFinder; const AList: TStrings;
  const AKeyList:TKeyList);
var iIndex :integer;
    lItemKeys: String;
    lqryNames: TJnccQuery;
    lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  AFinder.ClearSourceList;
  lItemKeys := '';
  with AList do
    for iIndex:=0 to Count-1 do
      lItemKeys := ',''' + AKeyList.Items[iIndex].KeyField1 + '''';
  lItemKeys[1] := ' ';

  lqryNames := TJnccQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lqryNames]);
  with lqryNames do
    try
      ParseSQL := false;
      SQL.Text := Format(SQL_NAME_FROM_LIST, [lItemKeys]);
      Open;
      while not Eof do begin
        if FieldByName('Organisation').AsBoolean then
          AFinder.AddToSourceList(FieldByName('Name_Key').AsString,
                                  FieldByName('OrganisationName').AsString,
                                  [FieldByName('Acronym').AsString, FieldByName('Full_Name').AsString])
        else
          AFinder.AddToSourceList(FieldByName('Name_Key').AsString,
                                  FieldByName('IndividualName').AsString,
                                  [FieldByName('Forename').AsString, FieldByName('Initials').AsString,
                                   FieldByName('Surname').AsString]);
        Next;
      end;
      Close;
    finally
      Free;
    end;
  DefaultCursor(lCursor);
end;  // InitFromNameList

//==============================================================================
procedure TdmSearch.AddReferencesToFinder(AFinder: TFinder; ASQL: String);
var lTitle: String;
begin
  AFinder.ClearSourceList;
  with qrySource do
    try
      if Active then Close;
      SQL.Text := ASQL;
      try
        Open;
        while not Eof do begin
          if FieldByName('Title').IsBlob then
            lTitle := dmGeneralData.ConvertRtfFieldToText(FieldByName('Title'))
          else
            lTitle := FieldByName('Title').AsString;
          AFinder.AddToSourceList(FieldByName('Source_Key').AsString,
                                  FieldByName('Author').AsString + ' - ' +
                                  FieldByName('Year_Vague_Date_Start').Text + ', ' +
                                  lTitle,
                                  [FieldByName('Author').AsString, lTitle]);
          Application.ProcessMessages;
          if FTerminated then Break;  // from while loop
          Next;
        end;
      except
        on EDatabaseError do Raise;
      end;
    finally
      Close;
    end;
end;  // AddReferencesToFinder

//==============================================================================
procedure TdmSearch.InitFromReferenceQuery(AFinder: TFinder);
var lTitleFilter: String;
    lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  FTerminated := false;
  try
    lTitleFilter := AFinder.Text;
    if lTitleFilter[1] <> '*' then lTitleFilter := '*' + lTitleFilter;
    AddReferencesToFinder(AFinder, Format(SQL_REFERENCE_FROM_QUERY,
                                          ['Author', 'VW_Reference_Authors',
                                           'Author', SetupFilterText(AFinder.Text),
                                           // Force a search anywhere in titles, as
                                           // they are RTF...
                                           'Title', SetupFilterText(lTitleFilter)]));
  finally
    DefaultCursor(lCursor);
    FTerminated := true;
  end;
end;  // InitFromReferenceQuery

{-------------------------------------------------------------------------------
}
procedure TdmSearch.InitFromReferenceKeywordQuery(AFinder: TFinder);
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  FTerminated := false;
  try
    AddReferencesToFinder(AFinder, Format(
        SQL_REFERENCE_FROM_KEYWORDS_QUERY, [AFinder.Text]));
  finally
    DefaultCursor(lCursor);
    FTerminated := true;
  end;
end;

//==============================================================================
procedure TdmSearch.InitFromReferenceList(AFinder: TFinder; const AList: TStrings;
  const AKeyList:TKeyList);
var iIndex   : integer;
    lItemKeys: String;
    lCursor  : TCursor;
begin
  lCursor:=HourglassCursor;
  FTerminated := false;
  try
    lItemKeys := '';
    with AList do
      for iIndex:=0 to Count-1 do
        lItemKeys := ',''' + AKeyList.Items[iIndex].KeyField1 + '''';
    lItemKeys[1] := ' ';
    AddReferencesToFinder(AFinder, Format(SQL_REFERENCE_FROM_LIST,
                                          ['Author', 'VW_Reference_Authors', lItemKeys]));
  finally
    DefaultCursor(lCursor);
    FTerminated := true;
  end;
end;  // InitFromReferenceList

//==============================================================================
procedure TdmSearch.InitFromSurveyQuery(AFinder: TFinder);
var
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  FTerminated := False;
  try
    AFinder.ClearSourceList;
    with dmDatabase.GetRecordset('usp_Surveys_Select', []) do begin
      while not Eof do begin
        AFinder.AddToSourceList(Fields['Survey_Key'].Value, Fields['Display_Name'].Value, []);
        Application.ProcessMessages;
        if FTerminated then Break;
        MoveNext;
      end;
      Close;
    end;
  finally
    DefaultCursor(cursor);
    FTerminated := True;
  end;
end;

//==============================================================================
procedure TdmSearch.InitFromSampleQuery(AFinder: TFinder);
var stSample, stLocation  :string;
    lVagueDate:TVagueDate;
    lCursor   :TCursor;
begin
  lCursor:=HourglassCursor;
  FTerminated:=false;
  try
    AFinder.ClearSourceList;
    with qrySource do
      try
        if Active then Close;
        SQL.Text := Format(SQL_SAMPLE_FROM_QUERY, [AppSettings.UserID, AppSettings.UserID]);
        try
          Open;
          First;
          while not Eof do begin
            lVagueDate.StartDate     :=FieldByName('Vague_Date_Start').AsInteger;
            lVagueDate.EndDate       :=FieldByName('Vague_Date_End').AsInteger;
            lVagueDate.DateTypeString:=FieldByName('Vague_Date_Type').AsString;
//PT - changed so that samples are found by date, location, type
//     check that a location is specified, if not then use the spatial reference
            if FieldByName('Item_Name').AsString <> '' then
              stLocation := FieldByName('Item_Name').AsString
            else
              stLocation := LocaliseSpatialRef(FieldByName('Spatial_Ref').AsString);
            stSample:=VagueDateToString(lVagueDate)+' - '+
                      stLocation+' - '+
                      FieldByName('Short_Name').AsString;
            AFinder.AddToSourceList(FieldByName('Sample_Key').AsString, stSample,
                                    [VagueDateToString(lVagueDate), stLocation,
                                     FieldByName('Short_Name').AsString]);
            Application.ProcessMessages;
            if FTerminated then Break;
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;  // InitFromSampleQuery

//==============================================================================
procedure TdmSearch.InitFromTaxOccQuery(AFinder: TFinder);
var stTaxOcc, stLocation  :string;
    lVagueDate:TVagueDate;
    lCursor   :TCursor;
    lText     : String;
begin
  lCursor:=HourglassCursor;
  FTerminated:=false;
  try
    AFinder.ClearSourceList;
    lText := SetupFilterText(AFinder.Text);
    with qrySource do
      try
        if Active then Close;
        SQL.Text := Format(SQL_TAXOCC_FROM_QUERY, [lText, lText]);
        FiSearchedChars := Length(AFinder.Text);
        try
          Open;
          First;
          while not Eof do begin
            lVagueDate.StartDate     :=FieldByName('Vague_Date_Start').AsInteger;
            lVagueDate.EndDate       :=FieldByName('Vague_Date_End').AsInteger;
            lVagueDate.DateTypeString:=FieldByName('Vague_Date_Type').AsString;
            if FieldByName('Location Name').AsString <> '' then
              stLocation := FieldByName('Location Name').AsString
            else
              stLocation := LocaliseSpatialRef(FieldByName('Spatial_Ref').AsString);
            stTaxOcc := FieldByName('Taxon Name').AsString + ' - ' +
                        FieldByName('Short_Name').AsString + ' at '+
                        stLocation + ' on ' +
                        VagueDateToString(lVagueDate);
            AFinder.AddToSourceList(FieldByName('Taxon_Occurrence_Key').AsString, stTaxOcc,
                                    [FieldByName('Taxon Name').AsString,
                                     FieldByName('Short_Name').AsString,
                                     stLocation, VagueDateToString(lVagueDate)]);
            Application.ProcessMessages;
            if FTerminated then Break;
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;  // InitFromTaxOccQuery

//==============================================================================
// Search for an Admin Area that appears in a hierarchy rooted at a node
// with ADMIN_TYPE_KEY = SourceParameter.
procedure TdmSearch.InitFromAdminAreaQuery(AFinder:TFinder; const SourceParameter:string);
var lAdminArea,lText:String;
    lCursor         :TCursor;
    lItemIndex      :integer;
    lTrapLoops      :TStringList;
  //----------------------------------------------------------------------------
  function FindTopArea(const AKey: TKeyString):TKeyString;
  var lParentKey : TKeyString;
  begin
    // Move up the hierarchy of admin areas to the top one
    with qryAdmin do begin
      ParseSQL := false;
      // Get the parent key for the current one
      SQL.Text:='SELECT Parent FROM Admin_Area WHERE Admin_Area_Key = '''+AKey+'''';
      Open;
      lParentKey:=FieldByName('Parent').AsString;
      Close;
    end;
    Application.ProcessMessages;
    lTrapLoops.Add(lParentKey); // keep a track of the parents we have found in this branch
    if FTerminated then
      Result:=''
    else if lParentKey<>'' then
      Result:=FindTopArea(lParentKey)  // Get the parent Area.
    else
      Result:=AKey;
  end;  // FindTopArea
  //----------------------------------------------------------------------------
begin
  if SourceParameter <> '' then begin
    FTerminated:=false;
    lCursor:=AppStartCursor;
    FTopKeys.Clear;
    lTrapLoops := TStringList.Create;
    lTrapLoops.Sorted := True;
    lTrapLoops.Duplicates := dupError;
    try
      AFinder.ClearSourceList;
      lText := SetupFilterText(AFinder.Text);
      dmDatabase.SetDatabaseLocal([qryAdmin]);
      with qrySource do
        try
          // Retreive all top Admin Area Keys
          if Active then Close;
          SQL.Text:=SQL_TOP_ADMINAREA;
          Parameters.ParamByName('AdminType').Value:=SourceParameter;
          Open;
          while not Eof do begin
            FTopKeys.Add(FieldByName('Admin_Area_Key').AsString);
            Next;
          end;
          Close;
          // Retreive all matching names
          SQL.Text := Format(SQL_FILTERED_ADMINAREA,[lText,lText]);
          Open;
          First;
          while not Eof do begin
            // For each record, check it belongs to current list
            lTrapLoops.Clear;
            lItemIndex := -1;
            try
              lItemIndex := FTopKeys.IndexOf(FindTopArea(FieldByName('Admin_Area_Key').AsString));
            except
              on EStringListError do; // ignore this, a loop occurred in the dictionary parentage
            end;
            if not FTerminated and (lItemIndex<>-1) then begin
             if FieldByName('Short_Code').AsString = '' then
                lAdminArea := FieldByName('Item_Name').AsString
              else
                lAdminArea := FieldByName('Short_Code').AsString + ', ' + FieldByName('Item_Name').AsString;
              AFinder.AddToSourceList(FieldByName('Admin_Area_Key').AsString, lAdminArea,
                                      [FieldByName('Item_Name').AsString, FieldByName('Short_Code').AsString]);
            end;
            Application.ProcessMessages;
            if FTerminated then Break;  // from while loop
            Next;
          end;
        finally
          Close;
        end;
    finally
      FTopKeys.Clear;
      DefaultCursor(lCursor);
      FTerminated:=true;
    end;
  end;
end;  // InitFromAdminQuery

{ ------------------------------------------------------------------------------
// Search for an Admin Area with ADMIN_TYPE_KEY = typekey
// --------------------------------------------------------------------------- }
procedure TdmSearch.InitFromAdminType(AFinder: TFinder; const TypeKey: TKeyString);
var oldcursor: TCursor;
    MatchText: string;
    AreaName: string;
begin
  oldcursor := HourglassCursor;
  FTerminated := False;
  try
    AFinder.ClearSourceList;
    MatchText := SetupFilterText(AFinder.Text);
    dmDatabase.SetDatabaseLocal([qryAdmin]);
    with qrySource do begin
      Close;
      SQL.Text := Format(SQL_TYPE_FILTERED_ADMINAREA, [MatchText, MatchText]);
      Parameters.ParamValues['AdminType'] := TypeKey;
      Open;
      try
        while not (Eof or FTerminated) do begin
          AreaName := FieldByName('Short_Code').AsString;
          if AreaName <> '' then AreaName := AreaName + ', ';
          AreaName := AreaName + FieldByName('Item_Name').AsString;
          AFinder.AddToSourceList(FieldByName('Admin_Area_Key').AsString, AreaName,
                                  [FieldByName('Item_Name').AsString,
                                   FieldByName('Short_Code').AsString]);
          Next;
          Application.ProcessMessages;
        end;
      finally
        Close;
      end;
    end;
  finally
    FTerminated := True;
    DefaultCursor(oldcursor);
  end;
end;

//==============================================================================
procedure TdmSearch.InitFromFeatureQuery(AFinder: TFinder);
var lText  :string;
    lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  FTerminated:=false;
  try
    AFinder.ClearSourceList;
    with qrySource do
      try
        if Active then Close;
        SQL.Text := SQL_FEATURE_FROM_QUERY;
        try
          Open;
          First;
          while not Eof do begin
            lText := FieldByName('Item_Name').AsString + ' - ' + FieldByName('Short_Name').AsString;
            AFinder.AddToSourceList(FieldByName('Location_Feature_Key').AsString, lText,
                                    [FieldByName('Item_Name').AsString, FieldByName('Short_Name').AsString]);
            Application.ProcessMessages;
            if FTerminated then Break;
            Next;
          end;
        except
          on EDatabaseError do Raise;
        end;
      finally
        Close;
      end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;  // InitFromFeatureQuery

//==============================================================================
procedure TdmSearch.SetDatabaseName(const ADatabaseLocation: string);
var ltfActive, ltfNeedReset: Boolean;
begin
  with qrySource do begin
    ltfNeedReset := ((Connection = dmDatabase.dbLocal) and (ADatabaseLocation<>'LOCAL'));
    if ltfNeedReset then begin
      ltfActive := Active;
      Close;
      dmDatabase.SetDatabaseLocal([qrySource]);
      Active := ltfActive;
    end;
  end;
end;  // SetDatabaseName

//==============================================================================
procedure TdmSearch.Terminate;
begin
  FTerminated := True;
end;  // Terminate

//==============================================================================
procedure TdmSearch.OpenSourceQuery;
var lCursor : TCursor;
begin
  lCursor := HourglassCursor;
  try
    qrySource.Open;
  finally
    DefaultCursor(lCursor);
  end;
end;  // OpenSourceQuery

//==============================================================================
{ Generic version of the initfromquery methods.  Takes the SQL, which assumes
   a parameter is present SearchStr, to search on.  Also takes a callback method
   which does the actual addition to the source lists }
procedure TdmSearch.InitGenericQuery(AFinder:TFinder; const SourceParameter: string;
  const iSQL: string; iAddToListMethod: TAddToListCallback; const iBiotope: Boolean;
  iEnterListKey: Boolean);
var lLVKeys: string;
    lCursor: TCursor;
    lNeedToFilterListVersion : boolean;
begin
  lCursor := AppStartCursor;
  try
    FTerminated := False;
    lNeedToFilterListVersion := ((FTaxonSearchType = stList) or iBiotope) and iEnterListKey;
    if (not lNeedToFilterListVersion) or (SourceParameter <> '') then begin
      AFinder.ClearSourceList;
      with qrySource do
        try
          if Active then Close;
          SQL.Text := iSQL;
          // Find latest version of list to search
          if lNeedToFilterListVersion then begin
            lLVKeys := dmGeneralData.SetListVersionKeys(iBiotope, SourceParameter).LatestVersions;
            dmGeneralData.EnterListVersionKeys(SQL, lLVKeys);
          end;
          try
           OpenSourceQuery;
            if Assigned(iAddToListMethod) then
            while not Eof do begin
              iAddToListMethod(qrySource, AFinder);
              Application.ProcessMessages;
              if FTerminated then Break;  // from while loop
              Next;
            end;
          except
            on EDatabaseError do Raise;
          end;
        finally
          Close;
        end;
    end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;

end;  // InitGenericQuery

//==============================================================================
{ Add an individual taxon into the source list - called by InitGenericQuery }
procedure TdmSearch.AddTaxonToList(iQuery: TJNCCQuery; AFinder: TFinder);
var
  lAuthority, lAbbr, lListName, lTaxon, lSearchCode: String;
  SearchCodeField: TField;
begin
  lAuthority := iQuery.FieldByName('Authority').AsString;
  lAbbr      := iQuery.FieldByName('Abbreviation').AsString;
  lListName  := iQuery.FieldByName('ListName').AsString;
  lTaxon     := iQuery.FieldByName('ItemName').AsString;

  SearchCodeField := iQuery.FindField('SearchCode');
  if Assigned(SearchCodeField) then
    lSearchCode := SearchCodeField.AsString
  else
    lSearchCode := '';

  { Check to see how we should order each line returned }
  if lAbbr <> '' then
    if FOrderByAbbreviation = True then
      lTaxon := lAbbr + ' - ' + lTaxon
    else
      // Don't order by abbreviation
      lTaxon := lTaxon + ' - [' + lAbbr + ']';

  if FTaxonSearchType in [stUnrestricted, stPreferredLists,stPreferredTaxa,stRecommendedFull] then begin
    lTaxon := lTaxon + ' - ' + lListName;
    AFinder.AddToSourceList(iQuery.FieldByName('Taxon_List_Item_Key').AsString, lTaxon,
                            [lAuthority, lListName]);
    if lSearchCode <> '' then
      AFinder.AddToSourceList(iQuery.FieldByName('Taxon_List_Item_Key').AsString,
                              lSearchCode + ' - ' + lTaxon,
                              [lAuthority, lListName]);
  end else
    // Don't add the ListName, all items are in same list
    AFinder.AddToSourceList(iQuery.FieldByName('Taxon_List_Item_Key').AsString, lTaxon,
                            [lAuthority]);
    if lSearchCode <> '' then
      AFinder.AddToSourceList(iQuery.FieldByName('Taxon_List_Item_Key').AsString,
                              lSearchCode + ' - ' + lTaxon,
                              [lAuthority]);
end;  // AddTaxonToList

//==============================================================================
{ Add an individual biotope into the source list - called by InitGenericQuery }
procedure TdmSearch.AddBiotopeToList(iQuery: TJNCCQuery; AFinder: TFinder);
begin
  with iQuery do
    AFinder.AddToSourceList(FieldByName('Biotope_List_Item_Key').AsString,
                            FieldByName('BiotopeName').AsString,
                            [FieldByName('Original_Code').AsString,
                             FieldByName('Short_Term').AsString]);
end;  // AddBiotopeToList

{-------------------------------------------------------------------------------
  Initialise a query that searche the keywords concept group
}
procedure TdmSearch.InitFromConceptQuery(const applicationConceptGroup: TKeyString; AFinder: TFinder);
var
  lCursor : TCursor;
  lText   : string;
  lCGKey  : string;
begin
  lCursor:=HourglassCursor;
  FTerminated:=false;
  try
    // retrieve the concept group responsible for keywords
    with dmDatabase.GetRecordset('usp_ConceptGroup_Select_ForApplication',
        ['@Key', applicationConceptGroup]) do
      lCGKey := Fields['Concept_Group_Key'].Value;
    AFinder.ClearSourceList;
    with qrySource do begin
      if Active then Close;
      lText := SetupFilterText(AFinder.Text);
      SQL.Text := Format(SQL_CONCEPT_FROM_QUERY, [lCGKey, lText]);
      try
        Open;
        First;
        while not Eof do begin
          AFinder.AddToSourceList(
              FieldByName('Concept_Key').AsString,
              FieldByName('Plaintext').AsString,
              []);
          Application.ProcessMessages;
          if FTerminated then Break;
          Next;
        end;
      finally
        Close;
      end;
    end;
  finally
    DefaultCursor(lCursor);
    FTerminated:=true;
  end;
end;

end.

