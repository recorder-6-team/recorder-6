//==============================================================================
//  Unit:        ReportsData
//
//  Implements:  TdmReports
//
//  Description: Implements data access functionality for the Reports.
//
//  Author:      Eric Salmon
//  Created:     1 September 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 24 $
//    $Date: 19/12/07 10:11 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DelphiVersions.Inc'}

unit ReportsData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, HTTPApp, Dataclasses, ExceptionForm, ADODB
  {$IFDEF DELPHI7UP}, HTTPProd {$ENDIF};

type
  EReportDataError = class(TExceptionPath);

  { Data types for passing constraints around }
  TConstraint = ( cConfidential, cChecked, cVerified, cZeroAbundance );

  TConstraints = set of TConstraint;

  TFilterType = ( ftTaxon, ftBiotope, ftLocation );

  TdmReports = class(TBaseDataModule)
    qryLocations: TJNCCQuery;
    qryOtherNames: TJNCCQuery;
    qryAdminAreas: TJNCCQuery;
    qryMeasurements: TJNCCQuery;
    qryDesignations: TJNCCQuery;
    qryBiotopes: TJNCCQuery;
    qryGetSampleKeys: TJNCCQuery;
    qryTaxa: TJNCCQuery;
    qryTxCommon: TJNCCQuery;
    qryTxSci: TJNCCQuery;
    qryOccurrences: TJNCCQuery;
    qryTaxonSynonyms: TJNCCQuery;
    qryTxDes: TJNCCQuery;
    qryTxFacts: TJNCCQuery;
    qryTxOccs: TJNCCQuery;
    qryResults: TJNCCQuery;
    qryLocationsByKey: TJNCCQuery;
    qryBioFacts: TJNCCQuery;
    qryBioOccs: TJNCCQuery;
    ppListItems: TPageProducer;
    ppLocationsForTaxon: TPageProducer;
    ppLocationsForBiotope: TPageProducer;
    ppLocations: TPageProducer;
    ppTxOccsForLocation: TPageProducer;
    ppBtOccsForLocation: TPageProducer;
    procedure ppListItemsHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings;
      var ReplaceText: String);
  private
    { Private declarations }
    FFilterTableName : string;
    FKeyList: TEditableKeyList;
    FConstraints : TConstraints;
    FCurrentTaxonItemKeys: string;
    FCurrentBiotopeItemKeys: string;
    FCurrentLocationItemKeys: string;
    function GetFilter( iType : TFilterType ) : string;
    function GetConfidentialFilter : string;
    function GetCheckedFilter : string;
    function GetVerifiedFilter : string;
    function GetZeroAbundanceFilter : string;
    function GetCurrentTaxon : string;
    function GetCurrentBiotope : string;
    function GetCurrentLocation : string;
    procedure SetConstraints(const Value: TConstraints);
    procedure SetCurrentTaxonItemKeys(const Value: string);
    procedure SetCurrentBiotopeItemKeys(const Value: string);
    procedure SetCurrentLocationItemKeys(const Value: string);

  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure AssignKeyList( iKeyList : TKeyList );
    property Constraints : TConstraints read FConstraints write SetConstraints;
    property CurrentTaxonItemKeys : string read FCurrentTaxonItemKeys write SetCurrentTaxonItemKeys;
    property CurrentBiotopeItemKeys : string read FCurrentBiotopeItemKeys write SetCurrentBiotopeItemKeys;
    property CurrentLocationItemKeys : string read FCurrentLocationItemKeys write SetCurrentLocationItemKeys;
  end;

const
  ST_TAXON_OCCURRENCE_SQL =
      'SELECT T.Item_Name AS ItemName, '+
      '       T.Authority, TLI.Taxon_List_Item_Key AS ListItemKey '+
      'FROM Taxon AS T INNER JOIN '+
      '        (Taxon_Version AS TV INNER JOIN '+
      '            Taxon_List_Item AS TLI ON '+
      '        TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON '+
      '    T.Taxon_Key = TV.Taxon_Key '+
      'WHERE T.Language = ''La'' '+
      'AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name '+
      'AND TLI.Preferred_Name IN '+
      '  (SELECT TLI2.Preferred_Name '+
      '   FROM Taxon_List_Item AS TLI2, Taxon_Occurrence AS [TO], Taxon_Determination AS TD '+
      '   WHERE [TO].Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key '+
      '   AND TD.Taxon_List_Item_Key = TLI2.Taxon_List_Item_Key ';

  ST_BIOTOPE_OCCURRENCE_SQL =
      'SELECT IsNull(B.Original_Code, '''') + case when B.Original_Code + B.Short_Term is null '''' else '' '' end  + ' +
      '       IsNull(B.Short_Term, '''') as [ItemName], ' +
      '       BLI.Biotope_List_Item_Key AS ListItemKey '+
      'FROM Biotope AS B, Biotope_List_Item AS BLI, Biotope_Occurrence AS BO, Biotope_Determination AS BD '+
      'WHERE B.Biotope_Key = BLI.Biotope_Key '+
      'AND   BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key '+
      'AND   BD.Biotope_List_Item_Key = BLI.Biotope_List_Item_Key ';

  TAXON_CONFIDENTIAL   = ' AND [TO].CONFIDENTIAL = 0 ';
  TAXON_BIOTOPE_CHECKED        = ' AND ([TO].CHECKED = True OR [TO].CHECKED IS NULL) AND (BO.CHECKED = True OR BO.CHECKED IS NULL)';
  TAXON_BIOTOPE_VERIFIED       = ' AND ([TO].VERIFIED = True OR [TO].VERIFIED IS NULL) AND (BO.VERIFIED = True OR BO.VERIFIED IS NULL)';
  TAXON_ZERO_ABUNDANCE = ' AND [TO].ZERO_ABUNDANCE = 0';


//==============================================================================
implementation

{$R *.DFM}

resourcestring
  ResStr_NothingSelectedToReport = 'No item is selected to generate a report for';
  ResStr_CannotReportMixedList = 'A report cannot be generated because the selection criteria are of more than one type';

//==============================================================================
{ Takes the keylist which defines the input for a locations or occurrences
        report }
procedure TdmReports.AssignKeyList(iKeyList: TKeyList);
  var i : integer;
  lEditableKeyList : TEditableKeyList;
begin

  FKeyList.Assign(iKeyList);
  if FKeyList.Header.TableName = 'SPATIAL_REF' then
  begin
    lEditableKeyList := TEditableKeyList.Create;
    lEditableKeyList.SetTable('LOCATION');
    for i := 0 to FKeyList.HEader.ItemCount - 1 do
      lEditableKeyList.AddItem(FKeyList.Items[i].KeyField1, '');
    FKeyList := lEditableKeyList;
  end;
  FKeyList.ConvertToMixedData;
  if FKeyList.Header.ItemCount < 1 then
    raise EReportDataError.CreateNonCritical(ResStr_NothingSelectedToReport); // shouldn't happen
  FFilterTableName := Uppercase(FKeyList.Items[0].KeyField2);
end;


//==============================================================================
constructor TdmReports.Create(AOwner: TComponent);
begin
  inherited;
  FKeyList := TEditableKeyList.Create;
end;

//==============================================================================
destructor TdmReports.Destroy;
begin
  FKeyList.Free;
  FKeyList := nil;
  inherited;
end;

//==============================================================================
{ We use the PageProducer component to format the SQL filters as the SQL
     is loaded into a query component.
     Note the clever use of aliases so that we can use the same filter SQL for
     the list of taxa and the list of biotopes. }
procedure TdmReports.ppListItemsHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: String; TagParams: TStrings; var ReplaceText: String);
begin
  inherited;
  if TagString = 'taxon_filter' then
    ReplaceText := GetFilter( ftTaxon )
  else if TagString = 'biotope_filter' then
    ReplaceText := GetFilter( ftBiotope )
  else if TagString = 'filter' then
    ReplaceText := GetFilter( ftLocation )
  else if TagString = 'confidential' then
    ReplaceText := GetConfidentialFilter
  else if TagString = '_checked' then
    ReplaceText := GetCheckedFilter
  else if TagString = 'taxon_verified' then
    ReplaceText := GetVerifiedFilter
  else if TagString = 'taxon_zero_abundance' then
    ReplaceText := GetZeroAbundanceFilter
  else if TagString = 'current_taxon' then
    ReplaceText := GetCurrentTaxon
  else if TagString = 'current_biotope' then
    ReplaceText := GetCurrentBiotope
  else if TagString = 'current_location' then
    ReplaceText := GetCurrentLocation;
end;


{ Return the SQL fragment to filter the list of taxa }
function TdmReports.GetFilter( iType : TFilterType ) : string;
var
  i : integer;
  lFilterPart : string;
const
  REJECT_RECORDS = ' S.SAMPLE_KEY IS NULL';
begin
  if (FFilterTableName = 'TAXON_OCCURRENCE') and (iType <> ftBiotope) then
    Result := ' [TO].TAXON_OCCURRENCE_KEY in ('
  else if (FFilterTableName = 'BIOTOPE_OCCURRENCE') and (iType <> ftTaxon) then
    Result := ' BO.BIOTOPE_OCCURRENCE_KEY in ('
  else if (FFilterTableName = 'NAME') or (FFilterTableName = 'INDIVIDUAL')
       or (FFilterTableName = 'ORGANISATION') then
    Result := ' XD.DETERMINER in ('
  else if FFilterTableName = 'REFERENCE' then
    Result := ' XOS.SOURCE_KEY in ('
  else if FFilterTableName = 'SAMPLE' then
    Result := ' S.SAMPLE_KEY in ('
  else if FFilterTableName = 'SURVEY_EVENT' then
    Result := ' SE.SURVEY_EVENT_KEY in ('
  else if FFilterTableName = 'SURVEY' then
    Result := ' SE.SURVEY_KEY in ('
  else if (FFilterTableName = 'TAXON_LIST_ITEM') and (iType <> ftBiotope) then
    Result := ' TLI.TAXON_LIST_ITEM_KEY in ('
  else if (FFilterTableName = 'BIOTOPE_LIST_ITEM') and (iType <> ftTaxon) then
    Result := ' BLI.BIOTOPE_LIST_ITEM_KEY in ('
  else if FFilterTableName = 'LOCATION' then
    Result := ' S.LOCATION_KEY in ('
  else
    Result := REJECT_RECORDS; // throw away all records as filter not for taxa
  if Result <> REJECT_RECORDS then begin
    for i := 0 to FKeyList.Header.ItemCount-2 do begin
      if Uppercase(FKeyList.Items[i].KeyField2) <> FFilterTableName then
        raise EReportDataError.CreateNonCritical(ResStr_CannotReportMixedList); // keylist should be all one type
      Result := Result + '''' + FKeyList.Items[i].KeyField1 + ''',';
    end; // for
    { Last item needs closing bracket }
    Result := Result + '''' + FKeyList.Items[FKeyList.Header.ItemCount-1].KeyField1 + ''')';
  end;
  { For names filter (occurrences for location) we need to filter for both
      types of determiner.  Replace XD alias with TD and BD }
  if ((FFilterTableName = 'NAME') or (FFilterTableName = 'INDIVIDUAL')
       or (FFilterTableName = 'ORGANISATION')) and (iType=ftLocation) then begin
    lFilterPart := Result;
    lFilterPart[2] := 'B'; // biotope
    Result[2] := 'T'; // taxon
    // join to filter both biotope and taxon determinations
    Result := ' (' + Result + ' or ' + lFilterPart + ')';
  end;
end;


//==============================================================================
{ Accessor method allowing constraints to be defined for report }
procedure TdmReports.SetConstraints(const Value: TConstraints);
begin
  FConstraints := Value;
end;


//==============================================================================
function TdmReports.GetCheckedFilter: string;
begin
  if cChecked in FConstraints then
    Result := TAXON_BIOTOPE_CHECKED
  else
    Result := '';  // no filter
end;

//==============================================================================
function TdmReports.GetConfidentialFilter: string;
begin
  if cChecked in FConstraints then
    Result := TAXON_CONFIDENTIAL
  else
    Result := '';  // no filter
end;

//==============================================================================
function TdmReports.GetVerifiedFilter: string;
begin
  if cVerified in FConstraints then
    Result := TAXON_BIOTOPE_VERIFIED
  else
    Result := '';  // no filter
end;

//==============================================================================
function TdmReports.GetZeroAbundanceFilter: string;
begin
  if cZeroAbundance in FConstraints then
    Result := TAXON_ZERO_ABUNDANCE
  else
    Result := '';  // no filter
end;

//==============================================================================
{ Filter for the taxon we are currently outputting in places for taxa }
function TdmReports.GetCurrentTaxon: string;
begin
  Result := FCurrentTaxonItemKeys;
end;


//==============================================================================
{ Filter for the biotope we are currently outputting in places for taxa }
function TdmReports.GetCurrentBiotope: string;
begin
  Result := FCurrentBiotopeItemKeys;
end;


//==============================================================================
{ Filter for the location we are currently outputting in occurrences for place }
function TdmReports.GetCurrentLocation: string;
begin
  Result := FCurrentLocationItemKeys;
end;


{ Accessor method to set the taxon list item we are currently outputting }
procedure TdmReports.SetCurrentTaxonItemKeys(const Value: string);
begin
  FCurrentTaxonItemKeys := Value;
end;


//==============================================================================
procedure TdmReports.SetCurrentBiotopeItemKeys(const Value: string);
begin
  FCurrentBiotopeItemKeys := Value;
end;


procedure TdmReports.SetCurrentLocationItemKeys(const Value: string);
begin
  FCurrentLocationItemKeys := Value;
end;

end.
 