//==============================================================================
//  Unit:        GeneralData
//
//  Implements:  TdmGeneralData
//               TIDGenerator
//
//  Description: Main data module for Recorder 2000.
//
//               TIDGenerator
//               Takes care of creating unique new ID's for use in database keys.
//
//  Author:      John van Breda
//  Created:     29 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase. Functions to access databases have been move to
//               a database specific module (TdmDatabase).
//
//               Polly Shaw 11/11/2002
//               Added function SourcesShowInternal which brings up
//               references screen with the desired document shown.
//
//  Last Revision Details:
//    $Revision: 338 $
//    $Date: 8/04/10 16:52 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit GeneralData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, ComCtrls, DataClasses, Db, DBCtrls, JNCCDatasets, ExceptionForm,
  SpatialRefFuncs, SpatialRef, StdCtrls, Constants, ComAddinUnit, Sources,
  Find, Recorder2000_tlb, BaseFormUnit, ImgList, SQLConstants, ADODB, Variants,
  DatabaseAccessADO, AddinLinkedControls, XMLIntf, XMLDoc, XMLDom, RapTree,
  GISShape, VagueDate, ADODB_TLB, DropTarget, StrUtils;

const
  INSTALLATION_ID_LENGTH = 8;
  ROLLING_ID_LENGTH      = 8;

resourcestring
  ResStr_ContentsOfRucksack = '---Contents of Rucksack---';
  ResStr_TaxonNoDataEntry =
      'The taxon selected is from a species list that ' +
      'is inappropriate for data entry purposes.  Please choose a taxonomic checklist ' +
      'to select the taxon from.';

  ResStr_StillEditing =
      'You are still editing the data.'#13#13 +
      'Do you want to save your changes?';

  ResStr_InvalidInstallationID = 'An invalid installation ID %s was passed to the ID Generator for Create';
  ResStr_NoTableID =  'Unable to open the LAST_KEY table to get the current ID for table %s. The exact error was described as ';
  ResStr_NoRecord = 'Unable to locate/insert record in the LAST_KEY table for table ';
  ResStr_MoreThanOneRecord =  'More than one record exists in the LAST_KEY table for table ';
  ResStr_UnableToEditID = 'Unable to edit LAST_KEY table to get the current ID for table ';
  ResStr_ExactError = '. The exact error was described as ';
  ResStr_CannotEditLastKey =  'Unable to edit LAST_KEY table for table ';
  ResStr_CannotLockTable = 'Unable to lock the ID for table %s in the LAST_KEY.  Another user is editing this record';
  ResStr_InvalidCurrentID = 'An invalid ID %s was retrieved from the LAST_KEY table for table ';
  ResStr_CannotInsertNewID =  'Unable to insert a new ID into LAST_KEY table for table %s.  The exact error was described as ';
  ResStr_CannotUpdateID = 'Unable to update an ID in the LAST_KEY table.  The record is not editable';
  ResStr_UpdateExactError = 'Unable to update an ID in the LAST_KEY table.  The exact error was described as ';
  ResStr_InvalidIDType =  'An invalid ID type was passed to the ID Generator for ValidateID';
  ResStr_InvalidIdForGetNextID =  'An invalid ID was passed to the ID generator for GetNextID: ';
  ResStr_NoVersion = 'There are no versions of this list in the database.';
  ResStr_CannotInsertVersionKey = 'The List Version keys for the top level query could not be inserted.';
  ResStr_CannotDeleteRecord = 'Unable to delete this item as it is referenced by other records in the database';
  ResStr_CannotUpdateItem = 'Unable to update this item as it references an item which does not exist';
  ResStr_CannotInsertItem = 'Unable to insert this item as it references an item which does not exist';
  ResStr_NameKeyNotFound =  'Internal error - name_key not found';
  ResStr_FindIndividual = 'Find Individual';
  ResStr_FindName = 'Find Name';
  ResStr_FindOrganisation = 'Find Organisation';
  ResStr_FileInUse = 'Unable to overwrite %s, it may be in use by another program.';

  //Status
  ResStr_CheckingPrefRecords =  'Checking preferred records...';

  ResStr_MUnitCount     = 'Count';
  ResStr_MTypeAbundance = 'Abundance';

type
  EGeneralData = class(TExceptionPath);
  EIDGenError  = class(TExceptionPath);

  ENoListVersionFound = class(TExceptionPath);
  EListVersionKeys    = class(TExceptionPath);

  TIDType = (idInstallation, idRolling, idUndefined);

  TFontAccessor = class(TControl);  // allow access to protected methods

  { Used for getting all latest list versions (incl. amendments) and THE latest list version}
  TListVersion = record
    LastVersion : TKeyString;
    LatestVersions : string;
  end;

  {TIDGenerator - creates new id's for use in database keys}
  TIDGenerator = class(TObject)
  private
    FInstallationID : string;

    function GetCurrentID(const TableName: string; InsertIfMissing: Boolean): string;
    procedure InsertNewID(const TableName: string);
    procedure UpdateID(const ID: string);

    function GenerateNewID: string;
    function ValidateID(const ID: string; const IDType: TIDType): Boolean;
    function IncrementChar(const IncChar: Char): Char;
    function IncrementID(const ID: string; const IDType: TIDType): string;
  public
    constructor Create(const InstallationID: string);
    function GetNextID(const ID: string): string;
    function GetCurrentKey(const TableName: string): TKeyString;
    function GetNextKey(const TableName: string): TKeyString;
  end;

  EGeneralDataError = class(TExceptionPath);
  ERecordMissingError = class(EGeneralDataError);

  // Simple class to hold an additional string on a combo box
  TStringClass = class
  public
    Item: String;
  end;

  TGetName = function(const key: TKeyString): String of object;

  TdmGeneralData = class(TBaseDataModule)
    qryIDGenSelect  : TJNCCQuery;
    qryIDGenInsert  : TJNCCQuery;
    qryGridSquares: TJNCCQuery;
    qrySurvey: TJNCCQuery;
    qryReference: TJNCCQuery;
    qryEvent: TJNCCQuery;
    qryPreferred: TJNCCQuery;
    qryAllPurpose: TJNCCQuery;
    qryRecordStrings: TJNCCQuery;
    qryUsers: TJNCCQuery;
    qryLatestListVersion: TJNCCQuery;
    qryMetadata: TJNCCQuery;
    qryTaxonName: TJNCCQuery;
    dsUsers: TDataSource;
    qryOrganisation: TJNCCQuery;
    qryIndividual: TJNCCQuery;
    spNextKey: TADOStoredProc;
    spRepairLastKey: TADOStoredProc;
    qryTempLoc: TJNCCQuery;
    qryTempRecorders: TJNCCQuery;
  private
    FIDGen       : TIDGenerator;
    FTableList   : TStringList;
    FPrimaryKeys : TStringList;
    function GetTableList: TStringList;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function ConvertRichTextToText(ARichEdit: TRichEdit): string; overload;
    function ConvertRichTextToText(ADBRichEdit: TDBRichEdit): string; overload;
    function ConvertRichTextToText(const rtfText: String): String; overload;
    function ConvertRtfFieldToText(AField: TField): String; 
    procedure ExecuteSQL(const iSQL, ErrMsg: string; ParseSQL: Boolean = True);
    procedure SetNameIDAndDate(ADataSet:TDataSet; const ANameIDField, ADateField:string);

    function GetTaxonName(const AKey:TKeyString):string;
    function GetCommonName(const AKey:TKeyString):string;
    function IsTaxonNameCommon(const AKey:TKeyString):boolean;
    function GetTaxonPreferredKey(const AKey:TKeyString):TKeyString;
    function GetTaxonChecklistKey(const AKey:TKeyString):TKeyString;
    function GetBiotopeCodeName(const AKey:TKeyString):string;
    function GetAdminAreaName(const AKey:TKeyString):string;
    function GetName(const AKey:TKeyString):string;
    function GetOrganisationName(const AKey:TKeyString):string;
    function GetIndividualName(const AKey:TKeyString):string;
    function GetLocationNameFromLocNameKey(var AKey:string): string;
    function GetReferenceText(const AKey:TKeyString):string;
    function GetSourceFileText(const AKey:TKeyString):string;
    function GetAnySourceText(const AKey:TKeyString): string;
    function CheckReference(const ARef: string): TKeyString;
    function GetLocNameKeyFromLocKey( const iLocationKey : TKeyString ) : TKeyString;
    function GetLocKeyFromLocNameKey( const iLocationNameKey: TKeyString): TKeyString;
    procedure GetRecordStrings( ioRecordList: TStrings;
      const iTable: string; const iKey: TKeyString; APk1Delimiter: string=''''); overload;
    procedure GetRecordStrings(ioRecordList: TStrings;
      const iTable: string; const iKey1, iKey2: TKeyString;
      APk1Delimiter: string=''''; APk2Delimiter: string=''''); overload;
    function IsSystemSupplied(const iTable:string; const iKey:TKeyString):boolean;
    function IsSystemSuppliedRecord( iDataset : TDataset):boolean;
    function GetNextKey(const ATableName, AKeyFieldName: string): TKeyString; overload;
    function GetNextKey(const ATableName, AKeyFieldName: string;AtfCheckKey : Boolean): TKeyString;overload;
    procedure RepairLastKeyTable(const ATableName, AKeyFieldName: string);
    function CheckKeyExists(const ATableName, AFieldName, AKeyValue: string):boolean;
    function CheckBiotope(const AText:string; var AKey: TKeyString; var
        ACaption:string): boolean;
    function CheckTaxon(iEditControl: TEdit; var oKey: TKeyString): boolean;
    function CheckIndividual(ALinkedName: TAddinLinkedEdit): Boolean; overload;
    function CheckConcept(const findTitle, noResultsMessage: String; findType: TFindType;
        aEditControl: TEdit; var oKey: TKeyString): Boolean; overload;
    function CheckConcept(const findTitle, noResultsMessage: String; findType: TFindType;
        var ASearchText: String; var oKey: TKeyString; ATabToFocus: TTabSheet = nil): Boolean; overload;
    function CheckZeroAbundance(const ATaxOccKey:TKeyString):boolean;
    procedure CheckZOForSample(const AsampleKey:TKeyString);
    procedure CheckLastKeyTable;
    procedure PostLastKey(istrTableName, istrLastKey: string);
    procedure CheckPreferred;
    procedure CompletePrefCheck(const istrTableName,istrPrefField: string);
    function CheckWithin2KM(iCoord1, iCoord2: TLatLong):boolean;
    procedure SetupStandardQuery(iQuery: TJNCCQuery;
      const iStandardSQL: TStandardSQL; const iKeyValue: TKeyString);
    procedure ConvertListUsingSQL(ASourceKeyList : TKeyList;
                                  ADestKeyList : TEditableKeyList;
                                  const ASQL : string);
    procedure GetKeyListFromStandardQuery(iKeyList: TEditableKeyList;
      const iStandardSQL: TStandardSQL; const iKeyValue: TKeyString);
    function SetListVersionKeys(const iBiotope: boolean; const iListKey : TKeyString) : TListVersion;
    procedure EnterListVersionKeys(ioSql : TStrings; const iListVersionKeys : string);
    procedure BuildMeasurementUnitList(ioList : TStrings);
    // Spatial Reference / Location transfer functions procedures
    function SetSpatialRef(const ASpatialRef, ALocKey, ADisplaySystem: string): TSpatialRefValues;
    // Database procedures/functions
    function GetKeysToString(const iSQL:string):string;
    procedure DelSources(const ATableName, AKeyField:string; const AKeyValue:TKeyString);
    // Determine whether a place card has a COM header or not.
    function GetPlaceCardCOMClassID(const AFilename: string; var ioAddinName : string;
      var ioClassID : string) : boolean;
    function COMClassInstalled( iStrClassID: string ) : boolean;
    // Functions for Internal Document and Sources component. Called from all over the place
    procedure SourcesFindInternal(Sender: TObject);
    procedure SourcesAddInternal(Sender: TObject);
    procedure SourcesShowInternal(Sender: TObject; SourceKey: String);
    function IsSourceInternal(const iSourceKey: TKeyString): Boolean;
    function CheckTaxonInstalled(var ATaxonListItemKey: TKeyString; ATaxonName: string): integer;
    procedure ReadLatLong(const iField: TField; var iRef: double); overload;
    procedure ReadLatLong(const AValue: Variant; var ARef: Double); overload;
    procedure PopulateQualifierCombo(iComboBox: TComboBox;
      const iTypeKey: String);
    property IDGenerator: TIDGenerator read FIDGen;
    property TableList : TStringList read GetTableList;
    function CheckName(const AText:string; var AKey: TKeyString; var
        ACaption:string): boolean; overload;
    function CheckIndividual(const AText:string; var AKey: TKeyString; var
        ACaption:string): boolean; overload;
    function CheckOrganisation(ALinkedName: TAddinLinkedEdit): Boolean; overload;
    function CheckName(ALinkedName: TAddinLinkedEdit): Boolean; overload;
    function GetTaxonCommonName(const ATaxonListItemKey: TKeyString; var ioLang: string): string;
    function GetTaxonScientificName(const ATaxonListItemKey: TKeyString; var ioLang: string): string;
    function GetTaxonNamesObject(const ATaxonListItemKey: TKeyString): TTaxonNames;
    function GetBestTaxonName(const ATaxonListItemKey: TKeyString): string;
    function GetCommonNameForTaxonOccurrence(const iTaxonOccurrenceKey: TKeyString): string;
    function IncludeSubTaxa: boolean;
    function CheckTempSurvey(const ASurveyKey:string ) : boolean;
    function CheckLicence(const ALicenceKey:string ) : boolean;
    procedure GetTaxonSearchOptions( icmbOptions : TComboBox );
    function GetRucksackTaxaAsCommaList(const iRucksackName: string): string;
    function GetAuthorFieldSQL : string;
    function Custodian(const ATableName, APrimaryKey: string; AKeyString: TKeyString): string;
    function HaveCustody(const ATableName, APrimaryKey: string; AKeyString: TKeyString): Boolean;
    procedure CheckMapTables;
    function CheckOrFindDataItem(var AText: string; var AKey: TKeyString;
        AFindtype: TFindType; const AFindCaption: string): boolean;
    function CheckIndividual(iEditControl: TEdit; var oKey: TKeyString): boolean;
        overload;
    function CheckOrganisation(const AText:string; var AKey: TKeyString; var
        ACaption:string): boolean; overload;
    procedure CheckTaxonAllowsDataEntry(const AListItemKey: string);
    function GetReportCaption(const AFileName: string): string;
    procedure PopulateWizardReportCombo(ACombo: TComboBox; const APath: string);
    function HasFullEditAccess(const ATableName, AKeyName, AKey: String): Boolean;
    function SessionHasFullEditAccess(const ATableName, AKeyName, AKey: String): Boolean;
    function GetLocationSpatialRef(const ALocationKey: string): TValidSpatialRef;
    function GetLocationName(const ALocationKey: string): string;
    function GetValidationState(const ATLIKey: string; ACompetency: integer;
        ADetCount: integer; APrefDetInvalid: boolean): Variant;
    function IsNameKeyForOrganisation(const AKey: string): Boolean;
    procedure SetCanvasColourForFilterNode(ACanvas: TCanvas; AFiltered: boolean);
    function GetDetTypeVerified(const ADetTypeKey: String): Integer;
    function GetPreferredTaxonDetType(const AOccurrenceKey: String): String;
    function GetPreferredBiotopeDetType(const AOccurrenceKey: String): String;
    procedure WriteSHPFiles(listOne, listTwo: TSVOShapeList; const fileName, markerOne, markerTwo, spatialRefSystem: String);
    function GetVagueDateFromRecordset(rs: ADODB._Recordset; const prefix: String = ''): TVagueDate;
    function DropLinkedEditText(ctrl: TAddinLinkedEdit; format: Integer; data: TKeyList;
        getName: TGetName; text: TStringList): Boolean;
    function IsGroupCorrect(const ATLIKey:string; AGroupKey: string): Boolean;
  end;

//----------------------------------------------------------------------------
// Global functions
//----------------------------------------------------------------------------
function  ConvertFieldName(const AText:string):string;
function  ConfirmSaveAndClose:word;
function  ConvertSearchString(AString:string):string;
procedure GenericDrawTaxonNames(ACanvas: TCanvas; const taxonName: TTaxonNames; iRect: TRect; iDrawChoppedText: boolean);

//------------------------------------------------------------------------------

var
  dmGeneralData : TdmGeneralData;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, ApplicationSettings, APIUtils, LocationDetailsData, GridSquareItem, Filter,
  ComObj, FormActions, GenFuncs, Rucksack, GeneralFunctions, Search, References, GISReadWrite,
  DefaultPaths;

const
  //Queries to deal with version amendments
  BIOTOPE_VERSION_QUERY = 'SELECT BT_CL_Version_Key AS ListVersionKey, Version_Is_Amendment ' +
                          'FROM Biotope_Classification_Version ' +
                          'WHERE Biotope_Classification_Key = :ListKey ' +
                          'ORDER BY Revision_Number DESC';

  TAXON_VERSION_QUERY =   'SELECT Taxon_List_Version_Key AS ListVersionKey, Version_Is_Amendment ' +
                          'FROM Taxon_List_Version ' +
                          'WHERE Taxon_List_Key = :ListKey ' +
                          'ORDER BY Version DESC';

resourcestring
  ResStr_NoMoreIDGenerated    =  'No more IDs can be generated';
  ResStr_RecordListNotCreated = 'Record list not created';
  ResStr_SourceRecNotFound    =  'Source record not found for external source';
  ResStr_SourceNotFoundChk    =  'Source not found in check for internal sources';
  ResStr_RuckSack             = 'Rucksack';
  ResStr_IncludeSubTaxa =
      'Do you want to expand the list to include all taxa in the hierarchy '
      + 'contained by the taxa you have selected?'#13#10
      + 'Hold the Control Key when dragging to bypass this dialog and expand the list.'#13#10
      + 'Hold the Shift Key when dragging to bypass this dialog without expanding the list.';

//==============================================================================
function ConvertSearchString(AString: string):string;
var lIdx: Integer;
begin
  Result := '';
  if AString = '' then Exit;
  
  // '*' doesn't work in SQL Server, '%' does.
  if (AString[1] = '*') and (dmDatabase.DatabaseType <> dbAccess) then
    AString[1] := '%';

  for lIdx := 1 to Length(AString) do
    if AString[lIdx] = '''' then
      Result := Result + ''''''
    else
    if AString[lIdx] in ['[', '#', '?'] then
      Result := Result + '[' + AString[lIdx] + ']'
    else
      Result := Result + AString[lIdx];
end;  // ConvertSearchString

//==============================================================================
function ConvertFieldName(const AText:string):string;
var lIdx,lLen:integer;
begin
  Result:=AText;
  if Pos('VAGUE_DATE',AText)>0 then
    Result:=Copy(AText,1,Pos('VAGUE_DATE',AText)-1)+'DATE';

  if Result<>'' then begin
    Result:=LowerCase(Result);
    lLen:=Length(Result);
    // First convert '_' to ' '
    lIdx:=Pos('_',Result);
    while lIdx>0 do begin
      Result[lIdx]:=' ';
      lIdx:=Pos('_',Result);
    end;  // while
    // Now change first letter of each 'word' to uppercase
    Result[1]:=UpCase(Result[1]);
    for lIdx:=1 to lLen do
      if (Result[lIdx]=' ') and (lIdx+1<=lLen) then
        Result[lIdx+1]:=UpCase(Result[lIdx+1]);
  end;  // if
end;  // ConvertFieldName

//==============================================================================
function ConfirmSaveAndClose:word;
begin
  Result:=MessageDlg(ResStr_StillEditing, mtConfirmation, [mbYes,mbNo,mbCancel], 0);
end;  // ConfirmSaveAndClose

//==============================================================================
{ Surrogate draw method allowing Taxon Names to be italicised in controls such
  as TListBoxes etc. Only supply an iControl if you want this font to be set, not
  the canvas (e.g for tree views) }
procedure GenericDrawTaxonNames(ACanvas: TCanvas; const taxonName: TTaxonNames; iRect: TRect; iDrawChoppedText: boolean);
var
  lRect : TRect;
  lstTaxonName, lstCommonName, lstTaxonAuthor, lstNameAsEntered, lstAuthorAsEntered : string;
  ltfTaxonItalic, ltfCommonItalic, drawCommonName, drawEnteredName   : boolean;
  //----------------------------------------------------------------------------
  procedure Italicise(iItalic: Boolean = true);
  begin
    if iItalic then begin
      if not (fsItalic in ACanvas.Font.Style) then begin
        ACanvas.Font.Style  := ACanvas.Font.Style + [fsItalic];
      end;
    end else begin
      if fsItalic in ACanvas.Font.Style then begin
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
        lRect.Left := lRect.Left + 2;
      end;
    end;
  end;  // Italicise
  //----------------------------------------------------------------------------
begin
  // because the Taxon Common Name table is incorrect, sometimes we need to swap
  // common and latin names
  if taxonName.CNItalic and (not taxonName.TNItalic) then begin
    lstCommonName   := taxonName.TaxonName;
    if taxonName.TNAttribute<>'' then
      lstCommonName := lstCommonName + ' ' + taxonName.TNAttribute;
    ltfCommonItalic := taxonName.TNItalic;
    lstTaxonName    := taxonName.CommonName;
    if taxonName.CNAttribute<>'' then
      lstTaxonName := lstTaxonName+ ' ' + taxonName.CNAttribute;
    lstTaxonAuthor := '';
    ltfTaxonItalic  := taxonName.CNItalic;
  end
  else begin // right way round
    lstCommonName   := taxonName.CommonName;
    if taxonName.CNAttribute<>'' then
      lstCommonName := lstCommonName + ' ' + taxonName.CNAttribute;
    ltfCommonItalic := taxonName.CNItalic;
    lstTaxonName    := taxonName.TaxonName;
    if taxonName.TNAttribute<>'' then
      lstTaxonName := lstTaxonName + ' ' + taxonName.TNAttribute;
    lstTaxonAuthor := taxonName.TNAuthor;
    ltfTaxonItalic  := taxonName.TNItalic;
  end;
  drawCommonName := (AppSettings.DisplayTaxonCommonNames) and
       (lstCommonName <> '') and (CompareText(lstCommonName, lstTaxonName)<>0);
  // default to output the name as entered, but can skip if the same as one of the other names
  lstNameAsEntered := taxonName.EnteredName;

  if taxonName.ENAttribute<>'' then
    lstNameAsEntered := lstNameAsEntered + ' ' + taxonName.ENAttribute;
  lstAuthorAsEntered := taxonName.ENAuthor;
  drawEnteredName:=CompareText(lstTaxonName, lstNameAsEntered)<>0;
  ACanvas.Font.name:='Tahoma'; // draws italics OK
  with ACanvas do begin
    FillRect(iRect);
    lRect := iRect;
    if drawCommonName then
    begin
      // Display in italics where a species name is present in latin
      Italicise(ltfCommonItalic);
      if iDrawChoppedText then DrawChoppedText(lstCommonName, ACanvas, lRect )
                          else ACanvas.TextOut(lRect.Left, lRect.Top, lstCommonName);
      // shift over required space before displaying proper
      lRect.Left := lRect.Left + TextWidth(lstCommonName + ' ');
      Italicise(false);
      if iDrawChoppedText then DrawChoppedText('(', ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, '(');
      lRect.Left := lRect.Left + TextWidth('(')+1;
      if CompareText(lstCommonName, taxonName.EnteredName)=0 then
        drawEnteredName:=false;
    end;

    // Display in italics where a species name is present in latin
    Italicise(ltfTaxonItalic);
    if iDrawChoppedText then DrawChoppedText( lstTaxonName, ACanvas, lRect )
                        else ACanvas.TextOut( lRect.Left, lRect.Top, lstTaxonName );
    lRect.Left := lRect.Left + TextWidth(lstTaxonName);
    Italicise(false);
    if AppSettings.DisplayTaxonAuthors and (lstTaxonAuthor<>'') then begin
      if iDrawChoppedText then DrawChoppedText(' ' + lstTaxonAuthor, ACanvas, lRect)
                        else ACanvas.TextOut(lRect.Left, lRect.Top, ' ' + lstTaxonAuthor);
      lRect.Left := lRect.Left + TextWidth(' ' + lstTaxonAuthor);
    end;
    if drawCommonName then begin
      lRect.Left := lRect.Left+1;
      if iDrawChoppedText then DrawChoppedText(')', ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, ') ');
      lRect.Left := lRect.Left + TextWidth(')');
    end;
    lRect.Left := lRect.Left + TextWidth(' ');
    if drawEnteredName and AppSettings.DisplayTaxonEnteredNames and (lstNameAsEntered<>'') then begin
      if iDrawChoppedText then DrawChoppedText('[', ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, '[');
      lRect.Left := lRect.Left + TextWidth('[ ');
      Italicise(taxonName.ENItalic);
      if iDrawChoppedText then DrawChoppedText(lstNameAsEntered, ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, lstNameAsEntered);
      lRect.Left := lRect.Left + TextWidth(lstNameAsEntered);
      Italicise(false);
      if AppSettings.DisplayTaxonAuthors and (lstAuthorAsEntered<>'') then begin
        if iDrawChoppedText then DrawChoppedText(' ' + lstAuthorAsEntered, ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, ' ' + lstAuthorAsEntered);
        lRect.Left := lRect.Left + TextWidth('  ' + lstAuthorAsEntered);
      end;
      if iDrawChoppedText then DrawChoppedText(']', ACanvas, lRect)
                          else ACanvas.TextOut(lRect.Left, lRect.Top, ']');
    end;
  end; // with ACanvas
end;  // GenericDrawTaxonNames

//==============================================================================
//==============================================================================
// TIDGenerator

constructor TIDGenerator.Create(const InstallationID: string);
begin
  inherited Create;
  //Validate the installation ID
  if not ValidateID(InstallationID, idInstallation) then
    raise EIDGenError.Create(Format(ResStr_InvalidInstallationID, [InstallationID]));

  FInstallationID:= InstallationID;
end;  // Create

//==============================================================================
//Get the current ID from the LAST_KEY table
function TIDGenerator.GetCurrentID(const TableName: string; InsertIfMissing: Boolean): string;
var
  CurrentID : string;
  Attempt   : Integer;
begin
  Result:='';
  with dmGeneralData.qryIDGenSelect do
  begin
    //Attempt to find the correct record in the database
    Close;
    Parameters.ParamByName('TableName').Value:= TableName;
    try
      Open;
    except
      on E:Exception do
      raise EIDGenError.Create(Format(ResStr_NoTableID, [TableName]) + E.Message);
    end;

    if Eof then //ID doesn't exist so insert a new one if this is the first attempt
    begin
      if InsertIfMissing then
      begin
        InsertNewID(TableName); //Insert new ID
        CurrentID:= GetCurrentID(TableName, False); //Call this procedure again to edit the new record
      end
      else
        raise EIDGenError.Create(ResStr_NoRecord + TableName);
    end
    else //At least one record has been found
    begin
      if RecordCount > 1 then //More than one record
        raise EIDGenError.Create(ResStr_MoreThanOneRecord + TableName);

      //Check the record is editable (ie. RequestLive didn't fail)
      if not CanModify then
        raise EIDGenError.Create(ResStr_UnableToEditID + TableName);

      //Attempt to put record in edit state, hence locking the record
      Attempt:= 1;
      while (State <> dsEdit) and (Attempt <= 3) do
        try
          Edit;
        except
          on E:Exception do
            //Another user is currently editing this record
            if dmdatabase.CheckError(E, dbeRecordLocked) then begin
              Sleep(2000);
              Inc(Attempt);
            end else
            if dmdatabase.CheckError(E, dbeOther) then
              raise EIDGenError.Create(ResStr_UnableToEditID + TableName + ResStr_ExactError + E.Message)
            else
              raise EIDGenError.Create(ResStr_CannotEditLastKey + TableName + ResStr_ExactError + E.Message);
        end;

      //Check to see if we managed to lock the record
      if Attempt > 3 then
        raise EIDGenError.Create(Format(ResStr_CannotLockTable,[TableName]));

      CurrentID:= FieldByName('Last_Key_Text').AsString;

      //Check current ID
      if not ValidateID(CurrentID, idRolling) then
        raise EIDGenError.Create(Format(ResStr_InvalidCurrentID, [CurrentID]) + TableName);
    end;
    Result:= CurrentID;
  end;
end;  // GetCurrentID

//==============================================================================
//Insert a new ID into the LAST_KEY table
procedure TIDGenerator.InsertNewID(const TableName: string);
begin
  with dmGeneralData.qryIDGenInsert do
  begin
    Parameters.ParamByName('TableName').Value := TableName;
    Parameters.ParamByName('Key').Value := GenerateNewID;
    try
      ExecSQL;
    except
      on E:Exception do
        raise EIDGenError.Create(Format(ResStr_CannotInsertNewID,[TableName]) + E.Message)
    end;
  end;
end;  // InsertNewID

//==============================================================================
//Update the ID in the LAST_KEY table
procedure TIDGenerator.UpdateID(const ID: string);
begin
  with dmGeneralData.qryIDGenSelect do
  begin
    //Check to see that we still have an editable dataset
    if State <> dsEdit then
      raise EIDGenError.Create(ResStr_CannotUpdateID);

    //Update ID
    try
      FieldByName('LAST_KEY_TEXT').AsString:= ID;
      Post;
    except
      on E:Exception do
      raise EIDGenError.Create(ResStr_UpdateExactError + E.Message);
    end;
    Close;
  end;
end;  // UpdateID

//==============================================================================
//Check that the ID is valid
function TIDGenerator.ValidateID(const ID: string; const IDType: TIDType): Boolean;
var
  IDLength    : Integer;
  currentChar : Integer;
begin
  Result:= True;

  //Check length
  case IDType of
    idInstallation: IDLength:= INSTALLATION_ID_LENGTH;
    idRolling: IDLength:= ROLLING_ID_LENGTH;
    idUndefined: IDLength:= 0;
  else
    raise EIDGenError.Create(ResStr_InvalidIDType);
  end;
  if (IDType <> idUndefined) and (Length(ID) <> IDLength) then
    Result:= False;

  //Check each character in ID
  for currentChar:= 1 to Length(ID) do
    case ID[currentChar] of
      '0'..'9':; //OK
      'A'..'Z':; //OK
    else
      Result:= False;
    end;
end;  // ValidateID

//==============================================================================
//Generate a new ID
function TIDGenerator.GenerateNewID: string;
var
  NewID        : string;
  iCurrentChar : Integer;
begin
  //Build new ID
  NewID:= '';
  for iCurrentChar:= 1 to ROLLING_ID_LENGTH do
    NewID:= NewID + '0';

  Result:= NewID;
end;  // GenerateNewID

//==============================================================================
//Get the next ID from the LAST_KEY table
function TIDGenerator.GetNextID(const ID: string): string;
begin
  //Check current ID is valid
  if not ValidateID(ID, idUndefined) then
    raise EIDGenError.Create(ResStr_InvalidIdForGetNextID + ID);

  //Increment current ID and return as result
  Result:= IncrementID(ID, idUndefined);
end;  // GetNextID

//==============================================================================
//Get the current key from the LAST_KEY table
function TIDGenerator.GetCurrentKey(const TableName: string): TKeyString;
var
  CurrentID : string;
begin
  //Get the current key
  CurrentID:= GetCurrentID(TableName, True);

  //Unlock the record
  UpdateID(CurrentID);

  Result:= FInstallationID + CurrentID;
end;  // GetCurrentKey

//==============================================================================
//Get the next key from the LAST_KEY table
function TIDGenerator.GetNextKey(const TableName: string): TKeyString;
var
  CurrentID, NextID : string;
begin
  Result:='';
  try
    //Get the current key
    CurrentID:= GetCurrentID(TableName, True);

    //Increment current key
    NextID:= IncrementID(CurrentID, idRolling);

    //Update the table with the new ID
    UpdateID(NextID);

    //Return next ID
    Result:= FInstallationID + NextID;
  except
    on Exception do
      Result:='';
  end;
end;  // GetNextKey

//==============================================================================
//Increment a character
function TIDGenerator.IncrementChar(const IncChar: Char): Char;
begin
  case IncChar of
    '9': Result:= 'A';
    'Z': Result:= '0'; //Roll around and inc previous char
  else
    Result:= Chr(Ord(IncChar) + 1);
  end;
end;  // IncrementChar

//==============================================================================
//Increment an ID
function TIDGenerator.IncrementID(const ID: string; const IDType: TIDType): string;
var
  iCurrentChar : Integer;
  chNewChar    : Char;
  newID        : string;
begin
  //Increment ID
  newID:= '';
  iCurrentChar:= Length(ID);  //Increment last character of ID
  chNewChar:= IncrementChar(ID[iCurrentChar]);
  while (chNewChar = '0') and (iCurrentChar > 0) do //If incremented character was rolled around, increment previous character
  begin
    newID:= chNewChar + newID; //Add rolled around character to the new ID
    Dec(iCurrentChar);
    chNewChar:= IncrementChar(ID[iCurrentChar]);
  end;

  //Check to see if we've run out of ID's
  if iCurrentChar <= 0 then
    raise EIDGenError.Create(ResStr_NoMoreIDGenerated);

  //Add new character to the new ID
  newID:= chNewChar + newID;

  //Add previous characters to the new ID
  newID:= Copy(ID, 0, iCurrentChar - 1) + newID;

  //Return new ID
  Result:= newID;
end;  // IncrementID

//==============================================================================
//==============================================================================
// TdmGeneralData

constructor TdmGeneralData.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FPrimaryKeys := TStringList.Create; // cache primary key names for each table
  FPrimaryKeys.Sorted := True;
  FIDGen:= TIDGenerator.Create(AppSettings.SiteID);
end;  // Create

//==============================================================================
{ Destructor - cleanup stuff }
destructor TdmGeneralData.Destroy;
begin
  FIDGen.Free;
  FIDGen := nil;
  FPrimaryKeys.Free;
  FPrimaryKeys := nil;
  FTableList.Free; // and again!
  FTableList := nil;
  inherited Destroy;
end;  // Destroy


//==============================================================================
{ This method handles the upgrade of map sheet user ids to the computer id, for
     this particular user.  This has to be handled here because the upgrade
     kit doesn't know which user uses which computer }
procedure TdmGeneralData.CheckMapTables;
begin
  dmDatabase.ExecuteSQL('UPDATE Map_Sheet SET Computer_ID = Host_Name(), User_ID = Null ' +
                        'WHERE User_ID = ''' + AppSettings.UserID + '''');
  // In case something wasn't cleaned up properly, do it now.
  dmDatabase.RunStoredProc('usp_ComputerMap_DeleteInvalid', []);
end;

//==============================================================================
function TdmGeneralData.GetTaxonName(const AKey:TKeyString):string;
begin
  Result:='';
  SetupStandardQuery(qryAllPurpose,ssTaxonName,AKey);
  with qryAllPurpose do begin
    Open;
    if not Eof then Result := FieldByName('Actual_Name').AsString;
    Close;
  end;
end;  // GetTaxonName

//==============================================================================
function TdmGeneralData.GetCommonName(const AKey:TKeyString):string;
begin
  Result:='';
  SetupStandardQuery(qryAllPurpose, ssTaxonName, AKey);
  with qryAllPurpose do begin
    Open;
    if not Eof then Result := FieldByName('Common_Name').AsString;
    Close;
  end;
end;  // GetCommonName

//==============================================================================
function TdmGeneralData.IsTaxonNameCommon(const AKey:TKeyString):boolean;
begin
  SetupStandardQuery(qryAllPurpose,ssTaxonName,AKey);
  with qryAllPurpose do begin
    Open;
    Result := FieldByName('Preferred_Name_Italic').AsBoolean;
    Close;
  end;
end;  // IsTaxonNameCommon

//==============================================================================
function TdmGeneralData.GetTaxonPreferredKey(const AKey:TKeyString):TKeyString;
begin
  Result:='';
  SetupStandardQuery(qryAllPurpose,ssTaxonName,AKey);
  with qryAllPurpose do begin
    Open;
    if not Eof then Result:=FieldByName('Preferred_Name_Key').AsString;
    Close;
  end;
end;  // GetTaxonPreferredKey

//==============================================================================
function TdmGeneralData.GetTaxonChecklistKey(const AKey:TKeyString):TKeyString;
begin
  Result:='';
  SetupStandardQuery(qryAllPurpose,ssTaxonName,AKey);
  with qryAllPurpose do begin
    Open;
    if not Eof then Result:=FieldByName('Taxon_List_Key').AsString;
    Close;
  end;
end;  // GetTaxonChecklistKey

//==============================================================================
function TdmGeneralData.GetBiotopeCodeName(const AKey:TKeyString):string;
begin
  SetupStandardQuery(qryAllPurpose,ssBiotopeName,AKey);
  with qryAllPurpose do begin
    ParseSQL := false;
    try
      Open;
      if not Eof then begin
        Result:=FieldByName('Short_Term').AsString;
        if FieldByName('Original_Code').AsString<>'' then
          Result:=FieldByName('Original_Code').AsString+', '+Result;
      end;
      Close;
    finally
      ParseSQL := true;
    end;
  end;
end;  // GetBiotopeCodeName

//==============================================================================
function TdmGeneralData.GetAdminAreaName(const AKey:TKeyString):string;
begin
  SetupStandardQuery(qryAllPurpose,ssAdminAreaName,AKey);
  with qryAllPurpose do begin
    ParseSQL := false;
    try
      Open;
      if not Eof then begin
        Result:=FieldByName('Item_Name').AsString;
        if FieldByName('Short_Code').AsString<>'' then
          Result:=FieldByName('Short_Code').AsString+', '+Result;
      end;
      Close;
    finally
      ParseSQL := true;
    end;
  end;
end;  // GetAdminAreaName

//==============================================================================
function TdmGeneralData.GetName(const AKey: TKeyString): string;
begin
  Result := '';
  if AKey = '' then Exit;
  if IsNameKeyForOrganisation(AKey) then
    Result := GetOrganisationName(AKey)
  else
    Result := GetIndividualName(AKey);
end;  // GetName

//==============================================================================
function TdmGeneralData.GetOrganisationName(const AKey:TKeyString):string;
begin
  with qryOrganisation do begin
    Parameters.ParamByName('NameKey').Value := AKey;
    Open;
    try
      if Eof then
        Result := ''
      else begin
        Result := FieldByName('ACRONYM').AsString;
        if Result <> '' then Result := Result + ', ';
        Result := Result + FieldByName('FULL_NAME').AsString;
      end;
    finally
      Close;
    end;
  end;
end;  // GetOrganisationName

//==============================================================================
function TdmGeneralData.GetIndividualName(const AKey:TKeyString): String;
begin
  with qryIndividual do begin
    Parameters.ParamByName('NameKey').Value := AKey;
    Open;
    try
      if Eof then
        Result := ''
      else begin
        Result := FieldByName('FORENAME').AsString;
        // If no Forename, get the Initials
        if Result = '' then Result := FieldByName('INITIALS').AsString;
        // If no Forename or Initials, get the title
        if Result = '' then Result := FieldByName('TITLE').AsString;

        if Result <> '' then Result := Result + ' ';
        Result:= Result + FieldByName('SURNAME').AsString;
      end;
    finally
      Close;
    end;
  end;
end;  // GetIndividualName

{-------------------------------------------------------------------------------
  Retrieves a location name using a location name key.  The key is updated to
      reflect the Location key.
}
function TdmGeneralData.GetLocationNameFromLocNameKey(var AKey:string): string;
begin
  Result:='';
  with qryAllPurpose do  begin
    if Active then Close;
    ParseSQL := false;
    try
      SQL.Text := 'SELECT Location_Key, Item_Name FROM Location_Name ' +
      'WHERE Location_Name_Key = ''' + AKey + ''' AND Preferred=1';
      Open;
      if not Eof then begin
        Result := FieldByName('Item_Name').AsString;
        AKey:=FieldByName('Location_Key').AsString;
      end;
      Close;
    finally
      ParseSQL := true;
    end;
  end;
end;  // GetLocationName

//==============================================================================
// Pass in a Location_Key and guess what...
function TdmGeneralData.GetLocNameKeyFromLocKey(const iLocationKey: TKeyString): TKeyString;
begin
  Result:='';
  with qryAllPurpose do begin
    if Active then Close;
    ParseSQL := false;
    try
      SQL.Text := 'SELECT * FROM Location_Name WHERE Location_Key = ''' +
                  iLocationKey +''' AND Preferred = 1';
      Open;
      if not Eof then
        Result := FieldByName('Location_Name_Key').AsString;
      Close;
    finally
      ParseSQL := true;
    end;
  end;
end;

//==============================================================================
// Pass in a Location_Key and guess what...
function TdmGeneralData.GetLocKeyFromLocNameKey(const iLocationNameKey: TKeyString): TKeyString;
begin
  Result:='';
  with qryAllPurpose do begin
    if Active then Close;
    ParseSQL := false;
    try
      SQL.Text := 'SELECT Location_Key FROM Location_Name WHERE Location_Name_Key = ''' +
                  iLocationNameKey + '''';
      Open;
      if not Eof then
        Result := FieldByName('LOCATION_KEY').AsString;
      Close;
    finally
      ParseSQL := true;
    end;
  end;
end;

//==============================================================================
{ Returns a string list containing field\value name value pairs.  Note this
     relies on the primary key being called TableName + '_KEY' }
procedure TdmGeneralData.GetRecordStrings(ioRecordList: TStrings;
          const iTable: string; const iKey: TKeyString; APk1Delimiter: string='''');
var
  i         : integer;
  lKeyField : string;
begin
  if ioRecordList = nil then
    raise EGeneralDataError.Create(ResStr_RecordListNotCreated);
  { Handle special cases for key field names }
  lKeyField := dmDatabase.GetPrimaryKey(iTable, False);
  with qryRecordStrings do
  begin
    ParseSQL := false;
    SQL.Text := 'SELECT * FROM [' + iTable + '] WHERE ' + lKeyField + ' = ' +
        APk1Delimiter + iKey + APk1Delimiter;
    Open;
    try
      if Eof then
        raise ERecordMissingError.Create(ResStr_RecordMissing + iTable);
      for i := 0 to FieldCount-1 do
      begin
        { Construct name value pair }
        ioRecordList.Add( Fields[i].FieldName + '=' +
                          Fields[i].Text );
      end; // for
    finally
      Close;
      ParseSQL := true;
    end; // try
  end; // with lQuery
end;  // GetRecordStrings

//==============================================================================
{ Overloaded version of GetRecordStrings which looks for a join table }
procedure TdmGeneralData.GetRecordStrings(ioRecordList: TStrings;
  const iTable: string; const iKey1, iKey2: TKeyString;
  APk1Delimiter: string=''''; APk2Delimiter: string='''');
var
  i         : integer;
  lPrimaryKey : TPrimaryKey;
begin
  if ioRecordList = nil then
    raise EGeneralDataError.Create(ResStr_RecordListNotCreated);
  { Handle special cases for key field names }
  lPrimaryKey := dmDatabase.SplitPrimaryKey(iTable);
  if lPrimaryKey.Key2 = '' then // pass reponsibility to first overloaded version
    GetRecordStrings(ioRecordlist, iTable, iKey1, APk1Delimiter)
  else
    with qryRecordStrings do
    begin
      ParseSQL := false;
      SQL.Text := 'SELECT * FROM ' + iTable + ' ' +
                  'WHERE ' + lPrimaryKey.Key1 + ' = ' + APk1Delimiter + iKey1 + APk1Delimiter + ' ' +
                  'AND ' + lPrimaryKey.Key2 + ' = ' + APk2Delimiter + iKey2 + APk2Delimiter;
      Open;
      try
        if Eof then
          raise ERecordMissingError.Create(ResStr_RecordMissing + iTable);
        for i := 0 to FieldCount-1 do
        begin
          { Construct name value pair }
          ioRecordList.Add( Fields[i].FieldName + '=' +
                            Fields[i].Text );
        end; // for
      finally
        Close;
        ParseSQL := true;
      end; // try
    end; // with lQuery
end;  // GetRecordStrings

//==============================================================================
{ Returns true only if a field called SYSTEM_SUPPLIED_DATA is present and the
     field value is set to true }
function TdmGeneralData.IsSystemSupplied(const iTable: string;
  const iKey: TKeyString): boolean;
var
  lRecordList : TStringList;
begin
  lRecordList := TStringList.Create;
  try
    GetRecordStrings( TStrings(lRecordList), iTable, iKey );
    { If missing or false, then our result is false }
    Result := (lRecordList.Values['SYSTEM_SUPPLIED_DATA'] = 'Yes')
  finally
    lRecordList.Free;
  end;
end;  // IsSystemSupplied

//==============================================================================
{ As IssystemSupplied, but where you already have a record so we don't reload
    the record twice }
function TdmGeneralData.IsSystemSuppliedRecord( iDataset : TDataset):boolean;
begin
  Result := false;
  if iDataset.FieldList.IndexOf('SYSTEM_SUPPLIED_DATA')<>-1 then
    Result := iDataset.FieldByName('SYSTEM_SUPPLIED_DATA').AsBoolean;
end;

//==============================================================================
function TdmGeneralData.GetNextKey(const ATableName, AKeyFieldName: string ): TKeyString;
begin
  Result := VarToStr(dmDatabase.GetStoredProcOutputParam('spNextKey',
                                                         ['@TableName', ATableName], '@Key'));
end;  // GetNextKey

{-------------------------------------------------------------------------------
  Return true if the specified record exists.
}
function TdmGeneralData.CheckKeyExists(const ATableName, AFieldName,
  AKeyValue: string):boolean;
begin
  with dmDatabase.ExecuteSQL('SELECT TOP 1 1 FROM [' + ATableName + '] WHERE [' + AFieldName + '] = ''' + AKeyValue + '''',
      true) do
    Result := not (EOF and BOF);
end;  // CheckKeyExists

{-------------------------------------------------------------------------------
  Find a unique match for the biotope, display the Find dialog if required. Use
     when the biotope is in a grid, otherwise check other overloads.
}
function TdmGeneralData.CheckBiotope(const AText:string; var AKey: TKeyString;
    var ACaption:string): boolean;
begin
  ACaption := AText;
  Result := CheckOrFindDataItem(ACaption, AKey, ftBiotope, ResStr_FindBiotope);
end;  // CheckBiotope

{-------------------------------------------------------------------------------
  Validates the taxon specified in the edit box.  Uses the Find dialog if
     necessary.  Returns the key in oKey.  A result of false indicates the
     taxon was not found.
}
function TdmGeneralData.CheckTaxon(iEditControl: TEdit; var oKey: TKeyString): boolean;
var key : TKeyString;
    caption: string;
begin
  caption := iEditControl.Text;
  key  := oKey;
  Result := true;
  // Taxon name found in database, get the Taxon_List_Item_Key
  if (iEditControl.Modified) or (key='') then begin
    Result := CheckOrFindDataItem(caption, key, ftTaxon, ResStr_FindTaxon);
    oKey := key;
    iEditControl.Text := caption;
  end;
end;  // CheckTaxon

{-------------------------------------------------------------------------------
  Check for a unique individual from a linked edit
}
function TdmGeneralData.CheckIndividual(ALinkedName: TAddinLinkedEdit): Boolean;
var
  name: string;
  key: TKeyString;
begin
  if ALinkedName.Key='' then begin
    Result := CheckIndividual(ALinkedName.Text, key, name);
    ALinkedName.Text := name;
    ALinkedName.Key  := key;
  end
  else
    Result := true;
end;

//==============================================================================
// Get a RichEdit control and return the lines as plain text and concatenated
// into a single string
function TdmGeneralData.ConvertRichTextToText(ARichEdit: TRichEdit): String;
var
  i: Integer;
begin
  Result := '';
  with ARichEdit do
    if Pos(#13,Text) = 0 then
      Result := Text
    else
      for i := 0 to Lines.Count - 1 do
        Result := Trim(Result) + ' ' + Lines[i];
  Result := Trim(Result);
end;  // ConvertRichTextToText

//==============================================================================
function TdmGeneralData.ConvertRichTextToText(ADBRichEdit: TDBRichEdit): String;
var
  i: Integer;
begin
  Result:='';
  with ADBRichEdit do
    if Pos(#13,Text) = 0 then
      Result := Text
    else
      for i := 0 to Lines.Count - 1 do
        Result := Trim(Result) + ' ' + Lines[i];
  Result := Trim(Result);
end;  // ConvertRichTextToText

//==============================================================================
function TdmGeneralData.ConvertRichTextToText(const rtfText: String): String;
var
  converter: TRichEdit;
  stream: TStringStream;
  i: Integer;
begin
  Result := '';
  converter := TRichEdit.Create(nil);
  try
    converter.Visible := False;
    converter.Parent  := frmMain;
    stream  := TStringStream.create(rtfText);
    with converter do
      try
        PlainText := False;
        Lines.LoadFromStream(stream);
        for i := 0 to Lines.Count - 1 do
          Result := Result + ' ' + Lines[i];

        Modified  := True;
        PlainText := True;
        Result    := Trim(
            StringReplace(
                StringReplace(Lines.Text, #13, ' ', [rfReplaceall]),
                #10, '', [rfReplaceAll]));
      finally
        stream.Free;
      end;
  finally
    converter.Free;
  end;
end;  // ConvertRichTextToText

//==============================================================================
// Get a Field control containing some Rich Text, and return the plain text
// into a single string
function TdmGeneralData.ConvertRtfFieldToText(AField: TField): String;
var
  lreConverter: TRichEdit;
begin
  lreConverter := TRichEdit.Create(nil);
  try
    lreConverter.Visible := False;
    lreConverter.Parent  := frmMain;
    Result := '';
    lreConverter.Clear;
    lreConverter.Lines.Assign(AField);
    Result := lreConverter.Text;
  finally
    lreConverter.Free;
  end;
end;  // ConvertRtfFieldToText

//==============================================================================
function TdmGeneralData.GetReferenceText(const AKey: TKeyString): string;
begin
  Result := '';
  if AKey <> '' then
    with qryReference do begin
      if Active then Close;
      Parameters.ParamByName('Key').Value := AKey;
      Open;
      try
        if not Eof then begin
          // Get the Date and reference title
          Result := FieldByName('Author').AsString + ' - ' +
                    FieldByName('Year_Vague_Date_Start').Text + ', ' +
                    ConvertRtfFieldToText(FieldByName('Title'));
        end;
      finally
        Close;
      end;
    end;
end;  // GetReferenceText

//==============================================================================
function TdmGeneralData.GetSourceFileText(const AKey: TKeyString): string;
begin
  with qryAllPurpose do begin
    SQL.Text := 'Select FILE_NAME from SOURCE_FILE where SOURCE_KEY=''' + AKey + '''';
    ParseSQL := false;
    Open;
    try
      if RecordCount=0 then
        raise EGeneralDataError.Create(ResStr_SourceRecNotFound);
      Result := FieldByName('FILE_NAME').AsString;
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;

{ Returns the text for a source, either the reference or the filename }
function TdmGeneralData.GetAnySourceText(const AKey:TKeyString): string;
begin
  if IsSourceInternal(AKey) then
    Result := GetReferenceText(AKey)
  else
    Result := GetSourceFileText(AKey);
end;

//==============================================================================
function TdmGeneralData.CheckReference(const ARef:string):TKeyString;
begin
  Result:='';
  if ARef<>'' then
    with qryAllPurpose do begin
      SQL.Text := 'SELECT Source_Key FROM Reference';
      ParseSQL := false;
      Open;
      while not Eof do begin
        if GetReferenceText(FieldByName('Source_Key').AsString)=ARef then begin
          Result:=FieldByName('Source_Key').AsString;
          Break;
        end;
        Next;
      end;
      Close;
      ParseSQL := true;
    end;
end;  // CheckReference

//==============================================================================
{ Accessor method for the tablelist property - creates a list of all tables in
     the DB as required }
{#TODO2 Move GetTableList into dmDatabase and just use property to access list}
function TdmGeneralData.GetTableList: TStringList;
begin
  if FTableList = nil then
  begin
    FTableList := TStringList.Create;
    dmDatabase.GetTableList(FTableList);
    FTableList.Sorted := True;     // improves search performance
  end;
  Result := FTableList;
end;  // GetTableList

//==============================================================================
function TdmGeneralData.CheckZeroAbundance(const ATaxOccKey: TKeyString): boolean;
begin
  Result := True;
  with dmDatabase.GetRecordset('usp_TaxonOccurrenceData_Select_DataForMeasurement',
      ['@Key', ATaxOccKey, '@Unit', ResStr_MUnitCount, '@Type', ResStr_MTypeAbundance]) do
  begin
    if Eof then Result := False;  // No measurements entered
    // Try and locate at least one record with non-zero count.
    while not Eof do begin
      if Fields['Data'].Value <> '0' then begin
        Result := False;
        Break;
      end;
      MoveNext;
    end;
    Close;
  end;
end; // TdmGeneralData.CheckZeroAbundance

{-------------------------------------------------------------------------------
  Description : Checks the sample and updates the ZERO_ABUNDANCE field for all
              taxon occurrences.  If there is a zero count specified, and
              no non-zero count then zero abundance is set to 1.
              This is a good candidate for a stored proc. }
procedure TdmGeneralData.CheckZOForSample(const ASampleKey: TKeyString);
begin
  dmDatabase.RunUpdateStoredProc('usp_TaxonOccurrence_Update_ZeroAbundanceForSample',
      ['@SampleKey', ASampleKey, '@Unit', ResStr_MUnitCount, '@Type', ResStr_MTypeAbundance]);
end;  //CheckZOForSample

//==============================================================================
//Loops through the relevent tables in the database, finds the value of the
//last key and checks that it's in the Last_Key table correctly.
procedure TdmGeneralData.CheckLastKeyTable;
var
  i: integer;
  lstrSQL, lstrKey, lstrKeyValue: String;
  lslTables                     : TStringList;
  lqryKey                       : TJnccQuery;
begin
  lslTables := TStringList.Create;
  lqryKey   := TJnccQuery.Create(nil);
  lqryKey.ParseSQL := false;
  dmDatabase.SetDatabaseLocal([lqryKey]);
  try
    dmDatabase.GetTableList(lslTables);
    for i := 0 to lslTables.Count - 1 do begin
      if (lslTables[i] <> 'DTD_FRAGMENT') and
         (lslTables[i] <> 'SPECIAL_XML_ELEMENT') and
         (lslTables[i] <> 'TAXON_VERSION_RELATION') and
         (lslTables[i] <> 'TERM_LIST') and
         (lslTables[i] <> 'LAST_KEY') and
         (lslTables[i] <> 'PREFERRED_LINKS') then begin
        try
          lstrKey := dmDatabase.GetPrimaryKey(lslTables[i], False);
        except
          on EMultiFieldKey do
            Continue; // skip this table - its a join table
        end;  // try
        lstrSQL := 'SELECT "' + lstrKey + '" FROM "' + lslTables[i] + '" ' +
                   'WHERE "' + lstrKey + '" LIKE ''' + AppSettings.SiteID + '''';
        lqryKey.SQL.Text := lstrSQL;
        try
          try
            lqryKey.Open;
            if not lqryKey.Eof then begin
              lqryKey.Last;
              lstrKeyValue := Copy(lqryKey.FieldByName(lstrKey).AsString, 9, 8);
              PostLastKey(lslTables[i], lstrKeyValue);
            end;
          finally
            lqryKey.Close;
          end;
        except
          on Exception do
            Continue;
        end;
      end;
    end;
  finally
    lqryKey.Free;
    lslTables.Free;
  end;
end;  //TdmGeneralData.CheckLastKeyTable

//==============================================================================
procedure TdmGeneralData.PostLastKey(istrTableName, istrLastKey: string);
begin
  with dmGeneralData.qryIDGenSelect do begin
    //Attempt to find the correct record in the database
    Close;
    Parameters.ParamByName('TableName').Value := istrTableName;
    try
      Open;
      if Eof then begin
        with dmGeneralData.qryIDGenInsert do begin
          Close;
          Parameters.ParamByName('TableName').Value := istrTableName;
          Parameters.ParamByName('Key').Value := istrLastKey;
          ExecSql;
        end;
      end
      else begin
        if FieldByName('Last_Key_Text').AsString <> istrLastKey then begin
          Edit;
          FieldByName('Last_Key_Text').AsString := istrLastKey;
          Post;
        end;
      end;
    except
      on E:Exception do
      raise EIDGenError.Create(Format(ResStr_NoTableID, [istrTableName]) + E.Message);
    end;
  end;
end;//TdmGeneralData.PostLastKey

//==============================================================================
//Gets table name and preferred field out of the table Preferred_Links and
//sends them to be checked.
procedure TdmGeneralData.CheckPreferred;
var lQry: TJnccQuery;
begin
  lqry := TJnccQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lqry]);
  frmMain.SetStatus(ResStr_CheckingPrefRecords);
  with lQry do
    try
      ParseSQL := false;
      SQL.Text := 'SELECT * FROM Preferred_Links';
      Open;
      First;
      while not Eof do begin
        CompletePrefCheck(FieldByName('Table_Name').AsString,
                          FieldByName('Preferred_Field').AsString);
        Next;
      end;
      Close;
    finally
      Free;
      frmMain.SetStatus('');
    end;
end; //TdmGeneralData.CheckPreferred

//==============================================================================
//Recieves the table name and preferred field of the table and checks that
//one and only one of the entries is -1 and the others are 0. If all are 0
//the first entry is set to -1 by default.
procedure TdmGeneralData.CompletePrefCheck(const istrTableName, istrPrefField: string);
var
  lstrlGroupBy: TStringList;
  i: integer;
  PrefSet: boolean;
begin
  // Mantis
  //This will hold the distinct items to group by.
  lstrlGroupBy := TStringList.Create;
  // use a query to locate any item that doesn't have exactly 1 preferred
  with qryPreferred do begin
    ParseSQL := false;
    // Find all records with either no "Preferred" set, or more than one set.
    SQL.Text := Format('SELECT %s, Preferred FROM %s ' +
                       'WHERE Preferred = 0 ' +
                       'AND %s NOT IN (SELECT %s FROM %s WHERE Preferred = 1)' +
                       ' UNION '  +
                       'SELECT %s, ' +
                       'Count(*) ' +
                       ' FROM %s  ' +
                       ' WHERE PREFERRED =1 ' +
                       ' GROUP BY %s ' +
                       ' HAVING count(*) > 1 ',
                       [istrPrefField, istrTableName, istrPrefField,
                        istrPrefField, istrTableName,istrPrefField, istrTableName, istrPrefField ]);
    Open;
    First;
    // store the items that will need work on
    while (not Eof) do begin
      lstrlGroupBy.Add(FieldByName(istrPrefField).AsString);
      Next;
    end;
    Close;
    for i := 0 to (lstrlGroupBy.Count - 1) do begin
      // for the item, bring back all the records ordered by last edit date
      SQL.Text := Format('SELECT * FROM %s WHERE %s = ''%s'' ' +
                         'ORDER BY ISNULL(Changed_Date, Entry_Date) DESC',
                         [istrTableName, istrPrefField, lstrlGroupBy[i]]);
      Open;
      First;
      //when a -1 has been encounted, PrefSet is set to True and all the other
      //entries are set to 0. If PrefSet is still false when all entries have
      //been checked, the first entry (most recent) is set to -1.
      PrefSet := false;
      while not Eof do begin
        if PrefSet then begin
          Edit;
          FieldbyName('Preferred').AsBoolean := false;
          Post;
        end else if FieldByName('Preferred').AsBoolean then
          PrefSet := true;
        Next;
      end;
      if PrefSet = false then begin
        First;
        Edit;
        FieldByName('Preferred').AsBoolean := true;
        Post;
      end;
      Close;
    end;
  end;
  lstrlGroupBy.Free;
end;  // TdmGeneralData.CompletePrefCheck

//==============================================================================
//Checks that the second coordinate is within 2 kilometres of the first and if
//so returns true.
function TdmGeneralData.CheckWithin2KM(iCoord1,
  iCoord2: TLatLong): boolean;
var
  lOSGBCoord1, lOSGBCoord2 : TMapCoord;
begin
  Result := False;
  lOSGBCoord1 := LatLongToOSGBEastNorth(iCoord1);
  lOSGBCoord2 := LatLongToOSGBEastNorth(iCoord2);
  if (Abs(lOSGBCoord1.x - lOSGBCoord2.x) < 2000) and
     (Abs(lOSGBCoord1.y - lOSGBCoord2.y) < 2000) then Result := True;
end;  // TdmGeneralData.CheckWithin2KM

//==============================================================================
procedure TdmGeneralData.GetKeyListFromStandardQuery(iKeyList:TEditableKeyList;
  const iStandardSQL:TStandardSQL; const iKeyValue:TKeyString);
begin
  qryAllPurpose.ParseSQL := False;
  SetStandardQuery(qryAllPurpose, iStandardSQL, iKeyValue, AppSettings.UserId);
  iKeyList.AddQueryResults(qryAllPurpose);
end;  // GetKeyListFromStandardSQL

//==============================================================================
{ The SetupStandardQuery method allows some standard SQL to be re-used through
    the application.  It sets up the SQL property according to the sql query
    chosen.  Additional info is provided by the iKeyValue }
procedure TdmGeneralData.SetupStandardQuery(iQuery: TJNCCQuery;
  const iStandardSQL: TStandardSQL; const iKeyValue: TKeyString);
begin
  iQuery.ParseSQL := False;
  // Extracted all the selection to SQLConstants. This allows the standard queries
  // to used without linking to GeneralData.
  SetStandardQuery(iQuery, iStandardSQL, iKeyValue, AppSettings.UserId);
end;  // SetupStandardQuery

//------------------------------------------------------------------------------
{ Procedure takes a key list, builds it into an IN clause for a query,
   then copies the ItemKey from the query back into the destination keylist.
   Handles situations where the access query is >64K, by looping and running the
   query several times.
   Input Query should contain SQL of the form :
    ' SELECT <KeyField> AS ItemKey FROM <Table> WHERE <FilterField> IN '
   and the rest is handled. }
procedure TdmGeneralData.ConvertListUsingSQL(ASourceKeyList:TKeyList;
  ADestKeyList:TEditableKeyList; const ASQL:string);
const
  MAX_IN_LIST = 3000;
var
  lCurrent, lMaxInBlock : integer;
  lIdx : integer;
  lSqlText : string;
  qry: TADOQuery;
begin
  lCurrent := 0;
  while lCurrent <= ASourceKeyList.Header.ItemCount-1 do
  begin
    lMaxInBlock := Min(lCurrent + MAX_IN_LIST, ASourceKeyList.Header.ItemCount-1);
    lSQLText := '(';
    { Build In clause - last item has no comma }
    for lIdx := lCurrent to lMaxInBlock-1 do
      lSQLText := lSQLText + '''' + ASourceKeyList.Items[lIdx].KeyField1 + ''',';
    lSQLText := lSQLText + '''' + ASourceKeyList.Items[lMaxInBlock].KeyField1 + ''')';
    lCurrent := lMaxInBlock + 1;
    { Set up the query }
    qry := TADOQuery.Create(nil);
    try
      qry.Connection := dmDatabase.dbLocal;
      qry.SQL.Text := ASQL + lSQLText;
      ADestKeyList.AddQueryResults(qry);
    finally
      qry.Free;
    end;
  end; // while
end;  // ConvertListUsingSQL

//==============================================================================
function TdmGeneralData.SetListVersionKeys(const iBiotope: boolean;
  const iListKey: TKeyString): TListVersion;
var
  lListVersionKeys : string;
  lLastVersionKey : TKeyString;
  lIsAmendment : boolean;
begin
  lListVersionKeys := '';
  lLastVersionKey := '';
  with qryLatestListVersion do
  begin
    try
      SQL.Clear;
      if iBiotope then SQL.Add(BIOTOPE_VERSION_QUERY)
                  else SQL.Add(TAXON_VERSION_QUERY);
      Parameters.ParamByName('ListKey').Value := iListKey;
      Open;
      First;
      if not Eof then begin
        lLastVersionKey := FieldByName('ListVersionKey').AsString;
        lListVersionKeys  := '''' + lLastVersionKey + '''';
        lIsAmendment := FieldByName('Version_Is_Amendment').AsBoolean;
        while lIsAmendment do begin
          Next;
          lListVersionKeys := lListVersionKeys + ', ''' + FieldByName('ListVersionKey').AsString + '''';
          lIsAmendment := FieldByName('Version_Is_Amendment').AsBoolean;
        end;
      end else
        raise ENoListVersionFound.Create(ResStr_NoVersion);
    finally
      Close;
    end;
  end;
  Result.LastVersion := lLastVersionKey;
  Result.LatestVersions := lListVersionKeys;
end;  // SetListVersionKeys

//==============================================================================
procedure TdmGeneralData.EnterListVersionKeys(ioSql: TStrings;
          const iListVersionKeys : string);
var
  lCount, lOpenBracket, lCloseBracket : integer;
  lCurrLine, lNewText, lEndString : string;
  lInsertedKeys : boolean;
begin
//Note - it is important that the text 'WHERE' in the SQL is on the same line as
//the brackets for the in clause.
  lInsertedKeys := False;
  for lCount := 0 to ioSql.Count - 1 do begin
    lCurrLine := ioSql[lCount];
    if Pos('WHERE', Uppercase(lCurrLine)) > 0 then begin
      lOpenBracket := Pos('(', lCurrLine);
      if lOpenBracket > 0 then begin
        lNewText := Copy(lCurrLine, 1, lOpenBracket) + iListVersionKeys;
        lEndString := Copy(lCurrLine, lOpenBracket + 1, Length(lCurrLine) - lOpenBracket);
        if Copy(lCurrLine, lOpenBracket + 1, 1) = ')' then
          lNewText := lNewText + lEndString
        else begin
          lCloseBracket := Pos(')', lEndString);
          if lCloseBracket <> 0 then
            lNewText := lNewText + Copy(lEndString, lCloseBracket, length(lEndString) - lCloseBracket + 1)
          else
            lNewText := lNewText + ')';
        end;
        ioSql[lCount] := lNewText;
        lInsertedKeys := True;
      end;
      Break;
    end;
  end;

  if not lInsertedKeys then
    Raise EListVersionKeys.Create(ResStr_CannotInsertVersionKey);
end;  // EnterListVersionKeys

//==============================================================================
{ Ask the user if they want to expand a taxon list to include sub taxa.
    Hold Ctrl to bypass this and include the subtaxa
    Hold Shift to bypass this and exclude subtaxa.
    Abort is raised if the user cancels }
function TdmGeneralData.IncludeSubTaxa : boolean;
var
  lDlgResult : Word;
begin
  // Control key down - include subtaxa
  if GetKeyState(VK_CONTROL) < 0  then
    Result := True
  else if GetKeyState(VK_SHIFT) < 0  then
    // shift key down - don't include
    Result := False
  else begin
    // else need to ask
    lDlgResult :=  MessageDlg(ResStr_IncludeSubTaxa, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if lDlgResult = mrCancel then
      Abort;
    Result := lDlgResult = mrYes;
  end;
end;

//==============================================================================
{ Prepares a list of strings for all numerical measurement units.  Adds an
    object containing the item's key to each string }
procedure TdmGeneralData.BuildMeasurementUnitList(ioList : TStrings);
var
  lKey : TKey;
begin
  with dmDatabase.ExecuteSQL(
    'SELECT MU.Measurement_Unit_Key, (MU.Short_Name + '' ('' + MT.Short_Name + '')'') AS DisplayName ' +
    'FROM Measurement_Unit AS MU ' +
    'INNER JOIN Measurement_Type AS MT ON MU.Measurement_Type_Key = MT.Measurement_Type_Key ' +
    'WHERE MU.Data_Type = ''N'' ORDER BY (MU.Short_Name + ''('' + MT.Short_Name + '')'') ASC',
    True) do
  begin
    while not Eof do begin
      lKey := TKey.Create;
      lKey.Key := Fields['Measurement_Unit_Key'].Value;
      ioList.AddObject(Fields['DisplayName'].Value, lKey);
      MoveNext;
    end;
    Close;
  //  ParseSQL := true;
  end;
end;  // BuildMeasurementUnitList

{-------------------------------------------------------------------------------
  Find a unique match for the name, display the Find dialog if required. Use
     when the name is in a grid, otherwise check other overloads.
}
function TdmGeneralData.CheckName(const AText:string; var AKey: TKeyString; var
    ACaption:string): boolean;
begin
  ACaption := AText;
  Result := CheckOrFindDataItem(ACaption, AKey, ftName, ResStr_FindName);
end;  // CheckBiotope

{-------------------------------------------------------------------------------
  checks to ensure that if there are any temp recorders then temp must be set to true
}
function TdmGeneralData.CheckTempSurvey(const ASurveyKey:string) : boolean;
var
lCountTempRecorders : integer;
begin
  lCountTempRecorders := 0;
  with qryTempRecorders do begin
     Parameters.ParamByName('SurveyKey').Value:= ASurveyKey;
    Open;
    if not Eof then
      lCountTempRecorders := FieldByName('RECORDS').AsInteger;
    close;
  end;

  If lCountTempRecorders = 0 then
    Result := true
  else
    Result := false


end;  // CheckTempSurvey

{-------------------------------------------------------------------------------
  If it is a temp survey then licence must be lKeyRequired
}
function TdmGeneralData.CheckLicence(const ALicenceKey:string) : boolean;
var
lKeyRequired : string;
begin
  with qryTempLoc do begin
    Open;
    try
      if Eof then
        lKeyRequired := 'NBNSYS0000000007'
      else begin
        lKeyRequired := FieldByName('DATA').AsString;
      end;
    finally
      Close;
    end;
  end;
  if lKeyRequired = ALicenceKey then
     Result := true
  else
     Result := false;

end;  // CheckLicence


{-------------------------------------------------------------------------------
  Find a unique match for the individual, display the Find dialog if required. Use
     when the individual is in a grid, otherwise check other overloads.
}
function TdmGeneralData.CheckIndividual(const AText:string; var AKey:
    TKeyString; var ACaption:string): boolean;
begin
  ACaption := AText;
  Result := CheckOrFindDataItem(ACaption, AKey, ftIndividual, ResStr_FindIndividual);
end;  // CheckBiotope

//==============================================================================
function TdmGeneralData.SetSpatialRef(const ASpatialRef, ALocKey,
    ADisplaySystem: string): TSpatialRefValues;
var
  lSystem:string;
  lRef, lOrigRef : TValidSpatialRef;
begin
  // Note if iLocKey is not blank then use this to determine result values.
  // Otherwise use the passed spatial reference.
  if ALocKey <> '' then begin
    with dmDatabase.GetRecordset('usp_Location_Select', ['@Key', ALocKey]) do
      if not (Eof or Bof) then begin
        Result.EnteredSystem := Fields['SPATIAL_REF_SYSTEM'].Value;
        Result.DisplaySystem := ADisplaySystem;
        // To format the display reference
        lRef := ValidSpatialRef(
            VarToStr(Fields['SPATIAL_REF'].Value),
            ADisplaySystem,
            Result.EnteredSystem);
        // to format the entered reference
        lOrigRef := ValidSpecificSpatialRef(
            VarToStr(Fields['SPATIAL_REF'].Value),
            Result.EnteredSystem);
        Result.EnteredRef := lOrigRef.FormattedSR;
        Result.Qualifier := Fields['SPATIAL_REF_QUALIFIER'].Value;
        Result.DisplayRef := lRef.FormattedSR;
      end;
  end else if ASpatialRef <> '' then begin
    lSystem              := DetermineSpatialRefSystem(ASpatialRef);
    Result.EnteredRef    := ASpatialRef;
    Result.EnteredSystem := lSystem;
    Result.DisplaySystem := ADisplaySystem;
    Result.Qualifier     := ResStr_InternalMap;
    lRef := ValidSpatialRef(ASpatialRef, ADisplaySystem);
    if lRef.Valid then begin
      Result.DisplayRef := lRef.FormattedSR;
    end else
      Raise EGeneralDataError.Create(lRef.Error);
  end else
    Raise EGeneralDataError.Create(ResStr_SetSpatialRef);
end;  // SetSpatialRef

//==============================================================================
procedure TdmGeneralData.ExecuteSQL(const iSQL, ErrMsg: string; ParseSQL: Boolean = True);
var lParse: Boolean;
begin
  try
    qryAllPurpose.SQL.Text := iSQL;
    // Save ParseSQL state.
    lParse := qryAllPurpose.ParseSQL;
    // Update with provided flag.
    qryAllPurpose.ParseSQL := ParseSQL;
    qryAllPurpose.ExecuteSQL;
    // Restore ParseSQL state.
    qryAllPurpose.ParseSQL := lParse;
  except
    on E:Exception do
      if dmDatabase.CheckError(E, dbeReferentialIntegrity) then
      begin
        //Attempt to find out why this command is causing a referential integrity error
        if Pos('DELETE', UpperCase(iSQL)) > 0 then
          Raise TExceptionPath.CreateNonCritical(ResStr_CannotDeleteRecord, E)
        else if Pos('UPDATE', UpperCase(iSQL)) > 0 then
          Raise TExceptionPath.CreateNonCritical(ResStr_CannotUpdateItem)
        else
          Raise TExceptionPath.CreateNonCritical(ResStr_CannotInsertItem);
      end
    else
      raise EGeneralDataError.Create(ErrMsg,E);
  end;
end;  // ExecuteSQL

//==============================================================================
// By adding ' around the key values, we can use the CommaText property
// of the StringList to have a formatted list of keys, ready to be used
// in a IN () clause > IN ('xxx','xxxx','xxxx').
function TdmGeneralData.GetKeysToString(const iSQL: string): string;
var lKeys:TStringList;
begin
  lKeys:=TStringList.Create;
  try
    with qryAllPurpose do begin
      SQL.Text:=iSQL;
      Open;
      while not Eof do begin
        lKeys.Add(''''+Fields[0].AsString+'''');
        Next;
      end;
      Close;
    end;
    Result:=lKeys.CommaText;
  finally
    lKeys.Free;
  end;
end;  // GetKeysToString

//==============================================================================
procedure TdmGeneralData.DelSources(const ATableName, AKeyField: string;
  const AKeyValue: TKeyString);
var lKeys:string;
begin
  lKeys:=GetKeysToString('SELECT S.Source_Key FROM ' + ATableName + ' T, Source S ' +
                         'WHERE ' + AKeyField + '=''' + AKeyValue + ''' ' +
                         'AND   T.Source_Key = S.Source_Key AND Internal=0');
  // Following order of deletion is important. Do not change it.
  // Remove all traces of the record in its Sources table
  ExecuteSQL('DELETE FROM ' + ATableName + ' WHERE ' + AKeyField + '=''' + AKeyValue + '''',
             ResStr_DelFail + ' - ' + UpperCase(ATableName) + ' table');

  // Remove all external sources
  if lKeys <> '' then begin
    // SQL wants strings surrounded by ' or ". Use the CommaText property to get
    // the full list of keys properly formatted -> 'xxxx','xxxx',...
    ExecuteSQL('DELETE FROM Source_File WHERE Source_Key IN (' + lKeys + ')',
               ResStr_DelFail + ' - SOURCE_FILE table');
    // And finally, as we only retrieved the external sources, no problem with the following SQL
    ExecuteSQL('DELETE FROM Source WHERE Source_Key IN (' + lKeys + ')',
               ResStr_DelFail + ' - SOURCE table');
  end;
end;  // DelSources

{==============================================================================
    Procedure Name: GetPlaceCardCOMClassID
            Called: AppSettings
           Written: IR - 1/12/99
           Purpose: Prior to displaying a place card, looks in the .crd file to
                    determine whether the card has a COM header and then points
                    to it, if it is there.
      Side Effects:
------------------------------------------------------------------------------}
function TdmGeneralData.GetPlaceCardCOMClassID(const AFilename:string; var ioAddinName:string;
  var ioClassID:string ):boolean;
var
  lCardFile : TextFile;
  lCurrent : string;
  lName : string;
  lGuid : string;
begin
  //Open file
  Result := false;
  AssignFile(lCardFile, AFileName);
  try
    Reset(lCardFile);
    //Read general
    ReadLn(lCardFile, lCurrent);
    if lCurrent <> '<General>' then
      raise EGeneralDataError.CreateNonCritical(Format(ResStr_BadRecCard, [ExtractWithoutExt(AFileName)]));

    While not SeekEof( lCardFile ) do
    begin
      if Comparetext( lCurrent, '<HeaderClsID>' ) <> 0 then
        ReadLn(lCardFile, lCurrent)
      else
        Break;
    end; // while not
    If not SeekEof( lCardFile ) then { no com stuff found... treat as normal }
    begin
      ReadLn( lCardFile, lName );
      If CompareText (lName, '</HeaderClsID>') <> 0 then
      begin
        Readln( lCardFile, lGuid );
        If COMClassInstalled( lGuid ) then
        begin
          Result := True;
          ioClassID := lGuid;
          ioAddinName := lName;
        end;
      end // if something other than the end of the declation found...
      else
        Result := false;
    end // if not eof
    else
      Result := False;
  finally
    //Close file
    CloseFile(lCardFile);
  end;
end;  // GetPlaceCardCOMClassID

//==============================================================================
function TdmGeneralData.COMClassInstalled( iStrClassID: String ): boolean;
var
  lCount : integer;
  lInstalledClsID : String;
begin
  Result := False;
  for lCount := 0 to AppSettings.ComAddins.AddinCount -1 do
  begin
    lInstalledClsID := GuidToString(AppSettings.ComAddins.AddinList[lCount].ClsID);
    if lInstalledClsID = iStrClassID then
    begin
      Result := true;
      Exit;
    end;
  end;
end;  // COMClassInstalled

//==============================================================================
procedure TdmGeneralData.SourcesAddInternal(Sender: TObject);
var lFind : TdlgFind;
begin
  inherited;
  lFind := TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference);
  with lFind do
    try
      SetSearchText('',true);
      if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then
          TSources(Sender).AddInternalRef(ItemKey);  // Get the associated key
      end else
        MessageDlg(ResStr_NoReferenceItems, mtInformation, [mbOK], 0);
    finally
      Release;
    end;
end;  // SourcesAddInternal

//==============================================================================
procedure TdmGeneralData.SourcesFindInternal(Sender: TObject);
var lCaller:TBaseForm;
begin
  inherited;
  // Calling form is the owner of the TSources component
  lCaller:=TBaseForm(TSources(Sender).Owner);
  // Activate the Document screen
  dmFormActions.actDocuments.Execute;
  // Setup the link for return data
  lCaller.SetupLink(TBaseForm(frmMain.ActiveMDIChild),lCaller,TSources(Sender).InternalRefUpdate);
end;  // SourcesFindInternal

procedure TdmGeneralData.SourcesShowInternal(Sender: TObject; SourceKey: String);
var lfrmReferences:TfrmReferences;
    lKeyList: TEditableKeyList;
begin
  inherited;
  lfrmReferences:=TfrmReferences(dmFormActions.DisplayForm(TfrmReferences));
  lKeyList := TEditableKeyList.Create();
  lKeyList.SetTable(TN_REFERENCE);
  lKeyList.AddItem(SourceKey,'');
  lfrmReferences.DisplaySources(lKeyList);
end;


//==============================================================================
procedure TdmGeneralData.SetNameIDAndDate(ADataSet: TDataSet;
  const ANameIDField, ADateField: string);
var lField:TField;
begin
  lField:=ADataSet.FindField(ANameIDField);
  if Assigned(lField) then lField.AsString:=AppSettings.UserID
                      else MessageDlg('Name field '''+ANameIDField+''' not found.',mtWarning,[mbOk],0);
  lField:=ADataSet.FindField(ADateField);
  if Assigned(lField) then lField.AsDateTime:=Now
                      else MessageDlg('Date field '''+ADateField+''' not found.',mtWarning,[mbOk],0);
end;  // SetNameIDAndDate

//==============================================================================
function TdmGeneralData.IsSourceInternal(const iSourceKey: TKeyString): Boolean;
begin
  Result := false;
  with qryAllPurpose do begin
    SQL.Text := 'Select INTERNAL from SOURCE where SOURCE_KEY=''' + iSourceKey + '''';
    ParseSQL := false;
    Open;
    try
      if RecordCount=0 then
        raise EGeneralDataError.Create(ResStr_SourceNotFoundChk)
      else
        Result := FieldByName('INTERNAL').AsBoolean;
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;  // IsSourceInternal

//==============================================================================
{ Checks whether a taxon is installed in the local database rather than on CD }
function TdmGeneralData.CheckTaxonInstalled(var ATaxonListItemKey: TKeyString;
  ATaxonName: string): integer;
var lCursor:TCursor;
begin
  lCursor:=HourglassCursor;
  Result := 0;
  try
    if AppSettings.DisplayTaxonCommonNames then begin
      with qryAllPurpose do begin
        Close;
        ParseSQL := false;
        SQL.Text := 'SELECT TUN.Taxon_List_Item_Key '+
                    'FROM TAXON_LIST_ITEM AS TLI, TAXON_USER_NAME AS TUN, '+
                    '     TAXON_LIST_VERSION AS TLV, TAXON_LIST AS TL '+
                    'WHERE TUN.Item_Name = ''' + ATaxonName + ''' '+
                    '  AND TUN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key '+
                    '  AND TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key '+
                    '  AND TLV.Taxon_List_Key = TL.Taxon_List_Key '+
                    '  AND TL.Local_Disk = 1 ';
        try
          Open;
          First;
          if not EOF then begin
            ATaxonListItemKey := FieldByName('Taxon_List_Item_Key').AsString;
            Result := 1;
            Exit; // We've found an installed taxon_list_item for which there is a common name
          end; // if not EOF
        finally
          Close;
          ParseSQL := true;
        end;
      end; // with qryAllPurpose
    end; // if AppSettings.DisplayTaxonCommonNames

    with qryAllPurpose do begin
      Close;
      ParseSQL := false;
      SQL.Text := 'SELECT TLI.TAXON_LIST_ITEM_KEY ' +
                  'FROM TAXON As T, TAXON_VERSION As TV, TAXON_LIST_ITEM As TLI, ' +
                  '     Taxon_List_Version As TLV, Taxon_List As TL '+
                  'WHERE T.ITEM_NAME = ''' + ATaxonName + ''' ' +
                  '  AND T.TAXON_KEY = TV.TAXON_KEY ' +
                  '  AND TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY '+
                  '  AND TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key '+
                  '  AND TLV.Taxon_List_Key = TL.Taxon_List_Key '+
                  '  AND TL.Local_Disk = 1 ';
      try
        Open;
        First;
        if not EOF then begin
          Result := 1;
          ATaxonListItemKey := FieldByName('Taxon_List_Item_Key').AsString;
        end;
      finally
        Close;
        ParseSQL := true;
      end;
    end; // with qryAllPurpose
  finally
    DefaultCursor(lCursor);
  end; // try..finally
end; // CheckTaxonInstalled

//==============================================================================
{ Returns a name for a taxon, specified by Taxon_List_Item_Key according to the
  following preference hierarchy:

    User designated common name > Official (NBN) common name > Standard taxon name

  NOTE: Common names are returned only when the flag in application settings
        (DisplayCommonTaxonNames) is set to True. Otherwise the default name
        only is specified.                                                             }
function TdmGeneralData.GetTaxonCommonName(const ATaxonListItemKey: TKeyString; var ioLang: string): string;
begin
  // Set pessimistic default return values
  Result := '';
  ioLang := '';
  with qryTaxonName do begin
    if Active then Close;
    SetupStandardQuery(qryTaxonName, ssPreferredCommonName, ATaxonListItemKey);
    try
      Open;
      First;
      if not Eof then begin
        { A common name of some sort has been found }
        Result := FieldByName('CommonName').AsString;
        ioLang := FieldByName('CommonLanguage').AsString;
      end; // if not EOF
    finally
      Close;
    end; // try..finally
  end; // with qryAllPurpose
end;  // GetTaxonCommonName

//==============================================================================
{ Analogous to GetTaxonCommonName, this returns the scientific name for the
  taxon, together with the language for the name as a var parameter }
function TdmGeneralData.GetTaxonScientificName(
  const ATaxonListItemKey: TKeyString; var ioLang: string): string;
begin
  with qryTaxonName do begin
    if Active then Close;
    SetupStandardQuery(qryTaxonName, ssTaxonName, ATaxonListItemKey);
    try
      Open;
      First;
      if not EOF then begin
        Result := FieldByName('Preferred_Name').AsString;
        if FieldByName('Preferred_Name_Italic').AsBoolean then ioLang := 'La';
      end else begin
        Result := '';
        ioLang := '';
      end;
    finally
      Close;
    end; // try..finally
  end; // with qryTaxonName;
end;  // GetTaxonScientificName

//==============================================================================
{ Returns an object holding a proper taxon name and common taxon name, together
  with flags denoting languages for each }
function TdmGeneralData.GetTaxonNamesObject( const ATaxonListItemKey: TKeyString ): TTaxonNames;
begin
  with qryTaxonName do begin
    if Active then Close;
    SetupStandardQuery(qryTaxonName, ssTaxonName, ATaxonListItemKey);
    try
      Open;
      First;
      if not EOF then begin
        Result := TTaxonNames.Create;
        with Result do
          try
            TaxonListItemKey := ATaxonListItemKey;
            TaxonName  := FieldByName('Preferred_Name').AsString;
            CommonName := FieldByName('Common_Name').AsString;
            EnteredName := FieldByName('Actual_Name').AsString;
            TNItalic := FieldByName('Preferred_Name_Italic').AsBoolean;
            CNItalic := FieldByName('Common_Name_Italic').AsBoolean;
            ENItalic := FieldByName('Actual_Name_Italic').AsBoolean;
            TNAttribute := FieldByName('Preferred_Name_Attribute').AsString;
            CNAttribute := FieldByName('Common_Name_Attribute').AsString;            
            ENAttribute := FieldByName('Actual_Name_Attribute').AsString;
            TNAuthor    := FieldByName('Preferred_Name_Authority').AsString;
            ENAuthor    := FieldByName('Authority').AsString;
        except
          on E:Exception do begin
            Result.Free;
            Result := nil;
          end; // on E:Exception
        end; // try..except
      end else
        Result := nil;
    finally
      Close;
    end; // try..finally
  end;
end; // GetTaxonNamesObject

//==============================================================================
function TdmGeneralData.GetBestTaxonName( const ATaxonListItemKey: TKeyString ): string;
var lTemp, lDummy: string;
begin
  lTemp := GetTaxonCommonName( ATaxonListItemKey, lDummy );
  if (lTemp <> '') then Result := lTemp
                   else Result := GetTaxonName( ATaxonListItemKey );
end;  // GetBestTaxonName

//==============================================================================
function TdmGeneralData.GetCommonNameForTaxonOccurrence(
  const iTaxonOccurrenceKey: TKeyString): string;
var lDummy : string;
    lTLIKey: TKeyString;
begin
  lTLIKey := '';
  with qryTaxonName do begin
    if Active then Close;
    SetupStandardQuery( qryTaxonName, ssTaxonListItemForOccurrence, iTaxonOccurrenceKey );
    try
      Open;
      First;
      if not EOF then
        lTLIKey := FieldByName('TAXON_LIST_ITEM_KEY').AsString;
    finally
      Close;
    end; // try
  end; // with qryTaxonName

  { Find the common name based on the taxon list item key }
  if lTLIKey <> '' then Result := GetTaxonCommonName(lTLIKey, lDummy)
                   else Result := '';
end;  // GetCommonNameForTaxonOccurrence

//==============================================================================
{ Populate a Qualifiers combo box with all those for the measurement
     type supplied.
     Called by the constructor only. }
procedure TdmGeneralData.PopulateQualifierCombo( iComboBox : TComboBox; const iTypeKey : String );
var
  lKeyObject : TKey;
begin
  iComboBox.Items.Clear;
  with qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := 'SELECT MEASUREMENT_QUALIFIER_KEY, SHORT_NAME FROM MEASUREMENT_QUALIFIER ' +
                'WHERE MEASUREMENT_TYPE_KEY=''' + iTypeKey + ''' ' +  // preset key for abundance
                'ORDER BY SHORT_NAME';
    try
      Open;
      while not EOF do begin
        lKeyObject := TKey.Create;
        lKeyObject.Key := FieldByName('MEASUREMENT_QUALIFIER_KEY').AsString;
        iComboBox.Items.AddObject(FieldByName('SHORT_NAME').AsString, lKeyObject);
        Next;
      end;
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;  // PopulateQualifierCombo

//==============================================================================
{ Reads a lat or long from a field.  Reads NULL_LATLONG if the value is null }
procedure TdmGeneralData.ReadLatLong(const iField: TField; var iRef: double);
begin
  if not iField.IsNull then
    iRef := iField.AsFloat
  else
    iRef := NULL_LATLONG;
end;

{-------------------------------------------------------------------------------
}
procedure TdmGeneralData.ReadLatLong(const AValue: Variant; var ARef: Double);
begin
  if VarIsNull(AValue) then ARef := NULL_LATLONG
                       else ARef := AValue;
end;

//==============================================================================
{ Add list of available rucksacks to taxon search combo box }
procedure TdmGeneralData.GetTaxonSearchOptions( icmbOptions : TComboBox ) ;
var
  lSearchRec : TSearchRec;
begin
  with icmbOptions do begin
    Items.Clear;
    Items.Add(ResStr_Preferred_Taxa);
    Items.Add(ResStr_Unrestricted);
    If not AppSettings.UsePreferredTaxa then begin
      Items.Add(ResStr_Recommended_Full);
      Items.Add(ResStr_PreferredLists);
    end;
    Items.Add(ResStr_CurrentChecklist);
    Items.Add(ResStr_ContentsOfRucksack);
    // Add in the Current rucksack
    if dmFormActions.GetFormInstance( TfrmRucksack ) <> nil then
      Items.Add( STR_RUCKSACK );
    //Find the first rucksack file
    if FindFirst(Appsettings.RucksackPath + '*.ruk', 0, lSearchRec) = 0 then
    begin
      //Add the rucksack to the menu
      Items.Add( ExtractWithoutExt(lSearchRec.Name) + ' ' + ResStr_Rucksack);
      //Add a new menu item for each remaining file
      while FindNext(lSearchRec) = 0 do
        Items.Add( ExtractWithoutExt(lSearchRec.Name) + ' ' + ResStr_Rucksack);
      //Free the search results
      FindClose(lSearchRec);
    end;
    // select the current option
    ItemIndex := Items.IndexOf(AppSettings.SessionTaxonomicSearchRestriction);
    if ItemIndex = -1 then
      AppSettings.SessionTaxonomicSearchRestriction := ResStr_Preferred_Taxa;
  end; // with icmbOptions
end;

//==============================================================================
{ Get a list of taxon list item keys for a rucksack, in a string.  E.g.
  'NBNSYS0000000001','NBNSYS0000000001'2'NBNSYS0000000003' }
function TdmGeneralData.GetRucksackTaxaAsCommaList(const iRucksackName: string): string;
var
  lContent: TStringList;
  lFileName: String;
  i: integer;
begin
  Result := '';
  lFileName := AppSettings.RucksackPath + iRucksackName + '.ruk';

  if (CompareText(iRucksackName, 'Current') = 0) or
     (CompareText(AppSettings.CurrentRucksack.FileName, lFileName) = 0) then
    with AppSettings.CurrentRucksack.TaxonList do begin
      // load current rucksack
      for i := 0 to Header.ItemCount - 1 do
        Result := Result + '''' + Items[i].KeyField1 + ''',';
      // lop off last comma
      Result := Copy(Result, 1, Length(Result) - 1);
    end // with
  else begin
    // load rucksack file
    lContent := TStringList.Create;
    try
      lContent.LoadFromFile(lFileName);
      { Locate correct section of rucksack and loop through }
      for i := lContent.IndexOf('<TAXON>') + 1 to lContent.IndexOf('</TAXON>') -1 do
        Result := Result + '''' + lContent[i] + ''',';
      // lop off last comma
      Result := Copy(Result, 1, Length(Result) - 1);
    finally
      lContent.Free;
    end; // try
  end;
  // safety check, if nothing in list then add a blank, so SQL is OK
  if Result = '' then
    Result := '''''';
end;

//==============================================================================
{ Function that creates SQL for returning the Authors from the Reference_Author_
  CrossTab query. This depends on the max number of authors per reference- we
  mustn't refer to a column not in the query }
function TdmGeneralData.GetAuthorFieldSQL: string;
begin
  with qryAllPurpose do begin
    SQL.Text := 'SELECT MAX(Sort_Order) FROM Reference_Author';
    Open;
    try
      if Fields[0].IsNull then // no authors
        Result := SQL_AUTHORS_0
      else if Fields[0].AsInteger = 1 then
        Result := SQL_AUTHORS_1
      else if Fields[0].AsInteger = 2 then
        Result := SQL_AUTHORS_2
      else if Fields[0].AsInteger >=3 then
        Result := SQL_AUTHORS_3;
    finally
      Close;
    end; // try
  end;
end;

//==============================================================================
function TdmGeneralData.Custodian
  (const ATableName, APrimaryKey: string; AKeyString: TKeyString): string;
{Returns the Custodian of for the record specified by the parameters.}
const
  SQL_TEXT = 'SELECT Custodian FROM %s WHERE %s = ''%s'';';
begin
  with qryAllPurpose do begin
    ParseSQL := false;
    SQL.Text := Format(SQL_TEXT, [ATableName, APrimaryKey, AKeyString]);
    Open;
    try
      if RecordCount = 1 then
        Result := FieldByName('Custodian').AsString
      else Result := '';
    finally
      Close;
      ParseSQL := true;
    end;
  end;
end;

//==============================================================================
function TdmGeneralData.HaveCustody
  (const ATableName, APrimaryKey: string; AKeyString: TKeyString): Boolean;
{Returns True iff the custodian for the record specified by the parameters is the SiteID.}
begin
  Result := Custodian(ATableName, APrimaryKey, AKeyString) = AppSettings.SiteID;
end;

//==============================================================================
function TdmGeneralData.GetNextKey(const ATableName, AKeyFieldName: string;
  AtfCheckKey: Boolean): TKeyString;
begin
  if not AtfCheckKey then
    result := GetNextKey(ATableName, AKeyFieldName)
  else
  begin
    spNextKey.Parameters.ParamByName('@TableName').Value := ATableName;
    spNextKey.Parameters.ParamByName('@SiteID').Value :=AppSettings.SiteID;
    spNextKey.ExecProc;
    result := spNextKey.Parameters.ParamByName('@NextKey').Value;
    //  Result:= FIDGen.GetNextKey(ATableName);
  end;
end;

procedure TdmGeneralData.RepairLastKeyTable(const ATableName,
  AKeyFieldName: string);
begin
  with spRepairLastKey do
  begin
    Parameters.ParamByName('@TableName').Value := ATableName;
    Parameters.ParamByName('@SiteID').Value := AppSettings.SiteId;
    Parameters.ParamByName('@KeyField').Value := AKeyFieldName;
    ExecProc;
  end;
end;

{-------------------------------------------------------------------------------
  Raises an exception if the taxon selected does not allow data entry
}
procedure TdmGeneralData.CheckTaxonAllowsDataEntry(const AListItemKey: string);
var
  lRs: ADODB._Recordset;
begin
  lRs := dmDatabase.ExecuteSQL(
      'SELECT Allow_Data_Entry FROM Index_Taxon_Name '
      + 'WHERE Taxon_List_Item_Key=''' + AListItemKey + '''', True);
  if lRs.RecordCount > 0 then begin
    if lRs.Fields['Allow_Data_Entry'].Value<>0 then
      Exit;
  end;
  raise EGeneralData.CreateNonCritical(ResStr_TaxonNoDataEntry);
end;

{-------------------------------------------------------------------------------
  Retrieve the caption for a report, appending the snapshot and template name
      if required.
}
function TdmGeneralData.GetReportCaption(const AFileName: string): string;
var
  lXMLDoc: IXMLDocument;
  lXMLNode: IXMLNode;
  lSnapshotString: String;
begin
  Result := '';
  lXMLDoc := NewXMLDocument;
  try
    lXMLDoc.LoadFromFile(AFileName);
    try
      lXMLNode := lXMLDoc.ChildNodes['Report'].ChildNodes['Output'];
      if lXMLNode.ChildNodes.Count > 0 then begin
        if lXMLNode.ChildNodes['Template'].AttributeNodes.Count >0 then
          Result   := ExtractFileName(
                                 VarToStr(lXMLNode.ChildNodes['Template'].Attributes['file']));

        if lXMLNode.ChildNodes['Snapshot'].AttributeNodes.Count >0 then
          lSnapshotString := ExtractFileName(
                                 VarToStr(lXMLNode.ChildNodes['Snapshot'].Attributes['file']));
        if Result = '' then
          Result := lSnapshotString
        else if lSnapshotString <> '' then
          Result := Result + '/' + lSnapshotString;
      end;
      // Wrap it up.
      if Result <> '' then
        Result := ' (' + Result + ')';
    except
      // Ignore any problems locating nodes, not that crucial.
      on Exception do
        Result := '';
    end;
    Result := ExtractWithoutExt(AFileName) + Result;
  except on E:EDOMParseError do
    Result := ExtractWithoutExt(AFileName);
  end;
end;

{-------------------------------------------------------------------------------
  Fills a combo box with the list of wizard reports.  The object for each item
      is set to a TKeyData object with the file name in it.
}
procedure TdmGeneralData.PopulateWizardReportCombo(ACombo: TComboBox;
  const APath: string);
var
  lSearchRec: TSearchRec;
  lSearchResult: Integer;
  lData: TStringClass;
begin
  lSearchResult := FindFirst(APath + '*.wzd', faAnyFile, lSearchRec);
  while lSearchResult = 0 do begin
    lData := TStringClass.Create;
    lData.Item := lSearchRec.Name;
    ACombo.Items.AddObject(GetReportCaption(APath + lSearchRec.Name), lData);
    lSearchResult := FindNext(lSearchRec);
  end;
  FindClose(lSearchRec);
end;

{-------------------------------------------------------------------------------
  Determines whether the current user is allowed to edit a record.
}
function TdmGeneralData.HasFullEditAccess(const ATableName, AKeyName, AKey: String): Boolean;
begin
  with AppSettings do begin
    // Anything below FullUser has no full edit rights.
    Result := UserAccessLevel > ualAddOnly;
    // For FullUser, check restriction flag.
    if (UserAccessLevel = ualFullUser) and RestrictFullEdit then
      with dmDatabase.ExecuteSQL(Format('SELECT Entered_By FROM %s WHERE %s = ''%s'' ',
                                        [ATableName, AKeyName, AKey]), True) do begin
        Result := (BOF and EOF); // if no record, then its new, so you have edit access
        if not Result then
          // record exists, so check you entered it
          Result := Fields['Entered_By'].Value = UserID;
      end;
  end;
end;

{-------------------------------------------------------------------------------
  Determines whether the current user is allowed to edit a record, based on
     a record which contains a session ID.
}
function TdmGeneralData.SessionHasFullEditAccess(const ATableName, AKeyName, AKey: String): Boolean;
begin
  with AppSettings do begin
    // Anything below FullUser has no full edit rights.
    Result := UserAccessLevel > ualAddOnly;
    // For FullUser, check restriction flag.
    if (UserAccessLevel = ualFullUser) and RestrictFullEdit then
      with dmDatabase.ExecuteSQL(Format('SELECT S.User_Name_Key '+
          'FROM Session S INNER JOIN %s T ON T.Entered_Session_ID=S.Session_ID WHERE T.%s = ''%s'' ',
                                        [ATableName, AKeyName, AKey]), True) do
        if RecordCount>0 then
          Result := Fields['User_Name_Key'].Value = UserID
        else
          Result := false;
  end;
end;

{-------------------------------------------------------------------------------
  Check for a unique keyword in the edit box.  Display the find dialog to
      obtain one if required, and focuses the supplied control.
}
function TdmGeneralData.CheckConcept(const findTitle, noResultsMessage: String; findType: TFindType;
    aEditControl: TEdit; var oKey: TKeyString): Boolean;
var
  lFind: TdlgFind;
begin
  Result := true;
  lFind := TdlgFind.CreateDialog(nil, findTitle, findType);
  with lFind do begin
    try
      if FindUnique(aEditControl.Text) then begin
        oKey := ItemKey;  // Get the associated key
        aEditControl.Text := ItemText;
      end
      else if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then begin
          oKey:=ItemKey;  // Get the associated key
          aEditControl.Text := ItemText;
        end else
          Result:=false;
      end else begin // No source items
        Result := False;
        ShowInformation(noResultsMessage);
      end;
    finally
      Free;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Check for a unique keyword from a string.  Display the find dialog to
      obtain one if required.
}
function TdmGeneralData.CheckConcept(const findTitle, noResultsMessage: String; findType: TFindType;
    var ASearchText: string; var oKey: TKeyString; ATabToFocus: TTabSheet = nil): Boolean;
var
  lFind: TdlgFind;
begin
  Result := true;
  lFind := TdlgFind.CreateDialog(nil, findTitle, findType);
  with lFind do begin
    try
      if FindUnique(ASearchText) then begin
        oKey := ItemKey;  // Get the associated key
        ASearchText := ItemText;
      end
      else if not eSearchText.NoSourceItems then begin
        if Assigned(ATabToFocus) then
          ATabToFocus.PageControl.ActivePage := ATabToFocus;
        if ShowModal = mrOk then begin
          oKey := ItemKey;  // Get the associated key
          ASearchText := ItemText;
        end else
          Result := False;
      end else begin // No source items
        Result := False;
        ShowInformation(noResultsMessage);
      end;
    finally
      Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Check for a unique individual from a linked edit
}
function TdmGeneralData.CheckOrganisation(ALinkedName: TAddinLinkedEdit):
    Boolean;
var
  key: TKeyString;
  name: string;
begin
  if ALinkedName.Key='' then begin
    Result := CheckOrganisation(ALinkedName.Text, key, name);
    ALinkedName.Text := name;
    ALinkedName.Key  := key;
  end
  else
    Result := true;
end;

{-------------------------------------------------------------------------------
  Check for a unique individual from a linked edit
}
function TdmGeneralData.CheckName(ALinkedName: TAddinLinkedEdit): Boolean;
var
  key: TKeyString;
  name: string;
begin
  if ALinkedName.Key='' then begin
    Result := CheckName(ALinkedName.Text, key, name);
    ALinkedName.Text := name;
    ALinkedName.Key  := key;
  end
  else
    Result := true;
end;

{-------------------------------------------------------------------------------
  Validates the taxon specified in the edit box.  Uses the Find dialog if
     necessary.  Returns the key in oKey.  A result of false indicates the
     taxon was not found.
}
function TdmGeneralData.CheckOrFindDataItem(var AText: string; var AKey:
    TKeyString; AFindtype: TFindType; const AFindCaption: string): boolean;
var
  lFind: TdlgFind;
begin
  Result := true;
  lFind := TdlgFind.CreateDialog(nil, True, AFindCaption, AFindType);
  with lFind do begin
    try
      StoreSearchText(AText);
      if FindUnique(AText) then begin
        AKey  := ItemKey;
        AText := ItemText;
      end else
      if ShowModal = mrOk then begin
        AKey  := ItemKey;
        AText := ItemText;
      end else
        Result := false;
    finally
      Release;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Validates the taxon specified in the edit box.  Uses the Find dialog if
     necessary.  Returns the key in oKey.  A result of false indicates the
     taxon was not found.
}
function TdmGeneralData.CheckIndividual(iEditControl: TEdit; var oKey:
    TKeyString): boolean;
var key : TKeyString;
    caption: string;
begin
  caption := iEditControl.Text;
  key  := oKey;
  Result := true;
  if (iEditControl.Modified) or (key='') then begin
    CheckOrFindDataItem(caption, key, ftIndividual, ResStr_FindIndividual);
    oKey := key;
    iEditControl.Text := caption;
  end;
end;  // CheckTaxon

{-------------------------------------------------------------------------------
  Find a unique match for the individual, display the Find dialog if required. Use
     when the individual is in a grid, otherwise check other overloads.
}
function TdmGeneralData.CheckOrganisation(const AText:string; var AKey:
    TKeyString; var ACaption:string): boolean;
begin
  ACaption := AText;
  Result := CheckOrFindDataItem(ACaption, AKey, ftOrganisation, ResStr_FindOrganisation);
end;  // CheckBiotope

{-------------------------------------------------------------------------------
  Simple method to retrieve the entered spatial reference for a location
}
function TdmGeneralData.GetLocationSpatialRef(const ALocationKey: string): 
    TValidSpatialRef;
begin
  Result.Valid := False;
  if ALocationKey<>'' then
  with dmDatabase.GetRecordset('usp_Location_Select', ['@Key', ALocationKey]) do
    if not (Eof or Bof) then begin
      Result := ValidSpecificSpatialRef(
          VarToStr(Fields['SPATIAL_REF'].Value),
          Fields['SPATIAL_REF_SYSTEM'].Value);
    end;
end;

{-------------------------------------------------------------------------------
  Simple method to retrieve the preferred name for a location
}
function TdmGeneralData.GetLocationName(const ALocationKey: string): string;
begin
  Result := '';
  if ALocationKey<>'' then
  with dmDatabase.GetRecordset('usp_Location_Select', ['@Key', ALocationKey]) do
    if not (Eof or Bof) then
      Result := Fields['ITEM_NAME'].Value;
end;

{-------------------------------------------------------------------------------
  Retrieve the validation state for a taxon occurrence based on the competency
    of the user and the validation requirements of the taxon, plus the number
    of determinations and the status of the preferred determination.
}
function TdmGeneralData.GetValidationState(const ATLIKey: string; ACompetency:
    integer; ADetCount: integer; APrefDetInvalid: boolean): Variant;
var
  lValidationLevel: variant;
  lCompetent: boolean;
begin
  lValidationLevel := dmDatabase.GetStoredProcOutputParam('usp_TaxonListItem_ValidationLevel_Get',
      ['@Key', ATLIKey], '@Output');
  if VarIsNull(lValidationLevel) then
    lCompetent := true
  else
    lCompetent := ACompetency >= lValidationLevel;
  if (ADetCount >= 2) and (lCompetent) and (not APrefDetInvalid) then
    Result := 2 // passed
  else if APrefDetInvalid then
    Result := 1 // failed
  else
    Result := 0; // not validated
end;

{-------------------------------------------------------------------------------
  Checks if a name key applies to an individual or an organisation.
}
function TdmGeneralData.IsNameKeyForOrganisation(const AKey: string): Boolean;
begin
  with dmDatabase.ExecuteSQL(Format(
      'SELECT Organisation FROM Name WHERE Name_Key=''%s''', [AKey]), True) do
    if not (EOF or BOF) then
      Result := Fields['Organisation'].Value
    else
      raise EGeneralData.Create(ResStr_NameKeyNotFound);
end;

{-------------------------------------------------------------------------------
  Set a node text and background colour according to it's Filtered state
}
procedure TdmGeneralData.SetCanvasColourForFilterNode(ACanvas: TCanvas;
    AFiltered: boolean);
begin
  if AFiltered then begin
    ACanvas.Brush.Color := MergeColours(clHighlight, clWindow, 30);
    ACanvas.Font.Color := GetContrastColour(ACanvas.Brush.Color);
  end
  else begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.Font.Color := clWindowText;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the verified value for the given determination type
}
function TdmGeneralData.GetDetTypeVerified(const ADetTypeKey: String): Integer;
begin
  Result := dmDatabase.GetStoredProcOutputParam('usp_DeterminationType_Verified_Get',
      ['@Key', ADetTypeKey], '@Output');
end;
    
{-------------------------------------------------------------------------------
  Gets the determination type of the preferred determination for the taxon occurrence
}
function TdmGeneralData.GetPreferredTaxonDetType(const AOccurrenceKey: String): String;
begin
  Result := VarToStr(dmDatabase.GetStoredProcOutputParam('usp_TaxonDetermination_PreferredType_Get',
      ['@Key', AOccurrenceKey], '@Output'));
end;

{-------------------------------------------------------------------------------
  Gets the determination type of the preferred determination for the biotope occurrence
}
function TdmGeneralData.GetPreferredBiotopeDetType(const AOccurrenceKey: String): String;
begin
  Result := VarToStr(dmDatabase.GetStoredProcOutputParam('usp_BiotopeDetermination_PreferredType_Get',
      ['@Key', AOccurrenceKey], '@Output'));
end;

{-------------------------------------------------------------------------------
  Write the relevant files. Called when exporting Datasets (squares/points) or
  polygon layers (polygons/lines) as SHP files.
}
procedure TdmGeneralData.WriteSHPFiles(listOne, listTwo: TSVOShapeList;
  const fileName, markerOne, markerTwo, spatialRefSystem: String);
var
  rw: TSVOGISReadWrite;
  fileRoot, fileExt, projection: String;
  projectionList: TStringList;

    procedure WriteFile(const AFileName: string);
    begin
      // remove the dbf files as the user doesn't need a repeat of the overwrite dialog
      DeleteFile(AFileName + '.dbf');
      if FileExists(AFileName + '.dbf') then // Didn't delete properly, someone else is probably using it.
        raise EFCreateError.CreateFmt(ResStr_FileInUse, [AFileName])
      else begin
        rw.ExportFileName := AFileName + fileExt;
        rw.WriteFile;
        if Assigned(projectionList) then
          projectionList.SaveToFile(AFileName + '.prj');
      end;
    end;

begin
  if spatialRefSystem <> '' then begin
    // Gets the projection data for the current spatial reference system- this is
    // just a single string, but is stored as a string list so as to use the SaveToFile
    // method.
    projection := dmDatabase.GetStoredProcOutputParam(
        'usp_Projection_Get_ForSpatialReferenceSystem',
        ['@SpatialRefSystem', spatialRefSystem,
         '@Projection',       ''],
        '@Projection');
    if projection <> '' then begin
      projectionList := TStringList.Create;
      projectionList.Add(projection);
    end;
  end;

  rw := TSVOGISReadWrite.Create(nil);
  try
    fileExt  := ExtractFileExt(fileName);
    fileRoot := LeftStr(fileName, Length(fileName) - Length(fileExt));
    // Only specify type in filename if both lists are populated.
    if (listOne.Count <> 0) and (listTwo.Count <> 0) then begin
      rw.ShapeList := listOne;
      WriteFile(fileRoot + ' (' + markerOne + ')');
      rw.ShapeList := listTwo;
      WriteFile(fileRoot + ' (' + markerTwo + ')');
    end else begin
      if listOne.Count = 0 then rw.ShapeList := listTwo
                           else rw.ShapeList := listOne;
      WriteFile(fileRoot);
    end;
  finally
    rw.ShapeList := nil;
    rw.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Gets a vague date out of a _Recordset record.
}
function TdmGeneralData.GetVagueDateFromRecordset(rs: ADODB._Recordset;
  const prefix: String = ''): TVagueDate;
begin
  if not VarIsNull(rs.Fields[prefix + 'Vague_Date_Start'].Value) then
    Result.StartDate := VarToDateTime(rs.Fields[prefix + 'Vague_Date_Start'].Value);
  if not VarIsNull(rs.Fields[prefix + 'Vague_Date_End'].Value) then
    Result.EndDate := VarToDateTime(rs.Fields[prefix + 'Vague_Date_End'].Value);
  Result.DateTypeString := VarToStr(rs.Fields[prefix + 'Vague_Date_Type'].Value);
end;  // GetVagueDateFromRecordset

{-------------------------------------------------------------------------------
  Updates the content of the given control with the dropped data. Two cases:
    Format is CF_JNCCDATA: Use provided method to find the content from Recorder
    Format is CF_TEXT:     Use provided text to set the content.
}
function TdmGeneralData.DropLinkedEditText(ctrl: TAddinLinkedEdit; format: Integer; data: TKeyList;
  getName: TGetName; text: TStringList): Boolean;
var
  selStart: Integer;
begin
  Result := False;
  if (format = CF_JNCCDATA) and (data.Header.ItemCount > 0) then
  begin
    Result    := True;
    ctrl.Text := getName(data.Items[0].KeyField1);
    ctrl.Key  := data.Items[0].KeyField1;
  end else
  if (format = CF_TEXT) and (text.Count > 0) and (text[0] <> '') then
  begin
    Result   := True;
    selStart := ctrl.SelStart;

    { If some text selected, replace it, otherwise insert at point }
    { note we only bother with the first string }
    if Length(ctrl.EditBox.SelText) > 0 then
      ctrl.EditBox.SelText := text[0]
    else
      ctrl.Text := StuffString(ctrl.Text, ctrl.SelStart + 1, 0, text[0]);

    ctrl.SelStart := selStart + Length(text[0]);
    ctrl.Key      := '';
    ctrl.Modified := True;
  end;
end;  // DropLinkedEditText
{-------------------------------------------------------------------------------
  Checks if the supplied group key is the same as that on the supplied tli key
}
function TdmGeneralData.IsGroupCorrect(const ATLIKey: string; AGroupKey: string): Boolean;
var Sql : string;
begin
  Sql := Format('Select * from Taxon_List_Item INNER JOIN Taxon_Version ' +
      'ON Taxon_List_Item.Taxon_Version_Key = Taxon_Version.Taxon_Version_Key ' +
      'WHERE Taxon_List_Item_Key = ''%s'' AND Output_Group_Key = ''%s''' ,[ATLIKey,AGroupKey]);

  with dmDatabase.ExecuteSQL(Sql,True) Do
    if not (EOF or BOF) then
      Result := True
    else
      Result := False;
  end;

end.

