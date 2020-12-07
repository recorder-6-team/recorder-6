//==============================================================================
//  Unit:        DictionaryHTMLDetails
//
//  Implements:  THTMLLines
//               TNBNSearchSettings
//
//  Description: Class to get data about biotope or taon and format the result
//               to HTML.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 43 $
//    $Date: 30/03/10 11:13 $
//    $Author: Robertjohnson $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DictionaryHTMLDetails;

interface

uses
  SysUtils, Classes, Forms, JNCCDatasets, DB, DataClasses, HierarchyNodes,
  BaseDictionaryDataUnit, HTMLDisplayFuncs, ExceptionForm, GeneralFunctions,
  ADODB, DatabaseAccessADO, SQLConstants;

const
  ST_SOURCE_DENOT_CHAR    = '*';              // Character used to denote the key for the source which has been clicked
  IMAGES_TAG              = '#IMAGES';        // tag in HTML - replaced with CD image folder
  LOCAL_IMAGES_TAG        = '#LOCAL_IMAGES';  // tag denoting path for local images folder
  DEST_LIST_DENOTE_STRING = '#CHECKLIST';     // Denotes checklist as destination in internal anchor HTML tag

resourcestring
  SDictDetails_ListSynonyms     = 'List Synonyms';
  SDictDetails_OtherSynonyms    = 'Other Synonyms';
  SDictDetails_OtherCommonNames = 'Other Common Names';
  SDictDetails_OtherChecklists  = 'Other Checklists';
  SDictDetails_Details          = 'Details';
  SDictDetails_Sources          = 'Sources';
  SDictDetails_Statuses         = 'Statuses';
  SDictDetails_Status           = 'Status';
  SDictDetails_StatusSources    = 'Status Sources';
  SDictDetails_Facts            = 'Facts';
  SDictDetails_FactSources      = 'Fact Sources';
  SDictDetails_Name             = 'Name';
  SDictDetails_TaxonListedAs    = 'taxon listed as';
  SDictDetails_Code             = 'Code';
  SDictDetails_TaxonUkNative    = 'This taxon is UK Native.';

type
  EHTMLDictionaryDetails = class(TExceptionPath);
  ESourceNotFound = class(EHTMLDictionaryDetails);
  EFileDoesNotExist = class(EHTMLDictionaryDetails);

  TSetParamProc = procedure (Sender: TObject; const ADictNode: THTMLDictionaryNode) of object;


  { Class that reads the NBN Gateway search settings from the SETTING table
    Created : 28/11/2002 }
  TNBNSearchSettings = class
  private
    FstNBNSearchUrl : string;
    FstNBNSearchSeparator : string;
    FtfHasNBNSearchSettings: Boolean;
    FProviderName: string;
    procedure GetNBNSearchStrings;
    procedure SetHasNBNSearchSettings(const Value: Boolean);
    function GetNBNSearchString(setting, default: string): string;
  public
    constructor Create;
    property HasNBNSearchSettings : Boolean read FtfHasNBNSearchSettings write SetHasNBNSearchSettings;
    property NBNSearchURL : string read FstNBNSearchURL;
    property NBNSearchSeparator : string read FstNBNSearchSeparator;
    property ProviderName: string read FProviderName;
  end;

  //----------------------------------------------------------------------------
  // Object which produces the HTML generated for the Details as a stringlist output
  THTMLLines = class
  private
    FListKey: TKeyString;
    FDictNode: THTMLDictionaryNode;
    FDictData: TBaseDictionaryData;
    FSetParamProc: TSetParamProc;

    // For details sections
    FslHeading : TStringList;
    FslListHeading : TStringList;
    FslSynonyms : TStringList;
    FslFacts : TStringList;
    FslCodes : TStringlist;
    FslChecklists : TStringList;
    FslStatuses : TStringList;
    FslUKNative : TStringList;
    FslAssociated : TStringList;
    // For sources sections
    FslSourcesMain : TStringList;
    FslSourcesFacts : TStringList;
    FslSourcesStatus : TStringList;
    FslSourcesSpecies : TStringList;
    FslSourcesAssociated : TStringList;
    // Links to NBN WebSite
    FslGatewayLink : TStringList;
    // Main output stringlist
    FslOutputHTML : TStringList;

    FShowSynonyms: Boolean;
    FShowCodes: Boolean;
    FShowStatuses: Boolean;
    FShowCheckLists: Boolean;
    FShowFacts: Boolean;
    FShowUKNative: Boolean;
    FShowAssociated: Boolean;
    FNBNLinkName: String;
    procedure AnchorSource(const ADetail, ASrcKey, ASectionPrefix: String;
      var ioslSources, ioslMain: TStringList);
    procedure CheckListsFromSource(const AQuery: TJnccQuery;
      const ACheckLists, APreferredNames, AListsSearched, ATaxonNameKeys: TStringList);
    function HaveString(const AString: String): Boolean;
    function ReplaceImagesFolder(const ATag, ALink: String): String;
    procedure ScanForImagesTag;
    function StandardImagePath: String;

    // HTML PAGE SECTIONS
    procedure GetMain;
    procedure GetHeading;
    procedure GetSynonyms;
    procedure GetCodes;
    procedure GetStatuses;
    procedure GetCheckLists(const ATaxonNameKeys: TStringList);
    procedure GetFacts;
    procedure GetUKNative;
    procedure GetAssociated;
    procedure GetLinkToNBN;
    procedure GetListHeading(const ASameName: Boolean);
    // HTML BUILDING ROUTINES
    procedure TidySources(var ioslSources: TStringList);
    function GetCurrentTaxonName(const AConnection: TADOConnection;
      const ATaxonPrefNameKey: TKeyString): string;
    function NBNSearchSettings : TNBNSearchSettings;
  public
    constructor Create(const AListKey: TKeyString; const ADicNode: THTMLDictionaryNode;
      const ADictData: TBaseDictionaryData; const ASetParamProc: TSetParamProc);
    destructor Destroy; override;
    function CreateHTML(const AKeyList: TStringList = nil): TStringList;
    function CreateMoreTaxonHTML(const ASameName: Boolean): TStringList;
    property ListKey: TKeyString read FListKey;
    property ShowAssociated: Boolean read FShowAssociated write FShowAssociated;
    property ShowCheckLists: Boolean read FShowCheckLists write FShowCheckLists;
    property ShowCodes: Boolean read FShowCodes write FShowCodes;
    property ShowFacts: Boolean read FShowFacts write FShowFacts;
    property NBNLinkName: String read FNBNLinkName write FNBNLinkName;
    property ShowStatuses: Boolean read FShowStatuses write FShowStatuses;
    property ShowSynonyms: Boolean read FShowSynonyms write FShowSynonyms;
    property ShowUKNative: Boolean read FShowUKNative write FShowUKNative;
  end;  // THTMLLines

procedure ShowSource(const AAnchorSource: String);

//==============================================================================
implementation

uses
  ApplicationSettings, GeneralData, FormActions, Maintbar, References, Dialogs,
  ComCtrls, DBTables, Constants;

resourcestring
  ResStr_NoSearchSettings  = 'The settings required for NBN Gateway searches are missing.';
  ResStr_WrongDataModule   = 'Data module is wrong type for taxon dictionary';
  ResStr_CannotFindFileSource = 'The file source at ''%s'' could not be found.';
  ResStr_Constraint           = 'Constraint';
  ResStr_GeographicArea       = 'Geographic Area';
  ResStr_FindInformation      = 'Find information on <i>%s</i> from %s';
  ResStr_As                   = 'As';
  ResStr_ProviderName         = 'the NBN';

var
  mNBNSearchSettings : TNBNSearchSettings;  // module level instance of class

//==============================================================================
procedure ShowSource(const AAnchorSource: String);
var lsSourceKey: TKeyString;
    loForm     : TfrmReferences;
    lsFileName : String;
begin
  if Copy(AAnchorSource, 1, 1) = ST_SOURCE_DENOT_CHAR then
  begin
    // a source has been clicked on in the HTML browser
    lsSourceKey := Copy(AAnchorSource, 2, Length(AAnchorSource) - 1); // remove Source denoting character
    try
      case dmGeneralData.IsSourceInternal(lsSourceKey) of
        true:
            begin
              // source is internal so display it using the action
              dmFormActions.actDocuments.Execute;
              loForm := TfrmReferences(frmMain.GetForm(TfrmReferences));
              if Assigned(loForm) then
                loForm.FindAndDisplaySource(lsSourceKey);
            end;
        false:
            begin
              // source is external file
              lsFileName := dmGeneralData.GetSourceFileText(lsSourceKey);
              if FileExists(lsFileName) then
                ShellFile(lsFileName)
              else
                raise EFileDoesNotExist.CreateNonCritical(Format(ResStr_CannotFindFileSource, [lsFileName]))
            end;
      end;
    except
      on EGeneralDataError do
        ShellFile(Copy(AAnchorSource, 2, Length(AAnchorSource) - 1)); // if Source not found, attempt to open using windows default viewer
    end; // try..except
  end
  else
  begin
    ShellFile(AAnchorSource);
  end;
end;  // ShowSource

//==============================================================================
constructor THTMLLines.Create(const AListKey: TKeyString; const ADicNode: THTMLDictionaryNode;
  const ADictData: TBaseDictionaryData; const ASetParamProc: TSetParamProc);
begin
  inherited Create;
  FListKey       := AListKey;
  FDictNode      := ADicNode;
  FDictData      := ADictData;
  FSetParamProc  := ASetParamProc;

  // Details sections
  FslHeading     := TStringList.Create;
  FslListHeading := TStringList.Create;
  FslSynonyms    := TStringList.Create;
  FslCodes       := TStringList.Create;
  FslCheckLists  := TStringList.Create;
  FslFacts       := TStringList.Create;
  FslStatuses    := TStringList.Create;
  FslUKNative    := TStringList.Create;
  FslAssociated  := TStringList.Create;

  // Sources sections
  FslSourcesMain       := TStringList.Create;
  FslSourcesFacts      := TStringList.Create;
  FslSourcesStatus     := TStringList.Create;
  FslSourcesSpecies    := TStringList.Create;
  FslSourcesAssociated := TStringList.Create;

  // Link to NBN WebSite
  FslGatewayLink := TStringList.Create;

  // Dont allow duplicate sources in each section
  FslSourcesFacts.Sorted          := true;
  FslSourcesStatus.Sorted         := true;
  FslSourcesSpecies.Sorted        := true;
  FslSourcesAssociated.Sorted     := true;
  FslSourcesFacts.Duplicates      := dupIgnore;
  FslSourcesStatus.Duplicates     := dupIgnore;
  FslSourcesSpecies.Duplicates    := dupIgnore;
  FslSourcesAssociated.Duplicates := dupIgnore;

  FslOutputHTML := TStringList.Create;

  // Set flags default value
  FShowStatuses   := true;
  FShowFacts      := true;
  FShowAssociated := true;
  FShowSynonyms   := false;  // Taxon
  FShowCodes      := false;  // Taxon
  FShowCheckLists := false;  // Taxon
  FShowUKNative   := false;  // Taxon
end;  // Create

//==============================================================================
destructor THTMLLines.Destroy;
begin
  //  Details sections
  FslHeading.Free;
  FslHeading := nil;
  FslListHeading.Free;
  FslListHeading := nil;
  FslSynonyms.Free;
  FslSynonyms := nil;
  FslCodes.Free;
  FslCodes := nil;
  FslCheckLists.Free;
  FslCheckLists := nil;
  FslFacts.Free;
  FslFacts := nil;
  FslStatuses.Free;
  FslStatuses := nil;
  FslUKNative.Free;
  FslUKNative := nil;
  FslAssociated.Free;
  FslAssociated := nil;
  // Sources section
  FslSourcesMain.Free;
  FslSourcesMain := nil;
  FslSourcesFacts.Free;
  FslSourcesFacts := nil;
  FslSourcesStatus.Free;
  FslSourcesStatus := nil;
  FslSourcesSpecies.Free;
  FslSourcesSpecies := nil;
  FslSourcesAssociated.Free;
  FslSourcesAssociated := nil;
  // Link to NBN WebSite
  FslGatewayLink.Free;
  FslGatewayLink := nil;
  // Main output stringlist
  FslOutputHTML.Free;
  FslOutputHTML := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
function THTMLLines.CreateHTML(const AKeyList: TStringList = nil): TStringList;
begin
  // Setup query parameteres
  FSetParamProc(Self, FDictNode);

  // Start HTML, open tags
  FslOutputHTML.Clear;
  FslOutputHTML.Add('<HTML>');
  FslOutputHTML.Add(Style);
  FslOutputHTML.Add('<BODY>');

  GetHeading;
  FslOutputHTML.AddStrings(FslHeading);

  if ShowSynonyms then begin
    GetSynonyms;
    FslOutputHTML.AddStrings(FslSynonyms);
  end;
  if ShowCodes then begin
    GetCodes;
    FslOutputHTML.AddStrings(FslCodes);
  end;
  if ShowStatuses then begin
    GetStatuses;
    FslOutputHTML.AddStrings(FslStatuses);
  end;
  // CheckLists
  if ShowCheckLists then begin
    GetCheckLists(AKeyList);
    FslOutputHTML.AddStrings(FslCheckLists);
  end;
  // Facts
  if ShowFacts then begin
    GetFacts;
    FslOutputHTML.AddStrings(FslFacts);
  end;
  if ShowUKNative then begin
    GetUKNative;
    FslOutputHTML.AddStrings(FslUKNative);
  end;
  if ShowAssociated then begin
    GetAssociated;
    FslOutputHTML.AddStrings(FslAssociated);
  end;
  // Sources
  GetMain; // Main Source Details
  FslOutputHTML.Add('<P>');
  if FslSourcesMain.Count + FslSourcesStatus.Count +
     FslSourcesFacts.Count + FslSourcesAssociated.Count > 0 then
    FslOutputHTML.Add(MiniHeaderStyle(SDictDetails_Sources));
  FslOutputHTML.AddStrings(FslSourcesMain);
  FslOutputHTML.AddStrings(FslSourcesStatus);
  FslOutputHTML.AddStrings(FslSourcesFacts);
  FslOutputHTML.AddStrings(FslSourcesAssociated);
  FslOutputHTML.Add('</P>');

  // Link to NBN Gateway
  GetLinkToNBN;
  FslOutputHTML.AddStrings(FslGatewayLink);

  // Close HTML tags
  FslOutputHTML.Add('</BODY>');
  FslOutputHTML.Add('</HTML>');

  ScanForImagesTag;

  Result := FslOutputHTML;
end;  // CreateHTML

//==============================================================================
// Taxon
function THTMLLines.CreateMoreTaxonHTML(const ASameName: Boolean): TStringList;
begin
  FSetParamProc(Self, FDictNode);
  with FDictData do
    dmDatabase.SetDatabaseLocal([TJnccQuery(FindComponent('qryGeneral')),
                                 TJnccQuery(FindComponent('qryListName')),
                                 TJnccQuery(FindComponent('qrySynonyms')),
                                 TJnccQuery(FindComponent('qryCodes')),
                                 TJnccQuery(FindComponent('qryStatus'))]);

  FslOutputHTML.Clear;
  FslOutputHTML.Add('<HTML>');
  FslOutputHTML.Add(Style);
  FslOutputHTML.Add('<BODY>');
  FslOutputHTML.Add(LineSpace);

  // Add taxon details for selected checklist
  GetListHeading(ASameName);
  FslOutputHTML.AddStrings(FslListHeading);
  GetSynonyms;
  FslOutputHTML.AddStrings(FslSynonyms);
  GetCodes;
  FslOutputHTML.AddStrings(FslCodes);
  GetStatuses;
  FslOutputHTML.AddStrings(FslStatuses);

  // If no info present in checklist, then state this
  if FslOutputHTML.Count <= 4 then FslOutputHTML.Add(NoNewInfo);

  FslOutputHTML.Add(LinkBackToMain); //Add link to top of page

  FslOutputHTML.Add('</BODY>');
  FslOutputHTML.Add('</HTML>');

  Result := FslOutputHTML;
end;  // CreateMoreTaxonHTML

//==============================================================================
procedure THTMLLines.GetHeading;
var lsHeader, lsAuthority: String;
    lstMainName          : string;
    lQuery               : TADOQuery;
begin
  lsHeader := '';
  lsAuthority := '';
  if FDictNode is TTaxonDictionaryNode then begin
    lQuery := TADOQuery.Create(nil);
    try
      dmDatabase.SetDatabaseLocal([lQuery]);
      // Use qryGeneral so that we can itallicise the latin name
      SetStandardQuery(lQuery, ssTaxonName, FDictNode.ItemKey, AppSettings.UserID);
      with lQuery do begin
        Open;
        try
          First;
          if not Eof then begin
            lstMainName := FieldByName('PREFERRED_NAME').AsString;
            lsHeader := lstMainName;
            // Itallicise the taxon name if it is latin and of an appropriate rank
            if FieldByName('PREFERRED_NAME_ITALIC').AsBoolean then
              lsHeader := '<I>' + lsHeader + '</I>';
            // Add version attribute, authorityand rank if present
            lsHeader := lsHeader + ' ' + FieldByName('Attribute').AsString
                                 + ' ' + FieldByName('Authority').AsString
                                 + ' [' + FieldByName('RankName').AsString + ']';
          end;
          lsHeader := AnchorHeading(lsHeader); // Gives link to top of HTML page
          FslHeading.Add(lsHeader);

          // if common name different to preferred name, display it
          if CompareText(FieldByName('COMMON_NAME').AsString, lstMainName)<>0 then
            if FieldByName('COMMON_NAME_ITALIC').AsBoolean then
              FslHeading.Add(MainHeaderStyle('<I>' + FieldByName('COMMON_NAME').AsString) + '</I>')
            else
              FslHeading.Add(MainHeaderStyle(FieldByName('COMMON_NAME').AsString));
        finally
          Close; // qryAllPurpose
        end; // try..finally
      end;
    finally
      lQuery.Free;
    end;
  end
  else if FDictNode is TBiotopeDictionaryNode then begin
    FslHeading.Add(AnchorHeading(FDictNode.Title));
  end;
end;  // GetHeading

//==============================================================================
procedure THTMLLines.GetStatuses;
var lsSrcKey: String;
    lqryStatus        : TJnccQuery;
    ltfFirstStatusDetail : boolean;
    procedure AddStatusDetail(const Title, Detail : string);
    begin
      if HaveString(Detail) then begin
        // separate details with <br> if not first item
        if not ltfFirstStatusDetail then
          fslStatuses[fslStatuses.Count-1] := fslStatuses[fslStatuses.Count-1]+'<br>';
        FslStatuses.Add(FontStyle(True, False, False, Title + ': ') + Detail);
        ltfFirstStatusDetail := False;
      end;
    end;

begin
  FslStatuses.Clear;
  FslSourcesStatus.Clear;
  lqryStatus := TJnccQuery(FDictData.FindComponent('qryStatus'));
  if lqryStatus <> nil then
    with lqryStatus do
      try
        Open;
        First;
        if not Eof then begin
          // Main Table
          FslStatuses.Add('<P>');
          FslStatuses.Add('<TABLE bgcolor=dimgray cellspacing=1 COLS=2 WIDTH=90%>');

          // Add Headers for table
          FslStatuses.Add(AddTagText('TH', SDictDetails_Status, ''));
          FslStatuses.Add(AddTagText('TH', SDictDetails_Details, ''));

          while not Eof do begin
            FslStatuses.Add('<TR>');
            // First Col
            FslStatuses.Add('<TD VALIGN=TOP>');
            FslStatuses.Add(FieldByName('ShortName').AsString);
            FslStatuses.Add('</TD>');

            // Second Col
            FslStatuses.Add('<TD VALIGN=TOP>');
            ltfFirstStatusDetail := True;
            // Dates
            AddStatusDetail(ResStr_DateFrom, FieldByName('DateFrom').AsString);
            AddStatusDetail(ResStr_DateTo, FieldByName('DateTo').AsString);

            // Main HTML paragraph
            AddStatusDetail(ResStr_GeographicArea, FieldByName('GeogArea').AsString);
            AddStatusDetail(ResStr_Constraint, FieldByName('StatusConstraint').AsString);

            // Sort out Status Sources

            lsSrcKey := FieldByName('SourceKey').AsString;
            if not ltfFirstStatusDetail then
            begin
              FslStatuses[FslStatuses.Count -1] := FslStatuses[FslStatuses.Count -1] + '<br>';
              ltfFirstStatusDetail := false;
            end;
            AnchorSource(
                dmGeneralData.ConvertRtfFieldToText(FieldByName('Details')),
                lsSrcKey,
                'S',
                FslSourcesStatus,
                FslStatuses);

            FslStatuses.Add('</TD>');
            FslStatuses.Add('</TR>');
            Next;
          end;
          FslStatuses.Add('</TABLE>');
          FslStatuses.Add('</P>');

          TidySources(FslSourcesStatus);

          // Add Headers
          if FslStatuses.Count > 0 then
            FslStatuses.Insert(0, MiniHeaderStyle(SDictDetails_Statuses));
          if FslSourcesStatus.Count > 0 then
            FslSourcesStatus.Insert(0, SubHeaderStyle(SDictDetails_StatusSources));
          ParaSL(FslStatuses);
        end;
      finally
        Close;
      end;
end;  // GetStatuses

//==============================================================================
procedure THTMLLines.GetFacts;
var lsTitle, lsDetail, lsSrcKey: String;
    lqryFacts                  : TJnccQuery;
begin
  FslFacts.Clear;
  FslSourcesFacts.Clear;
  lqryFacts := TJnccQuery(FDictData.FindComponent('qryFacts'));
  if lqryFacts <> nil then
    with lqryFacts do
      try
        Open;
        First;
        while not Eof do begin
          // Sort out Status Sources
          lsTitle  := FieldByName('Title').AsString;
          lsDetail := FieldByName('Data').AsString;
          lsSrcKey := FieldByName('SourceKey').AsString;
          if fslFacts.Count>0 then fslFacts.Add('<br>');  // new line between facts
          if lsTitle<>'' then
            FslFacts.Add(SubheaderStyle(lsTitle));
          AnchorSource(lsDetail, lsSrcKey, 'F', FslSourcesFacts, FslFacts);
          Next;
        end;
        TidySources(FslSourcesFacts);

        // Add headers
        if FslFacts.Count > 0 then
          FslFacts.Insert(0, MiniHeaderStyle(SDictDetails_Facts));
        if FslSourcesFacts.Count > 0 then
          FslSourcesFacts.Insert(0, SubHeaderStyle(SDictDetails_FactSources));

        ParaSL(FslFacts);
      finally
        Close;
      end;
end;  // GetFacts

//==============================================================================
procedure THTMLLines.GetAssociated;
var lsDetail, lsSrcKey, lstText: String;
    lqryAssociated: TJnccQuery;
begin
  FslAssociated.Clear;
  FslSourcesAssociated.Clear;
  lqryAssociated := TJnccQuery(FDictData.FindComponent('qryAssociated'));
  if lqryAssociated <> nil then
    with lqryAssociated do
      try
        Open;
        if not Eof then begin
          // Main Table
          FslAssociated.Add('<P>');
          FslAssociated.Add('<TABLE bgcolor="dimgray" cellspacing=1 COLS=2 WIDTH=60%>');

          // Add Headers for table
          FslAssociated.Add(AddTagText('TH', SDictDetails_Name, ''));
          FslAssociated.Add(AddTagText('TH', SDictDetails_Details, ''));
          while not Eof do begin
            FslAssociated.Add('<TR>');
            // First Col
            FslAssociated.Add('<TD>');
            lstText := Para(FieldByName('Name').AsString);
            FslAssociated.Add(lstText);
            FslAssociated.Add('</TD>');
            // Second Col
            FslAssociated.Add('<TD>');
            // Sort out Status Sources
            lsDetail := FieldByName('Data').AsString;
            lsSrcKey := FieldByName('SourceKey').AsString;
            AnchorSource(lsDetail, lsSrcKey, 'A', FslSourcesAssociated, FslAssociated);

            FslAssociated.Add('</TD>');
            FslAssociated.Add('</TR>');
            Next;
          end;
          FslAssociated.Add('</TABLE>');
          FslAssociated.Add('</P>');

          TidySources(FslSourcesAssociated);

          // Add Headers
          if FslAssociated.Count > 0 then
            FslAssociated.Insert(0, SubHeaderStyle(FDictNode.AssociatedTitle));
          if FslSourcesAssociated.Count > 0 then
            FslSourcesAssociated.Insert(0, MiniHeaderStyle(FDictNode.AssociatedSourcesTitle));

          ParaSL(FslAssociated);
        end;
      finally
        Close;
      end;
end;  // GetAssociated

//==============================================================================
procedure THTMLLines.GetMain;
var lqryMain: TJnccQuery;
begin
  FslSourcesMain.Clear;
  lqryMain := TJnccQuery(FDictData.FindComponent('qryMain'));
  if lqryMain <> nil then
    with lqryMain do
      try
        Open;
        First;
        // Populate Sources
        while not Eof do begin
          FslSourcesMain.Add(
              AddTagText(
                  'CITE',
                  Anchor(
                      '',
                      FieldByName('SourceKey').AsString,
                      dmGeneralData.GetAnySourceText(FieldByName('SourceKey').AsString)),
                  '') + LineBreak);
          Next;
        end;
        TidySources(FslSourcesMain);
        if FslSourcesMain.Count > 0 then
            FslSourcesMain.Insert(0, MiniHeaderStyle(FDictNode.MainSourcesTitle));
      finally
        Close;
      end;
end;  // GetMain

//==============================================================================
procedure THTMLLines.GetLinkToNBN;
var lsSearchfor: String;
begin
  FslGatewayLink.Clear;
  if (NBNLinkName <> '') and NBNSearchSettings.HasNBNSearchSettings then begin
    // Change to correct format for NBN Search
    lsSearchFor := StringReplace((Lowercase(NBNLinkName)), ' ',
                       NBNSearchSettings.NBNSearchSeparator, [rfReplaceAll]);

    FslGateWayLink.Add('<HR>');
    FslGateWayLink.Add('<TABLE BORDER="0"><TR>' +
                         '<TD>' +
                           '<IMG SRC="' + StandardImagePath + 'NBNicon.bmp" WIDTH=100 HEIGHT=60 ALIGN="middle">');
    FslGateWayLink.Add('<TD><A HREF="'
                       + NBNSearchSettings.NBNSearchURL
                       + lsSearchFor
                       + '">' + Format(ResStr_FindInformation, [NBNLinkName, NBNSearchSettings.FProviderName]) + '</A>');
    FslGateWayLink.Add('</TR></TABLE>');
  end;
end;  // GetLinkToNBN

//==============================================================================
// Taxon
procedure THTMLLines.GetListHeading(const ASameName: Boolean);
var lsListHeader: String;
    lqryListName: TJnccQuery;
begin
  FslListHeading.Clear;
  lqryListName := TJnccQuery(FDictData.FindComponent('qryListName'));
  if lqryListName <> nil then
    with lqryListName do begin
      if Active then Close;
      try
        Open;
        First;
        if not Eof then begin
          lsListHeader := FieldByName('TaxonListName').AsString;
          if not ASameName then
            lsListHeader := lsListHeader +
                            ' - ' + SDictDetails_TaxonListedAs + ': ' +
                            '<I>' + GetCurrentTaxonName(nil, '') + '</I>';
          lsListHeader := SubHeaderStyle(lsListHeader);

          // Use Checklist key to identify anchor and Checklist Name as text
          FslListHeading.Add(Anchor(DEST_LIST_DENOTE_STRING +
                                    FListKey +
                                    TTaxonDictionaryNode(FDictNode).ItemKey,
                                    '',
                                    lsListHeader));
        end;
      finally
        Close;
      end;
    end;
end;  // GetListHeading

//------------------------------------------------------------------------------
// Taxon
procedure THTMLLines.GetSynonyms;
var lslListLatinNames, lslOtherLatinNames, lslCommonNames: TStringList;
    lstNameToAdd : String;
    lqrySynonyms : TJnccQuery;

  {-----------------------------------------------------------------------------
    Gets the next Taxon name to add from the current position in lqrySynonyms
  }
  function GetNameToAdd: String;
  var
    lsActualName, lsAttribute, lsAuthority, lsShortName: String;
  begin
    lsActualName := lqrySynonyms.FieldByName('Actual_Name').AsString;
    if lqrySynonyms.FieldByName('Actual_Name_Italic').AsBoolean then
      Result := AddTagText('I', lsActualName, '') // Itallicise latin name
    else
      Result := lsActualName;
    lsAttribute := lqrySynonyms.FieldByName('Attribute').AsString;
    if lsAttribute <> '' then
      Result := Result + ' ' + lsAttribute;
    lsAuthority := lqrySynonyms.FieldByName('Authority').AsString;
    if lsAuthority <> '' then
      Result := Result + ' ' + lsAuthority;
    lsShortName := lqrySynonyms.FieldByName('Short_Name').AsString;
    if lsShortName <> '' then
      Result := Result + ' [' + lsShortName + ']';
    Result := AddTagText('LI', Result, '');
  end;

begin
  FslSynonyms.Clear;
  lqrySynonyms := TJnccQuery(FDictData.FindComponent('qrySynonyms'));
  if lqrySynonyms <> nil then
    with lqrySynonyms do
      if (FDictNode is TTaxonDictionaryNode) and (TTaxonDictionaryNode(FDictNode).PrefNameKey <> '') then
      begin
        lslListLatinNames  := TStringList.Create;
        lslOtherLatinNames := TStringList.Create;
        lslCommonNames     := TStringList.Create;
        try
          Open;
          First;
          if not Eof then begin
            while not Eof do begin
              // Add Latin name, attribute, authority and short name,
              // if present, to Latin name list
              lstNameToAdd := GetNameToAdd;
              if CompareText((FieldByName('Language').AsString), 'La') = 0 then begin
                // Here 1 represents 'True'
                if FieldByName('CurrentList').AsInteger = 1 then
                  lslListLatinNames.Add(lstNameToAdd)
                else
                  lslOtherLatinNames.Add(lstNameToAdd);  // formal synonym
              end else // Add name to list of common names
                lslCommonNames.Add(lstNameToAdd); // common name
              Next;
            end;   

            if lslListLatinNames.Count <> 0 then begin
              FslSynonyms.Add(MiniHeaderStyle(SDictDetails_ListSynonyms));
              FslSynonyms.Add('<MENU>');
              FslSynonyms.AddStrings(lslListLatinNames);
              FslSynonyms.Add('</MENU>');
            end;

            if (lslListLatinNames.Count <> 0) and
               ((lslCommonNames.Count <> 0) or (lslOtherLatinNames.Count <> 0)) then
              FslSynonyms.Add('<BR>');

            if lslOtherLatinNames.Count <> 0 then begin
              FslSynonyms.Add(MiniHeaderStyle(SDictDetails_OtherSynonyms));
              FslSynonyms.Add('<MENU>');
              FslSynonyms.AddStrings(lslOtherLatinNames);
              FslSynonyms.Add('</MENU>');
            end;

            if (lslOtherLatinNames.Count <> 0) and (lslCommonNames.Count <> 0) then
              FslSynonyms.Add('<BR>');

            if lslCommonNames.Count <> 0 then begin
              FslSynonyms.Add(MiniHeaderStyle(SDictDetails_OtherCommonNames));
              FslSynonyms.Add('<MENU>');
              FslSynonyms.AddStrings(lslCommonNames);
              FslSynonyms.Add('</MENU>');
              FslSynonyms.Add('<br>');
            end;
            ParaSL(FslSynonyms);
          end;
        finally
          Close;
          lslListLatinNames.Free;
          lslOtherLatinNames.Free;
          lslCommonNames.Free;
        end;
      end;
end;  // GetSynonyms

//------------------------------------------------------------------------------
// Taxon
procedure THTMLLines.GetCodes;
var lsSpeciesCode: String;
    lqryCodes    : TJnccQuery;
begin
  FslCodes.Clear;
  lsSpeciesCode:='';
  lqryCodes := TJnccQuery(FDictData.FindComponent('qryCodes'));
  if lqryCodes <> nil then
    if FDictNode is TTaxonDictionaryNode then
      with lqryCodes do begin
        if SQL.Text = '' then Exit;  // Check to see if we should open this query
        try
          Open;
          First;
          if not Eof then begin
            lsSpeciesCode := FieldByName('LST_ITM_CODE').AsString;
            if lsSpeciesCode <> '' then
              FslCodes.Add('<B>' + SDictDetails_Code + ': ' + lsSpeciesCode + '</B><BR>');
          end;
        finally
          Close;
        end;
      end;
end;  // GetCodes

//------------------------------------------------------------------------------
// Taxon
procedure THTMLLines.GetCheckLists(const ATaxonNameKeys: TStringList);
var lslFoundLists,
    lslLocalLists, lslPreferredNames: TStringList;
    liCount                         : Integer;
    lsAnchoredList                  : String;
    lqryCheckLists                  : TJnccQuery;
begin
  FslCheckLists.Clear;
  If AppSettings.UseRecommendedTaxaNames then
    lqryCheckLists := TJnccQuery(FDictData.FindComponent('qryCheckListsITN'))
  else
    lqryCheckLists := TJnccQuery(FDictData.FindComponent('qryCheckLists'));

  if lqryCheckLists <> nil then begin
    if FDictNode is TTaxonDictionaryNode then begin
      lslLocalLists := TStringList.Create;
      lslFoundLists := TStringList.Create;
      lslPreferredNames := TStringList.Create;
      try
        // Find the taxon on the local checklists
        dmDatabase.SetDatabaseLocal([lqryCheckLists]);
        CheckListsFromSource(lqryCheckLists, lslLocalLists, lslPreferredNames, lslFoundLists, ATaxonNameKeys);

        // If species is found on any other checklists
        if lslLocalLists.Count <> 0 then begin
          FslCheckLists.Add(MiniHeaderStyle(SDictDetails_OtherChecklists));
          FslCheckLists.Add('<Menu>');
          if lslLocalLists.Count <> 0 then // Taxon found in local checklists
            for liCount := 0 to lslLocalLists.Count - 1 do begin
              lsAnchoredList := AnchorCheckList(lslLocalLists[liCount], lslFoundLists[liCount], lslPreferredNames[liCount]);
              FslCheckLists.Add(lsAnchoredList);
            end;

          FslCheckLists.Add('</Menu>');
          ParaSL(FslChecklists);
        end;
      finally
        lslFoundLists.Free;
        lslLocalLists.Free;
        lslPreferredNames.Free;
      end;
    end; // if fDicNode is TTaxonDictionaryNode
  end;
end;  // GetCheckLists

//------------------------------------------------------------------------------
{ Finds names and keys for checklists on which taxa are found }
procedure THTMLLines.CheckListsFromSource(const AQuery: TJnccQuery;
  const ACheckLists, APreferredNames, AListsSearched, ATaxonNameKeys: TStringList);
var lslOrigSQL                 : TStringList;
    liCount                    : Integer;
    lsSQL, lsListName          : String;
    lPrefNameKey, lListItemKey : TKeyString;
    loOrigParams               : TParams;
begin
  lslOrigSQL := nil;
  loOrigParams := nil;
  lsSQL := '';
  with AQuery do begin
    if Active then Close;
    if SQL.Text = '' then Exit;
    ACheckLists.Clear;

    // Check to see if any lists have already been searched and then exclude these from query
    if AListsSearched.Count <> 0 then begin
      // Modify query's SQL to exclude list already searched
      lslOrigSQL    := TStringList.Create;
      lslOrigSQL.Assign(AQuery.SQL);       // Store original strings
      loOrigParams := TParams.Create;
      loOrigParams.Assign(AQuery.Parameters);
      lsSQL := SQL.Text;                    // Store original SQL
      SQL.Clear;
      for liCount:=0 to (AListsSearched.Count-1) do
        SQL.Add(' AND TL.Taxon_List_Key <> ''' + AListsSearched[liCount] + ''' ');
      Parameters.Assign(loOrigParams);
    end;

    // Execute query to obtain names and keys of Checklists
    try
      Open;
      First;
      while not Eof do begin
        lsListName   := FieldByName('Item_Name').AsString;
        lPrefNameKey := FieldByName('Preferred_Name').AsString;
        lListItemKey := FieldByName('Taxon_List_Item_Key').AsString;

        // If taxon has different name on checklist, find new name
        if lPrefNameKey <> lListItemKey then begin
          lsListName := lsListName + ' ' + ResStr_As + ' <I>' +
                        GetCurrentTaxonName(AQuery.Connection, lPrefNameKey) + '</I>';
          ATaxonNameKeys.Add(lPrefNameKey);
        end;
        // Add details to lists
        ACheckLists.Add(AddTagText('LI', lsListName, ''));
        APreferredNames.Add(lPrefNameKey);
        AListsSearched.Add(FieldByName('Taxon_List_Key').AsString);

        Next;
      end;
    finally
      Close;
    end; // try..finally

    // Replace original SQL and Params in query if these have been changed
    if lsSQL <> '' then begin
      SQL.Clear;
      SQL.Assign(lslOrigSQL);
      lslOrigSQL.Free;
      Parameters.Assign(loOrigParams);
      loOrigParams.Free;
    end;
  end; // with AQuery do
end; // CheckListsFromSource

//-----------------------------------------------------------------------------
function THTMLLines.GetCurrentTaxonName(const AConnection: TADOConnection;
  const ATaxonPrefNameKey: TKeyString): String;
var lOrigItemKey: TKeyString;
    lqryGeneral : TJnccQuery;
    loOrigConn  : TADOConnection;
begin
  loOrigConn   := nil;
  lOrigItemKey := '';
  lqryGeneral := TJnccQuery(FDictData.FindComponent('qryGeneral'));
  if lqryGeneral <> nil then
    with lqryGeneral do begin
      if Active then Close;
      // Change database name, if needed
      if (AConnection <> nil) and (Connection <> AConnection) then begin
        loOrigConn := Connection;
        Connection := AConnection;
      end;
      // Change Taxon Item Key to use, if needed
      if ATaxonPrefNameKey <> '' then begin
        lOrigItemKey := Parameters.ParamByName('Key').Value;
        Parameters.ParamByName('Key').Value := ATaxonPrefNameKey;
      end;
      try
        Open;
        First;
        if not Eof then Result := FieldByName('T_ITEM_NAME').AsString;
      finally
        Close;
      end;
      // Reset query database and item key, if necessary
      if loOrigConn <> nil then Connection := loOrigConn;
      if lOrigItemKey <> '' then Parameters.ParamByName('Key').Value := lOrigItemKey;
    end; 
end;  // GetCurrentTaxonName

//-----------------------------------------------------------------------------
// Taxon
procedure THTMLLines.GetUKNative;
var ltfUKNative : Boolean;
    lqryUKNative: TJnccQuery;
begin
  FslUKNative.Clear;
  lqryUKNative := TJnccQuery(FDictData.FindComponent('qryUKNative'));
  if lqryUKNative <> nil then
    with lqryUKNative do begin
      if SQL.Text = '' then Exit;  // Check to see if we should open this query
      try
        ltfUKNative := False;
        Open;
        if not Eof then ltfUKNative := FieldByName('UKNative').AsBoolean;

        if ltfUKNative then
          FslUKNative.Add(FontStyle(True, False, False, SDictDetails_TaxonUkNative));
      finally
        Close;
      end;
    end;
end;  // GetUKNative

//==============================================================================
function THTMLLines.StandardImagePath : string;
begin
  Result := ExtractFilePath(Application.Exename) + 'Images\';
end;  // StandardImagePath

//==============================================================================
{ Check through output HTML for the #IMAGES tag and put correct path in }
procedure THTMLLines.ScanForImagesTag;
var liIdx: Integer;
begin
  for liIdx := 0 to FslOutputHTML.Count - 1 do begin
    while Pos(IMAGES_TAG, FslOutputHTML[liIdx])<>0 do
      FslOutputHTML[liIdx] := ReplaceImagesFolder(IMAGES_TAG, FslOutputHTML[liIdx]);

    while Pos(LOCAL_IMAGES_TAG, FslOutputHTML[liIdx])<>0 do
      FslOutputHTML[liIdx] := ReplaceImagesFolder(LOCAL_IMAGES_TAG, FslOutputHTML[liIdx]);
  end;
end;  // ScanForImagesTag

//==============================================================================
function THTMLLines.ReplaceImagesFolder(const ATag, ALink: String): String;
var liTagPos   : Integer;
    lsImagePath: String;
begin
  liTagPos := Pos(ATAG, ALink);
  if ATAG = LOCAL_IMAGES_TAG then lsImagePath := AppSettings.LocalImagesPath
                             else lsImagePath := AppSettings.DictImagesPath;
  Result :=Copy(ALink, 1, liTagPos-1) +
           ExcludeTrailingPathDelimiter(lsImagePath) +
           Copy(ALink, liTagPos + Length(ATAG), Length(ALink));
end;  // ReplaceImagesFolder

//==============================================================================
procedure THTMLLines.AnchorSource(const ADetail, ASrcKey, ASectionPrefix: String;
  var ioslSources, ioslMain: TStringList);
var liAncStart               :Integer;
    lstDetail, lstInserted,
    lstFirstBit, lstSecondBit: String;
begin
  lstDetail := ADetail;   // HTML to parse for Anchor points
  if ASrcKey <> '' then begin
    // Find Anchor and set its HREF = ('#' + iSrcKey)
    liAncStart := Pos('<A>', lstDetail);
    if liAncStart > 0 then
      try
        // Add Hyperlink property into HTML Details as we have a valid source anchor
        lstInserted  := ' HREF="#' + ASectionPrefix + ASrcKey + '"';
        lstFirstBit  := Copy(lstDetail, 1, liAncStart + 1);
        lstSecondBit := Copy(lstDetail, liAncStart + 2, Length(lstDetail) - liAncStart - 1);
        lstDetail    := lstFirstBit + lstInserted + lstSecondBit;
      except
        on ESourceNotFound do else raise;
      end;

    // Add source to ioslSources
    ioslSources.Add(AddTagText('CITE',
                               Anchor(ASectionPrefix + ASrcKey, ASrcKey,
                                      dmGeneralData.GetAnySourceText(ASrcKey)),
                               '') + LineBreak);
  end;
  // Add Details to main HTML text
  ioslMain.Add(lstDetail + LineBreak);
end;  // AnchorSource

//==============================================================================
function THTMLLines.HaveString(const AString: String): Boolean;
begin
  Result := (AString <> '');
end;  // HaveString

//==============================================================================
procedure THTMLLines.TidySources(var ioslSources: TStringList);
begin
  ioslSources.Sorted := False; // set sorted property to zero to enable Insert method
end;  // TidySources

{ Description : returns an instance of the NBNSearchSettings class
  Created : 28/11/2002 }
function THTMLLines.NBNSearchSettings: TNBNSearchSettings;
begin
  if not Assigned(mNBNSearchSettings) then begin
    mNBNSearchSettings := TNBNSearchSettings.Create;
  end;
  Result := mNBNSearchSettings;
end;

//==============================================================================
{ TNBNSearchSettings }
//==============================================================================

{ Description : constructor reads the settings table
  Created : 28/11/2002 }
constructor TNBNSearchSettings.Create;
begin
  inherited;
  GetNBNSearchStrings;
end;

(*
  Retrieves a single setting from the settings table for an NBN search parameter.
  Provide a deafult, otherwise HasNBNSearchSettings will be set to false when the parameter
  is missing, disabling the search link.
*)
function TNBNSearchSettings.GetNBNSearchString(setting, default: string): string;
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(SQL_SELECT_SETTING, [setting]);
    Open;
    try
      if EOF then begin
        if default='' then
          HasNBNSearchSettings := False;
        result := default;
      end else
        result := FieldByName('Data').AsString;
    finally
      Close;
    end;
  end;
end;

{ Description : Reads the NBN Search URL settings from the SETTING table
  Created : 28/11/2002 }
procedure TNBNSearchSettings.GetNBNSearchStrings;
begin
  HasNBNSearchSettings := True; // default
  FstNBNSearchURL := GetNBNSearchString('GatewayURL', '');
  FstNBNSearchSeparator := GetNBNSearchString('URLSep', '');
  FProviderName := GetNBNSearchString('GatewayLbl', ResStr_ProviderName);
end;

//==============================================================================
{ Description : If finding that there are no settings available for the nbn
     search, display a message.  This causes the link to be dropped
  Created : 28/11/2002 }
procedure TNBNSearchSettings.SetHasNBNSearchSettings(const Value: Boolean);
begin
  // if changing from true to false
  if not Value and FtfHasNBNSearchSettings then begin
    MessageDlg(ResStr_NoSearchSettings, mtInformation, [mbOk], 0);
  end;
  FtfHasNBNSearchSettings := Value;
end;

end.
