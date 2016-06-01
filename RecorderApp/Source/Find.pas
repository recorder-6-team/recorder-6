//==============================================================================
//  Unit:        Find
//
//  Implements:  TdlgFind
//
//  Description: Allows user to find records in record sets for a selected field.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 92 $
//    $Date: 19/04/10 15:41 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Find;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, Db,
  ExtCtrls, BaseFormUnit, Finder, DataClasses, OnlineHelp, Constants, Dialogs,
  Messages, ImageListButton, ADODB, DatabaseAccessADO;

type
  TFindType = (ftTaxon, ftBiotope, ftIndividual, ftOrganisation, ftName,
               ftLocation, ftReference, ftSample, ftTaxOcc, ftAdminArea,
               ftNearGridRef, ftFeature, ftNone, ftAdminAreaByType, ftSurvey,
               ftKeyword, ftSurveyTag);

  TdlgFind = class(TForm)
    pnlMain: TPanel;
    bvlFrame: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    lbMatches: TListBox;
    eSearchText: TFinder;
    pnlSearchAbbr: TPanel;
    rgSearchAbbr: TRadioGroup;
    pnlRestrict: TPanel;
    lblTaxonSearches: TLabel;
    cmbTaxonRestriction: TComboBox;
    pnlButtons: TPanel;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    pnlReferenceKeywords: TPanel;
    chkSearchKeywords: TCheckBox;
    btnGo: TButton;
    procedure bbOKClick(Sender: TObject);
    procedure lbMatchesDblClick(Sender: TObject);
    procedure eSearchTextEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure eSearchTextPopulateList(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure eSearchTextPopulateStop(Sender: TObject);
    procedure rgSearchAbbrClick(Sender: TObject);
    procedure cbSearchCurrentListClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WMStartSearch(var Msg: TMessage); message WM_START_SEARCH;
    procedure cmbTaxonRestrictionChange(Sender: TObject);
    procedure chkSearchKeywordsClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure lbMatchesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
  private
    hlpFind  : TOnlineHelp;
    FFindType: TFindType;
    FItemText: string;
    FItemKey : TKeyString;
    FItemAdditional: String;
    FSearchTaxonBy: TTaxonSearchType;
    FCurrentChecklist: string;
    FStoredSearchText : string;
    FAdminTypeKey : TKeyString;
    FSearchForReferencesBy : TReferenceSearchType;
    procedure SetSearchForReferencesBy(const Value: TReferenceSearchType);
    procedure InitializeSearchList;
    procedure SetUpContextHelp;
    procedure FindCurrentChecklist;
    property  SearchForTaxonBy: TTaxonSearchType read FSearchTaxonBy write FSearchTaxonBy;
    property SearchForReferencesBy: TReferenceSearchType
        read FSearchForReferencesBy write SetSearchForReferencesBy;
    function  GetStringForItem( AItemKey: TKeyString; AFullItemName: string ): string;
    procedure SetUpFinder;
    procedure GenericCreate;
    procedure RerunSearch;
  public
    constructor CreateDialog(AOwner:TComponent; const Title:string;
      const AType:TFindType=ftNone); overload;
    constructor CreateDialog(AOwner:TComponent; ChecklistsOption: Boolean;
      const Title: String; const AType: TFindType = ftNone); overload;
    constructor CreateSearchAdminType(AOwner:TComponent; const Title:string; iAdminTypeKey : TKeyString );
    procedure SetSearchText(const AString:string; const ForcePopulate:boolean=false);
    function FindUnique(const AString: String): Boolean;
    function GetKey(const AString:string):TKeyString;
    property ItemText:string read FItemText;
    property ItemKey:TKeyString read FItemKey;
    property ItemAdditional: String read FItemAdditional;
    procedure StoreSearchText ( iTextToStore: string );
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, GeneralData, Locations, LocationDetails, ApplicationSettings,
  BiotopeDictEditor, BaseTaxonDictUnit, Search, AdminAreaDictBrowser,
  FormActions, ProjectSpecificAccess;

//==============================================================================
constructor TdlgFind.CreateDialog(AOwner: TComponent; const Title: string;
  const AType:TFindType=ftNone);
begin
  inherited Create(AOwner);
  Caption  :=Title;
  FFindType:=AType;
  GenericCreate;
  if FFindType = ftTaxon then
    cmbTaxonRestriction.ItemIndex :=
        cmbTaxonRestriction.Items.IndexOf(AppSettings.SessionTaxonomicSearchRestriction);
end;  // CreateDialog

//==============================================================================
constructor TdlgFind.CreateDialog(AOwner:TComponent; ChecklistsOption: Boolean;
      const Title: String; const AType: TFindType = ftNone);
begin
  inherited Create(AOwner);
  Caption   := Title;
  FFindType := AType;
  GenericCreate;
end; // CreateDialog

//==============================================================================
{ Code that is generic to all available constructors }
procedure TdlgFind.GenericCreate;
begin
  SetUpContextHelp;
  SetUpFinder;
  ActiveControl := eSearchText;
  pnlSearchAbbr.Visible        := FFindType = ftTaxon;
  pnlRestrict.Visible          := FFindType = ftTaxon;
  pnlReferenceKeywords.Visible := FFindType = ftReference;
  bvlFrame.Height              := pnlMain.Height - 12;
  lbMatches.Height             := bvlFrame.Height - lbMatches.Top;
  if FFindType = ftTaxon then
  begin
    Width := 550;  // Defaults to wider.
    SearchForTaxonBy := AppSettings.FindTaxonCriterion; // Remember how we searched previously
    { Check for criterion last searched by }
    if SearchForTaxonBy = stAbbreviation then rgSearchAbbr.ItemIndex := 1
                                         else rgSearchAbbr.ItemIndex := 0; // Default to Search by name
    { Set up for searching across checklists }
    if FFindType = ftTaxon then begin
      FindCurrentChecklist;
      dmGeneralData.GetTaxonSearchOptions(cmbTaxonRestriction);
    end;
  end
  else if FFindType = ftReference then
    chkSearchKeywords.Checked := AppSettings.FindReferenceCriterion = stKeyword; // Remember how we searched previously
end;  // GenericCreate

//==============================================================================
procedure TdlgFind.SetSearchText(const AString:string; const ForcePopulate:boolean=false);
begin
  if (AString<>'') or ForcePopulate then begin
    lbMatches.Items.Clear;
    eSearchText.SetSearchText(AString,ForcePopulate);
    FStoredSearchText:='';
  end;
end;  // SetSearchText

//==============================================================================
procedure TdlgFind.eSearchTextEnter(Sender: TObject);
begin
  eSearchText.SelStart:=Length(eSearchText.Text);
end;  // eSearchTextEnter

//==============================================================================
procedure TdlgFind.InitializeSearchList;
var lFormOn           : TForm;
    lTaxonSearchRestriction : string;
begin
  dmSearch.Terminate;
  Application.ProcessMessages; // allow dmSearch to tidy up
  // By default, all searches are done in local database
  dmSearch.SetDatabaseName('LOCAL');
  eSearchText.HandleDuplicate:=sfNone;
  case FFindType of
    ftTaxon :
        begin
          eSearchText.HandleDuplicate:=sfTaxon;
          lTaxonSearchRestriction := AppSettings.TaxonomicSearchRestriction;
          if rgSearchAbbr.ItemIndex = 1 then SearchForTaxonBy:=stAbbreviation
                                        else SearchForTaxonBy:=stName;
          if pnlRestrict.Visible then begin
            if cmbTaxonRestriction.ItemIndex = -1 then
              cmbTaxonRestriction.ItemIndex := cmbTaxonRestriction.Items.IndexOf(lTaxonSearchRestriction);
          end;
          if (frmMain.ActiveMDIChild is TBaseTaxonDict) then
            with TBaseTaxonDict(frmMain.ActiveMDIChild).cmbList do begin
              dmSearch.SetDatabaseName( TKeyData(Items.Objects[ItemIndex]).ItemAdditional );
              dmSearch.InitFromTaxonQuery( eSearchText,
                                           TKeyData(Items.Objects[ItemIndex]).ItemKey,
                                           AppSettings.DisplayTaxonCommonNames,
                                           SearchForTaxonBy,
                                           lTaxonSearchRestriction );
            end
          else begin
            // All other cases, use current Taxon checklist set in TaxonDictionary
            dmSearch.SetDatabaseName( 'LOCAL' );
            try
              dmSearch.InitFromTaxonQuery( eSearchText,
                                           AppSettings.TaxonListKey,
                                           AppSettings.DisplayTaxonCommonNames,
                                           SearchForTaxonBy,
                                           lTaxonSearchRestriction );
            except
              on ENoListVersionFound do ;
                  // ignore -- previously selected list no longer exists (or
                  // was in a different database).
            end;
          end;
        end;
    ftBiotope :
        begin
          // If BiotopeDictDetails has focus, use that form
          lFormOn:=frmMain.GetForm(TfrmBiotopeDictEditor);
          if (lFormOn<>nil) and (frmMain.ActiveMDIChild is TfrmBiotopeDictEditor) then
            with TfrmBiotopeDictEditor(lFormOn).cmbList do
              dmSearch.InitFromBiotopeQuery(eSearchText,TKeyData(Items.Objects[ItemIndex]).ItemKey)
          else
            // All other cases, use current Biotope classification set in BiotopeDictionary
            try
              dmSearch.InitFromBiotopeQuery(
                  eSearchText,
                  AppSettings.BiotopeListKey)
            except
              on ENoListVersionFound do ;
                  // ignore -- previously selected list no longer exists (or
                  // was in a different database).
            end;
        end;
    ftAdminArea :
        begin
          lFormOn:=frmMain.GetForm(TfrmAdminAreaDictBrowser);
          if lFormOn=nil then
            dmSearch.InitFromAdminAreaQuery(eSearchText, AppSettings.AdminAreaListKey)
          else
            with TfrmAdminAreaDictBrowser(lFormOn).cmbList do
              dmSearch.InitFromAdminAreaQuery(eSearchText,TKeyData(Items.Objects[ItemIndex]).ItemKey);
        end;

    ftAdminAreaByType :
        dmSearch.InitFromAdminType(eSearchText, FAdminTypeKey);

    ftLocation :
        begin
          if AppSettings.ExtraLocationSearchColumns = [] then
            eSearchText.HandleDuplicate:=sfLocation;
          dmSearch.InitFromLocationQuery(eSearchText);
        end;

    ftIndividual :
        begin
          eSearchText.HandleDuplicate:=sfName;
          dmsearch.InitFromIndividualQuery(eSearchText);
        end;
    ftOrganisation:
        begin
          eSearchText.HandleDuplicate:=sfName;
          dmsearch.InitFromOrganisationQuery(eSearchText);
        end;
    ftName :
        begin
          eSearchText.HandleDuplicate:=sfName;
          dmsearch.InitFromNameQuery(eSearchText);
        end;
    ftReference   :
        begin
          eSearchText.HandleDuplicate:=sfReference;
          if SearchForReferencesBy=stReference then
            dmSearch.InitFromReferenceQuery(eSearchText)
          else
            dmSearch.InitFromReferenceKeywordQuery(eSearchText)
        end;
    ftSurvey      : dmSearch.InitFromSurveyQuery(eSearchText);
    ftSample      : dmSearch.InitFromSampleQuery(eSearchText);
    ftTaxOcc      : dmSearch.InitFromTaxOccQuery(eSearchText);
    ftFeature     : dmSearch.InitFromFeatureQuery(eSearchText);
    ftKeyword     : dmSearch.InitFromConceptQuery(ACG_KEYWORDS, eSearchText);
    ftSurveyTag   : dmSearch.InitFromConceptQuery(ACG_SURVEY_TAGS, eSearchText);
    ftNearGridRef :
        begin
          lFormOn := frmMain.GetForm(TfrmLocations);
          if lFormOn <> nil then
            if TfrmLocations(lFormOn).DetailForm <> nil then
              if TBaseForm( TfrmLocations(lFormOn).DetailForm ) is TfrmLocationDetails then
              begin
                if AppSettings.ExtraLocationSearchColumns = [] then
                  eSearchText.HandleDuplicate:=sfLocation;
                dmSearch.InitFromLocationList(eSearchText,TfrmLocationDetails(TfrmLocations(lFormOn).detailform).NearGridRefs);
              end;
        end;
  end;
end;  // InitializeSearchList

//==============================================================================
procedure TdlgFind.lbMatchesDblClick(Sender: TObject);
begin
  bbOkClick(nil);
end;  // lbMatchesDblClick

//==============================================================================
procedure TdlgFind.bbOKClick(Sender: TObject);
var
  lFullItemName:string;
begin
  if lbMatches.ItemIndex=-1 then
    ModalResult:=mrNone
  else begin
    dmSearch.Terminate;
    with lbMatches do begin
      if Items.Objects[ItemIndex]<> nil then // trap 'please type 3 chars.. ' message
      begin
        lFullItemName   := Items[ItemIndex];
        FItemKey        := TKeyData(Items.Objects[ItemIndex]).ItemKey;
        FItemText       := GetStringForItem(FItemKey, lFullItemName);
        FItemAdditional := TKeyData(Items.Objects[ItemIndex]).ItemAdditional;
        ModalResult     := mrOk;
      end else
        ModalResult := mrNone; // don't close dialog
    end;
  end;
end;  // bbOkClick

//==============================================================================
function TdlgFind.GetKey(const AString:string): TKeyString;
var iIndex:integer;
begin
  Result:='';
  iIndex:=lbMatches.Items.IndexOf(AString);
  if iIndex<>-1 then Result := TKeyData(lbMatches.Items.Objects[iIndex]).ItemKey;
end;  // GetKey

//==============================================================================
procedure TdlgFind.eSearchTextPopulateList(Sender: TObject);
begin
  InitializeSearchList;
end;  // eSearchTextPopulateList

//==============================================================================
procedure TdlgFind.bbCancelClick(Sender: TObject);
begin
  dmSearch.Terminate;
end;  // bbCancelClick

//==============================================================================
procedure TdlgFind.eSearchTextPopulateStop(Sender: TObject);
begin
  dmSearch.Terminate;
end;  // eSearchTextPopulateStop

//==============================================================================
procedure TdlgFind.rgSearchAbbrClick(Sender: TObject);
begin
  if rgSearchAbbr.ItemIndex = 1 then SearchForTaxonBy:=stAbbreviation
                                else SearchForTaxonBy:=stName;
  RerunSearch;
end; // rgSearchAbbrClick

//==============================================================================
{ Constructor that allows us to search admin areas within a specified type }
constructor TdlgFind.CreateSearchAdminType(AOwner: TComponent;
  const Title: string; iAdminTypeKey: TKeyString);
begin
  inherited Create(AOwner);
  Caption  :=Title;
  FFindType:=ftAdminAreaByType;
  FAdminTypeKey := iAdminTypeKey;
  GenericCreate;
end;

//==============================================================================
procedure TdlgFind.SetUpContextHelp;
begin
  //Help Setup
  hlpFind := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpFind.OnHelpReplacement;
  case FFindType of
    ftTaxon       : Self.HelpContext:=IDH_FINDTAXON;
    ftBiotope     : Self.HelpContext:=IDH_FINDBIOTOPE;
    ftAdminArea   : Self.HelpContext:=IDH_FINDADMINAREA;
    ftLocation    : Self.HelpContext:=IDH_FINDLOCATION;
    ftIndividual  : Self.HelpContext:=IDH_FINDINDIVIDUAL;
    ftOrganisation: Self.HelpContext:=IDH_FINDORGANISATION;
    ftName        : Self.HelpContext:=IDH_FINDNAME;
    ftReference   : Self.HelpContext:=IDH_FINDREF;
    ftSample      : Self.HelpContext:=IDH_FINDSAMPLE;
    ftTaxOcc      : Self.HelpContext:=IDH_FINDTAXOCC;
    ftFeature     : Self.HelpContext:=IDH_FINDFEATURE;
    ftKeyword     : Self.HelpContext:=IDH_FINDKEYWORD;
    ftNearGridRef : Self.HelpContext:=IDH_FINDNEARGRIDREF;
    ftNone        : Self.HelpContext:=IDH_FIND;
  end;
  // if no specific help, use general help file
  if Self.HelpContext=0 then Self.HelpContext:=IDH_FIND;
end; // SetUpContextHelp

//==============================================================================
procedure TdlgFind.cbSearchCurrentListClick(Sender: TObject);
var
  lSearchString: string;
begin
  lbMatches.Clear;
  lSearchString:=eSearchText.Text;
  eSearchText.Clear;
  SetSearchText(lSearchString,True); // Start new search
end; // cbSearchCurrentListClick

//==============================================================================
procedure TdlgFind.FindCurrentChecklist;
begin
  if AppSettings.TaxonListKey<>'' then begin
    with dmGeneralData.qryAllPurpose do begin
      if Active then Close;
      SQL.Clear;
      SQL.Add(' SELECT Item_Name ' +
              ' FROM Taxon_List ' +
              ' WHERE Taxon_List_Key = '''+ AppSettings.TaxonListKey + ''' ');
      try
        Open;
        First;
        FCurrentChecklist:=FieldByName('Item_Name').AsString;
      finally
        Close;
      end;
    end;
  end else FCurrentChecklist:='Checklist not set.';
end; // FindCurrentChecklist

//==============================================================================
procedure TdlgFind.FormActivate(Sender: TObject);
begin
  if FStoredSearchText<>'' then
    PostMessage(Handle,WM_START_SEARCH,0,0);
end;

//==============================================================================
procedure TdlgFind.FormDestroy(Sender: TObject);
begin
  hlpFind.Free;
  if FFindType = ftTaxon then begin
    // Remember how we searched last
    AppSettings.FindTaxonCriterion := SearchForTaxonBy;
    if pnlRestrict.Visible then
      AppSettings.SessionTaxonomicSearchRestriction :=
          cmbTaxonRestriction.Items[cmbTaxonRestriction.ItemIndex];
  end;
  if FFindType = ftReference then
    AppSettings.FindReferenceCriterion:=SearchForReferencesBy;
  AppSettings.ReferencesSearchKeywords := chkSearchKeywords.Checked;
end;  // FormDestroy

{-------------------------------------------------------------------------------
}
procedure TdlgFind.FormResize(Sender: TObject);
begin
  lbMatches.Refresh;
end;

//==============================================================================
procedure TdlgFind.StoreSearchText(iTextToStore: string);
begin
  FStoredSearchText:=iTextToStore;
 end;

//==============================================================================
function TdlgFind.GetStringForItem(AItemKey: TKeyString; AFullItemName: string): string;

  function GetLocationName: String;
  var
    lStringKey: string;
  begin
    lStringKey := AItemKey; // translate as next line has var parameter of diff type
    Result := dmGeneralData.GetLocationNameFromLocNameKey(lStringKey);
  end;
  
begin
  Case eSearchText.HandleDuplicate of
    sfTaxon:     Result:=dmGeneralData.GetTaxonName(AItemKey);
    sfReference: Result:=dmGeneralData.GetReferenceText(AItemKey);
    sfName:      Result:=dmGeneralData.GetName(AItemKey);
    sfLocation:  Result := GetLocationName;
  else if (FFindType = ftLocation)
      and (AppSettings.ExtraLocationSearchColumns <> []) then
    Result := GetLocationName
  else
    Result:=AFullItemName;
  end;
end;

//==============================================================================
procedure TdlgFind.WMStartSearch(var Msg: TMessage);
begin
  if FStoredSearchText<>'' then begin
    SetSearchText(FStoredSearchText);
    if eSearchText.NoSourceItems then
      MessageDlg(ResStr_TaxonItems, mtInformation, [mbOK], 0);
  end;
end;

//==============================================================================
procedure TdlgFind.SetUpFinder;
begin
  { Set Database as Local }
  eSearchText.HandleDuplicate := sfNone;
  eSearchText.SetDatabase(dmDatabase.LocalDatabase);
  eSearchText.PartialTaxonSearch := (FFindType = ftTaxon)
    and AppSettings.PartialTaxonSearch;
end; // SetUpFinder

//==============================================================================
{ Don't allow header for list of rucksacks to be selected }
procedure TdlgFind.cmbTaxonRestrictionChange(Sender: TObject);
begin
  if cmbTaxonRestriction.ItemIndex = 3 then
    cmbTaxonRestriction.ItemIndex := 0;
  if cmbTaxonRestriction.ItemIndex > 3 then begin
    eSearchText.MinChars := RUCKSACK_SEARCH_CODE_MIN_CHARS;
  end else begin
    eSearchText.MinChars := MIN_CHARS;
  end;    // if cmbTaxonRestriction.ItemIndex > 3
  eSearchText.SetSearchText(eSearchText.Text); // force refresh
end;

//==============================================================================
function TdlgFind.FindUnique(const AString: String): Boolean;
begin
  Result := False;
  // Forces the search
  eSearchText.SetSearchText(AString, true);
  // See if there is a single result
  with lbMatches do
    if (Items.Count = 1) and (Items.Objects[0] <> nil) then begin
      FItemKey  := TKeyData(Items.Objects[0]).ItemKey;
      FItemText := GetStringForItem(FItemKey, Items[0]);
      Result    := True;
    end;
end;  // FindUnique

{-------------------------------------------------------------------------------
  When changing the reference search option, rerun the search
}
procedure TdlgFind.chkSearchKeywordsClick(Sender: TObject);
begin
  if chkSearchKeywords.Checked then
    SearchForReferencesBy:=stKeyword
  else
    SearchForReferencesBy:=stReference;
  RerunSearch;
end;

{-------------------------------------------------------------------------------
  Rerun the current search
}
procedure TdlgFind.RerunSearch;
var
  lSearchString: string;
begin
  lbMatches.Clear;
  lSearchString:=eSearchText.Text;
  eSearchText.Clear;
  SetSearchText(lSearchString);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgFind.SetSearchForReferencesBy(const Value: TReferenceSearchType);
begin
  FSearchForReferencesBy := Value;
  btnGo.Visible := Value = stKeyword;
  if btnGo.Visible then begin
    eSearchText.SearchMode := smOnRequest;
    btnGo.Left := lbMatches.Left + lbMatches.Width - btnGo.Width;
    eSearchText.Width := btnGo.Left - eSearchText.Left;
  end
  else begin
    eSearchText.Width := lbMatches.Width;
    eSearchText.SearchMode := smMinChars;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgFind.btnGoClick(Sender: TObject);
begin
  eSearchText.ForcePopulate;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgFind.lbMatchesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  dmInterface.DrawTerm(lbMatches.Canvas, Rect, lbMatches.Items[Index], odSelected in State);
end;

end.

