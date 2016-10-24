//==============================================================================
//  Unit:        BaseTaxonDictUnit
//
//  Implements:  TBaseTaxonDict
//
//  Description: Base class for all taxon dictionaries, browser and editor.
//               Implements some abstract functions declared in TBaseDictionary
//               class, including the dictionary items comparison function which
//               is specific to each dictionary type, and adds menus common to
//               taxon dictionaries.
//
//  Author:      Eric Salmon
//  Created:     30 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 29 $
//    $Date: 19/03/10 12:50 $
//    $Author: Robertjohnson $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseTaxonDictUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseDictionaryUnit, exgrid, RapTree, ComCtrls, StdCtrls, ExtCtrls,
  BaseTaxonDictDataUnit, DataClasses, HierarchyNodes, Constants, Menus,
  ActnList, KeyboardRapidTree,strUtils;

const
  DEFAULT_SORT     = 'Default';
  SCI_NAME_SORT    = 'ScientificName';
  AUTHORITY_SORT   = 'Authority';
  COMMON_NAME_SORT = 'CommonName';

type
  TBaseTaxonDict = class(TBaseDictionary)
    alTaxonDict: TActionList;
    actSortDefault: TAction;
    actSortSciName: TAction;
    actSortAuthority: TAction;
    actSortComName: TAction;
    actFind: TAction;
    actFilter: TAction;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    N1: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditSort: TMenuItem;
    mnuEditSortDefault: TMenuItem;
    mnuEditSortTaxonName: TMenuItem;
    mnuEditSortAuthority: TMenuItem;
    mnuEditSortCommonName: TMenuItem;
    mnuEditFilter: TMenuItem;
    pmSort: TPopupMenu;
    pmSortDefault: TMenuItem;
    pmSortName: TMenuItem;
    pmSortAuthority: TMenuItem;
    pmSortCommonName: TMenuItem;
    pmHierarchy: TPopupMenu;
    pmHSortBy: TMenuItem;
    pmHSortDefault: TMenuItem;
    pmHSortSciName: TMenuItem;
    pmHSortAuthority: TMenuItem;
    pmHSortComName: TMenuItem;
    procedure tvDictionaryDblClick(Sender: TObject);
    procedure actFindExecute(Sender: TObject); overload;
    procedure actSortDefaultExecute(Sender: TObject);
    procedure actSortSciNameExecute(Sender: TObject);
    procedure actSortAuthorityExecute(Sender: TObject);
    procedure actSortComNameExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure tvDictionaryDrawCell(Sender: TObject; aCanvas: TCanvas; ACol,
      ARow: Integer; Rect: TRect; State: TExGridDrawState);
  protected
    function CompareDictionaryItems(const Data1, Data2: TDictionaryNode; const ASortOrder: String): Integer; override;
    procedure SetupObjects; override;
    function GetListForItem(const AKey: string): string; override;
    function DefaultSortField: string; override;
    function SortRegistryName: string; override;
    function SortNameToSortSQL(const ASortName: string): string; override;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    procedure LocateDictionaryItem(const AItemKey: TKeyString; const AMsgText: String); override;
    procedure actFindExecute(Sender: TObject; ASearchString: String); overload;

  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, FormActions, Maintbar, Find, Rucksack,
  DatabaseAccessADO, Variants;

resourcestring
  ResStr_Taxa = 'taxa';

//==============================================================================
// May be overridden in descendent classes
procedure TBaseTaxonDict.SetupObjects;
begin
  DictionaryData := TBaseTaxonDictData.Create(nil);
end;  // SetupObjects

//==============================================================================
procedure TBaseTaxonDict.tvDictionaryDblClick(Sender: TObject);
var loForm: TForm;
begin
  inherited;
  // safety check in case filter has returned no items
  if Assigned(tvDictionary.Selected) then
    with tvDictionary.Selected do
      if not HasChildren then begin
        loForm := frmMain.GetForm(TfrmRucksack);
        if loForm <> nil then
          TfrmRucksack(loForm).AddTaxonToRucksack(TDictionaryNode(Data).ItemKey);
      end;
end;  // tvDictionaryDblClick

//==============================================================================
procedure TBaseTaxonDict.tvDictionaryDrawCell(Sender: TObject;
  aCanvas: TCanvas; ACol, ARow: Integer; Rect: TRect;
  State: TExGridDrawState);
var loNode    : TFlyNode;
    loDictNode: TTaxonDictionaryNode;
    lrRect    : TRect;
begin
  inherited;
  loNode := tvDictionary.GetNodeAtRow(ARow);
  if Assigned(loNode) then
    if Assigned(loNode.Data) then begin
      loDictNode := TTaxonDictionaryNode(loNode.Data);
      if Assigned(loDictNode.TaxonNamesObject) then begin
        lrRect := Rect;
        lrRect.Left := lrRect.Left + (loNode.Level + 2) * tvDictionary.Indent + 4; //line up with original text
        lrRect.Top := lrRect.Top + 1;
        GenericDrawTaxonNames(aCanvas, loDictNode.TaxonNamesObject, lrRect, false);
      end;
    end;
end;  // tvDictionaryDrawCell

//==============================================================================
procedure TBaseTaxonDict.actFindExecute(Sender: TObject);
var
  lOrigTaxonSearchRestriction: string;
begin
  inherited;
  // Force the taxon search restriction to current checklist, then reset it
  lOrigTaxonSearchRestriction := AppSettings.TaxonomicSearchRestriction;
  AppSettings.TaxonomicSearchRestriction := ResStr_CurrentCheckList;
  try
    LocateDictionaryItem(FindDictionaryItem('', ResStr_FindTaxon, ftTaxon), 'taxon');
  finally
    AppSettings.TaxonomicSearchRestriction := lOrigTaxonSearchRestriction;
  end;
end;  // actFindExecute

//==============================================================================
procedure TBaseTaxonDict.actFindExecute(Sender: TObject; ASearchString: String);
begin
  inherited;
  LocateDictionaryItem(FindDictionaryItem('', ResStr_FindTaxon, ftTaxon, ASearchString), 'taxon');
end;  // actFindExecute

//==============================================================================
//==============================================================================
//==============================================================================
procedure TBaseTaxonDict.actSortDefaultExecute(Sender: TObject);
begin
  SortOrder := DEFAULT_SORT;
end;  // actSortDefaultExecute

//==============================================================================
procedure TBaseTaxonDict.actSortSciNameExecute(Sender: TObject);
begin
  SortOrder := SCI_NAME_SORT;
end;  // actSortSciNameExecute

//==============================================================================
procedure TBaseTaxonDict.actSortAuthorityExecute(Sender: TObject);
begin
  SortOrder := AUTHORITY_SORT;
end;  // actSortAuthorityExecute

//==============================================================================
procedure TBaseTaxonDict.actSortComNameExecute(Sender: TObject);
begin
  SortOrder := COMMON_NAME_SORT;
end;  // actSortComNameExecute

//==============================================================================
function TBaseTaxonDict.CompareDictionaryItems(const Data1, Data2: TDictionaryNode;
  const ASortOrder: String): Integer;
var lItem1, lItem2: TTaxonDictionaryNode;

    function DoCompare(AValue1, AValue2: integer; A2ndValue1, A2ndValue2: string): integer; overload;
    begin
      if AValue1 < AValue2 then
        Result := -1
      else if AValue1 > AValue2 then
        Result :=  1
      else
        Result :=  CompareText(A2ndValue1, A2ndValue2);
    end;

    function DoCompare(AValue1, AValue2: string; A2ndValue1, A2ndValue2: integer): integer; overload;
    begin
      Result := CompareText(AValue1, AValue2);
      if Result=0 then begin
        if A2ndValue1 < A2ndValue2 then
          Result := -1
        else if A2ndValue1 > A2ndValue2 then
          Result :=  1
        else
          Result :=  0;
      end;
    end;

begin
  Result := 0;
  if Assigned(Data1) and Assigned(Data2) then begin
    lItem1 := TTaxonDictionaryNode(Data1);
    lItem2 := TTaxonDictionaryNode(Data2);

    if ASortOrder = DEFAULT_SORT then
      Result := DoCompare(
             lItem1.SortCode, lItem2.SortCode,
             lItem1.ItemName, lItem2.ItemName)
    else if ASortOrder = SCI_NAME_SORT then
      Result := DoCompare(
             lItem1.ItemName, lItem2.ItemName,
             lItem1.SortCode, lItem2.SortCode)
    else if ASortOrder = AUTHORITY_SORT then
      Result := DoCompare(
             lItem1.Authority, lItem2.Authority,
             lItem1.SortCode, lItem2.SortCode)
    else   // COMMON_NAME_SORT
      Result := DoCompare(
             lItem1.CommonName, lItem2.CommonName,
             lItem1.SortCode, lItem2.SortCode)
  end;
end;  // CompareDictionaryItems

//==============================================================================
procedure TBaseTaxonDict.actFilterExecute(Sender: TObject);
var lListVersion: TListVersion;
begin
  inherited;
  if DictionaryData.ListIsVirtual(ListKeyData.ItemKey) then begin
    FilterDictionary('Taxon', ResStr_Taxa, 'AND Taxon_List_Version_Key = Taxon_List_Version_Key');
  end else begin
    lListVersion := dmGeneralData.SetListVersionKeys(false, ListKeyData.ItemKey);
    FilterDictionary('Taxon', ResStr_Taxa, 'AND Taxon_List_Version_Key = ''' + lListVersion.LastVersion + '''');
  end;

end;  // actFilterExecute

//==============================================================================
{ Override LocateDictionaryItem to ensure user is searching for preferred
     name }
procedure TBaseTaxonDict.LocateDictionaryItem(const AItemKey: TKeyString;
  const AMsgText: String);
var
  lPreferred : TKeyString;
  lChecklist : TKeyString;
begin
  if AItemKey <> '' then begin
    if not DictionaryData.ListIsVirtual(ListKeyData.ItemKey) then begin
      lPreferred := dmGeneralData.GetTaxonPreferredKey( AItemKey );
      lChecklist := dmGeneralData.GetTaxonChecklistKey( AItemKey );
    end else begin
      lPreferred := AITemKey;
      lChecklist := ListKeyData.ItemKey;
    end;

    SelectList(lChecklist);
    inherited LocateDictionaryItem( lPreferred, AMsgText );

  end;
end;  // LocateDictionaryItem

{-------------------------------------------------------------------------------
  Retrieve the taxon list for a taxon
}
function TBaseTaxonDict.GetListForItem(const AKey: string): string;
const
  SQL_LIST_FOR_TAXON = 'SELECT TLV.Taxon_List_Key FROM Taxon_List_Version TLV '+
      'INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Version_Key=TLV.Taxon_List_Version_Key '+
      'WHERE TLI.Taxon_List_Item_Key=''%s''';
begin
  with dmDatabase.ExecuteSQL(Format(SQL_LIST_FOR_TAXON, [AKey]), True) do
    if not (EOF and BOF) then
      Result := VarToStr(Fields[0].Value)
    else
      Result := '';
end;

{-------------------------------------------------------------------------------
  Retrieve the field used for a default sort
}
function TBaseTaxonDict.DefaultSortField: string;
begin
  Result := DEFAULT_SORT;
end;

{-------------------------------------------------------------------------------
  Convert a sort field name to the actual sort SQL used
}
function TBaseTaxonDict.SortNameToSortSQL(const ASortName: string): string;
begin
  if ASortName = SCI_NAME_SORT then
    Result := 'ORDER BY ITN.Preferred_Name, TLI.Sort_Code'
  else if ASortName = AUTHORITY_SORT then
    Result := 'ORDER BY ITN.Authority, TLI.Sort_Code'
  else if ASortName = COMMON_NAME_SORT then
    Result := 'ORDER BY ITN.Common_Name, TLI.Sort_Code'
  else
    Result := 'ORDER BY TLI.Sort_Code, ITN.Preferred_Name';
end;

{-------------------------------------------------------------------------------
  Retrieve the name of the sort key in the registry
}
function TBaseTaxonDict.SortRegistryName: string;
begin
  Result := 'Taxon';
end;

constructor TBaseTaxonDict.Create(Owner: TComponent);
begin
  InitialListKey := AppSettings.TaxonListKey;
  TableName := 'TAXON_LIST_ITEM';
  inherited;
  // Just to make sure the images are there!
  tvDictionary.Images := dmFormActions.ilTaxon;
end;

end.
