//==============================================================================
//  Unit:        BaseTaxonDictDataUnit
//
//  Implements:  TBaseTaxonDictData
//
//  Description: Contains functions and processes specific to all taxon
//               dictionaries.
//
//  Author:      Eric Salmon
//  Created:     30 Nov 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 39 $                         
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseTaxonDictDataUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseDictionaryDataUnit, JNCCDatasets, RapTree, DataClasses, HierarchyNodes,
  ExceptionForm, ADODB, DatabaseAccessADO, HTTPApp, HTTPProd, StrUtils;

type
  EPropertyNotSet=class(TExceptionPath);

  TBaseTaxonDictData = class(TBaseDictionaryData)
    qryMain: TJNCCQuery;
    qryUKNative: TJNCCQuery;
    qryAssociated: TJNCCQuery;
    qrySynonyms: TJNCCQuery;
    qryFacts: TJNCCQuery;
    qryStatus: TJNCCQuery;
    qryCodes: TJNCCQuery;
    qryCheckLists: TJNCCQuery;
    qryListName: TJNCCQuery;
    qryGeneral: TJNCCQuery;
  private
    FLatestVersion: TKeyString;
    function GetLatestVersion: TKeyString;
  protected
    procedure AdditionalWork(const ANode: TFlyNode); virtual;
    procedure PopulateNodes(const ADataset: TDataset; const AParentNode: TFlyNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    function PopulateTopLevel(const AListKeyData: TKeyData): Boolean; override;
    property LatestVersion: TKeyString read GetLatestVersion;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, GeneralData;

resourcestring
  ResStr_TRapidTreeRequired = ' requires a TListBox or TRapidTree Assigned to the Control2 property.';
  ResStr_NoLatestVersion =  'Latest Version not set.';

{$R *.DFM}

//==============================================================================
constructor TBaseTaxonDictData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Override any previous settings
  FilterClause    := 'AND TLI.Taxon_List_Item_Key IN (%s)';
end;  // Create

//==============================================================================
{ Initialises the top level query to use the current checklist.  Also
     initialises qryChildLevel to do the same, but not the parent key for this
     query }
function TBaseTaxonDictData.PopulateTopLevel(const AListKeyData: TKeyData): Boolean;
var lListVersion: TListVersion;
begin
  Result := true;
  if Tree <> nil then begin
    if AListKeyData.ItemKey <> '' then begin
      lListVersion     := dmGeneralData.SetListVersionKeys(false, AListKeyData.ItemKey);
      FLatestVersion   := lListVersion.LastVersion;
      ListKeys         := lListVersion.LatestVersions;
      TableKeys        :=  AListKeyData.ItemKey;
      if NOT ListIsVirtual( AListKeyData.ItemKey) then
        qryTopLevel.SQL.Text := ppTopLevel.Content
      else
        qryTopLevel.SQL.Text := ppVirtualTopLevel.Content;

      PopulateTree(qryTopLevel, nil);
    end;
  end else
    Raise EUndefinedTarget.Create(ClassName + ResStr_TRapidTreeRequired);
end;  // PopulateTopLevel

//==============================================================================
procedure TBaseTaxonDictData.PopulateNodes(const ADataset: TDataset;
  const AParentNode: TFlyNode);
var lTaxonNodeInfo: TTaxonDictionaryNode;
    loNode        : TFlyNode;
begin
  with ADataset do
    while not Eof do begin
      lTaxonNodeInfo := TTaxonDictionaryNode.Create(nil);
      with lTaxonNodeInfo do begin
        Text              := FieldByName('DisplayField').AsString;
        ItemKey           := FieldByName('ListKey').AsString;
        RankKey           := FieldByName('RankKey').AsString;
        Authority         := FieldByName('Authority').AsString;
        SortCode          := FieldByName('SortCode').AsInteger;
        ChildrenPopulated := not FieldByName('HasChildren').AsBoolean;
        ItemName          := FieldByName('ItemName').AsString;
        SysSupplied       := FieldByName('SystemSupplied').AsBoolean;
        // taxon key not used in dictionary browser, just in editor
        if FieldList.IndexOf('TaxonKey')>-1 then
          TaxonKey          := FieldByName('TaxonKey').AsString;
        TaxonVersionKey   := FieldByName('TaxonVersionKey').AsString;
        PrefNameKey       := FieldByName('PrefNameKey').AsString;
        IsFiltered        := GetNodeFilteredState(PrefNameKey);
        Hint              := GetNodeHint(PrefNameKey);
        // TaxonNames
        TaxonNamesObject  := TTaxonNames.Create;
        TaxonNamesObject.TaxonListItemKey := ItemKey;
        TaxonNamesObject.TaxonName        := ItemName;
        TaxonNamesObject.CommonName       := FieldByName('CommonName').AsString;
        TaxonNamesObject.EnteredName      := '';
        TaxonNamesObject.TNItalic := FieldByName('ItemNameItalic').AsBoolean;
        TaxonNamesObject.CNItalic := FieldByName('CommonItalic').AsBoolean;
        TaxonNamesObject.TNAttribute := FieldByName('ItemNameAttribute').AsString;
        TaxonNamesObject.CNAttribute := FieldByName('CommonNameAttribute').AsString;
        TaxonNamesObject.ENAttribute := '';
        TaxonNamesObject.TNAuthor := FieldByName('ItemNameAuthor').AsString;
        TaxonNamesObject.ENAuthor := '';
      end;

      if Tree <> nil then begin
        if AppSettings.DisplayTaxonCommonNames then  // add node with correct default name to search by
          loNode := Tree.Items.AddChildObject(AParentNode, lTaxonNodeInfo.TaxonNamesObject.CommonName, lTaxonNodeInfo)
        else
          loNode := Tree.Items.AddChildObject(AParentNode, lTaxonNodeInfo.TaxonNamesObject.TaxonName, lTaxonNodeInfo);
        { Sort out images }
        loNode.ImageIndex   := lTaxonNodeInfo.ImageIndex;
        loNode.SelectedIndex:= lTaxonNodeInfo.ImageIndex;
        loNode.StateIndex   := lTaxonNodeInfo.StateImage;

        if not lTaxonNodeInfo.ChildrenPopulated then
          Tree.Items.AddChild(loNode, '.'); // Add a dummy child to enable the + box on the treeview

        AdditionalWork(loNode);
      end;
      Next;
    end; // while
end;  // PopulateNodes

//==============================================================================
procedure TBaseTaxonDictData.AdditionalWork(const ANode: TFlyNode);
begin
  // Do nothing here, but override in descendents
  // Essentially for TaxonDictBrowser and the ListView.
end;  // AddNodeToList

//==============================================================================
function TBaseTaxonDictData.GetLatestVersion: TKeyString;
begin
  if FLatestVersion = '' then
    EPropertyNotSet.Create(ResStr_NoLatestVersion)
  else
    Result := FLatestVersion;
end;  // GetLatestVersion

//==============================================================================
end.

