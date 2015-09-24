//==============================================================================
//  Unit:        TaxonDictBrowserData
//
//  Implements:  TdmTaxonDictBrowser
//
//  Description: Contains taxon dictionary editor specific functions to
//               access and format data.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//
//  Last Revision Details:
//    $Revision: 20 $
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TaxonDictBrowserData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseTaxonDictDataUnit, Db, ADODB, DatabaseAccessADO, JNCCDatasets, ComCtrls,
  RapTree, HierarchyNodes, DataClasses, DictionaryHTMLDetails, HtmlView,
  HTTPApp, HTTPProd;

type
  TdmTaxonDictBrowser = class(TBaseTaxonDictData)
    qryLocalLists: TJNCCQuery;
    dsLocalLists: TDataSource;
  private
    FListView: TListView;
    FslTaxonHTML: TStringList;
    FslSelectedItemKeys: TStringList;
    FAdditionalListAndName: String;
    FSelectedTaxonDictNode: TTaxonDictionaryNode;
    function HasFilteredChildNodes: Boolean;
  protected
    procedure AdditionalWork(const ANode: TFlyNode); override;
    function GetFilteredKeys: String; override;
    function GetNodeFilteredState(const key: String): Boolean; override;
    function GetNodeHint(const key: String): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MarkListAsLocal(const AListKey: TKeyString);
    procedure SetHTMLDetails(const ADictNode: THTMLDictionaryNode;
      const AListKey: TKeyString; const AHTMLViewer: THTMLViewer);
    procedure SetParamsForHTMLDetails(Sender: TObject; const ADictNode: THTMLDictionaryNode);
    procedure ShowCheckListDetails(const AListAndTaxonKeys: String; const AHTMLViewer: THTMLViewer);
    property ListView: TListView read FListView write FListView;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, Constants;
  
//==============================================================================
constructor TdmTaxonDictBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FslTaxonHTML := TStringList.Create;
  FslSelectedItemKeys := TStringList.Create;
  FilterClause    := 'AND Taxon_List_Item_Key IN (%s)';  
end;  // Create;

//==============================================================================
destructor TdmTaxondictBrowser.Destroy;
begin
  FslTaxonHTML.Free;
  FslTaxonHTML := nil;
  FslSelectedItemKeys.Free;
  FslSelectedItemKeys := nil;
  inherited Destroy;
end;  // Destroy;

//==============================================================================
procedure TdmTaxonDictBrowser.AdditionalWork(const ANode: TFlyNode);
var loListItem: TListItem;
    loData    : TTaxonDictionaryNode;
begin
  // Add Items to list view as they are added to the tree.
  if ListView <> nil then begin
    if ANode.Data <> nil then begin
      loData := TTaxonDictionaryNode(ANode.Data);
      loListItem := ListView.Items.Add;
      with loListItem do begin
        Data       := loData;
        Caption    := loData.Title;
        ImageIndex := loData.ImageIndex;
        StateIndex := loData.StateImage;
        SubItems.Add(loData.TaxonNamesObject.CommonName);
      end;
    end;
  end;
end;  // AdditionalWork

//==============================================================================
function TdmTaxonDictBrowser.GetNodeFilteredState(const key: String): Boolean;
begin
  Result := AppSettings.IndexOfFilteredRecord(TN_TAXON_LIST_ITEM, key) > -1;
end;

//==============================================================================
function TdmTaxonDictBrowser.GetNodeHint(const key: String): String;
begin
  Result := AppSettings.GetFilteredRecordHint(TN_TAXON_LIST_ITEM, key);
end;

//==============================================================================
function TdmTaxonDictBrowser.GetFilteredKeys: String;
begin
  if (FilterKeys <> '') and ((NodeToExpand = nil) or HasFilteredChildNodes) then
    Result := FilterKeys
  else
    Result := '';
end;

//==============================================================================
function TdmTaxonDictBrowser.HasFilteredChildNodes: Boolean;
var
  rs: _Recordset;
begin
  Result := False;

  rs := dmDatabase.ExecuteSQL(
      'SELECT Taxon_List_Item_Key FROM Taxon_List_Item WHERE Parent = '''
      + TNodeObject(NodeToExpand.Data).ItemKey + '''', True);
  try
    while not rs.Eof do begin
      if Pos(rs.Fields['Taxon_List_Item_Key'].Value, FilterKeys) > 0 then begin
        Result := True;
        Exit;
      end;
      rs.MoveNext;
    end;
  finally
    rs.Close;
  end;
end;

//==============================================================================
procedure TdmTaxonDictBrowser.SetHTMLDetails(const ADictNode: THTMLDictionaryNode;
  const AListKey: TKeyString; const AHTMLViewer: THTMLViewer);
//var loHTMLLines: THTMLLines;
begin
  // Focus on different node, clear previous selections
  FslSelectedItemKeys.Clear;
  FAdditionalListAndName := '';
  FSelectedTaxonDictNode := TTaxonDictionaryNode(ADictNode);  // Store ref to selected node
  FslTaxonHTML.Clear;           // Reset HTML details for taxon
  // Get HTML data
  with THTMLLInes.Create(AListKey, ADictNode, Self, SetParamsForHTMLDetails) do
    try
      NBNLinkName := TTaxonDictionaryNode(ADictNode).ItemName;
      ShowCheckLists := true;
      ShowCodes      := true;
      ShowSynonyms   := true;
      ShowUKNative   := true;
      FslTaxonHTML.Assign(CreateHTML(FslSelectedItemKeys));  // Store new details
      if not (csDestroying in ComponentState) then // safety check
        AHTMLViewer.LoadStrings(FslTaxonHTML);
    finally
      Free;
    end;
end;  // SetHTMLDetails

//==============================================================================
procedure TdmTaxonDictBrowser.SetParamsForHTMLDetails(Sender: TObject;
  const ADictNode: THTMLDictionaryNode);
var
  lListVersion : TListVersion;
begin
  // find latest version of current list
  lListVersion := dmGeneralData.SetListVersionKeys(False, THTMLLines(Sender).ListKey);
  with TTaxonDictionaryNode(ADictNode) do begin
    qryGeneral.Parameters.ParamByName('Key').Value             := ItemKey;
    qryMain.Parameters.ParamByName('Key').Value                := ItemKey;
    qryUKNative.Parameters.ParamByName('Key').Value            := ItemKey;
    qryAssociated.Parameters.ParamByName('Key').Value          := ItemKey;
    qrySynonyms.Parameters.ParamByName('PrefNameKey').Value    := PrefNameKey;
    qryFacts.Parameters.ParamByName('Key').Value               := TaxonVersionKey;
    qryStatus.Parameters.ParamByName('Key').Value              := ItemKey;
    qryCodes.Parameters.ParamByName('TaxonKey').Value          := TaxonVersionKey;
    qryCodes.Parameters.ParamByName('ListKey').Value           := THTMLLines(Sender).ListKey;
    qryCheckLists.Parameters.ParamByName('ItemKey').Value      := ItemKey;
    qryListName.Parameters.ParamByName('ListKey').Value        := THTMLLines(Sender).ListKey;
    qryListName.Parameters.ParamByName('ItemKey').Value        := ItemKey;
  end;
end;  // SetParamsForHTMLDetails

//==============================================================================
procedure TdmTaxonDictBrowser.ShowCheckListDetails(const AListAndTaxonKeys: String;
  const AHTMLViewer: THTMLViewer);
var lHTMLLines                 : THTMLLines;
    lCheckListKey, lPrefNameKey: TKeyString;
    lslLines, lslViewHTML      : TStringList;
begin
  if FAdditionalListAndName <> AListAndTaxonKeys then // If details not already present
  begin
    { Split combined keys into Checklist key and Preferred Name key }
    lCheckListKey := Copy(AListAndTaxonKeys,  1, 16);
    lPrefNameKey  := Copy(AListAndTaxonKeys, 17, 16);

    FSelectedTaxonDictNode.PrefNameKey := lPrefNameKey;
    FSelectedTaxonDictNode.ItemKey := lPrefNameKey;
    lHTMLLines := THTMLLines.Create(lCheckListKey, FSelectedTaxonDictNode, Self, SetParamsForHTMLDetails);
    lslViewHTML := TStringList.Create;
    try
      FAdditionalListAndName := AListAndTaxonKeys; // Remember CheckList and Taxon
      { Create HTML for additional details }
      if FslSelectedItemKeys.IndexOf(lPrefNameKey)=-1 then
        lslLines := lHTMLLines.CreateMoreTaxonHTML(true) // Taxon has same name on other checklist
      else
        lslLines := lHTMLLines.CreateMoreTaxonHTML(false); // Taxon has different name

      { Build HTML to display in viewer }
      lslViewHTML.Assign(FslTaxonHTML);
      lslViewHTML.AddStrings(lslLines);

      AHTMLViewer.Clear;
      AHTMLViewer.LoadStrings(lslViewHTML);
    finally
      lHTMLLines.Free;
      lslViewHTML.Free;
    end;
  end;
  AHTMLViewer.PositionTo(DEST_LIST_DENOTE_STRING + AListAndTaxonKeys); // Move to text in HTML Viewer
end;  // ShowCheckListDetails

//==============================================================================
procedure TdmTaxonDictBrowser.MarkListAsLocal(const AListKey: TKeyString);
var lqryUpdate: TJNCCQuery;
begin
  lqryUpdate := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryUpdate]);
    lqryUpdate.SQL.Add('UPDATE TAXON_LIST SET LOCAL_DISK=True');
    lqryUpdate.SQL.Add('WHERE TAXON_LIST_KEY=''' + AListKey + '''');
    lqryUpdate.ExecSQL;
  finally
    lqryUpdate.Free;
  end; // try
end;

//==============================================================================
end.
