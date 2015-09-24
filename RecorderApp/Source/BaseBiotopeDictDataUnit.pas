//==============================================================================
//  Unit:        BaseBiotopeDictDataUnit
//
//  Implements:  TBaseBiotopeDictData
//
//  Description: Contains functions and processes specific to all biotope
//               dictionaries.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 14 $
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseBiotopeDictDataUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseDictionaryDataUnit, Db, JNCCDatasets, DataClasses, HierarchyNodes,
  RapTree, ExceptionForm, ADODB, HTTPApp, HTTPProd;

type
  EPropertyNotSet = class(TExceptionPath);

  TBaseBiotopeDictData = class(TBaseDictionaryData)
    qryStatus: TJNCCQuery;
    qryFacts: TJNCCQuery;
    qryAssociated: TJNCCQuery;
    qryMain: TJNCCQuery;
    qryGeneral: TJNCCQuery;
  private
    { Private declarations }
    FLatestVersion: TKeyString;
    function GetLatestVersion: TKeyString;
  protected
    procedure PopulateNodes(const ADataset: TDataset; const AParentNode: TFlyNode); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function PopulateTopLevel(const AListKeyData: TKeyData): Boolean; override;
    property LatestVersion: TKeyString read GetLatestVersion;
  end;

//==============================================================================
implementation

uses
  GeneralData;

resourcestring
  ResStr_RequiresTListBox = ' requires a TListBox or TRapidTree. Assigned to the Control2 property.';
  ResStr_LatestVersionNotSet = 'Latest Version not set.';

{$R *.DFM}

//==============================================================================
constructor TBaseBiotopeDictData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FilterClause := 'AND BLI.Biotope_List_Item_Key IN (%s)';
end;  // Create

//==============================================================================
function TBaseBiotopeDictData.PopulateTopLevel(const AListKeyData: TKeyData): Boolean;
var lListVersion: TListVersion;
begin
  Result := true;
  if Tree <> nil then begin
    if AListKeyData.ItemKey <> '' then begin
      lListVersion     := dmGeneralData.SetListVersionKeys(true, AListKeyData.ItemKey);
      FLatestVersion   := lListVersion.LastVersion;
      ListKeys         := lListVersion.LatestVersions;
      qryTopLevel.SQL.Text := ppTopLevel.Content;
      PopulateTree(qryTopLevel, nil);
    end;
  end else
    Raise EUndefinedTarget.Create(ClassName + ResStr_RequiresTListBox);
end;  // PopulateTopLevel

//==============================================================================
procedure TBaseBiotopeDictData.PopulateNodes(const ADataset: TDataset;
  const AParentNode: TFlyNode);
var lsItem      : String;
    lBioNodeInfo: TBiotopeDictionaryNode;
    loNode      : TFlyNode;
begin
  with ADataSet do
    while not Eof do begin
      lBioNodeInfo := TBiotopeDictionaryNode.Create;
      with lBioNodeInfo do begin
        Text              := FieldByName('DisplayField').AsString;
        ItemKey           := FieldByName('ListKey').AsString;
        ChildrenPopulated := FieldByName('ChildrenCount').AsInteger = 0;
        SortCode          := FieldByName('SortCode').AsInteger;
        LongName          := FieldByName('LongName').AsString;
        ShortTerm         := FieldByName('ShortTerm').AsString;
        OriginalCode      := FieldByName('OriginalCode').AsString;
        SysSupplied       := FieldByName('SystemSupplied').AsBoolean;
        BiotopeKey        := FieldByName('BiotopeKey').AsString;
        IsFiltered        := GetNodeFilteredState(ItemKey);
        Hint              := GetNodeHint(ItemKey);
        if OriginalCode = '' then Text := ShortTerm else
        if ShortTerm    = '' then Text := OriginalCode;

        lsItem := Title;
      end;

      if Tree <> nil then begin
        loNode := Tree.Items.AddChildObject(AParentNode, lsItem, lBioNodeInfo);
        if not lBioNodeInfo.ChildrenPopulated then
          Tree.Items.AddChild(loNode, '.');
      end;
      Next;
    end; // while
end;  // PopulateNodes

//==============================================================================
function TBaseBiotopeDictData.GetLatestVersion: TKeyString;
begin
  if FLatestVersion = '' then
    EPropertyNotSet.Create(ResStr_LatestVersionNotSet)
  else
    Result := FLatestVersion;
end;  // GetLatestVersion

//==============================================================================
end.

