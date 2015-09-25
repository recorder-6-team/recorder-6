//==============================================================================
//  Unit:        BiotopeDictBrowserData
//
//  Implements:  TdmBiotopeDictBrowser
//
//  Description: Contains biotope dictionary editor specific functions to
//               access and format data.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 8 $
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BiotopeDictBrowserData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseBiotopeDictDataUnit, Db, JNCCDatasets, HtmlView, DataClasses,
  HierarchyNodes, DictionaryHTMLDetails, ADODB, HTTPApp, HTTPProd;

type
  TdmBiotopeDictBrowser = class(TBaseBiotopeDictData)
  private
    function HasFilteredChildNodes: Boolean;
  protected
    function GetFilteredKeys: String; override;
    function GetNodeFilteredState(const key: String): Boolean; override;
    function GetNodeHint(const key: String): String; override;
  public
    procedure SetHTMLDetails(const ADictNode: THTMLDictionaryNode;
      const AListKey: TKeyString; const AHTMLViewer: THTMLViewer);
    procedure SetParamsForHTMLDetails(Sender: TObject; const ADictNode: THTMLDictionaryNode);
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  DatabaseAccessADO, ApplicationSettings, Constants;

//==============================================================================
function TdmBiotopeDictBrowser.GetNodeFilteredState(const key: String): Boolean;
begin
  Result := AppSettings.IndexOfFilteredRecord(TN_BIOTOPE_LIST_ITEM, key) > -1;
end;

//==============================================================================
function TdmBiotopeDictBrowser.GetNodeHint(const key: String): String;
begin
  Result := AppSettings.GetFilteredRecordHint(TN_BIOTOPE_LIST_ITEM, key);
end;

//==============================================================================
function TdmBiotopeDictBrowser.GetFilteredKeys: String;
begin
  if (FilterKeys <> '') and ((NodeToExpand = nil) or HasFilteredChildNodes) then
    Result := FilterKeys
  else
    Result := '';
end;

//==============================================================================
function TdmBiotopeDictBrowser.HasFilteredChildNodes: Boolean;
var
  rs: _Recordset;
begin
  Result := False;

  rs := dmDatabase.ExecuteSQL(
      'SELECT Biotope_List_Item_Key FROM Biotope_List_Item WHERE Parent = '''
      + TNodeObject(NodeToExpand.Data).ItemKey + '''', True);
  try
    while not rs.Eof do begin
      if Pos(rs.Fields['Biotope_List_Item_Key'].Value, FilterKeys) > 0 then begin
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
procedure TdmBiotopeDictBrowser.SetHTMLDetails(const ADictNode: THTMLDictionaryNode;
  const AListKey: TKeyString; const AHTMLViewer: THTMLViewer);
begin
  with THTMLLInes.Create(AListKey, ADictNode, Self, SetParamsForHTMLDetails) do
    try
      NBNLinkName := TBiotopeDictionaryNode(ADictNode).LongName;
      AHTMLViewer.LoadStrings(CreateHTML);
    finally
      Free;
    end;
end;  // SetHTMLDetails

//==============================================================================
procedure TdmBiotopeDictBrowser.SetParamsForHTMLDetails(Sender: TObject;
  const ADictNode: THTMLDictionaryNode);
begin
  with TBiotopeDictionaryNode(ADictNode) do begin
    qryMain.Parameters.ParamByName('Key').Value       := ItemKey;
    qryFacts.Parameters.ParamByName('Key').Value      := BiotopeKey;
    qryStatus.Parameters.ParamByName('Key').Value     := ItemKey;
    qryAssociated.Parameters.ParamByName('Key').Value := ItemKey;
  end;
end;  // SetParamsForHTMLDetails

//==============================================================================
end.
