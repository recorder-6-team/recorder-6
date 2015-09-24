//==============================================================================
//  Unit:        AdminAreaDictBrowserData
//
//  Implements:  TdmAdminAreaDictBrowser
//
//  Description: Contains Admin Area dictionary specific functions to access
//               and format data.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 16 $
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DelphiVersions.Inc'}

unit AdminAreaDictBrowserData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseDictionaryDataUnit, Db, JNCCDatasets, HTTPApp, TreeColl, RapTree,
  DataClasses, HierarchyNodes, Htmlview, HTMLDisplayFuncs, Constants, ADODB
  {$IFDEF DELPHI7UP}, HTTPProd {$ENDIF};

resourcestring
  SAdminDict_AdminAreaType = 'Admin Area Type';
  SAdminDict_TypeName      = 'Name';
  SAdminDict_LastChanged   = 'Last Changed On';
  SAdminDict_Description   = 'Description';
  SAdminDict_RequiresTRapidTree = 'requires a TRapidTree assigned to the Tree property.';

type
  TdmAdminAreaDictBrowser = class(TBaseDictionaryData)
    qryDictDetailsSources: TJNCCQuery;
    qryDictDetails: TJNCCQuery;
    ppAdminAreaDetails: TPageProducer;
    procedure ppAdminAreaDetailsHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings; var ReplaceText: String);
  private
    function GetAdminSourcesDetails: String;
    function GetAdminTypeDetails: String;
    function HasFilteredChildNodes: Boolean;
  protected
    function GetFilteredKeys: String; override;
    procedure PopulateNodes(const ADataset: TDataset; const AParentNode: TFlyNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    function PopulateTopLevel(const AListKeyData: TKeyData): Boolean; override;
    procedure SetHTMLDetails(const AItemKey: TKeyString; const AHTMLViewer: THTMLViewer);
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  DatabaseAccessADO, GeneralData, ApplicationSettings;

//==============================================================================
constructor TdmAdminAreaDictBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FilterClause := 'AND A.Admin_Area_Key IN (%s)';
  qryList.ParseSQL := False; {disable SQL mangling, which breaks the query}
end;  // Create

//==============================================================================
function TdmAdminAreaDictBrowser.GetFilteredKeys: String;
begin
  if (FilterKeys <> '') and ((NodeToExpand = nil) or HasFilteredChildNodes) then
    Result := FilterKeys
  else
    Result := '';
end;

//==============================================================================
function TdmAdminAreaDictBrowser.HasFilteredChildNodes: Boolean;
var
  rs: _Recordset;
begin
  Result := False;

  rs := dmDatabase.ExecuteSQL(
      'SELECT Admin_Area_Key FROM Admin_Area WHERE Parent = '''
      + TNodeObject(NodeToExpand.Data).ItemKey + '''', True);
  try
    while not rs.Eof do begin
      if Pos(rs.Fields['Admin_Area_Key'].Value, FilterKeys) > 0 then begin
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
function TdmAdminAreaDictBrowser.PopulateTopLevel(const AListKeyData: TKeyData): Boolean;
begin
  Result := true;
  if Tree <> nil then begin
    if AListKeyData.ItemKey <> '' then begin
      ListKeys := '''' + AListKeyData.ItemKey + '''';
      qryTopLevel.SQL.Text := ppTopLevel.Content;
      PopulateTree(qryTopLevel, nil);
    end;
  end else
    Raise EUndefinedTarget.Create(ClassName + SAdminDict_RequiresTRapidTree);
end;  // PopulateTopLevel

//==============================================================================
procedure TdmAdminAreaDictBrowser.PopulateNodes(const ADataset: TDataset;
  const AParentNode: TFlyNode);
var lsItem        : String;
    lAdminNodeInfo: TAdminAreaDictionaryNode;
    loNode        : TFlyNode;
begin
  while not ADataset.Eof do
  begin
    lsItem := ADataset.FieldByName('DisplayField').AsString;
    lAdminNodeInfo := TAdminAreaDictionaryNode.Create;
    with lAdminNodeInfo do begin
      Text              := ADataset.FieldByName('DisplayField').AsString;
      ItemKey           := ADataset.FieldByName('ListKey').AsString;
      ItemName          := ADataset.FieldByName('ItemName').AsString;
      ShortCode         := ADataset.FieldByName('ShortCode').AsString;
      ChildrenPopulated := ADataset.FieldByName('ChildrenCount').AsInteger = 0;
      IsFiltered        := AppSettings.IndexOfFilteredRecord(TN_ADMIN_AREA, ItemKey) > -1;
      Hint              := AppSettings.GetFilteredRecordHint(TN_ADMIN_AREA, ItemKey);
    end; // with lAdminNodeInfo

    if Tree <> nil then
    begin
      if AParentNode = nil then
        loNode := Tree.Items.AddObject(nil, lsItem, lAdminNodeInfo)
      else
        loNode := Tree.Items.AddChildObject(AParentNode, lsItem, lAdminNodeInfo);
      // Always add children, if necessary.
      if not lAdminNodeInfo.ChildrenPopulated then
        Tree.Items.AddChild(loNode,'.');
    end;
    ADataset.Next;
  end; // while
  // Try to force a repaint to clear the HTMLViewer
  if Tree.Items.Count = 0 then
    PostMessage((Tree.Owner as TWinControl).Handle, WM_SHOW_DETAILS, 0, 0);  // Owner is the form
end;  // PopulateNodes

//==============================================================================
procedure TdmAdminAreaDictBrowser.SetHTMLDetails(const AItemKey: TKeyString;
  const AHTMLViewer: THTMLViewer);
var lslContent: TStringList;
begin
  with qryDictDetails do begin
    if Active then Close;
    Parameters.ParamByName('Admin_Area_Key').Value:= AItemKey;
    Open;
  end;
  with qryDictDetailsSources do begin
    if Active then Close;
    Parameters.ParamByName('Admin_Area_Key').Value:= AItemKey;
  end;

  lslContent := TStringList.Create;
  try
    lslContent.Text := ppAdminAreaDetails.Content;
    AHTMLViewer.LoadStrings(lslContent);
  finally
    lslContent.Free;
  end;
end;  // SetHTMLDetails

//==============================================================================
procedure TdmAdminAreaDictBrowser.ppAdminAreaDetailsHTMLTag(Sender: TObject;
  Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
var lTempText: String;
begin
  inherited;
  if TagString='ADMIN_AREA_NAME' then begin
    if not qryDictDetails.FieldByName('ITEM_NAME').IsNull then
      ReplaceText := MainHeaderStyle(qryDictDetails.FieldByName('ITEM_NAME').AsString);
  end
  else if TagString = 'LAST_UPDATED' then begin
    if not qryDictDetails.FieldByName('Changed_Date').IsNull then
      lTempText := qryDictDetails.FieldByName('Changed_Date').AsString
    else
      lTempText := qryDictDetails.FieldByName('Entry_Date').AsString;

    ReplaceText := AddTagText('blockquote',
                   Para(FontStyle(True, false, false, SAdminDict_LastChanged + ': ') + lTempText ),
                   '');
  end
  else if TagString = 'ADMIN_AREA_TYPE' then
    ReplaceText := GetAdminTypeDetails
  else if TagString = 'SOURCES' then
    ReplaceText := GetAdminSourcesDetails;
end;  // ppAdminAreaDetailsHTMLTag

//==============================================================================
{ Function to get HTML for the admin type section of the details }
function TdmAdminAreaDictBrowser.GetAdminTypeDetails: String;
var lTempText: String;
begin
  Result := MiniHeaderStyle(SAdminDict_AdminAreaType);
  // use long name or short name but not both
  lTempText := FontStyle(True, false, false, SAdminDict_TypeName + ': ');

  if not qryDictDetails.FieldByName('LONG_NAME').IsNull then
    lTempText := lTempText + qryDictDetails.FieldByName('LONG_NAME').AsString
  else if not qryDictDetails.FieldByName('SHORT_NAME').IsNull then
    lTempText := lTempText + qryDictDetails.FieldByName('SHORT_NAME').AsString;

  lTempText := Para(lTempText);

  if not qryDictDetails.FieldByName('Description').IsNull then
    lTempText := lTempText + Para(FontStyle(True, false, false, SAdminDict_Description + ': ') +
                             qryDictDetails.FieldByName('Description').AsString);

  Result := Result + AddTagText('blockquote', lTempText, '');
end;  // GetAdminTypeDetails

//==============================================================================
{ Function to get HTML for the admin sources section of the details }
function TdmAdminAreaDictBrowser.GetAdminSourcesDetails: String;
var lSourceText: String;
    lSourceList: String;
begin
  with qryDictDetailsSources do begin
    Open;
    try
      if RecordCount>0 then begin // got some records
        while not Eof do begin
          if FieldByName('FILE_NAME').IsNull then
            lSourceText := dmGeneralData.GetReferenceText(FieldByName('SOURCE_KEY').AsString)
          else
            lSourceText := FieldByName('FILE_NAME').AsString;

          lSourceList := lSourceList + Anchor('', FieldByName('SOURCE_KEY').AsString,
                                       lSourceText) + '<BR> ';
          Next;
        end; // while
        Result := SubHeaderStyle('Sources') + AddTagText('blockquote', lSourceList, '');
      end else // if recordcount>0
        Result := '';
    finally
      Close;
    end; //try
  end;
end;  // GetAdminSourceDetails

//==============================================================================
end.
