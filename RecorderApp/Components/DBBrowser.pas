//==============================================================================
//  Unit:        DBBrowser
//
//  Implements:  TDBBrowserUtility
//
//  Description: This unit is not a datamodule, but it is 'data-aware'.
//               Links to a treeview to allow records to be browsed using the
//               tree view
//
//  Author:      John van Breda
//  Created:     21 June 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 11 $
//    $Date: 28/12/07 14:05 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DBBrowser;

interface

uses
  Sysutils, Classes, comctrls, DataClasses, JNCCDatasets, ExceptionForm,
  GeneralData, JNCCRelationships, Relationships_ADO, DatabaseAccessADO, ADODB;

type
  { class to stick in the data of treenodes to record the info }
  TRecordDataNode = class
    FDataValue : string;
    FFieldName : string;
    FTableName : string;
  end;

  EDBBrowserUtility = class(TExceptionPath);

  TDBBrowserUtility = class
  private
    FTreeView : TTreeView;
    FQuery : TJNCCQuery;
    procedure GetRecordStruct(iKeyList: TKeyList; iNode: TTreenode);
    procedure GetRecordStrings(ioRecordList: TStrings;
      const iTable: string; const iKey1, iKey2: TKeyString);
  public
    constructor Create(iTreeView : TTreeView; AConnection: TADOConnection);
    destructor Destroy; override;
    procedure SetTreeViewItem(iSourceData: TKeyList);
    procedure PopulateRelationship(iRelIndex: integer; iNode: TTreeNode);
  end;

//==============================================================================
implementation

resourcestring
  ResStr_NilList =  'A call was made to GetRecordStrings with a nil string list';
  ResStr_RecordMissing = 'The record was not found in table : ';


//==============================================================================
{ TDBBrowserUtility }
//==============================================================================

constructor TDBBrowserUtility.Create(iTreeView : TTreeView;
  AConnection: TADOConnection);
begin
  inherited Create;
  FTreeView :=iTreeView;
  FQuery := TJNCCQuery.Create(nil);
  FQuery.Connection := AConnection;
end;


{ Populate a tree view with record structure determined by the item in the
     provided keylist }
procedure TDBBrowserUtility.SetTreeViewItem(iSourceData: TKeyList);
var
  lTableNode : TTreeNode;
begin
  if iSourceData.Header.ItemCount = 0 then
      Exit; // no data dropped
  FTreeView.Items.Clear;
  { Add a tree node for the table }
  lTableNode := FTreeView.Items.Add(nil, iSourceData.Header.TableName + ' - '
                               + iSourceData.Items[0].KeyField1);
  lTableNode.ImageIndex := 0; // table glyph
  lTableNode.SelectedIndex := 0;
    { Read the fields and data into the tree view }
  GetRecordStruct( iSourceData, lTableNode );
  { Expand the node one level }
  lTableNode.Expand(True);
  // Scroll back to top of tree, instead of always showing the last nodes.
  FTreeView.Items[0].MakeVisible;
end;



procedure TDBBrowserUtility.GetRecordStruct(iKeyList: TKeyList;
                                                      iNode : TTreenode);
var
  liItem, liRecord : integer;
  lFieldNode, lDataNode : TTreenode;
  lDataInstance : TRecordDataNode;
  lRecordStrings : TStringList;
begin
  lRecordStrings := TStringList.Create;
  try
    for liItem := 0 to iKeyList.Header.ItemCount-1 do
    begin
      lRecordStrings.Clear;
      { Locate the record }
      GetRecordStrings( lRecordStrings, iKeyList.Header.TableName,
                                  iKeylist.Items[liItem].KeyField1,
                                  iKeylist.Items[liItem].KeyField2 );
      for liRecord := 0 to lRecordStrings.Count-1 do
      begin
        lFieldnode := FTreeView.Items.AddChild(iNode,
                                          lRecordStrings.Names[liRecord]);
        lFieldNode.ImageIndex := 1;
        lFieldNode.Selectedindex := 1;
        lDataInstance := TRecordDatanode.Create;
        lDataInstance.FDataValue := lRecordStrings.Values[lRecordStrings.Names[liRecord]];
        lDataInstance.FFieldName := lRecordStrings.Names[liRecord];
        lDataInstance.FTableName := iKeyList.Header.TableName;
        if lDataInstance.FDataValue <> '' then
          lDataNode := FTreeView.Items.AddChildObject(lFieldNode,
                          lDataInstance.FDataValue, lDataInstance)
        else
          lDataNode := FTreeView.Items.AddChildObject(lFieldNode, '[Blank]',
                                                                  lDataInstance);
        lDataNode.Imageindex := 2; // data glyph
        lDataNode.Selectedindex := 2;
      end; // for liRecord
    end; // for liItem
  finally
    lRecordStrings.Free;
  end; // try.. finally
end;



{ Destructor - just cleanup }
destructor TDBBrowserUtility.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

{ Return a string list constructed from the Key item.  The string list contains
     every field=value pair for the record identified }
procedure TDBBrowserUtility.GetRecordStrings(ioRecordList: TStrings;
  const iTable: string; const iKey1, iKey2: TKeyString);
var
  i         : integer;
  lPrimaryKey : TPrimaryKey;
begin
  if ioRecordList = nil then
    raise EDBBrowserUtility.Create(ResStr_NilList);
  lPrimaryKey := dmDatabase.SplitPrimaryKey( iTable );
  with FQuery do
  begin
    SQL.Clear;
    SQL.Add('select * from ' + iTable);
    SQL.Add('where ' + lPrimaryKey.Key1 + ' = ''' + iKey1 + '''');
    if iKey2 <> '' then
      SQL.Add('and ' + lPrimaryKey.Key2 + ' = ''' + iKey2 + '''');
    Open;
    try
      if EOF then
        raise EDBBrowserUtility.Create(ResStr_RecordMissing + iTable);
      for i := 0 to FieldCount-1 do
      begin
        { Construct name value pair - max 255 chars}
        if Length(Fields[i].AsString) > 255 then
          ioRecordList.Add( Fields[i].FieldName + '=' +
                          Copy(Fields[i].AsString, 1, 252) + '...' )
        else
          ioRecordList.Add( Fields[i].FieldName + '=' +
                          Fields[i].AsString );
      end; // for
    finally
      Close;
    end;
  end; // with lQuery
end;  // GetRecordStrings

{ Populate a node with items identified according to a relationship index.
     The relationship index needs to be previously located with a call to
     find relationship. }
procedure TDBBrowserUtility.PopulateRelationship(iRelIndex: integer;
  iNode: TTreeNode);
var
  lKeyList : TEditableKeyList;
  lstRecordStrings : TStringList;
begin
  if TRecordDataNode(iNode.Data).FDataValue = '' then
    { blank data }
    Exit;
  lKeyList := TEditableKeylist.Create;
  try
    if iRelIndex = NAME_REL then
    begin
      lstRecordStrings := TStringList.Create;
      GetRecordStrings( lstRecordStrings, 'NAME',
                    TRecordDataNode(iNode.Data).FDataValue, '' );
      if lstRecordStrings.Values['ORGANISATION'] = 'True' then
        lKeyList.SetTable('ORGANISATION')
      else
        lKeyList.SetTable('INDIVIDUAL');
    end
    else
      lKeyList.SetTable(dmDatabase.Relationships.Relationship[iRelIndex].MasterTable);
    lKeyList.AddItem(TRecordDataNode(iNode.Data).FDataValue, '' );
    GetRecordStruct(lKeyList, iNode);
  finally
    lKeyList.Free;
  end; // try.. finally
end;

end.
