//==============================================================================
//  Unit:        MergeDataData
//
//  Implements:  TdmMergeData
//               TRecordDataNode
//
//  Description: Implements data access functionality for the MergeData screen.
//
//  Author:      John van Breda
//  Created:     28 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 45 $
//    $Date: 4/01/08 11:52 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MergeDataData;

interface                                            

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DataClasses, Db, GeneralFunctions, JNCCDatasets, ComCtrls, HierarchyNodes,
  ImgList, BaseData, ExceptionForm, ADODB, DatabaseAccessADO, ComObj;

type
  EMergeDataError = class(TExceptionPath);

  { class to stick in the data of treenodes to record the info }
  TRecordDataNode = class
    FDataValue: String;
    FFieldName: String;
    FTableName: String;
  end;

  TdmMergeData = class(TBaseDataModule)
    ilTreeImages: TImageList;
    qryFixupJoin: TJNCCQuery;
    qryHelpConstraints: TJNCCQuery;
    qryTables: TJNCCQuery;
  private
    FWorkToDo: Integer;
    FWorkDone: Integer;
    FDeleteSources: Boolean; // for progress bar management
    FUnsetPreferred: Boolean; // default behaviour is to uncheck preferred flag but mustn't do during import
    FPreferredLinks: TStringList; // identify joins on which the preferred flag operates
    procedure CheckSingleFieldKey(iTable: String);
    procedure SetDeleteSources(const Value: Boolean);
    procedure LoadPreferredLinks;
    procedure PrepareMerge(const ATable, ASourceKey, ADestKey: string);
  protected
    procedure FixupJoin(const iDetailTable, iDetailName: String;
      const iSourceKey, iDestKey: TKeyString);
    procedure UpdateNameTable(const iSourceKey, iDestKey: String);
    procedure DeleteSource(const iTable: String; const iSourceKey: TKeyString);
    procedure ProcessSingleTableMerge(const iTable: String;
      const iSourceKey: TKeyString;
      const iDestKey: TKeyString);
    procedure ProcessThreeTableMerge(const iTable1, iTable2, iTable3: String;
      const iSourceKey: TKeyString;
      const iDestKey: TKeyString);
    function OneToOneJoin(const iTable1, iTable2: String): Boolean;
    function IsPrimaryKey(AstReferencingTable, AstReferencingField: String): Boolean;
    function TableExists(const iTableName: String): Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); reintroduce; overload;
    constructor Create(AOwner: TComponent; const iPath: String); reintroduce; overload;
    destructor Destroy; override;
    procedure ProcessMerge(const iTable: String;
              const iSourceKey: TKeyString; const iDestKey: TKeyString);
    procedure ProcessMergeButLeavePreferred(const iTable: String;
              const iSourceKey, iDestKey: TKeyString);
    procedure GetRecordStruct(iKeyList: TKeyList; iTreeView: TTreeView;
      iNode: TTreeNode);
    procedure PopulateRelationship(iRelIndex: Integer; iNode: TTreeNode);
    property DeleteSources: Boolean read FDeleteSources write SetDeleteSources;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  MainTBar, JNCCRelationships, GeneralData, Constants;

const
  ST_NAME_TABLE   = 'NAME';
  ST_INDIVIDUAL   = 'INDIVIDUAL';
  ST_ORGANISATION = 'ORGANISATION';
  ST_SOURCE       = 'SOURCE';
  ST_SOURCE_FILE  = 'SOURCE_FILE';
  ST_REFERENCE    = 'REFERENCE';

resourcestring
  ResStr_CompoundKey = 'Items cannot be merged in a table with a compound key: ';
  ResStr_FixupFailure = 'Failure occurred whilst translating keys in table ';
  ResStr_MergeRollback = 'A problem occurred during the merge.  All data changes have been undone.';
  ResStr_CannotDeleteRecord = 'Failed to delete record from table %s';

  //Status
  ResStr_MergingItemsInTable =  'Merging items in %s table..';
  
{ TdmMergeData }

//==============================================================================
constructor TdmMergeData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeleteSources := True; // default
  FUnsetPreferred := True; // uncheck all preferred flags in merging data
  LoadPreferredLinks;
end;  // Create

//==============================================================================
{ Overloaded version of constructor which allows the merge data module to
     connect to databases other than the main one }
constructor TdmMergeData.Create(AOwner: TComponent; const iPath: String);
begin
  inherited Create(AOwner);
  SetDBPath(iPath);
  FDeleteSources := True; // default
  FUnsetPreferred := True; // uncheck all preferred flags in merging data
  LoadPreferredLinks;
end;  // Create


//==============================================================================
{ Load PREFERRED_LINKS into a String list so we can rapidly scan }
procedure TdmMergeData.LoadPreferredLinks;
begin
  FPreferredLinks := TStringList.Create;
  FPreferredLinks.Sorted := True; // improves IndexOf performance
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'Select TABLE_NAME, PREFERRED_FIELD from PREFERRED_LINKS';
    Open;
    try
      while not Eof do begin
        FPreferredLinks.Add(FieldByName('TABLE_NAME').AsString + '-' + FieldByName('PREFERRED_FIELD').AsString);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;


//==============================================================================
{ Data module destructor - cleanup }
destructor TdmMergeData.Destroy;
begin
  FPreferredLinks.Free;
  FPreferredLinks := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
{ Public method - calls ProcessMultiTableMerge to do the dirty work, but
    wraps it with transactions, hourglasses etc.  Also ensures that one-one
    joins are merged at both ends. }
procedure TdmMergeData.ProcessMerge(const iTable: String; const iSourceKey,
  iDestKey: TKeyString);
var
  lCursor: TCursor;
  lTaskIndex, lInnerTaskIndex: Integer;
begin
  lCursor := HourglassCursor;
  frmMain.SetStatus(Format(ResStr_MergingItemsInTable,[iTable]));
  lTaskIndex := frmMain.ProgressBar.EmbedTask(0, 95);
  lInnerTaskIndex := 0;
  frmMain.ProgressBar.TaskPosition := 0;
  { start a transaction }
  dmDatabase.dbLocal.Execute('Set xact_abort off');
  dmDatabase.dbLocal.BeginTrans;
  try
    try
      PrepareMerge(iTable, iSourceKey, iDestKey);
      // If updating name or individual tables, then we need to split the progress bar into the
      // initial merge task and the subsequent task of checking ALL tables for the metadata
      // joins (entered/updated by) which don't exist as physical db relationships.
      if (CompareText(iTable, ST_NAME_TABLE)=0) or (CompareText(iTable, ST_INDIVIDUAL)=0) then
        lInnerTaskIndex := frmMain.ProgressBar.EmbedTask(0, 40);
      { ensure related one to one records processed for certain tables }
      if    (CompareText(iTable, ST_NAME_TABLE)=0) or
            (CompareText(iTable, ST_INDIVIDUAL)=0) or
            (CompareText(iTable, ST_ORGANISATION)=0) then begin
        ProcessThreeTableMerge(ST_INDIVIDUAL, ST_ORGANISATION, ST_NAME_TABLE, iSourceKey, iDestKey);
      end
      else if (CompareText(iTable, ST_SOURCE)=0) or
              (CompareText(iTable, ST_SOURCE_FILE)=0) or
              (CompareText(iTable, ST_REFERENCE)=0) then
        ProcessThreeTableMerge(ST_SOURCE_FILE, ST_REFERENCE, ST_SOURCE, iSourceKey, iDestKey)
      else
        ProcessSingleTableMerge(iTable, iSourceKey, iDestKey);
      if (CompareText(iTable, ST_NAME_TABLE)=0) or (CompareText(iTable, ST_INDIVIDUAL)=0) then begin
        // Switch to the next sub-task as non-physical metadata joins also need to be updated.
        frmMain.ProgressBar.FinishAndEmbedNext(lInnerTaskIndex, 40, 100);
        UpdateNameTable(iSourceKey, iDestKey);
        frmMain.ProgressBar.FinishTask(lInnerTaskIndex);
      end;
      lTaskIndex := frmMain.ProgressBar.FinishAndEmbedNext(lTaskIndex, 95, 100);
      { All work done OK, so commit }
      dmDatabase.dbLocal.CommitTrans;
    except
      on E:Exception do // any probs, then rollback
      begin
        dmDatabase.dbLocal.RollbackTrans;
        raise E;
      end;
    end;
  finally
    frmMain.ProgressBar.FinishTask(lTaskIndex);
    DefaultCursor(lCursor);
  end; // try.. finally
end; // ProcessMerge



procedure TdmMergeData.ProcessMergeButLeavePreferred(const iTable: String; const iSourceKey,
  iDestKey: TKeyString);
begin
  FUnsetPreferred := False;
  try
    ProcessMerge(iTable, iSourceKey, iDestKey);
  finally
    FUnsetPreferred := True;
  end;
end;




//==============================================================================
{ Controls merging of several tables and the progress bar - for handling
     one to one joins where tables are subtyped }
procedure TdmMergeData.ProcessThreeTableMerge(const iTable1, iTable2, iTable3: String;
                            const iSourceKey: TKeyString;
                            const iDestKey: TKeyString);
var
  lTaskIndex: Integer;
begin
  lTaskIndex := frmMain.ProgressBar.EmbedTask(0, 33);
  try
    ProcessSingleTableMerge(iTable1, iSourceKey, iDestKey);
    frmMain.ProgressBar.FinishAndEmbedNext(lTaskIndex, 33, 67);
    ProcessSingleTableMerge(iTable2, iSourceKey, iDestKey);
    frmMain.ProgressBar.FinishAndEmbedNext(lTaskIndex, 67, 100);
    ProcessSingleTableMerge(iTable3, iSourceKey, iDestKey);
  finally
    frmMain.ProgressBar.FinishTask(lTaskIndex);
  end;
end;  // ProcessThreeTableMerge

//==============================================================================
{ Actually merge 2 data items within a single table identified by iTable.  The
     source data record is deleted afterwards - all data pointing to this is
     pointed to the destination record }
procedure TdmMergeData.ProcessSingleTableMerge(const iTable: String;
  const iSourceKey, iDestKey: TKeyString);
var
  i: Integer;
  liSort: Integer;
begin
  CheckSingleFieldKey(iTable);
  if TableExists(iTable) then
  begin
    { Get a count of tables we must work on so we can set the progress bar }
    FWorkToDo := dmDatabase.Relationships.GetForeignTableCount(iTable);
    FWorkDone := 0;
    { Find the tables we need to fixup }
    for i := 0 to dmDatabase.Relationships.Count-1 do
    begin
      with dmDatabase.Relationships.Relationship[i] do
      begin
        if (CompareText(MasterTable, iTable) = 0) and (not (OneToOneJoin(iTable, MasterTable))) then
          if not IsPrimaryKey(DetailTable, Fields[0].DetailName) then
          begin
            FixupJoin(DetailTable, Fields[0].DetailName, iSourceKey, iDestKey);
            Inc(FWorkDone);
            if FWorkToDo <> 0 then
              frmMain.ProgressBar.TaskPosition := ((FWorkDone * 100) div FWorkToDo);
          end
          else //Delete the record in the other table
          begin
            if CompareText(DetailTable,ST_REFERENCE) = 0 then //I have given up trying to be generic
            begin
              dmDatabase.LocalDatabase.Execute(
                'Update Reference_Author set SOURCE_KEY = ' + quotedStr(iDestKey) +
                'where SOURCE_KEY = ' + quotedStr(iSourceKey));
              dmDatabase.LocalDatabase.Execute(
                'Update Reference_Editor set SOURCE_KEY = ' + quotedStr(iDestKey) +
                'where SOURCE_KEY = ' + quotedStr(iSourceKey));
              dmDatabase.LocalDatabase.Execute(
                'Update Reference_Number set SOURCE_KEY = ' + quotedStr(iDestKey) +
                'where SOURCE_KEY = ' + quotedStr(iSourceKey));
              with dmGeneralData.qryAllPurpose do //realign the sort order.
              begin
                SQL.Clear;
                SQL.Add('Select Sort_Order, AUTHOR_KEY' +
                    ' from REFERENCE_AUTHOR where SOURCE_KEY = ' +quotedStr(iDestKey) +
                    ' order by SORT_ORDER asc');
                parseSQL := false;
                try
                  liSort := 1;
                  open;
                  try
                    First;
                    While not Eof do
                    begin
                      Edit;
                      Fields[0].AsInteger := liSort;
                      post;
                      Inc(liSort);
                      next;
                    end;
                  finally
                    close;
                  end;
                finally
                  ParseSQL := true;
                end;
              end;
            end;
            dmDatabase.LocalDatabase.Execute(
                Format('DELETE FROM "%s" WHERE "%s" = ''%s''',
                       [DetailTable, Fields[0].DetailName, iSourceKey]));
//            'Delete from "' + DetailTable + '" where ' +
//                FormatWithSquareBrackets(Fields[0].DetailName) +  '=' +
//                  QuotedStr(iSourceKey));
          end;
      end; // with
    end;
    if FDeleteSources then
      DeleteSource(iTable, iSourceKey);
  end; // if tableexists
end;  // ProcessSingleTableMerge

//==============================================================================
{ Raises an error if the table specified has a composite key - this sort
     of table cannot be merged }
procedure TdmMergeData.CheckSingleFieldKey(iTable: String);
begin
  { Check we are looking at a single field key }
  try
    dmDatabase.GetPrimaryKey(iTable, False);
  except on E:EMultiFieldKey do
    raise EMergeDataError.Create(ResStr_CompoundKey + iTable, E);
  end;
end;  // CheckSingleFieldKey

//==============================================================================
{ The name table has joins all over the place, which are not physically
     present.  They are either entered_by, changed_by or checked_by fields,
     which also need fixing }
procedure TdmMergeData.UpdateNameTable(const iSourceKey, iDestKey: String);
var
  i, tableCount: Integer;
  ltfDoEntered, ltfDoChecked, ltfDoChanged: Boolean;
begin
  tableCount := dmGeneralData.TableList.Count;
  for i := 0 to tableCount - 1 do
  begin
    if TableExists(dmGeneralData.TableList[i]) then
    begin
      ltfDoEntered := false;
      ltfDoChanged := false;
      ltfDoChecked := false;
      with dmGeneralData.qryAllPurpose do begin
        if Active then Close;
        SQL.Text := 'SELECT TOP 0 * FROM "' + dmGeneralData.TableList[i] + '"';
        Try
          Open;
          ltfDoEntered := Assigned(FindField('ENTERED_BY'));
          ltfDoChanged := Assigned(FindField('CHANGED_BY'));
          ltfDoChecked := Assigned(FindField('CHECKED_BY'));
          Close;
         Except on Exception do;

         end;
      end;
      if ltfDoEntered then
        FixupJoin(dmGeneralData.TableList[i], 'ENTERED_BY', iSourceKey, iDestKey);
      if ltfDoChanged then
        FixupJoin(dmGeneralData.TableList[i], 'CHANGED_BY', iSourceKey, iDestKey);
      if ltfDoChecked then
        FixupJoin(dmGeneralData.TableList[i], 'CHECKED_BY', iSourceKey, iDestKey);
      frmMain.ProgressBar.TaskPosition := ((i+1) * 100) div tableCount;
    end;
  end;
end;  // UpdateNameTable

//==============================================================================
{ A bug exists in titan with the Exists property of a table, so this function
    replicates the functionality.  We need to check before merging detail
    tables, as the database being merged could be a partial db and tables
    may be missing (eg the import db) }
function TdmMergeData.TableExists(const iTableName: String): Boolean;
begin
  if Copy(iTableName, 1, 1)='~' then
    Result := False // system table - ignore
  else
    with qryTables do
    begin
      SQL.Text := 'sp_tables ' + FormatWithSquareBrackets(iTableName);
      Open;
      try
        Result := not (Eof and Bof);
      finally
        Close;
      end;
   end; // with
end;  // TableExists

//==============================================================================
{ Runs a query against the iDetailTable.  Changes the field iDetailName.  Sets
     the value to iDestKey, where the value originally equalled iSourceKey }
procedure TdmMergeData.FixupJoin(const iDetailTable, iDetailName: String;
  const iSourceKey, iDestKey: TKeyString);
var lPrefField:Boolean;
  ltfCanUpdate: Boolean;
begin
  try
    if iDetailTable = 'Paste Errors' then // ignore this system table
      Exit;
    if TableExists(iDetailTable) then
    begin
      // Have to checked if detail table contains Preferred field, as in Location_Name table
      // If there is such a field, set it to false during the update so the record/name
      // doesn't appear wrongly in a tree
      with dmDatabase.ExecuteSQL('SELECT 0 FROM SysColumns ' +
          'WHERE ID=Object_ID('''+iDetailTable+''') AND [Name]=''Preferred''',
          true) do
        lPrefField := not (BOF or EOF) and (FUnsetPreferred) and
                (FPreferredLinks.IndexOf(iDetailTable + '-' + iDetailName)<>-1);
      with dmDatabase.ExecuteSQL('select Permissions(Object_Id(' +
          QuotedStr(iDetailTable) + '), ' + QuotedStr(iDetailName) + ') As Perm', true) do
        ltfCanUpdate := ((Fields['Perm'].Value and 2) <>0);
      if ltfCanUpdate then begin
        { Build an SQL statement to do the fixup }
        with qryFixUpJoin do
        begin
          //If can't update assume this is a table (such as Report_Attribute) which
          //isn't updatable but won't have data entered by a nobody
          SQL.Clear;
          SQL.Add('Select ' + FormatWithSquareBrackets(iDetailName));
          if lPrefField then
            SQL.Add(' , Preferred ');
          SQL.Add('FROM  "'+ iDetailTable + '"');
          SQL.Add('WHERE "' + iDetailName + '" = ''' + iSourceKey + '''');
          ParseSQL := false;
          try
            Open;
            First;
            While not EOF do
            begin
              Edit;
              FieldByName(iDetailName).AsString := iDestKey;
              if lPrefField then
                FieldByName('Preferred').AsBoolean := false;
              try
                Post;
                Next;
              except on EDatabaseError do //caused duplicate primary key
                Delete;
              end;
            end;
          finally
            Close;
          end;
        end;
      end;
    end;
  except
    on E:Exception do // add error info
      raise EMergeDataError.Create(ResStr_FixupFailure + iDetailTable, E);
  end; // try..except
end;  // FixupJoin

//==============================================================================
procedure TdmMergeData.GetRecordStruct(iKeyList: TKeyList; iTreeView: TTreeView;
                                                           iNode: TTreenode);
var
  liItem, liRecord: Integer;
  lFieldNode, lDataNode: TTreenode;
  lDataInstance: TRecordDataNode;
  lRecordStrings: TStringList;
begin
  lRecordStrings := TStringList.Create;
  try
    for liItem := 0 to iKeyList.Header.ItemCount-1 do
    begin
      lRecordStrings.Clear;
      { Locate the record depending on number of key fields required }
      if iKeyList.Items[liItem].KeyField2 = '' then
        dmGeneralData.GetRecordStrings(lRecordStrings, iKeyList.Header.TableName,
                                  iKeylist.Items[liItem].KeyField1)
      else
        dmGeneralData.GetRecordStrings(lRecordStrings, iKeyList.Header.TableName,
                                  iKeylist.Items[liItem].KeyField1,
                                  iKeylist.Items[liItem].KeyField2);
      for liRecord := 0 to lRecordStrings.Count-1 do
      begin
        lFieldnode := iTreeView.Items.AddChild(iNode,
                                          lRecordStrings.Names[liRecord]);
        lFieldNode.ImageIndex := 1;
        lFieldNode.Selectedindex := 1;
        lDataInstance := TRecordDatanode.Create;
        lDataInstance.FDataValue := lRecordStrings.Values[lRecordStrings.Names[liRecord]];
        lDataInstance.FFieldName := lRecordStrings.Names[liRecord];
        lDataInstance.FTableName := iKeyList.Header.TableName;
        if lDataInstance.FDataValue <> '' then
          lDataNode := iTreeView.Items.AddChildObject(lFieldNode,
                          lDataInstance.FDataValue, lDataInstance)
        else
          lDataNode := iTreeView.Items.AddChildObject(lFieldNode, '[Blank]',
                                                                  lDataInstance);
        lDataNode.ImageIndex   := 2; // data glyph
        lDataNode.SelectedIndex:= 2;
      end; // for liRecord
    end; // for liItem
  finally
    lRecordStrings.Free;
  end; // try.. finally
end;  // GetRecordStruct

//==============================================================================
{ Populate a node with items identified according to a relationship index.
     The relationship index needs to be previously located with a call to
     find relationship. }
procedure TdmMergeData.PopulateRelationship(iRelIndex: Integer;
  iNode: TTreeNode);
var
  lKeyList: TEditableKeyList;
  lstRecordStrings: TStringList;
begin
  if TRecordDataNode(iNode.Data).FDataValue = '' then
    { blank data }
    Exit;
  lKeyList := TEditableKeylist.Create;
  try
    if iRelIndex = NAME_REL then
    begin
      lstRecordStrings := TStringList.Create;
      dmGeneralData.GetRecordStrings(lstRecordStrings,
                                      ST_NAME_TABLE,
                                      TRecordDataNode(iNode.Data).FDataValue);
      if lstRecordStrings.Values[ST_ORGANISATION] = 'True' then
        lKeyList.SetTable(ST_ORGANISATION)
      else
        lKeyList.SetTable(ST_INDIVIDUAL);
    end else
      lKeyList.SetTable(dmDatabase.Relationships.Relationship[iRelIndex].MasterTable);
    lKeyList.AddItem(TRecordDataNode(iNode.Data).FDataValue, '');
    GetRecordStruct(lKeyList, TTreeView(iNode.TreeView), iNode);
  finally
    lKeyList.Free;
  end; // try.. finally
end;  // PopulateRelationship

//==============================================================================
{ When all the key information has been transferred across, we must delete the
     original record }
procedure TdmMergeData.DeleteSource(const iTable: String;
  const iSourceKey: TKeyString);
var
  lPrimaryKey: String;
begin
  lPrimaryKey := dmDatabase.GetPrimaryKey(iTable, False);
  dmGeneralData.ExecuteSQL(Format('DELETE FROM "%s" WHERE "%s" = ''%s''',
                                  [iTable, lPrimaryKey, iSourceKey]),
                           Format(ResStr_CannotDeleteRecord, [iTable]), False);
end;  // DeleteSource

//==============================================================================
{ returns true if tables at either end of a join are part of a one-one join
    (eg name-individual) }
function TdmMergeData.OneToOneJoin(const iTable1, iTable2: String): Boolean;
begin
  Result := False; // default
  if CompareText(iTable1, ST_NAME_TABLE)=0 then
  begin
    Result := (CompareText(iTable2, ST_INDIVIDUAL)=0) or
              (CompareText(iTable2, ST_ORGANISATION)=0);
  end else if CompareText(iTable2, ST_NAME_TABLE)=0 then
  begin
    Result := (CompareText(iTable1, ST_INDIVIDUAL)=0) or
              (CompareText(iTable1, ST_ORGANISATION)=0);
  end else if CompareText(iTable1, ST_SOURCE)=0 then
  begin
    Result := (CompareText(iTable2, ST_REFERENCE)=0) or
              (CompareText(iTable2, ST_SOURCE_FILE)=0);
  end else if CompareText(iTable2, ST_SOURCE)=0 then
  begin
    Result := (CompareText(iTable1, ST_REFERENCE)=0) or
              (CompareText(iTable1, ST_SOURCE_FILE)=0);
  end;
end;  // OneToOneJoin

//==============================================================================
procedure TdmMergeData.SetDeleteSources(const Value: Boolean);
begin
  FDeleteSources := Value;
end;

function TdmMergeData.IsPrimaryKey(AstReferencingTable, AstReferencingField: String): Boolean;
begin
  with qryHelpConstraints do
  begin
    SQL.Text := 'sp_helpConstraint ' + QuotedStr(AstReferencingTable) + ', ''nomsg''';
    //Parameters.ParamByName('table').Value := QuotedStr(AstReferencingTable);
    ParseSQL := false;
    Open;
    try
      Filter := 'constraint_keys = ' + QuotedStr(AstReferencingField) +
            ' and constraint_type like ''PRIMARY KEY%''';
      Filtered := True;
      result := not (Eof and Bof);
    finally
      Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  For Taxon List Item merges, cleanup index_taxon_name since this can't be
    handled through normal methods, as it has no primary key
}
procedure TdmMergeData.PrepareMerge(const ATable, ASourceKey, ADestKey: string);
begin
  if SameText(ATable, TN_TAXON_LIST_ITEM) then begin
    dmDatabase.ExecuteSQL('UPDATE Index_Taxon_Name ' +
        'SET Recommended_Taxon_List_Item_Key=''' + ADestKey + ''' ' +
        'WHERE Recommended_Taxon_List_Item_Key=''' + ASourceKey + '''');
    dmDatabase.ExecuteSQL('DELETE Index_Taxon_Name ' +
        'WHERE Taxon_List_Item_Key=''' + ASourceKey + '''');
  end;
end;

end.
