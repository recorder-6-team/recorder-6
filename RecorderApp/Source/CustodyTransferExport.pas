//==============================================================================
//  Unit: CustodyTransferExport
//
//  Description: Used to transfer custody of data before exporting it.  Called
//  by the ExportData dialogue box.
//
//  Author: Michael Bailey
//  Created: Dec 2002
//
//  Last Revision Details:
//    $Revision: 11 $
//    $Date: 25/03/09 10:05 $
//    $Author: Pauldavies $
//
//  $History: CustodyTransferExport.pas $
//  
//  *****************  Version 11  *****************
//  User: Pauldavies   Date: 25/03/09   Time: 10:05
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18907
//  CCN 292
//  
//  Set collation on the temporary tables to database_default to avoid
//  conflicts.
//  
//  *****************  Version 10  *****************
//  User: Pauldavies   Date: 4/03/09    Time: 8:56
//  Updated in $/JNCC/Development/Build/Source
//  Renamed usp_GetAdditionalLocationKeys and
//  usp_GetAdditionalObservationKeys to usp_LocationKeys_Get_FromKeylist
//  and usp_ObservationKeys_Get_FromKeylist.
//  
//  *****************  Version 9  *****************
//  User: Pauldavies   Date: 16/02/09   Time: 16:35
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18232
//  CCN 303
//  
//  Various changes to increase the speed on large recordsets.
//  
//  *****************  Version 8  *****************
//  User: Pauldavies   Date: 13/02/09   Time: 16:29
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18012
//  CCN 292
//  
//  Added support for custodianship transfer of Survey_Event_Owner,
//  Survey_Event_Owner_Type, Survey_Tag and Concept tables.
//  
//  *****************  Version 7  *****************
//  User: Pauldavies   Date: 13/02/09   Time: 11:52
//  Updated in $/JNCC/Development/Build/Source
//  Incident 18232
//  CCN 303
//  
//  Changed the row by row population of the list of items on which to
//  transfer custody into bulk queries using a temporary table. Also, when
//  the tree view is populated, BeginUpdate and EndUpdate are used so that
//  all the additions are done together.
//  
//  *****************  Version 6  *****************
//  User: Rickyshrestha Date: 3/01/08    Time: 11:39
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardocded strings to resourestring
//  
//  *****************  Version 5  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 13:51
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constants to resourcestring
//  ResStr_NonSite,
//  ResStr_NoneSelected,
//  ResStr_Proceed
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 12/02/03   Time: 10:05
//  Updated in $/JNCC/Source
//  IR 344 fixed.
//  
//  *****************  Version 3  *****************
//  User: Ericsalmon   Date: 9/01/03    Time: 17:14
//  Updated in $/JNCC/Source
//  IR 180 - v2.9.4.0
//  Fixed.
//  
//  *****************  Version 2  *****************
//  User: Michaelbailey Date: 20/12/02   Time: 16:35
//  Updated in $/JNCC/Source
//  Probably needs a few more comments, but I don't have a lot of time
//  left.
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

{ tvRecords is populated using the KeyList supplied in the constructor.  Only
  data for which the current site has custody is considered.

  tvRecords displays only top level records: Surveys, Survey Events, Samples,
  Taxon Occurrences, Biotope Occurrences, Locations, Names and References.

  The Child (Level 1) nodes are associated with an KeyList - a list of all
  records that need transferal of custody.  If a Sample is included in the
  supplied KeyList then the corresponding node will contain any associated
  records in SAMPLE_DATA, SAMPLE_SOURCES and SAMPLE_RELATION.  If instead one
  of these detail records is included in the supplied KeyList then the relevant
  sample node will be displayed but the KeyList will only contain the specific
  record or records supplied in the KeyList.  The exception to this is detail
  records with their own details.  For instance if a LOCATION_FEATURE is
  supplied then it and it's detail records (e.g. POTENTIAL_THREAT) are included
  under the Feature's Location.

  If the site has custody of a supplied detail record, but not the record under
  which it is displayed then (Partial Ownership) will be noted on the control. }

unit CustodyTransferExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Db,
  Dialogs, StdCtrls, ComCtrls, ImageListButton, exgrid, RapTree, TreeColl,
  RestrictedEdits, DataClasses, ImgList, DBBrowser, Custody;

type
  TdlgCustodyTransferExport = class(TForm)
    lblInstruct: TLabel;
    eNewCustodian: TRestrictedEdit;
    tvRecords: TRapidTree;
    btnSelectAll: TImageListButton;
    btnClear: TImageListButton;
    tvRecordDetails: TTreeView;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    lblNewCustodian: TLabel;
    ilCheckBoxes: TImageList;
    procedure tvRecordsClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure eNewCustodianCheckText(Sender: TObject; const iText: String; var ioAccept: Boolean);
    procedure tvRecordsChange(Sender: TObject; Node: TFlyNode);
    procedure btnOkClick(Sender: TObject);
  private
    FBrowser: TDBBrowserUtility;
    FIncludeObservations: Boolean;
    procedure AddMasterToList(AStringList: TStringList;
      const TableName, KeyField: string; const Key: TKeyString);
    procedure AddToList(AStringList: TStringList; const MasterName, MasterKeyField,
      TableName: string; const MasterKey, RecordKey: TKeyString);
    procedure CalculateAllAssigning(ARelinquisher: TCustodyRelinquisher; ANode: TFlyNode);
    procedure CalculateAssigning;
    procedure CalculatePopulation(AKeyList: TKeyList);
    procedure CalculateSomeAssigning(ARelinquisher: TCustodyRelinquisher; ANode: TFlyNode);
    procedure CheckChildren(ANode: TFlyNode);
    procedure CheckParent(ANode: TFlyNode);
    procedure GatherAdditionalKeys(AKeyList: TEditableKeyList);
    procedure FreeTVRecordData;
    procedure FreeSubStringListObjects(AStringList: TStringList);
    procedure FreeWithSubLists(AStringList: TStringList);
    procedure NextTable(const TableName: string);
    procedure PopulateTVRecords(AStringList: TStringList);
    procedure Proceed(ARelinquisher: TCustodyRelinquisher);
    procedure TalkToUser(ARelinquisher: TCustodyRelinquisher);
  public
    constructor Create(AOwner: TComponent; AKeyList: TEditableKeyList;
      AIncludeObservations: Boolean); reintroduce;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ApplicationSettings, GeneralData, ExceptionForm, Maintbar, GeneralFunctions,
  ADODB;

const
  INDEX_CHECKED = 0;
  INDEX_UNCHECKED = 1;
  INDEX_UNSURE = 2;

  CONST_OWN = ' (Partial Ownership)';
  
resourcestring
  ResStr_NonSite =  'Please provide a valid Site ID.';
  ResStr_NoneSelected = 'No data selected.  Do you wish to proceed with the export without ' +
                      'reassigning custody of any data?';
  ResStr_Proceed =  'Custody of the selected data will be reassigned to Site ID %s.  You will no longer be able'#13#10 +
                    'to modify the data once the export is complete.  Please make sure you have typed the Site ID correctly.'#13#10#13#10 +
                    'Select ''Yes'' if you wish to proceed, ''No'' to make any changes, or ''Cancel'' to abort the data export.';
  ResStr_BadKeyList = 'Bad Key List.';
  ResStr_ReassigningCustody = 'Reassigning Custody on Table: %s';
  ResStr_TransferringCustody =  'Transferring Custody of Data';
  ResStr_UnableToCreateTempTable = 'Unable to create temporary table #Key_List.';
  ResStr_UnableToDropTempTable = 'Unable to drop temporary table #Key_List.';
  ResStr_UnableToPopulateTempTable = 'Unable to populate temporary table #Key_List.';

//==============================================================================
constructor TdlgCustodyTransferExport.Create(AOwner: TComponent;
  AKeyList: TEditableKeyList; AIncludeObservations: Boolean);
var lCursor: TCursor;
begin
  inherited Create(AOwner);
  lCursor := HourglassCursor;
  try
    if AKeyList.Header.TableName <> MIXED_DATA then
      raise ECustodialError.Create(ResStr_BadKeyList);
    AKeyList.SortByTable;
    FIncludeObservations := AIncludeObservations;

    GatherAdditionalKeys(AKeyList);
    CalculatePopulation(AKeyList);

    FBrowser := TDBBrowserUtility.Create(tvRecordDetails, dmGeneralData.qryAllPurpose.Connection);
  finally
    DefaultCursor(lCursor);
  end;
end;

destructor TdlgCustodyTransferExport.Destroy;
begin
  FreeTVRecordData;
  FBrowser.Free;
  inherited;
end;

procedure TdlgCustodyTransferExport.AddMasterToList
  (AStringList: TStringList; const TableName, KeyField: string; const Key: TKeyString);
var i: Integer;
    lStringList: TStringList;
    lKeyList: TKeyList;
begin
  i := AStringList.IndexOf(TableName);
  if i = -1 then i := AStringList.AddObject(TableName, TStringList.Create);

  lStringList := TStringList(AStringList.Objects[i]);

  i := lStringList.IndexOf(Key);
  if i = -1 then begin
    i := lStringList.AddObject(Key, TEditableKeyList.Create);
    TEditableKeyList(lStringList.Objects[i]).SetTable(MIXED_DATA);
  end;
  TEditableKeyList(lStringList.Objects[i]).AddItem(Key, TableName);

  lKeyList := TCustodyInfo.MasterDetails(dmGeneralData.qryAllPurpose, TableName, Key);
  try TEditableKeyList(lStringList.Objects[i]).Append(lKeyList);
  finally lKeyList.Free end;
end;

procedure TdlgCustodyTransferExport.AddToList(AStringList: TStringList; const MasterName,
  MasterKeyField, TableName: string; const MasterKey, RecordKey: TKeyString);
var i: Integer;
    lStringList: TStringList;
    lKeyList: TKeyList;
begin
  i := AStringList.IndexOf(MasterName);
  if i = -1 then i := AStringList.AddObject(MasterName, TStringList.Create);

  lStringList := TStringList(AStringList.Objects[i]);

  i := lStringList.IndexOf(MasterKey);
  if i = -1 then i := lStringList.IndexOf(MasterKey + CONST_OWN);
  if i = -1 then begin
    if dmGeneralData.HaveCustody(MasterName, MasterKeyField, MasterKey) then
      i := lStringList.AddObject(MasterKey, TEditableKeyList.Create)
    else i := lStringList.AddObject(MasterKey + CONST_OWN, TEditableKeyList.Create);
    TEditableKeyList(lStringList.Objects[i]).SetTable(MIXED_DATA);
  end;
  TEditableKeyList(lStringList.Objects[i]).AddItem(RecordKey, TableName);

  if TCustodyInfo.IsSubMaster(TableName) then begin
    lKeyList := TCustodyInfo.SubMasterDetails(dmGeneralData.qryAllPurpose, TableName, RecordKey);
    try TEditableKeyList(lStringList.Objects[i]).Append(lKeyList);
    finally lKeyList.Free end;
  end;
end;

procedure TdlgCustodyTransferExport.btnClearClick(Sender: TObject);
var k: Integer;
    lNode: TFlyNode;
begin
  for k := tvRecords.Items.Count - 1 downto 0 do begin
    lNode := tvRecords.Items[k];
    lNode.ImageIndex := INDEX_UNCHECKED;
    lNode.SelectedIndex := INDEX_UNCHECKED;
    CheckChildren(lNode);
  end;
end;

procedure TdlgCustodyTransferExport.btnOkClick(Sender: TObject);
begin
  if Length(eNewCustodian.Text) < 8 then begin
    eNewCustodian.SetFocus;
    raise TExceptionPath.CreateNonCritical(ResStr_NonSite);
  end;
  CalculateAssigning;
end;

procedure TdlgCustodyTransferExport.btnSelectAllClick(Sender: TObject);
var k: Integer;
    lNode: TFlyNode;
begin
  for k := tvRecords.Items.Count - 1 downto 0 do begin
    lNode := tvRecords.Items[k];
    lNode.ImageIndex := INDEX_CHECKED;
    lNode.SelectedIndex := INDEX_CHECKED;
    CheckChildren(lNode);
  end;
end;

procedure TdlgCustodyTransferExport.CalculateAllAssigning
  (ARelinquisher: TCustodyRelinquisher; ANode: TFlyNode);
var lChildNode: TFlyNode;
begin
  lChildNode := ANode.GetFirstChild;
  repeat
    ARelinquisher.AssignKeyList(TKeyList(lChildNode.Data));
    lChildNode := ANode.GetNextChild(lChildNode);
  until lChildNode = nil;
end;

procedure TdlgCustodyTransferExport.CalculateAssigning;
var lRelinquisher: TCustodyRelinquisher;
    lNode: TFlyNode;
    k: Integer;
begin
  lRelinquisher := TCustodyRelinquisher.Create;
  try
    for k := tvRecords.Items.Count - 1 downto 0 do begin
      lNode := tvRecords.Items[k];
      case lNode.ImageIndex of
        INDEX_CHECKED: CalculateAllAssigning(lRelinquisher, lNode);
        INDEX_UNSURE : CalculateSomeAssigning(lRelinquisher, lNode);
      else { INDEX_UNCHECKED } // Do Nothing
      end;
    end;
    TalkToUser(lRelinquisher);
  finally
    lRelinquisher.Free
  end;
end;

procedure TdlgCustodyTransferExport.CalculatePopulation(AKeyList: TKeyList);
var k: Integer;
    tableName, keyField, masterName, masterKeyField: string;
    masterKey: TKeyString;
    lStringList: TStringList;
begin
  k := AKeyList.Header.ItemCount - 1;
  lStringList := TStringList.Create;
  try
    try
      while (k >= 0) do begin
        tableName := UpperCase(AKeyList.Items[k].KeyField2);
        keyField := TCustodyInfo.TableKey(tableName);
        masterName := TCustodyInfo.MasterTable(tableName);
        if masterName <> tableName then begin
          masterKeyField := TCustodyInfo.TableKey(masterName);
          while (k >= 0) and (UpperCase(AKeyList.Items[k].KeyField2) = tableName) do begin
            masterKey := TCustodyInfo.MasterKey
                (dmGeneralData.qryAllPurpose, tableName, AKeyList.Items[k].KeyField1);
            AddToList(lStringList, masterName, masterKeyField, tableName,
                        masterKey, AKeyList.Items[k].KeyField1);
            Dec(k);
          end;
        end else
          while (k >= 0) and (UpperCase(AKeyList.Items[k].KeyField2) = tableName) do begin
            AddMasterToList(lStringList, tableName, keyField, AKeyList.Items[k].KeyField1);
            Dec(k);
          end;
      end;
      PopulateTVRecords(lStringList);
    except
      on E: Exception do begin
        FreeSubStringListObjects(lStringList);
        raise E;
      end;
    end;
  finally
    FreeWithSubLists(lStringList);
  end;
end;

procedure TdlgCustodyTransferExport.CalculateSomeAssigning
  (ARelinquisher: TCustodyRelinquisher; ANode: TFlyNode);
var lChildNode: TFlyNode;
begin
  lChildNode := ANode.GetFirstChild;
  repeat
    if lChildNode.ImageIndex = INDEX_CHECKED then
      ARelinquisher.AssignKeyList(TKeyList(lChildNode.Data));
    lChildNode := ANode.GetNextChild(lChildNode);
  until lChildNode = nil;
end;

procedure TdlgCustodyTransferExport.CheckChildren(ANode: TFlyNode);
var lChildNode: TFlyNode;
begin
  lChildNode := ANode.GetFirstChild;
  while lChildNode <> nil do begin
    lChildNode.ImageIndex := ANode.ImageIndex;
    lChildNode.SelectedIndex := ANode.SelectedIndex;
    lChildNode := ANode.GetNextChild(lChildNode);
  end;
end;

procedure TdlgCustodyTransferExport.CheckParent(ANode: TFlyNode);
var foundCheck, foundNot: Boolean;
    lChildNode: TFlyNode;
begin
  foundCheck := False;
  foundNot := False;

  lChildNode := ANode.GetFirstChild;
  while (lChildNode <> nil) and not (foundCheck and foundNot) do begin
    foundCheck := foundCheck or (lChildNode.ImageIndex = INDEX_CHECKED);
    foundNot := foundNot or (lChildNode.ImageIndex = INDEX_UNCHECKED);
    lChildNode := ANode.GetNextChild(lChildNode);
  end;

  if foundCheck then
    if foundNot then
      ANode.ImageIndex := INDEX_UNSURE
    else
      ANode.ImageIndex := INDEX_CHECKED
  else
    ANode.ImageIndex := INDEX_UNCHECKED;
  ANode.SelectedIndex := ANode.ImageIndex;
end;

procedure TdlgCustodyTransferExport.eNewCustodianCheckText
  (Sender: TObject; const iText: String; var ioAccept: Boolean);
var k: Integer;
begin
  ioAccept := Length(iText) <= 8;
  for k := Length(iText) downto 1 do
    ioAccept := ioAccept and (iText[k] in ['A'..'Z', '0'..'9']);
end;

procedure TdlgCustodyTransferExport.FreeSubStringListObjects(AStringList: TStringList);
var i, j: Integer;
begin
  for i := 0 to AStringList.Count - 1 do
    for j := 0 to TStringList(AStringList.Objects[i]).Count - 1 do
      TKeyList(TStringList(AStringList.Objects[i]).Objects[j]).Free;
end;

procedure TdlgCustodyTransferExport.FreeTVRecordData;
var lNode, lChildNode: TFlyNode;
    k: Integer;
begin
  for k := tvRecords.Items.Count - 1 downto 0 do begin
    lNode := tvRecords.Items[k];
    lChildNode := lNode.GetFirstChild;
    repeat
      TKeyList(lChildNode.Data).Free;
      lChildNode := lNode.GetNextChild(lChildNode);
    until lChildNode = nil;
  end;
end;

procedure TdlgCustodyTransferExport.FreeWithSubLists(AStringList: TStringList);
var k: Integer;
begin
  for k := AStringList.Count - 1 downto 0 do
    AStringList.Objects[k].Free;
  AStringList.Free;
end;

procedure TdlgCustodyTransferExport.NextTable(const TableName: string);
begin
  frmMain.ProgressBar.Position := frmMain.ProgressBar.Position + 1;
  frmMain.Status.Panels[0].Text := Format(ResStr_ReassigningCustody, [TableName]);
  Application.ProcessMessages;
end;

procedure TdlgCustodyTransferExport.PopulateTVRecords(AStringList: TStringList);
var i, j: Integer;
    lStringList: TStringList;
    lNode, lChildNode: TFlyNode;
begin
  {for i := AStringList.Count - 1 downto 0 do
    AStringList.Strings[i] := TCustodyInfo.MasterCaption(AStringList.Strings[i]);}
  AStringList.Sort;
  tvRecords.Items.BeginUpdate;
  for i := 0 to AStringList.Count - 1 do begin
    lNode := tvRecords.Items.Add(nil, AStringList.Strings[i]);
    lNode.ImageIndex := INDEX_CHECKED;
    lNode.SelectedIndex := INDEX_CHECKED;
    lStringList := TStringList(AStringList.Objects[i]);
    lStringList.Sort;
    for j := 0 to lStringList.Count - 1 do begin
      lChildNode := tvRecords.Items.AddChild(lNode, lStringList.Strings[j]);
      lChildNode.ImageIndex := INDEX_CHECKED;
      lChildNode.SelectedIndex := INDEX_CHECKED;
      lChildNode.Data := lStringList.Objects[j];
    end;
  end;
  tvRecords.Items.EndUpdate;
end;

procedure TdlgCustodyTransferExport.Proceed(ARelinquisher: TCustodyRelinquisher);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    frmMain.ProgressBar.Min := 0;
    frmMain.ProgressBar.Max := ARelinquisher.TableCount;
    frmMain.ProgressBar.Position := 0;
    frmMain.Status.Panels[0].Text := ResStr_TransferringCustody;
    ARelinquisher.OnNextTable := NextTable;
    ARelinquisher.RelinquishAssignedCustody(dmGeneralData.qryAllPurpose, eNewCustodian.Text);
  finally
    DefaultCursor(lCursor);
    frmMain.ProgressBar.Position := 0;
    frmMain.Status.Panels[0].Text := '';
  end;
  ModalResult := mrOK;
end;

procedure TdlgCustodyTransferExport.TalkToUser(ARelinquisher: TCustodyRelinquisher);
begin
  if ARelinquisher.DataAssigned then
    case MessageDlg(Format(ResStr_Proceed, [eNewCustodian.Text]), mtWarning, mbYesNoCancel, 0) of
      idYes   : Proceed(ARelinquisher);
      idCancel: ModalResult := mrCancel;
    else // Do Nothing
    end
  else
    if MessageDlg(ResStr_NoneSelected, mtWarning, mbOKCancel, 0) = idOK then
      ModalResult := mrOK;
end;

procedure TdlgCustodyTransferExport.tvRecordsChange(Sender: TObject; Node: TFlyNode);
var lKeyList: TEditableKeyList;
begin
  tvRecordDetails.Items.BeginUpdate;
  try
    tvRecordDetails.Items.Clear;
    if Node.Level = 1 then begin
      lKeyList := TEditableKeyList.Create;
      try
        lKeyList.SetTable(Node.Parent.Caption);
        lKeyList.AddItem(Node.Caption, '');
        FBrowser.SetTreeViewItem(lKeyList);
      finally lKeyList.Free end;
    end;
  finally
    tvRecordDetails.Items.EndUpdate;
  end;
end;

procedure TdlgCustodyTransferExport.tvRecordsClick(Sender: TObject);
var ptPos: TPoint;
    HT: THitTests;
begin
  if (tvRecords.Items.Count > 0) and (tvRecords.Selected <> nil) then begin
    ptPos.X := Mouse.CursorPos.X;
    ptPos.Y := Mouse.CursorPos.Y;
    ptPos := tvRecords.ScreenToClient(ptPos);
    HT := tvRecords.GetHitTestInfoAt(ptPos.X,ptPos.Y);
    if htOnIcon in HT then
      with tvRecords.Selected do begin
        // toggle selection
        if ImageIndex = INDEX_UNCHECKED then ImageIndex := INDEX_CHECKED
        else ImageIndex := INDEX_UNCHECKED;
        SelectedIndex := ImageIndex;
        if Level = 0 then CheckChildren(tvRecords.Selected)
        else CheckParent(tvRecords.Selected.Parent);
      end;
  end;
end;

//==============================================================================
procedure TdlgCustodyTransferExport.GatherAdditionalKeys(AKeyList: TEditableKeyList);
var lKey        : TKeyString;
    i           : Integer;
    lSurveyKeys, lEventKeys, lSampleKeys,
    lTaxOccKeys, lBioOccKeys, lLocationKeys,
    lNameKeys, lRefKeys, lSurveyEventOwnerKeys,
    lSurveyEventOwnerTypeKeys, lSurveyTagKeys,
    lConceptKeys                            : TStringList;
    siteID       : String;
  //----------------------------------------------------------------------------
  procedure InitialiseStringLists(AStringLists: array of TStringList);
  var i: Integer;
  begin
    for i := Low(AStringLists) to High(AStringLists) do begin
      AStringLists[i].Sorted := true;
      AStringLists[i].Duplicates := dupIgnore;
    end;
  end;
  //----------------------------------------------------------------------------
  procedure TransferKeysToKeyList(AStringLists: array of TStringList; ATableNames: array of String);
  var i, j: Integer;
  begin
    AKeyList.Clear;
    for i := Low(AStringLists) to High(AStringLists) do
      for j := 0 to AStringLists[i].Count - 1 do
        AKeyList.AddItem(AStringLists[i][j], ATableNames[i]);
  end;
  //----------------------------------------------------------------------------
  procedure AddKeyToList(AField: TField; AList: TStringList);
  begin
    if not AField.IsNull then
      AList.Add(AField.AsString);
  end;

  //----------------------------------------------------------------------------
  // Fills in the string list with matches from the specified table, filtered
  // by the where clause.
  procedure GetBasicKeys(keyList: TStringList; table, where: String);
  begin
    with dmGeneralData.qryAllPurpose do begin
      Close;

      SQL.Clear;
      SQL.Add('SELECT     KL.KeyField ' +
              'FROM       "#Key_List"     KL ' +
              'INNER JOIN ' + table + ' T ' +
              '        ON T.' + table + '_Key = KL.KeyField ' +
              'WHERE      T.Custodian = ''' + siteID + '''' +
              '       AND KL.ItemTable ' + where);

      Open;
      while not Eof do begin
        AddKeyToList(FieldByName('KeyField'), keyList);
        Next;
      end;
      Close;
    end;
  end;

  //----------------------------------------------------------------------------
  // Fills in the string list with matches from the specified table, where the
  // ItemTable matches a list of tables.
  procedure GetBasicKeysMultiTable(keyList: TStringList; mainTable, subTables: String);
  begin
    GetBasicKeys(keyList, mainTable, 'IN (' + subTables + ')');
  end;
 
  //----------------------------------------------------------------------------
  // Fills in the string list with matches from the specified table.
  procedure GetBasicKeysSingleTable(keyList: TStringList; table: String);
  begin
    GetBasicKeys(keyList, table, '= ''' + table + '''');
  end;

  //----------------------------------------------------------------------------
  procedure GetAdditionalObservationKeys;
  begin
    with dmGeneralData.qryAllPurpose do begin
      Close;

      SQL.Clear;
      SQL.Add(Format('EXECUTE usp_ObservationKeys_Get_FromKeylist %d', [Integer(FIncludeObservations)]));

      Open;
      while not Eof do begin
        // Main records
        if FieldByName('Survey_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Key'), lSurveyKeys);
        if FieldByName('Survey_Event_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Event_Key'), lEventKeys);
        if FieldByName('Sample_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Sample_Key'), lSampleKeys); 
        if FieldByName('Taxon_Occurrence_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Taxon_Occurrence_Key'), lTaxOccKeys);
        if FieldByName('Biotope_Occurrence_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Biotope_Occurrence_Key'), lBioOccKeys);
        // Associated names
        if FieldByName('Run_By_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Run_By'), lNameKeys); 
        if FieldByName('SE_Recorder_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('SE_Recorder_Key'), lNameKeys);
        // Associated locations                                   
        if FieldByName('Event_Location_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Event_Location_Key'), lLocationKeys);
        if FieldByName('Sample_Location_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Sample_Location_Key'), lLocationKeys);
        // Associated references      
        if FieldByName('Survey_Reference_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Reference'), lRefKeys);
        if FieldByName('Event_Reference_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Event_Reference'), lRefKeys);
        if FieldByName('Sample_Reference_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Sample_Reference'), lRefKeys);
        if FieldByName('Taxon_Occurrence_Reference_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Taxon_Occurrence_Reference'), lRefKeys);
        if FieldByName('Biotope_Occurrence_Reference_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Biotope_Occurrence_Reference'), lRefKeys);
        // Survey sub keys
        if FieldByName('Survey_Event_Owner_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Event_Owner_Key'), lSurveyEventOwnerKeys); 
        if FieldByName('Survey_Event_Owner_Type_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Event_Owner_Type_Key'), lSurveyEventOwnerTypeKeys);
        if FieldByName('Survey_Tag_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Survey_Tag_Key'), lSurveyTagKeys);
        if FieldByName('Concept_Custodian').AsString = siteID then
          AddKeyToList(FieldByName('Concept_Key'), lConceptKeys);
        Next;
      end;
      Close;
    end;
  end;  // GetAdditionalObservationKeys
  //----------------------------------------------------------------------------
  procedure GetAdditionalLocationKeys;
  begin
    with dmGeneralData.qryAllPurpose do begin
      Close;  

      SQL.Clear;
      SQL.Add('EXECUTE usp_LocationKeys_Get_FromKeylist ' + siteID);

      Open;
      while not Eof do begin
        AddKeyToList(FieldByName('Location_Key'), lLocationKeys);
        Next;
      end;
      Close;
    end;
  end;
  //----------------------------------------------------------------------------
begin
  siteID := AppSettings.SiteID;
  lSurveyKeys   := TStringList.Create;
  lEventKeys    := TStringList.Create;
  lSampleKeys   := TStringList.Create;
  lTaxOccKeys   := TStringList.Create;
  lBioOccKeys   := TStringList.Create;
  lLocationKeys := TStringList.Create;
  lNameKeys     := TStringList.Create;
  lRefKeys      := TStringList.Create;
  lSurveyEventOwnerKeys:= TStringList.Create;
  lSurveyEventOwnerTypeKeys:= TStringList.Create;
  lSurveyTagKeys:= TStringList.Create;
  lConceptKeys  := TStringList.Create;

  InitialiseStringLists([lSurveyKeys, lEventKeys, lSampleKeys, lTaxOccKeys,
                         lBioOccKeys, lLocationKeys, lNameKeys, lRefKeys,
                           lSurveyEventOwnerKeys, lSurveyEventOwnerTypeKeys,
                           lSurveyTagKeys, lConceptKeys]);

  dmGeneralData.ExecuteSQL('CREATE TABLE #Key_List (' +
                               'KeyField CHAR(16)     COLLATE database_default,' +
                               'ItemTable VARCHAR(30) COLLATE database_default) ' +
                           'CREATE INDEX IDX_KeyField on #Key_List (KeyField)',
                           ResStr_UnableToCreateTempTable);

  try
    // Fetch the additional keys to add to the tree
    for i := 0 to AKeyList.Header.ItemCount - 1 do begin
      lKey := AKeyList.Items[i].KeyField1;
      dmGeneralData.ExecuteSQL(Format('INSERT INTO #Key_List VALUES (''%s'', ''%s'')',
                                      [lKey,
                                       AKeyList.ItemTable[i]]),
                               ResStr_UnableToPopulateTempTable,
                               False);
    end;

    GetBasicKeysSingleTable(lSurveyKeys, 'Survey');  
    GetBasicKeysSingleTable(lEventKeys, 'Survey_Event');
    GetBasicKeysSingleTable(lSampleKeys, 'Sample');
    GetBasicKeysSingleTable(lTaxOccKeys, 'Taxon_Occurrence');
    GetBasicKeysSingleTable(lBioOccKeys, 'Biotope_Occurrence');
    GetBasicKeysSingleTable(lLocationKeys, 'Location');
    GetBasicKeys(lRefKeys, 'Source', '= ''Reference''');
    GetBasicKeysMultiTable(lNameKeys, 'Name', '''Name'', ''Individual'', ''Organisation''');
    GetBasicKeysSingleTable(lSurveyEventOwnerKeys, 'Survey_Event_Owner');
    GetBasicKeysSingleTable(lSurveyEventOwnerTypeKeys, 'Survey_Event_Owner_Type');
    GetBasicKeysSingleTable(lSurveyTagKeys, 'Survey_Tag');
    GetBasicKeysSingleTable(lConceptKeys, 'Concept');

    GetAdditionalObservationKeys;
    GetAdditionalLocationKeys;
    // Get all the found keys back in the keylist
    TransferKeysToKeyList([lSurveyKeys, lEventKeys, lSampleKeys, lTaxOccKeys,
                           lBioOccKeys, lLocationKeys, lNameKeys, lRefKeys,
                           lSurveyEventOwnerKeys, lSurveyEventOwnerTypeKeys,
                           lSurveyTagKeys, lConceptKeys],
                          ['SURVEY', 'SURVEY_EVENT', 'SAMPLE', 'TAXON_OCCURRENCE',
                           'BIOTOPE_OCCURRENCE', 'LOCATION', 'NAME', 'SOURCE',
                           'SURVEY_EVENT_OWNER', 'SURVEY_EVENT_OWNER_TYPE',
                           'SURVEY_TAG', 'CONCEPT']);
    // Resort, just in case
    AKeyList.SortByTable;
  finally
    lSurveyKeys.Free;
    lEventKeys.Free;
    lSampleKeys.Free;
    lTaxOccKeys.Free;
    lBioOccKeys.Free;
    lLocationKeys.Free;
    lNameKeys.Free;
    lRefKeys.Free;
    lSurveyEventOwnerKeys.Free;
    lSurveyEventOwnerTypeKeys.Free;
    lSurveyTagKeys.Free;
    lConceptKeys.Free;

    dmGeneralData.ExecuteSQL('DROP TABLE #Key_List', ResStr_UnableToDropTempTable);
  end;
end;  // GatherAdditionalKeys

//==============================================================================
end.
