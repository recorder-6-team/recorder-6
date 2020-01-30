{===============================================================================
  Unit:        ImportAnalysis

  Defines:     TfraImportAnalysis

  Description:

  Model:       ImportWizard

  Last revision information:
    $Revision: 41 $
    $Date: 15/06/10 15:21 $
    $Author: Andrewkemp $

===============================================================================}

unit ImportAnalysis;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWBasePage, ComCtrls, StdCtrls, ExtCtrls, HTMLView, DataClasses, Constants,
  DBBrowserRapTree, ExceptionForm, TempData_ADO, Recorder2000_TLB, MarkupDocs,
  JNCCXMLDoc, exgrid, RapTree, IWSettings, DatabaseUtilities, ImportComplete, ExternalFilter,
  ImageListButton;

resourcestring
  ResStr_InstanceMissing     = 'A problem has occurred - the addin is not available.';
  ResStr_InvalidZipFile      = 'The specified ZIP file is not a valid NBN Database Import file.';
  ResStr_NotSpecified        = 'Not specified';
  ResStr_CompareNotAll       = 'Not all records could be compared on the basis of date stamps.';
  ResStr_TableNotRecord      = 'Cannot obtain record information for a table level node.';
  ResStr_ClearDuplicateItems = 'All duplicate items have been resolved';
  ResStr_ResultsDescription  = 'Description: Results of validation of import file';
  ResStr_On                  = 'on';
  ResStr_At                  = 'at';

  ResStr_From            = 'From:'#9#9'%s'#9#9;
  ResStr_DateExported    = 'Date Exported: %s';
  ResStr_Dataset         = 'Dataset:'#9#9'%s';
  ResStr_Title           = 'Title: '#9#9'%s';
  ResStr_Restriction     = 'Restrictions: '#9'%s';
  ResStr_SuccessfulRead  = '%s successfully read, of which there are:';
  ResStr_Item            = 'Item';
  ResStr_Items           = 'Items';
  ResStr_NewItem         = 'New item';
  ResStr_NewItems        = 'New items';
  ResStr_DuplicatedItem  = 'Duplicated item';
  ResStr_DuplicatedItems = 'Duplicated items';
  ResStr_InvalidItem     = 'Invalid items';
  ResStr_InvalidItems    = 'Invalid items';


  //Status
  ResStr_ImportingData = 'Importing Data...';

type
  EImportAnalysis = class(TExceptionPath);
  EInvalidZipFile = class(EImportAnalysis);

  {-----------------------------------------------------------------------------
    Frame displayed when importing ANY type of data that requires a temporary import database
    analysis (including validation and duplicate checking) to be checked by the user before
    final import of the data into Recorder.
    When importing NBN XML or Zipped Access Database format files, this page is the first and
    only wizard page displayed.  In this case the Previous button is hidden.  This is also the
    case when an addin called IRecorder2000.CheckDatabase.
    Importing data in the case of NBN Data is a two-stage process.  Firstly, the selected 
    import file is analysed for invalid and duplicate records.  Once the analysis has finished 
    the user is then presented with controls for dealing with duplicate records, and for 
    viewing invalid records, before choosing to finish and import the selected data or 
    aborting the whole process.  NBN Data (XML) and Zipped Access 97 Database are the native 
    formats supported, but the software can be configured to import data from any format that 
    supports the software's Import COM interface.  Available to system managers only.
    Data import of dictionary update files is handled in identical fashion to other data 
    imported into the system using XML files.  Only installed checklists may be updated using 
    a data import.
    When the wizard is completed, a progress bar is displayed as the data is copied from the
    temporary import database into the main database.  At the end of this process, a dialog is
    displayed confirming the number of records imported and the number rejected.  If records 
    are rejected, then the details are stored in a file 'ImportRejects'txt' and the user is
    informed of the location of this file.
  }
  TfraImportAnalysis = class (TBasePage)
    lblImportDetails: TLabel;
    lblInvalidData: TLabel;
    lblInvalidDetails: TLabel;
    mmDetails: TMemo;
    mmInvalidDetails: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    pcValidation: TPageControl;
    pnlDuplicateTab: TPanel;
    pnlInvalidTab: TPanel;
    reXML: TRichEdit;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    tsDuplicateItems: TTabSheet;
    tsInvalidItems: TTabSheet;
    tvInvalidData: TRapidTree;
    Panel6: TPanel;
    pnlOriginalDataTree: TPanel;
    Label1: TLabel;
    SplitDuplicateImported: TSplitter;
    pnlImportedDataTree: TPanel;
    Label2: TLabel;
    SplitImportedOriginal: TSplitter;
    pnlDuplicateDataTree: TPanel;
    Label3: TLabel;
    tvImportedData: TRapidTree;
    tvOriginalData: TRapidTree;
    pnlInvalidDataTree: TPanel;
    SplitInvalidData: TSplitter;
    pnlInvalidDataDetails: TPanel;
    SplitInvalidDetails: TSplitter;
    pnlSaveInvalidData: TPanel;
    btnSaveDetails: TImageListButton;
    dlgSaveDetails: TSaveDialog;
    Panel3: TPanel;
    Panel5: TPanel;
    btnAllImported: TButton;
    btnReject: TButton;
    btnAccept: TButton;
    btnAllOriginal: TButton;
    btnAllLatest: TButton;
    lblSurvey: TLabel;
    lblSurveyName: TLabel;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnRejectClick(Sender: TObject);
    procedure btnAllImportedClick(Sender: TObject);
    procedure btnAllOriginalClick(Sender: TObject);
    procedure btnAllLatestClick(Sender: TObject);
    procedure pnlInvalidTabClick(Sender: TObject);
    procedure pnlDuplicateTabClick(Sender: TObject);
    procedure tvInvalidDataChange(Sender: TObject; Node: TFlyNode);
    procedure DuplicateItemsResize(Sender: TObject);
    procedure SplitDuplicateImportedMoved(Sender: TObject);
    procedure InvalidItemsResize(Sender: TObject);
    procedure btnSaveDetailsClick(Sender: TObject);
  private
    FDBImportUtil: TDBBrowserUtility;
    FDBOrigUtil: TDBBrowserUtility;
    FCanProceed: boolean;
    FDuplicates: TStringList;
    FCancelled: boolean;
    procedure DuplicateNodeTreeChange(Sender: TObject; Node: TFlyNode);
    procedure SetImportButtonState(Node: TFlyNode);
    procedure SetupDetailMemo(iDisplayDescription: boolean);
    function GetTagText(const iParentTag, iName: string): string;
    procedure StoreItemInList(AList: TStringList; ANode: TFlyNode);
    procedure DeleteNode(iNode: TFlyNode);
    function GetRecordFromNode(iNode: TFlynode): TSingleRecord;
    function RejectUsingDateComparison(iOrigRecord,
      iImportRecord: TStringList; iNode: TFlyNode): boolean;
    function CompareSpecificDateAndReject(const iField: string;
      iOrigRecord, iImportRecord: TStringList; iNode: TFlyNode): boolean;
    procedure RebuildIndexes;
    procedure SetTabs(ATabOn, ATabOff: TPanel);
    procedure SaveInvalidItemsFilter;
    procedure SaveInvalidItems;
  protected
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    procedure LoadContent; override;
    function GetHtmlImageName: String; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    destructor Destroy; override;
    procedure SaveContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  IWResourceStrings, IWConstants, GeneralFunctions, Maintbar,
  ApplicationSettings, ComObj, DatabaseAccessADO, GeneralData, FormActions,
  Treecoll, DBMerger, ImportWizard, OnlineHelp, DefaultPaths, ADODB, DB,
  StrUtils, FileSelect;

resourcestring
  ResStr_FilterCSV         = 'CSV (comma delimited) (*.csv)';
  ResStr_TableName         = 'Table Name';
  ResStr_Key               = 'Record Key';
  ResStr_ErrorMessage      = 'Error Message';
  ResStr_OriginalRowNumber = 'Original Row Number';

const
  CHANGED_DATE = 'CHANGED_DATE';
  ENTRY_DATE = 'ENTRY_DATE';

  DETAILS_HEIGHT_FULL = 221;
  DETAILS_HEIGHT_SMALL = 61;

{-==============================================================================
    TfraImportAnalysis
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfraImportAnalysis.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited;
  FDBImportUtil := TDBBrowserUtility.Create(tvImportedData, Settings.TempData.Database);
  FDBOrigUtil := TDBBrowserUtility.Create(tvOriginalData, dmDatabase.dbLocal);
  FDuplicates := TStringList.Create;
  FDuplicates.Sorted := true;
  HelpContext := IDH_IWIMPORTANALYSIS;
end;  // TfraImportAnalysis.Create

{-------------------------------------------------------------------------------
}
destructor TfraImportAnalysis.Destroy;
begin
  FDBImportUtil.Free;
  FDBOrigUtil.Free;
  FDuplicates.Free;
  inherited;
end;  // TfraImportAnalysis.Destroy

{-------------------------------------------------------------------------------
}
function TfraImportAnalysis.GetHasNext: Boolean;
var
  lBullets: TStringList;
begin
  Result := FCanProceed;
  // Update HTML instructions
  lBullets := TStringList.Create;
  try
    if not FCanProceed then
      lBullets.Add(ResStr_ClearDuplicateItems);
    ChangedHTML(lBullets);
  finally
    lBullets.Free;
  end;
end;  // TfraImportAnalysis.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraImportAnalysis.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraImportAnalysis.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraImportAnalysis.GetNextCaption: String;
begin
  Result := ResStr_DoImport;
end;  // TfraImportAnalysis.GetNextCaption

{-------------------------------------------------------------------------------
  Populate the two comparison treeviews according to the item selected in the
    duplicate items treeview
}
procedure TfraImportAnalysis.DuplicateNodeTreeChange(Sender: TObject;
  Node: TFlyNode);
var
  lKeyList: TEditableKeyList;
  lCommaPos: integer;
  lCursor  : TCursor;
begin
  lCursor := HourglassCursor;
  tvImportedData.Items.BeginUpdate;
  tvOriginalData.Items.BeginUpdate;
  try
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
    { Only change if a record node is selected, not a table node }
    if Assigned(Node) and Assigned(Node.Parent) then
    begin
      { Create a key list for the item we wan't to compare - the same list applies
           to both databases }
      lKeyList := TEditableKeyList.Create;
      try
        lKeyList.SetTable(Node.Parent.Text);
        { Get the key values, allowing for join table cases }
        lCommaPos := Pos(', ', Node.Text);
        if lCommaPos = 0 then
          lKeyList.AddItem(Node.Text, '')
        else
          lKeyList.AddItem(Copy(Node.Text, 1, lCommaPos-1),
                           Copy(Node.Text, lCommaPos+2, 255));
        FDBImportUtil.SetTreeViewItem(lKeyList);
        FDBOrigUtil.SetTreeViewItem(lKeyList);
      finally
        lKeyList.Free;
      end;
    end; // if
    SetImportButtonState(Node);
  finally // reset cursor and allow update of treeviews
    tvOriginalData.Items.EndUpdate;
    tvImportedData.Items.EndUpdate;
    DefaultCursor(lCursor);
  end; // try..finally
end;

{-------------------------------------------------------------------------------
  Enable or disable the Import button when all the items in tcDuplicateData
    have been accounted for.  Also ensures comparison treeviews area cleared
    as appropriate
}
procedure TfraImportAnalysis.SetImportButtonState(Node: TFlyNode);
begin
  FCanProceed := (Settings.DuplicateNodeTree.Items.Count=0);
  btnAllImported.Enabled := not FCanProceed;
  btnAllOriginal.Enabled := not FCanProceed;
  btnAllLatest.Enabled := not FCanProceed;
  btnAccept.Enabled := Node<>nil;
  btnReject.Enabled := Node<>nil;
  if Node = nil then
  begin
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  end else if Node.Parent = nil then
  begin
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  end;
  ChangedContent;
end;  // SetImportButtonState

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.LoadContent;
var
  I: Integer;

  procedure CopyItemData(Source: TTreeCollection; Destination: TTreeCollection);
  var
    J: Integer;
  begin
    Destination.Data := Source.Data;
    for J := 0 to Source.Count - 1 do
      CopyItemData(Source.Items[J], Destination.Items[J]);
  end;

begin
  inherited;
  pcValidation.ActivePage := tsDuplicateItems;
  // Load the node lists into the tree views.
  with Settings.DuplicateNodeTree do begin
    Parent := pnlDuplicateDataTree;
    Align := alClient;
    OnChange := DuplicateNodeTreeChange;
    Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
    ColCount := 2;
    ColCount := 1;
    if Items.GetFirstNode <> nil then
      Items.GetFirstNode.Selected := True;
  end;
  with tvInvalidData do begin
    Items := Settings.InvalidNodes;
    if Items.GetFirstNode <> nil then
      Items.GetFirstNode.Selected := True;
  end;

  for I := 0 to Settings.InvalidNodes.Count - 1 do
    CopyItemData(Settings.InvalidNodes[I], tvInvalidData.Items[I]);
  SetupDetailMemo(Settings.ImportType in [itZippedAccess, itNBNData]);
  SetImportButtonState(Settings.DuplicateNodeTree.Selected);
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.SetupDetailMemo(iDisplayDescription: boolean);
var lstRestrictionText: String;
    rs: _recordset;
    lSurveyname: String;
begin
  rs:= dmDatabase.ExecuteSQL('SELECT Item_Name' +
    ' FROM Survey '
    + ' Where Survey_Key = '
    + '''' + AppSettings.IWSurveyKey + '''',true);
  if not rs.Eof then
    lSurveyname:= (rs.Fields['Item_Name'].Value);
  lblSurveyName.caption := lSurveyname;
  with mmDetails.Lines do begin
    if iDisplayDescription then
      if Settings.ImportType = itNBNData then begin
        Add(Format(ResStr_From, [GetTagText('dataset_source','dataset_owner')]) +
            Format(ResStr_DateExported, [GetTagText('dataset_source','date_exported')]));
        Add(Format(ResStr_Dataset, [GetTagText('dataset_source','dataset_name')]));
        Add(Format(ResStr_Title, [GetTagText('metadata','dataset_title')]));
        lstRestrictionText := GetTagText('dataset_source','dataset_restrictions');
        if lstRestrictionText <> ResStr_NotSpecified then
          Add(Format(ResStr_Restriction, [lstRestrictionText]));
      end // if iDisplayDescription
      else if Settings.ImportType = itZippedAccess then begin
        Add(Format(ResStr_DateExported, [DateToStr(FileDateToDateTime(FileAge(Settings.TempData.DatabasePath)))]));
        Add(Format(ResStr_Title, [ExtractFileName(Copy(Settings.TempData.DatabasePath, 1, Length(Settings.TempData.DatabasePath) - 4))]));
      end;
    Add('');
    Add(Format(ResStr_SuccessfulRead, [GetPlural(
        Settings.TotalCount - Settings.InvalidCount,
        ResStr_Item,
        ResStr_Items)]));
    Add('    ' + GetPlural(
        Settings.TotalCount - Settings.DuplicateCount - Settings.InvalidCount,
        ResStr_NewItem,
        ResStr_NewItems) + '.');
    Add('    ' + GetPlural(
        Settings.DuplicateCount,
        ResStr_DuplicatedItem,
        ResStr_DuplicatedItems) + '.');
    Add(GetPlural(
        Settings.InvalidCount + Settings.TempData.InvalidTagList.Count,
        ResStr_InvalidItem,
        ResStr_InvalidItems) + '.');
  end;
end;  // SetupDetailMemo

{-------------------------------------------------------------------------------
  Function to locate text in the parsed xml document, from a particular tag
    within the parent tag specified.  Parent tag should be a uniquely
    identifiable tag such as metadata.  }
function TfraImportAnalysis.GetTagText(const iParentTag, iName: string): string;
var
  i: integer;
  lTag: TTag;
begin
  Result := ResStr_NotSpecified;
  try
    lTag := Settings.XMLDoc.GetTagByName(iParentTag);
    for i := 0 to lTag.NestedTags.Count-1 do
    begin
      if TTag(lTag.NestedTags[i]).Name = iName then
      begin
        Result := TTag(lTag.NestedTags[i]).Text;
        break; // from loop - found a result
      end;
    end;
  except on EDTDNotComplete do
    ; // nothing - return not specified
  end; // try
end;  // GetTagText

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.btnAcceptClick(Sender: TObject);
begin
  StoreItemInList(FDuplicates, Settings.DuplicateNodeTree.Selected);
  SetImportButtonState(Settings.DuplicateNodeTree.Selected);
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.btnRejectClick(Sender: TObject);
begin
  StoreItemInList(Settings.Rejections, Settings.DuplicateNodeTree.Selected);
  SetImportButtonState(Settings.DuplicateNodeTree.Selected);
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.btnAllImportedClick(Sender: TObject);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  Settings.DuplicateNodeTree.Items.BeginUpdate;
  try
    with Settings.DuplicateNodeTree do begin
      Selected := Items.GetFirstNode;   // Get first node in tree
      while Selected <> nil do begin
        btnAcceptClick(nil);             // Accept node, and delete it from tree
        Selected := Items.GetFirstNode; // Get next first node, previous has been removed
      end;
    end;
  finally
    Settings.DuplicateNodeTree.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.btnAllOriginalClick(Sender: TObject);
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  Settings.DuplicateNodeTree.Items.BeginUpdate;
  try
    { Loop through tree view, rejecting the record level nodes }
    with Settings.DuplicateNodeTree do begin
      Selected := Items.GetFirstNode;   // Get first node in tree
      while Selected <> nil do begin
        btnRejectClick(nil);             // Reject node and delete it from tree
        Selected := Items.GetFirstNode; // Get next first node, previous has been removed
      end;
    end;
  finally
    Settings.DuplicateNodeTree.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.btnAllLatestClick(Sender: TObject);
var
  lOrigRecord  : TStringList;
  lImportRecord: TStringList;
  lSingleRecord: TSingleRecord;
  lCursor      : TCursor;
  lParentNode, lRecordNode, lDelNode: TFlyNode;
  lRecordCount: Integer;  
  lRecordIndex: Integer;

  procedure CountRecords;
  begin
    lRecordCount := 0;
    lParentNode := Settings.DuplicateNodeTree.Items.GetFirstNode;
    while Assigned(lParentNode) do
    begin
      lRecordNode := lParentNode.GetFirstChild;
      lParentNode := lParentNode.GetNextSibling;
      while Assigned(lRecordNode) do
      begin
        Inc(lRecordCount);
        lRecordNode := lRecordNode.GetNextSibling;
      end;
    end;
  end;
  
begin
  CountRecords;
  lRecordIndex := 0;
  lCursor := HourglassCursor;
  lOrigRecord := TStringList.Create;
  lImportRecord := TStringList.Create;
  frmMain.SetProgress(0);
  Settings.DuplicateNodeTree.Items.BeginUpdate;
  try
    lParentNode := Settings.DuplicateNodeTree.Items.GetFirstNode;
    if lParentNode <> nil then
      // Process all tables, "parent" nodes
      while lParentNode <> nil do begin
        lRecordNode := lParentNode.GetFirstChild;
        // Need to select next parent now, as "RejectUsingDateComparison" might
        // cause it to be deleted.
        lParentNode := lParentNode.GetNextSibling;
        // Process all records for the table
        while lRecordNode <> nil do begin
          { Read the table name and key values }
          lSingleRecord := GetRecordFromNode(lRecordNode);
          // Get next node now, in case current is deleted in next function
          lDelNode := lRecordNode;
          lRecordNode := lRecordNode.GetNextSibling;

          lOrigRecord.Clear;
          { Locate the main db record }
          if CompareText(lSingleRecord.TableName, 'Concept_Lineage')=0 then
            dmGeneralData.GetRecordStrings(lOrigRecord, lSingleRecord.TableName,
                           lSingleRecord.Key1, lSingleRecord.Key2, '''', '')
          else
            dmGeneralData.GetRecordStrings(lOrigRecord, lSingleRecord.TableName,
                           lSingleRecord.Key1, lSingleRecord.Key2);
          dmGeneralData.qryRecordStrings.Connection := Settings.TempData.Database;
          try
            lImportRecord.Clear;
            { Locate the import db record }
            if CompareText(lSingleRecord.TableName, 'Concept_Lineage')=0 then
              dmGeneralData.GetRecordStrings(lImportRecord, lSingleRecord.TableName,
                           lSingleRecord.Key1, lSingleRecord.Key2, '"', '')
            else
              dmGeneralData.GetRecordStrings(lImportRecord, lSingleRecord.TableName,
                           lSingleRecord.Key1, lSingleRecord.Key2, '"');
          finally // for safety, ensure we switch back to the main db
            dmDatabase.SetDatabaseLocal([dmGeneralData.qryRecordStrings]);
          end;
          { If a node is deleted, we don't need to advance to the next item,
             otherwise we must reindex the hierarchy }
          RejectUsingDateComparison(lOrigRecord, lImportRecord, lDelNode);

          Inc(lRecordIndex);
          frmMain.SetProgress(Round(100 * lRecordIndex / lRecordCount));
        end;
      end;
  finally
    lOrigRecord.Free;
    lImportRecord.Free;
    DefaultCursor(lCursor);
    frmMain.TaskFinished;
    Settings.DuplicateNodeTree.Items.EndUpdate;
  end;
  if Settings.DuplicateNodeTree.Items.Count > 0 then
    MessageDlg(ResStr_CompareNotAll, mtInformation, [mbOk], 0);
  SetImportButtonState(Settings.DuplicateNodeTree.Selected);
end;  // bbAllLatestClick


{-------------------------------------------------------------------------------
 Stores the record identified by a node in Settings.DuplicateNodeTree in the list
    supplied, either rejection or duplicate list.  The original node is then
    removed.
}
procedure TfraImportAnalysis.StoreItemInList(AList: TStringList; ANode: TFlyNode);
var lNode  : TFlyNode;
    lCursor: TCursor;
  //----------------------------------------------------------------------------
  procedure AddItemToList(ASingleRecord: TSingleRecord);
  begin
    { Trim down the memory usage as we go as it can get very large if lots of rejected items }
    if AList.Count mod 300 = 0 then
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
    AList.Add(Uppercase(ASingleRecord.TableName) + ';' +
              ASingleRecord.Key1 + ';' + ASingleRecord.Key2);
  end;
  //----------------------------------------------------------------------------
begin
  if ANode <> nil then begin
    lCursor := HourglassCursor;
    try
      if ANode.Count = 0 then
        // No children, one record node to process
        AddItemToList(GetRecordFromNode(ANode))
      else begin
        // Table node, several record nodes to process
        AList.Capacity := AList.Capacity + ANode.Count;
        lNode := ANode.GetFirstChild;
        while lNode <> nil do begin
          AddItemToList(GetRecordFromNode(lNode));
          lNode := lNode.GetNextSibling;
        end;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
  DeleteNode(ANode);
end;  // StoreItemInList

{-------------------------------------------------------------------------------
 Removes the node from the duplicate items tree view.  Removes the
    parent as well if it has no children
}
procedure TfraImportAnalysis.DeleteNode(iNode: TFlyNode);
var
  lParent, lNewFocused: TFlyNode;
  lParentDeleted: Boolean;
begin
  Settings.DuplicateNodeTree.Items.BeginUpdate;
  try
    lParentDeleted := False;
    if iNode <> nil then
    begin
      lParent := iNode.Parent;
      if lParent = nil then lNewFocused := iNode.GetNextSibling
                       else lNewFocused := iNode.GetNextVisible;
      if lNewFocused = nil then
        lNewFocused := iNode.GetPrevVisible;

      { Delete the parent if this is the last child }
      if lParent <> nil then
        if lParent.Count = 1 then
        begin
          // Delete last node within last parent. Update new focused node.
          if lParent.GetNextSibling = nil then
            lNewFocused := lParent.GetPrevVisible;
          Settings.DuplicateNodeTree.Items.Delete(lParent);
          lParentDeleted := True;
        end;
      { If we haven't deleted the whole lot, then delete the item }
      if not lParentDeleted then
        Settings.DuplicateNodeTree.Items.Delete(iNode);

      Settings.DuplicateNodeTree.Selected := lNewFocused;
    end;
    tvImportedData.Items.Clear;
    tvOriginalData.Items.Clear;
  finally
    Settings.DuplicateNodeTree.Items.EndUpdate;
  end;
end;  // DeleteNode

{-------------------------------------------------------------------------------
  Reads the information required to identify a record in the DB from a single
   node in tcDuplicateData
}
function TfraImportAnalysis.GetRecordFromNode(iNode: TFlynode): TSingleRecord;
var
  lKeyText: string;
begin
  if iNode.Parent = nil then
    raise EImportAnalysis.Create(ResStr_TableNotRecord);
  lKeyText := iNode.Text;
  if Pos(', ', lKeyText) = 0 then
  begin
    Result.Key1 := lKeyText;
    Result.Key2 := '';
  end else
  begin // split join key, so extract each keystring
    Result.Key1 := Copy(lKeyText, 1, Pos(', ', lKeyText)-1); // up to ', '
    Result.Key2 := Copy(lKeyText, Pos(', ', lKeyText)+2, 255); // skip ', ', then rest of string
  end;
  Result.TableName := iNode.Parent.Text;
end;  // GetRecordFromNode

{-------------------------------------------------------------------------------
  Rejects an import based on date time stamps and comparison with an existing
    record. If changed date or entry date present, then we can make our
    comparison, else we must ignore the node.  Returns true if a succesful
    comparison was achieved
}
function TfraImportAnalysis.RejectUsingDateComparison(iOrigRecord,
  iImportRecord: TStringList; iNode: TFlyNode): boolean;
begin
  Result := False; // comparison couldn't be done on date - default
  if iOrigRecord.IndexOfName(CHANGED_DATE)<>-1 then
  begin
    if (iOrigRecord.Values[CHANGED_DATE]='') or (iImportRecord.Values[CHANGED_DATE]='') then
    begin
      if iImportRecord.Values[CHANGED_DATE]<>'' then begin
        { must have a changed date, so accept this record - just remove from treeview }
        StoreItemInList(FDuplicates, iNode);
        Result := True; // successful comparison
      end else
        { One or other changed date missing, so try an entry date comparison }
        Result := CompareSpecificDateAndReject(ENTRY_DATE, iOrigRecord,
                                               iImportRecord, iNode);
    end else // do changed date comparison
      Result := CompareSpecificDateAndReject(CHANGED_DATE, iOrigRecord,
                                             iImportRecord, iNode);
  end else
  if iOrigRecord.IndexOfName(ENTRY_DATE)<>-1 then
    { Try an entry_date comparison }
    Result := CompareSpecificDateAndReject(ENTRY_DATE, iOrigRecord,
                                           iImportRecord, iNode);
end;  // RejectUsingDateComparison

{-------------------------------------------------------------------------------
  Once we have decided whether to compare records on entry date or changed date,
    actually do the comparison and reject the import if relevant.
}
function TfraImportAnalysis.CompareSpecificDateAndReject(const iField: string;
  iOrigRecord, iImportRecord: TStringList; iNode: TFlyNode): boolean;
var
  lOrigDate, lImportDate: TDateTime;
begin
  Result := True;// default is successful comparison
  { If either value is missing, ignore the node }
  if (iOrigRecord.Values[iField]='') or (iImportRecord.Values[iField]='') then
    Result := False
  else
  begin
    lOrigDate:=StrToDateTime(iOrigRecord.Values[iField]);
    lImportDate:=StrToDateTime(iImportRecord.Values[iField]);
    if lOrigDate >= lImportDate then
      StoreItemInList(Settings.Rejections, iNode)
    else
      DeleteNode(iNode);
  end;
end;  // CompareSpecificDateAndReject

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.SaveContent;
var
  lRecordsCopied: integer;
  lRejectCount  : integer;
  lCursor       : TCursor;
  lComplete     : TdlgImportComplete;
  lImportRejectsFile: TFileName;
begin
  lCursor := HourglassCursor;
  FCancelled       := False;
  try
    frmMain.SetStatus(ResStr_ImportingData);
    frmMain.ProgressBar.TaskPosition := 0;
    lRecordsCopied := Settings.TempData.CopyRecordsIntoMainDB(
        FDuplicates, Settings.Rejections, lImportRejectsFile);
    // Check tables with preferred records (eg location names) have 1 and only 1 preferred
    dmGeneralData.CheckPreferred;
    { Ensure sample type list correct }
    dmFormActions.UpdateSampleTypeList;
  finally
    DefaultCursor(lCursor);
    frmMain.TaskFinished;
  end;
  if not FCancelled then begin
    lRejectCount :=  Settings.TotalCount + Settings.TempData.InvalidTagList.Count -
                lRecordsCopied - Settings.DuplicateCount + FDuplicates.Count -
                Settings.Rejections.Count;
    lComplete := TdlgImportComplete.Create(self);
    try
      lComplete.SetDetails(Settings, lRecordsCopied, lRejectCount, lImportRejectsFile);
      lComplete.ShowModal;
    finally
      lComplete.free;
    end;
    frmMain.GetForm(TfrmImportWizard).BringToFront;
  end;
  RebuildIndexes;
end;  // bbImportClick

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.RebuildIndexes;
begin
  if Settings.TempData.RebuildTaxonIndex then begin
    RebuildIndexTaxonName(dmGeneralData.qryAllPurpose, frmMain.SetStatus, frmMain.SetProgress);
    Application.ProcessMessages;
    RebuildIndexTaxonSynonym(dmGeneralData.qryAllPurpose, frmMain.SetStatus, frmMain.SetProgress);
    frmMain.TaskFinished;
  end;
end;  // RebuildIndexes

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.pnlInvalidTabClick(Sender: TObject);
begin
  pcValidation.ActivePage := tsInvalidItems;
  SetTabs(pnlInvalidTab, pnlDuplicateTab);
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.SetTabs(ATabOn, ATabOff: TPanel);
begin
  ATabOn.Top     := 137;
  ATabOff.Top    := 138;
  ATabOn.Height  :=  20;
  ATabOff.Height :=  18;
  ATabOn.Color   := clWhite;
  ATabOff.Color  := clBtnFace;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.pnlDuplicateTabClick(Sender: TObject);
begin
  pcValidation.ActivePage := tsDuplicateItems;
  SetTabs(pnlDuplicateTab, pnlInvalidTab);
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.tvInvalidDataChange(Sender: TObject;
  Node: TFlyNode);
begin
  inherited;
  mmInvalidDetails.Clear;
  mmInvalidDetails.Height := DETAILS_HEIGHT_FULL;
  mmInvalidDetails.Align := alClient;
  reXML.Visible := False;
  SplitInvalidDetails.Visible := False;
  if Node.Data<>nil then
    if TObject(Node.Data) is TMessage then
    begin
      mmInvalidDetails.Lines.Add(TMessage(Node.Data).Msg);
    end
    else if TObject(Node.Data) is TTag then
    begin
      mmInvalidDetails.Height := DETAILS_HEIGHT_SMALL;
      mmInvalidDetails.Align := alTop;
      reXML.Clear;
      reXML.Visible := True;    
      SplitInvalidDetails.Visible := True;
      mmInvalidDetails.Lines.Add(TTag(Node.Data).InvalidationText);
      Settings.XMLDoc.PopulateErrorReport(reXML,
                                          TTag(Node.Data).DocRow,
                                          TTag(Node.Data).DocBlockPos);
    end;
end;

{-------------------------------------------------------------------------------
}
function TfraImportAnalysis.GetHtmlImageName: String;
begin
  Result := 'Import.jpg';
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.DuplicateItemsResize(Sender: TObject);
begin
  inherited;
  if Sender = pnlDuplicateDataTree then Settings.DuplicateNodeTree.Repaint else
  if Sender = pnlImportedDataTree then tvImportedData.Repaint else
  if Sender = pnlOriginalDataTree then tvOriginalData.Repaint;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.SplitDuplicateImportedMoved(Sender: TObject);
begin
  inherited;
  // This is because of what appears to be a slight bug with mutiple splitters, going the
  // same way.
  if SplitImportedOriginal.Left = SplitDuplicateImported.Left +
                                  SplitDuplicateImported.Width then
    SplitImportedOriginal.Left := pnlImportedDataTree.Left + pnlImportedDataTree.Width;
end;

{-------------------------------------------------------------------------------
}
procedure TfraImportAnalysis.InvalidItemsResize(Sender: TObject);
begin
  inherited;
  if Sender = pnlInvalidDataTree then tvInvalidData.Repaint;
end;

{-------------------------------------------------------------------------------
  Save results of validation to an appropriate file.
}
procedure TfraImportAnalysis.btnSaveDetailsClick(Sender: TObject);
begin
  inherited;
  dlgSaveDetails.InitialDir := GetProgramDataFolder(PATH_USER_FILES);

  case settings.ImportType of
    itNBNData,
    itZippedAccess,
    itAddin:
      SaveInvalidItemsFilter;
    else
      SaveInvalidItems;
  end;
end;

{-------------------------------------------------------------------------------
  Save results of validation to External Filter file.
}
procedure TfraImportAnalysis.SaveInvalidItemsFilter;
var
  nodeParent, nodeChild: TFlyNode;
begin
  if dlgSaveDetails.Execute then
    with TStringList.Create do
      try
        Add(ResStr_ResultsDescription + ' ' + Settings.SourceDataFile
            + ' ' + ResStr_On + ' ' + DateToStr(Now)
            + ' ' + ResStr_At + ' ' + TimeToStr(Now));

        nodeParent := tvInvalidData.Items.GetFirstNode;
        while Assigned(nodeParent) do begin
          nodeChild := nodeParent.GetFirstChild;
          while Assigned(nodeChild) do begin
            Add(Format(
                '%s'#9'%s'#9'%s',
                [nodeParent.Text,
                 nodeChild.Text,
                 StringReplace(
                     StringReplace(
                         StringReplace(TMessage(nodeChild.Data).Msg, #13#10, ', ', [rfReplaceAll]),
                         #13, ', ', [rfReplaceAll]),
                     #10, ', ', [rfReplaceAll])]));
            nodeChild := nodeChild.GetNextSibling;
          end;
          nodeParent := nodeParent.GetNextSibling;
        end;

        ForceDirectories(ExtractFilePath(dlgSaveDetails.FileName));
        SaveToFile(dlgSaveDetails.FileName);
      finally
        Free;
      end;
end;

{-------------------------------------------------------------------------------
  Save rejected rows to a CSV file.
}
procedure TfraImportAnalysis.SaveInvalidItems;
var
  Output: TextFile;
  Query: TADOQuery;
  CurrentTable: String;
  I: Integer;
  Rejection: TSingleRecord;
  Message: String;
  PrimaryKey: TPrimaryKey;

  {-----------------------------------------------------------------------------
    Returns a structure identfying the specified record.
  }
  function MakeRejection(const KeyData: String): TSingleRecord;
  var
    P1, P2: Integer;
  begin
    P1 := PosEx(';', KeyData);
    P2 := PosEx(';', KeyData, P1 + 1);
    Result.TableName := LeftStr(KeyData, P1 - 1);
    Result.Key1 := MidStr(KeyData, P1 + 1, P2 - (P1 + 1));
    Result.Key2 := MidStr(KeyData, P2 + 1, Length(KeyData) - P2);
  end;

  {-----------------------------------------------------------------------------
    Prepares Query to look up rows in the import file that correspond to the
    current table.
  }
  procedure PrepareQuery;
  begin
    Query.SQL.Text := Format(
        'SELECT i.*'
        + ' FROM #RN_%s AS n'
        + ' INNER JOIN #SMImport AS i'
        + ' ON i.__ID__ = n.Record_No'
        + ' WHERE n.%s = :Key1',
        [CurrentTable, PrimaryKey.Key1]);

    if PrimaryKey.Key2 <> '' then
    begin
      Query.SQL.Append(Format('AND n.%s = :Key2', [PrimaryKey.Key2]));
    end;
    Query.Prepared := True;
  end;

  {-----------------------------------------------------------------------------
    Prepares Query to look up rows in the import file that correspond to the
    current rejected record.
  }
  procedure ConfigureQueryForRejection;
  begin
    if Rejection.TableName <> CurrentTable then
    begin
      CurrentTable := Rejection.TableName;
      PrimaryKey := dmDatabase.SplitPrimaryKey(CurrentTable);
      PrepareQuery;
    end;

    Query.Parameters.ParamValues['Key1'] := Rejection.Key1;
    if PrimaryKey.Key2 <> '' then
    begin
      Query.Parameters.ParamValues['Key2'] := Rejection.Key2;
    end;
  end;

  {-----------------------------------------------------------------------------
    Writes a header line to the output file.
  }
  procedure WriteHeader;
  begin
    Writeln(
        Output,
        AnsiQuotedStr(ResStr_TableName, '"'), ',',
        AnsiQuotedStr(ResStr_Key, '"'), ',',
        AnsiQuotedStr(ResStr_ErrorMessage, '"'), ',',
        AnsiQuotedStr(ResStr_OriginalRowNumber, '"'));
  end;

  {-----------------------------------------------------------------------------
    Removes line break sequences from the given error message.
  }
  function StripLineBreaks(const Source: String): String;
  begin
    Result := StringReplace(Source, #13#10, ' ', [rfReplaceAll]);
  end;

  {-----------------------------------------------------------------------------
    Writes a line to the output file that corresponds to the current record
    in Query (a row in the import file).
  }
  procedure WriteRejectedRow;
  var
    I: Integer;
  begin
    Write(
        Output,
        AnsiQuotedStr(Rejection.TableName, '"'), ',',
        AnsiQuotedStr(Rejection.Key1, '"'), ',',
        AnsiQuotedStr(StripLineBreaks(Message), '"'), ',',
        // the last field in Query is the row number
        AnsiQuotedStr(Query.Fields[Query.FieldCount - 1].AsString, '"'));

    for I := 0 to Query.FieldCount - 2 do
    begin
      Write(Output, ',', AnsiQuotedStr(Query.Fields[I].AsString, '"'));
    end;
    Writeln(Output);
  end;

  {-----------------------------------------------------------------------------
    Writes lines to the output file for each row in the import file that
    corresponds to the current rejected record.
  }
  procedure WriteRejectedRows;
  begin
    ConfigureQueryForRejection;
    Query.Open;
    while not Query.Eof do
    begin
      WriteRejectedRow;
      Query.Next;
    end;
    Query.Close;
  end;

begin
  dlgSaveDetails.Filter := ResStr_FilterCSV + '|*.csv';
  if not dlgSaveDetails.Execute then Exit;

  AssignFile(Output, dlgSaveDetails.FileName);
  try
    Rewrite(Output);
    try
      WriteHeader;
      Query := TADOQuery.Create(nil);
      try
        Query.Connection := dmDatabase.Connection;
        CurrentTable := '';

        for I := 0 to Settings.Rejections.Count - 1 do
        begin
          if Settings.Rejections.Objects[I] is TMessage then
          begin
            Rejection := MakeRejection(Settings.Rejections[I]);
            Message := TMessage(Settings.Rejections.Objects[I]).Msg;
            WriteRejectedRows;
          end;
        end;
      finally
        Query.Free;
      end;
    finally
      CloseFile(Output);
    end;

  except
    on E: EInOutError do
    begin
      if E.ErrorCode > 99 then
        raise
      else
        MessageDlg(SysErrorMessage(E.ErrorCode), mtError, [mbOk], 0);
    end;
  end;
end;

end.

