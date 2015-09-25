//==============================================================================
//  Unit:        MergeData
//
//  Implements:  TfrmMergeData
//
//  Description:
//
//  Author:      John van Breda
//  Created:     28 April 1999
//
//  Last Revision Details:
//    $Revision: 27 $
//    $Date: 22/01/08 14:25 $
//    $Author: Davidkelly $
//
//  $History: MergeData.pas $
//  
//  *****************  Version 27  *****************
//  User: Davidkelly   Date: 22/01/08   Time: 14:25
//  Updated in $/JNCC/Development/Build/Source
//  VI 14779 (CCN 225b): Implemented EditMode property.
//  
//  *****************  Version 26  *****************
//  User: Rickyshrestha Date: 18/12/07   Time: 9:29
//  Updated in $/JNCC/Development/Build/Source
//  Changed some string constant and hardocoded strings to resourcestring
//  ResStr_ItemsSale
//    ResStr_MergeOk
//    ResStr_MergerecMissing
//    ResStr_CannotReassignData
//    ResStr_SingleTableMerge
//    ResStr_CannotMergeSiteSubsite
//    ResStr_ScreensOpen
//    ResStr_DataDelete
//    ResStr_ProecddConfirm 
//  
//  *****************  Version 25  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 12:01
//  Updated in $/JNCC/Development/Build/Source
//  Forced nil on freed objects, trying to prevent AVs when closing app
//  while forms are still loading.
//  
//  *****************  Version 24  *****************
//  User: Ericsalmon   Date: 25/06/04   Time: 10:46
//  Updated in $/JNCC/Development/Build/Source
//  Bug fix.
//  
//  *****************  Version 23  *****************
//  User: Ericsalmon   Date: 23/05/03   Time: 14:50
//  Updated in $/JNCC/Development/Build/Source
//  IR636 / JNCC611
//  Done. Minor adjustments to code for speed. Still slow on merging
//  individuals, as indexes on Entered_By/Changed_By fields have been
//  discarded.
//  
//  *****************  Version 22  *****************
//  User: Pollyshaw    Date: 10/01/03   Time: 16:05
//  Updated in $/JNCC/Source
//  IR 188, IR 182, IR 189
//  Made the foreign keys not get updated if they are marked On Delete
//  Cascade. Checked that the user has update permission before attempting
//  to update. 
//
//  *****************  Version 21  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 17:04
//  Updated in $/JNCC/Source
//  Removed USE_TITAN compiler directive and D4-specific code.
//  
//  *****************  Version 20  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 11:30
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MergeData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, StdCtrls, ExtCtrls, ComCtrls, Buttons, DataClasses, DropStruct,
  Menus, OnlineHelp, ExceptionForm, GeneralFunctions, ImageListButton,
  Relationships_ADO, DatabaseAccessADO, MergeDataData;

type
  EMergeDataError = class(TExceptionPath);

  TfrmMergeData = class(TBaseChild)
    pnlInstruct: TPanel;
    Label1: TLabel;
    lblDestItem: TLabel;
    Panel2: TPanel;
    tvSourceItem: TTreeView;
    pnlDest: TPanel;
    tvDestItem: TTreeView;
    pnlButtons: TPanel;
    lblInstruct2: TLabel;
    mnuEdit: TMenuItem;
    mnuPaste: TMenuItem;
    lblInstruct: TLabel;
    bbMerge: TImageListButton;
    bbCancel: TImageListButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ItemClick(Sender: TObject);
    procedure CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormResize(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbMergeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FdmMergeData: TdmMergeData;
    FSourceTable : string;
    FDestTable   : string;
    FSourceKey   : TKeyString;
    FDestKey     : TKeyString;
    procedure SetSourceItem(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure SetDestItem(const Sender: TObject; const iFormat: integer;
      const iSourceData: TKeyList; const iTextStrings: TStringList;
      var ioHandled: boolean);
    procedure SetTreeViewItem( iTreeView : TTreeView; iSourceData : TKeyList );
    procedure ClearTreeView(iTreeView : TTreeView);
    procedure CheckForHierarchicalMerge;
  protected
    procedure SetupDestinationControls; override;
    procedure RegisterCanDropAnything( iControl : TWinControl;
                                       iEvent : TDataDroppedEvent );
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

uses
  DropTarget, GeneralData, FormActions, Maintbar, Constants;

resourcestring
  ResStr_ItemsSale = 'You cannot merge two data items which are the same.  ' +
                     'Please select a different source or destination item.';

  ResStr_MergeOk = 'The data merge is complete.';
  ResStr_MergerecMissing = 'Cannot merge data items that are not in the local database';
  ResStr_CannotReassignData = 'The data item is system supplied and cannot be reassigned.';
  ResStr_SingleTableMerge = 'You can only merge data items in a single table.';
  ResStr_CannotMergeSiteSubsite = 'You cannot merge a site into one of its subsites.';

  ResStr_ScreensOpen =  'Some screens are still open. Please close these screens before '+
                       'initiating the data merging process.';

  ResStr_DataDelete = 'Warning.  The data item %s in the %s' +
                      ' table is about to be  deleted.  All data referring to this record will be ' +
                      'reassigned to point to record %s. ';

  ResStr_ProecddConfirm = 'Do you wish to proceed?';

{$R *.DFM}

//==============================================================================
constructor TfrmMergeData.Create(AOwner: TComponent);
var lCursor : TCursor;
begin
  inherited Create(AOwner);
  lCursor := HourglassCursor;
  try
    ReSize;
    FdmMergeData := TdmMergeData.Create(nil);
  finally
    DefaultCursor(lCursor);
  end; // try.. finally

  //Help Setup
  HelpContext := IDH_MERGEDATA;

  // Cannot do batch updates while this screen is open.
  FEditMode := emEdit;
end;  // Create

//==============================================================================
destructor TfrmMergeData.Destroy;
begin
  { Ensure we don't leak any of the objects on the tree view }
  ClearTreeView(tvSourceItem);
  ClearTreeView(tvDestItem);
  FdmMergeData.Free;
  FdmMergeData := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmMergeData.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
{ Set up all drag/drop stuff }
procedure TfrmMergeData.SetupDestinationControls;
begin
  RegisterCanDropAnything( tvSourceItem, SetSourceItem );
  RegisterCanDropAnything( tvDestItem, SetDestItem );
end;  // SetupDestinationControls

//==============================================================================
{ Populate a tree view with record structure determined by the item in the
     provided keylist }
procedure TfrmMergeData.SetTreeViewItem(iTreeView: TTreeView;
  iSourceData: TKeyList);
var
  lTableNode : TTreeNode;
begin
  if iSourceData.Header.ItemCount = 0 then
    Exit; // no data dropped
  with iTreeView.Items do begin
    Clear;
    { Add a tree node for the table }
    lTableNode := Add(nil, iSourceData.Header.TableName + ' - '
                            + iSourceData.Items[0].KeyField1);
    lTableNode.ImageIndex := 0; // table glyph
    lTableNode.SelectedIndex := 0;
  end;
  { Read the fields and data into the tree view }
  FdmMergeData.GetRecordStruct(iSourceData, iTreeView, lTableNode);
  { Expand the node one level }
  lTableNode.Expand(True);
  // Select first node.
  iTreeView.Selected := iTreeView.Items.GetFirstNode;
end;  // SetTreeViewItem

//==============================================================================
procedure TfrmMergeData.SetSourceItem(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TStringList; var ioHandled : boolean);
begin
  try
    if (iFormat = CF_JNCCDATA) then
    begin
      if iSourceData.Header.ItemCount > 0 then // stop list index out of bounds
        if dmGeneralData.IsSystemSupplied(iSourceData.Header.TableName,
                                     iSourceData.Items[0].KeyField1) then
        begin
          MessageDlg(ResStr_CannotReassignData, mtInformation, [mbOk], 0);
          Exit; // no further action required
        end;
      SetTreeViewItem(tvSourceItem, iSourceData);
      { If new table, then clear dest box as well }
      if iSourceData.Header.TableName <> FDestTable then
      begin
        tvDestItem.Items.Clear;
        { We are not able to proceed yet }
        FDestTable := '';
      end;
      { Store the table dropped so we can check when dropping a destination }
      FSourceTable := iSourceData.Header.TableName;
      FSourceKey := iSourceData.Items[0].KeyField1;
      ioHandled := True;
      bbMerge.Enabled := (FSourceTable = FDestTable);
      CheckForHierarchicalMerge;
    end; // if iFormat
  except
    on ERecordMissingError do begin // item being dropped not in local db so ignore
      tvSourceItem.Items.Clear;
      raise ERecordMissingError.CreateNonCritical(ResStr_MergerecMissing);
    end;
  end;
end;  // SetSourceItem

//==============================================================================
procedure TfrmMergeData.SetDestItem(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TStringList; var ioHandled : boolean);
begin
  try
    if (iFormat = CF_JNCCDATA) then
    begin
      { Must have a matching table }
      if (FSourceTable <> '') and (FSourceTable <> iSourceData.Header.TableName) then
      begin
        MessageDlg(ResStr_SingleTableMerge, mtWarning, [mbOk], 0);
        Exit;
      end;
      SetTreeViewItem(tvDestItem, iSourceData);
      FDestTable := iSourceData.Header.TableName;
      if FSourceTable = FDestTable then
        { Ready to proceed }
        bbMerge.Enabled := True;
      FDestKey := iSourceData.Items[0].KeyField1;
      ioHandled := True;
      CheckForHierarchicalMerge;
    end;
  except
    on ERecordMissingError do begin // item being dropped not in local db so ignore
      tvDestItem.Items.Clear;
      raise ERecordMissingError.CreateNonCritical(ResStr_MergerecMissing);
    end;
  end;
end;  // SetDestItem


//==============================================================================
{ If the user is trying to merge a location into one of its subsites, then a
    message is displayed and the destination cleared.  this is an invalid
    operation because it can create loops in the location hierarchy (locations
    with no top level site) }
procedure TfrmMergeData.CheckForHierarchicalMerge;
const
  SQL_SELECT_LOC_PARENT = 'SELECT PARENT_KEY FROM LOCATION WHERE LOCATION_KEY=''%s''';
var
  lLocKey : string;
begin
  if CompareText(FSourceTable, 'LOCATION') = 0 then begin
    lLocKey := FDestKey;
    with dmGeneralData.qryAllPurpose do repeat
      ParseSQL := True;
      SQL.Text := Format(SQL_SELECT_LOC_PARENT, [lLocKey]);
      Open;
      lLocKey := Fields[0].AsString;
      close;
    until (lLocKey = '') or (lLocKey = FSourceKey);
    // if the source is found in the chain of parents, then the merge is invalid
    if lLocKey = FSourceKey then begin
      ShowInformation(ResStr_CannotMergeSiteSubsite);
      bbMerge.Enabled := False;
    end;
  end;
end;


//==============================================================================
procedure TfrmMergeData.ItemClick(Sender: TObject);
var lRelIndex    : integer;
    lSelectedNode: TTreeNode;
begin
  inherited;
  if TTreeView(Sender).Selected <> nil then
  begin
    lSelectedNode:=TTreeView(Sender).Selected;
    if (lSelectedNode.Data <> nil) and (not lSelectedNode.HasChildren) then
    begin
      { Open the node }
      lRelIndex:= dmDatabase.Relationships.FindRelationship(
                         TRecordDataNode(lSelectedNode.Data).FTableName,
                         TRecordDataNode(lSelectedNode.Data).FFieldName);
      if lRelIndex <> NO_RELATIONSHIP then
      begin
        FdmMergeData.PopulateRelationship( lRelIndex, lSelectedNode );
      end; // if lRelindex <> -1
    end; // if..Data<>nil
  end; // if..Selected<>nil
end;  // ItemClick

//==============================================================================
{ Nodes which are not data items should be displayed bold - detected simply
     because the data items are nil.  Other nodes are in a different font
     colour for clarity - these are the data nodes }
procedure TfrmMergeData.CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  DefaultDraw:=True;
  if (Node.Data = nil) then
  begin
    Sender.Canvas.Font.Style:=[fsBold];
  end else
  begin
    Sender.Canvas.Font.Style:=[];
    { Can't draw selected colour on selected background! }
    if not Node.Selected then
      Sender.Canvas.Font.Color := clHighlight;
  end;
end;  // CustomDrawItem

//==============================================================================
procedure TfrmMergeData.RegisterCanDropAnything(iControl: TWinControl;
  iEvent: TDataDroppedEvent);
var
  lTableList : Array of string;
  i : integer;
begin
  { Tranlsate the global list of tables to a string array }
  SetLength(lTableList, dmGeneralData.TableList.Count);
  for i := 0 to dmGeneralData.TableList.Count-1 do
  begin
    lTableList[i] := dmGeneralData.TableList[i];
  end;
  { And use this array to determine what the control can accept }
  RegisterDropComponent( iControl, iEvent, lTableList, [CF_JNCCDATA]);
end;  // RegisterCanDropAnything

//==============================================================================
{ Ensure the splitter stays down the middle.  Also, centre the labels and keep
    the buttons on the right of the form }
procedure TfrmMergeData.FormResize(Sender: TObject);
begin
  tvSourceItem.Width := Width div 2 - 4;
  lblDestItem.Left := pnlDest.Left + 4;
  { centre the instruction labels }
  lblInstruct.Left := (pnlInstruct.Width - lblInstruct.Width) div 2;
  lblInstruct2.Left := (pnlInstruct.Width - lblInstruct2.Width) div 2;
  { Buttons }
  bbMerge.Left := Width - 176;
  bbCancel.Left := Width - 92;
  inherited;
end;  // FormResize

//==============================================================================
{ Clear button - Just clear the treeviews }
procedure TfrmMergeData.bbCancelClick(Sender: TObject);
begin
  inherited;
  ClearTreeView( tvSourceItem );
  ClearTreeView( tvDestItem );
  bbMerge.Enabled := False;
  FSourceTable := '';
  FDestTable := '';
  FSourceKey := '';
  FDestKey := '';
end;  // bbCancelClick

//==============================================================================
{ Merge button click handler }
procedure TfrmMergeData.bbMergeClick(Sender: TObject);
var lCursor:TCursor;
begin
  inherited;
  // Check if this screen is the only one left open. If so, all ok, else abort for now-
  if frmMain.MDIChildCount>1 then
    MessageDlg(ResStr_ScreensOpen,mtInformation,[mbOk],0)
  else
  if FSourceKey = FDestKey then
    MessageDlg(ResStr_ItemsSale, mtInformation, [mbOk], 0)
  else
  begin
    { Get a confirmation  as this is a serious thing to do }
    if MessageDlg(Format(ResStr_DataDelete, [FSourceKey, FSourceTable, FDestKey]) + ResStr_ProecddConfirm,
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      lCursor := HourglassCursor;
      try
        bbMerge.Enabled := False;
        FdmMergeData.ProcessMerge( FSourceTable, FSourceKey, FDestKey );
      finally
        DefaultCursor(lCursor);
        frmMain.TaskFinished;
      end; // try..finally
      { Only gets here if no exceptions }
      MessageDlg(ResStr_MergeOk, mtInformation, [mbOk], 0);
    end;
  end;
end;  // bbMergeClick

//==============================================================================
{ Clear either of the tree views, removing any assigned node data objects }
procedure TfrmMergeData.ClearTreeView(iTreeView: TTreeView);
var i : integer;
begin
  with iTreeView do
  begin
    for i := 0 to Items.Count-1 do
    begin
      if Items[i].Data <> nil then
      begin
        TRecordDataNode(Items[i].Data).Free;
        Items[i].Data := nil; // saftey measure
      end; //if
    end;
    Items.Clear;
  end;
end;  // ClearTreeView

//==============================================================================
procedure TfrmMergeData.FormActivate(Sender: TObject);
begin
  inherited;
  LockWindowUpdate(0);
end;

end.
