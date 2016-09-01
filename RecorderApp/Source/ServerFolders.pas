//==============================================================================
//  Unit:        ServerFolders
//
//  Implements:  TdlgServerFolders
//
//  Description: Dialog allowing the user to browse directories
//               on the server.
//
//  Author:      Polly Shaw
//  Created:     12 Feb 2003
//
//  Last Revision Details:
//    $Revision: 6 $
//    $Date: 22/04/08 15:31 $
//    $Author: Ericsalmon $
//
//  $History: ServerFolders.pas $
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 22/04/08   Time: 15:31
//  Updated in $/JNCC/Development/Build/Source
//  Cleanup.
//  
//  *****************  Version 5  *****************
//  User: Rickyshrestha Date: 19/12/07   Time: 11:02
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strinsg to resourcestring
//  ResStr_SelectBackupDirectory
//    ResStr_NoPermissionToChange
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 20/09/04   Time: 10:28
//  Updated in $/JNCC/Development/Build/Source
//  Changed TBitBtns to TImageListButtons.
//
//  *****************  Version 3  *****************
//  User: Pollyshaw    Date: 13/02/03   Time: 12:15
//  Updated in $/JNCC/Source
//  Changed the position of the form
//
//  *****************  Version 1  *****************
//  User: Pollyshaw    Date: 12/02/03   Time: 18:01
//  Created in $/JNCC/Source
//  Dialog allowing users to browse the directories on the SQL server.
//
//==============================================================================

unit ServerFolders;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DatabaseAccessADO, StdCtrls, DBCtrls, DB, ADODB, JNCCDatasets,
  exgrid, RapTree, KeyboardRapidTree, Buttons, ImageListButton, ExtCtrls, FormActions;

type
  TdlgServerFolders = class(TForm)
    Label1: TLabel;
    cmbDrive: TComboBox;
    tvDirectories: TKeyboardRapidTree;
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    lblInstruct: TLabel;
    lblPath: TLabel;
    qryMoveBackup: TADOQuery;
    Bevel1: TBevel;
    procedure cmbDriveChange(Sender: TObject);
    procedure tvDirectoriesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure tvDirectoriesExpanding(Sender: TObject; Node: TFlyNode;
      var AllowExpansion: Boolean);
  private
    FstPath : String;
    FtfCanChange: boolean;
    procedure SetPath(const Value: string);
    procedure SetCanChange(const Value: boolean);
    procedure AddLevelToTree(path: string; node: TFlyNode);
  public
    constructor create(AOwner : TComponent);override;
    property Path : string read FstPath write setPath;
    property CanChange : boolean read FtfCanChange write SetCanChange;
  end;

//==============================================================================
implementation

uses
  FileUtils, StrUtils;

resourcestring
  ResStr_SelectBackupDirectory =  'Please select a database backup directory.';
  ResStr_NoPermissionToChange = 'You do not have permission to change the backup location.';

{$R *.dfm}

{-------------------------------------------------------------------------------
}
constructor TdlgServerFolders.create(AOwner: TComponent);
var
  lrec : _Recordset;
begin
  inherited;
  //Populate combo box with drives on the server
  lrec := dmDatabase.dbLocal.Execute('master.dbo.xp_FixedDrives');
  if not (lrec.Eof and lrec.BOF) then
    while not lrec.EOF do
    begin
      cmbDrive.Items.Add(lrec.Fields.Item['drive'].Value + ':\');
      lrec.MoveNext;
    end;
  cmbDrive.ItemIndex := 0;
  cmbDriveChange(cmbDrive);
  CanChange := true;
end;

procedure TdlgServerFolders.cmbDriveChange(Sender: TObject);
begin
  //Populate tvDirectories with the contents of the drive
  tvDirectories.Items.Clear;
  AddLevelToTree('', nil);
end;

(**
 * Add a level of the directory structure at a given path to the tree, under the
 * provided node. Node may be nil to insert at the top level.
 *)
procedure TdlgServerFolders.AddLevelToTree(path: string; node: TFlyNode);
var
  lRec : _Recordset;
  lFlyNode : TFlyNode;
begin
  // use xp_dirtree with level=1 rather than xp_subdirs, because xp_dirtree defaults to public permissions.
  lrec := dmDatabase.dbLocal.Execute('master.dbo.xp_dirtree ''' + ExcludeTrailingPathDelimiter(cmbDrive.Text + path) + ''', 1');
  if not (lrec.Eof and lrec.Bof) then
    while not lrec.Eof do
    begin
      lFlyNode := tvDirectories.Items.AddChild(node, lrec.Fields['subdirectory'].Value);
      lFlyNode.ImageIndex := 59;
      lFlyNode.SelectedIndex := 74;
      // force a + to appear by the node
      lFlyNode.HasChildren := true;
      lRec.MoveNext;
    end;
  if assigned(node) and (node.Count=0) then
    node.HasChildren := false;
end;

(**
 * Expanding a node populates the folders beneath it.
 *)
procedure TdlgServerFolders.tvDirectoriesExpanding(Sender: TObject;
  Node: TFlyNode; var AllowExpansion: Boolean);
var
  n : TFlyNode;
  path: string;
begin
  n := Node;
  path := Node.text;
  while assigned(n.Parent) do begin
    n := n.Parent;
    path := n.Text + '\' + path;
  end;
  AddLevelToTree(path, Node);
end;

{-------------------------------------------------------------------------------
  This sets the path to the one represented by the selected node.
}
procedure TdlgServerFolders.tvDirectoriesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  lFlyNode : TFlyNode;
begin
  FstPath := '';
  lFlyNode := tvDirectories.GetNodeAtRow(ARow);
  while Assigned(lFlyNode) do
  begin
    FstPath := lFlyNode.Text + '\' + FstPath;
    lFlyNode := lFlyNode.Parent;
  end;
  FstPath := cmbDrive.Text + FstPath;
  lblPath.Caption := FstPath;
end;

{-------------------------------------------------------------------------------
  This sets the selected cell in the tree to the one representing the directory
  where possible.
}
procedure TdlgServerFolders.SetPath(const Value: string);
var
  liSlashPos : integer;
  lFlyNode : TFlyNode;
  lstCurDir : String;
  lstValue : String;
begin
  lblPath.Caption := ExtractFilePath(Value);
  cmbDrive.Text := Value[1];

  liSlashPos := Pos('\', Value);
  lstValue := RightStr(Value, length(Value) -liSlashPos);
  liSlashPos := Pos('\', lstValue);
  lstCurDir := LeftStr(lstValue, liSlashPos -1);
  lFlyNode := tvDirectories.TopItem;
  while Assigned(lFlyNode) do
  begin
    if lFlyNode.Text = lstCurDir then break;
    lFlyNode := lFlyNode.GetNextSibling;
  end;
  if Assigned(lFlyNode) then
    begin
      lstValue := RightStr(lstValue, length(lstValue) -liSlashPos);
      liSlashPos := Pos('\', lstValue);
      while Assigned(lFlyNode) and (liSlashPos<>0) do
      begin
        lstCurDir := LeftStr(lstValue, liSlashPos -1);
        lFlyNode.Expand(false);
        lFlyNode := lFlyNode.getFirstChild;
        while Assigned(lFlyNode) do
        begin
          if lFlyNode.Text = lstCurDir then
          begin
            lFlyNode.Parent.Expanded := true;
            break;
          end;
          lFlyNode := lFlyNode.GetNextSibling;
        end;
        lstValue := RightStr(lstValue, length(lstValue) -liSlashPos);
        liSlashPos := Pos('\', lstValue);
      end;
    end;
  if Assigned(lFlyNode) then begin
    tvDirectories.Selected := lFlyNode;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgServerFolders.SetCanChange(const Value: boolean);
begin
  FtfCanChange := Value;
  if FtfCanChange then
    lblInstruct.Caption := ResStr_SelectBackupDirectory
  else
    lblInstruct.Caption := ResStr_NoPermissionToChange;
  btnOK.Enabled := Value;
end;

end.
