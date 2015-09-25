unit ChangeCustodianDB;

// Gets the tables which the users wishes to change the Custodian on,
// plus the new Custodian and the option to transfer custody
// even where the export would result in a parent being changed
// when only some of the childrem would be. Eg The Custodian of the Sample
// being changed when only some of the occurrence for it are being changed.  
// Author Mike Weideli
// December 2014

Interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,Db,
  Dialogs, ImgList, ComCtrls, StdCtrls, ImageListButton, exgrid, RapTree,TreeColl,
  RestrictedEdits,DataClasses,DBBrowser;

type
  TdlgChangeCustodianDB = class(TForm)
    lblInstruct: TLabel;
    lblNewCustodian: TLabel;
    eNewCustodian: TRestrictedEdit;
    tvRecords: TRapidTree;
    btnSelectAll: TImageListButton;
    btnClear: TImageListButton;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    ilCheckBoxes: TImageList;
    cbChangePartial: TCheckBox;

    procedure btnSelectAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure tvRecordsClick(Sender: TObject);
    procedure eNewCustodianCheckText(Sender: TObject; const iText: String; var ioAccept: Boolean);

  private
    { Private declarations }
    FTablesToChange : TStringList;
    FNewSiteID : string;
    FBrowser: TDBBrowserUtility;
    FChangePartial : boolean;
    procedure CheckChildren(ANode: TFlyNode);
    procedure CheckParent(ANode: TFlyNode);
    procedure AssignNewCustodian;
    procedure PopulateTVRecords(AStringList: TStringList);
  public
    property TablesToChange: TStringList read FTablesToChange;
    property   NewSiteID : String read FNewSiteID ;
    property   ChangePartial : boolean read FChangePartial ;
    { Public declarations }
    constructor Create(AOwner: TComponent; AKeyList: TStringList); reintroduce;
    destructor Destroy; override;

  end;

var
  dlgChangeCustodianDB: TdlgChangeCustodianDB;


  implementation

{$R *.dfm}

uses
  GeneralData, ExceptionForm, Maintbar, GeneralFunctions;


resourcestring
    ResStr_NonSite =  'Please provide a valid Site ID.';

const
  INDEX_CHECKED = 0;
  INDEX_UNCHECKED = 1;
  INDEX_UNSURE = 2;

constructor TdlgChangeCustodianDB.Create(AOwner: TComponent;
  AKeyList: TStringList);
var lCursor: TCursor;
begin
  inherited Create(AOwner);
  lCursor := HourglassCursor;
  Try
    PopulateTvRecords(AKeyList);
  finally
    DefaultCursor(lCursor);
  end;
end;

destructor  TdlgChangeCustodianDB.Destroy;
begin
 // FreeTVRecordData;
  FBrowser.Free;
  inherited;
end;

procedure TdlgChangeCustodianDB.btnClearClick(Sender: TObject);
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

procedure TdlgChangeCustodianDB.btnOkClick(Sender: TObject);
begin
  if Length(eNewCustodian.Text) < 8 then begin
    eNewCustodian.SetFocus;
    MessageDlg(ResStr_NonSite, mtInformation, [mbOK], 0);
  end;
  AssignNewCustodian;
end;

procedure TdlgChangeCustodianDB.btnSelectAllClick(Sender: TObject);
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
procedure TdlgChangeCustodianDB.tvRecordsClick(Sender: TObject);
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

procedure TdlgChangeCustodianDB.eNewCustodianCheckText
  (Sender: TObject; const iText: String; var ioAccept: Boolean);
var k: Integer;
begin
  ioAccept := Length(iText) <= 8;
  for k := Length(iText) downto 1 do
    ioAccept := ioAccept and (iText[k] in ['A'..'Z', '0'..'9']);
end;


procedure TdlgChangeCustodianDB.CheckChildren(ANode: TFlyNode);
var lChildNode: TFlyNode;
begin
  lChildNode := ANode.GetFirstChild;
  while lChildNode <> nil do begin
    lChildNode.ImageIndex := ANode.ImageIndex;
    lChildNode.SelectedIndex := ANode.SelectedIndex;
    lChildNode := ANode.GetNextChild(lChildNode);
  end;
end;

procedure TdlgChangeCustodianDB.CheckParent(ANode: TFlyNode);
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

procedure TdlgChangeCustodianDB.PopulateTVRecords(AStringList: TStringList);
var i : Integer;
lNode: TFlyNode;
begin
  AStringList.Sort;
  tvRecords.Items.BeginUpdate;
  for i := 0 to AStringList.Count - 1 do begin
    lNode := tvRecords.Items.Add(nil, AStringList.Strings[i]);
    lNode.ImageIndex := INDEX_CHECKED;
    lNode.SelectedIndex := INDEX_CHECKED;

  end;
  tvRecords.Items.EndUpdate;
end;

procedure TdlgChangeCustodianDB.AssignNewCustodian();
var
k : Integer;
lNode: TFlyNode;

begin
   FTablesToChange := TStringList.Create;
   for k := tvRecords.Items.Count - 1 downto 0 do begin
      lNode := tvRecords.Items[k];
      case lNode.ImageIndex of
          INDEX_CHECKED: FTablesToChange.Add(lNode.Caption);
      else { INDEX_UNCHECKED } // Do Nothing
      end;
    end;
    FNewSiteID := eNewCustodian.Text;
    ModalResult := mrOK;
    FChangePartial := cbChangePartial.Checked;
end;



end.

