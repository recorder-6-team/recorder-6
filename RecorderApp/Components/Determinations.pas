{+--------------------------------------------------------------------------+
 | Component:   TDeterminations
 | Created:     07/04/1999 10:34:32
 | Company:     Dorset Software Services
 | Copyright    1999, all rights reserved.
 | Description: Determinations for Taxon and Biotopes
 | Version:
 | Modification History:
 +--------------------------------------------------------------------------+}

unit Determinations;  { TDeterminations compound component. }
{.CDK.REGLINK=eJnccReg.pas}  { Registration file is eJnccReg.pas. }
{ Created 07/04/1999 10:34:32 }
{ Eagle Software CDK, Version 4.02 Rev. M }
{ Modified 07/04/1999 10:54:39 by the CDK, Version 4.02 Rev. M }

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, Grids, Buttons, ComCtrls, VagueDateEdit,
  CompositeComponent, DisplayFuncs, DropStruct, DataClasses, DropTarget,
  DBGlyphCtrls, DetData;

type
  EDeterminationsError = class(Exception);

  { Actual component class }
  TDeterminations = class(TCompositeComponent)
  private
    sgDeterminations: TStringGrid;
    bbDetAdd: TBitBtn;
    bbDetEdit: TBitBtn;
    bbDetDel: TBitBtn;
    gbDetDetails: TGroupBox;
    lblTaxonBio: TLabel;
    lblDeterminer: TLabel;
    lblRole: TLabel;
    lblType: TLabel;
    lblDate: TLabel;
    lblReference: TLabel;
    lblComments: TLabel;
    shpTaxonBio: TShape;
    shpDeterminer: TShape;
    shpReference: TShape;
    eTaxonBio: TEdit;
    bbFindTaxonBio: TBitBtn;
    eDeterminer: TEdit;
    bbFindDeterminer: TBitBtn;
    cmbRole: TDBGlyphLookupComboBox;
    cbPreferred: TCheckBox;
    cmbType: TDBGlyphLookupComboBox;
    eDetDate: TVagueDateEdit;
    eWork: TEdit;
    bbFindReference: TBitBtn;
    reDetComments: TRichEdit;
    bbDetAccept: TBitBtn;
    bbDetDiscard: TBitBtn;

    { Private declarations }
    FDetType: TDeterminationType;
    FEditModeOn: Boolean;
    { Exposed Events: }
    FOnTaxonBioDragDrop: TDragDropEvent;
    FOnTaxonBioDragOver: TDragOverEvent;
    FOnTaxonBioKeyPress: TKeyPressEvent;
    FOnFindTaxonBioClick: TNotifyEvent;
    FOnDeterminerDragDrop: TDragDropEvent;
    FOnDeterminerDragOver: TDragOverEvent;
    FOnDeterminerKeyPress: TKeyPressEvent;
    FOnFindDeterminerClick: TNotifyEvent;
    FOnWorkDragDrop: TDragDropEvent;
    FOnWorkDragOver: TDragOverEvent;
    FOnWorkKeyPress: TKeyPressEvent;
    FOnFindWorkClick: TNotifyEvent;
    FOnAcceptClick: TNotifyEvent;
    FOnDiscardClick: TNotifyEvent;
    FDragDropTarget: string;
    { List of items }
    FDeterminationDataList : TDeterminationDataList;
    procedure DetAddClickHandler(Sender: TObject);  { TNotifyEvent }
    procedure DetEditClickHandler(Sender: TObject);  { TNotifyEvent }
    procedure DetDelClickHandler(Sender: TObject);  { TNotifyEvent }
    procedure DetGridDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure TaxonBioDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);  { TDragDropEvent }
    procedure TaxonBioDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);  { TDragOverEvent }
    procedure TaxonBioKeyPressTransfer(Sender: TObject; var Key: Char);  { TKeyPressEvent }
    procedure FindTaxonBioClickTransfer(Sender: TObject);  { TNotifyEvent }
    procedure DeterminerDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);  { TDragDropEvent }
    procedure DeterminerDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);  { TDragOverEvent }
    procedure DeterminerKeyPressTransfer(Sender: TObject; var Key: Char);  { TKeyPressEvent }
    procedure FindDeterminerClickTransfer(Sender: TObject);  { TNotifyEvent }
    procedure WorkDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);  { TDragDropEvent }
    procedure WorkDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);  { TDragOverEvent }
    procedure WorkKeyPressTransfer(Sender: TObject; var Key: Char);  { TKeyPressEvent }
    procedure FindWorkClickTransfer(Sender: TObject);  { TNotifyEvent }
    procedure AcceptClickTransfer(Sender: TObject);  { TNotifyEvent }
    procedure DiscardClickTransfer(Sender: TObject);  { TNotifyEvent }
    procedure SwitchToDetails(tfDetails:boolean);
  protected
    { Protected declarations }
    procedure SetDetType(newValue: TDeterminationType); virtual;
    { Exposed properties' Read/Write methods: }
    procedure SetTaxonBioText(newValue: TCaption); virtual;
    function GetTaxonBioText: TCaption; virtual;
    procedure SetDeterminerText(newValue: TCaption); virtual;
    function GetDeterminerText: TCaption; virtual;
    procedure SetRoleItems(newValue: TStrings); virtual;
    function GetRoleItems: TStrings; virtual;
    procedure SetRoleText(newValue: TCaption); virtual;
    function GetRoleText: TCaption; virtual;
    procedure SetPreferredState(newValue: boolean); virtual;
    function GetPreferredState: boolean; virtual;
    procedure SetTypeItems(newValue: TStrings); virtual;
    function GetTypeItems: TStrings; virtual;
    procedure SetTypeText(newValue: TCaption); virtual;
    function GetTypeText: TCaption; virtual;
    procedure SetDateText(newValue: TCaption); virtual;
    function GetDateText: TCaption; virtual;
    procedure SetWorkText(newValue: TCaption); virtual;
    function GetWorkText: TCaption; virtual;
    procedure SetCommentsLines(newValue: TStrings); virtual;
    function GetCommentsLines: TStrings; virtual;
    procedure SetCommentsPlainText(newValue: Boolean); virtual;
    function GetCommentsPlainText: Boolean; virtual;
    procedure SetCommentsPopupMenu(newValue: TPopupMenu); virtual;
    function GetCommentsPopupMenu: TPopupMenu; virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Loaded; override;
    { Drag drop methods }
    procedure TaxonDrop(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TstringList; var ioHandled : boolean);
    { Other methods }
    procedure ClearDetails;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddNew;
    procedure RegisterDragDrop;
    property EditModeOn: Boolean read FEditModeOn;  { Public }
    property DragDropTarget:string read FDragDropTarget;
  published
    { Published properties and events }
    property DetType: TDeterminationType read FDetType write SetDetType;  { Published }
    property Enabled;
    property OnClick;
    { Exposed subcomponent properties: }
    property TaxonBioText: TCaption read GetTaxonBioText write SetTaxonBioText;
    property DeterminerText: TCaption read GetDeterminerText write SetDeterminerText;
    property RoleItems: TStrings read GetRoleItems write SetRoleItems;
    property RoleText: TCaption read GetRoleText write SetRoleText;
    property PreferredState: boolean read GetPreferredState write SetPreferredState;
    property TypeItems: TStrings read GetTypeItems write SetTypeItems;
    property TypeText: TCaption read GetTypeText write SetTypeText;
    property DateText: TCaption read GetDateText write SetDateText;
    property WorkText: TCaption read GetWorkText write SetWorkText;
    property CommentsLines: TStrings read GetCommentsLines write SetCommentsLines;
    property CommentsPlainText: Boolean read GetCommentsPlainText write SetCommentsPlainText;
    property CommentsPopupMenu: TPopupMenu read GetCommentsPopupMenu write SetCommentsPopupMenu;
    { Exposed subcomponent events: }
    property OnTaxonBioDragDrop: TDragDropEvent read FOnTaxonBioDragDrop write FOnTaxonBioDragDrop;
    property OnTaxonBioDragOver: TDragOverEvent read FOnTaxonBioDragOver write FOnTaxonBioDragOver;
    property OnTaxonBioKeyPress: TKeyPressEvent read FOnTaxonBioKeyPress write FOnTaxonBioKeyPress;
    property OnFindTaxonBioClick: TNotifyEvent read FOnFindTaxonBioClick write FOnFindTaxonBioClick;
    property OnDeterminerDragDrop: TDragDropEvent read FOnDeterminerDragDrop write FOnDeterminerDragDrop;
    property OnDeterminerDragOver: TDragOverEvent read FOnDeterminerDragOver write FOnDeterminerDragOver;
    property OnDeterminerKeyPress: TKeyPressEvent read FOnDeterminerKeyPress write FOnDeterminerKeyPress;
    property OnFindDeterminerClick: TNotifyEvent read FOnFindDeterminerClick write FOnFindDeterminerClick;
    property OnWorkDragDrop: TDragDropEvent read FOnWorkDragDrop write FOnWorkDragDrop;
    property OnWorkDragOver: TDragOverEvent read FOnWorkDragOver write FOnWorkDragOver;
    property OnWorkKeyPress: TKeyPressEvent read FOnWorkKeyPress write FOnWorkKeyPress;
    property OnFindWorkClick: TNotifyEvent read FOnFindWorkClick write FOnFindWorkClick;
    property OnAcceptClick: TNotifyEvent read FOnAcceptClick write FOnAcceptClick;
    property OnDiscardClick: TNotifyEvent read FOnDiscardClick write FOnDiscardClick;
  end;  { TDeterminations }

//==============================================================================
implementation

uses
  BaseDragFormUnit;

resourcestring
  ResStr_OwnerNotBase = 'Owner of determinations control is not a base form';
  ResStr_AddNewDet =  'Add new determination';
  ResStr_EditSelectedDet =  'Edit selected determination';
  ResStr_DeleteSelDet = 'Delete selected determination';
  ResStr_GetDeterminerName = 'Get name of determiner';
  ResStr_DateRange =  'Dates range from 12/12/1998 to 01/02/1999';
  ResStr_AcceptChanges =  'Accept changes';
  ResStr_DiscardChanges = 'Discard changes';

  //Caption
  ResStr_Details =  'Details';
  ResStr_Taxon =  'Taxon:';
  ResStr_Biotope = 'Biotope:';
  ResStr_Determination = 'Determiner:';
  ResStr_Role = 'Role:';
  ResStr_Type = 'Type:';
  ResStr_Date = 'Date:';
  ResStr_Work = 'Work:';
  ResStr_Comments =  'Comments:'

{$R *.res}

//==============================================================================
// Sets data member FDetType to newValue.
procedure TDeterminations.SetDetType(newValue: TDeterminationType);
begin
  if FDetType <> newValue then begin
    FDetType := newValue;
    if FDetType=dtTaxon then begin
      lblTaxonBio.Caption:='Taxon:';
      bbFindTaxonBio.Hint:='Get species name';
      sgDeterminations.Rows[0].CommaText:='" ",Taxon,Determiner,Role,Type,Date';
    end else begin
      lblTaxonBio.Caption:='Biotope:';
      bbFindTaxonBio.Hint:='Get biotope name';
      sgDeterminations.Rows[0].CommaText:='" ",Biotope,Determiner,Role,Type,Date';
    end;
  end;
end;  { SetDetType }

//==============================================================================
//==============================================================================
// Exposed properties' Read/Write methods:
//==============================================================================
procedure TDeterminations.SetTaxonBioText(newValue: TCaption);
begin
  eTaxonBio.Text := newValue;
end;  { SetTaxonBioText }

//==============================================================================
function TDeterminations.GetTaxonBioText: TCaption;
begin
  GetTaxonBioText := eTaxonBio.Text;
end;  { GetTaxonBioText }

//==============================================================================
procedure TDeterminations.SetDeterminerText(newValue: TCaption);
begin
  eDeterminer.Text := newValue;
end;  { SetDeterminerText }

//==============================================================================
function TDeterminations.GetDeterminerText: TCaption;
begin
  GetDeterminerText := eDeterminer.Text;
end;  { GetDeterminerText }

//==============================================================================
procedure TDeterminations.SetRoleItems(newValue: TStrings);
begin
{  cmbRole.Items := newValue;}
end;  { SetRoleItems }

//==============================================================================
function TDeterminations.GetRoleItems: TStrings;
begin
{  GetRoleItems := cmbRole.Items;}
end;  { GetRoleItems }

//==============================================================================
procedure TDeterminations.SetRoleText(newValue: TCaption);
begin
{  cmbRole.Text := newValue;}
end;  { SetRoleText }

//==============================================================================
function TDeterminations.GetRoleText: TCaption;
begin
{  GetRoleText := cmbRole.Text;}
end;  { GetRoleText }

//==============================================================================
procedure TDeterminations.SetPreferredState(newValue: boolean);
begin
  cbPreferred.Checked := newValue;
end;  { SetPreferredState }

//==============================================================================
function TDeterminations.GetPreferredState: boolean;
begin
  GetPreferredState := cbPreferred.Checked;
end;  { GetPreferredState }

//==============================================================================
procedure TDeterminations.SetTypeItems(newValue: TStrings);
begin
{  cmbType.Items := newValue;}
end;  { SetTypeItems }

//==============================================================================
function TDeterminations.GetTypeItems: TStrings;
begin
{  GetTypeItems := cmbType.Items;}
end;  { GetTypeItems }

//==============================================================================
procedure TDeterminations.SetTypeText(newValue: TCaption);
begin
{  cmbType.Text := newValue;}
end;  { SetTypeText }

//==============================================================================
function TDeterminations.GetTypeText: TCaption;
begin
  GetTypeText := cmbType.Text;
end;  { GetTypeText }

//==============================================================================
procedure TDeterminations.SetDateText(newValue: TCaption);
begin
  eDetDate.Text := newValue;
end;  { SetDateText }

//==============================================================================
function TDeterminations.GetDateText: TCaption;
begin
  GetDateText := eDetDate.Text;
end;  { GetDateText }

//==============================================================================
procedure TDeterminations.SetWorkText(newValue: TCaption);
begin
  eWork.Text := newValue;
end;  { SetWorkText }

//==============================================================================
function TDeterminations.GetWorkText: TCaption;
begin
  GetWorkText := eWork.Text;
end;  { GetWorkText }

//==============================================================================
procedure TDeterminations.SetCommentsLines(newValue: TStrings);
begin
  reDetComments.Lines := newValue;
end;  { SetCommentsLines }

//==============================================================================
function TDeterminations.GetCommentsLines: TStrings;
begin
  GetCommentsLines := reDetComments.Lines;
end;  { GetCommentsLines }

//==============================================================================
procedure TDeterminations.SetCommentsPlainText(newValue: Boolean);
begin
  reDetComments.PlainText := newValue;
end;  { SetCommentsPlainText }

//==============================================================================
function TDeterminations.GetCommentsPlainText: Boolean;
begin
  GetCommentsPlainText := reDetComments.PlainText;
end;  { GetCommentsPlainText }

//==============================================================================
procedure TDeterminations.SetCommentsPopupMenu(newValue: TPopupMenu);
begin
  reDetComments.PopupMenu := newValue;
end;  { SetCommentsPopupMenu }

//==============================================================================
function TDeterminations.GetCommentsPopupMenu: TPopupMenu;
begin
  GetCommentsPopupMenu := reDetComments.PopupMenu;
end;  { GetCommentsPopupMenu }

//==============================================================================
//==============================================================================
procedure TDeterminations.DetAddClickHandler(Sender: TObject);
begin
  SwitchToDetails(true);
  ClearDetails;
end;  { DetAddClickHandler }

//==============================================================================
procedure TDeterminations.DetEditClickHandler(Sender: TObject);
begin
  SwitchToDetails(true);
end;  { DetEditClickHandler }

//==============================================================================
procedure TDeterminations.DetDelClickHandler(Sender: TObject);
begin
  DelLineInGrid(sgDeterminations);
end;  { DetDelClickHandler }

//==============================================================================
procedure TDeterminations.DetGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var xPos,yPos:integer;
begin
  inherited;
  with sgDeterminations do begin
    if (ACol=0) and (ARow>0) then begin
      Canvas.FillRect(Rect);
      xPos:=Rect.Left+(ColWidths[0]-13) div 2;
      yPos:=Rect.Top+(RowHeights[Row]-13) div 2;
      DrawCheckBox(Canvas,xPos,yPos,Cells[ACol,ARow]='+');
    end;
  end;
end;  // DetGridDrawCell

//==============================================================================
procedure TDeterminations.TaxonBioDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
begin
  if Assigned(FOnTaxonBioDragDrop) then
    FOnTaxonBioDragDrop(Sender, Source, X, Y);  { Substitute Self for subcomponent's Sender. }
end;  { TaxonBioDragDropTransfer }

//==============================================================================
procedure TDeterminations.TaxonBioDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnTaxonBioDragOver) then
    FOnTaxonBioDragOver(Sender, Source, X, Y, State, Accept);  { Substitute Self for subcomponent's Sender. }
end;  { TaxonBioDragOverTransfer }

//==============================================================================
procedure TDeterminations.TaxonBioKeyPressTransfer(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnTaxonBioKeyPress) then
    FOnTaxonBioKeyPress(Sender, Key);  { Substitute Self for subcomponent's Sender. }
end;  { TaxonBioKeyPressTransfer }

//==============================================================================
procedure TDeterminations.FindTaxonBioClickTransfer(Sender: TObject);
begin
  if Assigned(FOnFindTaxonBioClick) then
    FOnFindTaxonBioClick(Sender);  { Substitute Self for subcomponent's Sender. }
end;  { FindTaxonBioClickTransfer }

//==============================================================================
procedure TDeterminations.DeterminerDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
begin
  if Assigned(FOnDeterminerDragDrop) then
    FOnDeterminerDragDrop(Sender, Source, X, Y);  { Substitute Self for subcomponent's Sender. }
end;  { DeterminerDragDropTransfer }

//==============================================================================
procedure TDeterminations.DeterminerDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDeterminerDragOver) then
    FOnDeterminerDragOver(Sender, Source, X, Y, State, Accept);  { Substitute Self for subcomponent's Sender. }
end;  { DeterminerDragOverTransfer }

//==============================================================================
procedure TDeterminations.DeterminerKeyPressTransfer(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnDeterminerKeyPress) then
    FOnDeterminerKeyPress(Sender, Key);  { Substitute Self for subcomponent's Sender. }
end;  { DeterminerKeyPressTransfer }

//==============================================================================
procedure TDeterminations.FindDeterminerClickTransfer(Sender: TObject);
begin
  if Assigned(FOnFindDeterminerClick) then
    FOnFindDeterminerClick(Sender);  { Substitute Self for subcomponent's Sender. }
end;  { FindDeterminerClickTransfer }

//==============================================================================
procedure TDeterminations.WorkDragDropTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer);
begin
  if Assigned(FOnWorkDragDrop) then
    FOnWorkDragDrop(Sender, Source, X, Y);  { Substitute Self for subcomponent's Sender. }
end;  { WorkDragDropTransfer }

//==============================================================================
procedure TDeterminations.WorkDragOverTransfer(Sender: TObject; Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnWorkDragOver) then
    FOnWorkDragOver(Sender, Source, X, Y, State, Accept);  { Substitute Self for subcomponent's Sender. }
end;  { WorkDragOverTransfer }

//==============================================================================
procedure TDeterminations.WorkKeyPressTransfer(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnWorkKeyPress) then
    FOnWorkKeyPress(Sender, Key);  { Substitute Self for subcomponent's Sender. }
end;  { WorkKeyPressTransfer }

//==============================================================================
procedure TDeterminations.FindWorkClickTransfer(Sender: TObject);
begin
  if Assigned(FOnFindWorkClick) then
    FOnFindWorkClick(Sender);  { Substitute Self for subcomponent's Sender. }
end;  { FindWorkClickTransfer }

//==============================================================================
procedure TDeterminations.AcceptClickTransfer(Sender: TObject);
begin
  SwitchToDetails(false);
  if Assigned(FOnAcceptClick) then
    FOnAcceptClick(Sender);  { Substitute Self for subcomponent's Sender. }
end;  { AcceptClickTransfer }

//==============================================================================
procedure TDeterminations.DiscardClickTransfer(Sender: TObject);
begin
  SwitchToDetails(false);
  if Assigned(FOnDiscardClick) then
    FOnDiscardClick(Sender);  { Substitute Self for subcomponent's Sender. }
end;  { DiscardClickTransfer }

//==============================================================================
procedure TDeterminations.AddNew;
begin
  SwitchToDetails(true);
end;  // AddNew

//==============================================================================
procedure TDeterminations.SwitchToDetails(tfDetails:boolean);  { private }
var iCount:integer;
begin
  with gbDetDetails do
    for iCount:=0 to ControlCount-1 do
      if not (Controls[iCount] is TLabel) then
        Controls[iCount].Enabled:=tfDetails;

  bbDetAdd.Enabled :=not tfDetails;
  bbDetEdit.Enabled:=not tfDetails;
  bbDetDel.Enabled :=not tfDetails;
  sgDeterminations.Enabled:=false;

  FEditModeOn:=tfDetails;

  if tfDetails then eTaxonBio.SetFocus;
end;  { SwitchToDetails }

//==============================================================================
destructor TDeterminations.Destroy;
begin
  FDeterminationDataList.Free;
  inherited Destroy;
end;  { Destroy }

//==============================================================================
constructor TDeterminations.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 355;
  Height:= 265;

  sgDeterminations := TStringGrid.Create(Self);
  sgDeterminations.Parent := Self;

  bbDetAdd := TBitBtn.Create(Self);
  bbDetAdd.Parent := Self;

  bbDetEdit := TBitBtn.Create(Self);
  bbDetEdit.Parent := Self;

  bbDetDel := TBitBtn.Create(Self);
  bbDetDel.Parent := Self;

  gbDetDetails := TGroupBox.Create(Self);
  gbDetDetails.Parent := Self;

  lblTaxonBio := TLabel.Create(Self);
  lblTaxonBio.Parent := gbDetDetails;

  lblDeterminer := TLabel.Create(Self);
  lblDeterminer.Parent := gbDetDetails;

  lblRole := TLabel.Create(Self);
  lblRole.Parent := gbDetDetails;

  lblType := TLabel.Create(Self);
  lblType.Parent := gbDetDetails;

  lblDate := TLabel.Create(Self);
  lblDate.Parent := gbDetDetails;

  lblReference := TLabel.Create(Self);
  lblReference.Parent := gbDetDetails;

  lblComments := TLabel.Create(Self);
  lblComments.Parent := gbDetDetails;

  shpTaxonBio := TShape.Create(Self);
  shpTaxonBio.Parent := gbDetDetails;

  shpDeterminer := TShape.Create(Self);
  shpDeterminer.Parent := gbDetDetails;

  shpReference := TShape.Create(Self);
  shpReference.Parent := gbDetDetails;

  eTaxonBio := TEdit.Create(Self);
  eTaxonBio.Parent := gbDetDetails;

  bbFindTaxonBio := TBitBtn.Create(Self);
  bbFindTaxonBio.Parent := gbDetDetails;

  eDeterminer := TEdit.Create(Self);
  eDeterminer.Parent := gbDetDetails;

  bbFindDeterminer := TBitBtn.Create(Self);
  bbFindDeterminer.Parent := gbDetDetails;

  cmbRole := TDBGlyphLookupComboBox.Create(Self);
  cmbRole.Parent := gbDetDetails;

  cbPreferred := TCheckBox.Create(Self);
  cbPreferred.Parent := gbDetDetails;

  cmbType := TDBGlyphLookupComboBox.Create(Self);
  cmbType.Parent := gbDetDetails;

  eDetDate := TVagueDateEdit.Create(Self);
  eDetDate.Parent := gbDetDetails;

  eWork := TEdit.Create(Self);
  eWork.Parent := gbDetDetails;

  bbFindReference := TBitBtn.Create(Self);
  bbFindReference.Parent := gbDetDetails;

  reDetComments := TRichEdit.Create(Self);
  reDetComments.Parent := gbDetDetails;

  bbDetAccept := TBitBtn.Create(Self);
  bbDetAccept.Parent := gbDetDetails;

  bbDetDiscard := TBitBtn.Create(Self);
  bbDetDiscard.Parent := gbDetDetails;
  { CDK: Add your initialization code here. }
  dmDeterminations := TdmDeterminations.Create(self);
  FDeterminationDataList := TDeterminationDataList.Create( dtTaxon,
            dmDeterminations.qryDeterminations,
            sgDeterminations);
end;  { Create }

//==============================================================================
procedure TDeterminations.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  with sgDeterminations do begin
    SetBounds(0,0,335,85);
    ColCount := 6;
    DefaultRowHeight := 18;
    FixedCols:= 0;
    RowCount := 2;
    Options  := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect];
    TabOrder := 0;
    ColWidths[0] := 16;
    ColWidths[1] := 100;
    ColWidths[2] := 58;
    ColWidths[3] := 46;
    ColWidths[4] := 46;
    ColWidths[5] := 59;
    if FDetType=dtTaxon then Rows[0].CommaText:='" ",Taxon,Determiner,Role,Type,Date'
                        else Rows[0].CommaText:='" ",Biotope,Determiner,Role,Type,Date';
    OnDrawCell:=DetGridDrawCell;
  end;  { sgDeterminations }

  with bbDetAdd do begin
    SetBounds(335,0,20,20);
    Hint := ResStr_AddNewDet;
    TabOrder := 1;
    Glyph.LoadFromResourceName(HInstance,'BMP_DADD');
    OnClick := DetAddClickHandler;
  end;  { bbDetAdd }

  with bbDetEdit do begin
    SetBounds(335,20,20,20);
    Hint := ResStr_EditSelectedDet;
    TabOrder := 2;
    Glyph.LoadFromResourceName(Hinstance,'BMP_DEDIT');
    OnClick := DetEditClickHandler;
  end;  { bbDetEdit }

  with bbDetDel do begin
    SetBounds(335,40,20,20);
    Hint := ResStr_DeleteSelDet;
    TabOrder := 3;
    Glyph.LoadFromResourceName(Hinstance,'BMP_DDEL');
    OnClick := DetDelClickHandler;
  end;  { bbDetDel }

  with gbDetDetails do begin
    SetBounds(0,88,355,177);
    Caption := ResStr_Details;
    TabOrder := 4;
  end;  { gbDetDetails }

  with lblTaxonBio do begin
    SetBounds(8,20,39,13);
    if FDetType=dtTaxon then Caption:=ResStr_Taxon
                        else Caption:=ResStr_Biotope;
  end;  { lblTaxonBio }

  with lblDeterminer do begin
    SetBounds(8,44,54,13);
    Caption := ResStr_Determination;
  end;  { lblDeterminer }

  with lblRole do begin
    SetBounds(8,68,25,13);
    Caption := ResStr_Role;
  end;  { lblRole }

  with lblType do begin
    SetBounds(8,92,27,13);
    Caption := ResStr_Type;
  end;  { lblType }

  with lblDate do begin
    SetBounds(165,92,26,13);
    Caption := ResStr_Date;
  end;  { lblDate }

  with lblReference do begin
    SetBounds(8,116,29,13);
    Caption := ResStr_Work;
  end;  { lblReference }

  with lblComments do begin
    SetBounds(8,136,52,13);
    Caption := ResStr_Comments;
  end;  { lblComments }

  with shpTaxonBio do begin
    Tag := 2;
    SetBounds(63,15,259,23);  // Around Taxon/Biotope edit box
    Pen.Color := clRed;
  end;  { shpTaxonBio }

  with shpDeterminer do begin
    Tag := 2;
    SetBounds(63,39,259,23);  // Around Determiner edit box
    Pen.Color := clRed;
  end;  { shpDeterminer }

  with shpReference do begin
    Tag := 2;
    SetBounds(63,111,259,23);  // Around Work/Reference edit box
    Pen.Color := clRed;
  end;  { shpReference }

  with eTaxonBio do begin
    SetBounds(64,16,257,21);
    TabOrder := 0;
    OnDragDrop := TaxonBioDragDropTransfer;
    OnDragOver := TaxonBioDragOverTransfer;
    OnKeyPress := TaxonBioKeyPressTransfer;
  end;  { eTaxonBio }

  with bbFindTaxonBio do begin
    SetBounds(324,16,23,22);
    Hint := 'Get species name';
    TabOrder := 1;
    Glyph.LoadFromResourceName(HInstance,'BMP_DFIND');
    OnClick := FindTaxonBioClickTransfer;
  end;  { bbFindTaxonBio }

  with eDeterminer do begin
    SetBounds(64,40,257,21);
    TabOrder := 2;
    OnDragDrop := DeterminerDragDropTransfer;
    OnDragOver := DeterminerDragOverTransfer;
    OnKeyPress := DeterminerKeyPressTransfer;
  end;  { eDeterminer }

  with bbFindDeterminer do begin
    SetBounds(324,40,23,22);
    Hint := ResStr_GetDeterminerName;
    TabOrder := 3;
    Glyph.LoadFromResourceName(HInstance,'BMP_DFIND');
    OnClick := FindDeterminerClickTransfer;
  end;  { bbFindDeterminer }

  with cmbRole do begin
    SetBounds(64,64,145,21);
    ItemHeight := 13;
    TabOrder := 4;
  end;  { cmbRole }

  with cbPreferred do begin
    SetBounds(252,66,65,17);
    Caption := 'Preferred';
    TabOrder := 5;
  end;  { cbPreferred }

  with cmbType do begin
    SetBounds(64,88,97,21);
    ItemHeight := 13;
    TabOrder := 6;
  end;  { cmbType }

  with eDetDate do begin
    SetBounds(192,88,129,21);
    Hint := ResStr_DateRange;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clGreen;
    Font.Height := -11;
    Font.Name := 'MS Sans Serif';
    Font.Style := [];
    ParentFont := False;
    TabOrder := 7;
    Text := '12/12/1998-01/02/1999';
  end;  { eDetDate }

  with eWork do begin
    SetBounds(64,112,257,21);
    TabOrder := 8;
    OnDragDrop := WorkDragDropTransfer;
    OnDragOver := WorkDragOverTransfer;
    OnKeyPress := WorkKeyPressTransfer;
  end;  { eWork }

  with bbFindReference do begin
    SetBounds(324,112,23,22);
    Hint := ResStr_GetDeterminerName;
    TabOrder := 9;
    Glyph.LoadFromResourceName(HInstance,'BMP_DFIND');
    OnClick := FindWorkClickTransfer;
  end;  { bbFindReference }

  with reDetComments do begin
    SetBounds(64,136,237,33);
    ScrollBars := ssVertical;
    TabOrder := 10;
  end;  { reDetComments }

  with bbDetAccept do begin
    SetBounds(309,151,20,20);
    Hint := ResStr_AcceptChanges;
    TabOrder := 11;
    NumGlyphs := 2;
    Glyph.LoadFromResourceName(HInstance,'BMP_DACCEPT');
    OnClick := AcceptClickTransfer;
  end;  { bbDetAccept }

  with bbDetDiscard do begin
    SetBounds(329,151,20,20);
    Hint := ResStr_DiscardChanges;
    TabOrder := 12;
    NumGlyphs := 2;
    Glyph.LoadFromResourceName(HInstance,'BMP_DDISCARD');
    OnClick := DiscardClickTransfer;
  end;  { bbDetDiscard }
end;  { CreateWindowHandle }

//==============================================================================
procedure TDeterminations.Loaded;
begin
  inherited Loaded;
  SwitchToDetails(false);
end;  // Loaded


{ Set up the drag and drop controls on the component }
procedure TDeterminations.RegisterDragDrop;
begin
  if not (Owner is TBaseDragForm) then
    raise EDeterminationsError.Create(ResStr_OwnerNotBase);
  if DetType = dtTaxon then
    TBaseDragForm(Owner).RegisterDropComponent( eTaxonBio, TaxonDrop,
                                     ['TAXON_LIST_ITEM'], [CF_JNCCDATA] );
{  TBaseDragForm(Owner).RegisterDragComponent( FlbExternalRefs, GetExternalRefData );}
end;

procedure TDeterminations.TaxonDrop(const Sender: TObject;
      const iFormat: integer; const iSourceData: TKeyList;
      const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if iFormat<>CF_JNCCDATA then
    ioHandled := False
  else
  begin
     {Note - can only drop one list item at a time - ignore the rest }
     {eTaxonBio.Text := GetTaxon
     ioHandled := True;}
  end;
end;

{ Blank all the detail controls }
procedure TDeterminations.ClearDetails;
begin
  eTaxonBio.Text := '';
  eDeterminer.Text := '';
{  cmbRole.ItemIndex := -1;}
  cbPreferred.Checked := True;
{  cmbType.ItemIndex := -1;}
  eDetDate.Text := '';
  eWork.Text := '';
  reDetComments.Lines.Clear;
end;


end.

