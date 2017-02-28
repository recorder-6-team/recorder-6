//==============================================================================
//  Unit:        TermLists
//
//  Implements:  TfrmTermLists
//               TLinkedTable
//               TTermList
//               TRecordingCard
//
//  Description: Implements the functionalities to maintain lists of terms used
//               across the application. TLinkedTable, TTermList and
//               TRecordingCard are helper classes designed to facilitate the
//               process.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     DBRichEdit text attributes reset before displaying records.
//               Fixes the text attribute persistence problem.
//
//               Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 130 $
//    $Date: 12/03/09 13:41 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\DelphiVersions.Inc'}

unit TermLists;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  BaseChildUnit, ExtCtrls, ComCtrls, StdCtrls, Buttons, ImgList, Menus, DBCtrls,
  ExtDlgs, BaseFormUnit, DBListCombo, JNCCDatasets, DataClasses, ActnList,
  ExceptionForm, Mask, TermListsData, HierarchyNodes, OnlineHelp, Constants,
  CheckLst, GeneralFunctions, ImageListButton, DatabaseAccessADO, DropSource, Grids, DssStringGrid
  {$IFDEF DELPHI7UP}, Variants {$ENDIF};

resourcestring
  STermList_Details = '%s Details';

type
  ETermList = class(TExceptionPath);
  ERecordAccess = class(ETermList);

  TLinkedTable = class
  private
    FTable: String;
    FKeyField: String;
    FDescription: String;
  public
    property Table: String read FTable write FTable;
    property KeyField: String read FKeyField write FKeyField;
    property Description: String read FDescription write FDescription;
  end;

  TTermList = class
  private
    FTable : String;
    FKeyField: String;
    FDescription : String;
    FSystemSupplied : Boolean;
    FAdditionalFields : Boolean;
    FLinkedTables: TList;
    function GetLinkedTable(Index: Integer): TLinkedTable;
    function GetLinkedTablesCount: Integer;
  public
    destructor Destroy; override;
    procedure AddLinkedTable(const ATable, AKeyField, ADescription: String);
    property Table : String read FTable write FTable;
    property KeyField : string read FKeyField write FKeyField;
    property Description : String read FDescription write FDescription;
    property SystemSupplied : Boolean read FSystemSupplied write FSystemSupplied;
    property AdditionalFields : Boolean read FAdditionalFields write FAdditionalFields;
    property LinkedTablesCount: Integer read GetLinkedTablesCount;
    property LinkedTables[Index: Integer]: TLinkedTable read GetLinkedTable;
  end;

  TEntryMode = (emAddChild, emAddSibling, emEdit, emNone);
  TButtonMenuState = (bmsEdit, bmsAdd, bmsBrowse);

  TRecordingCard = class(TObject)
  private
    FCardName: string;
    FFileName: string;
  public
    property Cardname: string read FCardName write FCardName;
    property FileName: string read FFileName write FFileName;
  end;

  TfrmTermLists = class(TBaseChild)
    pmEdit: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuEdit: TMenuItem;
    mnuDelete: TMenuItem;
    mnuChildEdit: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    N2: TMenuItem;
    mnuEditBold: TMenuItem;
    mnuEditItalic: TMenuItem;
    mnuEditUnderline: TMenuItem;
    mnuEditAdd: TMenuItem;
    mnuEditEdit: TMenuItem;
    mnuEditDelete: TMenuItem;
    N3: TMenuItem;
    pnlButtons: TPanel;
    pnlLabel: TPanel;
    Label1: TLabel;
    TermListSplitter: TSplitter;
    scbTermDetails: TScrollBox;
    gbDetails: TGroupBox;
    gbAdditionalInfo: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    dlgOpenPic: TOpenPictureDialog;
    Label2: TLabel;
    cmbTermLists: TComboBox;
    lblTextField1: TLabel;
    lblTextField2: TLabel;
    lblTextField3: TLabel;
    bvlImage: TBevel;
    lblAssocRecordCard: TLabel;
    cmbAssocRecordCard: TComboBox;
    eShortName: TEdit;
    eLongName: TEdit;
    dbeAddField1: TDBEdit;
    dbeAddField2: TDBEdit;
    dbeAddField3: TDBEdit;
    mnuAddSibling: TMenuItem;
    mnuAddChild: TMenuItem;
    pmAdd: TPopupMenu;
    mnuChild: TMenuItem;
    mnuSibling: TMenuItem;
    mnuMainAddChild: TMenuItem;
    mnuMainAddSibling: TMenuItem;
    dbreDescription: TDBRichEdit;
    tvTermList: TTreeView;
    imgSample: TDBImage;
    alTermLists: TActionList;
    actAddSibling: TAction;
    actAddChild: TAction;
    actEdit: TAction;
    actDelete: TAction;
    chklbScreens: TCheckListBox;
    bbAdd: TImageListButton;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    bbSave: TImageListButton;
    bbCancel: TImageListButton;
    cmbDataTypes: TComboBox;
    bbOpen: TImageListButton;
    bbDefault: TButton;
    rgDeterminationVerification: TRadioGroup;
    chkHide: TCheckBox;
    btnAddValue: TImageListButton;
    btnDelValue: TImageListButton;
    sgUnitValues: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvTermListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvTermListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TermListSplitterCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
    procedure TermListSplitterPaint(Sender: TObject);
    procedure bbOpenClick(Sender: TObject);
    procedure bbDefaultClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actAddSiblingExecute(Sender: TObject);
    procedure actAddChildExecute(Sender: TObject);
    procedure tvTermListChange(Sender: TObject; Node: TTreeNode);
    procedure reDescriptionEnter(Sender: TObject);
    procedure reDescriptionExit(Sender: TObject);
    procedure eShortNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmbTermListsChange(Sender: TObject);
    procedure dbeAddField1Change(Sender: TObject);
    procedure tvTermListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgUnitValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgUnitValuesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnAddValueClick(Sender: TObject);
    procedure btnDelValueClick(Sender: TObject);
    procedure sgUnitValuesKeyPress(Sender: TObject; var Key: Char);
  private
    FdmTermLists     : TdmTermLists;
    FCurrTermList    : TTermList;
    FCurrTermListItem: TTermListNode;
    FLockNodeUpdate  : Boolean;
    FMode            : TEntryMode;
    FKeyField        : String;  // To remember the key field for active term list table
    FTableName       : String;  // And the Table name
    FSelectedItemCustodian: String;
    procedure AddFields(const iField: TFieldDef; const iiCount: integer);
    procedure AddRecordingCard(const iFilename: string);
    procedure ButtonMenuState(const iState: TButtonMenuState);
    function CheckTermList(ATermList: TTermList): boolean;
    procedure CheckValidationCompetency;
    procedure ClearContextsList;
    procedure ClearList;
    procedure ClearTreeView;
    function DuplicateDataValues: Boolean;
    procedure PopulateContextsList;
    procedure PopulateDetails(const itfIncludeDBFields: boolean);
    function PopulateItems(AParent: TTreeNode; iDataset: TDataset; const iLinkedTable: Integer = 0): boolean;
    function PopulateLists: boolean;
    procedure PopulateRecordingCards;
    procedure RetrieveRecordingCard;
    procedure SaveImage;
    procedure SetContextsList(const istKey: String; const itfShow: boolean);
    procedure SetDetTypesList(const istKey: String; const itfShow: boolean);
    procedure SetCurrTermListItem(const Value: TTermListNode);
    procedure SetImage(const itfExists: boolean);
    procedure SetRecordCard(const itfExists: boolean);
    procedure SetupAddInfo;
    procedure SetupAdditionalFieldDisplay;
    procedure SwitchMode(const iMode: TEntryMode);
    property CurrTermListItem: TTermListNode read FCurrTermListItem write SetCurrTermListItem;
    procedure SetTermListTable(const ATableName, AKeyField: String);
    procedure GetTermListNodeData(const Sender: TObject; var oDropSource: TJNCCDropSource);
    function CurrentCustodian(const AKey: TKeyString): string;
    procedure SetHideCheckBox(const ATableName: string);
  protected
    procedure SetUpDestinationControls; override;
    function GetCurrentControlEditMode:TEditMode; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, FormActions, GeneralData, ApplicationSettings, Genfuncs,
  BaseADODataModule;

const
  CARD_EXTENSION  = '.CRD';
  MAX_DATA_LENGTH = 20;    // Max length of data values.

type
  TGridAccessor = class(TStringGrid)
  public
    property InplaceEditor;
  end;

resourcestring
  ResStr_IndexOutOfBounds = 'Index out of bounds';

  ResStr_WrongBitmapSize =  'The chosen bitmap is the wrong size. ' +
                            'Bitmaps must have a height and width of 16 pixels (16x16).';

  ResStr_BitmapFileType = 'You may only load bitmap type files (*.bmp)';
  ResStr_UnableToFindFile = 'Unable to locate file.';
  ResStr_CannotLocateRecord = 'Unable to locate record';
  ResStr_ShortNameRequired =  'A Short Name is required. Please enter a Short Name.';
  ResStr_LongNameRequired =  'A Long Name is required. Please enter a Long Name.';
  ResStr_ValueRequired =  ' A value is required.';

  ResStr_CustodyChanged = 'The custody of this record has changed to another Site ID in the underlying database.'#13#10 +
                          'You are no longer allowed to edit it.';

  ResStr_CannotDeleteParent = 'Unable to delete a node that has children. Please delete the children first';
  ResStr_DeleteTermList = 'Are you sure you wish to delete ''%s''?';
  ResStr_CannotDeleteItemRef =  'Unable to delete this item as it is referenced by other records in the database';
  ResStr_CannotLocateRecordDelete = 'Unable to locate record for deletion.';
  ResStr_EnterFieldForDesc =  'You  have to enter at least one %s for each %s in the list.';
  ResStr_InvalidIntegerValue =  '''%s'' is not a valid integer value.';

  ResStr_TooBigValue =  'The value ''%s'' is too big. The maximum limit ' +
                        'is 3. Make sure the number entered doesn''t exceed this limit.';

  ResStr_AllowedValues = 'Allowed Values';
  ResStr_ValueAlreadyInList = 'This value is already in the list.';

//==============================================================================
//  TTermList
//==============================================================================
destructor TTermList.Destroy;
var liIdx: Integer;
begin
  if FLinkedTables <> nil then begin
    for liIdx:=0 to FLinkedTables.Count-1 do
      TObject(FLinkedTables[liIdx]).Free;
    FLinkedTables.Free;
    FLinkedTables := nil;
  end;

  inherited;
end;

//==============================================================================
procedure TTermList.AddLinkedTable(const ATable, AKeyField, ADescription: String);
var loLinkedTable: TLinkedTable;
begin
  // Make sure the list exists first
  if FLinkedTables = nil then FLinkedTables := TList.Create;
  // Create new Linked Table object and initialize it
  loLinkedTable := TLinkedTable.Create;
  with loLinkedTable do begin
    Table      := ATable;
    KeyField   := AKeyField;
    Description:= ADescription;
  end;
  // Add new linked table to list
  FLinkedTables.Add(loLinkedTable);
end;

//==============================================================================
function TTermList.GetLinkedTable(Index: Integer): TLinkedTable;
begin
  Result := nil;
  if FLinkedTables <> nil then begin
    if Index in [0..FLinkedTables.Count-1] then
      Result := FlinkedTables[Index]
    else
      raise TExceptionPath.CreateNonCritical(ResStr_IndexOutOfBounds)
  end;
end;

//==============================================================================
function TTermList.GetLinkedTablesCount: Integer;
begin
  Result := 0;
  if FLinkedTables<>nil then Result := FLinkedTables.Count;
end;

//==============================================================================
//  TfrmTermLists
//==============================================================================
constructor TfrmTermLists.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  cmbDataTypes.ItemIndex := 0;
  //Load data module
  FdmTermLists := TdmTermLists.Create(nil);
  if PopulateLists then
  begin
    with cmbTermLists do begin
      ItemIndex := 0;
      FCurrTermList := TTermList(Items.Objects[ItemIndex]);
    end;
    // Link the table to the selected TermList
    SetTermListTable(FCurrTermList.Table, FCurrTermList.KeyField);
    PopulateRecordingCards;
    PopulateContextsList;
    dbreDescription.Datafield := 'Description';
    { Force controls to update according to selected list }
    cmbTermListsChange(nil);
    ButtonMenuState(bmsBrowse);
  end;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);

  //Help Setup
  HelpContext        := IDH_TERMLISTS;
  mnuEdit.HelpContext:= IDH_EDITMENU;
  FEditMode := emView;
end;  // Create

//==============================================================================
procedure TfrmTermLists.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self, mnuChildEdit, 4, []);
end;  // FormActivate

//==============================================================================
procedure TfrmTermLists.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmTermLists.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose:=false;
  if bbSave.Enabled then begin  // Still in edit mode
    Beep;
    case ConfirmSaveAndClose of
      mrYes : begin
                bbSaveClick(nil);
                CanClose:=true;
              end;
      mrNo  : begin
                bbCancelClick(nil);
                CanClose:=true;
              end;
    end;
  end else
    CanClose:=CheckTermList(FCurrTermList);
end;  // FormCloseQuery

//==============================================================================
destructor TfrmTermLists.Destroy;
begin
  //Free objects
  ClearContextsList;
  ClearTreeView;
  ClearList;
  //Free data module
  FdmTermLists.Free;
  FdmTermLists:=nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmTermLists.ClearContextsList;
var lIdx:integer;
begin
  with chklbScreens.Items do begin
    for lIdx:=0 to Count-1 do Objects[lIdx].Free;
    Clear;
  end;
end;  // ClearContextsList

//==============================================================================
procedure TfrmTermLists.ClearList;
var lIdx:integer;
begin
  with cmbTermLists.Items do begin
    for lIdx:=0 to Count-1 do Objects[lIdx].Free;
    Clear;
  end;
end;  // ClearList

//==============================================================================
procedure TfrmTermLists.ClearTreeView;
var lIdx:integer;
begin
  LockWindowUpdate(Handle);
  try
    // Remove all objects
    with tvTermList do begin
      for lIdx:=0 to Items.Count-1 do begin
        TTermListNode(Items[lIdx].Data).Free;
        Items[lIdx].Data := nil;
      end; // for
      // Don't want any unnecessary processing going on if we're leaving the party...
      if csDestroying in ComponentState then FLockNodeUpdate := true;
      // Clear items
      Items.Clear;
    end;
  finally
    LockWindowUpdate(0);
  end; // try..finally
end;  // ClearTreeView

//==============================================================================
procedure TfrmTermLists.tvTermListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  Accept := (Source=Sender) and (cmbTermLists.ItemIndex<>2);
end;  // tvTermListDragOver

//==============================================================================
procedure TfrmTermLists.tvTermListDragDrop(Sender, Source: TObject; X, Y: Integer);
var DestNode : TTreeNode;
begin
  inherited;
  DestNode := tvTermList.GetNodeAt(X,Y);
  tvTermList.Selected.MoveTo(DestNode, naAddChild);
end;  // tvTermListDragDrop

//==============================================================================
procedure TfrmTermLists.FormResize(Sender: TObject);
begin
  inherited;
  tvTermList.Refresh;
end;  // FormResize

//==============================================================================
procedure TfrmTermLists.TermListSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  inherited;
  Accept:=(ClientWidth-NewSize>bbAdd.Width+bbEdit.Width+TermListSplitter.Width) or
          ((tvTermList.Width<bbAdd.Width+bbEdit.Width) and (NewSize<=scbTermDetails.Width));
end;  // TermListSplitterCanResize

//==============================================================================
procedure TfrmTermLists.TermListSplitterPaint(Sender: TObject);
begin
  inherited;
  DrawVertSplitter(Canvas, TermListSplitter);
end;  // TermListSplitterPaint

//==============================================================================
procedure TfrmTermLists.bbOpenClick(Sender: TObject);
begin
  inherited;
  dlgOpenPic.DefaultExt:=GraphicExtension(TBitmap);
  dlgOpenPic.Filter    :=GraphicFilter(TBitmap);
  if dlgOpenPic.Execute then
  begin
    if FileExists(dlgOpenPic.FileName) then
    begin
      if SameText(ExtractFileExt(dlgOpenPic.FileName),'.bmp') then
      begin
        imgSample.Picture.LoadFromFile(dlgOpenPic.FileName);
        if (imgSample.Picture.Width <> 16) or (imgSample.Picture.Height <> 16) then
        begin
          imgSample.Picture := nil;
          MessageDlg(ResStr_WrongBitmapSize, mtWarning,[mbOK],0);
        end;
      end else
        Raise TexceptionPath.CreateNonCritical(ResStr_BitmapFileType);
    end else
      Raise TExceptionPath.CreateNonCritical(ResStr_UnableToFindFile);
  end;
end;  // bbOpenClick

//==============================================================================
procedure TfrmTermLists.bbDefaultClick(Sender: TObject);
begin
  inherited;
  if FileExists(ExtractFilePath(Application.ExeName) + 'DEFAULT.BMP') then
    imgSample.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'DEFAULT.BMP');
end;  // bbDefaultClick

//==============================================================================
procedure TfrmTermLists.PopulateContextsList;
var loKey: TKey;
begin
  // Get all the screens from the MEASUREMENT_CONTEXT table and into the checklistbox
  with FdmTermLists.tblMeasurementContexts do begin
    TableName := TN_MEASUREMENT_CONTEXT;
    Open;
    First;
    while not Eof do begin
      // Get the Screen key, for Measurement_Type_Context later
      loKey := TKey.Create;
      loKey.Key := FieldByName('MEASUREMENT_CONTEXT_KEY').AsString;
      chklbScreens.Items.AddObject(FieldByName('CONTEXT_NAME').AsString, loKey);
      Next;
    end;
    Close;
  end;
  // Rearrange alphabetically once populated.
  chklbScreens.Sorted := true;
end;

//==============================================================================
function TfrmTermLists.PopulateLists: boolean;
var lNewTermList : TTermList;
begin
  // Populate the TermList combobox
  with FdmTermLists do
    with qryTermLists do begin
      Open;
      First;
      while not Eof do begin
        //Create the new object and populate it.
        lNewTermList := TTermList.Create;
        lNewTermList.Table           :=FieldByName('Table').AsString;
        lNewTermList.KeyField        :=Fieldbyname('Key_Field').AsString;
        lNewTermList.Description     :=FieldByName('Description').AsString;
        lNewTermList.SystemSupplied  :=FieldByName('System_Supplied_Data').AsBoolean;
        lNewTermList.AdditionalFields:=FieldByName('Additional_Fields').AsBoolean;

        // If any tables are linked to the current one, get them
        if (UpperCase(FieldByName('Linked_Table').AsString) = 'YES') then begin
          InitLinkedTablesQuery(lNewTermList.Table);
          with qryLinkedTables do begin
            Open;
            while not Eof do begin
              lNewTermList.AddLinkedTable(FieldByName('Table').AsString,
                                          FieldByName('Key_Field').AsString,
                                          FieldByName('Description').AsString);
              Next;
            end;
            Close;
          end;
        end;
        // Add TermList to combobox
        cmbTermLists.Items.AddObject(lNewTermList.Description, lNewTermList);
        Next;
      end; // while
      Close;
    end;  // with qryTermLists
  Result := cmbTermLists.Items.Count > 0; //Return true if items were added.
end;  // PopulateLists

//==============================================================================
function TfrmTermLists.PopulateItems(AParent: TTreeNode; iDataset: TDataset;
  const iLinkedTable: Integer): boolean;
var lNewTermListItem  : TTermListNode;
    lNewNode, lMidNode: TTreeNode;
    liIdx             : Integer;
    lCursor           : TCursor;
begin
  lCursor := HourglassCursor;
  tvTermList.Items.BeginUpdate;
  // Unlink control to stop flickering while refreshing the list.
  dbreDescription.DataSource := nil;
  try
    if AParent = nil then ClearTreeView; //Don't clear if we're adding children!
    // iDataset is ponting to the table containing the termlist items
    with iDataset do begin
      Open;
      First;
      // Get each term in turm
      while not Eof do begin
        lNewTermListItem := TTermListNode.Create;
        lNewTermListItem.ShortName  := FieldByName('Short_Name').AsString;
        lNewTermListItem.LongName   := FieldByName('Long_Name').AsString;
        lNewTermListItem.Description:= FieldByName('Description').AsString;

        if Assigned(FindField('System_Supplied_Data')) then // field is not necessarily in table
          lNewTermListItem.SysSupplied:= FieldByName('System_Supplied_Data').AsBoolean;

        // If it's a top level term
        if AParent = nil then begin
          // Add current term as a top level tree node
          lNewTermListItem.ItemKey := FieldByName(FCurrTermList.KeyField).AsString;
          lNewNode := tvTermList.Items.AddObject(nil, lNewTermListItem.ShortName, lNewTermListItem);
          //Check if this table has linked detail tables.
          if FCurrTermList.LinkedTablesCount > 0 then
          begin
            // More than one linked tables, get an extra level in tree to mark the fact
            if FCurrTermList.LinkedTablesCount > 1 then
              for liIdx:=0 to FCurrTermList.LinkedTablesCount-1 do begin
                // Extra middle nodes don't need objects
                lMidNode := tvTermList.Items.AddChild(lNewNode, FCurrTermList.LinkedTables[liIdx].Description);
                FdmTermLists.InitLinkedListQuery(FCurrTermList.LinkedTables[liIdx].Table,
                                                 FCurrTermList.KeyField,
                                                 lNewTermListItem.ItemKey);
                //call self with the node returned above.
                PopulateItems(lMidNode, FdmTermLists.qryLinkedList, liIdx);
              end
            else begin
              FdmTermLists.InitLinkedListQuery(FCurrTermList.LinkedTables[0].Table,
                                               FCurrTermList.KeyField,
                                               lNewTermListItem.ItemKey);
              //call self with the node returned above.
              PopulateItems(lNewNode, FdmTermLists.qryLinkedList);
            end;
          end; //LinkedTable check
        end else // AParent <> nil
        // Not top level term, must be from a linked table
        begin
          lNewTermListItem.ItemKey := FieldByName(FCurrTermList.LinkedTables[iLinkedTable].KeyField).AsString;
          tvTermList.Items.AddChildObject(AParent, lNewTermListItem.ShortName, lNewTermListItem);
        end;
        Next;
      end;  // while
      Close;
    end; {with}
    with tvTermList do begin
      SortType := ComCtrls.stText;
      SortType := ComCtrls.stNone;
      Selected:=Items.GetFirstNode;
      if Selected=nil then CurrTermListItem := nil
                      else CurrTermListItem := TTermListNode(Selected.Data);
      Result:=Selected<>nil;
    end;
  finally
    // Relink control.
    dbreDescription.DataSource := FdmTermLists.dsTermList;
    tvTermList.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // PopulateItems

//==============================================================================
procedure TfrmTermLists.SetupAddInfo;
var
  i           :integer;
  liAddFields :integer;
  lstFieldName:string;
begin
  // Hide detail section until required
  gbAdditionalInfo.Visible := false;
  SetContextsList('', false);
  SetDetTypesList('', false);
  SetImage(false);
  SetRecordCard(false);
  SetHideCheckBox(FCurrTermList.Table);
  AddFields(nil, 0); //A parameter of 0 hides all the fields.

  // Special additional field for measurements
  if SameText(FCurrTermList.Table, TN_MEASUREMENT_TYPE) and
     (tvTermList.Selected.Parent = nil) then begin
    SetContextsList(TTermListNode(tvTermList.Selected.Data).ItemKey, true);
    gbAdditionalInfo.Visible := true;
  end else
  if SameText(FCurrTermList.Table, TN_DETERMINATION_TYPE) and
     (tvTermList.Selected.Parent = nil) then begin
    SetDetTypesList(TTermListNode(tvTermList.Selected.Data).ItemKey, true);
    gbAdditionalInfo.Visible := true;
  end else
    // Otherwise, proceed with extra fields, if any
    with FdmTermLists.qryTermList do begin
      FieldDefs.Update;
      with FieldDefs do
        if Count > 8 then begin
          liAddFields:=0;
          for i:=0 to Count-1 do begin
            lstFieldName:=Uppercase(Items[i].Name);  // Make sure we have uppercase
            if lstFieldName = 'IMAGE' then begin
              SetImage(true);
              gbAdditionalInfo.Visible := true;
            end else if lstFieldName = 'RECORDING_CARD' then begin
              SetRecordCard(true);
              gbAdditionalInfo.Visible := true;
            end else if lstFieldName = 'SHORT_NAME' then
              // Short name fields vary in length
              eShortName.MaxLength := Items[i].Size
            else
              //The following field names MUST be UPPERCASE.
            if (Pos('_KEY',lstFieldName)=0) and  // Don't include any key fields (foreign or not)!!!!!
               (lstFieldName <> 'LONG_NAME') and
               (lstFieldName <> 'DESCRIPTION') and
               (lstFieldName <> 'ENTERED_BY') and
               (lstFieldName <> 'ENTRY_DATE') and
               (lstFieldName <> 'CHANGED_DATE') and
               (lstFieldName <> 'CHANGED_BY') and
               (lstFieldName <> 'SYSTEM_SUPPLIED_DATA') and
               (lstFieldName <> 'CUSTODIAN') and
               (lstFieldName <> 'HIDE') and
               (liAddFields < 3) then
            begin
              Inc(liAddFields);
              AddFields(Items[i], liAddFields);
              gbAdditionalInfo.Visible := true;
            end;
          end; //for i := 0 to tblTable.Fields.Count - 1
        end; //if tblTable.Fields.Count > 8
    end;  //with tblTable
end;  // SetupAddInfo

//==============================================================================
procedure TfrmTermLists.AddFields(const iField: TFieldDef; const iiCount: integer);
type
  TAddField = record
    FieldName  : String;
    FieldType  : TFieldType;
    DisplayName: String;
  end;

var
  arrAddFields: array[1..3] of TAddField;
begin
  if iField <> nil then
  begin
    arrAddFields[iiCount].FieldType := iField.DataType;
    arrAddFields[iiCount].FieldName := iField.Name;
    arrAddFields[iiCount].DisplayName := ReadableFormat(iField.DisplayName);
  end;

  // Show and hide additional fields
  lblTextField1.Visible := iiCount in [1, 2, 3];
  // Special case for first field if displaying Data Types, show list box instead of edit box
  dbeAddField1.Visible  := lblTextField1.Visible and (arrAddFields[iiCount].DisplayName <> 'Data Type');
  cmbDataTypes.Visible  := lblTextField1.Visible and (arrAddFields[iiCount].DisplayName = 'Data Type');
  sgUnitValues.Visible  := cmbDataTypes.Visible;
  btnAddValue.Visible   := cmbDataTypes.Visible;
  btnDelValue.Visible   := cmbDataTypes.Visible;
  
  // Show/hide the others
  lblTextField2.Visible := iiCount in [2, 3];
  dbeAddField2.Visible  := lblTextField2.Visible;
  lblTextField3.Visible := iiCount = 3;
  dbeAddField3.Visible  := lblTextField3.Visible;
  // Link to dataset fields if needed - note we reposition fields according to size
  case iiCount of
    0 : begin //Special case, hide all. Make the datafields = ''
          dbeAddField1.DataField := '';
          dbeAddField2.DataField := '';
          dbeAddField3.DataField := '';
        end;
    1 : begin //Only one add field, setup 1.
          lblTextField1.Caption  := arrAddFields[iiCount].DisplayName + ': ';
          lblTextField1.Left     := dbeAddField1.Left - (lblTextField1.Width + 10);
          dbeAddField1.DataField := iField.Name;
        end;
    2 : begin //two add fields, 1 already visible and set, setup 2.
          lblTextField2.Caption  := arrAddFields[iiCount].DisplayName + ': ';
          lblTextField2.Left     := dbeAddField2.Left - (lblTextField2.Width + 10);
          dbeAddField2.DataField := iField.Name;
        end;
    3 : begin //three add fields, 1 and 2 already visible and set, setup 3.
          lblTextField3.Caption  := arrAddFields[iiCount].DisplayName + ': ';
          lblTextField3.Left     := dbeAddField3.Left - (lblTextField3.Width + 10);
          dbeAddField3.DataField := iField.Name;
        end;
  end;
  sgUnitValues.Left  := lblTextField1.Left;
  sgUnitValues.Width := btnAddValue.Left - sgUnitValues.Left + 1;
end;  // AddFields

//==============================================================================
{ Turn on or off the group of components for attaching images to terms. }
procedure TfrmTermLists.SetImage(const itfExists: boolean);
begin
  imgSample.Visible:= itfExists;
  bvlImage.Visible := itfExists;
  bbOpen.Visible   := itfExists;
  bbDefault.Visible:= itfExists;
  if itfExists then imgSample.DataField := 'IMAGE'
               else imgSample.Datafield := '';
end;  // SetImage

//==============================================================================
{ Turn on or off the group of components for attaching record cards to terms. }
procedure TfrmTermLists.SetRecordCard(const itfExists: boolean);
begin
  cmbAssocRecordCard.Visible := itfExists;
  lblAssocRecordCard.Visible := itfExists;
end;  // SetRecordCard

//==============================================================================
procedure TfrmTermLists.SetupAdditionalFieldDisplay;
var lField: TField;
begin
  lField := FdmTermLists.qryTermList.FindField('Validation_Competency');
  if lField <> nil then begin
    lField.Alignment         := taLeftJustify;
    lField.ValidChars        := ['0'..'9'];
    lField.DefaultExpression := '0';
  end;
end;  // SetupAdditionalFieldDisplay

//==============================================================================
procedure TfrmTermLists.PopulateDetails(const itfIncludeDBFields: boolean);
begin
  inherited;
  with tvTermList do
    // If top level or no selected node
    if ((Selected<>nil) and (Selected.Parent=nil)) or (Selected=nil) then
      gbDetails.Caption := Format(STermList_Details, [ReadableFormat(FCurrTermList.Table)])
    else
      // Part of linked termlist nodes
      if FCurrTermList.LinkedTablesCount = 1 then
        gbDetails.Caption := Format(
            STermList_Details,
            [ReadableFormat(FCurrTermList.LinkedTables[0].Table)])
      else
      if Selected.Data = nil then  // Middle level node, no data
        gbDetails.Caption := Format(
            STermList_Details,
            [ReadableFormat(FCurrTermList.LinkedTables[Selected.Index].Table)])
      else
        gbDetails.Caption := Format(
            STermList_Details,
            [ReadableFormat(FCurrTermList.LinkedTables[Selected.Parent.Index].Table)]);

  if CurrTermListItem <> nil then begin
    eShortName.Text:=CurrTermListItem.ShortName;
    eLongName.Text :=CurrTermListItem.LongName;
    ResetDBRichEditControls([dbreDescription]);
    // Set image to true if required.  Need the false bit first to force a refresh
    if SameText(FCurrTermList.Table, 'SAMPLE_TYPE') then
      SetImage(True);  
    //Retrieve the recording card...
    if cmbAssocRecordCard.Visible then
      RetrieveRecordingCard;
    // Always clear this grid, regardless.
    sgUnitValues.Cols[0].Text := ResStr_AllowedValues;  // Easy way to reset content
    sgUnitValues.RowCount := 2;
    //Should we do the DB fields?
    if itfIncludeDBFields then begin
      with FdmTermLists.qryTermList do
      begin
        if Active then Close;
        FieldDefList.Update;
        Open;  // re-open and don't close!!
        SetupAdditionalFieldDisplay;
        Locate(FKeyField, CurrTermListItem.ItemKey, []);
        // If Data_Type field, locate the correct item in list box
        if FindField('Data_Type') <> nil then begin
          with cmbDataTypes do begin
            ItemIndex := Items.Count - 1;
            while (Items[ItemIndex][1] <> FieldByName('Data_Type').AsString) and
                  (ItemIndex <>  -1) do
              ItemIndex := ItemIndex - 1;
          end;
          // Do the Unit values if any
          with dmDatabase.GetRecordset(
              'usp_MeasurementUnitValue_Select_ForMeasurementUnit',
              ['@Key', CurrTermListItem.ItemKey]) do
          begin
            while not Eof do begin
              if sgUnitValues.Cells[0, sgUnitValues.RowCount - 1] <> '' then
                sgUnitValues.RowCount := sgUnitValues.RowCount + 1;
              sgUnitValues.Cells[0, sgUnitValues.RowCount - 1] := Fields['Data'].Value;
              MoveNext;
            end;
            Close;
          end;
        end;
        if FindField('Hide') <> nil then
          chkHide.Checked := FieldByName('Hide').AsBoolean;
      end;
    end;
  end else begin
    eShortName.Text:= '';
    eLongName.Text := '';
  end;
end;  // PopulateDetails

//==============================================================================
procedure TfrmTermLists.SwitchMode(const iMode: TEntryMode);
var
  lNode: TTreeNode;
  lTermListItem: TTermListNode;
begin
  // Just looking...
  if iMode = emNone then begin
    FEditMode := emView;
    ButtonMenuState(bmsBrowse);
    tvTermList.SetFocus;
    if (tvTermList.Selected = nil) or (tvTermList.Selected.Parent = nil) then
      SetTermListTable(FCurrTermList.Table, FCurrTermList.KeyField)
    else
      if FCurrTermList.LinkedTablesCount = 1 then
        // Only one linked table to choose from
        SetTermListTable(FCurrTermList.LinkedTables[0].Table, FCurrTermList.LinkedTables[0].KeyField)
      else
      if Assigned(tvTermList.Selected.Data) then
        // Selected.PARENT.Index is the index of the linked table in this case
        try
          SetTermListTable(FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].Table,
                           FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].KeyField);
        except on TExceptionPath do
          Exit;//exit function: this is an intermediate node;
        end
      else begin
        gbAdditionalInfo.Visible := False;
        Exit;
      end;
    PopulateDetails(True);
    FLockNodeUpdate := false;
    SetUpAddInfo;
    SetUpAdditionalFieldDisplay;
  end else
  // Doing something, Add/Edit
  begin
    FEditMode := TEditMode(emEdit);
    FLockNodeUpdate := true;
    if (iMode = emAddChild) or (iMode = emAddSibling) then
      ButtonMenuState(bmsAdd)
    else
      ButtonMenuState(bmsEdit);
    eShortName.SetFocus;

    with FdmTermLists.qryTermList do
    begin
      if Active then Close; //If it's active close it.

      if (iMode = emAddChild) then begin
        if FCurrTermList.LinkedTablesCount = 1 then
          // Only one linked table to choose from
          SetTermListTable(FCurrTermList.LinkedTables[0].Table, FCurrTermList.LinkedTables[0].KeyField)
        else
          // Selected.Index is the index of the linked table in this case
          SetTermListTable(FCurrTermList.LinkedTables[tvTermList.Selected.Index].Table,
                           FCurrTermList.LinkedTables[tvTermList.Selected.Index].KeyField);
      end else
      // Add sibling or edit current, need the correct table name.
      begin
        if (tvTermList.Selected = nil) or (tvTermList.Selected.Parent = nil) then
          SetTermListTable(FCurrTermList.Table, FCurrTermList.KeyField)
        else
          if FCurrTermList.LinkedTablesCount = 1 then
            // Only one linked table to choose from
            SetTermListTable(FCurrTermList.LinkedTables[0].Table, FCurrTermList.LinkedTables[0].KeyField)
          else
            // Selected.PARENT.Index is the index of the linked table in this case
            SetTermListTable(FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].Table,
                             FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].KeyField);
      end;
      // Setup additional fields before openning table for edit or insert
      SetupAddInfo;

      Open;
      SetupAdditionalFieldDisplay;
      // Adding new node
      if (iMode = emAddChild) or (iMode = emAddSibling) then
      begin
        lTermListItem := TTermListNode.Create;
        Append;
        FieldByName(FKeyField).AsString := dmGeneralData.GetNextKey(FTableName, FKeyField);
        ResetDBRichEditControls([dbreDescription]);
        dmGeneralData.SetNameIDAndDate(FdmTermLists.qryTermList, 'Entered_By', 'Entry_Date');

        if Assigned(FindField('SYSTEM_SUPPLIED_DATA')) then // field is not necessarily in table
          lTermListItem.SysSupplied:= FieldByName('SYSTEM_SUPPLIED_DATA').AsBoolean;

        lTermListItem.ItemKey    := FieldByName(FKeyField).AsString;
        lTermListItem.ShortName  := ResStr_NewItem;

        lTermListItem.LongName   := '';
        lTermListItem.Description:= '';
        if iMode = emAddChild then
          lNode := tvTermList.Items.AddChildObject(tvTermList.Selected, lTermListItem.ShortName, lTermListItem)
        else
          lNode := tvTermList.Items.AddObject(tvTermList.Selected, lTermListItem.ShortName, lTermListItem);

        lNode.Selected := true;
        CurrTermListItem := lTermListItem;
        SetupAddInfo;
        PopulateDetails(false); // Populate the details, DB fields already included
        if FileExists(ExtractFilePath(Application.ExeName) + 'DEFAULT.BMP') then
          imgSample.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'DEFAULT.BMP'); //Use default bitmap
        eShortName.SelectAll;
      end else
      // Editing current node
      if iMode = emEdit then begin
        CurrTermListItem := TTermListNode(tvTermList.Selected.Data);
        if Locate(FKeyField, CurrTermListItem.ItemKey, []) then
        begin
          Edit;
          dmGeneralData.SetNameIDAndDate(FdmTermLists.qryTermList, 'Changed_By', 'Changed_Date');
        end else
          raise ERecordAccess.Create(ResStr_CannotLocateRecord);
        // Something a bit different for Sample Type
        if (FCurrTermList.Description = 'Sample Type') and
           ((CurrTermListItem.SysSupplied) or
            (FieldByName('Custodian').AsString <> AppSettings.SiteID))  then
        begin
          eShortName.Readonly      := true;
          eLongName.Readonly       := true;
          dbreDescription.Readonly := true;
          bbOpen.Enabled           := false;
          bbDefault.Enabled        := false;
          cmbAssocRecordCard.SetFocus;
        end else
        if (FCurrTermList.Description = 'Measurement Type') and
           ((CurrTermListItem.SysSupplied) or
            (FieldByName('Custodian').AsString <> AppSettings.SiteID)) then begin
          eShortName.Readonly      := true;
          eLongName.Readonly       := true;
          dbreDescription.Readonly := true;
        end;
      end; //add or edit?
    end; //with FdmTermLists.qryTermList
    FLockNodeUpdate := false;
  end; //mode check
  FMode := iMode;
end;  // SwitchMode

//==============================================================================
procedure TfrmTermLists.bbCancelClick(Sender: TObject);
var lWasAdding: Boolean;
begin
  inherited;
  lWasAdding := (FdmTermLists.qryTermList.State = dsInsert);
  FdmTermLists.qryTermList.Cancel;
  if lWasAdding then 
  begin
    if Assigned(CurrTermListItem) then CurrTermListItem.Free;
    tvTermList.Selected.Delete;
    if Assigned(tvTermList.Selected) and Assigned(tvTermList.Selected.Data) then
      CurrTermListItem := TTermListNode(tvTermList.Selected.Data)
    else
      CurrTermListItem := nil;
  end;
  SwitchMode(emNone);
end;  // bbCancelClick

//==============================================================================
procedure TfrmTermLists.bbSaveClick(Sender: TObject);
var lTermListItem: TTermListNode;
    lParentNode  : TTreeNode;
    i            : Integer;
    lCustodian   : String;
    lTermListDesc: String;
  //----------------------------------------------------------------------------
  function CheckAddField(AField:TDBEdit): Boolean;
  begin
    Result := true;
    // No need to test anything if the field is not visible
    if AField.Visible then
      if dmDatabase.IsFieldRequired(FTableName, AField.Field) then
        Result := AField.Text <> '';
  end;  // CheckAddField
  //----------------------------------------------------------------------------
begin

  if FCurrTermList.Table = 'LICENCE'then
  begin
    ValidateValue(eLongName.Text <> '', ResStr_LongNameRequired, eLongName);
  end;
  ValidateValue(eShortName.Text <> '', ResStr_ShortNameRequired, eShortName);
  ValidateValue(CheckAddField(dbeAddField1), lblTextField1.Caption + ResStr_ValueRequired, dbeAddField1);
  // Some extra conditions for DATA_TYPE field
  if SameText(dbeAddField1.DataField, 'Data_Type') then
    dbeAddField1.Text := cmbDataTypes.Text[1] // Get the first character from the selected item
  else
  if SameText(dbeAddField1.DataField, 'Validation_Competency') then
    CheckValidationCompetency;

  ValidateValue(CheckAddField(dbeAddField2), lblTextField2.Caption + ResStr_ValueRequired, dbeAddField2);
  ValidateValue(CheckAddField(dbeAddField3), lblTextField3.Caption + ResStr_ValueRequired, dbeAddField3);

  lTermListItem := TTermListNode(tvTermList.Selected.Data);

  lCustodian := CurrentCustodian(TKeyDataSysSupplied(tvTermList.Selected.Data).ItemKey);
  if (FMode in [emAddChild, emAddSibling]) or
     ((FMode = emEdit) and
      ((lCustodian = AppSettings.SiteID) or cmbAssocRecordCard.Visible)) then
  begin
    lTermListItem.ShortName   := eShortName.Text;
    lTermListItem.LongName    := eLongName.Text;
    lTermListItem.Description := dbreDescription.Text;
    tvTermList.Selected.Text  := eShortName.Text;
    with FdmTermLists.qryTermList do begin
      FieldByName('Short_Name').AsString := eShortName.Text;
      FieldByName('Long_Name').AsString  := eLongName.Text;
      FieldByName('System_Supplied_Data').AsBoolean  := False;

      lParentNode := tvTermList.Selected.Parent;
      //We have ourselves a child (ie. linked table)
      if lParentNode <> nil then begin
        // Find top node
        if lParentNode.Data = nil then lParentNode := lParentNode.Parent;
        //Set the linked tables' parent key.
        FieldByName(FCurrTermList.KeyField).AsString := TTermListNode(lParentNode.Data).ItemKey;
      end;

      //Image stuff if required.
      if imgSample.Visible then
       SaveImage;

      //Record card stuff, may need to change.
      with cmbAssocRecordCard do
        if Visible and (ItemIndex > -1) then
          if ItemIndex = 0 then
            Fieldbyname('Recording_Card').Value := Null
          else
            FieldByName('Recording_Card').AsString := TRecordingCard(Items.Objects[ItemIndex]).FileName;

      // Hide attribute (where relevant)
      if chkHide.Visible then
        FieldByName('Hide').AsBoolean := chkHide.Checked;

      Post;

      // CheckListBox visible if Measurement_Type term list
      if chklbScreens.Visible then
        FdmTermLists.UpdateContexts(lTermListItem.ItemKey, chklbScreens);

      if rgDeterminationVerification.Visible then
        FdmTermLists.UpdateDetType(lTermListItem.ItemKey, rgDeterminationVerification.ItemIndex);

      if sgUnitValues.Visible then begin
        // Ensure the last change is in the grid
        with sgUnitValues do begin
          Options := Options - [goEditing];
          FdmTermLists.DeleteUnitValues(lTermListItem.ItemKey);
          for i := FixedRows to RowCount - 1 do
            if Cells[0, i] <> '' then
              FdmTermLists.InsertUnitValue(lTermListItem.ItemKey, Cells[0, i]);
        end;
      end;
    end;
  end else
  if (AppSettings.UserAccessLevel >= ualFullUser) and chkHide.Visible then begin
    FdmTermLists.qryTermList.FieldByName('Hide').AsBoolean := chkHide.Checked;
    FdmTermLists.qryTermList.Post;
  end else
  if (FMode = emEdit) and (lCustodian <> FSelectedItemCustodian) then begin
    FdmTermLists.qryTermList.Cancel;
    eShortName.Text := lTermListItem.ShortName;
    eLongName.Text  := lTermListItem.LongName;
    // Work out what list is being edited to display proper name in message
    with tvTermList.Selected, FCurrTermList do begin
      Text := lTermListItem.ShortName;
      // Root node, main termlist
      if Parent = nil then lTermListDesc := Description
      // Child node, only one linked table
      else if LinkedTablesCount = 1 then lTermListDesc := LinkedTables[0].Description
      // Chile node, more than one linked table
      else lTermListDesc := LinkedTables[Parent.Index].Description;
    end;
    MessageDlg(Format(ResStr_CustodyChanged, [lTermListDesc]), mtWarning, [mbOk], 0);
  end;

  if FCurrTermList.Description = 'Sample Type' then
    dmFormActions.UpdateSampleTypeList;

  // Need to add middle layer of nodes for multiple linked tables
  if (FMode <> emEdit) and (FCurrTermList.LinkedTablesCount>1) and (tvTermList.Selected.Parent=nil) then
    for i := 0 to FCurrTermList.LinkedTablesCount-1 do
      tvTermList.Items.AddChild(tvTermList.Selected, FCurrTermList.LinkedTables[i].Description);

  // Send a refresh message to all forms, so that the list boxes and combo boxes are
  // kept in synch with the current state of the term lists
  frmMain.BroadcastMessage(WM_REFRESH_TERM_LISTS);
  SwitchMode(emNone);
end;  // bbSaveClick

//==============================================================================
procedure TfrmTermLists.bbAddClick(Sender: TObject);
var PosPopup : TPoint;
begin
  inherited;
  if FCurrTermList.LinkedTablesCount>0 then
  begin
    PosPopup := ClientToScreen(Point(bbAdd.Left, pnlButtons.Top + bbAdd.Top + bbAdd.Height));
    pmAdd.Popup(PosPopup.X, PosPopup.Y);
  end else
    SwitchMode(emAddSibling);
end;  // bbAddClick

//==============================================================================
procedure TfrmTermLists.actEditExecute(Sender: TObject);
begin
  inherited;
  if Assigned(tvTermList.Selected) then begin
    tvTermListChange(Sender, tvTermList.Selected);
    if actEdit.Enabled then begin
      FSelectedItemCustodian := CurrentCustodian(TKeyDataSysSupplied(tvTermList.Selected.Data).ItemKey);
      SwitchMode(emEdit);
    end else
      MessageDlg(ResStr_CustodyChanged, mtInformation, [mbOk], 0);
  end else
    FSelectedItemCustodian := '';
end;  // actEditExecute

//==============================================================================
procedure TfrmTermLists.actDeleteExecute(Sender: TObject);
var lCount    :integer;
    lShortName:string;
  //----------------------------------------------------------------------------
  function NodeHasChildren: Boolean;
  var loNode: TTreeNode;
  begin
    Result := false;
    loNode := tvTermList.Selected;

    if loNode.HasChildren then begin
      loNode := loNode.GetFirstChild;
      // If data node, return true
      if loNode.Data <> nil then
        Result := true
      else
      // It's a middle layer. So check further down the tree
        while loNode <> nil do begin
          if loNode.Count>0 then Result := true;
          loNode := loNode.GetNextSibling;
        end;
    end;
  end;
  //----------------------------------------------------------------------------
begin
  inherited;
  if Assigned(tvTermList.Selected) then
  begin
    // Check if there is any DATA node below current one
    if NodeHasChildren then
      MessageDlg(ResStr_CannotDeleteParent, mtInformation,[mbOk],0)
    else begin
      with FdmTermLists.qryTermList do
      begin
        if Active then Close;
        if tvTermList.Selected.Parent <> nil then begin
          if FCurrTermList.LinkedTablesCount = 1 then
            SetTermListTable(FCurrTermList.LinkedTables[0].Table, FCurrTermList.LinkedTables[0].KeyField)
          else
            SetTermListTable(FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].Table,
                             FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].KeyField);
        end else
          SetTermListTable(FCurrTermList.Table, FCurrTermList.KeyField);

        Open;
        SetupAdditionalFieldDisplay;
        if Locate(FKeyField, CurrTermListItem.ItemKey, []) then
        begin
          // Build string for message
          lShortName:='';
          for lCount:=1 to Length(CurrTermListItem.ShortName) do begin
            lShortName:=lShortName+CurrTermListItem.ShortName[lCount];
            if CurrTermListItem.ShortName[lCount]='&' then lShortName:=lShortName+'&';
          end;
          // Confirm deletion
          if MessageDlg(Format(ResStr_DeleteTermList, [lShortName]),
                        mtConfirmation, [mbYes,mbNo],0) = mrYes then
          begin
            dmDatabase.Connection.BeginTrans;
            try
              // Measurement, special treatment for contexts (screens).
              if SameText(FTableName, TN_MEASUREMENT_TYPE) then
                FdmTermLists.DeleteContexts(CurrTermListItem.ItemKey);
              if SameText(FTableName, TN_MEASUREMENT_UNIT) then
                FdmTermLists.DeleteUnitValues(CurrTermListItem.ItemKey);
              Delete;
              dmDatabase.Connection.CommitTrans;
            except
              on E:Exception do begin
                dmDatabase.Connection.RollbackTrans;
                if dmDatabase.CheckError(E, dbeReferentialIntegrity) then
                  Raise TExceptionPath.CreateNonCritical(ResStr_CannotDeleteItemRef, E)
                else
                  Raise;
              end;
            end;

            // Clean up
            CurrTermListItem.Free;
            tvTermList.Selected.Delete;
            if FCurrTermList.Description = 'Sample Type' then //update the sample type list.
              dmFormActions.UpdateSampleTypeList;
            if tvTermList.Items.Count > 0 then
            begin
              tvTermList.Items[0].Selected := true;
              CurrTermListItem := TTermListNode(tvTermList.Selected.Data);
            end else
              CurrTermListItem := nil;
            // Refresh
            PopulateDetails(True);
          end;
        end else
          raise ERecordAccess.Create(ResStr_CannotLocateRecordDelete);
      end; //with
    end; //has children
  end; //is Assigned
  // Send a refresh message to all forms, so that the list boxes and combo boxes are
  // kept in synch with the current state of the term lists
  frmMain.BroadcastMessage(WM_REFRESH_TERM_LISTS);
  SwitchMode(emNone); //Ensure correct buttons / menu items are displayed
end;  // actDeleteExecute

//==============================================================================
procedure TfrmTermLists.SaveImage;
begin
  //Was initially checking for imgSample.PIcture = nil. This doesn't work!
  //Now check the width and height properties are 0.
  if (imgSample.Picture.Width <> 0) and (imgSample.Picture.Height <> 0) then
    FdmTermLists.qryTermList.FieldByName('IMAGE').Assign(imgSample.Picture)
  else
    FdmTermLists.qryTermList.FieldByName('IMAGE').Clear;
end;  // SaveImage


//==============================================================================
procedure TfrmTermLists.PopulateRecordingCards;
var lCount:integer;
    lPath :string;
begin
  {Probably overly complex but should cope with any length pathnames and add just
  the card "name" to the combo, the full path to the object.}

  //First clear the contents.
  with cmbAssocRecordCard.Items do begin
    for lCount:=0 to Count-1 do Objects[lCount].Free;
    Clear;
  end;

  //Now loop through the Recording Cards detected by the App.
  AddRecordingCard(ResStr_None);
  with AppSettings do begin
    lPath:=RecordingCardPath;
    if lPath = '' then lPath := ExtractFilePath(Application.ExeName);
    for lCount := 0 to RecordingCards.Items.Count-1 do
      AddRecordingCard(lPath + RecordingCards.Items[lCount].Caption + CARD_EXTENSION);
  end;
end;  // PopulateRecordingCard

//==============================================================================
procedure TfrmTermLists.actAddSiblingExecute(Sender: TObject);
begin
  inherited;
  SwitchMode(emAddSibling);
end;  // actAddSiblingExecute

//==============================================================================
procedure TfrmTermLists.actAddChildExecute(Sender: TObject);
begin
  inherited;
  SwitchMode(emAddChild);
end;  // actAddChildExecute

//==============================================================================
procedure TfrmTermLists.SetCurrTermListItem(const Value: TTermListNode);
var liColor: TColor;
begin
  FCurrTermListItem := Value;

  if FCurrTermListItem=nil then liColor := clBtnFace
                           else liColor := clWindow;
  eShortName.Color     := liColor;
  eLongName.Color      := liColor;
  dbreDescription.Color:= liColor;
  dbeAddField1.Color   := liColor;
  dbeAddField2.Color   := liColor;
  dbeAddField3.Color   := liColor;
  chklbScreens.Color   := liColor;
  cmbAssocRecordCard.Color := liColor;
end;

//==============================================================================
procedure TfrmTermLists.tvTermListChange(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  // Add sibling only for termlist nodes
  actAddSibling.Enabled := (Node.Data <> nil);
  // Add child for top level and bottom level, not in between
  actAddChild.Enabled := ((FCurrTermList.LinkedTablesCount=1) and (Node.Parent=nil)) or
                         ((FCurrTermList.LinkedTablesCount>1) and (Node.Data=nil));

  if not FLockNodeUpdate then begin
    if Node.Data = nil then
        CurrTermListItem := nil
      else
        CurrTermListItem := TTermListNode(Node.Data);
    SwitchMode(emNone);
  end;
end;  // tvTermListChange

//==============================================================================
procedure TfrmTermLists.reDescriptionEnter(Sender: TObject);
begin
  inherited;
  with dmFormActions do begin
    actCut.Enabled      :=not bbAdd.Enabled;
    actBold.Enabled     :=not bbAdd.Enabled;
    actItalic.Enabled   :=not bbAdd.Enabled;
    actUnderline.Enabled:=not bbAdd.Enabled;
  end;
end;  // reDescriptionEnter

//==============================================================================
procedure TfrmTermLists.reDescriptionExit(Sender: TObject);
begin
  inherited;
  with dmFormActions do begin
    actCut.Enabled      :=false;
    actBold.Enabled     :=false;
    actItalic.Enabled   :=false;
    actUnderline.Enabled:=false;
  end;
end;  // reDescriptionExit

function TfrmTermLists.CurrentCustodian(const AKey: TKeyString): string;
begin
  if Assigned(tvTermList.Selected) and Assigned(tvTermList.Selected.Data) then begin
    if tvTermList.Selected.Level = 0 then
      Result := dmGeneralData.Custodian(FCurrTermList.Table, FCurrTermList.KeyField, AKey)
    else
    if FCurrTermList.LinkedTablesCount = 1 then
      Result := dmGeneralData.Custodian(
          FCurrTermList.LinkedTables[0].Table,
          FCurrTermList.LinkedTables[0].KeyField,
          AKey)
    else
      Result := dmGeneralData.Custodian(
          FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].Table,
          FCurrTermList.LinkedTables[tvTermList.Selected.Parent.Index].KeyField,
          AKey);
  end else
    Result := '';
end;

//==============================================================================
procedure TfrmTermLists.ButtonMenuState(const iState: TButtonMenuState);
var lInEditMode,lSystemSupplied,lCustodian,ltfRestrictedEdit: Boolean;
    lStateData : TKeyDataSysSupplied;
    lHasFullEditAccess: boolean;
begin
  lInEditMode := (iState = bmsEdit) or (iState=bmsAdd); // will be true if in edit mode.
  ltfRestrictedEdit:=SameText(FCurrTermList.Table,'Sample_Type') and
                     (AppSettings.UserAccessLevel in [ualRecorder,ualReadOnly]);

  if iState = bmsAdd then begin
    lStateData := TKeyDataSysSupplied.Create;
    lStateData.SysSupplied := False; // adding record cannot be sys supplied
  end else
    lStateData             := GetStateDataFromNode(tvTermList.Selected);

  if Assigned(lStateData) then begin
    lSystemSupplied := lStateData.SysSupplied;
    lCustodian := EditButtonState(lStateData, CurrentCustodian) or (iState=bmsAdd);
  end else
  begin
    lSystemSupplied := false;
    lCustodian := true;
  end;

  if iState=bmsAdd then
    chkHide.Checked := false; // set default for new records

  // Set ReadOnly property according to current mode
  eShortName.ReadOnly     := ltfRestrictedEdit or (not lCustodian) or lSystemSupplied or (not lInEditMode);
  eLongName.ReadOnly      := eShortName.ReadOnly;
  dbreDescription.ReadOnly:= eShortName.ReadOnly;
  dbeAddField1.ReadOnly   := eShortName.ReadOnly;
  dbeAddField2.ReadOnly   := eShortName.ReadOnly;
  dbeAddField3.ReadOnly   := eShortName.ReadOnly;
  rgDeterminationVerification.Enabled := not eShortName.ReadOnly;
  cmbDataTypes.Enabled    := not eShortName.ReadOnly;
  btnAddValue.Enabled     := not eShortName.ReadOnly;
  btnDelValue.Enabled     := not eShortName.ReadOnly;
  with sgUnitValues do
    if eShortName.ReadOnly then Options := Options - [goEditing]
                           else Options := Options + [goEditing];

  bbOpen.Enabled       := (not eShortName.ReadOnly) and gbAdditionalInfo.Visible;
  bbDefault.Enabled    := bbOpen.Enabled;
  imgSample.Enabled    := lInEditMode;
  // don't need custody or sys supplied check
  cmbAssocRecordCard.Enabled := lInEditMode and gbAdditionalInfo.Visible;
  chklbScreens.Enabled := lInEditMode;
  chkHide.Enabled      := lInEditMode and (AppSettings.UserAccessLevel>=ualFullUser);

  bbSave.Enabled       := lInEditMode;
  bbCancel.Enabled     := lInEditMode;

  bbAdd.Enabled        := (not lInEditMode) and AddButtonState;
  if Assigned(lStateData) then begin
    bbEdit.Enabled       := (not lInEditMode) and
                            (AppSettings.UserAccessLevel > ualReadOnly) and
                            (lCustodian or
                             (SameText(FCurrTermList.Table, TN_MEASUREMENT_TYPE) and
                              (tvTermList.Selected.Level = 0)) or
                             SameText(FCurrTermList.Table, TN_SAMPLE_TYPE)) and
                            (tvTermList.Selected.Data <> nil) or
                            ((SameText(FCurrTermList.Table, TN_DETERMINATION_TYPE) or
                             SameText(FCurrTermList.Table, TN_DETERMINER_ROLE)) and
                             (AppSettings.UserAccessLevel>= ualFullUser));
    bbDelete.Enabled     := not lInEditMode and
                            ((tvTermList.Selected<>nil) and (tvTermList.Selected.Data <> nil)) and
                            DeleteButtonState(lStateData);
    if bbEdit.Enabled or bbDelete.Enabled then begin
      // Make sure Own Data only users can only edit their own
      lHasFullEditAccess := dmGeneralData.HasFullEditAccess(FCurrTermList.Table,
          FCurrTermList.FKeyField, TKeyDataSysSupplied(tvTermList.Selected.Data).ItemKey);
      bbEdit.Enabled := bbEdit.Enabled and lHasFullEditAccess;
      bbDelete.Enabled := bbDelete.Enabled and lHasFullEditAccess;
    end;
  end else
  begin
    bbEdit.Enabled := False;
    bbDelete.Enabled := False;
  end;
  mnuAdd.Enabled       := bbAdd.Enabled;
  mnuEditAdd.Enabled   := bbAdd.Enabled;
  actEdit.Enabled      := bbEdit.Enabled;
  actDelete.Enabled    := bbDelete.Enabled;

  tvTermList.Enabled   := not lInEditMode;
  cmbTermLists.Enabled := not lInEditMode;
end;  // ButtonMenuState

//==============================================================================
procedure TfrmTermLists.eShortNameChange(Sender: TObject);
begin
  inherited;
  with tvTermList do
    if (Selected <> nil) and (Selected.Data<>nil) then Selected.Text := eShortName.Text;
end;  // eShortNameChange

//==============================================================================
procedure TfrmTermLists.RetrieveRecordingCard;
var tblTable:TJnccTable;
    lField  :TField;
begin
  if CurrTermListItem <> nil then
  begin
    tblTable := TJnccTable.Create(nil);
    Try
      dmDatabase.SetDatabaseLocal([tblTable]);
      with tblTable do begin
        TableName:=FCurrTermList.Table;
        Open;
        lField:=FindField('Recording_Card');
        if (lField<>nil) and Locate(FKeyField, CurrTermListItem.ItemKey, []) then
          AddRecordingCard(lField.AsString);
      end;
    finally
      tblTable.Free;
    end;
  end;
end;  // RetrieveRecordingCard

//==============================================================================
procedure TfrmTermLists.AddRecordingCard(const iFilename: string);
var lRecCard : TRecordingCard;
    lCardName: string;
    lIdx     : integer;
begin
  lCardName:=iFileName;
  if (lCardName='') or (not FileExists(iFileName)) then
    lCardName:=ResStr_None;

  //Get filename without path
  lCardName:=ExtractFileName(lCardName);
  //Get filename without extension
  if ExtractFileExt(lCardName)<>'' then
    lCardName:=Copy(lCardName,1,Length(lCardName)-4);

  lIdx:=cmbAssocRecordCard.Items.IndexOf(lCardName);
  if lIdx=-1 then begin
    lRecCard:=TRecordingCard.Create;
    lRecCard.FileName:=iFileName;
    lRecCard.Cardname:=lCardName;
    lIdx:=cmbAssocRecordCard.Items.AddObject(lRecCard.CardName,lRecCard);
  end;
  cmbAssocRecordCard.ItemIndex:=lIdx;
end;  // AddRecordingCard

//==============================================================================
function TfrmTermLists.CheckTermList(ATermList:TTermList):boolean;
var loTopNode, loMidNode: TTreeNode;
    ltfAllClear         : Boolean;
begin
  Result:=true;
  loMidNode:=nil;

  if (ATermList.LinkedTablesCount>0) then begin
    loTopNode:=tvTermList.Items.GetFirstNode;

    ltfAllClear := true;  // Assume everything is OK to start with
    while (loTopNode <> nil) and ltfAllClear do begin
      loMidNode := loTopNode.GetFirstChild;
      if (ATermList.LinkedTablesCount=1) and (loTopNode.Count=0) then
        ltfAllClear := false  // Well, not all is ok then
      else
      if (ATermList.LinkedTablesCount>1) then begin
        while (loMidNode <> nil) and ltfAllClear do begin
          if loMidNode.Count = 0 then
            ltfAllClear := false  // Well, not all is ok then
          else
            loMidNode := loMidNode.GetNextSibling;
        end;
      end;
      // Stay on Top node if sub-tree is missing something
      if ltfAllClear then loTopNode := loTopNode.GetNextSibling;
    end;

    if not ltfAllClear then begin
      Beep;
      if ATermList.LinkedTablesCount > 1 then
        MessageDlg(Format(ResStr_EnterFieldForDesc, [ConvertFieldName(ATermList.LinkedTables[loMidNode.Index].Table),
                   ATermList.Description]), mtInformation, [mbOk], 0)
      else if ATermList.LinkedTablesCount = 1 then
        MessageDlg(Format(ResStr_EnterFieldForDesc, [ConvertFieldName(ATermList.LinkedTables[0].Table), ATermList.Description]),
                   mtInformation, [mbOk], 0);
      tvTermList.Selected := loTopNode;
      tvTermList.SetFocus;
      Result:=false;
    end;
  end;
end;  // CheckTermList

//==============================================================================
procedure TfrmTermLists.cmbTermListsChange(Sender: TObject);
begin
  inherited;
  if CheckTermList(FCurrTermList) then begin
    FCurrTermList  := TTermList(cmbTermLists.Items.Objects[cmbTermLists.ItemIndex]);
    FLockNodeUpdate:= true;
    //These lines are needed, to remove the datafield references from the add. fields.
    AddFields(nil, 0);
    SetImage(false);
    //Populate the treeview with the new lists' items.
    SetTermListTable(FCurrTermList.Table, FCurrTermList.KeyField);
    PopulateItems(nil, FdmTermLists.qryTermList);
    SetupAddInfo;
    PopulateDetails(true);
    FLockNodeUpdate := false;
  end else
    cmbTermLists.ItemIndex:=cmbTermLists.Items.IndexOf(FCurrTermList.Description);
  ButtonMenuState(bmsBrowse);
end;  // cmbTermListsChange


//==============================================================================
procedure TfrmTermLists.dbeAddField1Change(Sender: TObject);
begin
  inherited;
  if SameText(dbeAddField1.DataField,'Validation_Competency') then
    CheckValidationCompetency;
end;  // dbeAddField1Change

//==============================================================================
procedure TfrmTermLists.CheckValidationCompetency;
var lVal,lErr:integer;
    stVal:string;
begin
  stVal:=dbeAddField1.Text;
  if dbeAddField1.Visible and (stVal<>'') then begin
    Val(stVal,lVal,lErr);
    if (lErr<>0) or (lVal>3) or (lVal<0) then
      dbeAddField1.Text:=FdmTermLists.qryTermList.FieldByName(dbeAddField1.DataField).AsString;
    ValidateValue(lErr = 0, Format(ResStr_InvalidIntegerValue, [stVal]), dbeAddField1);
    ValidateValue(lVal <= 9, Format(ResStr_TooBigValue, [stVal]), dbeAddField1);
  end;
end;  // dbeAddField1Change

//==============================================================================
procedure TfrmTermLists.SetContextsList(const istKey: String; const itfShow: boolean);
begin
  FdmTermLists.CheckContexts(istKey, chklbScreens);
  if itfShow then begin
    lblTextField1.Caption := ResStr_Cap_AvailableOn;
    lblTextField1.Left    := chklbScreens.Left - (lblTextField1.Width + 10);
  end;
  chklbScreens.Visible := itfShow;
  lblTextField1.Visible := itfShow;
end;  // SetContextsList 

//==============================================================================
procedure TfrmTermLists.SetDetTypesList(const istKey: String; const itfShow: boolean);
begin
  FdmTermLists.CheckDetType(istKey, rgDeterminationVerification);
  rgDeterminationVerification.Visible := itfShow;
end;  // SetDetTypesList

//==============================================================================
procedure TfrmTermLists.SetTermListTable(const ATableName, AKeyField: String);
begin
  //Re-create qryTermList because if we don't then when it is re-opened it thinks
  //it should have the same fields as before and causes an error.
  //Perhaps there is a better way than this.
  with FdmTermLists.qryTermList do begin
    if Active then Close;
    ParseSQL := false;
    SQL.Text   := 'SELECT * FROM "' + ATableName + '"';
    FDmTermLists.dsTermList.DataSet := FdmTermLists.qryTermList;
    FTableName := ATableName;
    FKeyField  := AKeyField;
  end;
  dbeAddField1.DataField := '';
  dbeAddField2.DataField := '';
  dbeAddField3.DataField := '';
end;  // SetTermListTable

//==============================================================================
procedure TfrmTermLists.SetUpDestinationControls;
begin
  RegisterDragComponent(tvTermList, GetTermListNodeData) ;
end;

//==============================================================================
{Function called when a drag is attempted}
procedure TfrmTermLists.GetTermListNodeData(const Sender: TObject;var oDropSource: TJNCCDropSource);
var
  lParentNode: TTreeNode;
  i:integer;
begin
  if FCurrTermListItem<>nil then
    begin
    lParentNode := tvTermList.Selected.Parent;
    if lParentNode <> nil then
    begin
      // Find top node
      for i :=0 to FCurrTermList.LinkedTablesCount -1 do
      begin
        if FCurrTermList.LinkedTables[i].Description = lParentNode.Text then
        begin
          oDropSource.DropData.SetTable(FCurrTermList.LinkedTables[i].Table);
          break;
        end;
      end;
      //Set the linked tables' parent key.
      oDropSource.DropData.AddItem(FCurrTermListItem.ItemKey,'');
    end
    else
    begin
      oDropSource.DropData.AddItem(FCurrTermListItem.ItemKey, '');
      oDropSource.DropData.SetTable(FCurrTermList.Table);
    end;
  end;
end;  // GetDictionaryNodeData

//==============================================================================
procedure TfrmTermLists.tvTermListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_DELETE then
    if bbDelete.Enabled then actDeleteExecute(bbDelete);
end;

//==============================================================================
procedure TfrmTermLists.ApplySecurity;
begin
  ButtonMenuState(bmsBrowse);
end;

//==============================================================================
// Hides or displays the Hide checkbox (and positions it appropriately)
procedure TfrmTermLists.SetHideCheckBox(const ATableName: string);
begin
  if AnsiSameText(ATableName, TN_DETERMINATION_TYPE) then begin
    chkHide.Visible := True;
    chkHide.Top := rgDeterminationVerification.Top + rgDeterminationVerification.Height + 8;
    chkHide.Left := rgDeterminationVerification.Left;
  end else
  if AnsiSameText(ATableName, TN_DETERMINER_ROLE) then begin
    chkHide.Visible := True;
    chkHide.Top := dbeAddField1.Top + dbeAddField1.Height + 8;
    chkHide.Left := dbeAddField1.Left;
  end else begin
    chkHide.Visible := False;
  end;
end;    // TfrmTermLists.SetHideCheckBox

{-------------------------------------------------------------------------------
  Inform the custody checker whether the control is in edit mode
}
function TfrmTermLists.GetCurrentControlEditMode: TEditMode;
begin
  Result := EditMode; // default
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTermLists.sgUnitValuesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  isDup: Boolean;
begin
  inherited;
  with sgUnitValues do begin
    isDup := DuplicateDataValues;
    if (Cells[Col, Row] <> '') and not isDup then begin
      if (Key = VK_DOWN) and (Row = RowCount - 1) then
        btnAddValueClick(Sender);
    end else
    if isDup and (Key = VK_DOWN) then  // SelectCell handles the other combinations.
      ShowInformation(ResStr_ValueAlreadyInList);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTermLists.sgUnitValuesKeyPress(Sender: TObject; var Key: Char);
var
  editor: TInplaceEdit;
begin
  inherited;
  editor := TGridAccessor(sgUnitValues).InplaceEditor;
  if Assigned(editor) then
    if SendMessage(editor.Handle, EM_GETLIMITTEXT, 0, 0) <> MAX_DATA_LENGTH then
      SendMessage(editor.Handle, EM_LIMITTEXT, MAX_DATA_LENGTH, 0);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTermLists.sgUnitValuesSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  inherited;
  CanSelect := not DuplicateDataValues;
  if not CanSelect then
    ShowInformation(ResStr_ValueAlreadyInList);
end;

{-------------------------------------------------------------------------------
}
function TfrmTermLists.DuplicateDataValues: Boolean;
var
  idx: Integer;
begin
  inherited;
  Result := False;
  with sgUnitValues do begin
    if Cells[Col, Row] = '' then Exit;

    idx := Cols[0].IndexOf(Cells[Col, Row]);
    // Just in case someone adds a value the same as the column title, use " >0 ".
    Result := (idx > 0) and (idx <> Row);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTermLists.btnAddValueClick(Sender: TObject);
begin
  inherited;
  with sgUnitValues do begin
    if Cells[0, RowCount - 1] <> '' then
      RowCount := RowCount + 1;
    Rows[RowCount - 1].Clear;
    Row := RowCount - 1;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmTermLists.btnDelValueClick(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  with sgUnitValues do
    if RowCount = FixedRows + 1 then
      Rows[Row].Clear
    else begin
      for i := Row + 1 to RowCount - 1 do
        Rows[i - 1] := Rows[i];
      Rows[RowCount - 1].Clear;
      RowCount := RowCount - 1;
    end;
end;

end.
