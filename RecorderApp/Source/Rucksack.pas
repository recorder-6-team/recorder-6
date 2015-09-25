//==============================================================================
//  Unit:        Rucksack
//
//  Implements:  TfrmRucksack
//
//  Description: Implements a repository of taxa, biotopes, names, locations
//               and references terms.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 94 $
//    $Date: 25/06/09 15:53 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Rucksack;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseExportableUnit, StdCtrls, ComCtrls, ExtCtrls, Grids, Buttons, Menus,
  BaseFormUnit, ExceptionForm, DataClasses, DropSource, DropTarget, ActnList,
  FolderBrowser, OnlineHelp, Constants, BaseChildUnit, GeneralFunctions,
  DatabaseAccessADO, Variants;

type
  // Function type needs to be "of object" becasue the functions used are methods,
  // not regular functions.
  TGetItemText = function (const AKey:TKeyString):string of object;

  ERUKError = class(TExceptionPath);

  TRucksackListItem = class(TObject)
    FileName: String;
  end;

  TfrmRuckSack = class(TBaseExportable)
    pcRuckSack: TPageControl;
    tsTaxa: TTabSheet;
    tsBiotopes: TTabSheet;
    tsLocations: TTabSheet;
    pnlButtons: TPanel;
    shpTaxa: TShape;
    shpBiotopes: TShape;
    lbBiotopes: TListBox;
    lbLocations: TListBox;
    shpLocations: TShape;
    tsPeople: TTabSheet;
    tsDocuments: TTabSheet;
    shpPeople: TShape;
    shpReferences: TShape;
    lbPeople: TListBox;
    lbReferences: TListBox;
    lblRucksack: TLabel;
    cmbRucksacks: TComboBox;
    pmListOptions: TPopupMenu;
    mnuRemove: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditTransferData: TMenuItem;
    N1: TMenuItem;
    mnuEditRemove: TMenuItem;
    dlgFolder: TFolderBrowser;
    mnuEditClearRucksack: TMenuItem;
    ClearRucksack1: TMenuItem;
    alRucksack: TActionList;
    actRemove: TAction;
    actClearRucksack: TAction;
    bbBrowser: TButton;
    sgRucksack: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmbRucksacksChange(Sender: TObject);
    procedure bbBrowseClick(Sender: TObject);
    procedure RuckSackKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure actRemoveExecute(Sender: TObject);
    procedure actClearRucksackExecute(Sender: TObject);
    procedure pcRuckSackChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pcRuckSackEnter(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure sgRucksackMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgRucksackSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sgRucksackSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure sgRucksackDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgRucksackKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgRucksackExit(Sender: TObject);
  private
    { Private declarations }
    FChangesMade:Boolean;
    FLoading    :Boolean;
    FPath       :string;
    FAutoConfirmClear: Boolean;
    FPreviousSearchCode: string;
    FEditingSearchCode: Boolean;
    FSearchCodeRow: integer;
    FDragging: Boolean;
    procedure RefreshList;
    procedure AddRucksack(const FullPath: string);
    procedure LoadRucksack(const AFileName: string);
    procedure SaveSection(Rucksack: TStringList; const SectionName: string;
      KeyList: TEditableKeyList);
    procedure DelItemsInList(AListBox: TListBox; AKeyList: TEditableKeyList);
    procedure DelItemsInGrid(AStringGrid: TStringGrid; AKeyList, ASearchCodeList:
        TEditableKeyList);
    procedure DragTaxon( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DropTaxon(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    procedure DragBiotope( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DropBiotope(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    procedure DragLocation( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DropLocation(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    procedure DragName( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DropName(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    procedure DragReference( const Sender : TObject;
      var oDropSource : TJNCCDropSource);
    procedure DropReference(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList; var ioHandled : boolean);
    function ConfirmSaveRucksack:boolean;
    procedure UpdateMenus;
    function GetRucksackFileName: string;
    procedure DragItem(var oDropList:TJnccDropSource; const ATableName:string;
      AListBox:TListBox; AKeyList:TKeyList; const iNames:boolean=false);
    procedure DragStringGridItem(var oDropList:TJnccDropSource;
      const ATableName:string; AStringGrid:TStringGrid; AKeyList:TKeyList;
      const iNames:boolean=false);
    function GetLocationName(const AKey: TKeyString): String;
    procedure DropItem(const ASourceList:TKeyList; AListBox:TListBox;
      AKeyList:TEditableKeyList; AGetItemText:TGetItemText;
      const iNames:boolean=false);
    procedure DropItemOnStringGrid(const ASourceList:TKeyList;
      AStringGrid:TStringGrid; AKeyList:TEditableKeyList;
      AGetItemText:TGetItemText; const iNames:boolean=false);
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure UpdateList(AList:TListBox; AKeyList:TKeyList; AGetItemText:TGetItemText);
    procedure UpdateStringGrid(AStringGrid:TStringGrid; AKeyList:TKeyList; AGetItemText:TGetItemText);
    procedure CaptureSearchCode(const ASearchCode: string; const ARow: integer);
    procedure HandleDeleteKey(AInplaceEdit: TInplaceEdit);
    function IsDataStringPresent(const ADataString: string;
      AKeyList: TEditableKeyList): Boolean;
  protected
    procedure SetupDestinationControls; override;
    procedure SetFocusToListBox;
    property  RucksackFileName:string read GetRucksackFileName;
  public
    { Public declarations }
    procedure ApplySecurity; override;
    function GetKeyList: TKeyList; override;
    function GetCompleteKeyList: TKeyList;
    function SaveRucksackAs:boolean;
    function SaveRucksack(const AFileName: String):boolean;
    procedure AddTaxonToRuckSack(iTaxonListItemKey : TKeyString);
    procedure AddBiotopeToRuckSack(iBiotopeListItemKey : TKeyString);
    procedure UpdateTaxonList;
    procedure UpdateBiotopeList;
    procedure UpdateLocationList;
    procedure UpdateNameList;
    procedure UpdateReferenceList;
    class procedure SplitNameValuePair(var AName, AValue: string; const S: string);
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, TermLists, RucksackSave, ApplicationSettings,
  GeneralData, Math, Types, BaseLegend, BaseDragFormUnit, SQLConstants, ComObj;

const
  ST_SQL_LOAD_TAXON =
      'SELECT DISTINCT TLI.Taxon_List_Item_Key AS Key, ' +
      '    ITN.Preferred_Name, ITN.Preferred_Name_Italic, ' +
      '    ITN.Common_Name, ITN.Common_Name_Italic, ' +
      '    ITN.Actual_Name AS ItemName ' +
      'FROM Taxon_List_Item TLI ' +
      'INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
      'WHERE TLI.Taxon_List_Item_Key IN (%s)';

  ST_SQL_LOAD_BIOTOPE =
      'SELECT BLI.Biotope_List_Item_Key AS Key ' +
      '    IIF(ISNULL(B.Original_Code), B.Short_Term, B.Original_Code + '', '' + B.Short_Term AS ItemName ' +
      'FROM Biotope_List_Item BLI ' +
      'INNER JOIN Biotope B ON B.Biotope_Key = BLI.Biotope_Key ' +
      'WHERE BLI.Biotope_List_Item_Key IN (%s)';

  ST_SQL_LOAD_LOCATION =
      'SELECT Location_Key AS Key, Item_Name AS ItemName ' +
      'FROM Location ' +
      'WHERE Location_Key IN (%s)';

  ST_SQL_LOAD_PEOPLE =
      'SELECT N.Name_Key AS Key, IIF(N.Organisation = true, ''ORGANISATION'', ''INDIVIDUAL'') AS NameType, ' +
      '    IIF(N.Organisation = true, ' +
      '        IIF(ISNULL(O.Acronym), ' +
      '            O.Full_Name, ' +
      '            O.Acronym + '', '' + O.Full_Name), ' +
      '        IIF(ISNULL(I.Forename), ' +
      '            IIF(ISNULL(I.Initials), ' +
      '                IIF(ISNULL(I.Title), ' +
      '                    I.Surname, ' +
      '                    I.Title + '' '' + I.Surname), ' +
      '                I.Initials + '' '' + I.Surname), ' +
      '            I.Forename + '' '' + I.Surname)) AS ItemName ' +
      'FROM (Name N ' +
      'LEFT JOIN Individual I ON I.Name_Key = N.Name_Key) ' +
      'LEFT JOIN Organisation O ON O.Name_Key = N.Name_Key ' +
      'WHERE N.Name_Key IN (%s)';

  ST_SQL_LOAD_REFERENCE =
      'SELECT R.Source_Key AS Key, R.Title, R.Year_Vague_Date_Start, ' +
      '       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, %s ' +
      'FROM Reference R ' +
      'INNER JOIN %s RA ON RA.Source_Key = R.Source_Key ' +
      'WHERE R.Source_Key IN (%s)';

  TAXON_SECTION_HEADER = 'TAXON';
  BIOTOPE_SECTION_HEADER = 'BIOTOPE';
  LOCATION_SECTION_HEADER = 'LOCATION';
  PEOPLE_SECTION_HEADER = 'PEOPLE';
  REFERENCE_SECTION_HEADER = 'REFERENCE';
  TAXONSEARCHCODE_SECTION_HEADER = 'TAXONSEARCHCODE';
  MAX_SEARCH_CODE_LENGTH = 30;

resourcestring
  ResStr_FileError =  'Unable to complete file operation.  Please ensure that '+
                      'the rucksack has not been deleted';

  ResStr_InvalidSectionHeader = 'Invalid section name.';

  ResStr_Load = 'Failed to load rucksack. The following error occurred:'#13#13'%s';

  ResStr_ClearSackConfirm = 'You are about to clear the rucksack.  Do you wish ' +
                            'to continue?';

  ResStr_ItemsRejected =  'One or more taxa could not be added to the rucksack because the checklist is not installed.';

  ResStr_ItemNotInDB =  'Some items can not be found in the current database.'#13+
                        'Would you like them to be removed automatically now?';

  ResStr_ItemNotFound = 'Item %s could not be found in the current database.';
  ResStr_SaveRucksack = 'Would you like to save the changes made to the rucksack "%s" ?';
  ResStr_SaveChangedRucksack =  'Would you like to save the changes made to the new rucksack?';
  ResStr_BlankRucksack = '<Blank Rucksack>';
  ResStr_TaxonColumnHeader = 'Taxon';
  ResStr_SearchCodeColumnHeader = 'Search Code';
  ResStr_SearchCodeTooLong = 'The search code is restricted to %d characters.';
  ResStr_UniqueSearchCode = 'The search code "%s" is already in use in this rucksack.';
  ResStr_CannotCreateFile = 'You do not have the permissions required to save your rucksack to the '+
      'rucksacks folder.';
  ResStr_FileExists = 'A rucksack with that name already exists. Are you sure you want to overwrite it?';

//==============================================================================
procedure TfrmRuckSack.FormCreate(Sender: TObject);
begin
  inherited;
  if AppSettings.RucksackPath='' then
    FPath:=ExtractFilePath(Application.ExeName)
  else
    FPath:=AppSettings.RucksackPath;
  pcRucksack.ActivePage:=tsTaxa;
  //Refresh rucksack list
  RefreshList;
  actClearRucksack.Execute;
  actClearRucksack.Enabled := False;
  FChangesMade:=false;
  //Set loading mode
  FLoading:= True;
  //Help Setup
  Self.HelpContext   :=IDH_RUCKSACK;
  mnuEdit.HelpContext:=IDH_EDITMENU;
  // Set grid captions
  SetGridColumnTitles(sgRucksack, [ResStr_TaxonColumnHeader, ResStr_SearchCodeColumnHeader]);
  FEditingSearchCode := False;
end;  // FormCreate

//==============================================================================
procedure TfrmRuckSack.FormActivate(Sender: TObject);
var lIndex: Integer;
begin
  inherited;
  //Set toolbars
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self,mnuEdit,0,[]);
  frmMain.mnuFileSave.Enabled:=true;
  frmMain.mnuFileSaveAs.Enabled:=true;
  FormResize(Self);
  if FLoading then
  begin
    //Locate current rucksack in the list and load, if it is in the right
    //directory. Otherwise, leave it as the blank directory.
    if AppSettings.CurrentRucksack.FileName <> '' then
    begin
      lIndex:= cmbRucksacks.Items.IndexOf(RucksackFileName);
      if lIndex <> -1 then
      begin
        cmbRucksacks.ItemIndex:= lIndex;
        LoadRucksack(AppSettings.CurrentRucksack.FileName);
      end;
    end;
    FLoading:= False;
  end;
  UpdateMenus;
end;  // FormActivate

//==============================================================================
procedure TfrmRuckSack.FormDeactivate(Sender: TObject);
begin
  inherited;
  frmMain.mnuFileSave.Enabled:=false;
  frmMain.mnuFileSaveAs.Enabled:=false;
end;  // FormDeactivate

//==============================================================================
procedure TfrmRuckSack.FormResize(Sender: TObject);
var h,w:integer;
begin
  inherited;
  //Resize to shape
  h:=shpTaxa.Height-2;
  w:=shpTaxa.Width-2;
  lbBiotopes.Height  := h;
  lbBiotopes.Width   := w;
  lbLocations.Height := h;
  lbLocations.Width  := w;
  lbPeople.Height    := h;
  lbPeople.Width     := w;
  lbReferences.Height:= h;
  lbReferences.Width := w;
  sgRucksack.Height  := h;
  sgRucksack.Width   := w;
end;  // FormResize

//==============================================================================
procedure TfrmRuckSack.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  //Save current rucksack
  CanClose:=false;
  if FEditingSearchCode then begin
    FEditingSearchCode := False;
    CaptureSearchCode(sgRucksack.Cells[1, FSearchCodeRow], FSearchCodeRow);
  end;    // if FEditingSearchCode

  if FChangesMade then begin
    Beep;
    CanClose:=ConfirmSaveRucksack;
    if CanClose then FChangesMade:=false;
  end else
    CanClose:=true;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmRuckSack.FormClose(Sender: TObject; var Action: TCloseAction);
var lCount: Integer;
begin
  inherited;
  frmMain.mnuFileSave.Enabled:=false;
  frmMain.mnuFileSaveAs.Enabled:=false;
  //Clear existing list items
  with cmbRucksacks.Items do begin
    for lCount:= 0 to Count - 1 do Objects[lCount].Free;
    Clear;
  end;
  //Free Taxon name objects
  with sgRucksack.Cols[0] do begin
    for lCount := 0 to Count-1 do Objects[lCOunt].Free;
    Clear;
  end;

  Action:=caFree;
end;  //FormClose

//==============================================================================
procedure TfrmRuckSack.SetupDestinationControls;
begin
  RegisterDragComponent(sgRucksack, DragTaxon);
  RegisterDropComponent(sgRucksack, DropTaxon, [TN_TAXON_LIST_ITEM], [CF_JNCCDATA]);
  RegisterDragComponent(lbBiotopes, DragBiotope);
  RegisterDropComponent(lbBiotopes, DropBiotope, [TN_BIOTOPE_LIST_ITEM], [CF_JNCCDATA]);
  RegisterDragComponent(lbLocations, DragLocation);
  RegisterDropComponent(lbLocations, DropLocation, [TN_LOCATION], [CF_JNCCDATA]);
  RegisterDragComponent(lbPeople, DragName);
  RegisterDropComponent(lbPeople, DropName, [TN_NAME, TN_INDIVIDUAL, TN_ORGANISATION], [CF_JNCCDATA]);
  RegisterDragComponent(lbReferences, DragReference);
  RegisterDropComponent(lbReferences, DropReference, [TN_REFERENCE], [CF_JNCCDATA]);
end;  // SetupDestinationControls

//==============================================================================
function TfrmRuckSack.GetRucksackFileName: string;
begin
  if AppSettings.CurrentRucksack.FileName='' then
    Result:=''
  else begin
    //Get filename without path
    Result:=ExtractFileName(AppSettings.CurrentRucksack.FileName);
    //Get filename without extension
    Result:=Copy(Result, 1, Length(Result) - 4);
  end;
end;  // GetRucksackFileName

//==============================================================================
procedure TfrmRucksack.UpdateMenus;
var lCurrentListBox:TListBox;
    lPopup:TPopupMenu;
    ltfTaxaSelected: Boolean;
begin
  inherited;
  lCurrentListBox:=nil;
  ltfTaxaSelected := False;
  case pcRucksack.ActivePage.PageIndex of
    0: begin
      if (sgRucksack.RowCount > 1) and (sgRucksack.Selection.Top > 0) then
        ltfTaxaSelected := (Length(sgRucksack.Cells[0, sgRucksack.Row]) > 0)
            and not (goEditing in sgRucksack.Options);
    end;
    1: lCurrentListBox:= lbBiotopes;
    2: lCurrentListBox:= lbLocations;
    3: lCurrentListBox:= lbPeople;
    4: lCurrentListBox:= lbReferences;
  end;
  if (Assigned(lCurrentListBox)) then begin
    actRemove.Enabled:=(lCurrentListBox.SelCount>0) and (AppSettings.UserAccessLevel>=ualRecorder);
    { Hide standard menus if nothing selected }
    dmFormActions.actOccurrencesForPlacesReport.Enabled := lCurrentListBox.SelCount>0;
    dmFormActions.actPlacesForOccurrencesReport.Enabled := lCurrentListBox.SelCount>0;
  end else begin
    actRemove.Enabled := ltfTaxaSelected;
    { Hide standard menus if nothing selected }
    dmFormActions.actOccurrencesForPlacesReport.Enabled := ltfTaxaSelected;
    dmFormActions.actPlacesForOccurrencesReport.Enabled := ltfTaxaSelected;
  end;    // if (Assigned(lCurrentListBox))
  actClearRucksack.Enabled:=((sgRucksack.Cells[0, 1] <> '') or (lbBiotopes.Items.Count>0) or
                            (lbLocations.Items.Count>0) or (lbPeople.Items.Count>0) or
                            (lbReferences.Items.Count>0)) and (AppSettings.UserAccessLevel>=ualRecorder);
  // Enable or disable the popupmenu on the list boxes
  if actRemove.Enabled or actClearRucksack.Enabled then
    lPopup:=pmListOptions
  else
    lPopup:=nil;
  sgRucksack.PopupMenu  :=lPopup;
  lbBiotopes.PopupMenu  :=lPopup;
  lbLocations.PopupMenu :=lPopup;
  lbPeople.PopupMenu    :=lPopup;
  lbReferences.PopupMenu:=lPopup;
end;  // UpdateMenus

//==============================================================================
function TfrmRucksack.GetKeyList: TKeyList;
var
  lNewKeyList: TEditableKeyList;
  lCurrentListBox: TListBox;
  lCurrentStringGrid: TStringGrid;
  lCurrentKeyList: TKeyList;
  i: Integer;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;

  lCurrentlistBox    := nil;
  lCurrentKeyList    := nil;
  lCurrentStringGrid := nil;
  case pcRucksack.ActivePage.PageIndex of
    0: begin  //Taxa
      	 lNewKeyList.SetTable(TN_TAXON_LIST_ITEM);
         lCurrentStringGrid := sgRucksack;
         lCurrentKeyList:= AppSettings.CurrentRucksack.TaxonList;
       end;
    1: begin //Biotopes
         lNewKeyList.SetTable(TN_BIOTOPE_LIST_ITEM);
         lCurrentListBox := lbBiotopes;
         lCurrentKeyList := AppSettings.CurrentRucksack.BiotopeList;
       end;
    2: begin //Locations
         lNewKeyList.SetTable(TN_LOCATION);
         lCurrentListBox := lbLocations;
         lCurrentKeyList := AppSettings.CurrentRucksack.LocationList;
       end;
    3: begin //People
         lNewKeyList.SetTable(TN_NAME);
         lCurrentListBox := lbPeople;
         lCurrentKeyList := AppSettings.CurrentRucksack.NameList;
       end;
    4: begin //References
         lNewKeyList.SetTable(TN_REFERENCE);
         lCurrentListBox := lbReferences;
         lCurrentKeyList := AppSettings.CurrentRucksack.DocumentList;
       end;
  end;

  //Add selected items to keylist
  if Assigned(lCurrentListBox) then begin
    for i:= 0 to lCurrentListBox.Items.Count - 1 do
      if lCurrentListBox.Selected[i] then
        lNewKeyList.AddItem(lCurrentKeyList.Items[i].KeyField1, lCurrentKeyList.Items[i].KeyField2);
  end else
  if Assigned(lCurrentStringGrid) then begin
    for i := lCurrentStringGrid.Selection.Top to lCurrentStringGrid.Selection.Bottom do
      lNewKeyList.AddItem(lCurrentKeyList.Items[i - 1].KeyField1, lCurrentKeyList.Items[i - 1].KeyField2);
  end;

  //If we have no selection then return nil keylist
  if lNewKeyList.Header.ItemCount = 0 then
  begin
    lNewKeyList.Free;
    Result:= nil;
  end else
    Result:= lNewKeyList;
end;  // GetKeyList


//==============================================================================
{ Version of GetKeyList specially used during export - this time we include
    everything on every page, not just selected items on the current page }
function TfrmRucksack.GetCompleteKeyList: TKeyList;
var
  lNewKeyList: TEditableKeyList;

    procedure ReadKeyListIntoOutput( iCurrentKeyList : TKeyList; const iTable : string );
    var
      i: Integer;
    begin
      //Add selected items to keylist
      for i:= 0 to iCurrentKeyList.Header.ItemCount - 1 do
        lNewKeyList.AddItem(iCurrentKeyList.Items[i].KeyField1, iTable);
    end;

begin
  //Return an editable key list
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.ConvertToMixedData;
  { The taxa and biotopes are not required because they are system suuplied.
    Here's the code anyway incase things change }
//  ReadKeyListIntoOutput(AppSettings.CurrentRucksack.TaxonList, 'TAXON_LIST_ITEM');
//  ReadKeyListIntoOutput(AppSettings.CurrentRucksack.BiotopeList, 'BIOTOPE_LIST_ITEM');
  ReadKeyListIntoOutput(AppSettings.CurrentRucksack.LocationList, TN_LOCATION);
  ReadKeyListIntoOutput(AppSettings.CurrentRucksack.NameList, TN_NAME);
  ReadKeyListIntoOutput(AppSettings.CurrentRucksack.DocumentList, TN_REFERENCE);

  //If we have no selection then return nil keylist
  if lNewKeyList.Header.ItemCount = 0 then
  begin
    lNewKeyList.Free;
    Result:= nil;
  end else
    Result:= lNewKeyList;
end;  // GetKeyList


//==============================================================================
procedure TfrmRuckSack.RefreshList;
var lCount: Integer;
    lSearchRec: TSearchRec;
begin
  //Clear existing list items
  with cmbRucksacks.Items do begin
    for lCount:= 0 to Count - 1 do Objects[lCount].Free;
    Clear;
  end;
  cmbRucksacks.Clear;
  //Find the first file
  if FindFirst(FPath+'*.ruk', 0, lSearchRec) = 0 then
  begin
    //Add the file to the menu
    AddRucksack(FPath+lSearchRec.Name);
    //Add a new menu item for each remaining file
    while FindNext(lSearchRec) = 0 do
      AddRucksack(FPath+lSearchRec.Name);
    //Free the search results
    FindClose(lSearchRec);
  end;
  // Add default item at the top of the list
  cmbRucksacks.Items.Add(ResStr_BlankRucksack);
  cmbRucksacks.ItemIndex:=0;
  FChangesMade:=false;
end;  // RefreshList

//==============================================================================
procedure TfrmRuckSack.AddRucksack(const FullPath: string);
var lRucksackListItem: TRucksackListItem;
    lFileName: String;
begin
  //Create object to hold full name
  lRucksackListItem:= TRucksackListItem.Create;
  lRucksackListItem.FileName:= FullPath;

  //Get filename without path
  lFileName:= ExtractFileName(FullPath);

  //Get filename without extension
  lFileName:= Copy(lFileName, 1, Length(lFileName) - 4);

  //Add filename without extention and path to combo
  cmbRucksacks.Items.AddObject(lFileName,lRucksackListItem);
end;  // AddRucksack

//==============================================================================
procedure TfrmRuckSack.cmbRucksacksChange(Sender: TObject);
var
  lItem: TObject;
begin
  inherited;
  // Don't load new rucksack if save was cancelled
  if FChangesMade and not ConfirmSaveRucksack then
    cmbRucksacks.ItemIndex := cmbRucksacks.Items.IndexOf(RucksackFileName)

  else begin
    FAutoConfirmClear := True;
    try
      actClearRucksack.Execute;
    finally
      FAutoConfirmClear := False;
      SetGridColumnTitles(sgRucksack, [ResStr_TaxonColumnHeader, ResStr_SearchCodeColumnHeader]);
    end;
    if cmbRucksacks.ItemIndex = 0 then begin
      // Reset rucksack to empty state
      AppSettings.CurrentRucksack.FileName:='';
      pcRucksack.ActivePage:=tsTaxa;
      FChangesMade:=false;
    end
    else if cmbRucksacks.ItemIndex > 0 then begin
      try
        lItem := cmbRucksacks.Items.Objects[cmbRucksacks.ItemIndex];
        LoadRucksack((lItem as TRucksackListItem).FileName);
      except
        on E: Exception do begin
          cmbRucksacks.ItemIndex := cmbRucksacks.Items.IndexOf(RucksackFileName);
          if E is ERUKError then
            raise
          else
            MessageDlg(Format(ResStr_Load, [E.Message]), mtWarning, [mbOk], 0);
        end;
      end;
    end;
  end;
  UpdateMenus;
end;  // cmbRucksacksChange

//==============================================================================
procedure TfrmRuckSack.bbBrowseClick(Sender: TObject);
var stFolder:string;
    lChanges:boolean;
begin
  inherited;
  // Remember the Change state if Rucksack is BLANK, so it can be set back after RefreshList
  lChanges:=(cmbRucksacks.Text=ResStr_BlankRucksack) and FChangesMade;

  if ((cmbRucksacks.Text<>ResStr_BlankRucksack) and FChangesMade and ConfirmSaveRucksack) or
     not FChangesMade or lChanges then begin
    // Change folder
    stFolder:=FPath;
    if Length(stFolder) > 0 then
      if stFolder[Length(stFolder)]='\' then stFolder:=Copy(stFolder,1,Length(stFolder)-1);
    dlgFolder.Folder:=stFolder;
    if dlgFolder.Execute then begin
      FPath:=dlgFolder.Folder+'\';
      // Update list of rucksacks
      RefreshList;
      // Rucksack wasn't Blank and has been saved. So clear for new Blank rucksack
      if not lChanges then actClearRucksack.Execute;
      // Set flag to proper value.
      FChangesMade:=lChanges;
    end;
  end;
end;  // bbBrowseClick

//==============================================================================
procedure TfrmRuckSack.pcRuckSackChange(Sender: TObject);
begin
  inherited;
  FormResize(Self);
  // If we're in read-only mode due to a batch update, the listboxes will be disabled
  if AppSettings.UserAccessLevel >= ualRecorder then
    SetFocusToListBox;
  UpdateMenus;
end;  // pcRucksackChange

//==============================================================================
procedure TfrmRuckSack.SetFocusToListBox;
begin
  case pcRucksack.ActivePage.PageIndex of
    0: sgRucksack.SetFocus;
    1: lbBiotopes.SetFocus;
    2: lbLocations.SetFocus;
    3: lbPeople.SetFocus;
    4: lbReferences.SetFocus;
  end;
end;  // SetFocusToListBox

//==============================================================================
procedure TfrmRuckSack.RuckSackKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  //Process delete key
  if (Key=VK_DELETE) and actRemove.Enabled then
    actRemove.Execute;
end;  // RucksackKeyDown

//==============================================================================
procedure TfrmRuckSack.actRemoveExecute(Sender: TObject);
begin
  inherited;
  with AppSettings.CurrentRucksack do
    case pcRucksack.ActivePage.PageIndex of
      0: DelItemsInGrid(sgRucksack, TaxonList, TaxonSearchCodeList);
      1: DelItemsInList(lbBiotopes, BiotopeList);
      2: DelItemsInList(lbLocations, LocationList);
      3: DelItemsInList(lbPeople, NameList);
      4: DelItemsInList(lbReferences, DocumentList);
    end;
  UpdateMenus;
end;  // actRemoveExecute

//==============================================================================
procedure TfrmRuckSack.actClearRucksackExecute(Sender: TObject);
  //----------------------------------------------------------------------------
  procedure ClearOneList(AList:TStrings; AKeyList:TEditableKeyList);
  var lIdx:integer;
  begin
    with AList do begin
      for lIdx:=0 to Count-1 do Objects[lIdx].Free;
      Clear;
    end;
    AKeyList.Clear;
  end;  // ClearOneList
  //----------------------------------------------------------------------------
begin
  inherited;
  { Must ask confirmation if anything to clear }
  if (not FAutoConfirmClear) and (cmbRucksacks.Text<>ResStr_BlankRucksack) then
    if MessageDlg(ResStr_ClearSackConfirm, mtConfirmation, [mbYes, mbNo], 0)=mrNo then
      Exit; // from procedure
  with AppSettings.CurrentRucksack do begin
    ClearOneList(sgRucksack.Cols[0], TaxonList);
    ClearOneList(lbBiotopes.Items, BiotopeList);
    ClearOneList(lbLocations.Items, LocationList);
    ClearOneList(lbPeople.Items, NameList);
    ClearOneList(lbReferences.Items, DocumentList);
    ClearOneList(sgRucksack.Cols[1], TaxonSearchCodeList);
  end;

  sgRucksack.RowCount := 2;
  sgRucksack.Row := 1;

  FChangesMade:=true;
  if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
    AppSettings.LastSearchedRucksack.IsDirty := True;
  UpdateMenus;
end;  // actClearRucksackExecute

//==============================================================================
procedure TfrmRucksack.DelItemsInList(AListBox: TListBox; AKeyList: TEditableKeyList);
var lIdx:integer;
begin
  //Delete the currently selected item(s)
  with AListBox do
  begin
    for lIdx:= Items.Count-1 downto 0 do
      if Selected[lIdx] then
      begin
        //Delete string from listbox
      	Items.Delete(lIdx);
        //Delete key from keylist
        AKeyList.DeleteItem(lIdx);
      end;
    FChangesMade:= True;
    if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
      AppSettings.LastSearchedRucksack.IsDirty := True;
  end;
end;  // DelItemsInTermList

{-------------------------------------------------------------------------------
  Remove the selected item from the Taxon grid, including cleaning up the
     TaxonList and TaxonSearchCodeList items associated with each row.
}
procedure TfrmRuckSack.DelItemsInGrid(AStringGrid: TStringGrid; AKeyList,
    ASearchCodeList: TEditableKeyList);
var lIdx, lCodeIdx:integer;

begin
  // Delete the currently selected rows, along with their associated objects.
  if (AStringGrid.Row > 0) then begin
    for lIdx := AStringGrid.Selection.Bottom downto AStringGrid.Selection.Top do
    begin
      // check if we have a search code
      lCodeIdx := ASearchCodeList.IndexOf(AKeyList.Items[Pred(lIdx)].KeyField1, AStringGrid.Cells[1, lIdx]);
      if lCodeIdx<>-1 then
        // we have a search code, so clean it up
        ASearchCodeList.DeleteItem(lCodeIdx);
      //Delete taxon from grid
      AStringGrid.Objects[0, lIdx].Free;
      AStringGrid.Objects[0, lIdx] := nil;
      AStringGrid.Row := lIdx;
      DelLineInGrid(AStringGrid);
      //Delete key from keylist
      AKeyList.DeleteItem(Pred(lIdx));
    end;    // for
    FChangesMade:= True;
    if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
      AppSettings.LastSearchedRucksack.IsDirty := True;
  end;    // if (AStringGrid.Row > 0)

  // The object for the row beyond the last should no longer be assigned
  //  since it is now the same object as the previous row.
  AStringGrid.Objects[0, AStringGrid.RowCount] := nil;
end;

//==============================================================================
procedure TfrmRuckSack.LoadRucksack(const AFileName: string);
var lRuckFile:TextFile;
    lCursor  :TCursor;
    ltfKeepNotFound  :boolean;

  //----------------------------------------------------------------------------
  procedure LoadSection(ASectionName:string; AStringList:TStrings;
    AKeyList:TEditableKeyList; AGetItemText:TGetItemText);
  var lCurrent       : TKeyString;
      lTaxonName     : TTaxonNames;
      lOrderId       : Integer;
      lItemName, table, keyColumn: string;
  begin
    lOrderId:= 0;
    //Read list items from rucksack.
    // lCurrent is 16 long, enough to accomodate section headers and footers
    Readln(lRuckFile, lCurrent);
    if lCurrent <> '<' + ASectionName + '>' then
      Raise ERUKError.CreateNonCritical(Format(ResStr_BadRucksack, [ExtractWithoutExt(AFileName)]));
    AStringList.Clear; // Clear the lists of previous items
    AKeyList.Clear;
    // Replace the taxon column header at the top of the column
    if(ASectionName = TAXON_SECTION_HEADER) then  begin
      AStringList.Add(ResStr_TaxonColumnHeader); // display the header in grid
      dmDatabase.ExecuteSQL(SQL_TEMPLIST_CREATE_ORDERED);  // create temp table in DB
      ReadLn(lRuckFile, lCurrent);
      try
        while lCurrent <> '</' + ASectionName + '>' do begin
          // insert ICurrent(Taxon_itmw_key) into Temp table
          try
            dmDatabase.ExecuteSQL(Format(SQL_TEMPLIST_INSERT_ORDERED, [lCurrent,lOrderId]));
          except on EOleException do
            // ignore, duplicate key errors most likely which just means the file has been edited wrongly
          end;
          lOrderId := lOrderId +1;
          ReadLn(lRuckFile, lCurrent); // read next item from rucksack file
        end;

        with dmDatabase.GetRecordset('[usp_Rucksack_LoadTaxa]', []) do begin
          if RecordCount > 0 then
            sgRucksack.RowCount := RecordCount + 1
          else
            sgRucksack.RowCount :=  2;

          while not EOF do
          begin
            //built TaxaNamesobject to read all data out
            lTaxonName := TTaxonNames.Create;
            lTaxonName.TaxonListItemKey := Fields['ItemKey'].Value;        //taxon list irem key
            lTaxonName.TaxonName        := VarToStr(Fields['Preferred_Name'].Value); // user prefered name
            lTaxonName.CommonName       := VarToStr(Fields['Common_Name'].Value);    // user common name
            lTaxonName.EnteredName      := VarToStr(Fields['Actual_Name'].Value);    // user entered name
            lTaxonName.TNItalic         := Fields['Preferred_Name_Italic'].Value;
            lTaxonName.CNItalic         := Fields['Common_Name_Italic'].Value;
            lTaxonName.ENItalic         := Fields['Actual_Name_Italic'].Value;
            lTaxonName.TNAttribute      := VarToStr(Fields['Preferred_Name_Attribute'].Value);
            lTaxonName.CNAttribute      := VarToStr(Fields['Common_Name_Attribute'].Value);
            lTaxonName.ENAttribute      := VarToStr(Fields['Actual_Name_Attribute'].Value);
            lTaxonName.TNAuthor         := VarToStr(Fields['Preferred_Name_Authority'].Value);
            lTaxonName.ENAuthor         := VarToStr(Fields['Authority'].Value);
            AStringlist.AddObject(VarToStr(Fields['Actual_Name'].Value), lTaxonName);
            AKeyList.AddItem(Fields['ItemKey'].Value, '');
            MoveNext;
          end;
        end;
       finally
         dmDatabase.ExecuteSQL(SQL_TEMPLIST_DROP);
       end;
    end
    else begin
      ReadLn(lRuckFile, lCurrent);
      while lCurrent <> '</' + ASectionName + '>' do
      begin
        if ASectionName = BIOTOPE_SECTION_HEADER then begin
          table := TN_BIOTOPE_LIST_ITEM;
          keyColumn := TN_BIOTOPE_LIST_ITEM + '_KEY';
        end else
        if ASectionName = LOCATION_SECTION_HEADER then begin
          table := TN_LOCATION;
          keyColumn := TN_LOCATION + '_KEY';
        end else
        if ASectionName = PEOPLE_SECTION_HEADER then begin
          table := TN_NAME;
          keyColumn := TN_NAME + '_KEY';
        end else
        if ASectionName = REFERENCE_SECTION_HEADER then begin
          table := TN_REFERENCE;
          keyColumn := 'SOURCE_KEY';
        end else // Not one of the expected headers.
          raise ERUKError.Create(ResStr_InvalidSectionHeader);

        if dmGeneralData.CheckKeyExists(table, keyColumn, lCurrent) then begin
          lItemName:=AGetItemText(lCurrent);
          AStringlist.Add(lItemName);
          if (ASectionName <> PEOPLE_SECTION_HEADER)  then
            // Store the proper flag for Individual and Organisation.
            AKeyList.AddItem(lCurrent, '')
          else if dmGeneralData.GetIndividualName(lCurrent)<>'' then  // It was an individual then
            AKeyList.AddItem(lCurrent, TN_INDIVIDUAL)
          else
            AKeyList.AddItem(lCurrent,TN_ORGANISATION);
        end;

        ReadLn(lRuckFile, lCurrent);
      end;
    end;
  end;  // LoadSection

  //----------------------------------------------------------------------------
  procedure LoadNameValueSection(ASectionName: string;
                                 AStringList, AReferenceList: TStrings;
                                 AKeyList: TEditableKeyList);
  var lName, lValue, lLine: string;
      lReferenceIndex     : integer;
  begin
    Readln(lRuckFile, lLine);
    if (Length(lLine) > 0) then begin
      if lLine <> '<' + ASectionName + '>' then
        Raise ERUKError.CreateNonCritical(Format(ResStr_BadRucksack, [ExtractWithoutExt(AFileName)]));

      // Clear the lists of previous items
      AStringList.Clear;
      AKeyList.Clear;

      // Replace the taxon search code column header at the top of the column
      if (ASectionName = TAXONSEARCHCODE_SECTION_HEADER) then
        AStringList.Add(ResStr_SearchCodeColumnHeader);

      // Read first item in list, or if nothing, the end of section marker
      ReadLn(lRuckFile, lLine);

      while lLine <> '</' + ASectionName + '>' do
      begin
        // Get text for current key
        SplitNameValuePair(lName, lValue, lLine);
        lReferenceIndex := AppSettings.CurrentRucksack.TaxonList.IndexOf(lName,'');
        if (lReferenceIndex >= 0) then begin
          AStringList[lReferenceIndex+1] := lValue;
          AKeyList.AddItem(lName, lValue);
        end;  // if (lReferenceIndex > 0)*)
        ReadLn(lRuckFile, lLine);
      end;  // while               *)
     end;  // if (Length(lLine) > 0)
   end;  // LoadTaxonSearchKeySection

  //----------------------------------------------------------------------------
begin
  lCursor:=HourglassCursor;
  ltfKeepNotFound  :=true;
  try
    //Clear existing data
    with AppSettings.CurrentRucksack do begin
      //Open rucksack file
      AssignFile(lRuckFile, AFileName);
      Reset(lRuckFile);
      //Load sections
      LoadSection(TAXON_SECTION_HEADER,     sgRucksack.Cols[0],TaxonList,    dmGeneralData.GetTaxonName);
      LoadSection(BIOTOPE_SECTION_HEADER,   lbBiotopes.Items,  BiotopeList,  dmGeneralData.GetBiotopeCodeName);
      LoadSection(LOCATION_SECTION_HEADER,  lbLocations.Items, LocationList, GetLocationName);
      LoadSection(PEOPLE_SECTION_HEADER,    lbPeople.Items,    NameList,     dmGeneralData.GetName);
      LoadSection(REFERENCE_SECTION_HEADER, lbReferences.Items,DocumentList, dmGeneralData.GetReferenceText);
      LoadNameValueSection(TAXONSEARCHCODE_SECTION_HEADER, sgRucksack.Cols[1], sgRucksack.Cols[0], TaxonSearchCodeList);
      CloseFile(lRuckFile);
      //Set current rucksack filename
      FileName:= AFileName;
    end;
    // True if missing items removed, false if all ok or kept
    FChangesMade:=not ltfKeepNotFound;
  finally
    DefaultCursor(lCursor);
  end;
end;  // LoadRucksack

//==============================================================================
function TfrmRucksack.ConfirmSaveRucksack:boolean;
var lMsg   :string;
    lAnswer:integer;
begin
  if AppSettings.CurrentRucksack.FileName<>'' then
    lMsg:=Format(ResStr_SaveRucksack, [RucksackFileName])
  else
    lMsg:=ResStr_SaveChangedRucksack;

  //Get confirmation
  lAnswer:=MessageDlg(lMsg, mtConfirmation, [mbYes,mbNo,mbCancel], 0);

  case lAnswer of
    mrYes: Result:=SaveRucksack(AppSettings.CurrentRucksack.FileName);
    mrNo : Result:=true;
  else
    Result:=false;
  end;
end;  // ConfirmSaveRucksack

//==============================================================================
function TfrmRuckSack.SaveRucksackAs:boolean;
var lTemp: string;
begin
  with TdlgRucksackSave.Create(nil) do begin
    eRucksackName.Text:= RucksackFileName;
    if ShowModal = mrOK then
    begin

      lTemp:= eDestFolder.Text + eRucksackName.Text;
      if Pos(UpperCase(lTemp), '.RUK') = 0 then
        lTemp:= lTemp + '.Ruk';
      if FileExists(lTemp) then
        if MessageDlg(ResStr_FileExists, mtConfirmation, mbOKCancel, 0)=mrCancel then begin
          result := false;
          exit;
        end;
      FPath := ExtractFilePath(lTemp);
      Result := SaveRucksack(lTemp);
    end else
      Result:=false;  // Decided to cancel save
    Free;
  end;
end;  // SaveRucksackAs

//==============================================================================
function TfrmRuckSack.SaveRucksack(const AFileName: String):boolean;
var handle: integer;
    lRucksack:TStringList;
    lCursor  :TCursor;
    lRucksackName: string;
begin
  if AFileName='' then
    Result:=SaveRucksackAs
  else begin
    Result := true;
    lCursor:=HourglassCursor;
    lRucksack := TStringlist.Create;
    try
      // Create a new file to test permissions
      handle := FileCreate(AFileName);
      if handle>0 then begin
        FileClose(handle);
        with AppSettings.CurrentRucksack do begin
          SaveSection(lRucksack, TAXON_SECTION_HEADER, TaxonList);
          SaveSection(lRucksack, BIOTOPE_SECTION_HEADER, BiotopeList);
          SaveSection(lRucksack, LOCATION_SECTION_HEADER, LocationList);
          SaveSection(lRucksack, PEOPLE_SECTION_HEADER, NameList);
          SaveSection(lRucksack, REFERENCE_SECTION_HEADER, DocumentList);
          SaveSection(lRucksack, TAXONSEARCHCODE_SECTION_HEADER, TaxonSearchCodeList);
        end;
        lRucksack.SaveToFile(AFileName);
        FChangesMade:=false;
        FPath := ExtractFilePath(AFileName);
        // VI 16181 - Ensure that the newly selected rucksack is displayed after saving
        lRucksackName := ExtractWithoutExt(AFileName);
        RefreshList;
        cmbRucksacks.ItemIndex := cmbRucksacks.Items.IndexOf(lRucksackName);
        AppSettings.CurrentRucksack.FileName := AFileName;
      end else
      begin
        ShowInformation(ResStr_CannotCreateFile);
        Result := false;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // SaveRucksack

//==============================================================================
procedure TfrmRuckSack.SaveSection(Rucksack: TStringList; const SectionName: string;
  KeyList: TEditableKeyList);
var i: Integer;
begin
  //Write section header
  Rucksack.Add('<' + SectionName + '>');

  //Write KeyList
  if (SectionName = TAXONSEARCHCODE_SECTION_HEADER) then
    for i:= 0 to KeyList.Header.ItemCount - 1 do
      Rucksack.Add(Format('%s=%s',
        [KeyList.Items[i].KeyField1, KeyList.Items[i].KeyField2]))
  else
    for i:= 0 to KeyList.Header.ItemCount - 1 do
      Rucksack.Add(KeyList.Items[i].KeyField1);

  //Write section footer
  Rucksack.Add('</' + SectionName + '>');
end;  // SaveSection

//==============================================================================
//==============================================================================
function TfrmRuckSack.GetLocationName(const AKey: TKeyString): String;
var lKey:TKeyString;
begin
  lKey:=AKey;
  // Need a var as GetLocationName expects a var
  Result:=dmGeneralData.GetLocationName(lKey);
end;  // GetLocationName

//==============================================================================
//==============================================================================
procedure TfrmRuckSack.DragItem(var oDropList:TJnccDropSource; const ATableName:string;
  AListBox:TListBox; AKeyList:TKeyList; const iNames:boolean=false);
var lCount   :integer;
    lstField2:string;
begin
  oDropList.DropData.SetTable(ATableName);
  for lCount:= 0 to AListBox.Items.Count - 1 do
	  if AListBox.Selected[lCount] then
      with AKeyList.Items[lCount] do begin
        if iNames then lstField2:=KeyField2
                  else lstField2:='';
  	   	oDropList.DropData.AddItem(KeyField1,lstField2)
      end;
end;  // DragItem

//==============================================================================
//  Version of DragItem for use by TStringGrid controls
procedure TfrmRuckSack.DragStringGridItem(var oDropList: TJnccDropSource;
  const ATableName: string; AStringGrid: TStringGrid; AKeyList: TKeyList;
  const iNames: boolean);
var lCount   :integer;
    lstField2:string;
begin
  oDropList.DropData.SetTable(ATableName);
  for lCount:= AStringGrid.Selection.Top to AStringGrid.Selection.Bottom do
    with AKeyList.Items[lCount - 1] do begin
      if iNames then lstField2:=KeyField2
                else lstField2:='';
      oDropList.DropData.AddItem(KeyField1,lstField2)
    end;
end;

//==============================================================================
procedure TfrmRuckSack.DragTaxon( const Sender : TObject; var oDropSource : TJNCCDropSource);
begin
  if not (goEditing in sgRucksack.Options) then begin
//    sgRucksack.Options := sgRucksack.Options - [goRangeSelect];
    DragStringGridItem(oDropSource, TN_TAXON_LIST_ITEM, sgRucksack, AppSettings.CurrentRucksack.TaxonList);
    // Set the flag that prevents automatic row selection whilst dragging
    FDragging := True;
  end;    // if not (goEditing in sgRucksack.Options)
end;  // DragTaxon

//==============================================================================
procedure TfrmRuckSack.DragBiotope( const Sender : TObject; var oDropSource : TJNCCDropSource);
begin
  DragItem(oDropSource, TN_BIOTOPE_LIST_ITEM, lbBiotopes, AppSettings.CurrentRucksack.BiotopeList);
end;  // DragBiotope

//==============================================================================
procedure TfrmRuckSack.DragLocation( const Sender : TObject; var oDropSource : TJNCCDropSource);
begin
  DragItem(oDropSource, TN_LOCATION, lbLocations, AppSettings.CurrentRucksack.LocationList);
end;  // DragLocation

//==============================================================================
procedure TfrmRuckSack.DragName( const Sender : TObject; var oDropSource : TJNCCDropSource);
begin
  if (lbPeople.Items.Count >0) and (lbPeople.ITemIndex<>-1) then // safeguard
  begin
    if CompareText( Appsettings.CurrentRucksack.NameList.Items[lbPeople.ItemIndex].KeyField2,
                    TN_INDIVIDUAL)=0 then
      DragItem(oDropSource, TN_INDIVIDUAL, lbPeople, AppSettings.CurrentRucksack.NameList, true)
    else
      DragItem(oDropSource, TN_ORGANISATION, lbPeople, AppSettings.CurrentRucksack.NameList, true);
  end;
end;  // DragName

//==============================================================================
procedure TfrmRuckSack.DragReference( const Sender : TObject; var oDropSource : TJNCCDropSource);
//var lCount:integer;
begin
  DragItem(oDropSource, TN_REFERENCE, lbReferences, AppSettings.CurrentRucksack.DocumentList);
end;  // DragReference

//==============================================================================
// No longer handles the Taxa tab, since this is now a string grid
//  See DropItemOnStringGrid method instead.
//==============================================================================
procedure TfrmRuckSack.DropItem(const ASourceList:TKeyList; AListBox:TListBox;
  AKeyList:TEditableKeyList; AGetItemText:TGetItemText; const iNames:boolean=false);
var lCount: integer;
    lstField2: string;
    lDisplayText: string;
    lItemsRejected: boolean;
    ptPosition: TPoint;
    InsertAt: integer;
    ItemsIndex: integer;
begin
  for lCount := AListBox.Items.Count - 1 downto 0 do
    AlistBox.Selected[lCount] := False;
  lItemsRejected := false;
  for lCount:=0 to ASourceList.Header.ItemCount-1 do
    with ASourceList.Items[lCount] do begin
      if iNames then lstField2:=KeyField2
                else lstField2:='';
      lDisplayText := AGetItemText(KeyField1);
      ptPosition.X := Mouse.CursorPos.X;
      ptPosition.Y := Mouse.CursorPos.Y;
      ptPosition := AListBox.ScreenToClient(ptPosition);
      InsertAt := AListBox.ItemAtPos(ptPosition, false);
      if InsertAt = -1 then  // insert at end if insert position not available
        InsertAt := AListBox.Items.Count;
      ItemsIndex := AKeyList.IndexOf(KeyField1,lstField2);
      if ItemsIndex = -1 then
        if lDisplayText<>'' then begin
          AKeyList.Insert(InsertAt, KeyField1,lstField2);
          AListBox.Items.Insert(InsertAt, lDisplayText);
          AListBox.Selected[InsertAt] := True;
          FChangesMade:=true;
        end else
          lItemsRejected := True
      else begin
        if ItemsIndex < InsertAt then Dec(InsertAt);
        AKeyList.Move(ItemsIndex, InsertAt);
        AListBox.Items.Move(ItemsIndex, InsertAt);
        AListBox.Selected[InsertAt] := True;
        FChangesMade := True;
      end;
    end;
  if FChangesMade and AppSettings.LastSearchedRucksack.UseCurrentRucksack then
    AppSettings.LastSearchedRucksack.IsDirty := True;
  if lItemsRejected and (pcRucksack.ActivePage = tsTaxa) then
    MessageDlg(ResStr_ItemsRejected, mtInformation, [mbOk], 0);
end;  // DropItem

//==============================================================================
procedure TfrmRuckSack.DropItemOnStringGrid(const ASourceList: TKeyList;
  AStringGrid: TStringGrid; AKeyList: TEditableKeyList;
  AGetItemText: TGetItemText; const iNames: boolean);
var lCount: integer;
    lstField2: string;
    lDisplayText: string;
    lItemsRejected: boolean;
    ptPosition: TPoint;
    InsertAt, lRow, lCol: integer;
    ItemsIndex: integer;

  procedure MoveRow(const CurrentRowIndex, NewRowIndex: integer);
  var
    i: integer;
    Buffer: TStringList;
  begin
    Buffer := TStringList.Create;
    try
      Buffer.Assign(AStringGrid.Rows[CurrentRowIndex]);
      if (CurrentRowIndex > NewRowIndex) then begin
        for i := CurrentRowIndex downto NewRowIndex + 1 do
          AStringGrid.Rows[i].Assign(AStringGrid.Rows[i - 1]);
      end else
      if (CurrentRowIndex < NewRowIndex) then begin
        for i := CurrentRowIndex to NewRowIndex - 1 do
          AStringGrid.Rows[i].Assign(AStringGrid.Rows[i + 1]);
      end;
      AStringGrid.Rows[NewRowIndex].Assign(Buffer);
    finally
      Buffer.Free;
    end;
  end;
  
begin
  lItemsRejected := false;
  InsertAt := 0;
  for lCount:=0 to ASourceList.Header.ItemCount-1 do
    with ASourceList.Items[lCount] do begin
      if iNames then lstField2:=KeyField2
                else lstField2:='';
      lDisplayText := AGetItemText(KeyField1);
      ptPosition := AStringGrid.ScreenToClient(Mouse.CursorPos);
      AStringGrid.MouseToCell(ptPosition.X, ptPosition.Y, lCol, lRow);
      InsertAt := lRow;
      if InsertAt < 1 then  // insert at end if insert position not available
        InsertAt := AStringGrid.RowCount;
      ItemsIndex := AKeyList.IndexOf(KeyField1,lstField2);
      // Due to the single fixed row in the grid, the row numbers are 1 higher
      //  than the corresponding index into the AKeyList parameter.
      if ItemsIndex = -1 then
        if lDisplayText<>'' then begin
          // If the list is empty, the first item must be at row 1
          if (AKeyList.Header.ItemCount = 0) then InsertAt := 1;
          AKeyList.Insert(InsertAt - 1, KeyField1, lstField2);
          // VI 16856 - Ensure not in editing mode
          //  (so that AddLineInGrid can set Row property to new line)
          if (AStringGrid.EditorMode) then begin
            AStringGrid.EditorMode := False;
            AStringGrid.Options := AStringGrid.Options
                - [goEditing] + [goRowSelect];
          end;    // if (AStringGrid.EditorMode)
          AddLineInGrid(AStringGrid, 0, lDisplayText);
          // clear the search code
          AStringGrid.Cells[1,AStringGrid.RowCount - 1] := '';
          if (AStringGrid.RowCount - 1 <> InsertAt) then
            MoveRow(AStringGrid.RowCount - 1, InsertAt);
          // Add taxon names object for taxon list box
          if (AStringGrid = sgRucksack) then
            AStringGrid.Objects[0, InsertAt] :=
                dmGeneralData.GetTaxonNamesObject(KeyField1);
          FChangesMade:=true;
        end else
          lItemsRejected := True
      else
      if (InsertAt - ItemsIndex <> 1) then begin
        if (ItemsIndex < InsertAt) then Dec(InsertAt);
        AKeyList.Move(ItemsIndex, InsertAt - 1);
        MoveRow(ItemsIndex + 1, InsertAt);
        FChangesMade := True;
      end;
    end;
  if FChangesMade and AppSettings.LastSearchedRucksack.UseCurrentRucksack then
    AppSettings.LastSearchedRucksack.IsDirty := True;
  AStringGrid.Row := InsertAt;
  AStringGrid.Refresh;
  if lItemsRejected and (pcRucksack.ActivePage = tsTaxa) then
    MessageDlg(ResStr_ItemsRejected, mtInformation, [mbOk], 0);
end;

//==============================================================================
procedure TfrmRuckSack.DropTaxon(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if iFormat=CF_JNCCDATA then begin
    FDragging := False;
    DropItemOnStringGrid(iSourceData, sgRucksack,
      AppSettings.CurrentRucksack.TaxonList, dmGeneralData.GetTaxonName, True);
    ioHandled := True;
    // Prevent the mouse selecting cells automatically after drag
    postMessage(sgRucksack.Handle, WM_LBUTTONUP, 0, 0);
  end;    // if iFormat=CF_JNCCDATA
end;  // DropTaxon

//==============================================================================
procedure TfrmRuckSack.DropBiotope(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if iFormat=CF_JNCCDATA then
    DropItem(iSourceData,lbBiotopes,AppSettings.CurrentRucksack.BiotopeList,dmGeneralData.GetBiotopeCodeName);
end;  // DropBiotope

//==============================================================================
procedure TfrmRuckSack.DropLocation(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if iFormat=CF_JNCCDATA then
    DropItem(iSourceData,lbLocations,AppSettings.CurrentRucksack.LocationList,GetLocationName);
end; // DropLocation

//==============================================================================
procedure TfrmRuckSack.DropName(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if iFormat=CF_JNCCDATA then
    DropItem(iSourceData,lbPeople,AppSettings.CurrentRucksack.NameList,dmGeneralData.GetName,true);
end;  // DropName

//==============================================================================
procedure TfrmRuckSack.DropReference(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; var ioHandled: boolean);
begin
  if iFormat=CF_JNCCDATA then
    DropItem(iSourceData,lbReferences,AppSettings.CurrentRucksack.DocumentList,dmGeneralData.GetReferenceText);
end;  // DropReference

//==============================================================================
procedure TfrmRuckSack.AddTaxonToRuckSack(iTaxonListItemKey : TKeyString);
var
  lTaxonName : string;
begin
  if AppSettings.CurrentRucksack.TaxonList.IndexOf(iTaxonListItemKey, '') = -1 then
  begin
    lTaxonName := dmGeneralData.GetTaxonName(iTaxonListItemKey);
    if lTaxonName<>'' then begin // blank means on CD, so can't put in rucksack
      //Add to key list
      AppSettings.CurrentRucksack.TaxonList.AddItem(iTaxonListItemKey, '');
      //Add to string grid
      AddLineInGrid(sgRucksack, 0, lTaxonName);
      sgRucksack.Objects[0, sgRucksack.RowCount - 1] :=
        dmGeneralData.GetTaxonNamesObject(iTaxonListItemKey);
      FChangesMade:= True;
      if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
        AppSettings.LastSearchedRucksack.IsDirty := True;
    end; // if taxon on local disk
  end;
end;  // AddTaxonToRucksack

//==============================================================================
procedure TfrmRuckSack.AddBiotopeToRuckSack(iBiotopeListItemKey : TKeyString);
begin
  if AppSettings.CurrentRucksack.BiotopeList.IndexOf(iBiotopeListItemKey, '') = -1 then
  begin
    //Add to key list
    AppSettings.CurrentRucksack.BiotopeList.AddItem(iBiotopeListItemKey, '');
    //Add to list box
    lbBiotopes.Items.Add(dmGeneralData.GetBiotopeCodeName(iBiotopeListItemKey));
    FChangesMade:= True;
  end;
end;  // AddBiotopeToRucksack

//==============================================================================
procedure TfrmRuckSack.pcRuckSackEnter(Sender: TObject);
begin
  inherited;
  SetFocusToListBox;
  UpdateMenus;
end;  // pcRucksackEnter

//==============================================================================
procedure TfrmRuckSack.ListboxClick(Sender: TObject);
begin
  inherited;
  UpdateMenus;
end;  // ListBoxClick


//==============================================================================
procedure TfrmRuckSack.WMRefreshColours(var Msg: TMessage);
begin
  Repaint;
end;

//==============================================================================
procedure TfrmRucksack.UpdateList(AList:TListBox; AKeyList:TKeyList;
  AGetItemText:TGetItemText);
var lIdx :integer;
    lText:string;
begin
  with AKeyList do
    for lIdx:=0 to Header.ItemCount-1 do begin
      lText:=AGetItemText(Items[lIdx].KeyField1);
      if lText='' then begin
        lText:=Format(ResStr_ItemNotFound,[Items[lIdx].KeyField1]);
        FChangesMade:=true;
      end;
      AList.Items[lIdx]:=lText;
    end;
end;  // UpdateList

//==============================================================================
procedure TfrmRuckSack.UpdateStringGrid(AStringGrid: TStringGrid;
  AKeyList: TKeyList; AGetItemText: TGetItemText);
var lIdx :integer;
    lText:string;
begin
  with AKeyList do
    for lIdx:=0 to Header.ItemCount-1 do begin
      lText:=AGetItemText(Items[lIdx].KeyField1);
      if lText='' then begin
        lText:=Format(ResStr_ItemNotFound,[Items[lIdx].KeyField1]);
        FChangesMade:=true;
        if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
          AppSettings.LastSearchedRucksack.IsDirty := True;
      end;
      AStringGrid.Cells[0, lIdx]:=lText;
    end;
end;

//==============================================================================
procedure TfrmRuckSack.UpdateTaxonList;
begin
  UpdateStringGrid(sgRucksack, AppSettings.CurrentRuckSack.TaxonList,dmGeneralData.GetTaxonName);
end;  // UpdateTaxonList

//==============================================================================
procedure TfrmRuckSack.UpdateBiotopeList;
begin
  UpdateList(lbBiotopes,AppSettings.CurrentRuckSack.BiotopeList,dmGeneralData.GetBiotopeCodeName);
end;  // UpdateBiotopeList

//==============================================================================
procedure TfrmRuckSack.UpdateLocationList;
begin
  UpdateList(lbLocations, AppSettings.CurrentRuckSack.LocationList,GetLocationName);
end;  // UpdateLocationList

//==============================================================================
procedure TfrmRuckSack.UpdateNameList;
begin
  UpdateList(lbPeople,AppSettings.CurrentRuckSack.NameList,dmGeneralData.GetName);
end;  // UpdateNameList

//==============================================================================
procedure TfrmRuckSack.UpdateReferenceList;
begin
  UpdateList(lbReferences,AppSettings.CurrentRuckSack.DocumentList,dmGeneralData.GetReferenceText);
end;  // UpdateReferenceList

{-------------------------------------------------------------------------------
  If a batch update sets Recorder to read only mode, allow the user to browse
  and save any changes already made, but don't allow any more changes.
}
procedure TfrmRucksack.ApplySecurity;
begin
  actRemove.Visible := AppSettings.UserAccessLevel >= ualRecorder;
  actClearRucksack.Visible := AppSettings.UserAccessLevel >= ualRecorder;
  actRemove.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
  sgRucksack.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
  lbBiotopes.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
  lbLocations.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
  lbPeople.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
  lbReferences.Enabled := AppSettings.UserAccessLevel >= ualRecorder;
end;

{-------------------------------------------------------------------------------
  Determine the name and value from a name-value pair contained in a string.
}
class procedure TfrmRuckSack.SplitNameValuePair(var AName, AValue: string;
  const S: string);
var
  EqualsPos: integer;
begin
  EqualsPos := Pos('=', S);
  if  (EqualsPos > 0) then begin
    AName := Copy(S, 1, EqualsPos - 1);
    AValue := Copy(S, EqualsPos + 1, MaxInt);
  end else begin
    AName := S;
    AValue := '';
  end;    // if (EqualsPos > 0)
end;    // TfrmRuckSack.SplitNameValuePair

{-------------------------------------------------------------------------------
  Only allow editing in the Search Code column, and then only if there's a taxon
}
procedure TfrmRuckSack.sgRucksackMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lCol, lRow: integer;
begin
  inherited;
  sgRucksack.MouseToCell(X, Y, lCol, lRow);
  if (lCol <= 0) or (lRow < 0) or (sgRucksack.Cols[0][lRow] = '') then begin
    sgRucksack.Options := sgRucksack.Options - [goEditing] + [goRowSelect] + [goRangeSelect];
    sgRucksack.Col := 0;
  end else begin
    sgRucksack.Options := sgRucksack.Options + [goEditing] - [goRowSelect];
    sgRucksack.Col := lCol;
  end;    // if (lCol = 0) or (sgRucksack.Cols[0][lRow] = '')
  // Reset the flag that prevents automatic row selection whilst dragging
  FDragging := False;
  UpdateMenus;
end;    // TfrmRuckSack.sgRucksackMouseUp

{-------------------------------------------------------------------------------
  Only allow editing in the Search Code column
}
procedure TfrmRuckSack.sgRucksackSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  // Can select cells in column 0 when in Row Select mode.
  CanSelect := (not FDragging)
      and ((goRowSelect in sgRucksack.Options)
           or ((ACol = 1) and (sgRucksack.Cells[0, ARow] <> '') and (ARow <> 0)));
           
  if (not CanSelect) then
    // If the rucksack has changed and now has fewer rows,
    //  setting RowSelect gives an exception,
    //  but resetting the Row property gives a stack overflow
    if (sgRucksack.Row < sgRucksack.RowCount) then
      sgRucksack.Options := sgRucksack.Options - [goEditing] + [goRowSelect]
    else
      sgRucksack.Options := sgRucksack.Options - [goEditing];
  UpdateMenus;
end;  // TfrmRuckSack.sgRucksackSelectCell

{-------------------------------------------------------------------------------
  Capture the Search Code entered by the user.
  Despite what Delphi's help says, the SetEditText event is raised every time
  the value in the cell changes, but it also appears to be raised
  once at the end of editing, without the value changing
  - this is when the search code text is actually captured to avoid
  unnecessary modifications to the data stored in the TEditableKeyList.
}
procedure TfrmRuckSack.sgRucksackSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  lSelStart: integer;
begin
  inherited;
  FSearchCodeRow := ARow;
  if (FPreviousSearchCode = Value) then begin
    CaptureSearchCode(Value, FSearchCodeRow);
    FEditingSearchCode := False;
  end else begin
    FEditingSearchCode := True;
    if (Length(Value) > MAX_SEARCH_CODE_LENGTH) then begin
      lSelStart := TGridAccessor(Sender).InplaceEditor.SelStart;
      sgRucksack.Cells[ACol, FSearchCodeRow] := FPreviousSearchCode;
      TGridAccessor(Sender).InplaceEditor.SelStart := lSelStart - 1;
      ValidateValue(False, Format(ResStr_SearchCodeTooLong, [MAX_SEARCH_CODE_LENGTH]), sgRucksack);
    end else begin
      FPreviousSearchCode := Value;
    end;    // if (Length(Value) > MAX_SEARCH_CODE_LENGTH)
  end;    // if (FPreviousSearchCode = Value)
end;    // TfrmRuckSack.sgRucksackSetEditText

{-------------------------------------------------------------------------------
  Ensure that the search code is saved if being edited when leaving grid.
}
procedure TfrmRuckSack.sgRucksackExit(Sender: TObject);
begin
  inherited;
  if FEditingSearchCode then begin
    FEditingSearchCode := False;
    CaptureSearchCode(sgRucksack.Cells[1, FSearchCodeRow], FSearchCodeRow);
  end;    // if FEditingSearchCode
end;    // TfrmRuckSack.sgRucksackExit

{-------------------------------------------------------------------------------
  Ensure that text is drawn in italics when neccessary.
  Calls the generic procedure GenericDrawTaxonNames (in general data) to control
  the display of taxon names as either common names or the proper taxon names,
  depending upon upon the global "AppSettings.DisplayTaxonCommonNames" property

  Called: On drawing each item in the Taxon string grid (sgRucksack)
}
procedure TfrmRuckSack.sgRucksackDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  lTaxonNames: TTaxonNames;
  lRect: TRect;
begin
  inherited;
  if (ACol = 0) and (ARow > 0) then
  begin
    if not (csDestroying in sgRucksack.ComponentState) and
       Assigned(sgRucksack.Objects[ACol, ARow]) then
    begin
      sgRucksack.Canvas.Brush.Color := MergeColours(clWindow, clBtnFace, 50);
      if ARow = sgRucksack.Row then
        sgRucksack.Canvas.Brush.Color := MergeColours(sgRucksack.Canvas.Brush.Color, clHighlight, 50);
      sgRucksack.Canvas.Font.Color :=
          GetContrastColour(MergeColours(sgRucksack.Canvas.Brush.Color, sgRucksack.Canvas.Brush.Color, 50));
      sgRucksack.Canvas.FillRect(Rect);

      lTaxonNames  := (sgRucksack.Objects[ACol, ARow]) as TTaxonNames;
      lRect.Left   := Rect.Left + 2;
      lRect.Top    := Rect.Top + 1;
      lRect.Bottom := Rect.Bottom;
      lRect.Right  := Rect.Right;
      if Assigned(lTaxonNames) then
        GenericDrawTaxonNames(sgRucksack.Canvas, lTaxonNames, lRect, True);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Process the Delete key - Needs to be done on KeyUp rather than KeyDown
  because KeyDown doesn't seem to catch the Delete key press.
}
procedure TfrmRuckSack.sgRucksackKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Sender is TStringGrid) then
    if (Key = VK_DELETE) and (goEditing in TStringGrid(Sender).Options) then
      if (Assigned(TGridAccessor(Sender).InplaceEditor)) then
        HandleDeleteKey(TGridAccessor(Sender).InplaceEditor);
  sgRucksack.Repaint;
end;    // TfrmRuckSack.sgRucksackKeyUp

{-------------------------------------------------------------------------------
  Process the Delete key for the InPlaceEdit control
  - it doesn't seem to be handled automatically for some reason
}
procedure TfrmRuckSack.HandleDeleteKey(AInplaceEdit: TInplaceEdit);
var
  lRevisedText: string;
  lSelStart: integer;
begin
  if (Length(AInplaceEdit.Text) > 0)
    and (AInplaceEdit.SelStart < Length(AInplaceEdit.Text)) then
  begin
    lSelStart := AInplaceEdit.SelStart;
    if (AInplaceEdit.SelLength = 0) then begin
      lRevisedText := Copy(AInplaceEdit.Text, 1, lSelStart)
        + Copy(AInplaceEdit.Text, lSelStart + 2, MaxInt);
    end else begin
      lRevisedText := Copy(AInplaceEdit.Text, 1, lSelStart)
        + Copy(AInplaceEdit.Text, lSelStart + AInplaceEdit.SelLength + 1, MaxInt);
    end;    // if (AInplaceEdit.SelLength = 0)
    AInplaceEdit.Text := lRevisedText;
    AInplaceEdit.SelStart := lSelStart;
    CaptureSearchCode(AInplaceEdit.Text, sgRucksack.Row);
    FChangesMade := True;
    if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
      AppSettings.LastSearchedRucksack.IsDirty := True;
  end;
end;    // TfrmRuckSack.HandleDeleteKey

{-------------------------------------------------------------------------------
  Capture the Search Code entered by the user
}
procedure TfrmRuckSack.CaptureSearchCode(const ASearchCode: string;
  const ARow: integer);
var
  i, SearchCodeIndex: integer;
  KeyList: TEditableKeyList;
  Key: TKeyString;
  lSearchCode: string;
begin
  lSearchCode := Trim(ASearchCode);
  if not AnsiSameText(lSearchCode, ASearchCode) then
    sgRucksack.Cells[1, ARow] := lSearchCode;
  Key := TTaxonNames(sgRucksack.Objects[0, ARow]).TaxonListItemKey;
  SearchCodeIndex := -1;
  KeyList := AppSettings.CurrentRucksack.TaxonSearchCodeList;
  if (KeyList.IndexOf(Key, lSearchCode) < 0) then begin
    for i := 0 to KeyList.Header.ItemCount - 1 do
      if (KeyList.Items[i].KeyField1 = Key) then begin
        SearchCodeIndex := i;
        break;
      end;    // if (KeyList.Items[i].KeyField1 = Key)
    if IsDataStringPresent(lSearchCode, KeyList) then begin
      if (SearchCodeIndex >= 0) then
        sgRucksack.Cells[1, ARow] := KeyList.Items[SearchCodeIndex].KeyField2
      else
        sgRucksack.Cells[1, ARow] := '';
      sgRucksack.Row := ARow;
      sgRucksack.EditorMode := True;
      ValidateValue(False, Format(ResStr_UniqueSearchCode, [lSearchCode]), sgRucksack);
    end else begin
      if (SearchCodeIndex >= 0) then
        KeyList.DeleteItem(SearchCodeIndex);
      if (Length(lSearchCode) > 0) then
        KeyList.AddItem(Key, lSearchCode);
      FChangesMade := True;
      if (AppSettings.LastSearchedRucksack.UseCurrentRucksack) then
        AppSettings.LastSearchedRucksack.IsDirty := True;
    end;    // if IsDataStringPresent(lSearchCode, KeyList)
  end;    // if (KeyList.IndexOf(Key, lSearchCode) < 0)
end;    // TfrmRuckSack.CaptureSearchCode

{-------------------------------------------------------------------------------
  Determine whether the supplied data string is present in the key list
}
function TfrmRuckSack.IsDataStringPresent(const ADataString: string;
  AKeyList: TEditableKeyList): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to AKeyList.Header.ItemCount - 1 do
    if AnsiSameText(AKeyList.Items[i].KeyField2, ADataString) then begin
      Result := True;
      Break;
    end;    // if (AKeyList.Items[i].KeyField2 = ADataString)
end;

end.
