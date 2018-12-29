//==============================================================================
//  Unit:        NewPlaceCard
//
//  Implements:  dlgNewPlaceCard
//
//  Description: Used to create new recording cards.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 8 March 2002
//               CCN EHS3 - Page control for column selection. Measurement columns
//               built as all valid combinations of Type, Qualifier and Unit and
//               displayed as "[Type] of [Qualifier] (Unit)".
//
//               Eric Salmon - 11 March 2002
//               Bug fix. Measurement columns list populated before the first
//               recording card is parsed.
//
//  Last Revision Details:
//    $Revision: 71 $
//    $Date: 21/01/09 15:19 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit NewPlaceCard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, FileCtrl, DBListCombo, DataClasses, ComCtrls,
  ExceptionForm, TaxonDictBrowserData, OnlineHelp, GeneralFunctions, Grids,
  ImageListButton, Menus, Contnrs, PlaceCardData;

type
  ECRDError = class(TExceptionPath);

  TGuidObject = class(TObject)
  private
    FGuid : TGuid;
  public
    property Guid : TGuid read FGuid write FGuid;
  end;

  TdlgNewPlaceCard = class(TForm)
    gbStep2 : TGroupBox;
    lblSpeciesList: TLabel;
    rbTaxonDictionary : TRadioButton;
    rbRucksack : TRadioButton;
    cmbRucksack : TComboBox;
    cmbTaxonDictionary : TDBListCombo;
    gbStep3 : TGroupBox;
    lblAddColumns: TLabel;
    lbSelected : TListBox;
    gbAddin : TGroupBox;
    cmbHeaderTypes : TComboBox;
    lblAddin : TLabel;
    pcColumns: TPageControl;
    tsStandard: TTabSheet;
    tsMeasurements: TTabSheet;
    lbAvailable: TListBox;
    lbMeasurements: TListBox;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    eRename: TEdit;
    pmColNames: TPopupMenu;
    pmColNamesRename: TMenuItem;
    pmColNamesReset: TMenuItem;
    pnlButtons: TPanel;
    bbAdd: TImageListButton;
    bbAddAll: TImageListButton;
    bbRemove: TImageListButton;
    bbClearAll: TImageListButton;
    bbMoveUp: TImageListButton;
    bbMoveDown: TImageListButton;
    gbStep2a: TGroupBox;
    Label2: TLabel;
    gbStep1: TGroupBox;
    lblistName: TLabel;
    Label3: TLabel;
    bbListRemove: TImageListButton;
    cmbListNames: TComboBox;
    cmbSurveyList: TDBListCombo;
    cbDefaultSurvey: TCheckBox;
    gbStep2b: TGroupBox;
    Label1: TLabel;
    cmbTaxonGroupList: TDBListCombo;
    cbTaxonGroup: TCheckBox;
    Label4: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbAddClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure bbClearAllClick(Sender: TObject);
    procedure bbMoveUpClick(Sender: TObject);
    procedure bbMoveDownClick(Sender: TObject);
    procedure bbAddAllClick(Sender: TObject);
    procedure ColumnListsClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbListClick(Sender: TObject);
    procedure bbListRemoveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbListNamesChange(Sender: TObject);
    procedure cmbTaxonDictionaryChange(Sender: TObject);
    procedure cmbRucksackChange(Sender: TObject);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure pmColNamesRenameClick(Sender: TObject);
    procedure pmColNamesResetClick(Sender: TObject);
    procedure eRenameKeyPress(Sender: TObject; var Key: Char);
    procedure eRenameExit(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbDefaultSurveyClick(Sender: TObject);
    procedure cbTaxonGroupClick(Sender: TObject);
  private
    hlpNewRecCard : TOnlineHelp;
    FdmTaxonDictionary : TdmTaxonDictBrowser;
    FTaxonDiction : string;
    FRucksackName : string;
    FCardPath     : string;
    FRucksackPath : string;
    FdmPlaceCard: TdmPlaceCard;
    procedure AdjustForCOMHeaders;
    procedure CheckButtons;
    procedure GetFromRucksack(KeyList: TStringList);
    procedure GetFromTaxonList(KeyList: TStringList);
    function GenerateCRD: boolean;
    procedure MoveOneItem(const ASource, ADest: TListBox);
    procedure ParseCRD;
    procedure PopulateItemsFromFiles(icmbTarget: TComboBox; const istFileMask: string);
    procedure ResetForm;
    function ValidColumn(const originalName: String): Boolean;
    function ValidColumnName(const columnName: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, Maintbar, Finder, Search, Registry,
  Recorder2000_tlb, ComAddinUnit, ComObj, Constants,  SQLConstants,FormActions,
  DatabaseAccessADO;

resourcestring
  ResStr_DeleteRecordingCard =  'This will delete the ''%s'' recording card. Are you sure?';

  ResStr_RecordingCardDeleted = 'Recording card successfully deleted';

  ResStr_CannotDeleteRecordingCard =
      'Unable to delete the recording card. The associated file may no longer exist.';

  ResStr_SelectColumn =
      'Although the "Count" column is still available for selection, '
      + 'and has been kept for backward compatibility purposes, '
      + 'it is being replaced by a more comprehensive selection of measurement columns, '
      + 'available on the Measurements page.  '#13#13'Do you still want to select this column?';

  ResStr_RecordCardMissing =
      'The Record Card Name is missing. Enter a name or select one from the list.';

  ResStr_SelectTaxonList =  'You must select a Taxon List for the record card.';
  ResStr_SelectDefaultSurvey = 'Either select a default Survey or untick.';
  ResStr_SelectDefaultGroup  = 'Either select a default Group or untick.';
  ResStr_SelectRucksack = 'You must select a Rucksack for the record card.';
  ResStr_OverwriteRecordingCard =
      'A recording card with the name ''%s'' already exists. Do you wish to overwrite it?';
  ResStr_NoClassID =  'There is no class ID associated with this record card header.';
  ResStr_SelRecordCardHeaderStyle = 'You must select a record card header style';
  ResStr_ConfirmExpandList =
      'Do you want to expand the current rucksack list to '
      + 'include all the items below it in the taxon hierarchy?';
  ResStr_NoSurvey = 'No survey has been selected';
  ResStr_NoComRegistry = 'The registry setting for COM addins cannot be located on your machine';
  ResStr_CannotCreateFile = 'You do not have the permissions required to save your record card to the '+
      'record cards folder.';

//==============================================================================
//==============================================================================
procedure TdlgNewPlaceCard.FormCreate(Sender: TObject);
var lCursor:TCursor;
begin
  inherited;
  //Create data module
  FdmTaxonDictionary:= TdmTaxonDictBrowser.Create(nil);
  FdmTaxonDictionary.qryLocalLists.Open;
  cmbTaxonDictionary.Active:= True;
  cmbSurveyList.Active:= True;
  cmbTaxonGroupList.Active:= True;
  FdmPlaceCard := TdmPlaceCard.Create(nil);
  FdmPlaceCard.SetColumnNames(lbAvailable.Items, lbMeasurements.Items);

  lCursor:=HourglassCursor;
  try
    if AppSettings.RecordingCardPath='' then
      FCardPath:=ExtractFilePath(Application.ExeName)
    else
      FCardPath:=AppSettings.RecordingCardPath;
    PopulateItemsFromFiles(cmbListNames, FCardPath + '*.crd');
    if AppSettings.RucksackPath='' then
      FRucksackPath:=ExtractFilePath(Application.ExeName)
    else
      FRucksackPath:=AppSettings.RucksackPath;
    PopulateItemsFromFiles(cmbRucksack, FRucksackPath+ '*.ruk');

    { IDR : 30/11/99 }
    { Are there any COM addin headers? }
    AdjustForCOMHeaders;
    pcColumns.ActivePage := tsStandard;

    // Reset the form and ensure the default record card is parsed.
    // And do this after the Measurement Columns are known
    cmbListNamesChange(nil);

    //Help Setup
    hlpNewRecCard    := TOnlineHelp.Create(Self.Handle);
    OnHelp           := hlpNewRecCard.OnHelpReplacement;
    Self.HelpContext := IDH_NEWRECCARD;
  finally
    DefaultCursor(lCursor);
  end;
end;  // FormCreate

//==============================================================================
procedure TdlgNewPlaceCard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
end;  // FormClose

//==============================================================================
procedure TdlgNewPlaceCard.FormDestroy(Sender: TObject);
begin
  inherited;
  hlpNewRecCard.Free;
  FdmTaxonDictionary.Free;
  FdmPlaceCard.Free;
end;  // FormDestroy

//==============================================================================
procedure TdlgNewPlaceCard.ParseCRD;
var lCardFile: TextFile;
    lCardName: String;
    lCurrent : String;
    lCursor  : TCursor;
    lCount   : Integer;
    lIdx     : Integer;
begin
  //Open file
  lCursor:=HourglassCursor;
  lCardName := cmbListNames.Text;
  if not FileExists(FCardPath + cmbListNames.Text + '.crd') then Exit;

  AssignFile(lCardFile, FCardPath + cmbListNames.Text + '.crd');
  try
    Reset(lCardFile);
    //Read general stuff
    ReadLn(lCardFile, lCurrent);
    if CompareText(lCurrent, '<General>') <> 0 then
      raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));

    ReadLn(lCardFile, lCurrent);
    while CompareText(lCurrent, '</General>') <> 0 do
    begin
      if CompareText(lCurrent, 'Rucksack') = 0 then
      begin
        rbRucksack.Checked := True;
        ReadLn(lCardFile, lCurrent);
        FRucksackName := lCurrent;
        cmbRucksack.ItemIndex := cmbRucksack.Items.IndexOf(FRucksackName);
      end else
      if CompareText(lCurrent, 'TaxonDict') = 0 then
      begin
        rbTaxonDictionary.Checked := True;
        ReadLn(lCardFile, lCurrent);
        FTaxonDiction:=lCurrent;
        cmbTaxonDictionary.KeyValue:= lCurrent;
      end;
      ReadLn(lCardFile, lCurrent);
    end;

    //Read columns
    ReadLn(lCardFile, lCurrent);
    if CompareText(lCurrent, '<Columns>') <> 0 then
      raise ECRDError.CreateNonCritical(Format(ResStr_BadRecCard, [lCardName]));

    ReadLn(lCardFile, lCurrent);
    while CompareText(lCurrent, '</Columns>') <> 0 do
    begin
      // "Accuracy" and "Count of" are only added when saving and when "Count" is selected
      if ValidColumn(lCurrent) then
      begin
        // Add the columns only if they are valid
        lIdx := FdmPlaceCard.IndexOfMappedColumn(lCurrent, True);
        if lIdx <> -1 then begin
          lbSelected.Items.AddObject(FdmPlaceCard.MappedColumns[lIdx], FdmPlaceCard.MappedColumns.Objects[lIdx]);
          lbAvailable.Items.Delete(lbAvailable.Items.IndexOf(FdmPlaceCard.MappedColumns[lIdx]));
        end;
        // If a measurement column can not be found in main list, drop it
        lIdx := FdmPlaceCard.IndexOfMappedColumn(lCurrent, False);
        if lIdx <> -1 then begin
          lbSelected.Items.AddObject(FdmPlaceCard.MappedColumns[lIdx], FdmPlaceCard.MappedColumns.Objects[lIdx]);
          lbMeasurements.Items.Delete(lbMeasurements.Items.IndexOf(FdmPlaceCard.MappedColumns[lIdx]));
        end;
      end;
      ReadLn(lCardFile, lCurrent);
    end;
    // Read Column Names
    if not Eof(lCardFile) then begin
      ReadLn(lCardFile, lCurrent);
      // Must handle older cards, those without the Column Names section
      if CompareText(lCurrent, '<Column Names>') = 0 then begin
        lCount := 0;
        ReadLn(lCardFile, lCurrent);
        while CompareText(lCurrent, '</Column Names>') <> 0 do begin
          // "Accuracy" and "Count of" are only added when saving and when "Count" is selected
          if ValidColumnName(lCurrent) then
          begin
            if lCount < lbSelected.Items.Count then
              lbSelected.Items[lCount] := lCurrent;
            Inc(lCount);
          end;
          ReadLn(lCardFile, lCurrent);
        end;
      end;
    end;
  finally
    //Close file
    CloseFile(lCardFile);
    DefaultCursor(lCursor);
  end;
  CheckButtons;
end;  // ParseCRD

//==============================================================================
procedure TdlgNewPlaceCard.cmbListNamesChange(Sender: TObject);
var
  lFileName: String;
begin
  ResetForm;
  bbOk.Enabled := True;
  lFileName := FCardPath + cmbListNames.Text + '.crd';
  if FileExists(lFileName) then begin
    ParseCRD;
    bbOk.Enabled := (FileGetAttr(lFileName) and faReadOnly) <> faReadOnly;
  end;

  rbTaxonDictionary.Enabled  := bbOk.Enabled;
  rbRucksack.Enabled         := bbOk.Enabled;
  cmbTaxonDictionary.Enabled := rbTaxonDictionary.Checked and bbOk.Enabled;
  cmbRucksack.Enabled        := rbRucksack.Checked and bbOk.Enabled;

  lbAvailable.Enabled := bbOk.Enabled;
  lbMeasurements.Enabled := bbOk.Enabled;
  if bbOk.Enabled then lbSelected.PopupMenu := pmColNames
                  else lbSelected.PopupMenu := nil;
  CheckButtons;
end;  // cmbListNamesChange

//==============================================================================
procedure TdlgNewPlaceCard.ResetForm;
begin
  rbTaxonDictionary.Checked   := True;
  cmbTaxonDictionary.ItemIndex:= -1;
  cmbRucksack.ItemIndex       := -1;
  bbClearAllClick(nil);
end;  // ResetForm

//==============================================================================
procedure TdlgNewPlaceCard.bbListRemoveClick(Sender: TObject);
begin
  if cmbListNames.ItemIndex <> -1 then
    if Messagedlg(Format(ResStr_DeleteRecordingCard, [cmbListNames.Text]),
        mtConfirmation, [mbYes, mbNo],0) = mrYes then
    begin
      if DeleteFile(FCardPath + cmbListNames.Text + '.crd') then
        MessageDlg(ResStr_RecordingCardDeleted, mtInformation, [mbOK], 0)
      else
        MessageDlg(ResStr_CannotDeleteRecordingCard, mtWarning, [mbOK], 0);

      PopulateItemsFromFiles(cmbListNames, FCardPath + '*.crd');
      frmMain.UpdateRecordingCards;
      with cmbListNames do
        if Items.Count > 0 then
          Text := Items[0]
        else
          Text := '';
      cmbListNamesChange(Self);
    end;
end;  // bbListRemoveClick

//==============================================================================
procedure TdlgNewPlaceCard.rbListClick(Sender: TObject);
var tfTaxonOn : boolean;
begin
  tfTaxonOn := (Sender = rbTaxonDictionary);
  cmbTaxonDictionary.Enabled := tfTaxonOn and bbOk.Enabled;
  cmbRucksack.Enabled        := not tfTaxonOn and bbOk.Enabled;
  if tfTaxonOn then begin
    cmbTaxonDictionary.KeyValue:=FTaxonDiction;
    FRucksackName:='';
    cmbRucksack.ItemIndex:=-1;
  end else begin
    cmbRucksack.ItemIndex:=cmbRucksack.Items.IndexOf(FRucksackName);
    FTaxonDiction:='';
    cmbTaxonDictionary.ItemIndex:=-1;
  end;
end;  // rbListClisk

//==============================================================================
procedure TdlgNewPlaceCard.ColumnListsClick(Sender: TObject);
begin
  inherited;
  CheckButtons;
end;  // ColumnListsClick

//==============================================================================
procedure TdlgNewPlaceCard.MoveOneItem(const ASource, ADest: TListBox);
var lIdx, lIdx2: Integer;
    lColName: String;
    lStr1, lStr2: String;
begin
  lIdx := ASource.ItemIndex;
  if lIdx <> -1 then begin
    with ASource.Items do begin
      if ADest = lbSelected then
        lColName := Strings[lIdx]
      else
        lColName := FdmPlaceCard.MappedColumnNameFromObject(Objects[lIdx]);  // Get official column name

      ADest.Items.AddObject(lColName, Objects[lIdx]);
      Delete(lIdx);

      // If "Common Name" was selected, move it to top of the list, always
      if (lColName = ResStr_ColumnCommonName) and (ADest = lbSelected) then
        ADest.Items.Move(ADest.Items.Count - 1, 0)
      else begin
      // If Determiner or Date of Determination selected, deal with the paired item
        lStr1 := lColName;
        if SameText(lStr1, ResStr_ColumnDeterminer) then lStr2 := ResStr_ColumnDateOfDetermination
        else
        if SameText(lStr1, ResStr_ColumnDateOfDetermination) then lStr2 := ResStr_ColumnDeterminer
        else
          lStr1 := '';

        if lStr1 <> '' then begin
          // Locate correct location for other string, displayed name may have changed!
          if ADest = lbSelected then
            lIdx2 := IndexOf(lStr2)
          else
          // Source is lbSelected, so get correct index from MappedColumns
            lIdx2 := IndexOfObject(FdmPlaceCard.MappedColumns.Objects[FdmPlaceCard.MappedColumns.IndexOf(lStr2)]);

          if lIdx2 < lIdx then Dec(lIdx);
          ADest.Items.AddObject(lStr2, Objects[lIdx2]);
          Delete(lIdx2);
        end;
      end;

      if Count = 0 then lIdx := -1
      else
      if lIdx > 0  then lIdx := (lIdx - 1) mod Count;
    end;
    ASource.ItemIndex := lIdx;
    CheckButtons;
  end;
end;  // MoveOneItem

//==============================================================================
procedure TdlgNewPlaceCard.bbAddClick(Sender: TObject);
begin
  inherited;
  if pcColumns.ActivePage = tsStandard then begin
    if (lbAvailable.Items[lbAvailable.ItemIndex] <> ResStr_ColumnCount) or
       (MessageDlg(ResStr_SelectColumn, mtInformation, [mbYes, mbNo], 0) = mrYes) then
      MoveOneItem(lbAvailable, lbSelected)
  end else
    MoveOneItem(lbMeasurements, lbSelected);
end;  // bbAddClick

//==============================================================================
procedure TdlgNewPlaceCard.bbRemoveClick(Sender: TObject);
begin
  inherited;
  if TColumnName(lbSelected.Items.Objects[lbSelected.ItemIndex]).IsStandard then
    MoveOneItem(lbSelected, lbAvailable)
  else
    MoveOneItem(lbSelected, lbMeasurements);
end;  // bbRemoveClick

//==============================================================================
procedure TdlgNewPlaceCard.bbAddAllClick(Sender: TObject);
var liIdx  : Integer;
    loItems: TStrings;
begin
  inherited;
  if pcColumns.ActivePage = tsStandard then loItems := lbAvailable.Items
                                       else loItems := lbMeasurements.Items;
  with loItems do
    for liIdx := 0 to Count - 1 do begin
      lbSelected.Items.AddObject(Strings[0], Objects[0]);
      Delete(0);
    end;
  with lbSelected.Items do
    if IndexOf(ResStr_ColumnCommonName) > -1 then Move(IndexOf(ResStr_ColumnCommonName), 0);
  CheckButtons;
end;  // bbAddAllClick

//==============================================================================
procedure TdlgNewPlaceCard.bbClearAllClick(Sender: TObject);
var i   : Integer;
    lCol: TColumnName;
begin
  inherited;
  with lbSelected.Items do
    for i := 0 to Count - 1 do begin
      lCol := TColumnName(Objects[0]);
      if lCol.IsStandard then
        lbAvailable.Items.AddObject(FdmPlaceCard.MappedColumnNameFromObject(lCol), lCol)
      else
        lbMeasurements.Items.AddObject(FdmPlaceCard.MappedColumnNameFromObject(lCol), lCol);
      Delete(0);
    end;
  CheckButtons;
end;  // bbClearAllClick

//==============================================================================
procedure TdlgNewPlaceCard.bbMoveUpClick(Sender: TObject);
begin
  inherited;
  with lbSelected do
    if ItemIndex <> -1 then begin
      Items.Exchange(ItemIndex, ItemIndex - 1);
      CheckButtons;
    end;
end;  // bbMoveUpClick

//==============================================================================
procedure TdlgNewPlaceCard.bbMoveDownClick(Sender: TObject);
begin
  inherited;
  with lbSelected do
    if ItemIndex <> -1 then begin
      Items.Exchange(ItemIndex, ItemIndex + 1);
      CheckButtons;
    end;
end;  // bbMoveDownClick

//==============================================================================
procedure TdlgNewPlaceCard.CheckButtons;
var lLimit   : Byte;
    loListBox: TListBox;
begin
  with lbSelected do begin
    if Items.IndexOf(ResStr_ColumnCommonName) > -1 then lLimit := 1 else lLimit := 0;
    bbMoveUp.Enabled   := (ItemIndex > lLimit) and bbOk.Enabled;
    bbMoveDown.Enabled := (ItemIndex >= lLimit) and (ItemIndex < Items.Count - 1) and bbOk.Enabled;
    bbClearAll.Enabled := (Items.Count > 0) and bbOk.Enabled;
    bbRemove.Enabled   := (ItemIndex <> -1) and bbOk.Enabled;
  end;
  if pcColumns.ActivePage = tsStandard then loListBox := lbAvailable
                                       else loListBox := lbMeasurements;
  with loListBox do begin
    bbAdd.Enabled    := (ItemIndex <> -1) and bbOk.Enabled;
    bbAddAll.Enabled := (Items.Count > 0) and bbOk.Enabled;
  end;
end;  // CheckButtons

//==============================================================================
procedure TdlgNewPlaceCard.bbOKClick(Sender: TObject);
var ltfFileExists : boolean;
begin
  inherited;
  ValidateValue(cmbListNames.Text <> '', ResStr_RecordCardMissing, cmbListNames);
  if rbTaxonDictionary.Checked then
    ValidateValue(cmbTaxonDictionary.Text <> '',
                ResStr_SelectTaxonList, cmbTaxonDictionary);
  if rbRucksack.Checked then
    ValidateValue(cmbRucksack.Text <> '',ResStr_SelectRucksack, cmbRucksack);
  if cbDefaultSurvey.Checked then
    ValidateValue(cmbSurveyList.Text <> '',
                ResStr_SelectDefaultSurvey, cmbSurveyList);
  if cbTaxonGroup.Checked then
    ValidateValue(cmbTaxonGroupList.Text <> '',
                ResStr_SelectDefaultGroup, cmbTaxonGroupList);
  if gbAddin.Visible then
    ValidateValue(cmbHeaderTypes.Text <> '',
                   ResStr_SelRecordCardHeaderStyle, cmbHeaderTypes);

  ltfFileExists := FileExists(FCardPath + cmbListNames.Text + '.crd');
  if (ltfFileExists and
     (MessageDlg(Format(ResStr_OverwriteRecordingCard, [cmbListNames.Text]),
                 mtWarning, [mbYes, mbNo], 0) = mrYes)) or not ltfFileExists then
  begin
    bbOK.Enabled := false;
    if GenerateCRD then
      ModalResult := mrOk;
  end else
    ModalResult:=mrNone;
end;  // bbOkClick

//==============================================================================
procedure TdlgNewPlaceCard.bbCancelClick(Sender: TObject);
begin
  inherited;
  ModalResult := mrCancel;
end;  // bbCancelClick

//==============================================================================
function TdlgNewPlaceCard.GenerateCRD: boolean;
var lKeys    : TStringList;
    i, handle: Integer;
    lFileName: string;
    lCursor  : TCursor;
    lGuid    : TGuidObject;
    lColName : String;
begin
  // Check the file extension
  if Pos('.CRD',UpperCase(cmbListNames.Text))=0 then
    lFileName := cmbListNames.Text + '.crd';
  // check permissions
  handle := FileCreate(FCardPath + lFileName);
  if handle>0 then begin
    result := true;
    FileClose(handle);
    lKeys:= TStringList.Create;
    lCursor := HourglassCursor;
    try
      //Store general stuff
      lKeys.Add('<General>');
      if rbTaxonDictionary.Checked then
      begin
        lKeys.Add('TaxonDict');
        lKeys.Add(cmbTaxonDictionary.KeyValue);
      end else begin
        lKeys.Add('Rucksack');
        lKeys.Add(cmbRucksack.Text);
      end;
      lKeys.Add('</General>');

      //Store Columns and Column Names
      with lbSelected.Items do begin
        // Original column name
        lKeys.Add('<Columns>');
        for i := 0 to Count - 1 do begin
          lColName := TColumnName(Objects[i]).OriginalName;
          if not SameText(lColName, COL_SCIENTIFIC_NAME) then
            lKeys.Add(lColName);
          if CompareText(lColName, COL_COUNT) = 0 then begin
            lKeys.Add(COL_COUNT_OF);
            lKeys.Add(COL_ACCURACY);
          end;
        end;
        lKeys.Add('</Columns>');

        // Display name, which could be the same as original
        lKeys.Add('<Column Names>');
        for i := 0 to Count - 1 do begin
          if not SameText(TColumnName(Objects[i]).OriginalName, COL_SCIENTIFIC_NAME) then
          lKeys.Add(Strings[i]);
          if SameText(TColumnName(Objects[i]).OriginalName, COL_COUNT) then begin
            lKeys.Add(ResStr_ColumnCountOf);
            lKeys.Add(ResStr_ColumnAccuracy);
          end;
        end;
        lKeys.Add('</Column Names>');
      end;

      //Store rows
      lKeys.Add('<Taxon>');
      if rbTaxonDictionary.Checked then
        GetFromTaxonList(lKeys)
      else
        GetFromRucksack(lKeys);
      lKeys.Add('</Taxon>');

      //Store survey
      lKeys.Add('<Survey>');
      if cbDefaultSurvey.Checked then
         lkeys.Add(cmbSurveyList.keyvalue)
      else lkeys.Add('All');
      lKeys.Add('</Survey>');

      //Store taxon group
      lKeys.Add('<TaxonGroup>');
      if cbTaxonGroup.Checked then
         lkeys.Add(cmbTaxonGroupList.keyvalue)
      else lkeys.Add('All');
      lKeys.Add('</TaxonGroup>');

      //Store Header Details
      lKeys.Add('<HeaderClsID>');
      if gbAddin.Visible then
      begin
        lKeys.Add(cmbHeaderTypes.text);
        lGuid := TGuidObject.Create;
        try
          if TGuidObject(cmbHeaderTypes.Items.objects[cmbHeaderTypes.ItemIndex]) <> nil then
          begin
            lGuid := TGuidObject(cmbHeaderTypes.Items.objects[cmbHeaderTypes.ItemIndex]);
            lKeys.Add(GUIDToString(lGuid.Guid));
          end else
            raise ECRDError.CreateNonCritical(ResStr_NoClassID);
        finally
          lGuid.Free;
        end; // try..finally
      end // if gbAddin is visible
      else begin
        lKeys.Add(PLACECARD_STANDARD_HEADER_NAME);
        lKeys.Add(PLACECARD_STANDARD_HEADER_CLSID);
      end;
      lKeys.Add('</HeaderClsID>');
      lKeys.SaveToFile(FCardPath + lFileName);
    finally
      lKeys.Free;
      DefaultCursor(lCursor);
    end;
  end else begin
    // no permissions for the save operation
    ShowInformation(ResStr_CannotCreateFile);
    Result := false;
  end;
end;  // GenerateCRD

//==============================================================================
procedure TdlgNewPlaceCard.GetFromRucksack(KeyList: TStringList);
var lRucksack    : TextFile;
    lstCurrent   : String;
    lRucksackList: TStringList;
    i            : integer;
begin
  lRucksackList := nil;
  //Open rucksack file
  AssignFile(lRucksack, FRucksackPath + cmbRucksack.Text + '.ruk');
  Reset(lRucksack);
  try
    Readln(lRucksack, lstCurrent);
    if CompareText(lstCurrent, '<Taxon>')<>0 then
      raise ECRDError.CreateNonCritical(Format(ResStr_BadRucksack, [cmbRucksack.Text]));

    //Read taxon list items from rucksack and add to local list
    lRucksackList := TStringList.Create;
    ReadLn(lRucksack, lstCurrent);
    while CompareText(lstCurrent, '</Taxon>')<>0 do begin
      lRucksackList.Add(lstCurrent);
      ReadLn(lRucksack, lstCurrent);
    end;
    if ConfirmYesNo(ResStr_ConfirmExpandList)=mrYes then begin
      // expand list to get group contents
      dmDatabase.ExecuteSQL(SQL_TEMPLIST_CREATE);
      try
        for i := 0 to lRucksackList.Count-1 do
          dmDatabase.ExecuteSQL(Format(SQL_TEMPLIST_INSERT, [lRucksackList[i]]));
        lRucksackList.Clear;
        with dmDatabase.GetRecordset('usp_Taxa_GetChildren', []) do begin
          while not EOF do begin
            lRucksackList.Add(Fields['Contained_List_Item_Key'].Value);
            MoveNext;
          end;
        end;
      finally
        dmDatabase.ExecuteSQL(SQL_TEMPLIST_DROP);
      end;
    end;
  finally
    KeyList.AddStrings(lRucksackList);
    lRucksackList.Free;
    CloseFile(lRucksack);
  end;
end;  // GetFromRucksack

//==============================================================================
procedure TdlgNewPlaceCard.GetFromTaxonList(KeyList: TStringList);
begin
  //Get taxon list items from table
{  with FdmTaxonDictionary.qryTaxonLeafItems do
  begin
    Parameters.ParamByName('Key').Value := cmbTaxonDictionary.KeyValue;
    Open;
    while not Eof do
    begin
      KeyList.Add(FieldByName('TAXON_LIST_ITEM_KEY').AsString);
      if KeyList.Count mod 100=0 then Application.ProcessMessages;
      Next;
    end;
    Close;
  end;        }
end;  // GetFromTaxonlist

//==============================================================================
procedure TdlgNewPlaceCard.PopulateItemsFromFiles(icmbTarget: TComboBox;
  const istFileMask: string);
var lSR : TSearchRec;
    lCursor    : TCursor;
    lExt       : String;
begin
  lCursor:=HourglassCursor;
  try
    //Setup directory boxes
    icmbTarget.Items.Clear;
    //Find the first file
    if FindFirst(istFileMask, 0, lSR) = 0 then
    begin
      lExt := ExtractFileExt(lSR.Name);
      //Add the file to the menu
      if SameText(lExt, '.crd') or SameText(lExt, '.ruk') then
        icmbTarget.Items.Add(Copy(lSR.Name, 0, Length(lSR.Name) - 4));

      //Add a new menu item for each remaining file
      while FindNext(lSR) = 0 do begin
        lExt := ExtractFileExt(lSR.Name);
        if SameText(lExt, '.crd') or SameText(lExt, '.ruk') then
          icmbTarget.Items.Add(Copy(lSR.Name, 0, Length(lSR.Name) - 4));
      end;
      
      //Free the search results
      FindClose(lSR);
    end;
    if icmbTarget.Items.Count > 0 then
      icmbTarget.ItemIndex := 0;
  finally
    DefaultCursor(lCursor);
  end;
end;  // PopulateItemsFromFiles

//==============================================================================
procedure TdlgNewPlaceCard.cmbTaxonDictionaryChange(Sender: TObject);
begin
  inherited;
  FTaxonDiction := cmbTaxonDictionary.KeyValue;
end;  // cmbTaxonDictionaryChange

//==============================================================================
procedure TdlgNewPlaceCard.cmbRucksackChange(Sender: TObject);
begin
  inherited;
  FRucksackName := cmbRucksack.Text;
end;  // cmbRucksackChange

{==============================================================================
    Procedure Name: AddjustForCOMHeaders
            Called: FormCreate
           Written: IR - 1/12/99
           Purpose: To allow the user their choice of COM header when creating
                    a new record card.
      Side Effects: Rearranges the group boxes on the form, making "gbAddin"
                    visible and inseting it in the third position.  Populates
                    the combo box with the names and Class IDs of all the
                    COM iRecordCardHeader addins.
------------------------------------------------------------------------------}
procedure TdlgNewPlaceCard.AdjustForCOMHeaders;
var
  lCount : integer;
  lTotal : integer;
  lComObject : IUnknown;
  lGuidObj : TGuidObject;
  lCardHeader : iRecordCardHeader;
  lStandardClsID : TGuidObject;
begin
  try
    lStandardClsID := TGuidObject.Create;
    lStandardClsID.Guid := StringToGuid(PLACECARD_STANDARD_HEADER_CLSID);
    cmbHeaderTypes.Items.AddObject(PLACECARD_STANDARD_HEADER_NAME,
                                    lStandardClsID);

    { Go through the installed com objects and select those which are record
      card headers. }
    lGuidObj := TGuidObject.Create;
    lTotal := AppSettings.ComAddins.AddinCount;
    for lCount := 0 to lTotal -1 do
    begin
      try
        lGuidObj.Guid := AppSettings.ComAddins.AddinList[lCount].ClsID;
        lComObject := CreateComObject(lGuidObj.Guid);
        lCardHeader := lComObject as IRecordCardHeader;
        cmbHeaderTypes.Items.AddObject(AppSettings.ComAddins.AddinList[lCount].Name,
                                       lGuidObj);
      except
        on EIntfCastError do
        // do nothing... not an IRecordCardHeader interfacee
      end; // try..except
    end;
    if cmbHeaderTypes.items.count > 1 then
    begin
      gbAddin.Visible := true;
      gbAddin.Top     := gbStep3.Top;
      gbAddin.Left    := gbStep3.Left;
      { Move gbStep3 and bitbtns to be step 4 }
      gbStep3.Top  := gbStep3.Top + gbAddin.Height + 5;
      bbOK.Top     := gbStep3.Top + gbStep3.Height + 6;
      bbCancel.Top := bbOK.Top;
      Self.Height  := Self.Height + gbAddin.Height + 4;
      lblAddColumns.Caption := '4' + Copy(lblAddColumns.Caption, 2,
                                          length(lblAddColumns.Caption));
      gbAddin.TabOrder := 2;
    end;
  finally
  end; // try..finally
end;  // AdjustForCOMHeaders

{===============================================================================
 Description : Sets the  list box hint if the text is too wide to fit
 Created : 04/02/3004}
procedure TdlgNewPlaceCard.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SetListBoxHintIfTooWide(Sender, X, Y);
end;

//==============================================================================
procedure TdlgNewPlaceCard.pmColNamesRenameClick(Sender: TObject);
begin
  inherited;
  with lbSelected do
    if ItemIndex <> -1 then begin
      eRename.Top     := Top + ItemRect(ItemIndex).Top;
      eRename.Text    := Items[ItemIndex];
      eRename.Visible := true;
      eRename.Height  := 17;
      eRename.SetFocus;
      
      bbOk.Default    := false;
      bbCancel.Cancel := false;
    end;
end;  // pmColNamesRenameClick

//==============================================================================
procedure TdlgNewPlaceCard.pmColNamesResetClick(Sender: TObject);
begin
  inherited;
  with lbSelected do
    if ItemIndex <> -1 then
      Items[ItemIndex] := FdmPlaceCard.MappedColumnNameFromObject(Items.Objects[ItemIndex]);
end;  // pmColNamesResetClick

//==============================================================================
procedure TdlgNewPlaceCard.eRenameKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key in [#13, #27] then begin
    if Key = #13 then lbSelected.Items[lbSelected.ItemIndex] := eRename.Text;
    eRename.Visible := false;
    Key := #0;
    lbSelected.SetFocus;
    bbOk.Default    := true;
    bbCancel.Cancel := true;
  end;
end;  // eRenameKeyPress

//==============================================================================
procedure TdlgNewPlaceCard.eRenameExit(Sender: TObject);
var lKey: Char;
begin
  inherited;
  lKey := #13;
  eRenameKeyPress(nil, lKey);
end;  // eRenameExit

//==============================================================================
function TdlgNewPlaceCard.ValidColumn(const originalName: String): Boolean;
begin
  Result := not SameText(originalName, COL_ACCURACY) and
            not SameText(originalName, COL_COUNT_OF) and
            not SameText(originalName, COL_SCIENTIFIC_NAME);
end;

//==============================================================================
function TdlgNewPlaceCard.ValidColumnName(const columnName: String): Boolean;
begin
  Result := not SameText(columnName, ResStr_ColumnAccuracy) and
            not SameText(columnName, ResStr_ColumnCountOf)  and
            not SameText(columnName, ResStr_ColumnScientificName);
end;

procedure TdlgNewPlaceCard.FormResize(Sender: TObject);
var
  availableWidth, halfWidth: Integer;
begin
  availableWidth := gbStep3.Width
                      - 8 * 4 // Spacing between panels.
                      - pnlButtons.Width;
  halfWidth := availableWidth div 2;
  pcColumns.Width := halfWidth;
  pnlButtons.Left := pcColumns.Left + pcColumns.Width + 8;
  lbSelected.Left := pnlButtons.Left + pnlButtons.Width + 8;
  lbSelected.Width := halfWidth;
  // Large font issues - see Mantis 289
  lbSelected.Height := gbStep3.Height - lbSelected.Top - 12;
  pcColumns.Height := gbStep3.Height - pcColumns.Top - 12;
end;

constructor TdlgNewPlaceCard.Create(AOwner: TComponent);
begin
  inherited;
  // Set to sizeable after inherited create. Partially Fixes resize issues if using scaled fonts
  BorderStyle := bsSizeable;
  Constraints.MinWidth := lblAddColumns.Width + // widest control
      (lblAddColumns.Left + gbStep3.Left) * 2 + // it's left pos
      (Width - ClientWidth); // plus dialog border allowance
end;

procedure TdlgNewPlaceCard.cbDefaultSurveyClick(Sender: TObject);
begin
  if cbDefaultSurvey.checked then
    cmbSurveyList.Enabled:=true
  else begin
    cmbSurveyList.Enabled:=false;
    cmbSurveyList.ItemIndex:=-1;
  end;
end;

procedure TdlgNewPlaceCard.cbTaxonGroupClick(Sender: TObject);
begin
  if cbTaxonGroup.checked then
    cmbTaxonGroupList.Enabled:=true
  else begin
    cmbTaxonGroupList.Enabled:=false;
    cmbTaxonGroupList.ItemIndex:=-1;
  end;
end;

end.

