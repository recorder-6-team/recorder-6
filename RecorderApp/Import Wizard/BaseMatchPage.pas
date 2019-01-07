
{===============================================================================
  Unit:        BaseMatchPage

  Defines:     TBaseMatch

  Description: Base class for page requesting of the user to match imported data
               against existing Recorder data.

  Model:       ImportWizard

  Last revision information:
    $Revision: 59 $
    $Date: 21/03/13 15:11 $
    $Author: Michaelcaptain $

===============================================================================}

unit BaseMatchPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWSettings, IWBasePage, ExtCtrls, Grids, FormActions, StdCtrls, DataClasses,
  ImageListButton, AddinCompositeComponent, AddinLinkedControls, Buttons, Menus,
  DBGrids, ImportWizardDBGrid, DB, ADODB, IWColumnMappingClasses, BaseFormUnit,
  ComboListID, StrUtils, Constants, DropTarget, DefaultPaths;

resourcestring
  ResStr_AllMatchRequired = 'All %s must be matched';
  ResStr_Matching = 'All matched';
  ResStr_MultipleMatches = 'There are more than one possible match for the following item(s_:';
  ResStr_MoreUnmatched    = 'more items unmatched...';
  ResStr_ExcludeUnmatched = '%s that are not matched will be excluded from the import. ' +
                            'Are you sure?';
  ResStr_RuleNameTranslations =
    'Names=Names,Species=Species,Biotopes=Biotopes,Locations=Locations,References=References,'+
    '''AbundanceQualifiers=Abundance Qualifiers'',Substrates=Substrates,''RecordTypes=Record Types'','+
    '''AssociationTypes=Association Types'',''SpecimenTypes=Specimen Types'',''AssociatedSpecies=Associated Species'','+
    '''SampleTypes=Sample Types'',''DeterminerRoles=Determiner Roles'',''DeterminationTypes=Determination Types''';

const
  WM_AFTERSCROLL = WM_APP + 1;

var
  InputValue :string;
  UpdateProcedure :string;
  DisplayProcedure : string;
  DetailsProcedure : string;
type
  {-----------------------------------------------------------------------------
    Base class for import wizard pages that allow the user to match each unique instance of
    data for a particular column type to the associated data type in Recorder.
    The wizard Next button is disabled on each match page until all items listed have been
    matched against their Recorder equivalents.  If the Next button is disabled, then text
    is appended to the HTML Details panel: 'You cannot proceed until:' followed by a bullet
    point: 'all items have been matched'.
    In general, the matching process is run as soon as a matching wizard page is first
    displayed.  Note that some pages do not run the matching process, as described in the
    TSD section entitled Value Matching Details.
  }
  TBaseMatch = class(TBasePage)
    cmbCheckLists: TIDComboBox;
    cmbMatchValue: TIDComboBox;
    dbgMatch: TImportWizardDBGrid;
    dsMatchTable: TDataSource;
    eMatchValue: TAddinLinkedEdit;
    lblCheckList: TLabel;
    lblCheckListSearch: TLabel;
    pmBack: TPopupMenu;
    pmSourceData: TMenuItem;
    pnlCheckList: TPanel;
    pnlControls: TPanel;
    shpCheckList: TShape;
    tblMatch: TADOTable;
    dlgSaveUnmatched: TSaveDialog;
    pnlBtnSearch: TPanel;
    btnSearch: TBitBtn;
    pnlBtnNew: TPanel;
    btnNew: TBitBtn;
    pnlBtnNewEntries: TPanel;
    btnNewEntries: TBitBtn;
    pnlBtnCommit: TPanel;
    btnCommit: TBitBtn;
    pnlBtnExcludeUnmatched: TPanel;
    btnExcludeUnmatched: TBitBtn;
    procedure btnCommitClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnNewEntriesClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure cmbCheckListsChange(Sender: TObject);
    procedure cmbCheckListsPopulate(Sender: TObject);
    procedure cmbMatchValueChange(Sender: TObject);
    procedure cmbMatchValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbMatchValuePopulate(Sender: TObject);
    procedure dbgMatchCellClick(Column: TColumn);
    procedure dbgMatchColEnter(Sender: TObject);
    procedure dbgMatchColExit(Sender: TObject);
    procedure dbgMatchDisplayChanged(Sender: TObject);
    procedure dbgMatchDrawColumnCell(Sender: TObject; const ARect: TRect; DataCol: Integer;
        Column: TColumn; State: TGridDrawState);
    procedure dbgMatchGetCellHint(Sender: TObject; ACol: Integer; var AValue: String);
    procedure dbgMatchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure eMatchValueFindData(Sender: TObject);
    procedure eMatchValueGetData(Sender: TObject);
    procedure eMatchValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pmSourceDataClick(Sender: TObject);
    procedure dsMatchTableDataChange(Sender: TObject; Field: TField);
    procedure eMatchValueExit(Sender: TObject);
    procedure btnExcludeUnmatchedClick(Sender: TObject);
  private
    FBusyScrolling: Boolean;
    FMatchRule: TMatchRule;
    FRefreshing: Boolean;
    FScanningDataset: Boolean;
    FDropCell: TGridCoord;
    FRuleNameTranslations: TStringList;
    procedure DropMatchValue(const Sender: TObject; const format: Integer;
      const sourceData: TKeyList; const textStrings: TStringList; var handled: Boolean);
    procedure DropMatchValueOnGrid(const Sender: TObject;
      const iFormat : integer; const iSourceData: TKeyList;
      const iTextStrings : TStringList;
      const iIsPasteOperation: boolean; var ioHandled : boolean);
    procedure CheckDropMatchValueOnGrid(APoint: TPoint;
      const ATable, AFieldKey: String; var Accept: boolean);
    function TrimTags(const AString: String): String;
    procedure UpdateMatchValueBox(KeyList: TKeyList);
    procedure FindNextUnmatched (searchPastEof: Boolean); overload;
    function Translate(const name: string): string;
  protected
    procedure DoEditableValuesLoad; virtual;
    procedure DoEditableValuesSave; virtual;
    procedure FindNextUnmatched; overload; virtual;
    procedure GetDataSourceForm; virtual;
    function GetDataSourceTableName: String; virtual;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetHTMLFileName: String; override;
    function GetHTMLImageName: String; override;
    function GetMatchGrid: TCustomGrid; virtual;
    function GetRowImportValue: string; virtual;
    function GetSkip: Boolean; override;
    procedure GridEnterCell;
    procedure GridLeaveCell;
    procedure LoadContent; override;
    procedure MakeNewEntries; virtual;
    procedure MakeNewEntry; virtual;
    procedure RefreshRow; virtual;
    function RunSearch: Boolean; virtual;
    procedure SaveMatches; virtual;
    function SelectedField: TField; virtual;
    function SetCellColour(const AFieldName: String): Boolean; virtual;
    procedure SetInputControl; virtual;
    procedure SetupDisplay; virtual;
    property MatchGrid: TCustomGrid read GetMatchGrid;
    property Refreshing: boolean read FRefreshing;
    property ScanningDataset: boolean read FScanningDataset;
  public
    procedure Cancel; override;
    procedure CheckDuplicates;
    procedure RegisterDragDropComponents; override;
    procedure SaveContent; override;
    procedure UnregisterDragDropComponents; override;
    property MatchRule: TMatchRule read FMatchRule;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  ColumnTypes, MissingData, Import, GeneralFunctions, DatabaseAccessADO, IWConstants,
  IWSearchManager, AddinSearchManager, IWValidation, ProjectSpecificAccess,
  ApplicationSettings, BaseDictionaryUnit, GeneralData, FastColumnTypes,
  TaxonDictBrowser, BaseTaxonDictUnit, MultipleMatches;

resourcestring
  ResStr_ImportedData         = 'Imported Data';
  ResStr_RecorderMatches      = 'Recorder Matches';
  ResStr_NoMatchRuleForScreen = 'No match rule associated with match screen. Cannot proceed';
  ResStr_NotAvailable         = 'Not available';
  ResStr_DuplicatesMessageStart =
      'In the following records, more than one person has been linked with the same person in'
      + ' the database when they are noted as recording the same observation and so are probably'
      + ' different people. ';
  ResStr_MoreThanTen          = 'There are more than ten cases, so the first ten are listed below:';
  ResStr_DuplicatesMessageEnd =
      #13#10'If you continue, each group will be treated as one person,'
      + ' or you can cancel and correct the problem. Do you want to proceed?';
  ResStr_And                  = ' and ';
  ResStr_LinkedToPerson       = ' have been linked to the person %s in record %s.' + #13#10;
  ResStr_MakeNewEntries       =
      'A new entry will be created for each record still unmatched.  This may take some time'#13#10
      + 'depending on the number of entries to create.'#13#10#13#10
      + 'Do you want to proceed?';

const
  SQL_CHECK_ALL_MATCHED  = 'SELECT Count(*) AS Total FROM #%s WHERE Match_Key IS NULL';
  SQL_CHECK_MATCH_COUNTS = 'SELECT Import_Value FROM #%s WHERE Match_Count > 1';
  GENERIC_MATCH_COL      = 1;
  LOCATION_MATCH_COL     = 2;
 {-==============================================================================
    TBaseMatch
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TBaseMatch.btnCommitClick(Sender: TObject);
begin
  SaveMatches;
end;  // TBaseMatch.btnCommitClick

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.btnNewClick(Sender: TObject);
begin
  MakeNewEntry;
end;  // TBaseMatch.btnNewClick

{-------------------------------------------------------------------------------
  Go through the dataset to create new entries for all non-matched items.
}
procedure TBaseMatch.btnNewEntriesClick(Sender: TObject);
var
  lBookmark: TBookmark;
  lCursor: TCursor;
begin
  if ConfirmYesNo(ResStr_MakeNewEntries) = mrYes then
  begin
    eMatchValue.Visible   := False;
    cmbMatchValue.Visible := False;
    FRefreshing           := True;
    lCursor               := HourglassCursor;
    with tblMatch do begin
      lBookmark := GetBookmark;
      DisableControls;
      try
        MakeNewEntries;
        GotoBookmark(lBookmark);
        btnCommit.Enabled := True;
      finally
        FreeBookmark(lBookmark);
        EnableControls;
        FRefreshing := False;
        DefaultCursor(lCursor);
      end;
    end;
    ChangedContent;
  end;
end;  // TBaseMatch.btnNewEntriesClick

{-------------------------------------------------------------------------------
  Link all the unmatched rows to a corresponding newly created entry in Recorder.
}
procedure TBaseMatch.MakeNewEntries;
begin
  with tblMatch do
    try
      First;
      while not Eof do begin
        if FieldByName(FN_MATCH_KEY).IsNull then
          MatchRule.MakeNewEntry(FieldByName(FN_IMPORT_VALUE).AsString);
        Next;
      end;
    finally
      Requery;
      // Find any left empty one.
      FindNextUnmatched;
    end;
end;  // TBaseMatch.MakeNewEntries

{-------------------------------------------------------------------------------
  Link the current row to a newly created entry in Recorder
}
procedure TBaseMatch.MakeNewEntry;
var
  lBookmark: TBookmark;
begin
  eMatchValue.Visible   := False;
  cmbMatchValue.Visible := False;
  FRefreshing           := True;
  // Stops weird moving about after requery.
  lBookmark := tblMatch.GetBookmark;
  try
    MatchRule.MakeNewEntry(tblMatch.FieldByName(FN_IMPORT_VALUE).AsString);
    tblMatch.Requery;
    tblMatch.GotoBookmark(lBookmark);
    btnCommit.Enabled := True;
  finally
    tblMatch.FreeBookmark(lBookmark);
    FRefreshing := False;
  end;
  RefreshRow;

  // Find another empty one, after the current one.
  FindNextUnmatched;

  // Update combo
  if MatchRule.ControlType = ctIDComboBox then
    with cmbMatchValue do begin
      OnChange := nil;
      Clear;
      PopulateContent;
      OnChange  := cmbMatchValueChange;
      ItemIndex := IDIndexOf(tblMatch.FieldByName(FN_MATCH_KEY).AsString);
    end;
  ChangedContent;
end;  // TBaseMatch.MakeNewEntry

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.btnSearchClick(Sender: TObject);
var
  lBookmark: TBookmark;
  lCursor: TCursor;
  lMultipleMatches: String;
  lCount: Integer;
begin
  inherited;
  if RunSearch then begin
    eMatchValue.Visible   := False;
    cmbMatchValue.Visible := False;
    // Stops weird moving about after requery.
    lBookmark := tblMatch.GetBookmark;
    lCursor   := HourglassCursor;
    try
      MatchRule.MatchRecords(cmbCheckLists.CurrentStrID);
      tblMatch.Requery;
      // Back to where it was.
      tblMatch.GotoBookmark(lBookmark);
    finally
      tblMatch.FreeBookmark(lBookmark);
      DefaultCursor(lCursor);
    end;
    with dmDatabase.ExecuteSQL(Format(SQL_CHECK_MATCH_COUNTS, [MatchRule.Name]), True) do
      if not Eof then begin
        lCount := 0;
        lMultipleMatches := ResStr_MultipleMatches;
        while not Eof do begin
          lMultipleMatches := lMultipleMatches + #13#9 + Fields
          ['Import_Value'].Value;
          Inc(lCount);
          if lCount > 10 then begin
            lMultipleMatches :=
                lMultipleMatches
                + #13#13#9 + IntToStr(RecordCount - 10)
                + ' ' + ResStr_MoreUnmatched;
            Break;
          end;
          MoveNext;
        end;
        if MatchRule.UpdateNotesProcedure = '' then
          MessageDlg(ResStr_MultipleMatches, mtInformation,[mbOk], 0);
      end;

    ChangedContent;
  end;
end;  // TBaseMatch.btnSearchClick

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.cmbCheckListsChange(Sender: TObject);
begin
  inherited;
  btnSearch.Enabled := cmbCheckLists.ItemIndex <> -1;
end;  // TBaseMatch.cmbCheckListsChange 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.cmbCheckListsPopulate(Sender: TObject);
begin
  inherited;
  MatchRule.PopulateChecklistCombo(cmbCheckLists);
  cmbCheckLists.ItemIndex := -1;
end;  // TBaseMatch.cmbCheckListsPopulate

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.cmbMatchValueChange(Sender: TObject);
begin
  inherited;
  DoEditableValuesSave;
  ChangedContent;
end;  // TBaseMatch.cmbMatchValueChange 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.cmbMatchValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_RETURN:
      begin
        // Close combo before moving on, if it's a combo.
        cmbMatchValue.DroppedDown := False;
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      if not cmbMatchValue.DroppedDown and not (ssAlt in Shift) then
      begin
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_LEFT, VK_HOME, VK_RIGHT, VK_END:
      if not cmbMatchValue.DroppedDown then begin
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_ESCAPE:
      with cmbMatchValue do
        ItemIndex := IDIndexOf(tblMatch.FieldByName(FN_MATCH_KEY).AsString);
  end;
end;  // TBaseMatch.cmbMatchValueKeyDown 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.cmbMatchValuePopulate(Sender: TObject);
begin
  inherited;
  MatchRule.PopulateTermListCombo(cmbMatchValue);
end;  // TBaseMatch.cmbMatchValuePopulate 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchCellClick(Column: TColumn);
begin
  inherited;
  GridEnterCell;
end;  // TBaseMatch.dbgMatchCellClick

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchColEnter(Sender: TObject);
begin
  inherited;
  GridEnterCell;
end;  // TBaseMatch.dbgMatchColEnter

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchColExit(Sender: TObject);
begin
  inherited;
  GridLeaveCell;
end;  // TBaseMatch.dbgMatchColExit 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchDisplayChanged(Sender: TObject);
begin
  inherited;
  dbgMatch.Invalidate;
  PostMessage(Handle, WM_AFTERSCROLL, 0, 0);
  if not FRefreshing and not FScanningDataset then SetInputControl;
end;  // TBaseMatch.dbgMatchDisplayChanged

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchDrawColumnCell(Sender: TObject; const ARect: TRect; DataCol:
    Integer; Column: TColumn; State: TGridDrawState);
var
  lValue: String;
  lRect: TRect;
begin
  inherited;

  // If setting special colour, clear and redraw.
  if SetCellColour(Column.FieldName) then begin
    dbgMatch.Canvas.FillRect(ARect);

    // Handle possible 'Not available' values.
    lValue := VarToStr(Column.Field.Value);
    if CompareText(lValue, STR_NOT_AVAILABLE) = 0 then lValue := ResStr_NotAvailable;

    lRect := Rect(ARect.Left, ARect.Top + 2, ARect.Right - 2, ARect.Bottom);
    dmInterface.DrawTerm(dbgMatch.Canvas, lRect, lValue, False);
  end;
end;  // TBaseMatch.dbgMatchDrawColumnCell

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchGetCellHint(Sender: TObject; ACol: Integer; var AValue: String);
begin
  with dbgMatch do
    if dmInterface.TermTextWidth(Canvas, Fields[ACol].AsString) > Columns[ACol].Width - 4 then
      AValue := TrimTags(Fields[ACol].AsString);
end;  // TBaseMatch.dbgMatchGetCellHint

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dbgMatchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  with dbgMatch do
    case Key of
      VK_RETURN:
        begin
          if ssCtrl in Shift then Perform(WM_KEYDOWN, VK_UP, 0)
                             else Perform(WM_KEYDOWN, VK_DOWN, 0);
          Key := 0;
        end;
      VK_LEFT:
        if not tblMatch.Bof and (SelectedIndex = 0) then begin
          tblMatch.Prior;
          // Bof true if already on first record, not if just moved to first record!
          if not tblMatch.Bof then SelectedIndex := Columns.Count - 1;
          Key := 0;
        end;
      VK_RIGHT:
        if not tblMatch.Eof and (SelectedIndex = Columns.Count - 1) then begin
          SelectedIndex := 0;
          tblMatch.Next;
          // Eof true if already on last record, not if just moved to last record!
          if tblMatch.Eof then SelectedIndex := Columns.Count - 1;
          Key := 0;
        end;
    end;
end;  // TBaseMatch.dbgMatchKeyDown 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.DoEditableValuesLoad;
begin
  case MatchRule.ControlType of
    ctLinkedEdit:
        with eMatchValue do begin
          Text := TrimTags(SelectedField.AsString);
          If selectedfield.FieldName = FN_MATCH_VALUE then
            Key  := tblMatch.FieldByName(FN_MATCH_KEY).AsString
          else  // FN_MATCH_NOTES
            Key  := tblMatch.FieldByName(FN_IMPORT_VALUE).AsString;
          EditBox.SelLength := Length(Text);
        end;
    ctIDComboBox:
      with cmbMatchValue do
        ItemIndex := IDIndexOf(tblMatch.FieldByName(FN_MATCH_KEY).AsString);
  end;
end;  // TBaseMatch.DoEditableValuesLoad

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.DoEditableValuesSave;
var
  lKey: String;
  lBookmark: TBookmark;
begin
  if SelectedField.FieldName = FN_MATCH_VALUE then begin
    case MatchRule.ControlType of
      ctLinkedEdit: lKey := eMatchValue.Key;
      ctIDComboBox: lKey := cmbMatchValue.CurrentStrID;
    end;
    if tblMatch.FieldByName(FN_MATCH_KEY).AsString <> lKey then begin
      // Stops weird moving about after requery.
      LockWindowUpdate(MatchGrid.Handle);
      lBookmark := tblMatch.GetBookmark;
      try
        MatchRule.SetMatch(GetRowImportValue, lKey, cmbChecklists.CurrentStrID);
        btnCommit.Enabled := True;
        tblMatch.Requery;
        // Back to where it was.
        tblMatch.GotoBookmark(lBookmark);
      finally
        tblMatch.FreeBookmark(lBookmark);
        LockWindowUpdate(0);
      end;
    end;
  end;
end;  // TBaseMatch.DoEditableValuesSave

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.DropMatchValue(const Sender: TObject; const format: Integer;
  const sourceData: TKeyList; const textStrings: TStringList; var handled: Boolean);
begin
  if sourceData.Header.ItemCount > 0 then
    if (format = CF_JNCCDATA) and
       SameText(sourceData.ItemTable[0], GetDataSourceTableName) then
    begin
      eMatchValue.Key  := sourceData.Items[0].KeyField1;
      eMatchValue.Text := TrimTags(MatchRule.ConvertKeyToCaption(eMatchValue.Key));
      DoEditableValuesSave;
      handled:= True;
    end else
      handled := False
  else
    handled := True;
  ChangedContent;
end;  // TBaseMatch.DropMatchValue

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.dsMatchTableDataChange(Sender: TObject; Field: TField);
begin
  inherited;
  // Reload after sort, as content will probably be different from previous selection.
  GridEnterCell;
end;  // TBaseMatch.dsMatchTableDataChange

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.eMatchValueFindData(Sender: TObject);
var
  lResult: Boolean;
begin
  inherited;

  if MatchRule.RequiresCheckList then
    lResult := DoCheck(eMatchValue, MatchRule.SearchType, cmbChecklists.CurrentStrID)
  else
    lResult := DoCheck(eMatchValue, MatchRule.SearchType);
  if lResult then begin
    if FMatchRule.IsSpeciesRule then
      try
        dmGeneralData.CheckTaxonAllowsDataEntry(eMatchValue.Key);
      except
        on EGeneralData do begin
          // Returned item in a different list, so we have to reject it
          eMatchValue.Key := '';
          eMatchValue.Text := '';
          raise;
        end;
      end; // try
    eMatchValue.Text := TrimTags(eMatchValue.Text);
    GridLeaveCell;
    FindNextUnmatched;
    ChangedContent;
  end;
end;  // TBaseMatch.eMatchValueFindData

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.eMatchValueGetData(Sender: TObject);
var
  lLinkedEdit : TAddinLinkedEdit;
  lTaxonBrowser: TfrmTaxonDictBrowser;
  lSearchString : String;
  lGrid: TImportWizardDbGrid;
  lBookmark: TBookmark;
  lCursor: TCursor;
begin
inherited;
If SelectedField.FieldName = FN_MATCH_VALUE then begin
  GetDataSourceForm;
  ContainerForm.SetupLink(TBaseForm(Application.MainForm.ActiveMDIChild),
                       ContainerForm, UpdateMatchValueBox);

  // When browsing for a taxon, show the 'find' window upon
  // loading the taxon dictionary
  If (Application.MainForm.ActiveMDIChild is TfrmTaxonDictBrowser) and
      (Sender is TAddinLinkedEdit) then
  begin
    lLinkedEdit := Sender as TAddinLinkedEdit;
    if (lLinkedEdit.Parent is TImportWizardDbGrid) and
        (AppSettings.AutoCompleteSearch) then
    begin
    lGrid := lLinkedEdit.Parent as TImportWizardDbGrid;
      lTaxonBrowser := Application.MainForm.ActiveMDIChild as TfrmTaxonDictBrowser;
      // If text has been entered into the control then set the search text
      // to the entered text
      if Length(lLinkedEdit.text) > 0 then
        lSearchString := lLinkedEdit.Text
      // Otherwise, use the imported data as the search text
      else
      begin
        if lGrid.Columns.Count > 0 then
          lSearchString := lGrid.Columns[0].Field.Value
        else lSearchString := '';
      end;
        lTaxonBrowser.actFindExecute(Sender, lSearchString);
    end;
  end;
end else// end FN_MATCH_VALUE start FN_MATCH_NOTES
  begin
    DisplayProcedure := MatchRule.DisplayNotesProcedure;
    UpdateProcedure :=  MatchRule.UpdateNotesProcedure;
    DetailsProcedure :=  MatchRule.DetailedNotesProcedure;
    InputValue := ematchvalue.Key;
    with TdlgMultipleMatches.Create(nil) do
      try
        if ShowModal = mrOk then begin
          eMatchValue.Visible   := False;
          cmbMatchValue.Visible := False;
          // Stops weird moving about after requery.
          lBookmark := tblMatch.GetBookmark;
          lCursor   := HourglassCursor;
          try
            tblMatch.Requery;
            // Back to where it was.
            tblMatch.GotoBookmark(lBookmark);
          finally
            tblMatch.FreeBookmark(lBookmark);
            DefaultCursor(lCursor);
          end;
          ChangedContent;
          RefreshRow;
        end;
      finally
        Free;
    end;
  end;
end;  // TBaseMatch.eMatchValueGetData

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.eMatchValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lMoveLeftOk, lMoveRightOk: Boolean;
begin
  inherited;
  if FBusyScrolling then Exit;
  with eMatchValue do begin
    // editable, caret at left limit of text, or all text selected
    lMoveLeftOk := (SelStart = 0) or (SelLength = Length(Text));
    // editable, caret at right limit of text, or all text selected
    lMoveRightOk := (SelStart = Length(Text)) or (SelLength = Length(Text));
  end;

  case Key of
    VK_RETURN:
      begin
        if (eMatchValue.Key <> '') then
          MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_LEFT, VK_HOME:
      if lMoveLeftOk then begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
      end;
    VK_RIGHT, VK_END:
      if lMoveRightOk then begin
        // Some text, but no key, see if can automatically find match.
        if (eMatchValue.Text <> '') and (eMatchValue.Key = '') then eMatchValueFindData(nil);
        MatchGrid.Perform(WM_KEYDOWN, Key, 0);
      end;
    VK_ESCAPE:
      begin
        eMatchValue.Text := SelectedField.AsString;
        eMatchValue.Key  := tblMatch.FieldByName(FN_MATCH_KEY).AsString;
      end;
  end;
end;  // TBaseMatch.eMatchValueKeyDown

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.FindNextUnmatched;
begin
  FindNextUnmatched(True);
end;

{-------------------------------------------------------------------------------
Private overload, choose whether to continue searching at the top once you reach
the end.
}
procedure TBaseMatch.FindNextUnmatched (searchPastEof : Boolean);
var
  lCurrent: String;
begin
  with tblMatch do begin
    // Just in case the current one is actually unmatched.
    if FieldByName(FN_MATCH_KEY).IsNull then Exit;

    lCurrent := FieldByName(FN_IMPORT_VALUE).AsString;
    // Stop moving around on the screen while looking for another record.
    DisableControls;
    FScanningDataset := True;
    try
      while not Eof do begin
        if FieldByName(FN_MATCH_KEY).IsNull then Exit;
        Next;
      end;
      // Reached the end without finding an empty one, try from the top.
      if searchPastEof then begin
      First;
      while FieldByName(FN_IMPORT_VALUE). AsString <> lCurrent do begin
        if FieldByName(FN_MATCH_KEY).IsNull then Exit;
        Next;
      end;
      end;
    finally
      FScanningDataset := False;
      EnableControls;
    end;
  end;
end;  // TBaseMatch.FindNextUnmatched

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.GetDataSourceForm;
begin
  case MatchRule.SearchType of
    ST_INDIVIDUAL: dmFormActions.actNames.Execute;
    ST_LOCATION:   dmFormActions.actLocations.Execute;
    ST_BIOTOPE:    begin
                     dmFormActions.actBiotopeDiction.Execute;
                     if Application.MainForm.ActiveMDIChild is TBaseDictionary then
                       with TBaseDictionary(Application.MainForm.ActiveMDIChild) do
                         SelectList(cmbCheckLists.CurrentStrID);
                   end;
    ST_SPECIES:    begin
                     dmFormActions.actTaxonDiction.Execute;
                     if Application.MainForm.ActiveMDIChild is TBaseDictionary then
                       with TBaseDictionary(Application.MainForm.ActiveMDIChild) do
                         SelectList(cmbCheckLists.CurrentStrID);
                   end;
    ST_REFERENCE:  dmFormActions.actDocuments.Execute;
  end;
end;  // TBaseMatch.GetDataSourceForm 

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetDataSourceTableName: String;
begin
  case MatchRule.SearchType of
    ST_INDIVIDUAL: Result := TN_INDIVIDUAL;
    ST_LOCATION:   Result := TN_LOCATION;
    ST_BIOTOPE:    Result := TN_BIOTOPE_LIST_ITEM;
    ST_SPECIES:    Result := TN_TAXON_LIST_ITEM;
    ST_REFERENCE:  Result := TN_REFERENCE;
  end;
end;  // TBaseMatch.GetDataSourceTableName

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetHasNext: Boolean;
var
  lBullets: TStringList;
begin
  lBullets := TStringList.Create;
  try
    with dmDatabase.ExecuteSQL(Format(SQL_CHECK_ALL_MATCHED, [MatchRule.Name]), True) do
      try
        Result := Fields['Total'].Value = 0;
        btnNewEntries.Enabled := not Result;
      finally
        Close;
      end;
      if not Result then
        lBullets.Add(Format(ResStr_AllMatchRequired, [Translate(MatchRule.Name)]))
      else
        lBullets.Add(ResStr_Matching);

      ChangedHtml(lBullets);
  finally
    lBullets.Free;
  end;
end;  // TBaseMatch.GetHasNext

{-------------------------------------------------------------------------------
  A crude translator for the match rule names which are loaded from the database.
}
function TBaseMatch.Translate(const name: string): string;
begin
  if FRuleNameTranslations=nil then begin
    FRuleNameTranslations := TStringList.Create;
    // single quotes used here, as double quotes might cause problems in Localizer Language Manager
    FRuleNameTranslations.QuoteChar:='''';
    FRuleNameTranslations.Delimiter:=',';
    FRuleNameTranslations.DelimitedText := ResStr_RuleNameTranslations;
  end;
  result := FRuleNameTranslations.Values[name];
  if result='' then
    result := name;
end;

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TBaseMatch.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetHTMLFileName: String;
begin
  Result := MatchRule.Name + 'Match.htm';
end;  // TBaseMatch.GetHTMLFileName

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetHTMLImageName: String;
begin
  Result := 'Match.jpg';
end;  // TBaseMatch.GetHTMLImageName

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.GridEnterCell;
begin
  if LoadingPage or FRefreshing or FScanningDataset then Exit;

  // Try to reduce flicker.
  if MatchGrid.HandleAllocated then
    LockWindowUpdate(MatchGrid.Handle);
  try
    // Can't create new entry if match already set.
    btnNew.Enabled := tblMatch.FieldByName(FN_MATCH_KEY).IsNull;
    DoEditableValuesLoad;
    SetInputControl;
  finally
    LockWindowUpdate(0);
  end;
end;  // TBaseMatch.GridEnterCell

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.GridLeaveCell;
begin
  if LoadingPage then Exit;
  FRefreshing := True;
  try
    DoEditableValuesSave;
  finally
    FRefreshing := False;
  end;
  ChangedContent;
end;  // TBaseMatch.GridLeaveCell

{-------------------------------------------------------------------------------
}
function TBaseMatch.GetSkip: Boolean;
begin
  Result := tblMatch.RecordCount = 0;
end;  // TBaseMatch.GetSkip

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.LoadContent;
begin
  inherited;
  // Mantis
  // Locate match rule to use in conjunction with match screen.
  FMatchRule := Settings.ImportFile.MatchRules[Settings.MatchRuleIndex];
  if not Assigned(FMatchRule) then
    raise Exception.Create(ResStr_NoMatchRuleForScreen);

  eMatchValue.Parent   := MatchGrid;
  cmbMatchValue.Parent := MatchGrid;

  tblMatch.Connection := dmDatabase.Connection;
  tblMatch.TableName  := '#' + MatchRule.Name;
  tblMatch.Open;
  tblMatch.Sort := FN_IMPORT_VALUE;

  SetupDisplay;
  if AppSettings.DisableDragDropFrames then
    eMatchValue.DragDestinationColour := clWindowFrame
  else
    eMatchValue.DragDestinationColour := AppSettings.DragDestColour;
  if not MatchRule.RequiresCheckList then btnSearch.Click;
  ChangedContent;

  btnNew.Enabled := tblMatch.FieldByName(FN_MATCH_KEY).IsNull;
end;  // TBaseMatch.LoadContent

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.pmSourceDataClick(Sender: TObject);
begin
  Settings.SelectedMatchValue := tblMatch.FieldByName(FN_IMPORT_VALUE).AsString;
  Settings.BackFromMatchPage := True;
  if Settings.UseOldImportWizard then
    ForceNextPage(TfraColumnTypes)
  else
    ForceNextPage(TfraFastColumnTypes);
end;  // TBaseMatch.pmSourceDataClick

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.RegisterDragDropComponents;
begin
  ContainerForm.RegisterDropComponent(
      eMatchValue,
      DropMatchValue,
      [GetDataSourceTableName],
      [CF_JNCCDATA]);
  ContainerForm.RegisterDropComponentAdvanced(
      dbgMatch,
      DropMatchValueOnGrid,
      [GetDataSourceTableName],
      [CF_JNCCDATA],
      CheckDropMatchValueOnGrid);
end;  // TBaseMatch.RegisterDragDropComponents

{-------------------------------------------------------------------------------
  Override and return False to prevent search.
}
function TBaseMatch.RunSearch: Boolean;
begin
  Result := True;
end;  // TBaseMatch.RunSearch

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.SaveContent;
begin
  inherited;
  tblMatch.Close;
  SaveMatches;
  Settings.MatchPageChecklistKeys.Values[MatchRule.Name] := cmbCheckLists.CurrentStrID;
end;  // TBaseMatch.SaveContent

{-------------------------------------------------------------------------------
}
function TBaseMatch.SetCellColour(const AFieldName: String): Boolean;
begin
  Result := False;
  if AFieldName = FN_MATCH_VALUE then begin
    Result := True;
    if tblMatch.FieldByName(FN_REMEMBERED).AsBoolean then
      with dbgMatch do begin
        Canvas.Brush.Color := MergeColours(Canvas.Brush.Color, clGreen, 90);
        Canvas.Font.Color  := GetContrastColour(Canvas.Brush.Color);
      end;
  end;
end;  // TBaseMatch.SetCellColour 

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.SetInputControl;
var
  lRect: TRect;
  lCtrl: TWinControl;
begin
  eMatchValue.Visible   := False;
  cmbMatchValue.Visible := False;

  if SelectedField = nil then Exit;

  if (SelectedField.FieldName = FN_MATCH_VALUE)
    or  (SelectedField.FieldName = FN_MATCH_NOTES) then
  begin
    lRect := dbgMatch.ActiveCellRect;
    lCtrl := nil;
    case MatchRule.ControlType of
      ctLinkedEdit: lCtrl := eMatchValue;
      ctIDComboBox: lCtrl := cmbMatchValue;
    end;

    if Assigned(lCtrl) then
      with lCtrl do begin
        SetBounds(lRect.Left - 1, lRect.Top - 1,
                  lRect.Right - lRect.Left + 2, lRect.Bottom - lRect.Top);
        Visible := True;
        if CanFocus then SetFocus;
      end;
  end else
  if MatchGrid.CanFocus then
    MatchGrid.SetFocus;
end;  // TBaseMatch.SetInputControl

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.SetupDisplay;
var
  i: Integer;
  lDefaultListKey: String;

  procedure SetField(const AFieldName: String; AReadOnly: Boolean = True);
  var
    lField: TField;
  begin
    lField := tblMatch.FindField(AFieldName);
    if Assigned(lField) then begin
      lField.ReadOnly := AReadOnly;
      lField.Visible  := True;

      // Find out if need a checklist/classification key, if first time around.
      if lDefaultListKey = '' then
        if AFieldName = FN_CHECKLIST then
          lDefaultListKey := AppSettings.TaxonListKey
        else
        if AFieldName = FN_CLASSIFICATION then
          lDefaultListKey := AppSettings.BiotopeListKey;
    end;
  end;

begin
  lDefaultListKey := Settings.MatchPageChecklistKeys.Values[MatchRule.Name];
  dbgMatch.Columns[0].ReadOnly := True;
  with tblMatch do begin
    // Size fields to reasonable width to start with. Hide all, then show required only.
    for i := 0 to Fields.Count - 1 do begin
      if Fields[i].DisplayWidth > 30 then Fields[i].DisplayWidth := 30;
      Fields[i].Visible := False;
    end;

    with FieldByName(FN_IMPORT_VALUE) do begin
      DisplayLabel := ResStr_ImportedData;
      ReadOnly     := True;
      Visible      := True;
    end;
    FieldByName(FN_MATCH_VALUE).DisplayLabel := ResStr_RecorderMatches;
    FieldByName(FN_MATCH_VALUE).Visible      := True;

    // Additional that are known but not in every table, but still need to be visible.
    SetField(FN_TAXON_ORDER);
    SetField(FN_MATCH_NOTES);
    SetField(FN_CHECKLIST);
    SetField(FN_CLASSIFICATION);
  end;

  with MatchRule do begin
    pnlBtnNew.Visible       := NewEntryProcedure <> '';
    pnlBtnSearch.Visible := RequiresCheckList;
    pnlCheckList.Visible := RequiresCheckList;
    pnlBtnExcludeUnmatched.Visible := (ExcludeUnmatchedProcedure <> '') and (not Settings.UseOldImportWizard);
  end;

  // Hide anything not applicable to the "old" wizard.
  pnlBtnNewEntries.Visible := not Settings.UseOldImportWizard and pnlBtnNew.Visible;
  pnlBtnCommit.Visible     := not Settings.UseOldImportWizard;
  btnCommit.Enabled     := False;

  pnlControls.Visible := pnlBtnSearch.Visible or pnlBtnNew.Visible;

  cmbCheckLists.Clear;
  if pnlBtnSearch.Visible then begin
    cmbCheckLists.PopulateContent;
    if AppSettings.UsePreferredTaxa then
      cmbCheckLists.ItemIndex := 0
    else
      cmbCheckLists.ItemIndex := cmbCheckLists.IDIndexOf(lDefaultListKey);
    btnSearch.Enabled       := cmbCheckLists.ItemIndex <> -1;
    btnSearch.Visible       := true; // can't see why this is necessary, but it is
  end;

  if MatchRule.ControlType = ctIDComboBox then begin
    cmbMatchValue.Clear;
    cmbMatchValue.PopulateContent;
  end;
end;  // TBaseMatch.SetupDisplay

{-------------------------------------------------------------------------------
  Remove HTML tags from string to display in edit box.
}
function TBaseMatch.TrimTags(const AString: String): String;
begin
  Result := StringReplace(AString, '<i>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '</i>', '', [rfReplaceAll]);
end;  // TBaseMatch.TrimTags

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.UnregisterDragDropComponents;
begin
  ContainerForm.UnRegisterDragDropComponents([eMatchValue, dbgMatch]);
end;  // TBaseMatch.UnregisterDragDropComponents

{-------------------------------------------------------------------------------
}
procedure TBaseMatch.UpdateMatchValueBox(KeyList: TKeyList);
begin
  try
    if Assigned(KeyList) then begin
      if (KeyList.Header.ItemCount > 0) and
         SameText(KeyList.ItemTable[0], GetDataSourceTableName) then
      begin
        if FMatchRule.IsSpeciesRule then
          dmGeneralData.CheckTaxonAllowsDataEntry(KeyList.Items[0].KeyField1);
        eMatchValue.Key  := KeyList.Items[0].KeyField1;
        eMatchValue.Text := TrimTags(MatchRule.ConvertKeyToCaption(eMatchValue.Key));
        DoEditableValuesSave;
      end;
    end;
  finally
    KeyList.Free;
  end;
  ChangedContent;
end;  // TBaseMatch.UpdateMatchValueBox

{-------------------------------------------------------------------------------
  Retrieive the current row's import value.  Virtual as read from a string
    grid on the locations match screen
}
function TBaseMatch.GetRowImportValue: string;
begin
  Result := tblMatch.FieldByName(FN_IMPORT_VALUE).AsString;
end;

{-------------------------------------------------------------------------------
  Give subclasses the opportunity to do any special behaviour when the content
     of a row is refreshed
}
procedure TBaseMatch.RefreshRow;
begin
end;  // TBaseMatch.RefreshRow;

{-------------------------------------------------------------------------------
  Returns the grid used for matching.  Overriden where matching is performed
    using a string grid.
}
function TBaseMatch.GetMatchGrid: TCustomGrid;
begin
  Result := dbgMatch;
end;  // TBaseMatch.GetMatchGrid

{-------------------------------------------------------------------------------
  Retrieve the grid's selected field.  Allow subclasses to override this.
}
function TBaseMatch.SelectedField: TField;
begin
  Result := dbgMatch.SelectedField;
end;  // TBaseMatch.SelectedField
  
{-------------------------------------------------------------------------------
  Check if there are any duplicates and let the user decide what to do with them.
}
procedure TBaseMatch.CheckDuplicates;
var
  duplicates: TStringList;
  i, j, displaycount: Integer;
  msg: string;
begin
  if Settings.MatchRuleIndex < 0 then
    Exit;

    // We currently only check if names are duplicated.
    if MatchRule.Key = MR_KEY_NAMES then begin
      duplicates := MatchRule.CheckDuplicates;
      if duplicates.Count > 0 then begin
        displaycount := Min(10, duplicates.Count);
        msg := Format(ResStr_DuplicatesMessageStart, [IntToStr(duplicates.Count)]);
        if duplicates.Count > 10 then
          // The message will only list ten duplicates at most.
          // Let the user know if there are more.
          msg := msg + ResStr_MoreThanTen;
        msg := msg + #13#10 + #13#10;
        for i := 0 to displaycount - 1 do begin
          // For each set of duplicates say *set* matches *person* in *this record*
          for j := 0 to TStringList(duplicates.Objects[i]).Count - 1 do begin
            msg := msg + TStringList(duplicates.Objects[i]).Strings[j];
            if j < TStringList(duplicates.Objects[i]).Count - 2 then
              msg := msg + ', '
            else if j = TStringList(duplicates.Objects[i]).Count - 2 then
              msg := msg + ResStr_And;
          end;
          msg := msg + Format(ResStr_LinkedToPerson, [MatchRule.ConvertKeyToCaption(
              duplicates.Names[i]), duplicates.ValueFromIndex[i]]);
        end;
        // Show the message, and explain what OK and Cancel do.
        msg := msg + ResStr_DuplicatesMessageEnd;
        if MessageDlg(msg, mtWarning, mbOkCancel, 0) = mrCancel then
          Abort;
      end;
      for i := 0 to duplicates.Count - 1 do
        duplicates.Objects[i].Free;
      duplicates.Free;
    end;
end;  // TBaseMatch.CheckDuplicates

{-------------------------------------------------------------------------------
  Check whether a data item can be dropped on the grid.
}
procedure TBaseMatch.CheckDropMatchValueOnGrid(APoint: TPoint;
  const ATable, AFieldKey: String; var Accept: boolean);
var
  GridPoint: TPoint;
begin
  GridPoint := dbgMatch.ScreenToClient(APoint);
  FDropCell := dbgMatch.MouseCoord(GridPoint.X, GridPoint.Y);
  Accept := (FDropCell.Y > 0)
          and (AFieldKey <> '')
          and (MatchRule.ControlType = ctLinkedEdit)
          and SameText(ATable, GetDataSourceTableName);
end;  // TBaseMatch.CheckDropMatchValueOnGrid

{-------------------------------------------------------------------------------
  Data item dropped on the grid.
}
procedure TBaseMatch.DropMatchValueOnGrid(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TStringList; const iIsPasteOperation: boolean;
  var ioHandled: boolean);
var
  lCurrentField: TField;
begin
  if (iFormat = CF_JNCCDATA)
     and (iSourceData.Header.ItemCount > 0)
     and (MatchRule.ControlType = ctLinkedEdit)
     and SameText(iSourceData.ItemTable[0], GetDataSourceTableName) then
  begin
    // Select correct cell to receive new data item, save it, then reset column
    lCurrentField          := dbgMatch.SelectedField;
    dbgMatch.SelectedField := dbgMatch.Fields[GENERIC_MATCH_COL];
    tblMatch.RecNo         := FDropCell.Y;
    eMatchValue.Key        := iSourceData.Items[0].KeyField1;
    eMatchValue.Text       := TrimTags(MatchRule.ConvertKeyToCaption(eMatchValue.Key));
    DoEditableValuesSave;
    dbgMatch.SelectedField := lCurrentField;
    ioHandled              := True;
  end;    // if (iFormat = CF_JNCCDATA) and (iSourceData.Header.ItemCount > 0)
end;  // TBaseMatch.DropMatchValueOnGrid

{-------------------------------------------------------------------------------
  Cancels the match.
}
procedure TBaseMatch.Cancel;
begin
  inherited;
  MatchRule.Cancel;
end;  // TBaseMatch.Cancel

{-------------------------------------------------------------------------------
  Forces a save of entries that have been matched so far to the DB.
}
procedure TBaseMatch.SaveMatches;
begin
  MatchRule.RecordMatches(cmbCheckLists.CurrentStrID);
  btnCommit.Enabled := False;
end;  // TBaseMatch.SaveMatches

{-------------------------------------------------------------------------------
  Calls GridLeaveCell
}
procedure TBaseMatch.eMatchValueExit(Sender: TObject);
begin
  inherited;
  GridLeaveCell;
end;  // TBaseMatch.eMatchValueExit

{-------------------------------------------------------------------------------
  Removes unmatched rows from the import, writes the row details to a file.
}
procedure TBaseMatch.btnExcludeUnmatchedClick(Sender: TObject);
var
  Output : TextFile;
  I, J : Integer;
  lImportValue : String;
  lSpeciesColumnTitle : String;
  lIsSavedToFile : Boolean;
begin
  lIsSavedToFile := False;
  dlgSaveUnmatched.InitialDir := GetProgramDataFolder(PATH_USER_FILES);
  if ConfirmYesNoDefaultNo(Format(ResStr_ExcludeUnmatched,[Translate(MatchRule.Name)])) = mrYes then begin
    if dlgSaveUnmatched.Execute then
      lIsSavedToFile := True;

    if lIsSavedToFile then begin
      AssignFile(Output, dlgSaveUnmatched.FileName);
      Rewrite(Output);
    end;

    with Settings.ImportedData do begin
      DisableControls;
      try
        First;
        tblMatch.First;
        for I := 1 to FieldCount - 1 do begin
          if lIsSavedToFile then begin
            if I = 1 then
              Write(Output, AnsiQuotedStr(Fields[I].DisplayName, '"'))
            else
              Write(Output, ',', AnsiQuotedStr(Fields[I].DisplayName, '"'));
          end;
          with Settings.ImportFile do begin
            if (ColumnMapping.MappedType(Fields[I].FullName) <> nil) then
              if (ColumnMapping.MappedType(Fields[I].FullName).Key = CT_KEY_SPECIES) then
                lSpeciesColumnTitle := Fields[I].FullName;
          end;
        end;

        if lIsSavedToFile then
          WriteLn(Output);
        while not tblMatch.Eof do begin
          FindNextUnmatched(False);
          if not tblMatch.Eof then begin
            lImportValue := tblMatch.FieldByName(FN_IMPORT_VALUE).AsString;
            tblMatch.Next;
            while Locate(lSpeciesColumnTitle, lImportValue, []) do begin
              for J := 1 to FieldCount - 1 do begin
                if lIsSavedToFile then begin
                  if J = 1 then
                    Write(Output, AnsiQuotedStr(Fields[J].AsString, '"'))
                  else
                    Write(Output, ',', AnsiQuotedStr(Fields[J].AsString, '"'));
                end;
              end;
              if lIsSavedToFile then
                WriteLn(Output);
              Delete;
            end;
          end;
        end;
      Settings.RecordCount := Settings.RecordCount - MatchRule.RemoveUnmatched;
      finally
        EnableControls;
        if lIsSavedToFile then
          CloseFile(Output);
      end; // try
    end; // with
    tblMatch.Requery;
    ChangedContent;
  end;
end;

end.
