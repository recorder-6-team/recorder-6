{===============================================================================
  Unit:        ImportWizard

  Defines:     TfrmImportWizard

  Description: Main form for Import Wizard (IW) addin.

  Model:       ImportWizard

  Last revision information:
    $Revision: 45 $
    $Date: 4/08/09 10:29 $
    $Author: Ericsalmon $

===============================================================================}

unit ImportWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, DssStringGrid, ImgList, BaseChildUnit,
  ImageListButton, IWSettings, IWBasePage, Htmlview, Buttons, XPMenu, IWConstants,
  BaseFormUnit, BaseMatchPage, Menus, Constants, StrUtils;

resourcestring
  ResStr_ErrorsIdentified = 'The following issues must be resolved before you can ' +
      'proceed to the next step in the Import Wizard:';
  ResStr_Records = '%d records';
  ResStr_TemplateOverwrite = 'The template, %s, already exists.'#13#10 +
      'Do you wish to overwrite it?';
  ResStr_DefaultTemplateName = 'MyTemplate';
  ResStr_PleaseEnterTemplateName = 'Please enter a template name.';
  ResStr_PageClassNotSpecified =  'Page class not specified.';
  ResStr_InvalidData =  'Invalid Data'#13'Content: %s';
  ResStr_EnterTemplateName = 'Enter the name of the template';
  ResStr_SaveTemplate = 'Save Template';

type
  TDirection = (dirNone, dirForward, dirReverse);

  {-----------------------------------------------------------------------------
    Import wizard main form.  This contains controls common to all steps in the import
    wizard, and embeds a frame onto itself to show the controls that are specific to each
    page.  The frame is replaced with a new one as each page in the wizard is displayed.
  }
  TfrmImportWizard = class(TBaseChild)
    btnCancel: TBitBtn;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    btnReturn: TBitBtn;
    btnSaveTemplate: TBitBtn;
    hvHelp: THTMLViewer;
    pnlButtons: TPanel;
    pnlButtons2: TPanel;
    pnlData: TPanel;
    pnlPages: TPanel;
    StatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnSaveTemplateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReturnClick(Sender: TObject);
    procedure hvHelpHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
  private
    FCallingMatchPageClass: TClass;
    FClosingForm: Boolean;
    FCurrentPage: TBasePage;
    FSettings: TdmIWSettings;
    FXPMenu: TXPMenu;
    FLoadingTemplatePages: boolean;
    FIsSkip:boolean;
    procedure ChangePage(const APageClass: TBasePageClass; ADirection: TDirection);
    procedure PageContentChanged(Sender: TObject);
    procedure PageHTMLChanged(Sender: TObject; AErrors: TStrings);
    procedure PageDoValidateContent(Sender: TObject; var DoValidation: Boolean);
    procedure PageForcedNextPage(Sender: TObject; APageClass: TBasePageClass);
    procedure SynchronizeNavigationButtons;
    procedure UMCancelImport(var Msg: TMessage); message UM_CANCEL_IMPORT;
    procedure UMChangePage(var Msg: TMessage); message UM_CHANGEPAGE;
    procedure UMReturn(var Msg: TMessage); message UM_RETURN;
    procedure WMRefreshColours(var Msg: TMessage); message WM_REFRESH_COLOURS;
    procedure WMRefreshSRefSystem(var Msg: TMessage); message WM_REFRESH_SPATIAL_REF_SYSTEM;
    procedure WMRefreshTermLists(var Msg: TMessage); message WM_REFRESH_TERM_LISTS;
    procedure WMTransferDone(var Msg:TMessage); message WM_TRANSFER_DONE;
    procedure WMRefreshRecordCount(var Msg:TMessage); message WM_REFRESH_RECORD_COUNT;
    procedure SetLoadingTemplatePages(const Value: boolean);
  protected
    procedure DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
        override;
    property LoadingTemplatePages: boolean read FLoadingTemplatePages write SetLoadingTemplatePages;
  public
    constructor Create(AOwner: TComponent; const AExt, AAdditional: String); reintroduce;
        overload;
    destructor Destroy; override;
    procedure RefreshXPMenu; override;
    procedure SetFileExtension(const AExt: String);
    procedure UpdateMapWindowSelector; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FileSelect, ColumnTypes, Import, GeneralFunctions, IWResourceStrings, ApplicationSettings,
  DatabaseAccessADO, ImportAnalysis, MatchLocations, OnlineHelp, MarkupDocs;

{-==============================================================================
    TfrmImportWizard
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmImportWizard.Create(AOwner: TComponent; const AExt, AAdditional: String);
begin
  inherited Create(AOwner);
  FSettings := TdmIWSettings.Create(Self);
  SetFileExtension(AExt);
  FSettings.SourceDataFile := AAdditional;
  StatusBar.Panels[1].Text := FSettings.SourceDataFile;

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := [xcPopupMenu,  xcCheckBox, xcRadioButton, xcButton, xcBitBtn,
                         xcGroupBox, xcRichEdit];
  FXPMenu.Active := True;
  FLoadingTemplatePages := false;
  FIsSkip:=false;
  btnSaveTemplate.Visible := not (FSettings.ImportType in [itNBNData, itZippedAccess, itAddin]);
  PostMessage(Handle, UM_CHANGEPAGE, 0, 0);

  // BaseChild property, so you can't do batch updates while the Import Wizard is open.
  FEditMode := emEdit;

  HelpContext := IDH_IMPORTWIZARD;
end;  // TfrmImportWizard.Create

{-------------------------------------------------------------------------------
}
destructor TfrmImportWizard.Destroy;
begin
  if Assigned(FCurrentPage) then begin
    FCurrentPage.UnRegisterDragDropComponents;
    FCurrentPage.Free;
    FCurrentPage := nil;
  end;
  
  FSettings.Free;
  FXPMenu.Active := False;
  FXPMenu.Free;
  inherited;
end;  // TfrmImportWizard.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnCancelClick(Sender: TObject);
begin
  FXPMenu.Active := False;
  Application.ProcessMessages;
  Close;
end;  // TfrmImportWizard.btnCancelClick

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnNextClick(Sender: TObject);
var
  lContinue: Boolean;
begin
  // Back from Match page, but now want to go forward again, so reset a couple of flags.
  if FSettings.BackFromMatchPage then begin
    FSettings.BackFromMatchPage := False;
    FSettings.MatchRuleIndex := -1;
  end;

  if FCurrentPage.IsFinal then begin
    FCurrentPage.SaveContent;
    Close;
  end
  else begin
    lContinue := True;
    // If it's a match page, check for duplicates. If the user cancels the
    // duplicates warning, go no further.
    if FCurrentPage is TBaseMatch then
      TBaseMatch(FCurrentPage).CheckDuplicates;
    if lContinue then begin
      FCurrentPage.ValidateContent;
      if Assigned(FCurrentPage) then begin
        if (FCurrentPage is TBaseMatch) or FCurrentPage.Next.InheritsFrom(TBaseMatch) then
          FSettings.MatchRuleIndex := FSettings.MatchRuleIndex + 1;
        ChangePage(FCurrentPage.Next, dirForward);
      end;
    end;
  end;
end;  // TfrmImportWizard.btnNextClick

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnPrevClick(Sender: TObject);
begin
  // Back from Match page, but now want to go back further, so reset a couple of flags.
  if FSettings.BackFromMatchPage then begin
    FSettings.BackFromMatchPage := False;
    FSettings.MatchRuleIndex := -1;
  end;

  if (FCurrentPage is TBaseMatch) or FCurrentPage.Previous.InheritsFrom(TBaseMatch) then
    FSettings.MatchRuleIndex := FSettings.MatchRuleIndex - 1;
  ChangePage(FCurrentPage.Previous, dirReverse);
end;  // TfrmImportWizard.btnPrevClick

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnReturnClick(Sender: TObject);
begin
  inherited;
  if FCallingMatchPageClass <> nil then begin
    FSettings.BackFromMatchPage := False;
    ChangePage(TBasePageClass(FCallingMatchPageClass), dirNone);
  end;
end;  // TfrmImportWizard.btnReturnClick

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnSaveTemplateClick(Sender: TObject);
var
  lTemplateName, lTemplateFullPath: String;
  lReloop: Boolean;
begin
  lReloop := true;
  while lReloop do begin
    lReloop := false;
    lTemplateName := ResStr_DefaultTemplateName;
    if not InputQuery(ResStr_SaveTemplate, ResStr_EnterTemplateName+':',
        lTemplateName) then
      Exit; // do not save
    if lTemplateName = '' then begin
      ShowInformation(ResStr_PleaseEnterTemplateName);
      lReloop := True;
      Continue;
    end;
    lTemplateFullPath := AppSettings.ImportTemplatePath + lTemplateName;
    if CompareText(ExtractFileExt(lTemplateFullPath), '.xml') <> 0 then
      lTemplateFullPath := lTemplateFullPath + '.xml';

    // If file exists, confirm overwrite.
    if FileExists(lTemplateFullPath) then
      case MessageDlg(Format(Resstr_TemplateOverwrite, [lTemplateName]),
                      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrCancel: Exit;  // cancelled do not save and exit loop
        mrNo:     lReloop := true;  // no to save - allow user to try again
      end;
  end;

  //save whatever is on current page to the setting class
  FCurrentPage.SaveCurrentSettings;
  FSettings.SaveSettingsToFile(lTemplateFullPath);
  FCurrentPage.RefreshContent;
end;  // TfrmImportWizard.btnSaveTemplateClick

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.ChangePage(const APageClass: TBasePageClass; ADirection: TDirection);
var
  lCursor: TCursor;
  lOrigPage: TBasePage;
  lGuid: TGUID;

begin
  lCursor := HourglassCursor;
  try
    if (FCurrentPage is TfraFileSelect) then
      if TfraFileSelect(FCurrentPage).cmbTemplate.ItemIndex>0 then
        // when loading a template, need to skip pages that load successfully
        // with no errors
        LoadingTemplatePages := True;
    // Stop potential double-clicks from users.
    btnPrev.Enabled := False;
    btnNext.Enabled := False;

    if APageClass = nil then
      raise Exception.Create(ResStr_PageClassNotSpecified);

    btnReturn.Visible := False;
    lOrigPage := nil;
    if Assigned(FCurrentPage) then begin
      if not FIsSkip then
        FCurrentPage.UnRegisterDragDropComponents; {ignored for skip page call}
      if not FCurrentPage.Cancelled then
        FCurrentPage.SaveContent;
      lOrigPage := FCurrentPage;
      CreateGuid(lGuid);
      // ensure the old page has a unique component name so it doesn't clash with the
      // new frame
      lOrigPage.Name := 'x' + StringReplace(
          StringReplace(
              StringReplace(GuidToString(lGuid), '{', '', [rfReplaceAll]),
              '-', '', [rfReplaceAll]),
          '}', '', [rfReplaceAll]);
  
    end;
    // If page wants to be skipped, keep going in the same direction until there's one
    // that displays.
    FCurrentPage := APageClass.Create(Self, FSettings);

    FreeAndNil(lOrigPage);
    if FCurrentPage.Skip and (ADirection <> dirNone) then begin
      FIsSkip:= true;
      case ADirection of
        dirForward: btnNext.Click;
        dirReverse: btnPrev.Click;
      end;
      Exit;
    end;

    // Now proceed with the created frame.
    with FCurrentPage do begin
      // Setup frame's container, if it needs some special re-alignment.
      // Now set put the frame on the panel and set other properties.
      Parent := pnlPages;
      pnlPages.BorderWidth := BorderWidth;
      Align := alClient;
      OnDoValidateContent := PageDoValidateContent;
      OnChangedContent    := PageContentChanged;
      OnChangedHTML       := PageHTMLChanged;
      OnForcedNextPage    := PageForcedNextPage;
      TabOrder := 0;
    end;
    PageHTMLChanged(FCurrentPage, nil);

    SynchronizeNavigationButtons;

    FXPMenu.InitComponent(Self);
    Refresh;
    FCurrentPage.RegisterDragDropComponents;

    try
      if FCurrentPage.Execute then
        ChangePage(FCurrentPage.Next, dirForward)
      else
      if Assigned(btnNext) and LoadingTemplatePages then begin
        // loading a template - wind on through the wizard until we hit the
        // first page that needs user input
        if btnNext.Enabled then begin
          FCurrentPage.ValidateContent;
          if (FCurrentPage is TBaseMatch) or FCurrentPage.Next.InheritsFrom(TBaseMatch) then
            FSettings.MatchRuleIndex := FSettings.MatchRuleIndex + 1;
          ChangePage(FCurrentPage.Next, dirForward);
        end else
          LoadingTemplatePages := False;
      end;
    except
      on E:EImportCantProceed do begin
        PostMessage(Handle, WM_CLOSE, 0, 0);
        // markup exceptions have already been shown
        if not (E is TMarkupException) then
          raise;
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // TfrmImportWizard.ChangePage

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  lCol, lRow: Integer;
begin
  if (HintInfo.HintControl is TDssStringGrid) then
    with TStringGrid(HintInfo.HintControl) do begin
      HintStr := Hint;
      MouseToCell(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, lCol, lRow);
      if (lCol = 3) and (lRow in [2, 8, 9, 12]) then begin
        HintStr := Format(ResStr_InvalidData, [Cells[lCol, lRow]]);
        HintInfo.ReshowTimeout := 400;
        CanShow := True;
      end else
        Hint := '';
    end
  else
    inherited;
end;  // TfrmImportWizard.DoShowHint 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.FormActivate(Sender: TObject);
begin
  inherited;
  if Assigned(FCurrentPage) then FCurrentPage.RefreshContent;
end;  // TfrmImportWizard.FormActivate 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FClosingForm := True;
  if Assigned(FCurrentPage) then FCurrentPage.Cancel;
  Action := caFree;
end;  // TfrmImportWizard.FormClose 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.PageContentChanged(Sender: TObject);
begin
  SynchronizeNavigationButtons;
end;  // TfrmImportWizard.PageContentChanged 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.PageDoValidateContent(Sender: TObject; var DoValidation: Boolean);
var
  lPos: TPoint;
begin
  lPos := btnCancel.ScreenToClient(Mouse.CursorPos);
  DoValidation := not (((lPos.X in [0..btnCancel.Width]) and
                        (lPos.Y in [0..btnCancel.Height])) or
                       FClosingForm);
end;  // TfrmImportWizard.PageDoValidateContent

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.PageForcedNextPage(Sender: TObject; APageClass: TBasePageClass);
begin
  // From a match page back to Column Types page through popup menu.
  if (APageClass = TfraColumnTypes) and FCurrentPage.InheritsFrom(TBaseMatch) then
    FCallingMatchPageClass := FCurrentPage.ClassType
  else
    FCallingMatchPageClass := nil;

  ChangePage(APageClass, dirNone);
end;  // TfrmImportWizard.PageForcedNextPage

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.RefreshXPMenu;
begin
  FXPMenu.InitComponent(Self);
end;  // TfrmImportWizard.RefreshXPMenu 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.SetFileExtension(const AExt: String);
begin
  with FSettings do
    if AExt = '.xml' then ImportType := itNBNData else
    if AExt = '.zip' then ImportType := itZippedAccess else
    if AExt = '.txt' then ImportType := itText else
    if AExt = '.csv' then ImportType := itCSV else
    if (AExt = '.xls') or (AExt = '.xlsx') then ImportType := itExcel else
    if AExt = '.dbf' then ImportType := itDBase else
    if AExt = '.db'  then ImportType := itParadox else
    if AExt = '.wk1' then ImportType := itLotus else
    if AExt = '.wq1' then ImportType := itQuattro else
    if AExt = 'ADO'  then ImportType := itADO
    else
      ImportType := itAddin;
end;  // TfrmImportWizard.SetFileExtension

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.SynchronizeNavigationButtons;
begin
  btnNext.Caption := FCurrentPage.NextCaption;  // Can be anything, "Next", "Import"...
  btnNext.Enabled := FCurrentPage.HasNext;
  btnPrev.Enabled := FCurrentPage.HasPrevious;

  if btnNext.Caption <> ResStr_Next then btnNext.Spacing := 10;
end;  // TfrmImportWizard.SynchronizeNavigationButtons

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UMCancelImport(var Msg: TMessage);
begin
  btnCancelClick(nil);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UMChangePage(var Msg: TMessage);
begin
  if FSettings.ImportType in [itNBNData, itZippedAccess, itAddin] then
    ChangePage(TfraImport, dirNone)
  else
    ChangePage(TfraFileSelect, dirNone);
end;  // TfrmImportWizard.UMChangePage

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UMReturn(var Msg: TMessage);
begin
  btnReturn.Visible := Msg.WParam = 1;
  btnReturn.Enabled := Msg.LParam = 1;
  // Something changed to disable the option. Reset a few flags then.
  if not btnReturn.Enabled then begin
    FSettings.BackFromMatchPage := False;
    FSettings.MatchRuleIndex := -1;
    FCallingMatchPageClass := nil;
  end;
end;  // TfrmImportWizard.UMReturn

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UpdateMapWindowSelector;
begin
  inherited;
  if Assigned(FCurrentPage) then FCurrentPage.UpdateMapWindowSelector;
end;  // TfrmImportWizard.UpdateMapWindowSelector 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.WMRefreshColours(var Msg: TMessage);
begin
  if Assigned(FCurrentPage) then FCurrentPage.RefreshContent;
end;  // TfrmImportWizard.WMRefreshColours

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.WMRefreshSRefSystem(var Msg: TMessage);
begin
  if Assigned(FCurrentPage) then FCurrentPage.RefreshContent;
end;  // TfrmImportWizard.WMRefreshSRefSystem

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.WMRefreshTermLists(var Msg: TMessage);
begin
  if Assigned(FCurrentPage) then FCurrentPage.RefreshTermLists;
end;  // TfrmImportWizard.WMRefreshTermLists

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.WMTransferDone(var Msg:TMessage);
begin
  Show;
end;  // TfrmImportWizard.WMTransferDone

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.PageHTMLChanged(Sender: TObject; AErrors: TStrings);
var
  lHtml: TStringList;
  i: integer;
  lPage: TBasePage;
const
  HTML_HEADER = '<HTML><BODY Background="%s" style="background-repeat:no-repeat"><BR><BR><BR><BR><BR><BR><BR>';
begin
  lHtml := TStringList.Create;
  try
    lPage := TBasePage(Sender);
    if FileExists(ExtractFilePath(AppSettings.HelpPath) + lPage.HtmlFileName) then
      lHtml.LoadFromFile(ExtractFilePath(AppSettings.HelpPath) + lPage.HtmlFileName);
    lHtml.Insert(0, Format(HTML_HEADER,
                    [ExtractFilePath(Application.ExeName) + 'Images\' + lPage.HtmlImageName]));
    if Assigned(AErrors) then
      if AErrors.Count>0 then begin
        lHtml.Add('<p>' + ResStr_ErrorsIdentified + '</p>');
        lHtml.Add('<ul>');
        for i := 0 to AErrors.Count-1 do
          lHtml.Add('<li>' + AErrors[i] + '</li>');
        lHtml.Add('</ul>');
      end;
    hvHelp.LoadStrings(lHtml);
  finally
    lHtml.Free;
  end; // try
end;

{-------------------------------------------------------------------------------
  Update the status bar to reflect the record count
}
procedure TfrmImportWizard.WMRefreshRecordCount(var Msg: TMessage);
begin
  StatusBar.Panels[0].Text := Format(ResStr_Records, [FSettings.RecordCount]);
end;  // TfrmImportWizard.WMRefreshRecordCount

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.SetLoadingTemplatePages(const Value: boolean);
begin
  if FLoadingTemplatePages <> Value then begin
    FLoadingTemplatePages := Value;
    // If true, then need to lock window updates till we finish
    if Value then
      LockWindowUpdate(Handle)
    else
      LockWindowUpdate(0);
  end;
end;

{-------------------------------------------------------------------------------
  Any hyperlink in the HTML text which contains just a number is used as the
     context link number to the main help file.
}
procedure TfrmImportWizard.hvHelpHotSpotClick(Sender: TObject;
  const SRC: String; var Handled: Boolean);
begin
  inherited;
  if IsInt(SRC) then begin
    OHelp.ShowContext(StrToInt(SRC));
    Handled := true;
  end;
end;
end.
