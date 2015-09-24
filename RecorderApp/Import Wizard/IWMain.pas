{===============================================================================
  Unit:        IWMain

  Defines:     TfrmMain

  Description: Main form for Import Wizard (IW) addin.

  Model:

  Created:     April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/04 14:13 $
    $Author: Ericsalmon $

===============================================================================}

unit IWMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, DssStringGrid, ImgList,
  ImageListButton, IWSettings, IWBasePage, Htmlview, Buttons, XPMenu, IWConstants;

type
  {-----------------------------------------------------------------------------
    Import wizard main form.  This contains controls common to all steps in the import wizard, 
    and embeds a frame onto itself to show the controls that are specific to each page.  The 
    frame is replaced with a new one as each page in the wizard is displayed.
  }
  TfrmImportWizard = class (TForm)
    btnCancel: TBitBtn;
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    btnReturn: TBitBtn;
    btnSaveTemplate: TBitBtn;
    hvHelp: THTMLViewer;
    pnlButtons: TPanel;
    pnlData: TPanel;
    pnlPages: TPanel;
    StatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnSaveTemplateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCurrentPage: TBasePage;
    FOriginalAppShowHint: TShowHintEvent;
    FSettings: TdmIWSettings;
    FXPMenu: TXPMenu;
    procedure ApplicationShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: 
        THintInfo);
    procedure ChangePage(const APageClass: TBasePageClass);
    procedure PageContentChanged(Sender: TObject);
    procedure PageForcedNextPage(Sender: TObject; APageClass: TBasePageClass);
    procedure SynchronizeNavigationButtons;
    procedure UMChangePage(var Msg: TMessage); message UM_CHANGEPAGE;
    procedure UMReturn(var Msg: TMessage); message UM_RETURN;
  public
    constructor Create(AOwner: TComponent; const AExt, AAdditional: String); reintroduce; 
        overload;
    destructor Destroy; override;
    procedure SetFileExtension(const AExt: String);
    property XPMenu: TXPMenu read FXPMenu write FXPMenu;
  end;
  
var
  frmImportWizard: TfrmImportWizard;

//==============================================================================
implementation

{$R *.dfm}

uses
  FileSelect, GeneralFunctions, IWResourceStrings, MissingData;

{-==============================================================================
    TfrmImportWizard
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmImportWizard.Create(AOwner: TComponent; const AExt, AAdditional: String);
begin
  inherited Create(AOwner);
  FOriginalAppShowHint := Application.OnShowHint;
  Application.OnShowHint := ApplicationShowHint;
  
  FSettings := TdmIWSettings.Create(nil);
  SetFileExtension(AExt);
  FSettings.SourceDataFile := AAdditional;

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.Active := True;
  PostMessage(Handle, UM_CHANGEPAGE, 0, 0);
end;  // TfrmImportWizard.Create 

{-------------------------------------------------------------------------------
}
destructor TfrmImportWizard.Destroy;
begin
  Application.OnShowHint := FOriginalAppShowHint;
  FSettings.Free;
  inherited;
end;  // TfrmImportWizard.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.ApplicationShowHint(var HintStr: String; var CanShow: Boolean; var 
    HintInfo: THintInfo);
var
  lCol, lRow: Integer;
begin
  if (HintInfo.HintControl is TStringGrid) then
    with TStringGrid(HintInfo.HintControl) do begin
      HintStr := Hint;
      MouseToCell(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, lCol, lRow);
      if (lCol = 3) and (lRow in [2, 8, 9, 12]) then begin
        HintStr := Format('Invalid Data'#13'Content: %s', [Cells[lCol, lRow]]);
        HintInfo.ReshowTimeout := 400;
        CanShow := True;
      end else
        Hint := '';
    end
  else begin
    // Show hint as normal
    HintInfo.ReshowTimeout := 400;
    HintStr := HintInfo.HintControl.Hint;
    CanShow := True;
  end;
end;  // TfrmImportWizard.ApplicationShowHint 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnCancelClick(Sender: TObject);
begin
  FCurrentPage.Cancel;
  Close;
end;  // TfrmImportWizard.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnNextClick(Sender: TObject);
begin
  if FCurrentPage.IsFinal then begin
    Close;
  end else
    ChangePage(FCurrentPage.Next);
end;  // TfrmImportWizard.btnNextClick 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnPrevClick(Sender: TObject);
begin
  ChangePage(FCurrentPage.Previous);
end;  // TfrmImportWizard.btnPrevClick 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.btnSaveTemplateClick(Sender: TObject);
begin
  InputBox('Save Template', 'Enter the name of the template', 'MyTemplate');
end;  // TfrmImportWizard.btnSaveTemplateClick 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.ChangePage(const APageClass: TBasePageClass);
begin
  if APageClass = nil then
    raise Exception.Create('Next page class not specified.');
  
  if Assigned(FCurrentPage) then
    FreeAndNil(FCurrentPage);
  
  Screen.Cursor := crDefault;
  btnReturn.Visible := False;
  FCurrentPage := APageClass.Create(Self);
  with FCurrentPage do begin
    // Setup frame's container, if it needs some special re-alignment.
    // Now set put the frame on the panel and set other properties.
    Parent := pnlPages;
    // Missing data page has own scrollbar, so remove space around the panel
    if FCurrentPage is TfraMissingData then
      pnlPages.BorderWidth := 0
    else
      pnlPages.BorderWidth := 4;
    Align := alClient;
    OnChangedContent := PageContentChanged;
    OnForcedNextPage := PageForcedNextPage;
    TabOrder := 0;
    LoadHTML(hvHelp);
  end;
  // btnCancel is last "focusable" control on main form before moving to controls on frame.
  // So ask for the next after it. And it should be one of the frame's own.
  SelectNext(btnCancel, True, True);
  SynchronizeNavigationButtons;
  
  XPMenu.InitComponent(Self);
  Refresh;
  if FCurrentPage.Execute then ChangePage(FCurrentPage.Next);
end;  // TfrmImportWizard.ChangePage 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCurrentPage.Cancel;
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
procedure TfrmImportWizard.PageForcedNextPage(Sender: TObject; APageClass: TBasePageClass);
begin
  ChangePage(APageClass);
end;  // TfrmImportWizard.PageForcedNextPage 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.SetFileExtension(const AExt: String);
begin
  with FSettings do
    if AExt = '.txt' then ImportType := itText else
    if AExt = '.csv' then ImportType := itText else
    if AExt = '.xls' then ImportType := itExcel else
    if AExt = '.dbf' then ImportType := itDBase else
    if AExt = '.db'  then ImportType := itParadox else
    if AExt = '.wk1' then ImportType := itLotus else
    if AExt = '.wq1' then ImportType := itQuattro else
    if AExt = 'ADO'  then ImportType := itADO
    else
      raise Exception.Create('Unrecognised import type.');
end;  // TfrmImportWizard.SetFileExtension 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.SynchronizeNavigationButtons;
begin
  btnNext.Caption := FCurrentPage.NextCaption;  // Can be anything, "Next", "Import"...
  btnNext.Enabled := FCurrentPage.HasNext;
  btnPrev.Enabled := FCurrentPage.HasPrevious;
  
  if btnNext.Caption <> ResStr_Next then btnNext.Spacing := 10;
  
  if FSettings.DataFileLoaded then begin
    StatusBar.Panels[0].Text := Format('%d records', [FSettings.RecordCount]);
    StatusBar.Panels[1].Text := FSettings.SourceDataFile;
  end;
end;  // TfrmImportWizard.SynchronizeNavigationButtons 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UMChangePage(var Msg: TMessage);
begin
  ChangePage(TfraFileSelect);
end;  // TfrmImportWizard.UMChangePage 

{-------------------------------------------------------------------------------
}
procedure TfrmImportWizard.UMReturn(var Msg: TMessage);
begin
  btnReturn.Visible := Msg.WParam = 1;
  btnReturn.Enabled := Msg.LParam = 1;
end;  // TfrmImportWizard.UMReturn 

end.






