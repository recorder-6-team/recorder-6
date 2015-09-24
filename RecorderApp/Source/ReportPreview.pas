//==============================================================================
//  Unit:        ReportPreview
//
//  Implements:  TfrmReportPreview
//
//  Description: Implements preview function for reports.
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 18 $
//    $Date: 9/11/04 17:07 $
//    $Author: Ericsalmon $
//
//  $History: ReportPreview.pas $
//  
//  *****************  Version 18  *****************
//  User: Ericsalmon   Date: 9/11/04    Time: 17:07
//  Updated in $/JNCC/Development/Build/Source
//  ID 7550. Fixed.
//  
//  *****************  Version 17  *****************
//  User: Ericsalmon   Date: 24/08/04   Time: 16:56
//  Updated in $/JNCC/Development/Build/Source
//  ID 3469. Added inherited in FormClose method.
//  
//  *****************  Version 16  *****************
//  User: Ericsalmon   Date: 14/06/02   Time: 10:45
//  Updated in $/JNCC/Source
//  JNCC 554: Export button disabled on Report Preview until the report is
//  fully generated.
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ReportPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, QRPrntr, QuickRpt, QRExport, Buttons, ComCtrls,
  ToolWin, ImgList, BaseFormUnit, BaseChildUnit, Menus, ActnList, OnlineHelp,
  Constants;

type
  TfrmReportPreview = class(TBaseChild)
    QRPreview: TQRPreview;
    dlgSave: TSaveDialog;
    tbPreview: TToolBar;
    ToolButton1: TToolButton;
    Separator1: TToolButton;
    Separator2: TToolButton;
    mnuReports: TMenuItem;
    mnuReportRun: TMenuItem;
    mnuRpeortWizard: TMenuItem;
    N1: TMenuItem;
    mnuReportZoomToFit: TMenuItem;
    mnuReportZoom100: TMenuItem;
    mnuReportZoomToWidth: TMenuItem;
    mnuReportZoomIn: TMenuItem;
    mnuReportZoomOut: TMenuItem;
    N2: TMenuItem;
    mnuReportFirst: TMenuItem;
    mnuReportPrev: TMenuItem;
    mnuReportNext: TMenuItem;
    mnuReportLast: TMenuItem;
    alPreview: TActionList;
    actZoomToFit: TAction;
    actZoom100: TAction;
    actZoomToWidth: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actFirstPage: TAction;
    actPrevPage: TAction;
    actNextPage: TAction;
    actLastPage: TAction;
    N3: TMenuItem;
    actExport: TAction;
    mnuReportExport: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure actPrevPageClick(Sender: TObject);
    procedure actNextPageClick(Sender: TObject);
    procedure actZoomInClick(Sender: TObject);
    procedure actZoomOutClick(Sender: TObject);
    procedure actFirstPageClick(Sender: TObject);
    procedure actLastPageClick(Sender: TObject);
    procedure actZoomToFitClick(Sender: TObject);
    procedure actZoom100Click(Sender: TObject);
    procedure actZoomToWidthClick(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure QRPreviewPageAvailable(Sender: TObject; PageNum: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure QRPreviewProgressUpdate(Sender: TObject; Progress: Integer);
  private
    { Private declarations }
    FQRPreviewPrinter: TQRPrinter;
    procedure Init;
    procedure UpdateButtons;
  public
    { Public declarations }
    pQuickreport:TQuickRep;
    sTitle : string;
    constructor CreatePreview(AOwner : TComponent; aQRPrinter : TQRPrinter);
    property QRPreviewPrinter : TQRPrinter read FQRPreviewPrinter write FQRPreviewPrinter;
    procedure PreviewScreen; override;
    procedure PrintScreen; override;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, FormActions, Maintbar, FilterResult;

{$R *.DFM}

//==============================================================================
constructor TfrmReportPreview.CreatePreview(AOwner : TComponent; aQRPrinter : TQRPrinter);
begin
  inherited Create(AOwner);
  FQRPreviewPrinter:= aQRPrinter;
  QRPreview.QRPrinter := aQRPrinter;
  if (QRPreviewPrinter<> nil) and (QRPreviewPrinter.Title <> '') then
    Caption := QRPreviewPrinter.Title;
  Init;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);
  dmFormActions.actOpenReport.Enabled  :=false;
  dmFormActions.actReportWizard.Enabled:=false;
  //Help Setup
  mnuReports.HelpContext:=IDH_REPORT;
  Self.HelpContext      :=IDH_REPORT;
end;  // CreatePreview

//==============================================================================
procedure TfrmReportPreview.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  frmMain.SetContextToolbar(Self,mnuReports,3,[]);
  dmFormActions.actPrint.Enabled  :=true;
  dmFormActions.actPreview.Enabled:=false;
end;  // FormActivate

//==============================================================================
procedure TfrmReportPreview.FormDeactivate(Sender: TObject);
begin
  inherited;
  dmFormActions.actPrint.Enabled  :=false;
  dmFormActions.actPreview.Enabled:=false;
end;  // FormDeactivate

//==============================================================================
procedure TfrmReportPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  dmFormActions.actPrint.Enabled := false;
  QRPreviewPrinter.ClosePreview(Self);
  QRPreview.QRPrinter := nil;
  // Re-enable report menu items only if preview came from Run and not Wizard.
  dmFormActions.actOpenReport.Enabled   := frmMain.GetForm(TfrmFilterResult) = nil;
  dmFormActions.actReportWizard.Enabled := dmFormActions.actOpenReport.Enabled;
  Action := caFree;
end;  // FormClose

//==============================================================================
procedure TfrmReportPreview.UpdateButtons;
begin
  actFirstPage.Enabled := QRPreview.PageNumber > 1;
  actPrevPage.Enabled  := actFirstPage.Enabled;
  actNextPage.Enabled  := QRPreview.PageNumber < QRPreviewPrinter.PageCount;
  actLastPage.Enabled  := actNextPage.Enabled;
  actExport.Enabled    := QRPreview.QRPrinter.Status = mpFinished;
end;  // UpdateButtons

//==============================================================================
procedure TfrmReportPreview.actZoomToFitClick(Sender: TObject);
begin
  Application.ProcessMessages;
  QRPreview.ZoomToFit;
end;  // actZoomToFitClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actZoom100Click(Sender: TObject);
begin
  Application.ProcessMessages;
  QRPreview.Zoom := 100;
end;  // actZoom100Click

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actZoomToWidthClick(Sender: TObject);
begin
  Application.ProcessMessages;
  QRPreview.ZoomToWidth;
end;  // actZoomToWidthClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actZoomInClick(Sender: TObject);
begin
  Application.ProcessMessages;
  with QRPreview do Zoom := Zoom + 10;
end;  // actZoomInClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actZoomOutClick(Sender: TObject);
begin
  Application.ProcessMessages;
  with QRPreview do
    if Zoom > 10 then Zoom := Zoom - 10;
end;  // actZoomOutClick

//==============================================================================
procedure TfrmReportPreview.actFirstPageClick(Sender: TObject);
begin
  Application.ProcessMessages;
  QRPreview.PageNumber:=1;
  UpdateButtons;
end;  // actFirstPageClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actPrevPageClick(Sender: TObject);
begin
  Application.ProcessMessages;
  with QRPreview do
    if PageNumber > 1 then PageNumber := PageNumber - 1;
  UpdateButtons;
end;  // actPrevPageClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actNextPageClick(Sender: TObject);
begin
  Application.ProcessMessages;
  with QRPreview do
    if PageNumber < QRPrinter.PageCount then PageNumber := PageNumber + 1;
  UpdateButtons;
end;  // actNextPageClick

//------------------------------------------------------------------------------
procedure TfrmReportPreview.actLastPageClick(Sender: TObject);
begin
  Application.ProcessMessages;
  QRPreview.PageNumber := QRPreviewPrinter.PageCount;
  UpdateButtons;
end;  // actLastPageClick

//==============================================================================
procedure TfrmReportPreview.Init;
begin
  QRPreview.ZoomToWidth;
  UpdateButtons;
end;  // Init

//==============================================================================
procedure TfrmReportPreview.QRPreviewPageAvailable(Sender: TObject;
  PageNum: Integer);
var
  lForm: TForm;
begin
  Init;
  Caption := QRPreview.QRPrinter.Title;
  if Caption <> '' then Caption := Caption+' - ';
  if PageNum = 1 then Caption := Caption + '1 page'
                 else Caption := Caption + IntToStr(PageNum) + ' pages';

  case QRPreview.QRPrinter.Status of
    mpReady   : Caption := Caption + ' READY';
    mpBusy    : Caption := Caption + ' BUSY';
    mpFinished:
      begin
        Caption := Caption + ' FINISHED';
        // QuickReport hijacks application's message flow
        lForm := dmFormActions.GetFormInstance(TfrmFilterResult);
        if lForm <> nil then
          TfrmFilterResult(lForm).PreviewFinished;
      end;
  end;
  UpdateButtons;
end;  // QRPreviewPageAvailable

//==============================================================================
procedure TfrmReportPreview.QRPreviewProgressUpdate(Sender: TObject;
  Progress: Integer);
begin
  inherited;
  UpdateButtons;
end;  // QRPreviewProgressUpdate

//==============================================================================
procedure TfrmReportPreview.actExportExecute(Sender: TObject);
var CSVFilter   :TQRCommaSeparatedFilter;
    HTMLFilter  :TQRHTMLDocumentFilter;
    ExcelFilter :TQRXLSFilter;
    RTFFilter   :TQRRTFExportFilter;
    WMFFilter   :TQRWMFExportFilter;
    AsciiFilter :TQRAsciiExportFilter;
    lstFileName :string;
    lstExtension:string;
begin
  inherited;
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSave.Execute then begin
    lstFileName := dlgSave.FileName;
    lstExtension:= ExtractFileExt(lstFileName);
    if UpperCase(lstExtension) = '.CSV' then begin
      CSVFilter := TQRCommaSeparatedFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(CSVFilter);
      finally
        CSVFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.HTM' then begin
      HTMLFilter := TQRHTMLDocumentFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(HTMLFilter);
      finally
        HTMLFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.XLS' then begin
      ExcelFilter := TQRXLSFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(ExcelFilter);
      finally
        ExcelFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.RTF' then begin
      RTFFilter := TQRRTFExportFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(RTFFilter);
      finally
        RTFFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.WMF' then begin
      WMFFilter := TQRWMFExportFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(WMFFilter);
      finally
        WMFFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.TXT' then begin
      AsciiFilter := TQRAsciiExportFilter.Create(lstFileName);
      try
        QRPreviewPrinter.ExportToFilter(AsciiFilter);
      finally
        AsciiFilter.Free;
      end;
    end else if UpperCase(lstExtension) = '.QRP' then begin
      QRPreviewPrinter.Save(lstFileName);
    end;
  end;
end;  // actExportExecute

//==============================================================================
procedure TfrmReportPreview.PreviewScreen;
begin
  // not implemented in this class
end;

//==============================================================================
procedure TfrmReportPreview.PrintScreen;
begin
  // not implemented in this class
end;

//==============================================================================
end.
 