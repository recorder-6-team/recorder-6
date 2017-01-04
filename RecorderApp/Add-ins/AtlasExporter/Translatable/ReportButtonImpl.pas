unit ReportButtonImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, AtlasExporter_TLB, StdVcl, StdCtrls, Recorder2000_TLB,
  Registry, Buttons, gnugettext, ShellApi;

type
  TReportButton = class(TActiveForm, IReportButton, IRecorderAddin, IRecorderFormEvents)
    btnAtlasExporter: TBitBtn;
    procedure btnAtlasExporterClick(Sender: TObject);
  private
    { Private declarations }
    FEvents: IReportButtonEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function GetAddinPath: string;
    procedure CopyScriptsAndTranslations(const srcAddinFile: string);
    procedure CopyFolder(const src, dest: string);
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    // IRecorderAddin
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    // IRecorderFormEvents
    function Get_FormName: WideString; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    procedure DoCancel; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
    procedure DoEditMode; safecall;
    procedure DoAdd; safecall;
    procedure DoDelete; safecall;
    function CheckCanSave: WordBool; safecall;
  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ, ExportWizard, StrUtils, VersionInfo;

{$R *.DFM}

{ TReportButton }

procedure TReportButton.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ReportButtonPage); }
end;

procedure TReportButton.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IReportButtonEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TReportButton.Initialize;
begin
  TranslateComponent(self, 'atlasexporter');
  TextDomain('atlasexporter');
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TReportButton.Get_Active: WordBool;
begin
  Result := Active;
end;

function TReportButton.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TReportButton.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TReportButton.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TReportButton.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TReportButton.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TReportButton.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TReportButton.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TReportButton.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TReportButton.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TReportButton.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TReportButton.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TReportButton.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TReportButton.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TReportButton.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TReportButton.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TReportButton.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TReportButton.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TReportButton.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TReportButton.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TReportButton._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TReportButton.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TReportButton.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TReportButton.CreateEvent(Sender: TObject);
begin
  TranslateComponent(self);
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TReportButton.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TReportButton.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TReportButton.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TReportButton.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TReportButton.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TReportButton.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TReportButton.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TReportButton.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TReportButton.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TReportButton.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TReportButton.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TReportButton.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TReportButton.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TReportButton.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TReportButton.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TReportButton.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TReportButton.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TReportButton.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TReportButton.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TReportButton.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TReportButton.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TReportButton.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

function TReportButton.Get_Description: WideString;
begin
  result := 'Atlas Exporter: ' + _('description')+#13#10+_('Version')+' '+ GetFileVersion(GetAddinPath()+'\AtlasExporter.ocx')+#13#10+#13#10+_('Shape export uses Shape Viewer Objects from Ecological Software Solutions (http://www.ecostats.com)');
end;

function TReportButton.Get_ImageFileName: WideString;
begin
  result := 'atlas_exporter.bmp';
end;

function TReportButton.Get_Name: WideString;
begin
  result := 'Atlas exporter';
end;

procedure TReportButton.Install(const iInstalledFilePath: WideString);
begin
  CopyScriptsAndTranslations(iInstalledFilePath);
end;

procedure TReportButton.CopyScriptsAndTranslations(const srcAddinFile: string);
var
  addinPath: string;
  srcFolder: string;
begin
  addinPath := GetAddinPath;
  srcFolder := ExtractFileDir(srcAddinFile) + '\';
  // Can ignore installing the scripts if already in the addins folder
  if (addinPath<>srcFolder) then begin
    try
      CopyFolder(srcFolder + 'Atlas Exporter', addinPath);
      CopyFolder(srcFolder + 'locale', addinPath);
    except
      on e:Exception do begin
        ShowMessage(e.Message);
        ShowMessage(Format(_('You can complete the installation by copying the %s and %s folder to %s and %s respectively.'),
            [srcFolder + 'Atlas Exporter\', srcFolder + 'locale\', addinPath + 'Atlas Exporter\', addinPath + 'locale\']));
      end;
    end;
  end;
end;

procedure TReportButton.CopyFolder(const src, dest: string);
var
  Fos : TSHFileOpStruct;
  Buf : array[0..4096] of char;
  p : pchar;
begin
  FillChar(Buf, sizeof(Buf), #0) ;
  p := @buf;
  p := StrECopy(p, PChar(src)) + 1;

  FillChar(Fos, sizeof(Fos), #0) ;
  with Fos do
  begin
    Wnd := Handle;
    wFunc := FO_COPY;
    pFrom := @Buf;
    pTo := PChar(dest);
    fFlags := 0;
  end;
  if ((SHFileOperation(Fos) <> 0) or (Fos.fAnyOperationsAborted <> false)) then
    Raise Exception.Create(_('Cancelled'));
end;

function TReportButton.CheckCanSave: WordBool;
begin
  result := true;
end;

procedure TReportButton.DoAdd;
begin

end;

procedure TReportButton.DoCancel;
begin

end;

procedure TReportButton.DoDelete;
begin

end;

procedure TReportButton.DoEditMode;
begin

end;

procedure TReportButton.DoItemChange(const iTableName,
  iKeyValue: WideString);
begin

end;

procedure TReportButton.DoSave;
begin

end;

function TReportButton.Get_FormName: WideString;
begin
  result := 'TfrmFilterResult';
end;

procedure TReportButton.SetForm(const iForm: IRecorderForm);
var parent: IRecorderControl;
begin
  parent := iForm.Control['pnlButtons'] as IRecorderControl;
  iForm.EmbedActiveX(self as IUnknown, CLASS_ReportButton, parent, 129, 6, btnAtlasExporter.Width, btnAtlasExporter.Height);
end;

function TReportButton.GetAddinPath: string;
var
  reg: TRegistry;
  addinPath: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly('SOFTWARE\Dorset Software\Recorder 6');
    addinPath := reg.ReadString('Addin Path');
  finally
    reg.free;
  end;
  result := addinPath;
end;

procedure TReportButton.btnAtlasExporterClick(Sender: TObject);
begin
  with TdlgExportWizard.Create(nil) do try
    ShowModal;
  finally
    free;
  end;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TReportButton,
    Class_ReportButton,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
