//================================================================================
// Author:  Stuart Ball     stuart.ball@jncc.gov.uk
// Date:    Feb 2009
// Purpose: Do grid ref vs VC check for SURVEY_EVENT and SAMPLEs in Recorder 6
//          The result is an External Filter file which can be loaded to filter
//          out the cases with problems and show the error messages.
//          The whole database can be checked, or items related to a specific
//          row that is dragged and dropped onto the addin.
//--------------------------------------------------------------------------------
//================================================================================
unit VC_Grid_CheckerImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$WARNINGS OFF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, VCchecker_TLB, StdVcl, StdCtrls, Buttons, ComCtrls,
  Recorder2000_TLB, Addinclasses, VersionInfo, DragDropComponent, DropTarget,
  Grids, Registry, ExtCtrls, ADODB, ShlObj, StrUtils,
  ItemsList, VC_check, AbBase, AbBrowse, AbZBrows, AbUnzper,
  PageControlWithoutBorder, SampleAdminArea;
{$WARNINGS ON}

type

  TVC_Grid_CheckerX = class(TActiveForm, IVC_Grid_CheckerX, IRecorderAddin,
                            INewAction, IFormCaption)
    panelBottom: TPanel;
    bCheck: TBitBtn;
    bStop: TBitBtn;
    UnZipper: TAbUnZipper;
    PageControl: TPageControlWithoutBorder;
    tabGetItems: TTabSheet;
    tabProgress: TTabSheet;
    memo: TMemo;
    ProgressBar: TProgressBar;
    labelErrors: TLabel;
    rgOptions: TRadioGroup;
    sgItems: TStringGrid;
    cbPopulateSampleAdmin: TCheckBox;
    procedure rgOptionsClick(Sender: TObject);
    procedure sgItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure bCheckClick(Sender: TObject);
    procedure PageControlResize(Sender: TObject);
    procedure bStopClick(Sender: TObject);
  private
    { Private declarations }
  {Our global variables}
    FRecorder2000: IRecorder2000;  // interface to Rec2002
    FConnection:   TADOConnection; // connection to the database
    FQuery:        TADOQuery;      // Query on the database
    FItems:        TInfoList;
    FAddinPath: string; // path to Addin directory where the temporary database is located
    FExternalFilterPath: string; // path to which to write reslts
    FStop: boolean;
    FRunning: boolean;
    FSampleAdminAreasExists: boolean;
    FSampleAdminArea: TSampleAdminArea;

    FEvents: IVC_Grid_CheckerXEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseLeaveEvent(Sender: TObject);
    procedure PaintEvent(Sender: TObject);
    procedure SetUpForm;
    function Get_AddinPath: string;
    function Get_Recorder2000: IRecorder2000;
    function Get_Connection: TADOConnection;
    function Get_Query: TADOQuery;
    procedure GridDeleteRow(RowNumber: Integer);
    procedure SetColWidths;
    procedure ReportTime(const msg: string);
    procedure DoChecks(vcInfo: TVC_Checker);
    procedure GetGridVC(const aTable, theKeys: string; var nErrs: Longint;
              vcInfo: TVC_Checker);
    function GetSystemPath(Folder: Integer): string;
    function Get_ExternalFilterPath: string;
    function DoesSampleAdmineAreasExist: boolean;
    procedure DoAbort;
    procedure WriteErrors(vcInfo: TVC_Checker);
    procedure DoCallBack(Sender: TObject; const aType: TCallBackType; const n: Integer);
    procedure DoCheck(infoCheck: TInfoToCheck; var nErrs: Integer;
              vcInfo: TVC_Checker);
    function Get_SampleAdminArea: TSampleAdminArea;
    function plural(const n: integer): string;
  protected
    {Drag and drop}
    FItemDropper : TJNCCDragDrop;

    procedure DropItem(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TstringList; var ioHandled : boolean);
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AlignWithMargins: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DockSite: WordBool; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_ExplicitHeight: Integer; safecall;
    function Get_ExplicitLeft: Integer; safecall;
    function Get_ExplicitTop: Integer; safecall;
    function Get_ExplicitWidth: Integer; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_MouseInClient: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PopupMode: TxPopupMode; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_UseDockManager: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AlignWithMargins(Value: WordBool); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DockSite(Value: WordBool); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PopupMode(Value: TxPopupMode); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_UseDockManager(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    {IFormCaption}
    function Get_FormCaption: WideString; safecall;
  public
    { Public declarations }
    procedure Initialize; override;
    destructor Destroy; override;
    property Recorder2000: IRecorder2000 read Get_Recorder2000;
    property ADOConnection: TADOConnection read Get_Connection;
    property ADOQuery: TADOQuery read Get_Query;
    property AddinPath: string read Get_AddinPath;
    property ExternalFilterPath: string read Get_ExternalFilterPath;
    property SampleAdminArea: TSampleAdminArea read Get_SampleAdminArea;
  end;

implementation

uses
  ComObj, ComServ;

const
  RegistryKey = 'Software\Dorset Software\Recorder 6';
  SAMPLE_SQL = 'SELECT SAMPLE.SAMPLE_KEY as ItemKey, SAMPLE.SPATIAL_REF as GridRef, ADMIN_AREA.SHORT_CODE as VC ' +
                 'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ' +
                 'INNER JOIN SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                 'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') ' +
                 'AND (LEN(SAMPLE.SPATIAL_REF) >= 4) AND (SAMPLE.SAMPLE_KEY in (%s)) ';
  SAMPLE_SAA = 'SELECT SAMPLE.SAMPLE_KEY AS ItemKey, SAMPLE.SPATIAL_REF AS GridRef, ADMIN_AREA.SHORT_CODE AS VC ' +
                  'FROM ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY INNER JOIN ' +
                  'SAMPLE ON LOCATION_ADMIN_AREAS.LOCATION_KEY = SAMPLE.LOCATION_KEY ' +
                  'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (SAMPLE.SPATIAL_REF_SYSTEM = ''OSGB'') AND (LEN(SAMPLE.SPATIAL_REF) >= 4) ' +
                  'AND (SAMPLE.SAMPLE_KEY IN (%s)) ';
  EVENT_SQL =  'SELECT SURVEY_EVENT.SURVEY_EVENT_KEY as ItemKey,SURVEY_EVENT.SPATIAL_REF AS GridRef, ADMIN_AREA.SHORT_CODE AS VC ' +
                 'FROM SURVEY_EVENT INNER JOIN ADMIN_AREA INNER JOIN LOCATION_ADMIN_AREAS ' +
                 'ON ADMIN_AREA.ADMIN_AREA_KEY = LOCATION_ADMIN_AREAS.ADMIN_AREA_KEY ON SURVEY_EVENT.LOCATION_KEY = LOCATION_ADMIN_AREAS.LOCATION_KEY ' +
                 'WHERE (ADMIN_AREA.ADMIN_TYPE_KEY = ''NBNSYS0000000032'') AND (LEN(SURVEY_EVENT.SPATIAL_REF) >= 4) ' +
                 'AND (SURVEY_EVENT.SPATIAL_REF_SYSTEM = ''OSGB'') AND (SURVEY_EVENT.SURVEY_EVENT_KEY IN (%s)) ';

{$R *.DFM}

{ TVC_Grid_CheckerX }

//--------------------------------------------------------------------------------
// Automatically generated by Delphi
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_VC_Grid_CheckerXPage); }
end;

procedure TVC_Grid_CheckerX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IVC_Grid_CheckerXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TVC_Grid_CheckerX.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnMouseEnter := MouseEnterEvent;
  OnMouseLeave := MouseLeaveEvent;
  OnPaint := PaintEvent;
end;

function TVC_Grid_CheckerX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TVC_Grid_CheckerX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TVC_Grid_CheckerX.Get_AlignWithMargins: WordBool;
begin
  Result := AlignWithMargins;
end;

function TVC_Grid_CheckerX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TVC_Grid_CheckerX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TVC_Grid_CheckerX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TVC_Grid_CheckerX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TVC_Grid_CheckerX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TVC_Grid_CheckerX.Get_DockSite: WordBool;
begin
  Result := DockSite;
end;

function TVC_Grid_CheckerX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TVC_Grid_CheckerX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TVC_Grid_CheckerX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TVC_Grid_CheckerX.Get_ExplicitHeight: Integer;
begin
  Result := ExplicitHeight;
end;

function TVC_Grid_CheckerX.Get_ExplicitLeft: Integer;
begin
  Result := ExplicitLeft;
end;

function TVC_Grid_CheckerX.Get_ExplicitTop: Integer;
begin
  Result := ExplicitTop;
end;

function TVC_Grid_CheckerX.Get_ExplicitWidth: Integer;
begin
  Result := ExplicitWidth;
end;

function TVC_Grid_CheckerX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TVC_Grid_CheckerX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TVC_Grid_CheckerX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TVC_Grid_CheckerX.Get_MouseInClient: WordBool;
begin
  Result := MouseInClient;
end;

function TVC_Grid_CheckerX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TVC_Grid_CheckerX.Get_PopupMode: TxPopupMode;
begin
  Result := Ord(PopupMode);
end;

function TVC_Grid_CheckerX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TVC_Grid_CheckerX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TVC_Grid_CheckerX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TVC_Grid_CheckerX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TVC_Grid_CheckerX.Get_UseDockManager: WordBool;
begin
  Result := UseDockManager;
end;

function TVC_Grid_CheckerX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TVC_Grid_CheckerX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TVC_Grid_CheckerX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TVC_Grid_CheckerX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TVC_Grid_CheckerX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TVC_Grid_CheckerX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TVC_Grid_CheckerX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TVC_Grid_CheckerX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TVC_Grid_CheckerX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TVC_Grid_CheckerX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TVC_Grid_CheckerX.MouseEnterEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseEnter;
end;

procedure TVC_Grid_CheckerX.MouseLeaveEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseLeave;
end;

procedure TVC_Grid_CheckerX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TVC_Grid_CheckerX.Set_AlignWithMargins(Value: WordBool);
begin
  AlignWithMargins := Value;
end;

procedure TVC_Grid_CheckerX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TVC_Grid_CheckerX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TVC_Grid_CheckerX.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TVC_Grid_CheckerX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TVC_Grid_CheckerX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TVC_Grid_CheckerX.Set_DockSite(Value: WordBool);
begin
  DockSite := Value;
end;

procedure TVC_Grid_CheckerX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TVC_Grid_CheckerX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TVC_Grid_CheckerX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TVC_Grid_CheckerX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TVC_Grid_CheckerX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := string(Value);
end;

procedure TVC_Grid_CheckerX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TVC_Grid_CheckerX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TVC_Grid_CheckerX.Set_PopupMode(Value: TxPopupMode);
begin
  PopupMode := TPopupMode(Value);
end;

procedure TVC_Grid_CheckerX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TVC_Grid_CheckerX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TVC_Grid_CheckerX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TVC_Grid_CheckerX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TVC_Grid_CheckerX.Set_UseDockManager(Value: WordBool);
begin
  UseDockManager := Value;
end;

procedure TVC_Grid_CheckerX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

//=============================================================================
// Recorder 2000 interface implementations
//=============================================================================
function TVC_Grid_CheckerX.Get_ActionCaption: WideString;
begin
  Result := 'VC vs gridref check';
end;

function TVC_Grid_CheckerX.Get_CanAddToToolbar: WordBool;
begin
  Result := True;
end;

function TVC_Grid_CheckerX.Get_Description: WideString;
begin
  Result := 'Check that grid references in SAMPLEs and SURVEY_EVENTs fall within ' +
            'the vice-county they are related to.' +
            AddinVersion('VCchecker.ocx') +
            ' Note that it is restricted to OSGB grid refs and the VCs of Great Britain. '+
            'It DOES NOT work with the OSNI grid and Irish VCs at the moment!';
end;

function TVC_Grid_CheckerX.Get_DimmedImageFilename: WideString;
begin
  Result := 'vcs.bmp';
end;

function TVC_Grid_CheckerX.Get_DisabledImageFileName: WideString;
begin
  Result := 'vcs.bmp';
end;

function TVC_Grid_CheckerX.Get_FormCaption: WideString;
begin
  Result := 'Check VCs vs. grid references';
  SetUpForm;
end;

function TVC_Grid_CheckerX.Get_Hint: WideString;
begin
  Result := 'Check OSGB grid references against Vice-counties from SAMPLEs and SURVEY_EVENTs.';
end;

function TVC_Grid_CheckerX.Get_ImageFileName: WideString;
begin
  Result := 'vcs.bmp';
end;

function TVC_Grid_CheckerX.Get_Name: WideString;
begin
  Result := 'VC vs gridref check';
end;

function TVC_Grid_CheckerX.Get_ParentMenu: WideString;
begin
  Result := 'Tools';
end;

//--------------------------------------------------------------------------------
// We need to unpack the refence info and install it under the AddinPath
// The ref info is supplied in a zip file called vcinfo.zip
// It is unzipped to <AddinPath>\vc_check
// Note that UnZipper is setup with options to restore paths from the archive
// and create directories as needed
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.Install(const iInstalledFilePath: WideString);
var zFile: string;
begin
  // make sure our ref file path exists
  zFile := AddinPath + 'vc_check';
  if not DirectoryExists(zFile) then
    MkDir(zFile);
  // set it as the base path for unzipping
  UnZipper.BaseDirectory := zFile;

  // the zip file containing our ref info
  zFile := ExtractFilePath(iInstalledFilePath) + 'vcinfo.zip';
  if FileExists(zFile) then
    with UnZipper do
    begin
      OpenArchive(zFile);
      ExtractFiles('*.txt');
      CloseArchive;
    end
  else
    MessageDlg(Format('Cannot find file %s' + #13 + 'The addin will not work properly!',
                       [zFile]), mtError, [mbOK], 0);
end;

//=============================================================================
// Drag and drop implementations
//=============================================================================
//--------------------------------------------------------------------------------
// This manages a table in the interface which accepts items dragged from
// other Recorder windows. The type of item and the number of SURVEY_EVENTs
// and SAMPLEs associated with it are shown. Behind the scenes, it builds
// a TInfoList object (FItems)
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DropItem(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
var i: integer;
    iCursor: Longint;
begin
  iCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    with rgOptions do
      if ItemIndex <> 1 then
      begin
        ItemIndex := 1;
        // the onClick event gets trigerred here
        Application.ProcessMessages;
      end;
    with sgItems do
    begin
      // if the bottom line is in use - add a new line
      if Cells[0,RowCount-1] <> '' then
        RowCount := RowCount+1;
      i := FItems.FindItem(iSourceData.Header.TableName, iSourceData.Items[0].KeyField1);
      if i < 0 then
        i := FItems.AddItem(iSourceData.Header.TableName, iSourceData.Items[0].KeyField1);
      if (i >= 0) then
      begin
        Cells[0,RowCount-1] := FItems.ItemLabel[i];
        Cells[1,RowCount-1] := FItems.TypeLabel[i];
        Cells[2,RowCount-1] := FItems.SampleCount[i];
        Cells[3,RowCount-1] := FItems.EventCount[i];
        FItems.Selected[i] := True;
        Objects[0,RowCount-1] := TObject(i);
      end
      else
      begin
        Cells[0,RowCount-1] := 'Unknown';
        Cells[1,RowCount-1] := 'Unknown';
        Cells[2,RowCount-1] := '0';
        Cells[3,RowCount-1] := '0';
      end;
    end;
  finally
    Screen.Cursor := iCursor;
  end;
  ioHandled := True;
  SetFocusedControl(bCheck);
end;

//=============================================================================
// Property implementations
//=============================================================================
//--------------------------------------------------------------------------------
// Clean up before we quit
//--------------------------------------------------------------------------------
destructor TVC_Grid_CheckerX.Destroy;
begin
  // free drag-and-drop objects
  If FItemDropper <> nil then
    FItemDropper.Free;
  FRecorder2000 := nil;
  If FQuery <> nil then
  begin
    FQuery.Close;
    FQuery.Free;
  end;
  if FSampleAdminArea <> nil then
    FSampleAdminArea.Free;
  If FConnection <> nil then
  begin
    FConnection.Close;
    FConnection.Free;
  end;
    FItems.Free;
  inherited;
end;

//--------------------------------------------------------------------------------
// The base directory for storing the reference files we need
// Get in from the registry
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_AddinPath: string;
var oReg: TRegistry;
begin
  if FAddinPath = '' then
  begin
     {Get addin path from registry}
     oReg := TRegistry.Create;
     try
        oReg.RootKey := HKEY_LOCAL_MACHINE;
        oReg.OpenKey(RegistryKey, False);
        FAddinPath := oReg.ReadString('Addin Path');
     finally
        oReg.Free;
     end;
  end;
  Result := FAddinPath;
end;

//-------------------------------------------------------------------------------
// Connect to NBNData SQL Server database-
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_Connection: TADOConnection;
begin
  if FConnection = nil then
  begin
    FConnection := TADOConnection.Create(nil);
    FConnection.LoginPrompt := False;
    FConnection.ConnectionString := Recorder2000.ConnectionString;
    FConnection.Open;
    Recorder2000.SetApplicationSecurity(FConnection.ConnectionObject);
  end;
  Result := FConnection;
end;

//--------------------------------------------------------------------------------
// Provides a query on the database
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_Query: TADOQuery;
begin
  If FQuery = nil then
  begin
    FQuery := TADOQuery.Create(nil);
    FQuery.Connection := ADOConnection;
  end;
  Result := FQuery;
end;

//--------------------------------------------------------------------------------
// Get the Recorder object
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_Recorder2000: IRecorder2000;
begin
  if FRecorder2000 = nil then
  begin
     FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  end;
  Result := FRecorder2000;
end;

//--------------------------------------------------------------------------------
// Get the object tomanage the insertion of Sample_AdminAreas
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_SampleAdminArea: TSampleAdminArea;
begin
  if FSampleAdminArea = nil then
  begin
    FSampleAdminArea := TSampleAdminArea.Create(ADOConnection, Recorder2000);
  end;
  Result := FSampleAdminArea;
end;

//=============================================================================
// Events on the form
//=============================================================================
//--------------------------------------------------------------------------------
// Respond to the choice between querying the whole database or specific items
// Changing the option clears the table on the form
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.rgOptionsClick(Sender: TObject);
var iCursor: Longint;
    i: integer;
begin
  iCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
  with sgItems do
  begin
    if RowCount > 2 then
      RowCount := 2;
    with rgOptions do
      case ItemIndex of
      0:  begin
            i := FItems.FindItem('Database','All');
            if i < 0 then
              i := FItems.AddItem('Database','All');
            if i >= 0 then
            begin
              Cells[0,RowCount-1] := FItems.ItemLabel[i];
              Cells[1,RowCount-1] := FItems.TypeLabel[i];
              Cells[2,RowCount-1] := FItems.SampleCount[i];
              Cells[3,RowCount-1] := FItems.EventCount[i];
              Objects[0,RowCount-1]:= Tobject(i);
            end;
          end;
      1:  begin
            Cells[0,1] := '';
            Cells[1,1] := '';
            Cells[2,1] := '';
            Cells[3,1] := '';
          end;
      end;
  end;
  finally
    Screen.Cursor := iCursor;
  end;
end;

//--------------------------------------------------------------------------------
// delete a row in the grid if the DEL key is pressed
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.sgItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var iRow, i: integer;
begin
  if Key = VK_DELETE then
    with sgItems do
    begin
      iRow := Selection.Top;
      i := integer(Objects[0,iRow]);
      FItems.Selected[i] := False;
      if RowCount > 2 then
      begin
        GridDeleteRow(iRow);
      end
      else
      begin
        Cells[0,1] := '';
        Cells[1,1] := '';
        Cells[2,1] := '';
        Cells[3,1] := '';
      end;
    end;
end;

//=============================================================================
// THE RUN BUTTON!
//=============================================================================
//--------------------------------------------------------------------------------
// This is the core routine that starts the checking process
// It mainly deals with setting up the screen and providing feedback
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.bCheckClick(Sender: TObject);
var oVCInfo: TVC_Checker;
    iCursor: Longint;
    errMsg: string;
begin
  if FItems.Count > 0 then
  begin
    // the main execute method
    iCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    bCheck.Visible:= False;
    bCheck.Enabled := False;
    bStop.Visible := True;
    bStop.Enabled := True;
    FRunning := True;
    try
      PageControl.ActivePageIndex := 1;
      memo.Lines.Add(Format('Checking %0.0n SURVEY_EVENTs and %0.0n SAMPLEs.',
                     [FItems.TotalEvents * 1.0, FItems.TotalSamples * 1.0]));
      memo.Lines.Add('');
      ReportTime('Started at');
      memo.Lines.Add('Loading gridsq/VC lookup table');
      oVCInfo := TVC_Checker.Create(AddinPath);
      try
        DoChecks(oVCInfo);
      finally
        oVCInfo.Free;
      end;
    finally
      Screen.Cursor := iCursor;
    end;
    ReportTime('Finished at');
    FRunning := False;
    bStop.Enabled := False;
  end
  else
  begin
    if (rgOptions.ItemIndex < 0) then
      errMsg := 'Please select an option from "What do you want to check?".'
    else
      errMsg := 'Please select something to check by dragging and dropping' +
                #13 + 'items such as Surveys, Locations, Species names, etc.' +
                #13 + 'into the box on this form.';
    MessageDlg(errMsg, mtInformation, [mbOK], 0);
  end;
end;

//--------------------------------------------------------------------------------
// The stop button
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.bStopClick(Sender: TObject);
begin
  if FRunning then
    FStop := True
  else
    Close;
end;

//--------------------------------------------------------------------------------
// Get confirmation from user and stop
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DoAbort;
begin
  if MessageDlg('Do you really want to quit now?', mtConfirmation,
             [mbYes, mbNo], 0) = mrYes then
  begin
    ReportTime('Stopped by user!');
    bStop.Enabled := False;
    FStop := False;
    FRunning := False;
    Abort;
  end;
end;

//=============================================================================
// VC Checking routines
//=============================================================================
//--------------------------------------------------------------------------------
// Load the keys to be checked and call the checker
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DoChecks(vcInfo: TVC_Checker);
var sKeys: string;
    nErrors: Longint;
begin
  nErrors := 0;
  ReportTime('Loading SAMPLE keys:');
  if FStop then DoAbort;
  sKeys := FItems.Samples;
  if sKeys <> '' then
  begin
    ReportTime('Checking SAMPLEs:');
    if FStop then DoAbort;
    GetGridVC('SAMPLE', sKeys, nErrors, vcInfo);
  end;
  labelErrors.Visible := False;
  if nErrors > 0 then
    memo.Lines.Add(Format('%0.0n error%s found in SAMPLEs.', [nErrors * 1.0, plural(nErrors)]));
  Application.ProcessMessages;
  ReportTime('Loading SURVEY_EVENT keys:');
  if FStop then DoAbort;
  sKeys := FItems.Events;
  if sKeys <> '' then
  begin
    ReportTime('Checking SURVEY_EVENTs:');
    if FStop then DoAbort;
    GetGridVC('SURVEY_EVENT', sKeys, nErrors, vcInfo);
  end;
  labelErrors.Visible := False;
  Application.ProcessMessages;
  if nErrors = 0 then
  begin
    sKeys := 'No errors found.';
    memo.Lines.Add('');
    memo.Lines.Add(sKeys);
    memo.Lines.Add(DupeString('-',Length(sKeys)));
    memo.Lines.Add('');
  end
  else
  begin
    sKeys := Format('%0.0n error%s found.', [nErrors * 1.0, plural(nErrors)]);
    memo.Lines.Add('');
    memo.Lines.Add(sKeys);
    memo.Lines.Add(DupeString('-',Length(sKeys)));
    memo.Lines.Add('');
    WriteErrors(vcInfo);
  end;
  memo.Lines.Add(Format('%0.0n unique grid ref/VC combination%s checked.',
                        [vcInfo.ChecksDone * 1.0, plural(vcInfo.ChecksDone)]));
  memo.Lines.Add(Format('%0.0n grid ref in VC polygon check%s done.',
                        [vcInfo.GridInVcDone * 1.0, plural(vcInfo.GridInVcDone)]));
  if cbPopulateSampleAdmin.Checked then
    memo.Lines.Add(Format('%0.0n Sample Admin Area%s added.',
                        [SampleAdminArea.nAdded * 1.0, plural(SampleAdminArea.nAdded)]));
end;

//--------------------------------------------------------------------------------
// Helper functions to handle singular or plurals in messages
//--------------------------------------------------------------------------------
function TVC_Grid_CheckerX.plural(const n: integer): string;
begin
  Result := 's were';
  if n = 1 then
    Result := ' was';
end;

//--------------------------------------------------------------------------------
// Does the callback from the FItems object which shows progress as
// keys are loaded
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DoCallBack(Sender: TObject; const aType: TCallBackType; const n: Longint);
begin
  if FStop then DoAbort;
  case aType of
  cbInit: begin
            ProgressBar.Position := 0;
            ProgressBar.Max := n
          end;
  cbPos:  ProgressBar.Position := n;
  cbDone: ProgressBar.Position := 0;
  end; {case}
end;

//--------------------------------------------------------------------------------
// Does the query which actually gets the grid ref and VC for the selected
// keys and passes them to vcInfo for the check
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.GetGridVC(const aTable, theKeys: string; var nErrs: Longint;
                                      vcInfo: TVC_Checker);
var theSQL, aKey, lastKey: string;
    i, nVCs, thisVc: byte;
    chk: TInfoToCheck;
    match: boolean;
begin
  with ADOQuery do
  begin
    SQL.Clear;
    if aTable = 'SAMPLE' then
    begin
      theSQL := Format(SAMPLE_SQL, [theKeys]);
      if FSampleAdminAreasExists then
        theSQL := theSQL + 'UNION ' + Format(SAMPLE_SAA, [theKeys]);
    end
    else
      theSQL := Format(EVENT_SQL, [theKeys]);
    theSQl := theSQL + 'ORDER BY ItemKey';
    SQL.Add(theSQL);
    try
      Open;
      If RecordCount > 0 then
      begin
        ProgressBar.Position := 0;
        ProgressBar.Max := RecordCount;
        lastKey := '';
        nVCs := 0;
        while not EoF do
        begin
          ProgressBar.Position := ProgressBar.Position + 1;
          aKey := FieldByName('ItemKey').AsString ;
          if aKey <> lastKey then
          begin
            if (lastKey <> '') and (nVcs > 0) then
            begin
              SetLength(chk.VCs, nVCs);
              DoCheck(chk, nErrs, vcInfo);
            end;
            //new(chk);
            chk.Table := aTable;
            chk.Key := aKey;
            chk.gridRef := FieldByName('GridRef').AsString;
            SetLength(chk.VCs, 4);
            nVCs := 1;
            chk.VCs[nVCs-1] := FieldByName('VC').AsInteger;
            lastKey := aKey;
          end
          else
          // we have another VC for the same key
          begin
            // is it a new VC?
            match := False;
            thisVC := FieldByName('VC').AsInteger;
            for i := 0 to nVCs-1 do
              if (chk.VCs[i] = thisVC) then
              begin
                match := True;
                break;
              end;
            // if so - add it to chk
            if not match then
            begin
              inc(nVCs);
              chk.VCs[nVCs-1] := thisVC;
            end;
          end;
          Next;
        end;
        // check the last one
        if (lastKey <> '') and (nVcs > 0) then
        begin
          SetLength(chk.VCs, nVCs);
          DoCheck(chk, nErrs, vcInfo);
        end;
      end;
    finally
      Close;
      ProgressBar.Position := 0;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Pass the infoCheck record to vcInfo object and deal with reporting
// errors if necessary
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.DoCheck(infoCheck: TInfoToCheck; var nErrs: Longint;
                                    vcInfo: TVC_Checker);
begin
  // do the check
  if vcInfo.Check(infoCheck) then
  begin
    // the check succeeded
    // add a Sample_Admin_Area entry if appropriate
    if FSampleAdminAreasExists and  // Sample_Admin_Area table availabel
       cbPopulateSampleAdmin.Checked and // user has requested we populate
       (infoCheck.Table = 'SAMPLE') then // we are dealing with a SAMPLE
          SampleAdminArea.Insert(infoCheck);
  end
  else
  begin
    // the check found an error - increment the error count
    inc(nErrs);
    labelErrors.Caption := Format('%0.0n error%s found.', [nErrs * 1.0, plural(nErrs)]);
    labelErrors.Visible := True;
    Application.ProcessMessages;
  end;
end;

//--------------------------------------------------------------------------------
// Write the results
// The file name is generated and involves the date., but check whether
// the resulting name already exists and add (1), (2) etc until a new file
// name is found
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.WriteErrors(vcInfo: TVC_Checker);
var aFile, work: string;
    y,m,d: word;
begin
  DecodeDate(now(), y, m, d);
  aFile := ExternalFilterPath + 'Grid_VC_Check_' + Format('%0.4d-%0.2d-%0.2d', [y,m,d]);
  d := 0;
  work := aFile;
  while FileExists(work + '.ref') do
  begin
    inc(d);
    work := format('%s(%d)', [aFile, d]);
  end;
  aFile := work + '.ref';
  vcInfo.WriteErrors(aFile);
  memo.Lines.Add(Format('External Filter file %s written.', [aFile]));
  memo.Lines.Add('');
  memo.Lines.Add('Use "Tools - Load External Filter" to read this file and show the errors in Recorder''s Observation window.');
  memo.Lines.Add('');
  Application.ProcessMessages;
end;

//--------------------------------------------------------------------------------
// Write a message to the progress memo and suffix it with the current time.
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.ReportTime(const msg: string);
var h, m, s, ms: word;
begin
  DecodeTime(now(),h,m,s,ms);
  memo.Lines.Add(Format('%s %0.2d:%0.2d:%0.2d', [msg,h,m,s]));
  Application.ProcessMessages;
end;

//=============================================================================
// Local routines
//=============================================================================
//--------------------------------------------------------------------------------
// Initialisation code
//--------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.SetUpForm;
begin
  PageControl.ActivePageIndex := 0;
  ProgressBar.Visible := True;
  memo.Clear;
  labelErrors.Visible := False;
  labelErrors.Caption := '';
  // initial appearance of the string grid
  with sgItems do
  begin
    SetColWidths;
    Cells[0,0] := 'Item';
    Cells[1,0] := 'Type';
    Cells[2,0] := 'Samples';
    Cells[3,0] := 'Events';
  end;
  // create the drag and drop object
  FItemDropper := TJNCCDragDrop.Create(Self);
  FItemDropper.RegisterDropComponent(sgItems, DropItem,
           DROP_TABLES,[CF_JNCCDATA]);
  // create the TItemInfo object
  FItems := TInfoList.Create(ADOQuery);
  FItems.OnCallBack := DoCallBack;
  FStop := False;
  FRunning := False;
  cbPopulateSampleAdmin.Visible := DoesSampleAdmineAreasExist;
end;

//------------------------------------------------------------------------------
// Works out appropriate width for the columns to utelise thw width of the
// string grid.
//------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.SetColWidths;
var w: integer;
begin
  with sgItems do
  begin
    // make the two numeric columns 1/3 of the width
    // but restrict them to max of 60 and min of 50
    w := width div 6;
    if w > 60 then w := 60;
    if w < 50 then w := 50;
    ColWidths[2] := w;
    ColWidths[3] := w;
    // share the remaining sopace between the name and the type in ration 2:1
    w := width - 2 * w;
    ColWidths[0] := w * 2 div 3;
    ColWidths[1] := w - ColWidths[0];
  end;
  bCheck.Left := (width - bCheck.Width) div 2;
  with bStop do
  begin
    Visible := False;
    Enabled := False;
    Left := bCheck.Left;
    Top := bCheck.Top;
    Width := bCheck.Width;
    Height := bCheck.Height;
  end;
end;

//------------------------------------------------------------------------------
// Reset the column widths if the controls get resized
//------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.PageControlResize(Sender: TObject);
begin
  SetColWidths;
end;

//------------------------------------------------------------------------------
// Delete a row from the string grid
//------------------------------------------------------------------------------
procedure TVC_Grid_CheckerX.GridDeleteRow(RowNumber: Integer);
var
  i: Integer;
begin
  sgItems.Row := RowNumber;
  if (sgItems.Row = sgItems.RowCount - 1) then
    { On the last row}
    sgItems.RowCount := sgItems.RowCount - 1
  else
  begin
    { Not the last row}
    for i := RowNumber to sgItems.RowCount - 1 do
      sgItems.Rows[i] := sgItems.Rows[i + 1];
    sgItems.RowCount := sgItems.RowCount - 1;
  end;
end;

//------------------------------------------------------------------------------
// Get the path where we are going to write the error report file
// Recorder 6 seems to use /Recorder 6/User Files if it exists
// otherwise the user's Document path
//------------------------------------------------------------------------------
function TVC_Grid_CheckerX.Get_ExternalFilterPath: string;
var oReg: TRegistry;
    work: string;
begin
  if FExternalFilterPath = '' then
  begin
     {Get installation path from registry}
     oReg := TRegistry.Create;
     try
        oReg.RootKey := HKEY_LOCAL_MACHINE;
        oReg.OpenKey(RegistryKey, False);
        work := oReg.ReadString('Installation Path');
     finally
        oReg.Free;
     end;
     // see if User Files exists
     work := work + 'User Files';
     if DirectoryExists(work) then
      FExternalFilterPath := work + '\'
     else
     begin
       work := GetSystemPath(CSIDL_PERSONAL);
       if work[Length(work)] <> '\' then
          work := work + '\';
       FExternalFilterPath := work;
     end;
  end;
  Result := FExternalFilterPath;
end;

//------------------------------------------------------------------------------
// Helper function to get the system path for Documents
// In this case Folder = CSIDL_PERSONAL
//------------------------------------------------------------------------------
function TVC_Grid_CheckerX.GetSystemPath(Folder: Integer): string;
var
  PIDL: PItemIDList;
  Path: LPSTR;
  AMalloc: IMalloc;
begin
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
  if SHGetPathFromIDList(PIDL, Path) then
    Result := Path;
  SHGetMalloc(AMalloc);
  AMalloc.Free(PIDL);
  StrDispose(Path);
end;

//------------------------------------------------------------------------------
// Function to determine whether the Sample_Admin_Areas table (added in 6.14)
// exists or not
//------------------------------------------------------------------------------
function TVC_Grid_CheckerX.DoesSampleAdmineAreasExist: boolean;
var theSQL: string;
begin
  Result := False;
  theSQL := 'IF EXISTS (SELECT TABLE_NAME ' +
               'FROM INFORMATION_SCHEMA.TABLES ' +
               'WHERE TABLE_TYPE=''BASE TABLE'' AND TABLE_NAME = ''Sample_Admin_Areas'') ' +
            'SELECT 1 ELSE	SELECT 0';
  with ADOQuery do
  begin
    SQL.Clear;
    SQL.Add(theSQL);
    Open;
    if RecordCount > 0 then
      Result := (Fields[0].AsInteger = 1);
    Close;
  end;
  FSampleAdminAreasExists := Result;
end;

//=============================================================================
initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TVC_Grid_CheckerX,
    Class_VC_Grid_CheckerX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
