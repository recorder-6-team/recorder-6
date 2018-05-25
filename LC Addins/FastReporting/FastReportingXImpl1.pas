unit FastReportingXImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils,strutils, Classes, Graphics, Controls, Forms, Dialogs,Registry,
  ActiveX, AxCtrls, LCFastReporting_TLB, StdVcl,Recorder2000_Tlb, DB, ADODB,ADoInt,
  frxClass, frxADOComponents, frxDesgn, frxDCtrl, StdCtrls, Menus,
   madListHardware,
  madListProcesses,
  madListModules,
  madExcept, frxMap, fs_ipascal, frxExportBaseDialog, frxExportDOCX,
  frxExportPDF, frxExportXLS, frxExportPPTX, frxTableObject;
resourcestring
    RESTR_FAILING_TO_LOAD_REPORT = 'This report is failing to load';
    RESTR_FAILING_TO_CONNECT = 'A Report Result window must be open to use this feature. Press cancel to exit';
    RESTR_NO_LICENCE = 'Some function are not available as the addin is not licenced for Recorder 6 version - ';
    RESTR_PLEASE_SELECT = 'Please select a report';
    RESTR_NO_DATA = 'No data to report on';

type
  TFastReportingX = class(TActiveForm, IFastReportingX,IRecorderAddin,
  INewAction,IFormCaption,Idialog )
    frxReport1: TfrxReport;
    frxDialogControls1: TfrxDialogControls;
    frxDesigner1: TfrxDesigner;
    frxADOComponents1: TfrxADOComponents;
    btnDesignReport: TButton;
    lblReportId: TLabel;
    Label1: TLabel;
    lstReports: TListBox;
    Label2: TLabel;
    btnRunReport: TButton;
    cbAllReports: TCheckBox;
    btnDesignSelected: TButton;
    Label3: TLabel;
    frxDOCXExport1: TfrxDOCXExport;
    fsPascal1: TfsPascal;
    frxMapObject1: TfrxMapObject;
    btnRefresh: TButton;
    frxPPTXExport1: TfrxPPTXExport;
    frxXLSExport1: TfrxXLSExport;
    frxPDFExport1: TfrxPDFExport;
    Copyro: TfrxReportTableObject;
    procedure ActiveFormDestroy(Sender: TObject);
    procedure btnDesignReportClick(Sender: TObject);
    procedure cbAllReportsClick(Sender: TObject);
    procedure btnDesignSelectedClick(Sender: TObject);
    procedure btnRunReportClick(Sender: TObject);
    procedure ActiveFormDeactivate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);

  private
    {My  Private declarations }
    FConnection : TADOConnection;
    FOldConnection : _Connection;
    FReportIdentifier : string;
    Flicence : boolean;
    FOrderBy : string;
    { Private declarations }
    FEvents: IFastReportingXEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);

    {My  Private procedures and functions }
    procedure SetUpForm;
    function  GetConnection: Boolean;
    function  LoadNewReport : Boolean;
    function  ButtonsEnabled(Value: boolean)  : Boolean;
    function  GetFastReportPath : string;
    function  PopulateReportList(Value: boolean) : integer;
    function  GetGroupFromFileName(Value: string) : string;
    function  ConnectionIsActive :Boolean;
    Procedure RunReport;
    function  Scramble(const ACode: String): String;
    procedure CheckPathExists(PathName: String);
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
    // INewAction
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;

    { IFormCaption}
    function Get_FormCaption: WideString; safecall;

    {IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;


  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TFastReportingX }

procedure TFastReportingX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_FastReportingXPage); }
end;

procedure TFastReportingX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IFastReportingXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TFastReportingX.Initialize;
begin
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

function TFastReportingX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TFastReportingX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TFastReportingX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TFastReportingX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TFastReportingX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TFastReportingX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TFastReportingX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TFastReportingX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TFastReportingX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TFastReportingX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TFastReportingX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TFastReportingX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TFastReportingX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TFastReportingX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TFastReportingX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TFastReportingX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TFastReportingX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TFastReportingX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TFastReportingX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TFastReportingX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TFastReportingX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TFastReportingX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TFastReportingX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TFastReportingX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TFastReportingX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TFastReportingX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TFastReportingX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TFastReportingX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TFastReportingX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TFastReportingX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TFastReportingX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TFastReportingX.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TFastReportingX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TFastReportingX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TFastReportingX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TFastReportingX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TFastReportingX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TFastReportingX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TFastReportingX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TFastReportingX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TFastReportingX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TFastReportingX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TFastReportingX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TFastReportingX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TFastReportingX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TFastReportingX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

function TFastReportingX.Get_Description: WideString;
begin
  result:= 'LC - Fast reportin addin for Recorder 6';
end;

function TFastReportingX.Get_ImageFileName: WideString;
begin
  result := 'FastReport.bmp';
end;

function TFastReportingX.Get_Name: WideString;
begin
  result := 'Fast reporting'
end;

procedure TFastReportingX.Install(const iInstalledFilePath: WideString);
begin

end;

function TFastReportingX.Get_ActionCaption: WideString;
begin
  result := 'Fast Reporting';
end;



function TFastReportingX.Get_CanAddToToolbar: WordBool;
begin
  result := true;
end;

function TFastReportingX.Get_DimmedImageFilename: WideString;
begin
  result := 'default.bmp';
end;

function TFastReportingX.Get_DisabledImageFileName: WideString;
begin
  result := 'default.bmp';
end;

function TFastReportingX.Get_Hint: WideString;
begin
  result := 'Fast Reporting - from Filter Results'
end;

function TFastReportingX.Get_ParentMenu: WideString;
begin
   result := 'Tools'
end;


function TFastReportingX.Get_FormCaption: WideString;
begin
   result := 'Fast Report';
   SetUpForm;
end;

procedure TFastReportingX.SetUpForm;
var IResult: _Recordset;
begin
  if not GetConnection then begin
    ButtonsEnabled(false);
    showmessage (RESTR_FAILING_TO_CONNECT);
  end else begin
    ButtonsEnabled(true);
    FReportIdentifier := '';
    Try
      iResult:= FConnection.Execute('Select Report_Id From #Report_Output');
      FReportIdentifier := IResult.Fields.Item[0].Value;
      PopulateReportList(false);
    except
      FReportIdentifier := 'Not specified';
      PopulateReportList(true);
    end;
    label3.Caption := GetFastReportPath;
    FOrderBy := '';
    Try
      iResult:= FConnection.Execute('Select Report_Order From #Report_Output');
      FOrderBy := ' ORDER BY ' + IResult.Fields.Item[0].Value;
    except
      FOrderBy := '';
      Try
        iResult:= FConnection.Execute('Select * From #Report_Output');
      Except
        ButtonsEnabled(false);
        showmessage (RESTR_FAILING_TO_CONNECT);
      end;
    end;
    label3.Caption := GetFastReportPath;


  end;
lblReportId.caption := FReportIdentifier;

end;

 //------------------------------------------------------------------------
// Get a connection to the database
//------------------------------------------------------------------------
function TFastReportingX.GetConnection: Boolean;
var
IRecorder : IRECORDER2000;
lVersion : WideString;
begin
  FLicence := true;
  result:= false;
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  lVersion :=  Irecorder.Version;
  if strtofloat(leftstr(lVersion,4)) >  6.29 then begin
    showmessage (RESTR_NO_LICENCE + lVersion);
    FLicence := false;
  end
  else begin
    if Irecorder.ReportResults = nil then begin
      showmessage (RESTR_NO_DATA);
      label3.Caption := '';
    end
    else begin
      FConnection:= TADOConnection.Create(nil);
      FConnection.ConnectionObject := Irecorder.ReportResults.ReportConnection as ADOInt._Connection;
      FOldConnection := FConnection.ConnectionObject;
      frxAdoComponents1.DefaultDatabase := FConnection;
      FConnection.open;
      Result := FConnection.Connected;
    end;
  end;
end;


procedure TFastReportingX.ActiveFormDestroy(Sender: TObject);
begin
  FrxADOComponents1.DefaultDatabase := nil;
  // FrxADOComponent1.Free;
  FConnection.ConnectionObject := FOldConnection;
  FConnection.Free;

end;


function TFastReportingX.LoadNewReport: Boolean;
var
DataPage :TfrxDataPage;
Page: TfrxReportPage;
DataQuery :TFrxADoQuery;
DataBand : TfrxMasterData;
Band : TfrxBand;

begin
  Try
  frxReport1.Clear;
  frxReport1.FileName := 'Untitled.fr3';
  TfrxDataPage.Create(frxReport1);
  Page := TfrxReportPage.Create(frxReport1);
  Page.CreateUniqueName;
  DataPage := TfrxDataPage.Create(frxReport1);
  DataPage.CreateUniqueName;
  DataQuery := TFrxADOQuery.Create(DataPage);
  DataQuery.SQl.Text := 'Select * from #Report_Output' + FOrderby;
  DataQuery.Name := 'F';
  frxReport1.DataSets.Add(DataQuery);
  Band := TfrxReportTitle.create(page);
  Band.CreateUniqueName;
  Band.Top := 0;
  Band.Height := 20;
  DataBand := TfrxMasterData.Create(page);
  DataBand.CreateUniqueName;
  DataBand.DataSet := DataQuery;
  DataBand.Top  := 100;
  DataBand.Height := 20;

  frxReport1.DesignReport;
  Result := true;
 Except
  Result := false;
 End;


end;


procedure TFastReportingX.btnDesignReportClick(Sender: TObject);
begin
  {Design Report}
  if ConnectionIsActive then
    if not LoadNewReport then begin
      showmessage (RESTR_FAILING_TO_LOAD_REPORT)
    end

end;

function TFastReportingX.ButtonsEnabled(Value : boolean ): Boolean;
begin
    Result := value;
    btnRunReport.enabled := value;
    btnDesignReport.enabled := value;
    btnDesignSelected.enabled := value;
    if Not FLicence then begin
      Result := false;
      btnDesignReport.enabled := false;
      btnDesignSelected.enabled := false;
    end;

end;

function TFastReportingX.GetFastReportPath: string;
 var
R6Registry : TRegistry;
begin
  R6Registry := TRegistry.Create;
  try
    //r6registry.rootkey := 1
    if R6Registry.OpenKey('Software\Dorset Software\Recorder 6\Settings', FALSE) then
      begin
        Result :=  R6Registry.Readstring('Report Path') ;
        CheckPathExists(Result);
      end;
  finally
    R6Registry.free;
  end;
end;


procedure TFastReportingX.cbAllReportsClick(Sender: TObject);
begin
  if cbAllReports.checked then begin
    PopulateReportList (True);
  end
  else begin
    PopulateReportList (false);
  end;
end;

function TFastReportingX.PopulateReportList(Value: boolean): integer;
var lFastReportPath: string;
MySearch: TSearchRec;
FindResult: Integer;

begin
  Screen.Cursor := crHourGlass;
  lstReports.Clear;
  lFastReportPath := GetFastReportPath;
  FindResult:=FindFirst(lFastReportPath +'\*.FR3', faAnyFile, MySearch);
  if (MySearch.Name<>'.')and(MySearch.Name<>'..') and
    ((GetGroupFromFileName(MySearch.Name) =
    GetgroupFromFileName(FReportIdentifier))
    or (cbAllReports.Checked = true)) then
    lstReports.items.add (MySearch.Name);
  while FindNext(MySearch)=0 do
  begin
    if (MySearch.Attr<>faDirectory)and
      (MySearch.Name<>'.')and
      (MySearch.Name<>'..') and
      ((GetGroupFromFileName(MySearch.Name) =
      GetgroupFromFileName(FReportIdentifier))
      or (cbAllReports.Checked = true)) then
      lstReports.items.add (MySearch.Name);

  end;

  Screen.Cursor := crdefault;
  Result := lstReports.Count;

end;

function TFastReportingX.GetGroupFromFileName(Value: string): string;
var i : integer;
begin
  Result := Value;
  i := ansipos('-',value);
  if i > 1 then
    Result := UpperCase(leftstr(value,i-1));


end;

procedure TFastReportingX.btnDesignSelectedClick(Sender: TObject);
var reportName : string;
    nItem : integer;
begin
 if ConnectionIsActive then begin
  // FHasChanged := true;
  // btnDesignReport.enabled := false
   nitem:= lstReports.ItemIndex;
   if nItem >= 0 then begin
     reportName := GetFastReportPath + lstReports.Items [nitem];
     if Not frxReport1.Loadfromfile(ReportName) then
       showmessage (RESTR_FAILING_TO_LOAD_REPORT)
     else
       frxReport1.DesignReport();
    end
   else
     showmessage (RESTR_PLEASE_SELECT);
 end;
end;

procedure TFastReportingX.btnRunReportClick(Sender: TObject);
begin
 RunReport;
end;
procedure TFastReportingX.RunReport;
var reportName : string;
    nItem : integer;
begin
 if ConnectionIsActive then begin
   nitem:= lstReports.ItemIndex;
   if nItem >= 0 then begin
     reportName := GetFastReportPath + lstReports.Items [nitem];
     if  frxReport1.Loadfromfile(ReportName) then
       frxReport1.ShowReport()
     else
       showmessage (RESTR_FAILING_TO_LOAD_REPORT);
   end
   else
     showmessage (RESTR_PLEASE_SELECT);
 end;
end;

function TFastReportingX.ConnectionIsActive: Boolean;
var
IRecorder : IRECORDER2000;
begin
  Result := true;
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  If Irecorder.ReportResults = nil then begin
    Result := false;
    showmessage (RESTR_FAILING_TO_CONNECT);
  end;

end;

procedure TFastReportingX.DoCancel;
begin

end;

function TFastReportingX.DoOk: WordBool;
begin
 Result := true;
 RunReport;
end;

function TFastReportingX.Get_Height: Integer;
begin
  Result:= 600;
end;

function TFastReportingX.Get_Width: Integer;
begin
  Result := 350;
end;

procedure TFastReportingX.ActiveFormDeactivate(Sender: TObject);
begin
    FrxADOComponents1.DefaultDatabase := nil;
  // FrxADOComponent1.Free;
  FConnection.ConnectionObject := FOldConnection;
  FConnection.Free;
end;

procedure TFastReportingX.btnRefreshClick(Sender: TObject);
begin
  cbAllReports.Checked := false;
  PopulateReportList(false);
end;

function  TFastReportingX.Scramble(const ACode: String): String;
var
	i: Integer;
	lTotal: Int64;
	lString: String;
	lSpare: String;
begin
	lTotal := 0;
	Result := '0000';
	// Function is not case sensitive.
	lString  := UpperCase(ACode);
	// Stores the built-in random number generator's seed.
	Randseed := 6531; {6527}

	//  Creates an integer total from the sum of (the ASCII values of each character
	//  in the Key Sting multiplied by a random umber in the ranger 0 to 501)
	for i := 1 to Length(lString) do
		lTotal := lTotal + ((Random(500) + 1) * Ord(lString[i]));

	// if lTotal is greater than FFFF we want to use the integer remainder of 1Total/4093
	if lTotal > 65535 then
		lTotal := lTotal mod 4093;

	//Convert to Hexadeciamal
	lSpare := IntToHex(lTotal, 4);
	// Swaps the order of the characters round in lSpare
	for i := 1 to 4 do
		Result[i] := lSpare[5 - i];
end;  // Scramble

procedure TFastReportingX.CheckPathExists(PathName: String);
var
  sSubPath: String;
begin
  //If a drive is specified, we assume the path does exist
  if PathName[Length(PathName)] <> ':' then
  begin
	  //Remove trailing \ if required
	  if PathName[Length(PathName)] = '\' then
	    PathName:= Copy(PathName, 0, Length(PathName) - 1);

	  //If directory does not exist, attempt to create it
	  if not DirectoryExists(PathName) then
	  begin
	    //Use ExtractFileDir to get the subdirectory
	    sSubPath:= ExtractFileDir(PathName);

	    //Check subdirectory exists
	    CheckPathExists(sSubPath);
       //Create new directory
      SysUtils.CreateDir(PathName);


   end;
  end;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TFastReportingX,
    Class_FastReportingX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
