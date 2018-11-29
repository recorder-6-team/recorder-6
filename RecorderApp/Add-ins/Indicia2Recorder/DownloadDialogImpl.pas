unit DownloadDialogImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, Indicia2Recorder_TLB, StdVcl, ComCtrls, StdCtrls,
  ExtCtrls, Recorder2000_TLB, IdBaseComponent, IdComponent, uLkJSON,
  IdTCPConnection, IdTCPClient, IdHTTP, IdMultipartFormData, AdoDb, Variants,
  SHFolder, IdSSLOpenSSL;

type
  EDownloadDialogConfigException = class(Exception);

  TDownloadDialog = class(TActiveForm, IDownloadDialog, IRecorderAddin, IDialog, INewAction)
    pnlInfo: TPanel;
    ProgressBar: TProgressBar;
    mmLog: TMemo;
    Panel1: TPanel;
    pnlSelectDownload: TPanel;
    lblSurvey: TLabel;
    lblStartDate: TLabel;
    lblEndDate: TLabel;
    lblIntoSurvey: TLabel;
    rgDownloadType: TRadioGroup;
    cmbSurvey: TComboBox;
    dtpStartDate: TDateTimePicker;
    dtpEndDate: TDateTimePicker;
    cmbIntoSurvey: TComboBox;
    pnlLogin: TPanel;
    lblEmail: TLabel;
    lblPassword: TLabel;
    lblLoggedInAs: TLabel;
    eEmail: TEdit;
    ePassword: TEdit;
    btnLogin: TButton;
    lblLoginInstruct: TLabel;
    cbLimitToAccepted: TCheckBox;
    IdHTTP1: TIdHTTP;
    procedure btnLoginClick(Sender: TObject);
    procedure cmbSurveyChange(Sender: TObject);
  private
    { Private declarations }
    FEvents: IDownloadDialogEvents;
    FSecret: string;
    FFirstName: string;
    FSurname: string;
    FEmail: string;
    FRecorder: IRecorder2000;
    FConnection: TADOConnection;
    FSurveys: TStringList;
    FDoneSignatures: TStringList;
    FURL: string;
    FAppSecret: string;
    FRemoteSiteID: string;
    FRemoteSite: string;
    FDrupalVersion: string;
    FDone: integer;
    FTotal: integer;
    FSettingsFolder: string;
    FAttrs: TStringList;
    FAttrTermMappings: TStringList;
    FSmpAttrs: string;
    FOccAttrs: string;
    FRunning: boolean;
    Ffsiso: TFormatSettings;
    FKnownPeople: TStringList;
    FFileLog: TStringList;
    FEventsCreated: integer;
    FEventsUpdated: integer;
    FSamplesCreated: integer;
    FSamplesUpdated: integer;
    FOccurrencesCreated: integer;
    FOccurrencesUpdated: integer;
    FOccurrencesRejected: integer;
    FPeopleRecordInfo: TStringList;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    procedure DoLogin;
    procedure EnableDownloadControls;
    procedure FetchDownloadOptions;
    function GetDownloadType: string;
    function DateToIsoStr(date: TDateTime): string;
    procedure Log(const msg: string);
    procedure LogFile(const msg: string);
    procedure ResetLogFile;
    procedure SaveLogFile;
    procedure ImportRecords(records: TlkJSONbase; surveyKey: string);
    procedure ConnectToDb;
    procedure DisconnectFromDb;
    procedure CreateTempTables;
    procedure CleanupTempTables;
    procedure EmptyTempTables;
    procedure ExecuteSql(sql: string);
    function IdToKey(id: variant): string;
    procedure CreateSample(sampleKey: string; surveyKey: string; thisrec: TStringList);
    procedure CreateOccurrence(occKey, sampleKey: String; thisrec: TStringList);
    procedure CreateMedia(occKey: String; thisrec: TStringList);
    function GetIndividual(indiciaId: string; name: string): string;
    function GetSampleTypeKey(sampleTypeLabel: string): string;
    procedure eachSurvey(ElName: string; Elem: TlkJSONbase; data: pointer;
      var Continue: Boolean);
    procedure PopulateRecorderSurveys;
    procedure LoadSurveyBatches;
    procedure GetStringsFromJsonRec(thisrec: TStringList;
      rec: TlkJSONbase);
    function ConvertSrefSystem(input: string): string;
    procedure CreateIndividual(indkey, name: string);
    procedure GetConnectionToIndicia;
    function GetFolder(csidl: Integer): String;
    procedure LoadAttrConfig;
    procedure CreateData(thisrec: TStringList; table, key, prefix, fieldTag: string);
    function CreateConnectionFile: boolean;
    procedure SaveSettings;
    procedure LoadSettings(login: boolean);
    function EscapeSqlLiteral(literal: string): string; overload;
    function EscapeSqlLiteral(literal: string; maxlen: integer): string; overload;
    procedure LoadKnownPeople;
    procedure ImportSurvey(indiciaSurveyId: integer; surveyKey, quality: string);
    function GetUrl(const endpoint: string): string;
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
    // Recorder addin methods
    // IRecorderAddin
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    // IDialog
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    // INewAction
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ, Math, VagueDate, DECUtil, DECCipher, DECHash, DECFmt,
    registry;

resourcestring
  ResStr_AddinDescription = 'Download records you have access to in iRecord or a Drupal Indicia website.';
  ResStr_AddinHint = 'Download records you have access to in iRecord.';
  ResStr_AddinTitle = 'Indicia2Recorder';
  ResStr_CouldNotFindTVK = 'Could not find a taxon version key for %s (%s) in the Name Server.';
  ResStr_DeterminedByComment = 'Determined by person named %s';
  ResStr_Done = 'Done';
  ResStr_DownloadStarting = 'Download starting';
  ResStr_FetchingRecords = 'Fetching some records';
  ResStr_InvalidConfigLine = 'Invalid config setting on line %d.';
  ResStr_InvalidData = 'Invalid data';
  ResStr_InvalidResponse = 'Invalid response: %s';  
  ResStr_LoggedInAs = 'Logged in as %s %s';
  ResStr_NoConfigFile = 'There is no configuration file available to define connection settings for the remote site. ' +
      'If you are an administrator wanting to create a connection then please answer the following questions.';
  ResStr_NoConnectionAvailable =
      'No connection available. Please contact an administrator who can help you set up the connection details.';
  ResStr_ParsingRecords = 'Parsing records';
  ResStr_ProcessedNRecords = 'Processed %d records';
  ResStr_ProvideSharedSecret =
      'Please provide the Shared App Secret of the website you want to download records from';
  ResStr_ProvideSiteID =
      'Please provide the Site ID for records created in Recorder from the website you want to download records from';
  ResStr_ProvideUrl = 'Please provide the URL of the website you want to download records from';      
  ResStr_ProvideWebsiteTitle = 'Please provide the title of the website you want to download records from';
  ResStr_ProvideDrupalVersion = 'Please provide the Drupal version, either 7 or 8';
  ResStr_PleaseLogInIrecord = 'Please log in before downloading any records.';
  ResStr_ReceivedNRecords = 'Received %d records';
  ResStr_RecordedByComment = 'Recorded by person named %s';
  ResStr_RecordsICanCollate = 'Records I can collate';
  ResStr_RecordsICanVerify = 'Records I can verify';
  ResStr_RejectedStatusFromIndicia = 'Rejected';
  ResStr_SameSettingsRepeat =
      'You just downloaded records using exactly these settings. Are you sure you want to do it again?';
  ResStr_SelectDownloadType = 'Please select a download type.';
  ResStr_SelectSurveyToImport = 'Please select a survey to import.';
  ResStr_SelectSurveyToImportInto = 'Please select a survey to import into.';
  ResStr_SettingsFolderMissing = 'The Indicia2Recorder folder does not exist in My Documents or Public Documents';
  ResStr_SensitiveLocationNameFromIndicia = 'Sensitive';
  ResStr_SkippingSensitiveRecord = 'Skipping sensitive record %s';
  ResStr_SpecifyEmailAddress = 'Please specify your email address registered against your %d account.';
  ResStr_SpecifyPassword = 'Please specify the password for your %s account.';
  ResStr_SurveyTypeTermlistRequired = 'Please create a Survey Type termlist entry called Indicia ' +
      'and at least one survey of this type to import into';
  ResStr_ToolsMenuName = 'Tools';
  ResStr_VerifiedStatusFromIndicia = 'Verified';

var
  ACipherClass: TDECCipherClass = TCipher_Rijndael;
  ACipherMode: TCipherMode = cmCBCx;
  AHashClass: TDECHashClass = THash_Whirlpool;
  ATextFormat: TDECFormatClass = TFormat_Mime64;
  AKDFIndex: LongWord = 1;

function Encrypt(const AText: String; const APassword: String): String; overload;
var
  ASalt: Binary;
  AData: Binary;
  APass: Binary;
begin
  with ValidCipher(ACipherClass).Create, Context do
  try
    ASalt := RandomBinary(16);
    APass := ValidHash(AHashClass).KDFx(APassword[1], Length(APassword) * SizeOf(APassword[1]), ASalt[1], Length(ASalt), KeySize, TFormat_Copy, AKDFIndex);
    Mode := ACipherMode;
    Init(APass);
    SetLength(AData, Length(AText) * SizeOf(AText[1]));
    Encode(AText[1], AData[1], Length(AData));
    Result := ValidFormat(ATextFormat).Encode(ASalt + AData + CalcMAC);
  finally
    Free;
    ProtectBinary(ASalt);
    ProtectBinary(AData);
    ProtectBinary(APass);
  end;
end;

function Decrypt(const AText: String; const APassword: String): String; overload;
var
  ASalt: Binary;
  AData: Binary;
  ACheck: Binary;
  APass: Binary;
  ALen: Integer;
begin
  with ValidCipher(ACipherClass).Create, Context do
  try
    ASalt := ValidFormat(ATextFormat).Decode(AText);
    ALen := Length(ASalt) - 16 - BufferSize;
    AData := System.Copy(ASalt, 17, ALen);
    ACheck := System.Copy(ASalt, ALen + 17, BufferSize);
    SetLength(ASalt, 16);
    APass := ValidHash(AHashClass).KDFx(APassword[1], Length(APassword) * SizeOf(APassword[1]), ASalt[1], Length(ASalt), KeySize, TFormat_Copy, AKDFIndex);
    Mode := ACipherMode;
    Init(APass);
    SetLength(Result, ALen div SizeOf(AText[1]));
    Decode(AData[1], Result[1], ALen);
    if ACheck <> CalcMAC then
      raise Exception.Create(ResStr_InvalidData);
  finally
    Free;
    ProtectBinary(ASalt);
    ProtectBinary(AData);
    ProtectBinary(ACheck);
    ProtectBinary(APass);
  end;
end;

{$R *.DFM}

{ TDownloadDialog }

procedure TDownloadDialog.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_DownloadDialogPage); }
end;

procedure TDownloadDialog.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IDownloadDialogEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TDownloadDialog.Initialize;
var
  y, m, d: word;
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
  DecodeDate(Date, y, m, d);
  dtpStartDate.Date := EncodeDate(y-1, m, d);
  dtpEndDate.Date := Date;
  FSettingsFolder := '';
  FSurveys := TStringList.Create;
  FDoneSignatures := TStringList.Create;
  FKnownPeople := TStringList.Create;
  FRunning := false;
  FSecret := '';
  // A log file that contains useful information on the sync results.
  FFileLog := TStringList.Create;
  FPeopleRecordInfo := TStringList.Create;
end;

function TDownloadDialog.Get_Active: WordBool;
begin
  Result := Active;
end;

function TDownloadDialog.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TDownloadDialog.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TDownloadDialog.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TDownloadDialog.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TDownloadDialog.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TDownloadDialog.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TDownloadDialog.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TDownloadDialog.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TDownloadDialog.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TDownloadDialog.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TDownloadDialog.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TDownloadDialog.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TDownloadDialog.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TDownloadDialog.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TDownloadDialog.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TDownloadDialog.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TDownloadDialog.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TDownloadDialog.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TDownloadDialog.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TDownloadDialog._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TDownloadDialog.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TDownloadDialog.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TDownloadDialog.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TDownloadDialog.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TDownloadDialog.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TDownloadDialog.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TDownloadDialog.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TDownloadDialog.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TDownloadDialog.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TDownloadDialog.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TDownloadDialog.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TDownloadDialog.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TDownloadDialog.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TDownloadDialog.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TDownloadDialog.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TDownloadDialog.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TDownloadDialog.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TDownloadDialog.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TDownloadDialog.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TDownloadDialog.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TDownloadDialog.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TDownloadDialog.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TDownloadDialog.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TDownloadDialog.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TDownloadDialog.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

{*
 * Cancel the dialog.
 *}
procedure TDownloadDialog.DoCancel;
begin
  // no action
end;

function TDownloadDialog.DoOk: WordBool;
var
  quality, signature: string;
  batch: TStringList;
  i: integer;
  indiciaSurveyId: integer;
  surveyKey: string;
begin
  result:=false;
  if (self.ActiveControl=eEmail) or (self.ActiveControl=ePassword) then begin
    // return key fired default action, but whilst in login controls so user expects login not run
    btnLoginClick(nil);
    exit;
  end;
  if FRunning then
    exit;
  if pnlLogin.Enabled then begin
    ShowMessage(ResStr_PleaseLogInIrecord);
    eEmail.SetFocus;
  end
  else if rgDownloadType.ItemIndex=-1 then begin
    ShowMessage(ResStr_SelectDownloadType);
    rgDownloadType.SetFocus;
  end
  else if cmbSurvey.ItemIndex=-1 then begin
    ShowMessage(ResStr_SelectSurveyToImport);
    cmbSurvey.SetFocus;
  end
  else if cmbIntoSurvey.enabled and (cmbIntoSurvey.ItemIndex=-1) then begin
    ShowMessage(ResStr_SelectSurveyToImportInto);
    cmbIntoSurvey.SetFocus;
  end
  else
  begin
    quality := '!R';
    if cbLimitToAccepted.Checked then
      quality := 'V';

    signature := cmbSurvey.Items[cmbSurvey.ItemIndex] + '|'
        + DateToIsoStr(dtpStartDate.Date) + '|' + DateToIsoStr(dtpEndDate.Date) + '|'
        + FSurveys[cmbIntoSurvey.ItemIndex] + '|' + quality;
    if FDoneSignatures.IndexOf(signature)>=0 then
      if MessageDlg(ResStr_SameSettingsRepeat, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
    Log('---------------------------------');
    Log(ResStr_DownloadStarting);
    Log('---------------------------------');
    ResetLogFile;
    // Get a short date format for iso dates returned from the Indicia reports
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, Ffsiso);
    Ffsiso.DateSeparator := '-';
    Ffsiso.ShortDateFormat := 'yyyy-mm-dd';
    FRunning := true;
    SaveSettings;
    FDoneSignatures.Add(signature);
    ConnectToDb;
    CreateTempTables;
    LoadAttrConfig;
    LoadKnownPeople;
    try
      if cmbSurvey.ItemIndex > 0 then
      begin
        if cmbSurvey.Items.Objects[cmbSurvey.ItemIndex] = nil then
        begin
          batch := TStringList.Create;
          try
            // single a batch of surveys
            batch.LoadFromFile(FSettingsFolder + cmbSurvey.Items[cmbSurvey.ItemIndex]);
            for i :=0 to batch.Count - 1 do
            begin
              indiciaSurveyId := StrToInt(batch.names[i]);
              surveyKey := batch.values[batch.names[i]];
              ImportSurvey(indiciaSurveyId, surveyKey, quality);
            end;
          finally
            batch.free;
          end;
        end
        else
          // single selected survey
          ImportSurvey(
            integer(cmbSurvey.Items.Objects[cmbSurvey.ItemIndex]),
            FSurveys[cmbIntoSurvey.ItemIndex],
            quality
          );
        end
      else
        // all surveys
        ImportSurvey(0, FSurveys[cmbIntoSurvey.ItemIndex], quality);
      Log(ResStr_Done);
    finally
      FAttrs.Free;
      for i := 0 to FAttrTermMappings.Count-1 do begin
        FAttrTermMappings.Objects[i].Free;
      end;
      FAttrTermMappings.Free;
      SaveLogFile;
      CleanupTempTables;
      DisconnectFromDb;
      FRunning := false;
    end;
  end;
end;

procedure TDownloadDialog.ImportSurvey(indiciaSurveyId: integer; surveyKey, quality: string);
var records, responseObj: TlkJSONbase;
  request: TIdMultiPartFormDataStream;
  response: string;
  i: integer;
const
  LIMIT=100;
begin
  records := nil;
  FDone := 0;
  repeat
    Log(ResStr_FetchingRecords);
    request := TIdMultiPartFormDataStream.Create;
    try
      request.AddFormField('email', FEmail);
      request.AddFormField('appsecret', FAppSecret);
      request.AddFormField('usersecret', FSecret);
      request.AddFormField('type', GetDownloadType);
      request.AddFormField('date_from', DateToIsoStr(dtpStartDate.Date));
      request.AddFormField('date_to', DateToIsoStr(dtpEndDate.Date));
      request.AddFormField('limit', IntToStr(LIMIT));
      request.AddFormField('smpAttrs', FSmpAttrs);
      request.AddFormField('occAttrs', FOccAttrs).ContentTransfer := '';
      request.AddFormField('quality', quality);
      if FDone=0 then
        // first time through, so get the grand total
        request.AddFormField('wantCount', '1')
      else
        request.AddFormField('offset', IntToStr(FDone));
      if indiciaSurveyId<>0 then
        request.AddFormField('survey_id', IntToStr(indiciaSurveyId));
      response := idHttp1.Post(GetUrl('download'), request);
    finally
      request.Free;
    end;
    Log(ResStr_ParsingRecords);
    responseObj := TlkJSON.ParseText(response);
    if FDone=0 then begin
      for i := 0 to responseObj.Count-1 do begin
        if responseObj.Child[i] is TlkJSONobjectmethod then begin
          if TlkJSONobjectmethod(responseObj.Child[i]).Name='records' then
            records := TlkJSONobjectmethod(responseObj.Child[i]).ObjValue
          else if TlkJSONobjectmethod(responseObj.Child[i]).Name='count' then
            FTotal := StrToInt(TlkJSONobjectmethod(responseObj.Child[i]).ObjValue.value)
          else if TlkJSONobjectmethod(responseObj.Child[i]).Name='error' then begin
            LogFile('Error in response from Indicia web services');
            LogFile(TlkJSONobjectmethod(responseObj.Child[i]).ObjValue.value);
            Log('Error in response from Indicia web services');
            Log(TlkJSONobjectmethod(responseObj.Child[i]).ObjValue.value);
          end;
        end;
      end;
    end else
      records := responseObj;
    LogFile('');
    LogFile('Survey: ' + surveyKey);
    if assigned(records) then begin
      Log(Format(ResStr_ReceivedNRecords, [records.Count]));
      LogFile(Format(ResStr_ReceivedNRecords, [records.Count]));
      ImportRecords(records, surveyKey);
      FDone := FDone + records.Count;
    end;
  until FDone>=FTotal;
end;

(**
 * Gets a URL for accessing the web services on the Drupal site. Adapts to
 * Drupal 7 or Drupal 8 to build correct URLs.
 *
 * Endpoint - parameter should be either 'login', 'privileges' or 'download'.
 *)
function TDownloadDialog.GetUrl(const endpoint: string): string;
begin
  if FDrupalVersion = '7' then begin
    if endpoint = 'login' then
      result := FUrl + '/?q=user/mobile/register'
    else if endpoint = 'privileges' then
      result := FUrl + '/?q=user/remote_download/privileges'
    else if endpoint = 'download' then
      result := FUrl + '/?q=user/remote_download/download'
  end
  else if FDrupalVersion = '8' then begin
    if endpoint = 'login' then
      result := FUrl + '/remote_download/login'
    else if endpoint = 'privileges' then
      result := FUrl + '/remote_download/privileges'
    else if endpoint = 'download' then
      result := FUrl + '/remote_download/download'
  end
  else
    raise Exception.Create('Unknown Drupal version in indicia connection info');
end;

(**
 * Load the contents of the knownPeople.txt file if it exists. This gives us mappings for known names
 *)
procedure TDownloadDialog.LoadKnownPeople;
begin
  FKnownPeople.Clear;
  if FileExists(FSettingsFolder + 'knownPeople.txt') then
    FKnownPeople.LoadFromFile(FSettingsFolder + 'knownPeople.txt');
end;

procedure TDownloadDialog.LoadAttrConfig;
var
  i: integer;
  def: string;
  termMappingsFileName: string;
  termMappings: TStringList;
begin
  FAttrs := TStringList.Create;
  FAttrTermMappings := TStringList.Create;
  FSmpAttrs := '';
  FOccAttrs := '';
  if FileExists(FSettingsFolder + 'config.txt') then begin
    FAttrs.LoadFromFile(FSettingsFolder + 'config.txt');
    for i:=0 to FAttrs.Count-1 do begin
      def:=trim(FAttrs.Names[i]);
      // Skip comments and blank lines in the attrs config file.
      if (Copy(def, 1, 1)<>'#') and (def<>'') then begin
        if CompareStr(Copy(def, 1, 8), 'smpAttr:')=0 then begin
          if FSmpAttrs<>'' then FSmpAttrs := FSmpAttrs+',';
          FSmpAttrs := FSmpAttrs + Copy(def, 9, 255);
        end
        else if CompareStr(Copy(def, 1, 8), 'occAttr:')=0 then begin
          if FOccAttrs<>'' then FOccAttrs := FOccAttrs+',';
          FOccAttrs := FOccAttrs + Copy(def, 9, 255);
        end
        else
          raise EDownloadDialogConfigException.Create(Format(ResStr_InvalidConfigLine, [i+1]));
        // Is there a config file for mapping terms for this attribute?
        termMappingsFileName := StringReplace(def, ':', '', []) + 'TermMappings.txt';
        if FileExists(FSettingsFolder + termMappingsFileName) then begin
          termMappings := TStringList.Create;
          termMappings.LoadFromFile(FSettingsFolder + termMappingsFileName);
          FAttrTermMappings.AddObject(def, termMappings);
        end;
      end;
    end;
  end;
end;

function TDownloadDialog.GetFolder(csidl: Integer): String;
var
  i: Integer;
begin
  SetLength(Result, MAX_PATH);
  SHGetFolderPath(0, csidl, 0, 0, PChar(Result));
  i := Pos(#0, Result);
  if i > 0 then begin
    SetLength(Result, Pred(i));
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

procedure TDownloadDialog.GetConnectionToIndicia();
var
  connectionFile, tokens: TStringList;
  encrypted, decrypted: string;
begin
  connectionFile := TStringList.Create;
  tokens := TStringList.Create;
  if not FileExists(FSettingsFolder + 'indiciaConnection.txt') then
    if not CreateConnectionFile then begin
      pnlLogin.Enabled := false;
      pnlSelectDownload.Enabled := false;
      ShowMessage(ResStr_NoConnectionAvailable);
      // lock the dialog out
      FRunning := true;
      exit;
    end;
  try
    connectionFile.LoadFromFile(FSettingsFolder + 'indiciaConnection.txt');
    encrypted := connectionFile.Text;
    decrypted := Decrypt(encrypted, 'brim5tone');
    // Split string on delimiter. Using delimitedText doesn't work as it always
    // splits on spaces.
    tokens.Text := StringReplace(decrypted, '|', sLineBreak, [rfReplaceAll]);
    FURL := tokens[0];
    FAppSecret := tokens[1];
    FRemoteSiteID := tokens[2];
    FRemoteSite := tokens[3];
    if tokens.count > 4 then
      FDrupalVersion := tokens[4]
    else
      FDrupalVersion := '7';
    // if https, then set up the IO Handler. See
    // http://stackoverflow.com/questions/11554003/tidhttp-get-eidiohandlerpropinvalid-error
    if Copy(FUrl, 1, 5) = 'https' then begin
      idHttp1.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create();
    end;
    lblLoginInstruct.Caption := StringReplace(lblLoginInstruct.Caption, 'Indicia', FRemoteSite, [rfReplaceAll]);
    lblEmail.Caption := StringReplace(lblEmail.Caption, 'Indicia', FRemoteSite, [rfReplaceAll]);
    lblPassword.Caption := StringReplace(lblPassword.Caption, 'Indicia', FRemoteSite, [rfReplaceAll]);
  finally
    connectionFile.Free;
    tokens.Free;
  end;
end;

function TDownloadDialog.CreateConnectionFile: boolean;
var
  connectionFile: TStringList;
begin
  result := false;
  if MessageDlg(ResStr_NoConfigFile, mtConfirmation, [mbOk, mbCancel], 0) = mrCancel then
    exit;
  if not InputQuery('Remote Site', ResStr_ProvideUrl, FURL) then
    exit;
  if not InputQuery('Remote Site', ResStr_ProvideSharedSecret, FAppSecret) then
    exit;
  if not InputQuery('Remote Site', ResStr_ProvideSiteID, FRemoteSiteID) then
    exit;
  if not InputQuery('Remote Site', ResStr_ProvideWebsiteTitle, FRemoteSite) then
    exit;
  repeat
    if not InputQuery('Drupal version (7 or 8)', ResStr_ProvideDrupalVersion, FDrupalVersion) then
      exit;
    FDrupalVersion := trim(FDrupalVersion);
  until (FDrupalVersion = '7') or (FDrupalVersion = '8');
  connectionFile := TStringList.Create;
  try
    connectionFile.Add(Encrypt(FURL+'|'+FAppSecret+'|'+FRemoteSiteID+'|'+FRemoteSite+'|'+FDrupalVersion, 'brim5tone'));
    connectionFile.SaveToFile(FSettingsFolder + 'indiciaConnection.txt');
  finally
    connectionFile.Free;
  end;
  result := true;
end;

procedure TDownloadDialog.ImportRecords(records: TlkJSONbase; surveyKey: string);
var i: integer;
  rec: TlkJSONbase;
  doneSamples, thisrec: TStringList;
  sampleKey, occKey: string;
begin
  EmptyTempTables;
  // we only need 1 sample per sample ID, so track this. Saves repeat importing.
  doneSamples := TStringList.Create;
  thisrec := TStringList.Create;
  try
    for i := 0 to records.Count-1 do begin
      rec := records.Child[i];
      thisrec.clear;
      GetStringsFromJsonRec(thisrec, rec);
      if not VarIsNull(rec.Field['sample_id'].Value) then begin
        sampleKey := FRemoteSiteID + IdToKey(rec.Field['sample_id'].Value);
        if (doneSamples.IndexOf(sampleKey)=-1) then
          CreateSample(sampleKey, surveyKey, thisrec);
        doneSamples.Add(sampleKey);
        occKey := FRemoteSiteID + IdToKey(rec.Field['occurrence_id'].Value);
        CreateOccurrence(occKey, sampleKey, thisrec);
        ProgressBar.Position := (FDone+i+1) * 100 div FTotal;
        Application.ProcessMessages;
      end
      else
        log(Format(ResStr_SkippingSensitiveRecord, [rec.Field['recordkey'].Value]));
      end;
    Log(Format(ResStr_ProcessedNRecords, [records.Count]));
  finally
    doneSamples.Free;
    thisrec.free;
  end;
end;

{*
 * Insert or update a sample (plus a 1:1 related survey event) in the database.
 * @todo: survey event recorders
 * @todo: sample recorders
 * @todo: check sensitive records create samples and don't duplicate OK as sample_id not in download
 * @todo: Chunking if lots of records
 * @todo: handle dialog state on completion
 *
 * Nice to have:
 * @todo: progressBar
 * @todo: logs of taxon determinations?
 * @todo: remember previous settings
 * @todo: set a location and vice county admin area??
 * @todo: intelligently set the determination date
 *}
procedure TDownloadDialog.CreateSample(sampleKey: string; surveyKey: string; thisrec: TStringList);
var
  existing: _Recordset;
  vd: TVagueDate;
  locationName, recorder, comment, sampleTypeKey: string;
  sref: string;
begin
  existing := FConnection.Execute('SELECT survey_event_key FROM survey_event WHERE survey_event_key=''' + sampleKey + '''');
  vd.StartDate := StrToDate(thisrec.values['date_start'], Ffsiso);
  vd.EndDate := StrToDate(thisrec.values['date_end'], Ffsiso);
  vd.DateTypeString := thisrec.values['date_type'];
  sref := trim(thisrec.values['entered_sref']);
  if (thisrec.values['projection']='WGS84') then begin
    // need to check for lat longs without a separating comma, which Recorder does not support.
    sref := StringReplace(sref, ', ', ' ', []);
    sref := StringReplace(sref, ' ', ', ', []);
    // Plus we need to convert to include N,E,S,W if these are missing.
    if (Pos('N', sref)=0) and (Pos('S', sref)=0) then begin
      if (Copy(sref, 1, 1)='-') then begin
        // Insert an S as Southern Hemisphere
        sref := StringReplace(sref, ', ', 'S, ', []);
        // don't need the minus now
        sref := Copy(sref, 2, 255);
      end
      else
        // Insert an N as Northern Hemisphere
        sref := StringReplace(sref, ', ', 'N, ', []);
      if Pos('-', sref)>0 then begin
        // Insert a W as Western Hemisphere
        sref := sref + 'W';
        // don't need the minus now
        sref := StringReplace(sref, '-', '', []);
      end
      else
        // Insert an E as Eastern Hemisphere
        sref := sref + 'E';
    end;
  end;
  locationName := thisrec.values['location_name'];
  if (locationName<>'') and (thisrec.values['vicecounty']<>'') then
    locationName := locationName + ', ';
  locationName := locationName + thisrec.values['vicecounty'];
  recorder := GetIndividual(thisrec.values['recorder_person_id'], thisrec.values['recorder']);
  comment := thisrec.values['sample_comment'];
  sampleTypeKey := GetSampleTypeKey(thisrec.values['sample_method']);
  if (recorder = 'NBNSYS0000000004') and (thisrec.values['recorder']<>'') then
    // since we couldn't resolve the name key, don't lose the recorder name data
    comment := Format(ResStr_RecordedByComment, [thisrec.values['recorder']]) + #13#10 + comment;
  if existing.RecordCount=0 then begin
    // Insert new event
    FConnection.Execute('INSERT INTO survey_event (survey_event_key, vague_date_start, vague_date_end, vague_date_type, '+
        'spatial_ref, spatial_ref_system, lat, long, spatial_ref_qualifier, location_name, survey_key, '+
        'entered_by, entry_date) ' +
        'VALUES(''' + sampleKey + ''',' +
        IntToStr(trunc(vd.StartDate)) + ',' +
        IntToStr(trunc(vd.EndDate)) + ',' +
        '''' + vd.DateTypeString + ''',' +
        '''' + thisrec.values['entered_sref'] + ''',' +
        '''' + ConvertSrefSystem(thisrec.values['projection']) + ''',' +
        thisrec.values['lat'] + ',' +
        thisrec.values['long'] + ',' +
        '''Imported'',' +
        '''' + EscapeSqlLiteral(locationName, 100) + ''',' +
        '''' + surveyKey + ''',' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
    Inc(FEventsCreated);
  end else
  begin
    // Update existing event
    FConnection.Execute('UPDATE survey_event SET '+
        'vague_date_start=' + IntToStr(trunc(vd.StartDate)) + ', ' +
        'vague_date_end=' + IntToStr(trunc(vd.EndDate)) + ', ' +
        'vague_date_type=''' + vd.DateTypeString + ''', ' +
        'spatial_ref=''' + thisrec.values['entered_sref'] + ''', ' +
        'spatial_ref_system=''' + ConvertSrefSystem(thisrec.values['projection']) + ''', ' +
        'lat=' + thisrec.values['lat'] + ', ' +
        'long=' + thisrec.values['long'] + ', ' +
        'spatial_ref_qualifier=''Imported'', ' +
        'location_name=''' + EscapeSqlLiteral(locationName, 100) + ''', ' +
        'survey_key=''' + surveyKey + ''', ' +
        'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
        'WHERE survey_event_key=''' + sampleKey + '''');
    Inc(FEventsUpdated);
  end;
  // Add a survey event recorder
  existing := FConnection.Execute('SELECT se_recorder_key FROM survey_event_recorder WHERE se_recorder_key=''' + sampleKey + '''');
  if existing.RecordCount=0 then begin
    // Insert new se recorder
    FConnection.Execute('INSERT INTO survey_event_recorder(se_recorder_key, name_key, survey_event_key, recorder_role_key, '+
        'entered_by, entry_date) ' +
        'VALUES(''' + sampleKey + ''', ' +
        '''' + recorder + ''', ' +
        '''' + sampleKey + ''', ' +
        '''NBNSYS0000000002'', ' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
  end
  else begin
    // Update existing se recorder
    FConnection.Execute('UPDATE survey_event_recorder SET '+
        'name_key=''' + recorder + ''', ' +
        'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
        'WHERE se_recorder_key=''' + sampleKey + '''');
  end;
  // sample table
  existing := FConnection.Execute('SELECT sample_key FROM sample WHERE sample_key=''' + sampleKey + '''');
  if existing.RecordCount=0 then begin
    // Insert new sample
    FConnection.Execute('INSERT INTO sample (sample_key, vague_date_start, vague_date_end, vague_date_type, '+
        'spatial_ref, spatial_ref_system, lat, long, spatial_ref_qualifier, location_name, survey_event_key, '+
        'sample_type_key, comment, entered_by, entry_date) ' +
        'VALUES(''' + sampleKey + ''', ' +
        IntToStr(trunc(vd.StartDate)) + ', ' +
        IntToStr(trunc(vd.EndDate)) + ', ' +
        '''' + vd.DateTypeString + ''', ' +
        '''' + thisrec.values['entered_sref'] + ''', ' +
        '''' + ConvertSrefSystem(thisrec.values['projection']) + ''', ' +
        thisrec.values['lat'] + ', ' +
        thisrec.values['long'] + ', ' +
        '''Imported'', ' +
        '''' + EscapeSqlLiteral(locationName, 100) + ''',' +
        '''' + sampleKey + ''', ' +
        '''' + sampleTypeKey + ''', ' +
        '''' + EscapeSqlLiteral(comment) + ''',' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
    Inc(FSamplesCreated);
  end else
  begin
    FConnection.Execute('UPDATE sample SET '+
        'vague_date_start=' + IntToStr(trunc(vd.StartDate)) + ', ' +
        'vague_date_end=' + IntToStr(trunc(vd.EndDate)) + ', ' +
        'vague_date_type=''' + vd.DateTypeString + ''', ' +
        'spatial_ref=''' + thisrec.values['entered_sref'] + ''', ' +
        'spatial_ref_system=''' + ConvertSrefSystem(thisrec.values['projection']) + ''', ' +
        'lat=' + thisrec.values['lat'] + ', ' +
        'long=' + thisrec.values['long'] + ', ' +
        'spatial_ref_qualifier=''Imported'', ' +
        'location_name=''' + EscapeSqlLiteral(locationName, 100) + ''', ' +
        'survey_event_key=''' + sampleKey + ''', ' +
        'sample_type_key=''' + sampleTypeKey + ''', ' + 
        'comment=''' + EscapeSqlLiteral(comment) + ''', ' +
        'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
        'WHERE sample_key=''' + sampleKey + '''');
    Inc(FSamplesUpdated);
  end;
  // sample recorders table
  existing := FConnection.Execute('SELECT sample_key FROM sample_recorder WHERE sample_key=''' + sampleKey +
      ''' AND se_recorder_key=''' + sampleKey + '''');
  if existing.RecordCount=0 then begin
    // Insert new sample recorder
    FConnection.Execute('INSERT INTO sample_recorder(sample_key, se_recorder_key, '+
        'entered_by, entry_date) ' +
        'VALUES(''' + sampleKey + ''', ' +
        '''' + sampleKey + ''', ' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
  end
  else begin
    // Nothing to update
  end;
  CreateData(thisrec, 'Sample', sampleKey, 'smp', 'sample');
end;

procedure TDownloadDialog.CreateOccurrence(occKey, sampleKey: String;
  thisrec: TStringList);
var
  existing, tli: _Recordset;
  zeroAbundance, confidential, verified, tlikey, determiner, comment: string;
  vd: TVagueDate;
  nameInfo: string;
begin
  existing := FConnection.Execute('SELECT taxon_occurrence_key FROM taxon_occurrence WHERE taxon_occurrence_key=''' + occKey + '''');
  tli := FConnection.Execute('SELECT recommended_taxon_list_item_key FROM nameserver ' +
      'WHERE input_taxon_version_key=''' + thisrec.values['taxonversionkey'] + ''' AND recommended_taxon_list_item_key IS NOT NULL');
  if tli.RecordCount=0 then begin
    Log(Format(ResStr_CouldNotFindTVK, [thisrec.values['taxonversionkey'], thisrec.values['taxon']]));
    LogFile('Indicia record ' + thisrec.values['occurrence_id'] + ' was not imported. TVK ' + thisrec.values['taxonversionkey']
      + ' could not be found for record ' + VarToStr(thisrec.values['occurrence_id']) + '.');
    Inc(FOccurrencesRejected);
    exit;
  end;
  vd.StartDate := StrToDate(thisrec.values['date_start'], Ffsiso);
  vd.EndDate := StrToDate(thisrec.values['date_end'], Ffsiso);
  vd.DateTypeString := thisrec.values['date_type'];
  tlikey:=tli.fields['recommended_taxon_list_item_key'].value;
  comment := thisrec.values['record_comment'];
  determiner := GetIndividual(thisrec.values['determiner_person_id'], thisrec.values['determiner']);
  if (determiner = 'NBNSYS0000000004') and (thisrec.values['determiner']<>'') then
    // since we couldn't resolve the name key, don't lose the determiner name data
    comment := Format(ResStr_DeterminedByComment, [thisrec.values['determiner']])
        + #13#10 + comment;
  if thisrec.values['zeroabundance']='T' then
    zeroAbundance := '1'
  else
    zeroAbundance := '0';
  if copy(thisrec.values['location_name'], 1, 9)=ResStr_SensitiveLocationNameFromIndicia then
    confidential := '1'
  else
    confidential := '0';
  if thisrec.values['record_status']=ResStr_VerifiedStatusFromIndicia then
    verified := '2'
  else if thisrec.values['record_status']=ResStr_RejectedStatusFromIndicia then
    verified := '1'
  else
    verified := '0';
  if thisrec.values['recorder']='' then
    nameInfo := 'Recorder: <not specified>'
  else
    nameInfo := 'Recorder: ' + thisrec.values['recorder'];
  if thisrec.values['determiner']<>'' then
    nameInfo := nameInfo + ', determiner: ' + thisrec.values['determiner'];
  if existing.RecordCount=0 then begin
    // insert new taxon occurrence
    FConnection.Execute('INSERT INTO taxon_occurrence (taxon_occurrence_key, comment, zero_abundance, confidential, verified, '+
        'checked, checked_by, checked_date, surveyors_ref, provenance, sample_key, substrate_key, record_type_key, ' +
        'entered_by, entry_date) ' +
        'VALUES(''' + occKey + ''', ' +
        '''' + EscapeSqlLiteral(comment) + ''', ' +
        zeroAbundance + ', ' +
        confidential + ', ' +
        verified + ', ' +
        '1, '+
        '''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + ''', ' +
        '''' + thisrec.values['recordkey'] + ''', ' +
        '''None'', ' +
        '''' + sampleKey + ''', ' +
        '''NBNSYS0000000001'', ' +
        '''NBNSYS0000000001'', ' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
    Inc(FOccurrencesCreated);
    LogFile('Taxon occurrence ' + occKey + ' was created from Indicia record ' + thisrec.values['occurrence_id'] + '. '+
      nameInfo);
  end
  else begin
    // update existing taxon occurrence
    FConnection.Execute('UPDATE taxon_occurrence SET '+
        'comment=''' + EscapeSqlLiteral(comment) + ''', ' +
        'zero_abundance=' + zeroAbundance + ', ' +
        'confidential=''' + confidential + ''', ' +
        'verified=''' + verified + ''', ' +
        'surveyors_ref=''' + thisrec.values['recordkey'] + ''', ' +
        'sample_key=''' + sampleKey + ''', ' +
        'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
        'WHERE taxon_occurrence_key=''' + occKey + '''');
    Inc(FOccurrencesUpdated);
    LogFile('Existing taxon occurrence ' + occKey + ' was updated from Indicia record ' +
      thisrec.values['occurrence_id'] + '. ' + nameInfo);
  end;
  // Create a 1:1 relationship with a determination. At the moment we are not doing anything with the log of determinations, just the latest.
  existing := FConnection.Execute('SELECT taxon_determination_key FROM taxon_determination WHERE taxon_determination_key=''' + occKey + '''');
  if existing.RecordCount=0 then begin
    // insert new taxon determination
    FConnection.Execute('INSERT INTO taxon_determination (taxon_determination_key, taxon_list_item_key, taxon_occurrence_key, ' +
        'vague_date_start, vague_date_end, vague_date_type, preferred, determiner, determination_type_key, determiner_role_key, '+
        'entered_by, entry_date) ' +
        'VALUES(''' + occKey + ''', ' +
        '''' + tlikey + ''', ' +
        '''' + occKey + ''', ' +
        IntToStr(trunc(vd.StartDate)) + ', ' +
        IntToStr(trunc(vd.EndDate)) + ', ' +
        '''' + vd.DateTypeString + ''', ' +
        '1, ' +
        '''' + determiner + ''', ' +  // determiner
        '''NBNSYS0000000004'', ' + // determination type original
        '''NBNSYS0000000003'', ' + // original recorder
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
  end
  else begin
    // update existing taxon determination. Don't reset the preferred flag if a diff determination has been
    // added locally. Likewise, no need to update the determination type or determiner role.
    FConnection.Execute('UPDATE taxon_determination SET '+
        'taxon_list_item_key=''' + tlikey + ''', ' +
        'taxon_occurrence_key=''' + occKey + ''', ' +
        'vague_date_start=' + IntToStr(trunc(vd.StartDate)) + ', ' +
        'vague_date_end=' + IntToStr(trunc(vd.EndDate)) + ', ' +
        'vague_date_type=''' + vd.DateTypeString + ''', ' +
        'determiner=''' + determiner + ''', '+
        'determination_type_key=''NBNSYS0000000004'', '+
        'determiner_role_key=''NBNSYS0000000003'', '+
        'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
        'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
        'WHERE taxon_determination_key=''' + occKey + '''');
  end;
  CreateData(thisrec, 'Taxon_Occurrence', occKey, 'occ', 'occurrence');
  CreateMedia(occKey, thisrec);
end;

procedure TDownloadDialog.CreateMedia(occKey: String; thisrec: TStringList);
var
  existing: _Recordset;
  files, tokens: TStringList;
  sourceKey: string;
  i: integer;
  title: string;
begin
  files := TStringList.Create;
  tokens := TStringList.Create;
  try
    files.Text := StringReplace(thisrec.Values['media'], ';', sLineBreak, [rfReplaceAll]);
    for i:=0 to files.Count-1 do begin
      tokens.Text := StringReplace(files[i], '|', sLineBreak, [rfReplaceAll]);
      sourceKey := FRemoteSiteID + IdToKey(tokens[0]);
      existing := FConnection.Execute(
        'SELECT source_key ' +
        'FROM taxon_occurrence_sources ' +
        'WHERE taxon_occurrence_key=''' + occKey + ''' ' +
        'AND source_key=''' + sourceKey + ''''
      );
      // Title in caption [licence code] format.
      if (tokens.count > 2) and (tokens[2] <> '') then
        title := tokens[2]
      else
        title := tokens[1];
      if tokens.count > 3 then begin
        if tokens[3] <> '' then begin
          title := title + ' [' + tokens[3] + ']';
        end;
      end;
      if existing.RecordCount > 0 then begin
        // If existing then update fields.
        FConnection.Execute(
          'UPDATE source_file SET file_name=''' + tokens[1] + ''', title=''' + title + ''' ' +
          'WHERE source_key=''' + sourceKey + ''''
        );
      end
      else begin
        // else add new source_file, source and taxon_occurrence_sources records
        FConnection.Execute(
          'INSERT INTO source(source_key, internal) ' +
             'VALUES(''' + sourceKey + ''', 0)'
        );
        FConnection.Execute(
          'INSERT INTO source_file(source_key, file_name, title) ' +
            'VALUES(''' + sourceKey + ''', ''' + tokens[1] + ''', ''' + title + ''')'
        );
        FConnection.Execute(
          'INSERT INTO taxon_occurrence_sources(source_link_key, taxon_occurrence_key, source_key, original) ' +
            'VALUES(''' + sourceKey + ''', ''' + occKey + ''', ''' + sourceKey + ''', 0)'
        );
      end;
    end;
  finally
    files.Free;
    tokens.Free;
  end;
end;

function TDownloadDialog.EscapeSqlLiteral(literal: string): string;
begin
  result := StringReplace(literal, '''', '''''', [rfReplaceAll]);
end;

function TDownloadDialog.EscapeSqlLiteral(literal: string;
  maxlen: integer): string;
begin
  if length(literal)>maxlen then
    literal := copy(literal, 1, maxlen-3) + '...';
  result := EscapeSqlLiteral(literal);
end;

procedure TDownloadDialog.CreateData(thisrec: TStringList; table, key, prefix, fieldTag: string);
var i: integer;
  dataKey, def, attrId, mu_key, mq_key, val: string;
  temp, mappings: TStringList;
  existing: _Recordset;
begin
  temp := TStringList.Create;
  try
    for i:=0 to FAttrs.Count-1 do begin
      def:=trim(FAttrs.Names[i]);
      if Copy(FAttrs.Names[i], 1, 8)=prefix+'Attr:' then begin
        attrId:=Copy(def, 9, 255);
        if thisrec.values['attr_id_'+fieldTag+'_'+attrId]<>'' then begin
          if thisrec.values['attr_'+fieldTag+'_term_'+attrId]<>'' then
            val:=thisrec.values['attr_'+fieldTag+'_term_'+attrId]
          else
            val:=thisrec.values['attr_'+fieldTag+'_'+attrId];
          val := EscapeSqlLiteral(val, 20);
          // Allow configuration to map the value
          if FAttrTermMappings.IndexOf(def) > -1 then begin
            mappings := TStringList(FAttrTermMappings.Objects[FAttrTermMappings.IndexOf(def)]);
            if mappings.IndexOfName(val) > -1 then
              val := mappings.Values[val];
          end;
          dataKey := FRemoteSiteID + IdToKey(StrToInt(thisrec.values['attr_id_'+fieldTag+'_'+attrId]));
          temp.CommaText := FAttrs.Values[FAttrs.Names[i]];
          mu_key := temp[0];
          mq_key := temp[1];
          existing := FConnection.Execute('SELECT ' + table + '_data_key FROM ' + table + '_data ' +
              'WHERE ' + table + '_data_key=''' + dataKey + '''');
          if existing.RecordCount=0 then begin
            // insert new taxon determination
            FConnection.Execute('INSERT INTO ' + table + '_data (' + table + '_data_key, ' + table + '_key, data, accuracy, '+
              'measurement_qualifier_key, measurement_unit_key, entered_by, entry_date) ' +
              'VALUES (''' + dataKey + ''', ' +
              '''' + key + ''', ' +
              '''' + val + ''', ' +
              '''Unknown'', ' +
              '''' + mq_key + ''', ' +
              '''' + mu_key + ''', ' +
              '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
              '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
              ')');
          end
          else begin
            FConnection.Execute('UPDATE ' + table + '_data SET ' +
              table + '_key=''' + key + ''', ' +
              'data=''' + val + ''', ' +
              'accuracy=''Unknown'', ' +
              'measurement_qualifier_key=''' + mq_key + ''', ' +
              'measurement_unit_key=''' + mu_key + ''', ' +
              'changed_by=''' + FRecorder.CurrentSettings.UserIDKey + ''', ' +
              'changed_date=''' + FormatDateTime('yyyy-mm-dd', Date) + ''' ' +
              'WHERE ' + table + '_data_key=''' + dataKey + '''');
          end;
        end;
      end;
    end;
  finally
    temp.Free;
  end;
end;

// looks for an existing individual matching the supplied Indicia ID. If found, returns the name key.
// Else it creates one.
function TDownloadDialog.GetIndividual(indiciaId: string; name: string): string;
var
  first, last: string;
begin
  if Pos(',', name)>0 then begin
    first := Trim(Copy(name, Pos(',', name)+1, 255));
    last := Trim(Copy(name, 1, Pos(',', name)-1));
  end else begin
    first := Trim(Copy(name, 1, Pos(' ', name)-1));
    last := Trim(Copy(name, Pos(' ', name)+1, 255));
  end;
  // even if Indicia is proposing an ID for this person, the knownPeople.txt file can provide overrides.
  result := FKnownPeople.Values[last + ',' + first];
  if result='' then begin
    if (indiciaId<>'') and (indiciaId<>'unknown') then begin
      result := FRemoteSiteID + IdToKey(indiciaId);
      CreateIndividual(result, name);
    end
    else
      result := 'NBNSYS0000000004';
  end
  else begin
    FPeopleRecordInfo.Values[result] := 'Using configured known person with name ' + name;
  end;
end;

function TDownloadDialog.GetSampleTypeKey(sampleTypeLabel: string): string;
var
  existing, newKey: _Recordset;
  sampleTypeKey: string;
  shortName: string;
  longName: string;
begin
  if sampleTypeLabel = '' then
    sampleTypeKey := 'NBNSYS0000000001' // field observation
  else begin
    // Trim long sample type labels.
    if Length(sampleTypeLabel) > 20 then begin
      shortName := Copy(sampleTypeLabel, 1, 18) + '..';
      longName := ''' + sampleTypeLabel + ''';
    end
    else begin
      shortName := sampleTypeLabel;
      longName := 'null';
    end;
    existing := FConnection.Execute(
      'SELECT sample_type_key FROM sample_type WHERE short_name=''' + shortName + ''''
    );
    if existing.RecordCount=0 then begin
      FConnection.Execute(
        'DECLARE @key CHAR(16); ' +
        'EXEC spNextKey ''sample_type'', @key OUTPUT;'
      );
      newKey := FConnection.Execute('SELECT last_key_text FROM last_key WHERE table_name=''sample_type''');
      sampleTypeKey := FRemoteSiteId + newKey.fields['last_key_text'].value;
      FConnection.Execute('INSERT INTO sample_type ' +
        '(sample_type_key, short_name, long_name, entered_by, entry_date, system_supplied_data) ' +
        'VALUES (''' + sampleTypeKey + ''',' +
        '''' + shortName + ''',' +
        longName + ',' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + ''',' +
        '0)');
    end
    else begin
      sampleTypeKey := existing.fields['sample_type_key'].value;
    end;
  end;
  result := sampleTypeKey;
end;

procedure TDownloadDialog.CreateIndividual(indkey: string; name: string);
var
  first, last: string;
  existing: _Recordset;
begin
  existing := FConnection.Execute('SELECT name_key FROM name WHERE name_key=''' + indkey + '''');
  if existing.RecordCount=0 then
    FConnection.Execute('INSERT INTO name (name_key, organisation, entered_by, entry_date) ' +
        'VALUES(''' + indkey + ''', ' +
        '0, ' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
  existing := FConnection.Execute('SELECT name_key FROM individual WHERE name_key=''' + indkey + '''');
  if existing.RecordCount=0 then begin
    // extract the name, either surname, firstname or firstname surname format.
    if Pos(',', name)>0 then begin
      first := Trim(Copy(name, Pos(',', name)+1, 255));
      last := Trim(Copy(name, 1, Pos(',', name)-1));
    end else begin
      first := Trim(Copy(name, 1, Pos(' ', name)-1));
      last := Trim(Copy(name, Pos(' ', name)+1, 255));
    end;
    FConnection.Execute('INSERT INTO individual (name_key, forename, surname, entered_by, entry_date) ' +
        'VALUES(''' + indkey + ''', ' +
        '''' + EscapeSqlLiteral(first, 20) + ''',' +
        '''' + EscapeSqlLiteral(last, 30) + ''',' +
        '''' + FRecorder.CurrentSettings.UserIDKey + ''',' +
        '''' + FormatDateTime('yyyy-mm-dd', Date) + '''' +
        ')');
    FPeopleRecordInfo.Values[indkey] := 'New person created for name ' + name;
  end
  else begin
    if FPeopleRecordInfo.Values[indkey] = '' then begin
      FPeopleRecordInfo.Values[indkey] := 'Existing person used for name ' + name;
    end;
  end;
end;

function TDownloadDialog.ConvertSrefSystem(input: string): string;
begin
  if input='4326' then
    result := 'LTLN'
  else if input='WGS84' then
    result := 'LTLN'
  else if input='OSIE' then
    result := 'OSNI'
  else if input='2169' then
    result := 'LUX'
  else
    result := uppercase(input);
end;

procedure TDownloadDialog.GetStringsFromJsonRec(thisrec: TStringList; rec: TlkJSONbase);
var i: integer;
begin
  for i:=0 to rec.count-1 do begin
    thisrec.Add(TlkJSONobjectmethod(rec.child[i]).Name + '=' + VarToStr(TlkJSONobjectmethod(rec.child[i]).ObjValue.Value));
  end;
end;

function TDownloadDialog.IdToKey(id: variant): string;
var
  chars: TStringList;
  thisInBase36: integer;
  i: integer;
  key: string;
begin
  chars := TStringList.Create;
  try
    while chars.Count<8 do begin
      thisInBase36 := id mod 36;
      id := id div 36;
      chars.add(intToStr(thisInBase36));
    end;
    key := '';
    for i := 7 downto 0 do begin
      if StrToInt(chars[i])<=9 then
        key := key + chars[i]
      else
        key := key + char(StrToInt(chars[i]) + 55);
    end;
  finally
    chars.Free;
  end;
  result := key;
end;

procedure TDownloadDialog.ConnectToDb;
begin
  if not assigned(FConnection) then begin
    FRecorder := CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
    FConnection := TADOConnection.Create(nil);
    FConnection.ConnectionString := FRecorder.ConnectionString;
    FConnection.LoginPrompt := False;
    FConnection.Open;
    ExecuteSql('SET ARITHABORT ON');
  end;
end;

procedure TDownloadDialog.DisconnectFromDb;
begin
  FConnection.Close;
  FreeAndNil(FConnection);
end;

procedure TDownloadDialog.CreateTempTables;
begin
  ExecuteSql('SELECT TOP 0 * INTO #TempSample FROM Sample');
  ExecuteSql('SELECT TOP 0 * INTO #TempOccurrence FROM Taxon_Occurrence');
  ExecuteSql('SELECT TOP 0 * INTO #TempDetermination FROM Taxon_Determination');
end;

procedure TDownloadDialog.EmptyTempTables;
begin
  ExecuteSql('TRUNCATE TABLE #TempSample');
  ExecuteSql('TRUNCATE TABLE #TempOccurrence');
  ExecuteSql('TRUNCATE TABLE #TempDetermination');
end;

procedure TDownloadDialog.CleanupTempTables;
begin
  ExecuteSql('DROP TABLE #TempSample');
  ExecuteSql('DROP TABLE #TempOccurrence');
  ExecuteSql('DROP TABLE #TempDetermination');
end;

procedure TDownloadDialog.ExecuteSql(sql: string);
var
  rows: integer;
begin
  FConnection.Execute(sql, rows);
end;

procedure TDownloadDialog.Log(const msg: string);
begin
  mmLog.Lines.Add(msg);
  Application.ProcessMessages;
end;

(**
 * Logs a message to add to a log file.
 *
 * Keeps information useful regarding the history of Indicia2Recorder runs.
 *)
procedure TDownloadDialog.LogFile(const msg: string);
begin
  FFileLog.Add(msg);
end;

(**
 * Clears the stringlist used to capture messages for the file log.
 *)
procedure TDownloadDialog.ResetLogFile;
begin
  FFileLog.Clear;
  LogFile('Indicia2Recorder started at ' + FormatDateTime('yyyy/mm/dd hh:mm:ss', Date));
  FEventsCreated := 0;
  FEventsUpdated := 0;
  FSamplesCreated := 0;
  FSamplesUpdated := 0;
  FOccurrencesCreated := 0;
  FOccurrencesUpdated := 0;
  FOccurrencesRejected := 0;
  FPeopleRecordInfo.Clear;
end;

(**
 * Saves the file log.
 *)
procedure TDownloadDialog.SaveLogFile;
var ds: string;
begin
  LogFile('');
  LogFile('Summary');
  LogFile('-------');
  LogFile('Survey events created: ' + IntToStr(FEventsCreated));
  LogFile('Survey events updated: ' + IntToStr(FEventsUpdated));
  LogFile('Samples created: ' + IntToStr(FSamplesCreated));
  LogFile('Samples updated: ' + IntToStr(FSamplesUpdated));
  LogFile('Taxon occurrences created: ' + IntToStr(FOccurrencesCreated));
  LogFile('Taxon occurrences updated: ' + IntToStr(FOccurrencesUpdated));
  LogFile('Taxon occurrences rejected: ' + IntToStr(FOccurrencesRejected));
  // Add the information about people linked to
  LogFile('');
  LogFile('Person records');
  LogFile('--------------');
  FFileLog.AddStrings(FPeopleRecordInfo);
  ds := FormatDateTime('yyyy-mm-dd-hh-mm-ss', Now);
  ForceDirectories(FSettingsFolder + 'Log');
  FFileLog.SaveToFile(FSettingsFolder + 'Log/log-' + ds + '.txt');
end;

{*
 * Encode a date in yyyy-mm-dd format.
 *}
function TDownloadDialog.DateToIsoStr(date: TDateTime): string;
var y, m, d: word;
begin
  DecodeDate(date, y, m, d);
  result := SysUtils.Format('%.*d', [4, y]) + '-' +
      SysUtils.Format('%.*d', [2, m]) + '-' +
      SysUtils.Format('%.*d', [2, d]);
end;

{*
 * Converts the selection in the download type radio to a machine readable type code.
 *}
function TDownloadDialog.GetDownloadType: string;
var caption: string;
begin
  caption := rgDownloadType.Items[rgDownloadType.ItemIndex];
  if caption=ResStr_RecordsICanVerify then
    result := 'expert-records'
  else if caption=ResStr_RecordsICanCollate then
    result := 'collate-records'
  else
    result := 'my-records';
end;

function TDownloadDialog.Get_ActionCaption: WideString;
begin
  result := 'Indicia2Recorder';
end;

function TDownloadDialog.Get_CanAddToToolbar: WordBool;
begin
  result := true;
end;

function TDownloadDialog.Get_Description: WideString;
begin
  result := ResStr_AddinDescription;
end;

function TDownloadDialog.Get_DimmedImageFilename: WideString;
begin
  result := 'default.bmp';
end;

function TDownloadDialog.Get_DisabledImageFileName: WideString;
begin
  result := 'default.bmp';
end;

function TDownloadDialog.Get_Height: Integer;
begin
  result := 383;
end;

function TDownloadDialog.Get_Hint: WideString;
begin
  result := ResStr_AddinHint;
end;

function TDownloadDialog.Get_ImageFileName: WideString;
begin
  result := 'default.bmp';
end;

function TDownloadDialog.Get_Name: WideString;
begin
  result := ResStr_AddinTitle;
end;

function TDownloadDialog.Get_ParentMenu: WideString;
begin
  result := ResStr_ToolsMenuName;
end;

function TDownloadDialog.Get_Width: Integer;
begin
  result := 719;
  if FSettingsFolder='' then begin
    FSettingsFolder := GetFolder(CSIDL_PERSONAL) + 'Indicia2Recorder\';
    if not DirectoryExists(FSettingsFolder) then begin
      FSettingsFolder := GetFolder(CSIDL_COMMON_DOCUMENTS) + 'Indicia2Recorder\';
      CreateDir(FSettingsFolder);
    end;
    if not DirectoryExists(FSettingsFolder) then
      raise Exception.Create(ResStr_SettingsFolderMissing);
    GetConnectionToIndicia;
    LoadSettings(true);
  end;
end;

procedure TDownloadDialog.Install(const iInstalledFilePath: WideString);
begin
  // no action
end;

procedure TDownloadDialog.btnLoginClick(Sender: TObject);
var
  errorMsg: string;
begin
  errorMsg := '';
  if eEmail.Text='' then
    errorMsg := Format(ResStr_SpecifyEmailAddress, [FRemoteSite]) + #13#10;
  if ePassword.Text='' then
    errorMsg := errorMsg + Format(ResStr_SpecifyPassword, [FRemoteSite]) + #13#10;
  if errorMsg<>'' then begin
    ShowMessage(errorMsg);
    exit;
  end;
  DoLogin;
end;

procedure TDownloadDialog.DoLogin;
var
  request: TIdMultiPartFormDataStream;
  response: string;
  tokens: TStringList;
begin
  request := TIdMultiPartFormDataStream.Create;
  try
    request.AddFormField('email', eEmail.Text);
    request.AddFormField('password', ePassword.Text);
    request.AddFormField('appsecret', FAppSecret);
    try
      response := idHttp1.Post(GetUrl('login'), request);
    except
      on E:EIdHTTPProtocolException do begin
        ShowMessage(EIdHTTPProtocolException(E).ErrorMessage);
        exit;
      end;
    end;
  finally
    request.free;
  end;
  tokens := TStringList.Create;
  try
    // If the response has a UTF8 BOM at the start, we need a hack to strip that.
    if copy(response, 1, 3)='﻿' then
      response := copy(response, 4, 255);
    tokens.text := response;
    if (tokens.Count = 1) then
      // single line indicates an error message
      raise Exception.Create(response)
    else if (tokens.Count = 3) then begin
      // 3 items = secret, first name and surname
      Log('Getting privileges from ' + FRemoteSite);
      FSecret := tokens[0];
      FFirstName := tokens[1];
      FSurname := tokens[2];
      FEmail := eEmail.Text;
      lblLoggedInAs.Caption := Format(ResStr_LoggedInAs, [FFirstName, FSurname]);;
      EnableDownloadControls;
      FetchDownloadOptions;
      LoadSettings(false);
    end
    else
      raise Exception.Create(Format(ResStr_InvalidResponse, [response]));
  finally
    tokens.free;
  end;
end;

(*
 * Save stuff like the email address to the registry.
 *)
procedure TDownloadDialog.SaveSettings;
var
  reg: TRegistry;
  quality: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software\Dorset Software\Recorder 6\Indicia2Recorder', true);
    reg.WriteString('Email', eEmail.Text);
    if cmbSurvey.ItemIndex>=0 then
      reg.WriteInteger('Remote Survey', integer(cmbSurvey.Items.Objects[cmbSurvey.ItemIndex]));
    if cmbIntoSurvey.ItemIndex>=0 then
      reg.WriteString('Local Survey', FSurveys[cmbIntoSurvey.ItemIndex]);
    reg.WriteString('Start Date', DateToStr(dtpStartDate.Date));
    reg.WriteString('End Date', DateToStr(dtpEndDate.Date));
    reg.WriteInteger('Download Type', rgDownloadType.ItemIndex);
    quality := '!R';
    if cbLimitToAccepted.Checked then
      quality := 'V';
    reg.WriteString('Quality', quality);
  finally
    reg.Free;
  end;
end;

procedure TDownloadDialog.LoadSettings(login: boolean);
var
  reg: TRegistry;
  i, remoteSurvey: integer;
  localSurvey: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software\Dorset Software\Recorder 6\Indicia2Recorder', true);
    if (login) then begin
      if reg.ValueExists('Email') then
        eEmail.Text := reg.ReadString('Email');
    end
    else begin
      if reg.ValueExists('Remote Survey') then begin
        remoteSurvey := reg.ReadInteger('Remote Survey');
        for i:=0 to cmbSurvey.Items.Count-1 do begin
          if integer(cmbSurvey.Items.Objects[i])=remoteSurvey then
          begin
            cmbSurvey.ItemIndex := i;
            cmbSurveyChange(nil);
          end;
        end;
      end;
      if reg.ValueExists('Local Survey') then begin
        localSurvey := reg.ReadString('Local Survey');
        for i:=0 to cmbIntoSurvey.Items.Count-1 do begin
          if FSurveys[i]=localSurvey then
            cmbIntoSurvey.ItemIndex := i;
        end;
      end;
      if reg.ValueExists('Start Date') then
        dtpStartDate.Date := StrToDate(reg.ReadString('Start Date'));
      if reg.ValueExists('End Date') then
        dtpEndDate.Date := StrToDate(reg.ReadString('End Date'));
      if reg.ValueExists('Download Type') then
        rgDownloadType.ItemIndex := reg.ReadInteger('Download Type');
      if reg.ValueExists('Quality') then
        cbLimitToAccepted.Checked := reg.ReadString('End Date')='V'
      else
        cbLimitToAccepted.Checked := true;
    end;
  finally
    reg.Free;
  end;
end;

{*
 * Enable controls as appropriate after logging in.
 *}
procedure TDownloadDialog.EnableDownloadControls;
begin
  pnlSelectDownload.Enabled := true;
  pnlLogin.Enabled := false;
  // plus individual controls so they grey out
  lblEmail.Enabled := false;
  eEmail.Enabled := false;
  lblPassword.Enabled := false;
  ePassword.Enabled := false;
  btnLogin.Enabled := false;
  rgDownloadType.Enabled := true;
  lblSurvey.Enabled := true;
  cmbSurvey.Enabled := true;
  cbLimitToAccepted.Enabled := true;
  lblStartDate.Enabled := true;
  dtpStartDate.Enabled := true;
  lblEndDate.Enabled := true;
  dtpEndDate.Enabled := true;
  lblEndDate.Enabled := true;
  cmbIntoSurvey.Enabled := true;
  lblIntoSurvey.Enabled := true;
end;

{*
 * Ask the Indicia website server which download options this user is allowed to access.
 *}
procedure TDownloadDialog.FetchDownloadOptions;
var
  request: TIdMultiPartFormDataStream;
  response: string;
  info, types: TlkJSONbase;
  surveys: TlkJSONcustomlist;
  i: integer;
begin
  try
    ConnectToDb;
    PopulateRecorderSurveys;
    LoadSurveyBatches;
  except
    on E:EDownloadDialogConfigException do begin
      ShowMessage(E.Message);
      exit;
    end;
  end;
  request := TIdMultiPartFormDataStream.Create;
  try
    request.AddFormField('email', FEmail);
    request.AddFormField('appsecret', FAppSecret);
    request.AddFormField('usersecret', FSecret);
    response := idHttp1.Post(GetUrl('privileges'), request);
    info := TlkJSON.ParseText(response);
    types := info.Field['types'];
    surveys := TlkJSONcustomlist(info.Field['surveys']);
    surveys.ForEach(eachSurvey, nil);

    for i:=0 to types.count - 1 do begin
      if types.Child[i].value='expert-records' then
        rgDownloadType.Items.Add(ResStr_RecordsICanVerify)
      else if types.Child[i].value='collate-records' then
        rgDownloadType.Items.Add(ResStr_RecordsICanCollate);
    end;
  finally
    request.free;
    DisconnectFromDb;
  end;
end;

procedure TDownloadDialog.PopulateRecorderSurveys;
var rs: _recordset;
begin
  cmbIntoSurvey.Clear;
  FSurveys.Clear;
  rs := FConnection.Execute('SELECT s.survey_key, s.item_name ' +
      'FROM survey s ' +
      'JOIN survey_type st on st.survey_type_key=s.survey_type_key '+
      'WHERE st.short_name=''Indicia''');
  if rs.RecordCount=0 then
    raise EDownloadDialogConfigException.Create(ResStr_SurveyTypeTermlistRequired);
  while not rs.EOF do begin
    cmbIntoSurvey.Items.Add(rs.Fields['item_name'].value);
    FSurveys.Add(rs.Fields['survey_key'].value);
    rs.MoveNext;
  end;
  // Autoselect if only one survey
  if cmbIntoSurvey.Items.Count=1 then
    cmbIntoSurvey.ItemIndex:=0;
end;

(**
 * Any .batch files in the configuration directory are loaded. Contains sets of
 * mappings from remote surveys to local ones which can be used to import
 * multiple datasets at onces.
 *)
procedure TDownloadDialog.LoadSurveyBatches;
var sr: TSearchRec;
begin
  if FindFirst(FSettingsFolder + '*.batch', 0, sr) = 0 then
  begin
    cmbSurvey.Items.Add(sr.name);
    while FindNext(sr) = 0 do
    begin
      cmbSurvey.Items.Add(sr.name);
    end;
  end;
  FindClose(sr);
end;

procedure TDownloadDialog.eachSurvey(ElName: string; Elem: TlkJSONbase;
    data: pointer; var Continue: Boolean);
begin
  cmbSurvey.items.AddObject(Elem.Value, TObject(StrToInt(ElName)));
end;

destructor TDownloadDialog.Destroy;
begin
  FSurveys.Free;
  FDoneSignatures.Free;
  FKnownPeople.Free;
  FFileLog.Free;
  inherited;
end;

(**
 * Change handler for the Indicia survey selection drop down.
 * Disables the recorder survey drop down if a batch chosen.
 *)
procedure TDownloadDialog.cmbSurveyChange(Sender: TObject);
begin
  cmbIntoSurvey.Enabled := (cmbSurvey.ItemIndex = 0) or (cmbSurvey.Items.Objects[cmbSurvey.ItemIndex] <> nil);
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TDownloadDialog,
    Class_DownloadDialog,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
