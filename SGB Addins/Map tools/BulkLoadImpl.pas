unit BulkLoadImpl;
{==============================================================================
 Author:    Stuart Ball, JNCC
 Date:      Mar 2004
 Version:   3
 Type:      ActiveX - Recorder 6 add-in
 Purpose:   To load a set of GSF files as background layers to a map.
            This is the version for Recorder 6.3 which takes account of
            multiple base maps. 
 Requires:

 Third party components
 ----------------------
 psvDialogs - uses the Browse Folder Dialog. This is freeware with no restrictions.
     Permission is granted to anyone to use this software for any
     purpose, including commercial applications, and to alter it
     and redistribute it freely without any  restrictions.

 Modification history
 ====================
 Author   Date   Description
 ------------------------------------------------------------------------------
 SGB   03/01/2007  Recorder 6.9.3 version. Switched to BrowseForFolderU
 ==============================================================================}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, BulkLoad_TLB, StdVcl, ExtCtrls, StdCtrls, Buttons,
  Recorder2000_TLB, VersionInfo, ADODB, Registry, BrowseForFolderU;

type
  TSaveID = class(TObject)
  private
    FID: string;
  public
    property ID: string read FID write FID;
  end;

  TformBulkLoadX = class(TActiveForm, IBulkLoadX, IRecorderAddin, IFormCaption,
                         IDialog, INewAction)
    cbMap: TComboBox;
    edDirectory: TEdit;
    bBrowse: TSpeedButton;
    listFiles: TListBox;
    cbCutIn: TComboBox;
    cbCutOut: TComboBox;
    lbMap: TLabel;
    lbDirectory: TLabel;
    labFiles: TLabel;
    lbCutIn: TLabel;
    lbCutOut: TLabel;
    Bevel: TBevel;
    procedure bBrowseClick(Sender: TObject);
  private
    { Private declarations }
    FEvents: IBulkLoadXEvents;

    {Local variables}
    FRecorder2000: IRecorder2000;
    FConnection : TADOConnection;
    FQuery : TAdoQuery;
    FMapPath: string;
    FAdded: integer;

    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function Get_Recorder2000: IRecorder2000;
    function GetComputerID: string;
    function Get_MapPath: string;
    function IsMapInstalled(const lFileName: string): boolean;
    procedure LoadMapFiles;
    function NextMapOrder: string;
    function Get_Query: TADOQuery;
    function InsertMapSetting(const lFileName: string): integer;
    procedure LoadBaseMaps;
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
    //----------------------------------------------
    // Recorder 2000 interface methods
    //----------------------------------------------
    { IRecorderAddin }
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    { INewAction }
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    { IDialog }
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    { IForm Caption }
    function Get_FormCaption: WideString; safecall;
  public
    { Public declarations }
    procedure Initialize; override;
    { our properties }
    destructor Destroy; override;
    property Recorder2000: IRecorder2000 read Get_Recorder2000;
    property MapPath: string read Get_MapPath;
    property Query: TADOQuery read Get_Query;
  end;

implementation

uses ComObj, ComServ;

const QUOTE = Char(39);

{$R *.DFM}

{ TBulkLoadX }

procedure TformBulkLoadX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_BulkLoadXPage); }
end;

procedure TformBulkLoadX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IBulkLoadXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TformBulkLoadX.Initialize;
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

function TformBulkLoadX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TformBulkLoadX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TformBulkLoadX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TformBulkLoadX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TformBulkLoadX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TformBulkLoadX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TformBulkLoadX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TformBulkLoadX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TformBulkLoadX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TformBulkLoadX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TformBulkLoadX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TformBulkLoadX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TformBulkLoadX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TformBulkLoadX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TformBulkLoadX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TformBulkLoadX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TformBulkLoadX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TformBulkLoadX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TformBulkLoadX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TformBulkLoadX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TformBulkLoadX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TformBulkLoadX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TformBulkLoadX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TformBulkLoadX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TformBulkLoadX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TformBulkLoadX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TformBulkLoadX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TformBulkLoadX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TformBulkLoadX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TformBulkLoadX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TformBulkLoadX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TformBulkLoadX.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TformBulkLoadX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TformBulkLoadX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TformBulkLoadX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TformBulkLoadX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TformBulkLoadX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TformBulkLoadX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TformBulkLoadX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TformBulkLoadX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TformBulkLoadX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TformBulkLoadX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TformBulkLoadX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TformBulkLoadX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TformBulkLoadX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TformBulkLoadX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

//===========================================================================
// Recorder interface implementation
//===========================================================================

procedure TformBulkLoadX.DoCancel;
begin

end;

//---------------------------------------------------------------------------
// Actually do it when the user presses the OK button ib the dialog.
//---------------------------------------------------------------------------
function TformBulkLoadX.DoOk: WordBool;
var iCursor: TCursor;
    sInfo: string;
begin
     if listFiles.SelCount > 0 then
     begin
          iCursor := Screen.Cursor;
          try
             Screen.Cursor := crHourglass;
             LoadMapFiles;
          finally
             Screen.Cursor := iCursor;
          end;
          // tell the user what happened
          if FAdded > 0 then
          begin
             If FAdded > 1 then
                sInfo := 's were'
             else
                sInfo := ' was';
             MessageDlg(Format('%d map%s registered.', [FAdded, sInfo]), mtInformation, [mbOK], 0);
          end;
          Result := True; // allow it to close
     end
     else
     begin
         MessageDlg('Please select some files to load!', mtWarning, [mbOK], 0);
         Result := False;  // prevent it from closing
     end;
end;

function TformBulkLoadX.Get_ActionCaption: WideString;
begin
     Result := 'Bulk load maps';
end;

function TformBulkLoadX.Get_CanAddToToolbar: WordBool;
begin
     Result := False;
end;

function TformBulkLoadX.Get_Description: WideString;
begin
     Result := 'Bulk load map tiles from the GSF files found ' +
               'in a path specified by the user.' +
               AddinVersion('BulkLoad.ocx');
end;

function TformBulkLoadX.Get_DimmedImageFilename: WideString;
begin
     Result := '';
end;

function TformBulkLoadX.Get_DisabledImageFileName: WideString;
begin
     Result := '';
end;

function TformBulkLoadX.Get_FormCaption: WideString;
begin
     // populate the drop-down list of base maps
     LoadBaseMaps;
     
     Result := 'Bulk load maps';
end;

function TformBulkLoadX.Get_Height: Integer;
begin
     Result := 355;
end;

function TformBulkLoadX.Get_Hint: WideString;
begin
     Result := 'Bulk load maps';
end;

function TformBulkLoadX.Get_ImageFileName: WideString;
begin
     Result := '';
end;

function TformBulkLoadX.Get_Name: WideString;
begin
     Result := 'Bulk load maps';
end;

function TformBulkLoadX.Get_ParentMenu: WideString;
begin
     Result := 'Map';
end;

function TformBulkLoadX.Get_Width: Integer;
begin
     Result := 230;
end;

procedure TformBulkLoadX.Install(const iInstalledFilePath: WideString);
begin

end;

//===========================================================================
// Property implementations
//===========================================================================
destructor TformBulkLoadX.Destroy;
begin
  FRecorder2000 := nil;
  // Close the database connection
  if FConnection <> nil then
     FConnection.Close;
  FConnection := nil;
  inherited;
end;

function TformBulkLoadX.Get_Recorder2000: IRecorder2000;
begin
     If FRecorder2000 = nil then
        FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     Result := FRecorder2000;
end;

function TformBulkLoadX.Get_MapPath: string;
var lReg: TRegistry;
begin
  If FMapPath = '' then
  begin
     lReg := TRegistry.Create;
     try
        lReg.OpenKey('Software\Dorset Software\Recorder 6\Settings',False);
        FMapPath := lReg.ReadString('Map File Path');
     finally
        lReg.Free;
     end;
  end;
  Result := FMapPath;
end;

function TformBulkLoadX.Get_Query: TADOQuery;
begin
     if FQuery = nil then
     begin
         // open the connection to the database
         FConnection := TADOConnection.Create(nil);
         FConnection.LoginPrompt := False;
         FConnection.ConnectionString := Recorder2000.ConnectionString;
         FConnection.Open;
         Recorder2000.SetApplicationSecurity(FConnection.ConnectionObject);
         //set up a query object
         FQuery := TADOQuery.Create(nil);
         FQuery.Connection := FConnection;
     end;
     Result := FQuery;
end;

//===========================================================================
// Implementation
//===========================================================================

//-----------------------------------------------------------------------------
// Gets a directory and loads the names of .GSF from it
//-----------------------------------------------------------------------------
procedure TformBulkLoadX.bBrowseClick(Sender: TObject);
var oSearch: TSearchRec;
    sPath: string;
begin
  sPath := edDirectory.Text;
  if sPath = '' then sPath := 'c:';

  sPath := BrowseForFolder('Path to map tiles', sPath);
  if DirectoryExists(sPath) then
  begin
       listFiles.Items.Clear;
       edDirectory.Text := sPath;
       if FindFirst(sPath + '\*.gsf', faAnyFile, oSearch) = 0 then
       begin
           repeat
             listFiles.Items.Add(ExtractFileName(oSearch.Name))
           until FindNext(oSearch) <> 0;
           FindClose(oSearch);
       end;
  end;
end;

//-----------------------------------------------------------------------------
// Actually loads the files
// Note that we have already checked that there are files selected in the
// list before we get here.
//-----------------------------------------------------------------------------
procedure TformBulkLoadX.LoadMapFiles;
var i, n: integer;
    sFileName: string;
begin
   n := 0;
   FAdded := 0;
   Recorder2000.RecorderMainForm.StartProgressBar;
   try
       For i:= 0 to listFiles.Count-1 do
           if listFiles.Selected[i] then
           begin
               sFileName := edDirectory.Text + '\' + listFiles.Items[i];
               If not IsMapInstalled(sFileName) then
               begin
                    // check whether the file already exists
                    If not FileExists(MapPath + ExtractFileName(sFileName)) then
                    begin
                         // it doesn't - so copy it to the \MapFiles directory
                         Recorder2000.RecorderMainForm.StatusText := 'Loading ' + sFileName + ' ...';
                         Application.ProcessMessages;
                         CopyFile(PChar(sFileName), PChar(MapPath + ExtractFileName(sFileName)), False);
                    end;
                    // "register" it in the MAP_SHEET table
                    If InsertMapSetting(ExtractFileName(sFileName)) > 0 then
                       inc(FAdded);
                    inc(n);
                    Recorder2000.RecorderMainForm.Progress := trunc(n/listFiles.SelCount*100.0);
                    Application.ProcessMessages;
               end;
           end;
   finally
       // make sure we clean up the screen
       Recorder2000.RecorderMainForm.StopProgressBar;
       Recorder2000.RecorderMainForm.StatusText := '';
       Application.ProcessMessages;
   end;
end;

//-----------------------------------------------------------------------------
// Insert the entry into MAP_SHEET for the current user
//-----------------------------------------------------------------------------
Function TformBulkLoadX.InsertMapSetting(const lFileName: string): integer;
var lKey, lName, lExt, lComputerID, lSQL: string;
    y,m,d: word;
begin
     lName := lFileName;
     DecodeDate(now(),y,m,d);
     lExt := ExtractFileExt(lName);
     lComputerID := GetComputerID;
     If Length(lExt) > 0 then
        lName := Copy(lName, 1, Length(lName) - Length(lExt));
     lKey := Recorder2000.GetNextKey('MAP_SHEET');
     lSQL := 'INSERT INTO MAP_SHEET ';
     lSQL := lSQL + '(MAP_SHEET_KEY, SHEET_NAME, FILE_NAME, SHEET_TYPE, CUT_IN_SCALE, ';
     lSQL := lSQL + 'CUT_OUT_SCALE, SHEET_DISPLAYED, ENTERED_BY, ENTRY_DATE, NEW_DATA, ';
     lSQL := lSQL + 'MODIFIED_DATA, DATASET_SHEET_ORDER, DATASET_SHEET_FILENAME, COMPUTER_ID, BASE_MAP_KEY) ';
     lSQL := lSQL + 'VALUES ';
     lSQL := lSQL + '(' + Quote + lKey + Quote + ', ';
     //lSQL := lSQL + Quote + '<NULL>' + Quote + ', ';
     lSQL := lSQL + Quote + lName + Quote + ',';
     lSQL := lSQL + Quote + MapPath + lFileName + Quote + ',';
     lSQL := lSQL + Quote + '2' + Quote + ', ';
     lSQL := lSQL + Quote + cbCutIn.Items[cbCutIn.ItemIndex] + Quote + ', ';
     lSQL := lSQL + Quote + cbCutOut.Items[cbCutOut.ItemIndex] + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + Recorder2000.CurrentSettings.UserIDKey + Quote + ', ';
     lSQL := lSQL + Quote + Format('%d-%d-%d', [y,m,d]) + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + '1' + Quote + ', ';
     lSQL := lSQL + Quote + NextMapOrder + Quote + ', ';
     lSQL := lSQL + Quote + lFileName + Quote + ',';
     lSQL := lSQL + Quote + lComputerID + Quote +  ', ';
     lSQL := lSQL + Quote + TSaveID(cbMap.Items.Objects[cbMap.ItemIndex]).ID + Quote +')';

     // now we can do the query
     Query.SQL.Text := lSQL;
     // return the number of rows affected
     Result := Query.ExecSQL;
end;

//-----------------------------------------------------------------------------
// If this map already installed by the current user for the selected base map
//-----------------------------------------------------------------------------
function TformBulkLoadX.IsMapInstalled(const lFileName: string): boolean;
begin
     Query.SQL.Text := 'SELECT MAP_SHEET_KEY FROM MAP_SHEET ' +
                       'WHERE (((USER_ID)=''' + Recorder2000.CurrentSettings.UserIDKey + ''') ' +
                       'AND ((FILE_NAME)='''  + MapPath + ExtractFileName(lFileName) + ''') ' +
                       'AND ((BASE_MAP_KEY)='''  + TSaveID(cbMap.Items.Objects[cbMap.ItemIndex]).ID + '''));';

     try
        Query.Open;
        Result := (Query.RecordCount > 0);
     finally
        Query.Close;
     end;
end;

//-----------------------------------------------------------------------------
//  Get the order number for the next sheet
//-----------------------------------------------------------------------------
function TformBulkLoadX.NextMapOrder: string;
var iMax: integer;
begin
     Query.SQL.Text :=  'SELECT Max(MAP_SHEET.DATASET_SHEET_ORDER) AS MaxOrd ' +
                        'FROM MAP_SHEET ' +
                        'WHERE (((MAP_SHEET.DATASET_SHEET_ORDER)<2000000000));';
     try
        Query.Open;
        If Query.RecordCount > 0 then
           iMax := Query.FieldByName('MaxOrd').AsInteger
        else
           iMax := 0;
     finally
        Query.Close;
     end;
     Result := IntToStr(iMax+1);
end;

//-----------------------------------------------------------------------------
//  Get the unique name of this computer
//-----------------------------------------------------------------------------
function TformBulkLoadX.GetComputerID: string;
var
  ilen : DWORD;
begin
  ilen := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength (Result, ilen);
  Windows.GetComputerName (PChar (result), ilen);
  SetLength (Result, ilen);
end;

//-----------------------------------------------------------------------------
// Get the Base Map names from the BASE_MAP table
//-----------------------------------------------------------------------------
procedure TformBulkLoadX.LoadBaseMaps;
var  oID: TSaveID;
begin
     cbMap.Items.Clear;
     cbMap.Text := '';
     edDirectory.Text := '';

     Query.SQL.Text := 'SELECT Base_Map_Key, Display_Name FROM BASE_MAP ' +
                       'ORDER BY Display_Name;';
     try
        Query.Open;
        If Query.RecordCount > 0 then
        begin
           while not Query.EoF do
           begin
                oID := TSaveID.Create;
                oID.ID := Query.FieldByName('Base_Map_Key').AsString;
                cbMap.Items.AddObject(Query.FieldByName('Display_Name').AsString, oID);
                Query.Next;
           end;
        end;
     finally
        Query.Close;
     end;

     if cbMap.Items.Count > 0 then
        cbMap.ItemIndex := 0;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TformBulkLoadX,
    Class_BulkLoadX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
