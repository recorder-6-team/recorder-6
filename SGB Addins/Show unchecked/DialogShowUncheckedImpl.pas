unit DialogShowUncheckedImpl;
//--------------------------------------------------------------------------------------
// Author:        Stuart Ball, JNCC
// Date:          22/01/2003
// Purpose:       Find any TAXON_OCCURRENCEs which are unchecked, not verified
//                or with zero abundance and then open the Observation window
//                filtered to these selected occurrences.
//
// Modification history
//--------------------------------------------------------------------------------------
// Initials Date     Comment
// SGB   02/03/2003  Recorder 6 version in Delphi 7 (updated version number to 6)
//--------------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ShowUnchecked_TLB, ExtCtrls, StdCtrls, Recorder2000_TLB,
  AddinClasses, StdVcl, ADODB, VersionInfo;

type
  TDialogShowUnchecked = class(TActiveForm, IDialogShowUnchecked,
                         IRecorderAddin, INewAction, IDialog, IFormCaption)
    cbChecked: TCheckBox;
    cbZeroAbundance: TCheckBox;
    cbInvalid: TCheckBox;
    Bevel: TBevel;
  private
    { Private declarations }
    FRecorder2000: IRecorder2000;
    FConnection:   TADOConnection;
    FQuery:        TADOQuery;

    FEvents: IDialogShowUncheckedEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function GetQuery: TADOQuery;
    function GetRecorder2000: IRecorder2000;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_BiDiMode: TxBiDiMode; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_BiDiMode(Value: TxBiDiMode); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
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
    {IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    {IFormCaption}
    function Get_FormCaption: WideString; safecall;
  public
    { Public declarations }

    procedure Initialize; override;
    destructor Destroy; override;
    property Recorder2000: IRecorder2000 read GetRecorder2000;
    property ADOQuery: TADOQuery read GetQuery;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TDialogShowUnchecked }

procedure TDialogShowUnchecked.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_DialogShowUncheckedPage); }
end;

procedure TDialogShowUnchecked.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IDialogShowUncheckedEvents;
end;

procedure TDialogShowUnchecked.Initialize;
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

function TDialogShowUnchecked.Get_Active: WordBool;
begin
  Result := Active;
end;

function TDialogShowUnchecked.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TDialogShowUnchecked.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TDialogShowUnchecked.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TDialogShowUnchecked.Get_BiDiMode: TxBiDiMode;
begin
  Result := Ord(BiDiMode);
end;

function TDialogShowUnchecked.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TDialogShowUnchecked.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TDialogShowUnchecked.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TDialogShowUnchecked.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TDialogShowUnchecked.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TDialogShowUnchecked.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TDialogShowUnchecked.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TDialogShowUnchecked.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TDialogShowUnchecked.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TDialogShowUnchecked.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TDialogShowUnchecked.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TDialogShowUnchecked.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TDialogShowUnchecked.Get_Visible: WordBool;
begin
  Result := Visible;
end;

procedure TDialogShowUnchecked._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TDialogShowUnchecked.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TDialogShowUnchecked.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TDialogShowUnchecked.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TDialogShowUnchecked.Set_BiDiMode(Value: TxBiDiMode);
begin
  BiDiMode := TBiDiMode(Value);
end;

procedure TDialogShowUnchecked.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TDialogShowUnchecked.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TDialogShowUnchecked.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TDialogShowUnchecked.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TDialogShowUnchecked.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TDialogShowUnchecked.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TDialogShowUnchecked.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TDialogShowUnchecked.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TDialogShowUnchecked.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TDialogShowUnchecked.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TDialogShowUnchecked.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TDialogShowUnchecked.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TDialogShowUnchecked.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TDialogShowUnchecked.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TDialogShowUnchecked.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TDialogShowUnchecked.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TDialogShowUnchecked.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TDialogShowUnchecked.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TDialogShowUnchecked.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TDialogShowUnchecked.KeyPressEvent(Sender: TObject;
  var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TDialogShowUnchecked.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

//--------------------------------------------------------------------------------------
// Recorder interface implementation
//--------------------------------------------------------------------------------------
function TDialogShowUnchecked.Get_ActionCaption: WideString;
begin
     Result := 'Display &Unchecked records';
end;

function TDialogShowUnchecked.Get_CanAddToToolbar: WordBool;
begin
     Result := True;
end;

function TDialogShowUnchecked.Get_Description: WideString;
begin
     Result := 'Open the Observation window displaying only unchecked, ' +
               'unverified and/or zero-abundance observation.' +
               AddinVersion('ShowUnchecked.ocx');
end;

function TDialogShowUnchecked.Get_DimmedImageFilename: WideString;
begin
     Result := 'UnCheckedCold.bmp';
end;

function TDialogShowUnchecked.Get_DisabledImageFileName: WideString;
begin
     Result := 'UnCheckedDisabled.bmp';
end;

function TDialogShowUnchecked.Get_Hint: WideString;
begin
     Result := 'Open the Observation window displaying only unchecked observation.';
end;

function TDialogShowUnchecked.Get_ImageFileName: WideString;
begin
     Result := 'UnChecked.bmp';
end;

function TDialogShowUnchecked.Get_Name: WideString;
begin
     Result := 'Display unchecked records';
end;

function TDialogShowUnchecked.Get_ParentMenu: WideString;
begin
     Result := 'Data Entry';
end;

procedure TDialogShowUnchecked.Install(const iInstalledFilePath: WideString);
begin
     // Don't need to do anything
end;

procedure TDialogShowUnchecked.DoCancel;
begin
     // Don't need to do anything
end;

function TDialogShowUnchecked.Get_FormCaption: WideString;
begin
     Result := 'Type of occurrence to display';
end;

//--------------------------------------------------------------------------------------
// Set dialog size
//--------------------------------------------------------------------------------------
function TDialogShowUnchecked.Get_Height: Integer;
begin
     Result := 152;
end;

function TDialogShowUnchecked.Get_Width: Integer;
begin
     Result := 200;
end;

//--------------------------------------------------------------------------------------
// This is the key routine that actually does the job when the [OK] button is clicked
//--------------------------------------------------------------------------------------
function TDialogShowUnchecked.DoOk: WordBool;
var lKey, lSQL: string;
    oKeyList: TComKeyList;
begin
     // check we have at least one option selected
     If (cbChecked.Checked) OR
        (cbZeroAbundance.Checked) OR
        (cbInvalid.Checked) then
     try
          Screen.Cursor := crHourGlass;

          {Build the SQL}
          lSQL := 'SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY  ' +
                  'FROM TAXON_OCCURRENCE ' +
                  'WHERE (';
          lKey := ''; // temporarily holds the OR if needed
          If cbChecked.Checked then
          begin
               lSQL := lSQL + '(TAXON_OCCURRENCE.CHECKED=0) ';
               lKey := 'OR ';
          end;
          If cbZeroAbundance.Checked then
          begin
               lSQL := lSQL + lKey + '(TAXON_OCCURRENCE.ZERO_ABUNDANCE=1) ';
               lKey := 'OR ';
          end;
          If cbInvalid.Checked then
               lSQL := lSQL + lKey + '(TAXON_OCCURRENCE.VERIFIED=1)';
          lSQL := lSQL + ');';

          {Run the query}
          ADOQuery.SQL.Add(lSQL);
          try
             ADOQuery.Open;
             If ADOQuery.RecordCount > 0 then
             begin
                  // we found something - so build a keylist
                  oKeyList := TComKeyList.Create(nil);
                  try
                     oKeyList.KeyList.Clear;
                     oKeyList.KeyList.SetTable(MIXED_DATA);
                     While not ADOQuery.EoF do
                     begin
                          lKey := ADOQuery.FieldByName('TAXON_OCCURRENCE_KEY').AsString;
                          oKeyList.KeyList.AddItem(lKey, 'TAXON_OCCURRENCE');
                          ADOQuery.Next;
                     end;
                     // open the Observation window
                     //FRecorder2000.MenuOptionClick('Data Entry;Observations...');
                     // ... and send it the KeyList
                     FRecorder2000.DisplayData('Occurrence', oKeyList as IKeyList);
                  finally
                     //oKeyList.Free;   <- causing Invalid Pointer 27/6/07
                  end;
             end
             else
             begin
                  //Nothing found - construct feedback message
                  lKey := '';
                  lSQL := '';
                  If cbChecked.Checked then
                  begin
                       lSQL := 'unchecked';
                       lKey := ', ';
                  end;
                  If cbZeroAbundance.Checked then
                  begin
                       lSQL := lSQL + lKey + 'zero-abundance';
                       lKey := ' or ';
                  end;
                  If cbInvalid.Checked then
                  begin
                       If lKey <> '' then lKey := ' or ';
                       lSQL := lSQL + lKey + 'invalid';
                  end;
                  // display message to user
                  MessageDlg('No ' + lSQL + #13 + 'observations were found!', mtInformation,
                                 [mbOK], 0);
             end;
          finally
             ADOQuery.Close;
          end;
          Result := True; // allow the dialog to close
     finally
      Screen.Cursor := crDefault;
     end
     else
     begin
         // non of the checkboxes is ticked!
         MessageDlg('Please select at least' + #13 + 'one of the options!',
                     mtInformation, [mbOK], 0);
         Result := False;  // stop the dialog closing
     end
end;

//--------------------------------------------------------------------------------------
// Properties
//--------------------------------------------------------------------------------------
function TDialogShowUnchecked.GetRecorder2000: IRecorder2000;
begin
     {Get Recorder2000 interface}
     If FRecorder2000 = nil then
        FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     Result := FRecorder2000;
end;

function TDialogShowUnchecked.GetQuery: TADOQuery;
begin
     If FQuery = nil then
     begin
          FQuery := TADOQuery.Create(nil);
          FConnection := TADOConnection.Create(nil);
          FConnection.LoginPrompt := False;
          FConnection.ConnectionString := Recorder2000.ConnectionString;
          FConnection.Open;
          Recorder2000.SetApplicationSecurity(FConnection.ConnectionObject);
          FQuery.Connection := FConnection;
     end;
     Result := FQuery;
end;

destructor TDialogShowUnchecked.Destroy;
begin
     If FQuery <> nil then
     begin
        FQuery.Close;
        FQuery.Free;
     end;
     If FConnection <> nil then
     begin
        FConnection.Close;
        FConnection.Free;
     end;
  inherited;
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TDialogShowUnchecked,
    Class_DialogShowUnchecked,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
