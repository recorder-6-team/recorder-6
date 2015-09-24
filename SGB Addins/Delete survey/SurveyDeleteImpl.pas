//------------------------------------------------------------------------------
// Author:   Stuart Ball, JNCC
// Date:     Jan 2007
// Version:  6
// Rec2K:    6.9.3
// Comments: Addin which will delete all the survey events, samples and
//           observations related to a survey and, optionally, the survey
//           record itself.
//
//           Works by turning on cascading deletes, deleting the SURVEY
//           row and then turning cascading deletes back off.
//
//           If the option to keep the SURVEY row is selected, then the row is
//           copied to a temp key before being deleted, then copied back to the
//           original key and the temp copy deleted.
//
//           Only System Manager level users are allowed to delete a survey.
//
//------------------------------------------------------------------------------
// Modifications
// Date    Initials Changes
// 19/12/07 SGB  The SQL to copy a SURVEY is now built dynamically
//               The relationships are built from the DATABASE_RELATIONSHIP
//               table rather than being hard-coded
// 09/04/08 SGB  If the SURVEY_KEY is in the EXPORT_FILTER_SURVEY table, then
//               referential integrity prevents the survey from being deleted.
//               Either delete such entries or rename them to TEMP_KEY
// 11/08/09 SGB  Need to handle SURVEY_TAG is a similar way to EXPORT_FILTER_SURVEY
//               So generalise the mechanism incase another, similar feature
//               is added in future
//------------------------------------------------------------------------------
unit SurveyDeleteImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, SurveyDelete_TLB, StdVcl, StdCtrls, ExtCtrls,
  Recorder2000_TLB, ADODB;

type
  TCascadeDelete = (cdOff, cdOn);
  TManageAssocSurveyKey = (skDelete, skToTemp,  skToOriginal);

  // Object used to store SURVEY_KEY in the combobox
  TIDStore = class(TObject)
  private
    FID: string;
  property
    ID: string read FID write FID;
  end;

  // hold details about a relationship in the database
  TRelation = record
    KeyName: string;
    ChildTable: string;
    FKField: string;
    ParentTable: string;
    PKField: string;
  end;
  TRelations = array of TRelation;

  TSurveyDeleteX = class(TActiveForm, ISurveyDeleteX, IRecorderAddin, INewAction,
                         IDialog, IFormCaption)
    Bevel1: TBevel;
    labelSurvey: TLabel;
    cbSurveys: TComboBox;
    memStatistics: TMemo;
    cbSurveyRow: TCheckBox;
    procedure cbSurveysChange(Sender: TObject);
  private
    { Private declarations }
    FEvents: ISurveyDeleteXEvents;
    // global variables
    FRecorder2000: IRecorder2000;
    FConnection:   TADOConnection;
    FQuery:        TADOQuery;
    FRelations:    TRelations;    // Array of relationships
    FTables:       TStrings;      // used in finding relationships
    FVersion:      string;

    // auto generated
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
    // our routines
    function GetQuery: TADOQuery;
    function GetRecorder2000: IRecorder2000;
    function UserSecurityLevel: integer;
    procedure GetSurveys;
    procedure SetUp;
    procedure CascadeDelete(const switch: TCascadeDelete);
    function DoDelete(const SurveyID: string): boolean;
    function SaveSurveyRow(const SurveyID: string): boolean;
    procedure GetSurveyStats;
    function CopySurvey(const FromID, ToID: string): boolean;
    function ExecuteSQL(const sSQL: string): boolean;
    function GetConnection: TADOConnection;
    function RelationshipName(const sParent, sRef: string): string;
    function AddRelations(const sTableName: string): integer;
    function GetSqlVersion: string;
    function GetVersion: string;
    function ManageAssocTable(const TableName, SurveyID: string; const action: TManageAssocSurveyKey): boolean;
  protected
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
    { IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    { INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    { IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    { IFormCaption}
    function Get_FormCaption: WideString; safecall;
  public
    { Public declarations }
    procedure Initialize; override;
    destructor Destroy; override;
    property Recorder2000: IRecorder2000 read GetRecorder2000;
    property ADOQuery: TADOQuery read GetQuery;
    property ADOConnection: TADOConnection read GetConnection;
    property SQLVersion: string read GetVersion;
  end;

implementation

uses ComObj, ComServ, VersionInfo;

const TEMP_KEY = 'DeleteSurveyTemp';

{$R *.DFM}

{ TSurveyDeleteX }

procedure TSurveyDeleteX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_SurveyDeleteXPage); }
end;

procedure TSurveyDeleteX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ISurveyDeleteXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TSurveyDeleteX.Initialize;
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

function TSurveyDeleteX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TSurveyDeleteX.Get_AlignWithMargins: WordBool;
begin
  Result := AlignWithMargins;
end;

function TSurveyDeleteX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TSurveyDeleteX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TSurveyDeleteX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TSurveyDeleteX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TSurveyDeleteX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TSurveyDeleteX.Get_DockSite: WordBool;
begin
  Result := DockSite;
end;

function TSurveyDeleteX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TSurveyDeleteX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TSurveyDeleteX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TSurveyDeleteX.Get_ExplicitHeight: Integer;
begin
  Result := ExplicitHeight;
end;

function TSurveyDeleteX.Get_ExplicitLeft: Integer;
begin
  Result := ExplicitLeft;
end;

function TSurveyDeleteX.Get_ExplicitTop: Integer;
begin
  Result := ExplicitTop;
end;

function TSurveyDeleteX.Get_ExplicitWidth: Integer;
begin
  Result := ExplicitWidth;
end;

function TSurveyDeleteX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TSurveyDeleteX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TSurveyDeleteX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TSurveyDeleteX.Get_MouseInClient: WordBool;
begin
  Result := MouseInClient;
end;

function TSurveyDeleteX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TSurveyDeleteX.Get_PopupMode: TxPopupMode;
begin
  Result := Ord(PopupMode);
end;

function TSurveyDeleteX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TSurveyDeleteX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TSurveyDeleteX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TSurveyDeleteX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TSurveyDeleteX.Get_UseDockManager: WordBool;
begin
  Result := UseDockManager;
end;

function TSurveyDeleteX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TSurveyDeleteX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TSurveyDeleteX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TSurveyDeleteX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TSurveyDeleteX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TSurveyDeleteX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TSurveyDeleteX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TSurveyDeleteX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TSurveyDeleteX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TSurveyDeleteX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TSurveyDeleteX.MouseEnterEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseEnter;
end;

procedure TSurveyDeleteX.MouseLeaveEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseLeave;
end;

procedure TSurveyDeleteX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TSurveyDeleteX.Set_AlignWithMargins(Value: WordBool);
begin
  AlignWithMargins := Value;
end;

procedure TSurveyDeleteX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TSurveyDeleteX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TSurveyDeleteX.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TSurveyDeleteX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TSurveyDeleteX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TSurveyDeleteX.Set_DockSite(Value: WordBool);
begin
  DockSite := Value;
end;

procedure TSurveyDeleteX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TSurveyDeleteX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TSurveyDeleteX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TSurveyDeleteX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TSurveyDeleteX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := string(Value);
end;

procedure TSurveyDeleteX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TSurveyDeleteX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TSurveyDeleteX.Set_PopupMode(Value: TxPopupMode);
begin
  PopupMode := TPopupMode(Value);
end;

procedure TSurveyDeleteX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TSurveyDeleteX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TSurveyDeleteX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TSurveyDeleteX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TSurveyDeleteX.Set_UseDockManager(Value: WordBool);
begin
  UseDockManager := Value;
end;

procedure TSurveyDeleteX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

//==========================================================================
// Interface implimentation
//==========================================================================
function TSurveyDeleteX.Get_ActionCaption: WideString;
begin
     Result := 'Delete Survey ...';
end;

procedure TSurveyDeleteX.Install(const iInstalledFilePath: WideString);
begin
     // don't need to do anything
end;

function TSurveyDeleteX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TSurveyDeleteX.Get_CanAddToToolbar: WordBool;
begin
     Result := True;
end;

function TSurveyDeleteX.Get_Description: WideString;
begin
     Result := 'Allows a Survey to be deleted, complete with any ' +
               'survey events, samples and observations it contains. ' +
               AddinVersion('SurveyDelete.ocx');
end;

function TSurveyDeleteX.Get_DimmedImageFilename: WideString;
begin
     Result := 'DeleteSurveyDimmed.bmp';
end;

function TSurveyDeleteX.Get_DisabledImageFileName: WideString;
begin
     Result := 'DeleteSurveyDisabled.bmp';
end;

function TSurveyDeleteX.Get_FormCaption: WideString;
begin
     Result := 'Delete Survey';
     SetUp;
end;

function TSurveyDeleteX.Get_Height: Integer;
begin
     Result := 271;
end;

function TSurveyDeleteX.Get_Width: Integer;
begin
     Result := 279;
end;

procedure TSurveyDeleteX.DoCancel;
begin
     // don't need to do anything
end;

function TSurveyDeleteX.Get_Hint: WideString;
begin
     Result := 'Allows a Survey to be deleted, complete with its ' +
               'survey events, samples and observations.';
end;

function TSurveyDeleteX.Get_ImageFileName: WideString;
begin
     Result := 'DeleteSurvey.bmp';
end;

function TSurveyDeleteX.Get_Name: WideString;
begin
     Result := 'Delete Survey';
end;

function TSurveyDeleteX.Get_ParentMenu: WideString;
begin
     Result := 'Tools';
end;

//-----------------------------------------------------------------------
// This is the core routine that does the deletion if the user presses
// the [OK] button in the dialog box!
//-----------------------------------------------------------------------
function TSurveyDeleteX.DoOk: WordBool;
var lSurveyID: string;
    iCursor: Longint;
    bDone: boolean;
    sMsg: string;
begin
     // Check user is a System Manager
     If UserSecurityLevel < 5 then
     begin
          MessageDlg('Sorry - you need to be a System Manager to use this facility',
                     mtInformation, [mbOK], 0);
          DoOK := True;
     end
     else
     With cbSurveys do
     begin
          if ItemIndex >= 0 then
          begin
              lSurveyID := TIDStore(Items.Objects[ItemIndex]).ID;
              if lSurveyID <> '' then
              begin
                  // Get confirmation from user
                  If MessageDlg('Are you sure you want to delete survey "' +
                                Items[ItemIndex] + '".', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                  begin
                    iCursor := Screen.Cursor;
                    Screen.Cursor := crHourGlass;
                    try
                      // Check whether to keep the Survey record
                      If not cbSurveyRow.Checked then
                        SaveSurveyRow(lSurveyID);

                      // get relationships from the database
                      // start at the SURVEY table and work down the relationships
                      FTables.Clear;
                      FTables.Add('SURVEY');
                      AddRelations('SURVEY');
                      if not cbSurveyRow.Checked then
                      begin
                        ManageAssocTable('EXPORT_FILTER_SURVEY', lSurveyID, skToTemp);
                        ManageAssocTable('SURVEY_TAG', lSurveyID, skToTemp);
                      end
                      else
                      begin
                        ManageAssocTable('EXPORT_FILTER_SURVEY', lSurveyID, skDelete);
                        ManageAssocTable('SURVEY_TAG', lSurveyID, skDelete);
                      end;

                      // Switch cascading deletes on
                      CascadeDelete(cdOn);

                      // Delete the survey
                      bDone := DoDelete(lSurveyID);

                      // Restore the survey if user wanted to keep it
                      If not cbSurveyRow.Checked then
                      begin
                        // copy the saved survey record back to the original key
                        Result := CopySurvey(TEMP_KEY, lSurveyID);
                        // update associated tables
                        ManageAssocTable('EXPORT_FILTER_SURVEY', lSurveyID, skToOriginal);
                        ManageAssocTable('SURVEY_TAG', lSurveyID, skToOriginal);
                        // delete the temp copy of the survey key
                        DoDelete(TEMP_KEY);
                      end;

                      // Switch cascading deletes back off
                      CascadeDelete(cdOff);

                      // confirmation message
                      if bDone then
                      begin
                        sMsg := Format('Survey "%s" has been ',[Items[ItemIndex]]);
                        if cbSurveyRow.Checked then
                          sMsg := sMsg + 'deleted.'
                        else
                          sMsg := sMsg + 'emptied.';
                        MessageDlg(sMsg, mtInformation, [mbOK], 0);
                      end
                      else
                        MessageDlg('Failed to delete survey!', mtWarning, [mbOK], 0);

                      DoOK := True;
                    finally
                      Screen.Cursor := iCursor;
                    end;
                  end
                  // user didn't confirm the delete - stop dialog closing
                  else DoOK := False;
              end
              else // no Survey has been selected
              begin
                  MessageDlg('Please select a Survey!', mtWarning, [mbOK], 0);
                  DoOK := False;
              end;
          end
          else // no Survey has been selected
          begin
              MessageDlg('Please select a Survey!', mtWarning, [mbOK], 0);
              DoOK := False;
          end;
     end;
end;

//=======================================================================
//  Property implimentation
//=======================================================================
//-------------------------------------------------------------------------
// Clean up - free memeory for objects we have allocated
//-------------------------------------------------------------------------
destructor TSurveyDeleteX.Destroy;
begin
  FRecorder2000 := nil;
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
  If FTables <> nil then
    FTables.Free;
  inherited;
end;

//-------------------------------------------------------------------------
// Connect to the database
//-------------------------------------------------------------------------
function TSurveyDeleteX.GetConnection: TADOConnection;
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

//-------------------------------------------------------------------------
// ... and get us a query
//-------------------------------------------------------------------------
function TSurveyDeleteX.GetQuery: TADOQuery;
begin
  If FQuery = nil then
  begin
    FQuery := TADOQuery.Create(nil);
    FQuery.Connection := ADOConnection;
  end;
  Result := FQuery;
end;

//-------------------------------------------------------------------------
// Get access to Recorder interfaces
//-------------------------------------------------------------------------
function TSurveyDeleteX.GetRecorder2000: IRecorder2000;
begin
  {Get IRecorder2000 object}
  If FRecorder2000 = nil then
    FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  Result := FRecorder2000;
end;

//-------------------------------------------------------------------------
// Save the SQL Server version
//-------------------------------------------------------------------------
function TSurveyDeleteX.GetVersion: string;
begin
  if FVersion = '' then
    FVersion := GetSqlVersion;
  Result := FVersion;
end;

//-------------------------------------------------------------------------
// React to user selecting a survey in the drop-down box
//-------------------------------------------------------------------------
procedure TSurveyDeleteX.cbSurveysChange(Sender: TObject);
begin
  GetSurveyStats;
end;

//=======================================================================
//  Local routines
//=======================================================================
//-------------------------------------------------------------------------
// Setup initial appearance of the form
//-------------------------------------------------------------------------
procedure TSurveyDeleteX.SetUp;
begin
  cbSurveys.Items.Clear;
  cbSurveys.Text := '';
  memStatistics.Clear;
  // create the list used to check whether we have found the
  // relationships for a given table yet
  FTables := TStringList.Create;

  // populate the drop-down with Survey names
  // but don't tick one by default - user must do that
  GetSurveys;
end;

//-------------------------------------------------------------------------
// Get security level setting (integer 1-5) for the current user from the
// USER table
//-------------------------------------------------------------------------
function TSurveyDeleteX.UserSecurityLevel: integer;
begin
  Result := -1;
  with ADOQuery do
  begin
    SQL.Clear;
    SQL.Add(Format('SELECT SECURITY_LEVEL FROM [USER] WHERE (NAME_KEY = ''%s'')',
                   [Recorder2000.CurrentSettings.UserIDKey]));
    Open;
    if RecordCount = 1 then
      Result := FieldByName('SECURITY_LEVEL').AsInteger;
    Close;
  end;
end;

//-------------------------------------------------------------------------
// Populate the cbSurveys drop-down list with an alpahbetic list of
// survey names from the database. We need to remember their IDs.
//-------------------------------------------------------------------------
procedure TSurveyDeleteX.GetSurveys;
var  sName: string;
     oID: TIDStore;
     iCursor: Longint;
begin
  iCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    with ADOQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT SURVEY_KEY, ITEM_NAME FROM SURVEY ORDER BY ITEM_NAME');
      Open;
      If RecordCount > 0 then
        with cbSurveys do
          while not EoF do
          begin
            sName := FieldByName('ITEM_NAME').AsString;
            oID := TIDStore.Create;
            oID.ID := FieldByName('SURVEY_KEY').AsString;
            Items.AddObject(sName, oID);
            Next;
          end;
      Close;
    end;
  finally
    Screen.Cursor := iCursor;
  end;
end;

//-------------------------------------------------------------------------
// Get counts of survey events, samples, taxon and biotope occurrences
// for the selected survey and populate the memStatistics
// memo field with them.
//-------------------------------------------------------------------------
procedure TSurveyDeleteX.GetSurveyStats;
var sSQL, sKey: string;
    iCursor: longint;

      //-----------------------------------------------------
      // This runs the SQL and returns the (formatted) count
      //-----------------------------------------------------
      function GetCount(const sSQL, sFld: string): string;
      begin
        with ADOQuery do
        begin
          SQL.Clear;
          SQL.Add(sSQL);
          try
            Open;
            If RecordCount > 0 then
              Result := Trim(Format('%0.0n', [FieldByName(sFld).AsFloat]))
            else
              Result := '0';
          finally
            Close;
          end;
        end;
      end; {GetCount}

begin
  iCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    memStatistics.Clear;
    memStatistics.Update;
    with cbSurveys do
    begin
      memStatistics.Lines.Add('Survey "' + Items[ItemIndex] + '":');
      sKey := TIDStore(Items.Objects[ItemIndex]).ID;
    end;

    // count survey events
     sSQL := 'SELECT Count(SURVEY_EVENT.SURVEY_EVENT_KEY) AS nRecs ' +
             'FROM SURVEY INNER JOIN SURVEY_EVENT ON SURVEY.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY ' +
             'GROUP BY SURVEY.SURVEY_KEY ' +
             'HAVING (((SURVEY.SURVEY_KEY)=''' + sKey + '''))';
     memStatistics.Lines.Add('Survey events = ' + GetCount(sSQL,'nRecs'));

     // count samples
     sSQL := 'SELECT Count(SAMPLE.SAMPLE_KEY) AS nRecs ' +
             'FROM (SURVEY INNER JOIN SURVEY_EVENT ON SURVEY.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY) ' +
             'INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY ' +
             'GROUP BY SURVEY.SURVEY_KEY ' +
             'HAVING (((SURVEY.SURVEY_KEY)=''' + sKey + '''));';
     memStatistics.Lines.Add('Samples = ' + GetCount(sSQL,'nRecs'));

     // count taxon occurrences
     sSQL := 'SELECT Count(TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY) AS nRecs ' +
             'FROM (SURVEY INNER JOIN (SURVEY_EVENT INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = ' +
             'SAMPLE.SURVEY_EVENT_KEY) ON SURVEY.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY) INNER JOIN ' +
             'TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY ' +
             'GROUP BY SURVEY.SURVEY_KEY ' +
             'HAVING (((SURVEY.SURVEY_KEY)=''' + sKey + '''));';
     memStatistics.Lines.Add('Taxon occurrences = ' + GetCount(sSQL,'nRecs'));

     // count biotope occurrences
     sSQL := 'SELECT Count(BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY) AS nRecs ' +
             'FROM (SURVEY INNER JOIN (SURVEY_EVENT INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = ' +
             'SAMPLE.SURVEY_EVENT_KEY) ON SURVEY.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY) INNER JOIN ' +
             'BIOTOPE_OCCURRENCE ON SAMPLE.SAMPLE_KEY = BIOTOPE_OCCURRENCE.SAMPLE_KEY ' +
             'GROUP BY SURVEY.SURVEY_KEY ' +
             'HAVING (((SURVEY.SURVEY_KEY)=''' + sKey + '''));';
     memStatistics.Lines.Add('Biotope occurrences = ' + GetCount(sSQL,'nRecs'));
     memStatistics.Update;
  finally
    Screen.Cursor := iCursor;
  end;
end;

//-------------------------------------------------------------------------
// Save the SURVEY record to a temporary id
//-------------------------------------------------------------------------
function TSurveyDeleteX.SaveSurveyRow(const SurveyID: string): boolean;
begin
  Result := CopySurvey(SurveyID, TEMP_KEY);
end;

//-------------------------------------------------------------------------
// Copy a SURVEY record
//-------------------------------------------------------------------------
function TSurveyDeleteX.CopySurvey(const FromID, ToID: string): boolean;
var sSQL,sList: string;
    p: integer;
begin
  // get the column names in the SURVEY table
  // and build a list of them in sList (comma delinated)
  sSQL := 'SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS ' +
          'WHERE table_name = ''SURVEY''';
  with ADOQuery do
  begin
    SQL.Clear;
    SQL.Add(sSQL);
    try
      Open;
      If RecordCount > 0 then
      begin
        sList := '';
        while not ADOQuery.EOF do
        begin
          sList := sList + ADOQuery.Fields[0].AsString + ',';
          ADOQuery.Next;
        end;
        // trim the trailing comma
        sList := Copy(sList, 1, Length(sList)-1);
      end;
    finally
      Close;
    end;
  end;

  // build the SQL string
  sSQL := 'INSERT INTO SURVEY (' + sList + ') ';

  // replace the first item, SURVEY_KEY, in sList with the value in ToID
  p := Pos(',', sList);
  if p>0 then
    sList := Copy(sList, p+1, Length(sList));

  // now we can finish off the SQL string
  sSQL := sSQL + 'SELECT ''' + ToID + ''',' + sList +
          ' FROM SURVEY AS SURVEY_1' +
          ' WHERE (SURVEY_KEY = ''' + FromID + ''')';

  // Execute the SQL
  Result := ExecuteSQL(sSQL);
end;

//-------------------------------------------------------------------------
// Delete the selected survey row
//-------------------------------------------------------------------------
function TSurveyDeleteX.DoDelete(const SurveyID: string): boolean;
var sSQL: string;
begin
  sSQL := 'DELETE FROM SURVEY WHERE (SURVEY_KEY = ''' + SurveyID + ''')';
  Result := ExecuteSQL(sSQL);
end;

//-------------------------------------------------------------------------
// Manage SURVEY_KEY in associated tables
// These tables are EXPORT_FILTER_SURVEY and SURVEY_TAG at present
// (11/08/2009)
//-------------------------------------------------------------------------
function TSurveyDeleteX.ManageAssocTable(const TableName, SurveyID: string;
                             const action: TManageAssocSurveyKey): boolean;
var sSQL: string;
begin
  // generate the SQL depending on the action and table name
  case action of
  skDelete:      sSQL := Format('DELETE FROM %s WHERE (SURVEY_KEY = ''%s'')',
                                [TableName, SurveyID]);
  skToTemp:      sSQL := Format('UPDATE %s SET SURVEY_KEY = ''%s'' WHERE (SURVEY_KEY = ''%s'')',
                                [TableName, TEMP_KEY, SurveyID]);
  skToOriginal:  sSQL := Format('UPDATE %s SET SURVEY_KEY = ''%s'' WHERE (SURVEY_KEY = ''%s'')',
                                [TableName, SurveyID, TEMP_KEY]);
  end {CASE};
  Result := ExecuteSQL(sSQL);
end;

//-------------------------------------------------------------------------
// ADD or DROP cascade delete conditions to foriegn key constraints
//-------------------------------------------------------------------------
procedure TSurveyDeleteX.CascadeDelete(const switch: TCascadeDelete);
var i: integer;

    function DropConstraint(const i: integer): boolean;
    var sSQL, sCheckSQL: string;
    begin
      if SQLVersion[1] = '9' then
        // SQL Server 2005
        sCheckSQL := 'SELECT * FROM sys.foreign_keys ' +
                     'WHERE object_id = OBJECT_ID(N''[dbo].[%s]'') ' +
                     'AND parent_object_id = OBJECT_ID(N''[dbo].[%s]'')'
      else
        // SQL Server 2000
        sCheckSQL := 'SELECT * FROM sysreferences ' +
                     'WHERE constid = OBJECT_ID(N''[dbo].[%s]'') ' +
                     'AND fkeyid = OBJECT_ID(N''[dbo].[%s]'')';

      sCheckSQL := Format(sCheckSQL, [FRelations[i].KeyName, FRelations[i].ChildTable]);

      sSQL := Format('IF EXISTS (%s) ALTER TABLE [dbo].[%s] DROP CONSTRAINT [%s]',
              [sCheckSQL, FRelations[i].ChildTable, FRelations[i].KeyName]);
      Result := ExecuteSQL(sSQL);
    end; { DropConstraint }

    function AddConstraint(const i: integer): boolean;
    var sSQL: string;
    begin
      sSQL := Format('ALTER TABLE [dbo].[%s] WITH NOCHECK ADD CONSTRAINT ' +
                '[%s] FOREIGN KEY ([%s]) REFERENCES [dbo].[%s] ([%s])',
                [FRelations[i].ChildTable, FRelations[i].KeyName, FRelations[i].FKField,
                FRelations[i].ParentTable, FRelations[i].PKField]);
      if switch = cdOn then
        sSQL := sSQL + ' ON DELETE CASCADE';
      Result := ExecuteSQL(sSQL);
    end; { AddConstraint }

begin
  for i := Low(FRelations) to High(FRelations) do
  begin
    DropConstraint(i);
    AddConstraint(i);
  end;
end;

//-------------------------------------------------------------------------
// This finds the relationships relating to a given table from the
// DATABSE_RELATIONSHIP table.
// It is recursive, so it is kicked off by passing it the SURVEY table.
// It "follows down" from that to get all the necessary relationships
//-------------------------------------------------------------------------
function TSurveyDeleteX.AddRelations(const sTableName: string): integer;
var sSQL: string;
    oQuery: TADOQuery;
    i:  integer;
begin
  Result := 0;
  // get relationships from the DATABASE_RELATIONSHIP table
  // following down from the this table
  sSQL := 'SELECT MASTER_TABLE, MASTER_FIELD, DETAIL_TABLE, DETAIL_FIELD ' +
          'FROM DATABASE_RELATIONSHIP ' +
          'WHERE (MASTER_TABLE = ''' + sTableName + ''') AND (FOLLOW_DOWN = 1)';

  oQuery := TADOQuery.Create(nil);
  oQuery.Connection := ADOConnection;
  try
    with oQuery do
    begin
      SQL.Clear;
      SQL.Add(sSQL);
      try
        Open;
        If RecordCount > 0 then
        begin
          while not EOF do
          begin
            // add another item to the FRelations array
            // not very efficient!
            // but we are only dealing with c. 20 of them
            i := High(FRelations);
            inc(i);
            SetLength(FRelations, i+1);
            // store info in FRelations array
            FRelations[i].ParentTable := FieldByName('MASTER_TABLE').AsString;
            FRelations[i].ChildTable := FieldByName('DETAIL_TABLE').AsString;
            FRelations[i].FKField := FieldByName('DETAIL_FIELD').AsString;
            FRelations[i].PKField := FieldByName('MASTER_FIELD').AsString;
            FRelations[i].KeyName := RelationshipName(FRelations[i].ChildTable, FRelations[i].ParentTable );

            // have we found the relationships for the child table yet?
            // if not we need to add them recursively
            if FTables.IndexOf(FRelations[i].ChildTable) < 0 then
            begin
              FTables.Add(FRelations[i].ChildTable);
              AddRelations(FRelations[i].ChildTable);
            end;

            Next;
          end;
        end;
      finally
        Close;
      end;
    end;
  finally
    oQuery.Free;
  end;
end;

//-------------------------------------------------------------------------
// Get the name of a given relationship from the database given
// its parent and reference object name.
// This is needed for the ALTER SQL to switch cascade on or off
//-------------------------------------------------------------------------
function TSurveyDeleteX.RelationshipName(const sParent,sRef: string): string;
var sSQL: string;
    oQuery: TADOQuery;
begin
  Result := '';
  // We want FOREIGN_KEY_CONSTRAINTS (Type = F) involving our
  // parent and child table
  if SQLVersion[1] = '9' then
    // SQL Server 2005 / Express version
    sSQL := 'SELECT name FROM sys.foreign_keys ' +
            'WHERE parent_object_id = OBJECT_ID(N''[dbo].[' + sParent + ']'') ' +
            'AND referenced_object_id = OBJECT_ID(N''[dbo].[' + sRef + ']'') ' +
            'AND type = ''F'''
  else
    // SQL Server 2000 / MSDE version
    sSQL := 'SELECT name ' +
            'FROM sysreferences INNER JOIN sysobjects ON constid = id ' +
            'WHERE fkeyid = OBJECT_ID(N''[dbo].[' + sParent + ']'') ' +
            'AND rkeyid = OBJECT_ID(N''[dbo].[' + sRef + ']'')';

  oQuery := TADOQuery.Create(nil);
  oQuery.Connection := ADOConnection;
  try
    with oQuery do
    begin
      SQL.Clear;
      SQL.Add(sSQL);
      try
        Open;
        If RecordCount > 0 then
          Result := FieldByName('name').AsString;
      finally
        Close;
      end;
    end;
  finally
    oQuery.Free;
  end;
end;

//-------------------------------------------------------------------------
// Execute an action SQL statement
//-------------------------------------------------------------------------
Function TSurveyDeleteX.ExecuteSQL(const sSQL: string): boolean;
begin
  with ADOQuery do
  begin
    Close;
    SQL.Clear;
    SQL.Add(sSQL);
    try
      ExecSQL;
      Result := (RowsAffected > 0);
    except
      on Exception do Result := False;
    end;
    Close;
  end;
end;

//-------------------------------------------------------------------------
// Get the SQL Server version number from the database
// The result of  "SELECT @@Version" is like: (all in one string)
//   "Microsoft SQL Server  2000 - 8.00.760 (Intel X86)
//    Dec 17 2002 14:22:05
//    Copyright (c) 1988-2003 Microsoft Corporation
//    Standard Edition on Windows NT 5.2 (Build 3790: Service Pack 2)"
//-------------------------------------------------------------------------
function TSurveyDeleteX.GetSqlVersion: string;
var sValue: string;
    oQuery: TADOQuery;
begin
  // query SQL Server version
  oQuery := TADOQuery.Create(nil);
  with oQuery do
  try
    sValue := '';
    Connection := ADOConnection;
    SQL.Text := 'SELECT @@Version';
    Open;
    If RecordCount > 0 then
      sValue := Fields[0].AsString;
    Close;
  finally
    Free;
  end;

  // process the result
  if sValue <> '' then
  begin
    // It starts like
    //  "Microsoft SQL Server 2005 - 9.00.1399.06 (Intel X86) ..."
    // so extract the number from between "-" and " ("
    sValue := trim(copy(sValue, pos('-', sValue) + 1, 99));
    Result := trim(copy(sValue, 1, pos(' (', sValue)));
  end
  else
    Result := '?';
end;

//=======================================================================
initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TSurveyDeleteX,
    Class_SurveyDeleteX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
