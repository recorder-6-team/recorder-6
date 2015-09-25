//--------------------------------------------------------------------------------------
// Author:        Stuart Ball, JNCC
// Date:          22/01/2003
// Purpose:       Go to a particular row by specifying the key and table.
//
// Modification history
//--------------------------------------------------------------------------------------
// Initials Date     Comment
// SGB   02/03/2003  Recorder 6 version in Delphi 2006
//--------------------------------------------------------------------------------------
unit GoToKeyImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, GoToKey_TLB, StdVcl, Recorder2000_TLB, StdCtrls, ExtCtrls,
  ADODB;

type
  TGoToKeyX = class(TActiveForm, IGoToKeyX, IRecorderAddin, INewAction,
                    IDialog, IFormCaption)
    Bevel: TBevel;
    labelKey: TLabel;
    labelType: TLabel;
    comboType: TComboBox;
    editKey: TEdit;
    procedure comboTypeClick(Sender: TObject);
  private
    { Private declarations }
    FTableName:  string;
    FDataType:   string;
//    FMenuOption: string;
    FKeyItem2:   string;
    FKeyField:   string;
    FRec2000:    IRecorder2000;
    FConnection: TADOConnection;
    FQuery:      TADOQuery;

    FEvents: IGoToKeyXEvents;
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
    function GetRecorder2000: IRecorder2000;
    procedure SetUpForm;
    function GetQuery: TADOQuery;
    function RowExists(const sTable, sKey: string): boolean;
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
  end;

implementation

uses ComObj, ComServ, VersionInfo, AddinClasses;

{$R *.DFM}

{ TGoToKeyX }

procedure TGoToKeyX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_GoToKeyXPage); }
end;

procedure TGoToKeyX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IGoToKeyXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TGoToKeyX.Initialize;
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

function TGoToKeyX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TGoToKeyX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TGoToKeyX.Get_AlignWithMargins: WordBool;
begin
  Result := AlignWithMargins;
end;

function TGoToKeyX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TGoToKeyX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TGoToKeyX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TGoToKeyX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TGoToKeyX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TGoToKeyX.Get_DockSite: WordBool;
begin
  Result := DockSite;
end;

function TGoToKeyX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TGoToKeyX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TGoToKeyX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TGoToKeyX.Get_ExplicitHeight: Integer;
begin
  Result := ExplicitHeight;
end;

function TGoToKeyX.Get_ExplicitLeft: Integer;
begin
  Result := ExplicitLeft;
end;

function TGoToKeyX.Get_ExplicitTop: Integer;
begin
  Result := ExplicitTop;
end;

function TGoToKeyX.Get_ExplicitWidth: Integer;
begin
  Result := ExplicitWidth;
end;

function TGoToKeyX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TGoToKeyX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TGoToKeyX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TGoToKeyX.Get_MouseInClient: WordBool;
begin
  Result := MouseInClient;
end;

function TGoToKeyX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TGoToKeyX.Get_PopupMode: TxPopupMode;
begin
  Result := Ord(PopupMode);
end;

function TGoToKeyX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TGoToKeyX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TGoToKeyX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TGoToKeyX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TGoToKeyX.Get_UseDockManager: WordBool;
begin
  Result := UseDockManager;
end;

function TGoToKeyX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TGoToKeyX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TGoToKeyX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGoToKeyX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TGoToKeyX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TGoToKeyX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TGoToKeyX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TGoToKeyX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TGoToKeyX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TGoToKeyX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TGoToKeyX.MouseEnterEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseEnter;
end;

procedure TGoToKeyX.MouseLeaveEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnMouseLeave;
end;

procedure TGoToKeyX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TGoToKeyX.Set_AlignWithMargins(Value: WordBool);
begin
  AlignWithMargins := Value;
end;

procedure TGoToKeyX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TGoToKeyX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TGoToKeyX.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TGoToKeyX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TGoToKeyX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TGoToKeyX.Set_DockSite(Value: WordBool);
begin
  DockSite := Value;
end;

procedure TGoToKeyX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TGoToKeyX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TGoToKeyX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TGoToKeyX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGoToKeyX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := string(Value);
end;

procedure TGoToKeyX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TGoToKeyX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TGoToKeyX.Set_PopupMode(Value: TxPopupMode);
begin
  PopupMode := TPopupMode(Value);
end;

procedure TGoToKeyX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TGoToKeyX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TGoToKeyX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TGoToKeyX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TGoToKeyX.Set_UseDockManager(Value: WordBool);
begin
  UseDockManager := Value;
end;

procedure TGoToKeyX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

//===========================================================================
// Interface implimentation
//===========================================================================
function TGoToKeyX.Get_ActionCaption: WideString;
begin
     Result := '&Go To';
end;

function TGoToKeyX.Get_CanAddToToolbar: WordBool;
begin
     Result := True;
end;

function TGoToKeyX.Get_Description: WideString;
begin
     Result := 'Go to a particular row, specified by picking a table and typing a key.'
               + AddinVersion('GoToKey.ocx');
end;

function TGoToKeyX.Get_DimmedImageFilename: WideString;
begin
     Result := 'GoToKeyDim.bmp';
end;

function TGoToKeyX.Get_DisabledImageFileName: WideString;
begin
     Result := 'GoToKeyDisabled.bmp';
end;

function TGoToKeyX.Get_FormCaption: WideString;
begin
     Result := 'Go to a key';
     SetUpForm;
end;

function TGoToKeyX.Get_Height: Integer;
begin
     Result := 145;
end;

function TGoToKeyX.Get_Hint: WideString;
begin
     Result := 'Go to a particular row by typing its key.';
end;

function TGoToKeyX.Get_ImageFileName: WideString;
begin
     Result := 'GoToKey.bmp';
end;

function TGoToKeyX.Get_Name: WideString;
begin
   Result := 'GoTo Key';
end;

function TGoToKeyX.Get_ParentMenu: WideString;
begin
    Result := 'Tools';
end;

function TGoToKeyX.Get_Width: Integer;
begin
    Result := 197;
end;

procedure TGoToKeyX.Install(const iInstalledFilePath: WideString);
begin
     // don't need to do anything
end;

procedure TGoToKeyX.DoCancel;
begin
     // don't do anything, just let it close
end;

//------------------------------------------------------------------------
// This is the key routine that actually does it!
//------------------------------------------------------------------------
function TGoToKeyX.DoOk: WordBool;
var lKeyList: TCOMKeyList;
    lTable: string;
begin
  // check that we look as if we have an NBN key
  If Length(editKey.Text) = 16 then
  begin
    // put it in an IKeyList
    lKeyList := TCOMKeyList.Create(nil);
    try
      lKeyList.KeyList.SetTable(FTableName);
      lKeyList.KeyList.AddItem(editKey.Text, FkeyItem2);

      If FkeyItem2 = '' then
        lTable := FTableName
      else
        lTable := FkeyItem2;

      // check that the row exists in the database
      If RowExists(lTable, editKey.Text) then
      begin
        try
          // it no longer seems to be necessary to open the window
          // first before calling DisplayData interface
          //Recorder2000.MenuOptionClick(FMenuOption);
          Recorder2000.DisplayData(FDataType, lKeyList as IKeyList);
          Result := True;
        except
          on e:Exception do
            ShowMessageFmt('Error showing "%s".' + #13 +
                           'Error: %s', [editKey.text, e.Message]);
        end;
      end
      else
        // row does not exists in the designated table
        ShowMessageFmt('Row "%s" not found in table "%s".',[editKey.text, lTable]);
    finally
//      lKeyList.Free;    <-  This causes an Invalid pointer error! 27/6/7
    end;
  end else
    // we don't have a 16-character key in the edit field
    ShowMessageFmt('"%s" is not a 16-character NBN key!',[editKey.text]);
end;

//===========================================================================
// Properties and initialisation
//===========================================================================
//------------------------------------------------------------------------
// Clean up - close database connection
//------------------------------------------------------------------------
destructor TGoToKeyX.Destroy;
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

//------------------------------------------------------------------------
// Get a connection to the database
//------------------------------------------------------------------------
function TGoToKeyX.GetQuery: TADOQuery;
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

//------------------------------------------------------------------------
// Get the Recorder 2000 interface
//------------------------------------------------------------------------
function TGoToKeyX.GetRecorder2000: IRecorder2000;
begin
  {Get IRecorder2000 object}
  If Frec2000 = nil then
    FRec2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  Result := FRec2000;
end;

//------------------------------------------------------------------------
// Initialise the form - set up to search for a SURVEY
//------------------------------------------------------------------------
procedure TGoToKeyX.SetUpForm;
begin
  If FTableName = '' then
  begin
    {Initialise values for global variables - for Survey}
    FTableName  := 'SURVEY';
    FKeyField   := 'SURVEY_KEY';
    FDataType   := 'Survey';
//    FMenuOption := 'Data Entry;Observations...';
    FKeyItem2   := '';
    comboType.ItemIndex := 0; // set combo for Survey
    editKey.Text := ''; // blank out key
  end;
end;

//===========================================================================
// Our local routines
//===========================================================================
//------------------------------------------------------------------------
// Responds to the combobox to select the table to search
// We need to set up the various parameters depending on the selected item
//------------------------------------------------------------------------
procedure TGoToKeyX.comboTypeClick(Sender: TObject);
begin
  With comboType do
    Case ItemIndex of
    0:   begin
             FTableName  := 'SURVEY';
             FKeyField   := 'SURVEY_KEY';
             FDataType   := 'Survey';
             FKeyItem2   := '';
//             FMenuOption := 'Data Entry;Observations...';
        end;
    1:   begin
             FTableName  := 'SURVEY_EVENT';
             FKeyField   := 'SURVEY_EVENT_KEY';
             FDataType   := 'Event';
             FKeyItem2   := '';
//             FMenuOption := 'Data Entry;Observations...';
        end;
    2:   begin
             FTableName  := 'SAMPLE';
             FKeyField   := 'SAMPLE_KEY';
             FDataType   := 'Sample';
             FKeyItem2   := '';
//             FMenuOption := 'Data Entry;Observations...';
        end;
    3:   begin
             FTableName  := MIXED_DATA;
             FKeyField   := 'TAXON_OCCURRENCE_KEY';
             FDataType   := 'Occurrence';
             FKeyItem2   := 'Taxon_occurrence';
//             FMenuOption := 'Data Entry;Observations...';
        end;
    4:   begin
             FTableName  := MIXED_DATA;
             FKeyField   := 'BIOTOPE_OCCURRENCE_KEY';
             FDataType   := 'Occurrence';
             FKeyItem2   := 'Biotope_occurrence';
//             FMenuOption := 'Data Entry;Observations...';
        end;
    5:   begin
             FTableName  := 'LOCATION';
             FKeyField   := 'LOCATION_KEY';
             FDataType   := 'Location';
             FKeyItem2   := '';
//             FMenuOption := 'Data Entry;Locations...';
        end;
    6:   begin
             FTableName  := MIXED_DATA;
             FKeyField   := 'NAME_KEY';
             FDataType   := 'Name';
             FKeyItem2   := 'Individual';
//             FMenuOption := 'Data Entry;Names and Addresses...';
        end;
    7:   begin
             FTableName  := MIXED_DATA;
             FKeyField   := 'NAME_KEY';
             FDataType   := 'Name';
             FKeyItem2   := 'Organisation';
//             FMenuOption := 'Data Entry;Names and Addresses...';
        end;
    8:   begin
             FTableName  := 'REFERENCE';
             FKeyField   := 'SOURCE_KEY';
             FDataType   := 'Document';
             FKeyItem2   := '';
//             FMenuOption := 'Data Entry;Documents...';
        end;
    9:   begin
             FTableName  := 'TAXON_LIST_ITEM';
             FKeyField   := 'TAXON_LIST_ITEM_KEY';
             FDataType   := 'Taxon';
             FKeyItem2   := '';
//             FMenuOption := 'Dictionaries;Taxon...';
        end;
    10:   begin
             FTableName  := 'BIOTOPE_LIST_ITEM';
             FKeyField   := 'BIOTOPE_LIST_ITEM_KEY';
             FDataType   := 'Biotope';
             FKeyItem2   := '';
//             FMenuOption := 'Dictionaries;Biotope...';
        end;
    11:   begin
             FTableName  := 'ADMIN_AREA';
             FKeyField   := 'ADMIN_AREA_KEY';
             FDataType   := 'Admin';
             FKeyItem2   := '';
//             FMenuOption := 'Dictionaries;Admin Area...';
        end;
    end; {case}
end;

//------------------------------------------------------------------------
// Check that the row sKey exists in table sTable
// Note that the name of the Key field is put in FKeyField at
// initialisation or when the ComboBox is clicked
//------------------------------------------------------------------------
Function TGoToKeyX.RowExists(const sTable, sKey: string): boolean;
var sSQL: string;
begin
  sSQL := Format('SELECT %s FROM %s WHERE (%s=''%s'')',
                 [FKeyField, sTable, FKeyField, sKey]);
  with ADOQuery do
  begin
    SQL.Clear;
    SQL.Add(sSQL);
    Open;
    Result := (RecordCount > 0);
    Close;
  end;
end;

//===========================================================================
initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TGoToKeyX,
    Class_GoToKeyX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
