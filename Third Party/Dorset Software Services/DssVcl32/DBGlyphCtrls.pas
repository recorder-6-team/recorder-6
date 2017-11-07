{===============================================================================

  Copyright © Dorset Software Services Ltd, 1996

  Component:
    TDBGlyphLookupListBox - Eric Salmon 13/04/1999
    TDBGlyphLookupComboBox - Eric Salmon 13/04/1999

  Updates:

  Packages:
    DBInHouse, Delphi 4 package for in house components.
    DBInHouse5, Delphi 5 package for in house components.

  Description:
    TDBGlyphLookupListBox:

    TDBGlyphLookupComboBox:


  Additional information:

===============================================================================}

unit DBGlyphCtrls;

{$R-}
{$INCLUDE DelphiVersions.Inc}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, Graphics, Db, DbCtrls
  {$IFDEF DELPHI7UP}, VDBConsts, Variants {$ENDIF};

type
  TDBGlyphLookupControl = class;

  TGlyphDataSourceLink = class(TDataLink)
  private
    FDBGlyphLookupControl: TDBGlyphLookupControl;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create;
  end;  // TGlyphDataSourceLink

  //----------------------------------------------------------------------------
  TGlyphListSourceLink = class(TDataLink)
  private
    FDBGlyphLookupControl: TDBGlyphLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
  public
    constructor Create;
  end;  // TGlyphListSourceLink

  //----------------------------------------------------------------------------
  TDBGlyphLookupControl = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TGlyphDataSourceLink;
    FListLink: TGlyphListSourceLink;
    FDataFieldName: string;
    FKeyFieldName: string;
    FListFieldName: string;
    FGlyphFieldName:string;  //  Added
    FListFieldIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FListField: TField;
    FGlyphField:TField;      // Added
    FGlyph:TBitmap;          // Added
    FItemHeight:integer;     // Added
    FListFields: TList;
    FKeyValue: Variant;
    FSearchText: string;
    FLookupMode: Boolean;
    FListActive: Boolean;
    FHasFocus: Boolean;
    procedure CheckNotCircular;
    procedure CheckNotLookup;
    procedure DataLinkRecordChanged(Field: TField);
    function GetDataSource: TDataSource;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetGlyphFieldName(const Value: string);  // Added
    procedure SetItemHeight(const Value:integer);      // Added
    function GetItemHeight:integer;                    // Added
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
  protected
    function CanModify: Boolean; virtual;
    function GetBorderSize: Integer; virtual;
    function GetTextHeight: Integer; virtual;
    procedure KeyValueChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    function LocateKey: Boolean; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ProcessSearchKey(Key: Char); virtual;
    procedure SelectKeyValue(const Value: Variant); virtual;
    procedure UpdateDataFields; virtual;
    procedure UpdateListFields; virtual;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property GlyphField: string read FGlyphFieldName write SetGlyphFieldName;  // Added
    property HasFocus: Boolean read FHasFocus;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;    // Added
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListActive: Boolean read FListActive;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListFields: TList read FListFields;
    property ListLink: TGlyphListSourceLink read FListLink;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchText: string read FSearchText write FSearchText;
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read FDataField;
  end; // TDBGlyphLookupControl

  //----------------------------------------------------------------------------
  { TDBGlyphLookupListBox }
  TDBGlyphLookupListBox = class(TDBGlyphLookupControl)
  private
    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FBorderStyle: TBorderStyle;
    FPopup: Boolean;
    FKeySelected: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FLockPosition: Boolean;
    FMousePos: Integer;
    FSelectedItem: string;
    function GetKeyIndex: Integer;
    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;
    procedure SetItemHeight(const Value:integer);      // Added
    function GetItemHeight:integer;                    // Added
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyValueChanged; override;
    procedure ListLinkDataChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateListFields; override;
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property KeyValue;
    property SelectedItem: string read FSelectedItem;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GlyphField;  // Added
    property ImeMode;
    property ImeName;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;    // Added
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;  // TDBGlyphLookupListBox

  //----------------------------------------------------------------------------
  { TDBGlyphLookupComboBox }
  TGlyphPopupDataList = class(TDBGlyphLookupListBox)
  private
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;  // TGlyphPopupDataList

  TDBGlyphLookupComboBox = class(TDBGlyphLookupControl)
  private
    FDataList: TGlyphPopupDataList;
    FButtonWidth: Integer;
    FText: string;
    FDropDownRows: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TDropDownAlign;
    FListVisible: Boolean;
    FPressed: Boolean;
    FTracking: Boolean;
    FAlignment: TAlignment;
    FLookupMode: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyValueChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateListFields; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure DropDown; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property KeyValue;
    property ListVisible: Boolean read FListVisible;
    property Text: string read FText;
  published
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property Enabled;
    property Font;
    property GlyphField;  // Added
    property ImeMode;
    property ImeName;
    property ItemHeight;  // Added
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;  // TDBGlyphLookupComboBox

//==============================================================================
implementation

uses Clipbrd, DBConsts, Dialogs;

//==============================================================================
{ TGlyphDataSourceLink }

constructor TGlyphDataSourceLink.Create;
begin
  inherited Create;
  VisualControl:=true;
end;  // Create

//==============================================================================
procedure TGlyphDataSourceLink.ActiveChanged;
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.UpdateDataFields;
end;  // ActiveChanged

//==============================================================================
procedure TGlyphDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FDBGlyphLookupControl.Field) and
    (FDBGlyphLookupControl <> nil) and FDBGlyphLookupControl.CanFocus then
  begin
    Field^ := nil;
    FDBGlyphLookupControl.SetFocus;
  end;
end;  // FocusControl

//==============================================================================
procedure TGlyphDataSourceLink.LayoutChanged;
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.UpdateDataFields;
end;  // LayoutChanged

//==============================================================================
procedure TGlyphDataSourceLink.RecordChanged(Field: TField);
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.DataLinkRecordChanged(Field);
end;  // RecordChanged

//==============================================================================
//==============================================================================
{ TGlyphListSourceLink }

constructor TGlyphListSourceLink.Create;
begin
  inherited Create;
  VisualControl:=true;
end;  // Create

//==============================================================================
procedure TGlyphListSourceLink.ActiveChanged;
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.UpdateListFields;
end;  // ActiveChanged

//==============================================================================
procedure TGlyphListSourceLink.DataSetChanged;
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.ListLinkDataChanged;
end;  // DataSetChanged

//==============================================================================
procedure TGlyphListSourceLink.LayoutChanged;
begin
  if FDBGlyphLookupControl <> nil then FDBGlyphLookupControl.UpdateListFields;
end;  // LayoutChanged

//==============================================================================
//==============================================================================
{ TDBGlyphLookupControl }

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;  // VarEqual

var
  SearchTickCount: Integer = 0;

//==============================================================================
constructor TDBGlyphLookupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := [csOpaque] else
    ControlStyle := [csOpaque, csFramed];
  ParentColor := False;
  TabStop := True;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TGlyphDataSourceLink.Create;
  FDataLink.FDBGlyphLookupControl := Self;
  FListLink := TGlyphListSourceLink.Create;
  FListLink.FDBGlyphLookupControl := Self;
  FListFields := TList.Create;
  FKeyValue := Null;
  FGlyph:=TBitmap.Create;  // Added
end;  // Create

//==============================================================================
destructor TDBGlyphLookupControl.Destroy;
begin
  FGlyph.Free;      // Added
  FListFields.Free;
  FListLink.FDBGlyphLookupControl := nil;
  FListLink.Free;
  FDataLink.FDBGlyphLookupControl := nil;
  FDataLink.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
function TDBGlyphLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;  // CanModify

//==============================================================================
procedure TDBGlyphLookupControl.CheckNotCircular;
begin
  if FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource) then
    DatabaseError(SCircularDataLink);
end;  // CheckNotCircular

//==============================================================================
procedure TDBGlyphLookupControl.CheckNotLookup;
begin
  if FLookupMode then DatabaseError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then DatabaseError(SDataSourceFixed);
end;  // CheckNotLookup

//==============================================================================
procedure TDBGlyphLookupControl.UpdateDataFields;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := GetFieldProperty(FDataLink.DataSet, Self, FDataFieldName);
    FMasterField := FDataField;
  end;
  SetLookupMode((FDataField <> nil) and (FDataField.FieldKind = fkLookup));
  DataLinkRecordChanged(nil);
end;  // UpdateFields

//==============================================================================
procedure TDBGlyphLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
    if FMasterField <> nil then
      SetKeyValue(FMasterField.Value) else
      SetKeyValue(Null);
end;  // DataLinkRecordChange

//==============================================================================
function TDBGlyphLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;
end;  // GetBorderSize

//==============================================================================
function TDBGlyphLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;  // GetDataSource

//==============================================================================
function TDBGlyphLookupControl.GetKeyFieldName: string;
begin
  if FLookupMode then Result := '' else Result := FKeyFieldName;
end;  // GetKeyFieldName

//==============================================================================
function TDBGlyphLookupControl.GetListSource: TDataSource;
begin
  if FLookupMode then Result := nil else Result := FListLink.DataSource;
end;  // GetListSource

//==============================================================================
function TDBGlyphLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;  // GetReadOnly

//==============================================================================
function TDBGlyphLookupControl.GetTextHeight: Integer;
var DC      :HDC;
    SaveFont:HFont;
    Metrics :TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
  if (Result<>FItemHeight) and (GlyphField<>'') then Result:=FItemHeight;  // Added
end;  // GetTextHeight

//==============================================================================
procedure TDBGlyphLookupControl.KeyValueChanged;
begin
end;  // KeyValueChanged

//==============================================================================
procedure TDBGlyphLookupControl.UpdateListFields;
var
  DataSet: TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FGlyphField:=nil;    // Added
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    if GlyphField<>'' then
      FGlyphField:=GetFieldProperty(DataSet,Self,FGlyphFieldName);  // Added
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      DatabaseErrorFmt(SFieldNotFound, [Self.Name, FListFieldName]);
    end;
    if FLookupMode then
    begin
      ResultField := GetFieldProperty(DataSet, Self, FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FListField := ResultField;
    end else
    begin
      if FListFields.Count = 0 then FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex] else
        FListField := FListFields[0];
    end;
    FListActive := True;
  end;
end;  // UpdateListFields

//==============================================================================
procedure TDBGlyphLookupControl.ListLinkDataChanged;
begin
end;  // ListLinkDataChanged

//==============================================================================
function TDBGlyphLookupControl.LocateKey: Boolean;
var
  KeySave: Variant;
begin
  Result := False;
  try
    KeySave := FKeyValue;
    if not VarIsNull(FKeyValue) and FListLink.DataSet.Active and
      FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
    begin
      Result := True;
      FKeyValue := KeySave;
    end;
  except
  end;
end;  // LocateKey

//==============================================================================
procedure TDBGlyphLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then DataSource := nil;
    if (FListLink <> nil) and (AComponent = ListSource) then ListSource := nil;
  end;
end;  // Notification

//==============================================================================
procedure TDBGlyphLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Integer;
  S: string;
  CharMsg: TMsg;
begin
  if (FListField <> nil) and (FListField.FieldKind = fkData) and
    (FListField.DataType = ftString) then
    case Key of
      #8, #27: SearchText := '';
      #32..#255:
        if CanModify then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then SearchText := '';
          SearchTickCount := TickCount;
          if SysLocale.FarEast and (Key in LeadBytes) then
            if PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
            begin
              if CharMsg.Message = WM_Quit then
              begin
                PostQuitMessage(CharMsg.wparam);
                Exit;
              end;
              SearchText := SearchText + Key;
              Key := Char(CharMsg.wParam);
            end;
          if Length(SearchText) < 32 then
          begin
            S := SearchText + Key;
            if FListLink.DataSet.Locate(FListField.FieldName, S,
              [loCaseInsensitive, loPartialKey]) then
            begin
              SelectKeyValue(FKeyField.Value);
              SearchText := S;
            end;
          end;
        end;
    end;
end;  // ProcessSearchKey

//==============================================================================
procedure TDBGlyphLookupControl.SelectKeyValue(const Value: Variant);
begin
  if FMasterField <> nil then
  begin
    if FDataLink.Edit then
      FMasterField.Value := Value;
  end else
    SetKeyValue(Value);
  Repaint;
  Click;
end;  // SelectKeyValue

//==============================================================================
procedure TDBGlyphLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    UpdateDataFields;
  end;
end;  // SetDataFieldName

//==============================================================================
procedure TDBGlyphLookupControl.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;  // SetDataSource

//==============================================================================
////////// Added
procedure TDBGlyphLookupControl.SetGlyphFieldName(const Value: string);
begin
  if FGlyphFieldName <> Value then begin
    FGlyphFieldName:=Value;
    UpdateListFields;
    if FGlyphFieldName='' then ItemHeight:=GetTextHeight;
  end;
end;  // SetGlyphFieldName
//////////
//==============================================================================
////////// Added
procedure TDBGlyphLookupControl.SetItemHeight(const Value:integer);
begin
  if (Value > 0) then begin
    FItemHeight:=Value;
    SetBounds(Left,Top,Width,FItemHeight+6); //Invalidate;
  end;
end;  // SetItemHeight
//////////
//==============================================================================
////////// Added
function TDBGlyphLookupControl.GetItemHeight:integer;
begin
  Result:=GetTextHeight;
  if FItemHeight<>Result then FItemHeight:=Result;
end;  // GetItemHeight
//////////
//==============================================================================
procedure TDBGlyphLookupControl.SetKeyFieldName(const Value: string);
begin
  CheckNotLookup;
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    UpdateListFields;
  end;
end;  // SetKeyFieldName

//==============================================================================
procedure TDBGlyphLookupControl.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;  // SetKeyValue

//==============================================================================
procedure TDBGlyphLookupControl.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;  // SetListFieldName

//==============================================================================
procedure TDBGlyphLookupControl.SetListSource(Value: TDataSource);
begin
  CheckNotLookup;
  FListLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;  // SetListSource

//==============================================================================
procedure TDBGlyphLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := GetFieldProperty(FDataField.DataSet, Self, FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FKeyFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FListLink.DataSource := FLookupSource;
    end else
    begin
      FListLink.DataSource := nil;
      FLookupMode := False;
      FKeyFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;  // SetLookupMode

//==============================================================================
procedure TDBGlyphLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;  // SetReadOnly

//==============================================================================
procedure TDBGlyphLookupControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;  // WMGetDlgCode

//==============================================================================
procedure TDBGlyphLookupControl.WMKillFocus(var Message: TMessage);
begin
  FHasFocus := False;
  Inherited;
  Invalidate;
end;  // WMKillFocus

//==============================================================================
procedure TDBGlyphLookupControl.WMSetFocus(var Message: TMessage);
begin
  FHasFocus := True;
  Inherited;
  Invalidate;
end;  // WMSetFocus

//==============================================================================
function TDBGlyphLookupControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;  // ExecuteAction

//==============================================================================
function TDBGlyphLookupControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;  // UpdateAction

//==============================================================================
//==============================================================================
{ TDBGlyphLookupListBox }

constructor TDBGlyphLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDoubleClicks];
  Width := 121;
  FBorderStyle := bsSingle;
  RowCount := 7;
end;  // Create;

//==============================================================================
procedure TDBGlyphLookupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
end;  // CreateParams

//==============================================================================
procedure TDBGlyphLookupListBox.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;  // CreateWnd

//==============================================================================
function TDBGlyphLookupListBox.GetKeyIndex: Integer;
var
  FieldValue: Variant;
begin
  if not VarIsNull(FKeyValue) then
    for Result := 0 to FRecordCount - 1 do
    begin
      ListLink.ActiveRecord := Result;
      FieldValue := FKeyField.Value;
      ListLink.ActiveRecord := FRecordIndex;
      if VarEquals(FieldValue, FKeyValue) then Exit;
    end;
  Result := -1;
end;  // GetKeyIndex

//==============================================================================
////////// Added
procedure TDBGlyphLookupListBox.SetItemHeight(const Value:integer);
begin
  if (Value > 0) and (Value<>FItemHeight) then begin
    FItemHeight:=Value;
    RecreateWnd;
  end;
end;  // SetItemHeight
//////////
//==============================================================================
////////// Added
function TDBGlyphLookupListBox.GetItemHeight:integer;
begin
  Result:=GetTextHeight;
  if FItemHeight<>Result then FItemHeight:=Result;
end;  // GetItemHeight
//////////
//==============================================================================
procedure TDBGlyphLookupListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta, KeyIndex: Integer;
begin
  inherited KeyDown(Key, Shift);
  if CanModify then
  begin
    Delta := 0;
    case Key of
      VK_UP, VK_LEFT: Delta := -1;
      VK_DOWN, VK_RIGHT: Delta := 1;
      VK_PRIOR: Delta := 1 - FRowCount;
      VK_NEXT: Delta := FRowCount - 1;
      VK_HOME: Delta := -Maxint;
      VK_END: Delta := Maxint;
    end;
    if Delta <> 0 then
    begin
      SearchText := '';
      if Delta = -Maxint then ListLink.DataSet.First else
        if Delta = Maxint then ListLink.DataSet.Last else
        begin
          KeyIndex := GetKeyIndex;
          if KeyIndex >= 0 then
            ListLink.DataSet.MoveBy(KeyIndex - FRecordIndex)
          else
          begin
            KeyValueChanged;
            Delta := 0;
          end;
          ListLink.DataSet.MoveBy(Delta);
        end;
      SelectCurrent;
    end;
  end;
end;  // KeyDown

//==============================================================================
procedure TDBGlyphLookupListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ProcessSearchKey(Key);
end;  // KeyPress

//==============================================================================
procedure TDBGlyphLookupListBox.KeyValueChanged;
begin
  if ListActive and not FLockPosition then
    if not LocateKey then ListLink.DataSet.First;
  if FListField <> nil then
    FSelectedItem := FListField.DisplayText else
    FSelectedItem := '';
end;  // KeyValueChanged

//==============================================================================
procedure TDBGlyphLookupListBox.UpdateListFields;
begin
  try
    inherited;
  finally
    if ListActive then KeyValueChanged else ListLinkDataChanged;
  end;
end;  // UpdateListFields

//==============================================================================
procedure TDBGlyphLookupListBox.ListLinkDataChanged;
begin
  if ListActive then
  begin
    FRecordIndex := ListLink.ActiveRecord;
    FRecordCount := ListLink.RecordCount;
    FKeySelected := not VarIsNull(FKeyValue) or
      not ListLink.DataSet.BOF;
  end else
  begin
    FRecordIndex := 0;
    FRecordCount := 0;
    FKeySelected := False;
  end;
  if HandleAllocated then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;  // ListLinkDataChanged

//==============================================================================
procedure TDBGlyphLookupListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SearchText := '';
    if not FPopup then
    begin
      SetFocus;
      if not HasFocus then Exit;
    end;
    if CanModify then
      if ssDouble in Shift then
      begin
        if FRecordIndex = Y div GetTextHeight then DblClick;
      end else
      begin
        MouseCapture := True;
        FTracking := True;
        SelectItemAt(X, Y);
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;  // MouseDown

//==============================================================================
procedure TDBGlyphLookupListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;  // MouseMove

//==============================================================================
procedure TDBGlyphLookupListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FTracking then
  begin
    StopTracking;
    SelectItemAt(X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;  // MouseUp

//==============================================================================
procedure TDBGlyphLookupListBox.Paint;
var
  I, J, W, X, iTextWidth, iTextHeight, LastFieldIndex: Integer;
  GlyphWidth, GlyphPos, Gap:integer;
  S: string;
  R: TRect;
  Selected: Boolean;
  Field: TField;
  AAlignment: TAlignment;
begin
  Canvas.Font   :=Font;
  iTextWidth    :=Canvas.TextWidth('0');
  iTextHeight   :=Canvas.TextHeight('0');
  LastFieldIndex:=ListFields.Count - 1;

  if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace else
    Canvas.Pen.Color := clBtnShadow;
  for I := 0 to FRowCount - 1 do
  begin
    if Enabled then Canvas.Font.Color := Font.Color
               else Canvas.Font.Color := clGrayText;
    Canvas.Brush.Color := Color;
    Selected := not FKeySelected and (I = 0);
    R.Top := I * ItemHeight; //iTextHeight;
    R.Bottom := R.Top + ItemHeight;  //iTextHeight;
    if I < FRecordCount then
    begin
      ListLink.ActiveRecord := I;
      if not VarIsNull(FKeyValue) and
        VarEquals(FKeyField.Value, FKeyValue) then
      begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
        Selected := True;
      end;
      // Added
      GlyphWidth:=0;
      if FGlyphField<>nil then begin
        FGlyph.Assign(FGlyphField);
        GlyphWidth:=FGlyph.Width+2;
      end;
      ////////
      R.Right := 0;
      for J := 0 to LastFieldIndex do
      begin
        Field := ListFields[J];
        if J < LastFieldIndex then
          W := Field.DisplayWidth * iTextWidth + 4 else
          W := ClientWidth - R.Right;
        S := Field.DisplayText;
        X := 2 + GlyphWidth;  // Modified;
        GlyphPos:=1;          // Added
        AAlignment := Field.Alignment;
        if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
        case AAlignment of
          taRightJustify:
              begin
                X := W - Canvas.TextWidth(S)-GlyphWidth-3;  // Modified
                GlyphPos:=W-GlyphWidth-1;                   // Added
              end;
          taCenter:
              begin
                X := (W - Canvas.TextWidth(S)-GlyphWidth) div 2 + GlyphWidth;  // Modified
                GlyphPos:=X-GlyphWidth;                                        // Added
              end;
        end;
        R.Left := R.Right;
        R.Right := R.Right + W;
        if SysLocale.MiddleEast then TControlCanvas(Canvas).UpdateTextFlags;

        Canvas.FillRect(R);
        if FGlyphField=nil then
          Canvas.TextRect(R, R.Left + X, R.Top, S)  // Original code
        else begin
          // Added
          Gap:=R.Top+(ItemHeight-FGlyph.Height) div 2;
          with Canvas do begin
            TextRect(R, R.Left+X,R.Top+(ItemHeight-iTextHeight) div 2, S);
            StretchDraw(Rect(GlyphPos, Gap, GlyphPos+FGlyph.Width, Gap+FGlyph.Height),FGlyph);
          end;
        end;

        if J < LastFieldIndex then
        begin
          Canvas.MoveTo(R.Right, R.Top);
          Canvas.LineTo(R.Right, R.Bottom);
          Inc(R.Right);
          if R.Right >= ClientWidth then Break;
        end;
      end;
    end;
    R.Left := 0;
    R.Right := ClientWidth;
    if I >= FRecordCount then Canvas.FillRect(R);
    if Selected and (HasFocus or FPopup) then
      Canvas.DrawFocusRect(R);
  end;
  if FRecordCount <> 0 then ListLink.ActiveRecord := FRecordIndex;
end;  // Paint

//==============================================================================
procedure TDBGlyphLookupListBox.SelectCurrent;
begin
  FLockPosition := True;
  try
    SelectKeyValue(FKeyField.Value);
  finally
    FLockPosition := False;
  end;
end;  // SelectCurrent

//==============================================================================
procedure TDBGlyphLookupListBox.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if Y < 0 then Y := 0;
  if Y >= ClientHeight then Y := ClientHeight - 1;
  Delta := Y div GetTextHeight - FRecordIndex;
  ListLink.DataSet.MoveBy(Delta);
  SelectCurrent;
end;  // SelectItemAt

//==============================================================================
procedure TDBGlyphLookupListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
    RowCount := RowCount;
  end;
end;  // SetBorderStyle

//==============================================================================
procedure TDBGlyphLookupListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  BorderSize, TextHeight, Rows: Integer;
begin
  BorderSize := GetBorderSize;
  TextHeight := GetTextHeight;
  Rows := (AHeight - BorderSize) div TextHeight;
  if Rows < 1 then Rows := 1;
  FRowCount := Rows;
  if ListLink.BufferCount <> Rows then
  begin
    ListLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, Rows * TextHeight + BorderSize);
end;  // SetBounds

//==============================================================================
function TDBGlyphLookupListBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;  // UseRightToLeftAlignment

//==============================================================================
procedure TDBGlyphLookupListBox.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  Height := Value * GetTextHeight + GetBorderSize;
end;  // SetRowCount

//==============================================================================
procedure TDBGlyphLookupListBox.StopTimer;
begin
  if FTimerActive then
  begin
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;  // StopTimer

//==============================================================================
procedure TDBGlyphLookupListBox.StopTracking;
begin
  if FTracking then
  begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;  // StopTracking

//==============================================================================
procedure TDBGlyphLookupListBox.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then
  begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then
  begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then StopTimer else
  begin
    if ListLink.DataSet.MoveBy(Delta) <> 0 then SelectCurrent;
    Interval := 200 - Distance * 15;
    if Interval < 0 then Interval := 0;
    SetTimer(Handle, 1, Interval, nil);
    FTimerActive := True;
  end;
end;  // TimerScroll

//==============================================================================
procedure TDBGlyphLookupListBox.UpdateScrollBar;
var
  Pos, Max: Integer;
  ScrollInfo: TScrollInfo;
begin
  Pos := 0;
  Max := 0;
  if FRecordCount = FRowCount then
  begin
    Max := 4;
    if not ListLink.DataSet.BOF then
      if not ListLink.DataSet.EOF then Pos := 2 else Pos := 4;
  end;
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_POS or SIF_RANGE;
  if not GetScrollInfo(Handle, SB_VERT, ScrollInfo) or
    (ScrollInfo.nPos <> Pos) or (ScrollInfo.nMax <> Max) then
  begin
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := Max;
    ScrollInfo.nPos := Pos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;  // UpdateScrollBar

//==============================================================================
procedure TDBGlyphLookupListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
  begin
    RecreateWnd;
    RowCount := RowCount;
  end;
  inherited;
end;  // CMCtl3DChanged

//==============================================================================
procedure TDBGlyphLookupListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
  RowCount:=RowCount;
//  Height := GetTextHeight;
end;  // CMFontChanged

//==============================================================================
procedure TDBGlyphLookupListBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;  // WMCancelMode

//==============================================================================
procedure TDBGlyphLookupListBox.WMTimer(var Message: TMessage);
begin
  TimerScroll;
end;  // WMTimer

//==============================================================================
procedure TDBGlyphLookupListBox.WMVScroll(var Message: TWMVScroll);
begin
  SearchText := '';
  with Message, ListLink.DataSet do
    case ScrollCode of
      SB_LINEUP: MoveBy(-FRecordIndex - 1);
      SB_LINEDOWN: MoveBy(FRecordCount - FRecordIndex);
      SB_PAGEUP: MoveBy(-FRecordIndex - FRecordCount + 1);
      SB_PAGEDOWN: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
      SB_THUMBPOSITION:
        begin
          case Pos of
            0: First;
            1: MoveBy(-FRecordIndex - FRecordCount + 1);
            2: Exit;
            3: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
            4: Last;
          end;
        end;
      SB_BOTTOM: Last;
      SB_TOP: First;
    end;
end;  // WMVScroll

//==============================================================================
function TDBGlyphLookupListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;  // ExecuteAction

//==============================================================================
function TDBGlyphLookupListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;  // UpdateAction

//==============================================================================
//==============================================================================
{ TGlyphPopupDataList }

constructor TGlyphPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  FPopup := True;
end;  // Create

//==============================================================================
procedure TGlyphPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;  // CreateParams

//==============================================================================
procedure TGlyphPopupDataList.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;  // WMMouseActivate

//==============================================================================
//==============================================================================
{ TDBGlyphLookupComboBox }

constructor TDBGlyphLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 145;
  Height := 0;
  FDataList := TGlyphPopupDataList.Create(Self);
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FDataList.OnMouseUp := ListMouseUp;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownRows := 7;
end;  // Create

//==============================================================================
procedure TDBGlyphLookupComboBox.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.KeyValue;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    FDataList.ListSource := nil;
    Invalidate;
    SearchText := '';
    if Accept and CanModify then SelectKeyValue(ListValue);
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  end;
end;  // CloseUp

//==============================================================================
procedure TDBGlyphLookupComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  FDataList.BiDiMode := BiDiMode;
end;  // CMBiDiModeChanged

//==============================================================================
procedure TDBGlyphLookupComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
end;  // CreateParams

//==============================================================================
procedure TDBGlyphLookupComboBox.DropDown;
var
  P: TPoint;
  I, Y: Integer;
  S: string;
  ADropDownAlign: TDropDownAlign;
begin
  if not FListVisible and ListActive then
  begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    FDataList.Color := Color;
    FDataList.Font := Font;
    if FDropDownWidth > 0 then
      FDataList.Width := FDropDownWidth else
      FDataList.Width := Width;
    FDataList.ReadOnly := not CanModify;
    FDataList.KeyField := FKeyFieldName;
    for I := 0 to ListFields.Count - 1 do
      S := S + TField(ListFields[I]).FieldName + ';';
    FDataList.ListField := S;
    FDataList.ListFieldIndex := ListFields.IndexOf(FListField);
    FDataList.ListSource := ListLink.DataSource;
    FDataList.KeyValue := KeyValue;
    FDataList.GlyphField:=FGlyphFieldName;  // Added
    FDataList.ItemHeight:=GetTextHeight;      // Added
    FDataList.RowCount := FDropDownRows;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FDataList.Height > Screen.Height then Y := P.Y - FDataList.Height;
    ADropDownAlign := FDropDownAlign;
    { This alignment is for the ListField, not the control }
    if DBUseRightToLeftAlignment(Self, FListField) then
    begin
      if ADropDownAlign = daLeft then
        ADropDownAlign := daRight
      else if ADropDownAlign = daRight then
        ADropDownAlign := daLeft;
    end;
    case ADropDownAlign of
      daRight: Dec(P.X, FDataList.Width - Width);
      daCenter: Dec(P.X, (FDataList.Width - Width) div 2);
    end;
    SetWindowPos(FDataList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Repaint;
  end;
end;  // DropDown

//==============================================================================
procedure TDBGlyphLookupComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if ListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
    if ssAlt in Shift then
    begin
      if FListVisible then CloseUp(True) else DropDown;
      Key := 0;
    end else
      if not FListVisible then
      begin
        if not LocateKey then
          ListLink.DataSet.First
        else
        begin
          if Key = VK_UP then Delta := -1 else Delta := 1;
          ListLink.DataSet.MoveBy(Delta);
        end;
        SelectKeyValue(FKeyField.Value);
        Key := 0;
      end;
  if (Key <> 0) and FListVisible then FDataList.KeyDown(Key, Shift);
end;  // KeyDown

//==============================================================================
procedure TDBGlyphLookupComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FListVisible then
    if Key in [#13, #27] then
      CloseUp(Key = #13)
    else
      FDataList.KeyPress(Key)
  else
    ProcessSearchKey(Key);
end;  // KeyPress

//==============================================================================
procedure TDBGlyphLookupComboBox.KeyValueChanged;
begin
  if FLookupMode then
  begin
    FText := FDataField.DisplayText;
    FGlyph.Assign(nil);
    FAlignment := FDataField.Alignment;
  end else
  if ListActive and LocateKey then
  begin
    FText := FListField.DisplayText;
    FGlyph.Assign(FGlyphField);
    FAlignment := FListField.Alignment;
  end else
  begin
    FText := '';
    FGlyph.Assign(nil);
    FAlignment := taLeftJustify;
  end;
  Invalidate;
end;  // KeyValueChanged

//==============================================================================
procedure TDBGlyphLookupComboBox.UpdateListFields;
begin
  inherited;
  KeyValueChanged;
end;  // UpdateListFields

//==============================================================================
procedure TDBGlyphLookupComboBox.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FDataList.ClientRect, Point(X, Y)));
end;  // ListMouseUp

//==============================================================================
procedure TDBGlyphLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocus;
    if not HasFocus then Exit;
    if FListVisible then CloseUp(False) else
      if ListActive then
      begin
        MouseCapture := True;
        FTracking := True;
        TrackButton(X, Y);
        DropDown;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;  // MouseDown

//==============================================================================
procedure TDBGlyphLookupComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FDataList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FDataList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;  // MouseMove

//==============================================================================
procedure TDBGlyphLookupComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;  // MouseUp

//==============================================================================
procedure TDBGlyphLookupComboBox.Paint;
var
  W, X, Flags : Integer;
  GlyphWidth, GlyphPos, Gap : integer;
  Text: string;
  AAlignment: TAlignment;
  Selected: Boolean;
  R: TRect;
begin
  // Set colours
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := HasFocus and not FListVisible and
    not (csPaintCopy in ControlState);
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end;
  // Determine alignment
  if (csPaintCopy in ControlState) and (FDataField <> nil) and
    (FDataField.Lookup) then
  begin
    Text := FDataField.DisplayText;
    AAlignment := FDataField.Alignment;
  end else
  begin
    if (csDesigning in ComponentState) and (FDataField = nil) then
      Text := Name else
      Text := FText;
    AAlignment := FAlignment;
  end;
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
  // Determine drawing area
  // Added
  GlyphWidth:=0;
  if Assigned(FGlyph) then  // Use loaded glyph in KeyValueChanged and set display width
    GlyphWidth:=FGlyph.Width+2;
  ////////
  W := ClientWidth - FButtonWidth;
  X := 2+GlyphWidth;    // Modified
  GlyphPos:=1;          // Added
  case AAlignment of
    taRightJustify:
        begin
          X := W-Canvas.TextWidth(Text)-GlyphWidth-3;  // Modified
          GlyphPos:=W-GlyphWidth-1;                    // Added
        end;
    taCenter:
        begin
          X := (W - Canvas.TextWidth(Text)-GlyphWidth) div 2 + GlyphWidth;  // Modified
          GlyphPos:=X-GlyphWidth;                                           // Added
        end;
  end;
  SetRect(R, 1, 1, W - 1, ClientHeight - 1);
  if (BiDiMode = bdRightToLeft) then
  begin
    Inc(X, FButtonWidth);
    Inc(R.Left, FButtonWidth);
    R.Right := ClientWidth;
    Inc(GlyphPos,FButtonWidth);  // Added
  end;
  if SysLocale.MiddleEast then TControlCanvas(Canvas).UpdateTextFlags;
  // Output Text and Glyph
  Canvas.FillRect(R);
  if (FGlyphField=nil) then
    Canvas.TextRect(R, X, 2, Text)
  else begin
    // Added
    Gap:=R.Top+(ItemHeight-FGlyph.Height) div 2;
    with Canvas do begin
      TextRect(R, X, 1+(ItemHeight-TextHeight(Text)) div 2, Text);
      if not VarIsNull(KeyValue) then
        StretchDraw(Rect(GlyphPos, Gap, GlyphPos+FGlyph.Width, Gap+FGlyph.Height),FGlyph);
    end;
  end;
  // Finish drawing control
  if Selected then Canvas.DrawFocusRect(R);
  SetRect(R, W, 0, ClientWidth, ClientHeight);
  if (BiDiMode = bdRightToLeft) then
  begin
    R.Left := 0;
    R.Right:= FButtonWidth;
  end;
  if not ListActive then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE
  else if FPressed then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED
  else
    Flags := DFCS_SCROLLCOMBOBOX;
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
end;  // Paint

//==============================================================================
procedure TDBGlyphLookupComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FGlyphFieldName='' then  // Added
    inherited SetBounds(ALeft, ATop, AWidth, GetTextHeight + GetBorderSize + 4)
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);  // Added
end;  // SetBounds

//==============================================================================
function TDBGlyphLookupComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;  // UseRightToLeftAlignment

//==============================================================================
procedure TDBGlyphLookupComboBox.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;  // StopTracking

//==============================================================================
procedure TDBGlyphLookupComboBox.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  NewState := PtInRect(Rect(ClientWidth - FButtonWidth, 0, ClientWidth,
    ClientHeight), Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    Repaint;
  end;
end;  // TrackButton

//==============================================================================
procedure TDBGlyphLookupComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDataList) then
    CloseUp(False);
end;  // CMCancelMode

//==============================================================================
procedure TDBGlyphLookupComboBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
//    Height := 0;
  end;
  inherited;
end;  // CMCtl3DChanged

//==============================================================================
procedure TDBGlyphLookupComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
//  Height := 0;
end;  // CMFontChanged

//==============================================================================
procedure TDBGlyphLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;  // CMGetDataLink

//==============================================================================
procedure TDBGlyphLookupComboBox.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;  // WMCancelMode

//==============================================================================
procedure TDBGlyphLookupComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;  // WMKillFocus

//==============================================================================
function TDBGlyphLookupComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;  // ExecuteAction

//==============================================================================
function TDBGlyphLookupComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;  // UpdateAction

//==============================================================================
end.
