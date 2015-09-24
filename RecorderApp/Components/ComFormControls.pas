unit ComformControls;

interface

uses
  Classes, Sysutils, ComObj, ActiveX, Recorder2000_TLB, Controls, Forms,
  stdctrls, olectnrs, ExceptionForm, DBGlyphCtrls;

const
  Class_ComForm : TGUID = '{E90D87F3-2D28-11D3-B746-0060085B710F}';
  Class_ComControl : TGUID = '{E90D87FE-2D28-11D3-B746-0060085B710F}';
  Class_ComEdit : TGUID = '{E90D87FF-2D28-11D3-B746-0060085B710F}';
  Class_ComListControl : TGUID = '{F1DA8BD5-2E00-11D3-B747-0060085B710F}';
  Class_ComComboControl : TGUID = '{F1DA8BD6-2E00-11D3-B747-0060085B710F}';
  Class_ComGridControl : TGUID = '{F1DA8BD7-2E00-11D3-B747-0060085B710F}';
  Class_ComLabelControl : TGUID = '{F1DA8BD8-2E00-11D3-B747-0060085B710F}';
  Class_ComTabPageControl : TGUID = '{F1DA8BD9-2E00-11D3-B747-0060085B710F}';
  Class_ComSpatialRefControl : TGUID = '{52723FE1-037B-433F-9EBD-C96BA30AABE1}';

type
  EComAddinError = class(TExceptionPath);

  TComForm = class(TComObject, IRecorderForm)
  private
    function InternalGetControl(const iName: WideString;
      controlParent: TComponent): IUnknown;
  protected
    { Protected declarations }
    FForm : TForm;
    {IRecorderForm stuff}
    function Get_Control(const iName: WideString): IUnknown; safecall;
    procedure EmbedActiveX(const iControl: IUnknown; iClsID : TGUID;
                           const iParent: IRecorderControl;
                           iLeft: Integer; iTop: Integer; iWidth: Integer;
                           iHeight: Integer); safecall;
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;

  public
    constructor Create( iForm : TForm );
  end;

  TComControl = class(TComObject, IRecorderControl)
  protected
    FControl : TControl;

    function Get_Name: WideString; virtual; safecall;
    function Get_Left: Integer; virtual; safecall;
    procedure Set_Left(Value: Integer); virtual; safecall;
    function Get_Top: Integer; virtual; safecall;
    procedure Set_Top(Value: Integer); virtual; safecall;
    function Get_Width: Integer; virtual; safecall;
    procedure Set_Width(Value: Integer); virtual; safecall;
    function Get_Height: Integer; virtual; safecall;
    procedure Set_Height(Value: Integer); virtual; safecall;
    function Get_Visible: WordBool; virtual; safecall;
    procedure Set_Visible(Value: WordBool); virtual; safecall;
    function Get_Enabled: WordBool; virtual; safecall;
    procedure Set_Enabled(Value: WordBool); virtual; safecall;
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure RegisterEventHandler(const iEventName: WideString;
           const EventHandler: IEventHandler); safecall;
  public
    constructor Create(iControl : TControl);
  end;

  { Control to support TCustomEdit descendants }
  TComEdit = class(TComControl, IEditControl)
  protected
    function Get_Text: WideString; safecall;
    procedure Set_Text(const Value: WideString); safecall;
  end;

  TComListControl = class(TComControl, IListControl)
  protected
    function Get_ItemCount: Integer; safecall;
    function Get_Items(iIndex: Integer): WideString; safecall;
    procedure Set_Items(iIndex: Integer; const iText: WideString); safecall;
    function Get_SelectedIndex: Integer; safecall;
    procedure Set_SelectedIndex(Value: Integer); safecall;
    function Get_Selected(iIndex: Integer): Wordbool; safecall;
  end;


  TComComboControl = class(TComControl, IComboControl)
  protected
    function Get_ItemCount: Integer; safecall;
    function Get_Items(iIndex: Integer): WideString; safecall;
    procedure Set_Items(iIndex: Integer; const iText: WideString); safecall;
    function Get_SelectedIndex: Integer; safecall;
    procedure Set_SelectedIndex(Value: Integer); safecall;
  end;


  TComGridControl = class(TComControl, IGridControl)
  protected
    function Get_ColumnCount: Integer; safecall;
    function Get_RowCount: Integer; safecall;
    function Get_CurrentRow: Integer; safecall;
    function Get_CurrentColumn: Integer; safecall;
    function Get_Cells(iX: Integer; iY: Integer): WideString; safecall;
    procedure Set_Cells(iX: Integer; iY: Integer; const iText: WideString); safecall;
  end;


  TComLabelControl = class(TComControl, ILabelControl)
  protected
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
  end;


  TComTabPageControl = class(TComControl, ITabPagecontrol)
  protected
    function Get_Visible: WordBool; override; safecall;
    procedure Set_Visible(Value: WordBool); override; safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
  end;

  TComSpatialRefControl = class(TComControl, ISpatialRefControl)
  protected
    function Get_EnteredRef: WideString; safecall;
    procedure Set_EnteredRef(const Value: WideString); safecall;
    function Get_EnteredSystem: WideString; safecall;
    procedure Set_EnteredSystem(const Value: WideString); safecall;
    function Get_DisplayRef: WideString; safecall;
    procedure Set_DisplayRef(const Value: WideString); safecall;
    function Get_DisplaySystem: WideString; safecall;
    function Get_Qualifier: WideString; safecall;
    procedure Set_Qualifier(const Value: WideString); safecall;
  end;

  { Class to allow us to create new event handlers that call events registered
      in an addin for any control.  Derived from TComponent so we can use
      ownership to free it }
  TProxyNotifyEvent = class(TComponent)
  private
    FEventHandler : IEventHandler;
    FEventName : string;
    FOriginalEvent : TNotifyEvent;
    FNewNotifyvent : TNotifyEvent;
    procedure EventProxy(Sender : TObject);
  public
    Constructor Create(AOwner : TComponent; EventHandler : IEventHandler;
                const iEventName : string); reintroduce;
  end;


implementation

uses ComServ, OLETools, checklst, grids, comctrls, csProp, DBCtrls, SpatialRef;

resourcestring
  ResStr_NotOleControl = 'Cannot embed a non-OLE control onto a form';
  ResStr_AddinTriedNonEvent = 'An addin tried to register an event handler for ' +
                            'a control which does not support that handler : ';
  ResStr_AddinTriedNonNotify = 'An addin tried to register an event handler for ' +
                            'a control but the event is not a Notify event : ';



//==============================================================================
{ TComForm }
//==============================================================================

constructor TComForm.Create(iForm: TForm);
begin
  inherited Create;
  FForm := iForm;
end;


procedure TComForm.EmbedActiveX(const iControl: IUnknown; iClsID : TGUID;
  const iParent: IRecorderControl; iLeft, iTop, iWidth, iHeight: Integer);
var
  lOleProxy : TOLEProxy;
  i : integer;
  lParent : TWinControl;
  lOleObject : IOleObject;
begin
  try
    lOleObject := iControl as IOleObject;
  except
    on EIntfCastError do
      raise EComAddinError.Create(ResStr_NotOleControl);
  end;
  lOleProxy := TOLEProxy.Create(FForm, iClsID, iControl as IOleObject);
  if iParent <> nil then
  begin
    lParent := nil; // default until we locate the parent control
    for i := 0 to FForm.ComponentCount-1 do
      if (FForm.Components[i] is TWinControl) and
         (CompareText(FForm.Components[i].Name, iParent.Name)=0) then
      begin
        lParent := TWincontrol(FForm.Components[i]);
        break; // from loop - parent found
      end;
  end // if iParent<>nil
  else
    { If a nil parent, this means control should embed onto form }
    lParent := FForm;
  with lOleProxy do
  begin
    if lParent <> nil then
      Parent := lParent;
    Left := iLeft;
    Top := iTop;
    Width := iWidth;
    Height := iHeight;
  end; // with        
end;


function TComForm.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TComForm.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0;
end;

function TComForm.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

(**
 * Interface method to return a named control on a form.
 *)
function TComForm.Get_Control(const iName: WideString): IUnknown;
begin
  Result := InternalGetControl(iName, FForm);
end;

(**
 * Internal method to return a named control on a form or frame. Will recurse when it
 * finds frames to look inside.
 *)
function TComForm.InternalGetControl(const iName: WideString; controlParent: TComponent): IUnknown;
var
  i : integer;
begin
  Result := nil; // default
  for i := 0 to controlParent.ComponentCount-1 do
    if (controlParent.Components[i] is TControl) and
       (CompareText(controlParent.Components[i].Name, iName)=0) then
    begin
      { Return the correct instance type according to the requested control }
      if (controlParent.Components[i] is TCustomEdit) or
         (controlParent.Components[i] is TDBGlyphLookupComboBox) then
        Result := TComEdit.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TDBLookupComboBox) then
        Result := TComEdit.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TCustomListBox) then
        Result := TComListControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TCustomComboBox) then
        Result := TComComboControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TStringGrid) then
        Result := TComGridControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TLabel) then
        Result := TComLabelControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TTabSheet) then
        Result := TComTabPageControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else if (controlParent.Components[i] is TSpatialRef) then
        Result := TComSpatialRefControl.Create(TControl(controlParent.Components[i])) as IUnknown
      else // default - normal com control
        Result := TComControl.Create(TControl(controlParent.Components[i])) as IUnknown;
      break; // finished - found our control
    end
    else if (controlParent.Components[i] is TFrame) then begin
      // search inside TFrames
      Result := InternalGetControl(iName, controlParent.Components[i]);
      // break if we found the control we are looking for
      if Result<>nil then
        break;
    end;
end;



//==============================================================================
{ TComControl }
//==============================================================================

{ Constructor just stores the control for future use }
constructor TComControl.Create(iControl: TControl);
begin
  inherited Create;
  FControl := iControl;
end;


{ Lots of accessor methods --->}

function TComControl.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TComControl.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0;
end;

function TComControl.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

function TComControl.Get_Enabled: WordBool;
begin
  Result := FControl.Enabled;
end;

function TComControl.Get_Height: Integer;
begin
  Result := FControl.Height;
end;

function TComControl.Get_Left: Integer;
begin
  Result := FControl.Left;
end;

function TComControl.Get_Name: WideString;
begin
  Result := FControl.Name;
end;

function TComControl.Get_Top: Integer;
begin
  Result := FControl.Top;
end;

function TComControl.Get_Visible: WordBool;
begin
  Result := FControl.Visible;
end;

function TComControl.Get_Width: Integer;
begin
  Result := FControl.Width;
end;

function TComControl.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TComControl.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;


{ Allow addins to register interest in any event that can occur to this
     control (as long as it is a TNotifyEvent) }
procedure TComControl.RegisterEventHandler(const iEventName: WideString;
           const EventHandler: IEventHandler); safecall;
begin
  TProxyNotifyEvent.Create(FControl, EventHandler, iEventName);
end;




procedure TComControl.Set_Enabled(Value: WordBool);
begin
  FControl.Enabled := Value;
end;

procedure TComControl.Set_Height(Value: Integer);
begin
  FControl.Height := Value;
end;

procedure TComControl.Set_Left(Value: Integer);
begin
  FControl.Left := Value;
end;

procedure TComControl.Set_Top(Value: Integer);
begin
  FControl.Top := Value;
end;

procedure TComControl.Set_Visible(Value: WordBool);
begin
  FControl.Visible := Value;
end;

procedure TComControl.Set_Width(Value: Integer);
begin
  FControl.Width := Value;
end;



//==============================================================================
{ TComEdit }
//==============================================================================

{ returns the Text from the edit control }
function TComEdit.Get_Text: WideString;
begin
  if FControl is TDBGlyphLookupComboBox then
    Result := TDBGlyphLookupComboBox(FControl).Text
  else if FControl is TDBLookupComboBox then
    Result := TDBLookupComboBox(FControl).Text
  else
    Result := TCustomEdit(FControl).Text;
end;

{ Sets the text in the edit control }
procedure TComEdit.Set_Text(const Value: WideString);
begin
  if FControl is TCustomEdit then // ignore combos as read only
    TCustomEdit(FControl).Text := Value;
end;



function TComForm.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TComForm.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;


//==============================================================================
{ TComListControl }
//==============================================================================

{ Return the count of items in the list }
function TComListControl.Get_ItemCount: Integer;
begin
  Result := TCustomListBox(FControl).Items.Count;
end;

{ Return a list box item }
function TComListControl.Get_Items(iIndex: Integer): WideString; safecall;
begin
  if iIndex < TCustomListBox(FControl).Items.Count then
    Result := TCustomListBox(FControl).Items[iIndex]
  else
    Result := '';
end;

{ Return the true if the item is selected }
function TComListControl.Get_Selected(iIndex: Integer): WordBool;
begin
  Result := False;  // not selected - default
  if iIndex < TCustomListBox(FControl).Items.Count then
  begin
    { Use check property for Checklistboxes }
    if FControl is TCheckListBox then
      Result := TCheckListbox(FControl).Checked[iIndex]
    else
      Result := TCustomListBox(FControl).Selected[iIndex];
  end;
end;


{ Return the index of the selected item }
function TComListControl.Get_SelectedIndex: Integer;
begin
  Result := TCustomListBox(FControl).ItemIndex;
end;


{ Set the text of an item in the listbox }
procedure TComListControl.Set_Items(iIndex: Integer;
          const iText: WideString); safecall;
begin
  if iIndex < TCustomListBox(FControl).Items.Count then
    TCustomListBox(FControl).Items[iIndex] := iText;
end;


{ Set the selected item in the list box }
procedure TComListControl.Set_SelectedIndex(Value: Integer);
begin
  if Value < TCustomListBox(FControl).Items.Count then
    TCustomListBox(FControl).ItemIndex := Value;
end;



//==============================================================================
{ TComComboControl }
//==============================================================================

{ Return the count of items in the Combo }
function TComComboControl.Get_ItemCount: Integer;
begin
  Result := TCustomComboBox(FControl).Items.Count;
end;

{ Return a Combo box item }
function TComComboControl.Get_Items(iIndex: Integer): WideString; safecall;
begin
  if iIndex < Get_ItemCount then
    Result := TCustomComboBox(FControl).Items[iIndex]
  else
    Result := '';
end;


{ Return the index of the selected item }
function TComComboControl.Get_SelectedIndex: Integer;
begin
  Result := TCustomComboBox(FControl).ItemIndex;
end;


{ Set the text of an item in the Combobox }
procedure TComComboControl.Set_Items(iIndex: Integer;
          const iText: WideString); safecall;
begin
  if iIndex < Get_ItemCount then
    TCustomComboBox(FControl).Items[iIndex] := iText;
end;


{ Set the selected item in the Combo box }
procedure TComComboControl.Set_SelectedIndex(Value: Integer);
begin
  if Value < Get_ItemCount then
    TCustomComboBox(FControl).ItemIndex := Value
end;




//==============================================================================
{ TComGridControl }
//==============================================================================


{ Return text of a particular cell }
function TComGridControl.Get_Cells(iX, iY: Integer): WideString;
begin
  { default if out of bounds }
  Result := '';
  if   (iX >= 0) and (iX < TStringGrid(FControl).ColCount) and
       (iY >= 0) and (iY < TStringGrid(FControl).RowCount) then
    Result := TStringGrid(FControl).Cells[iX, iY];
end;

function TComGridControl.Get_ColumnCount: Integer;
begin
  Result := TStringGrid(FControl).ColCount;
end;

{ Current cell's column }
function TComGridControl.Get_CurrentColumn: Integer;
begin
  Result := TStringGrid(FControl).Col;
end;

{ Current cell's row }
function TComGridControl.Get_CurrentRow: Integer;
begin
  Result := TStringGrid(FControl).Row;
end;

{ Return number of rows in grid }
function TComGridControl.Get_RowCount: Integer;
begin
  Result := TStringGrid(FControl).RowCount;
end;


{ Set the text of a particular grid cell }
procedure TComGridControl.Set_Cells(iX, iY: Integer;
  const iText: WideString);
begin
  if   (iX >= 0) and (iX < TStringGrid(FControl).ColCount) and
       (iY >= 0) and (iY < TStringGrid(FControl).RowCount) then
    TStringGrid(FControl).Cells[iX, iY] := iText;
end;

//==============================================================================
{ TComLabelControl }
//==============================================================================

function TComLabelControl.Get_Caption: WideString;
begin
  Result := TLabel(FControl).Caption;

end;

procedure TComLabelControl.Set_Caption(const Value: WideString);
begin
  TLabel(FControl).Caption := Value;
end;


//==============================================================================
{ TComTabPageControl }
//==============================================================================

function TComTabPageControl.Get_Caption: WideString;
begin
  Result := TTabSheet(FControl).Caption;
end;

{ Override Visible stuff to map it to the TabVisible property }
function TComTabPageControl.Get_Visible: WordBool;
begin
  Result := TTabSheet(FControl).TabVisible;
end;

procedure TComTabPageControl.Set_Caption(const Value: WideString);
begin
  TTabSheet(FControl).Caption := Value;
end;

procedure TComTabPageControl.Set_Visible(Value: WordBool);
begin
  TTabSheet(FControl).TabVisible := Value;
end;


{ TProxyNotifyEvent }

{ Construct a TProxynotifyEvent.  Safety checks, then stores the event name and
    original event handler in private data }
constructor TProxyNotifyEvent.Create(AOwner: TComponent;
  EventHandler: IEventHandler; const iEventName : string);
begin
  inherited Create(AOwner);
  If (Copy(iEventName, 1, 2)<>'On') or
     (not PropertyExists(AOwner, iEventName)) then
    raise EComAddinError.CreateNonCritical(ResStr_AddinTriedNonEvent +
          AOwner.Name + ' - ' + iEventName);
  try
    FOriginalEvent := TNotifyEvent(GetMethodProperty(AOwner, iEventName));
  except on Exception do
    raise EComAddinError.CreateNonCritical(ResStr_AddinTriedNonNotify +
          AOwner.Name + ' - ' + iEventName);
  end;
  { Create a TNotifyEvent pointer to our class method, so we can extract the
      code part }
  FNewNotifyvent := EventProxy;
  { Reassign the original event handler to our new one which will call the
      COM object instead }
  SetMethodProperty(AOwner, iEventName, TMethod(FNewNotifyvent).Code,
                                         TMethod(FNewNotifyvent).Data );
  FEventHandler := EventHandler;
  FEventName := iEventName;
end;



{ Splices a call into the event handler for our parent control.  Calls the
    COM object which set up the new handler, and if the result is true, then
    calls the original event as well }
procedure TProxyNotifyEvent.EventProxy(Sender: TObject);
begin          
  if (FEventHandler.OnEvent( TComponent(Sender).Name, FEventName )) and
      (@FOriginalEvent<>nil) then
    FOriginalEvent(Sender);
end;


{ ---------------------------------------------------------------------------- }
{ TComSpatialRefControl }
{ ---------------------------------------------------------------------------- }

function TComSpatialRefControl.Get_DisplayRef: WideString;
begin
  Result := TSpatialRef(FControl).DisplayRef;
end;

function TComSpatialRefControl.Get_DisplaySystem: WideString;
begin
  Result := TSpatialRef(FControl).DisplaySystem;
end;

function TComSpatialRefControl.Get_EnteredRef: WideString;
begin
  Result := TSpatialRef(FControl).EnteredRef;
end;

function TComSpatialRefControl.Get_EnteredSystem: WideString;
begin
  Result := TSpatialRef(FControl).EnteredSystem;
end;

function TComSpatialRefControl.Get_Qualifier: WideString;
begin
  Result := TSpatialRef(FControl).Qualifier;
end;

procedure TComSpatialRefControl.Set_DisplayRef(const Value: WideString);
begin
  TSpatialRef(FControl).DisplayRef := Value;
end;

procedure TComSpatialRefControl.Set_EnteredRef(const Value: WideString);
begin
  TSpatialRef(FControl).EnteredRef := Value;
end;

procedure TComSpatialRefControl.Set_EnteredSystem(const Value: WideString);
begin
  TSpatialRef(FControl).EnteredSystem := Value;
end;

procedure TComSpatialRefControl.Set_Qualifier(const Value: WideString);
begin
  TSpatialRef(FControl).Qualifier := Value;
end;

initialization

  TComObjectFactory.Create(ComServer, TComForm, Class_ComForm,
    'ComForm', 'Class for accessing Recorder Forms', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComControl, Class_ComControl,
    'ComControl', 'Class for accessing Recorder controls', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComEdit, Class_ComEdit,
    'ComEdit', 'Class for accessing Recorder edit controls', ciMultiInstance, tmApartment);
    TComObjectFactory.Create(ComServer, TComListControl, Class_ComListControl,
    'ComEdit', 'Class for accessing Recorder list controls', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComComboControl, Class_ComComboControl,
    'ComEdit', 'Class for accessing Recorder combo controls', ciMultiInstance, tmApartment);
    TComObjectFactory.Create(ComServer, TComGridControl, Class_ComGridControl,
    'ComEdit', 'Class for accessing Recorder grid controls', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComLabelControl, Class_ComLabelControl,
    'ComEdit', 'Class for accessing Recorder label controls', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComTabPageControl, Class_ComTabPageControl,
    'ComEdit', 'Class for accessing Recorder tabsheet controls', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TComSpatialRefControl, Class_ComSpatialRefControl,
    'ComEdit', 'Class for accessing Recorder tabsheet controls', ciMultiInstance, tmApartment);
end.
