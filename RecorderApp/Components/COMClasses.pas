// Contains TCOMKeyList and TCOMKeyItem which use IKeyList and IKeyItem to
// pass key lists from the client application to the COM server application

unit COMClasses;

interface

uses
  Windows, ActiveX, ComObj, DataClasses, Recorder2000_TLB;

type
  TCOMKeyList = class(TComObject, IKeyList)
  private
    FKeyList : TEditableKeyList;
  public
    constructor Create( iKeyList : TKeyList );
    destructor Destroy; override;
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    { TCOMKeyList }
    function Get_ItemCount: Integer; safecall;
    function Get_TableName: WideString; safecall;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;

    property KeyList : TEditableKeyList read FKeyList;
  end;

  TCOMKeyItem = class(TComObject, IKeyItem)
  private
    FItem : TStructItem;
  protected
    function Get_KeyField1: WideString; safecall;
    function Get_KeyField2: WideString; safecall;
  public
    { IUnknown }
    function QueryInterface(const IID:TGuid; out Obj): HResult; stdcall;
    { IDispatch }
    function GetTypeInfoCount( out Count: integer ):HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: integer;
                       DispIDs: pointer):HResult; stdcall;
    function Invoke(DispID: integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
                    var Params; varResult, ExepInfo, ArgErr: Pointer): HResult; stdcall;
    { TCOMKeyItem }
    constructor Create(iItem: TStructItem);
    property KeyField1: WideString read Get_KeyField1;
    property KeyField2: WideString read Get_KeyField2;
  end;

const
  Class_COMKeyList     : TGUID = '{3ADCD3B1-1350-11D3-B6E0-0060979160B0}';
  Class_COMKeyItem     : TGUID = '{3ADCD3B2-1350-11D3-B6E0-0060979160B0}';

//==============================================================================
implementation

uses ComServ;

{ TCOMKeyList }

constructor TCOMKeyList.Create(iKeyList: TKeyList);
begin
  inherited Create;
  FKeyList := TEditablekeyList.Create;
  if iKeyList <> nil then
    FKeyList.Assign(iKeyList);
end;

destructor TCOMKeyList.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;

function TCOMKeyList.Get_ItemCount: Integer;
begin
  result := KeyList.Header.ItemCount;
end;

function TCOMKeyList.Get_TableName: WideString;
begin
  result := KeyList.Header.TableName;
end;

function TCOMKeyList.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.GetKeyItem(iIndex: Integer): IKeyItem;
var
  lCOMKeyItem : TCOMKeyItem;
begin
  lCOMKeyItem := TComKeyItem.Create(KeyList.Items[iIndex]);
  result := lCOMKeyItem as IKeyItem;
end;

function TCOMKeyList.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyList.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;

{ TCOMKeyItem }

constructor TCOMKeyItem.Create(iItem: TStructItem);
begin
  inherited Create;
  FItem := iItem;
end;

function TCOMKeyItem.Get_KeyField1: WideString;
begin
  result := FItem.KeyField1;
end;

function TCOMKeyItem.Get_KeyField2: WideString;
begin
  result := FItem.KeyField2;
end;

function TCOMKeyItem.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: integer; DispIDs: pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := 0
end;

function TCOMKeyItem.GetTypeInfoCount(out Count: integer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.Invoke(DispID: integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; varResult, ExepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := 0;
end;

function TCOMKeyItem.QueryInterface(const IID: TGuid; out Obj): HResult;
begin
  Result := 0;
end;

initialization
  TComObjectFactory.Create(ComServer, TCOMKeyList, Class_COMKeyList,
    'COMKeyList', '', ciMultiInstance, tmApartment);
  TComObjectFactory.Create(ComServer, TCOMKeyItem, Class_COMKeyItem,
    'COMKeyItem', '', ciMultiInstance, tmApartment);
end.
