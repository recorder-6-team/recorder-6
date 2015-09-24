unit VagueDateLatLong;

interface

uses
  SysUtils, Windows, Recorder2000_TLB, VagueDate, SpatialRefFuncs;

type
  TAppVagueDate=Class(TInterfacedObject, IVagueDate)
  private
    FStartDate, FEndDate: TDateTime;
    FDateType, FErrorMsg: WideString;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
        Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall;
    function Get_EndDate: TDateTime; safecall;
    function Get_ErrorMsg: WideString; safecall;
    function Get_StartDate: TDateTime; safecall;
    function Get_DateType: WideString; safecall;
  public
    constructor Create(iStartDate, iEndDate: TDateTime; iDateType, iErrorMsg: WideString);
    property StartDate: TDateTime read Get_StartDate;
    property EndDate: TDateTime read Get_EndDate;
    property DateType: WideString read Get_DateType;
    property ErrorMsg: WideString read Get_ErrorMsg;
  end;

  TAppLatLong=Class(TInterfacedObject, ILatLong)
  private
    FLatitude: Double;
    FLongitude: Double;
    FErrorMsg: WideString;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall;
  public
    constructor Create(iLatitude, iLongitude: Double; iErrorMsg: WideString);
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function Get_ErrorMsg: WideString; safecall;
    Property Latitude: Double read FLatitude;
    Property Longitude: Double read FLongitude;
    Property ErrorMsg: WideString read FErrorMsg;
  end;

//==============================================================================
implementation

//==============================================================================
{ TAppVagueDate }

constructor TAppVagueDate.Create(iStartDate, iEndDate: TDateTime; iDateType,
  iErrorMsg: WideString);
begin
  FStartDate := iStartDate;
  FEndDate   := iEndDate;
  FDateType  := iDateType;
  FErrorMsg  := iErrorMsg;
end;

function TAppVagueDate.Get_EndDate: TDateTime;
begin
  Result := FEndDate
end;

function TAppVagueDate.Get_ErrorMsg: WideString;
begin
  Result := FErrorMsg;
end;

function TAppVagueDate.Get_StartDate: TDateTime;
begin
  Result := FStartDate
end;

function TAppVagueDate.Get_DateType: WideString;
begin
  Result := FDateType;
end;

function TAppVagueDate.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppVagueDate.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppVagueDate.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppVagueDate.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

//==============================================================================
{ TAppLatLong }

constructor TAppLatLong.Create(iLatitude, iLongitude: Double; iErrorMsg:WideString);
begin
  FLatitude  := iLatitude;
  FLongitude := iLongitude;
  FErrorMsg  := iErrorMsg;
end;

function TAppLatLong.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppLatLong.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppLatLong.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TAppLatLong.Get_ErrorMsg: WideString;
begin
  Result := FErrorMsg;
end;

function TAppLatLong.Get_Latitude: Double;
begin
  Result := FLatitude;
end;

function TAppLatLong.Get_Longitude: Double;
begin
  Result := FLongitude;
end;

function TAppLatLong.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

//==============================================================================
end.
