{ DSS In-house unit
   Copyright Dorset Software Services Ltd 1999.
   Provides tools for OLE work, including reading class information from the
   registrty. }

{$I DelphiVersions.Inc}

unit OleTools;

interface

uses
  Classes, {$IFDEF DELPHI7UP} CustomOleCtrls7 {$ELSE} CustomOleCtrls {$ENDIF},
  Sysutils, Controls, ActiveX, Windows, Messages, APIUtils;

type

  EOleToolException = class(Exception);

  { Class which can be connected to an ActiveX or automation object, to allow
       it to be displayed on a form as a WinControl (ie it has a parent etc) }
  TOLEProxy = class(TCustomOleControl)
  private
    FIntf: IUnknown;
    FCLSID : TGuid;
    function  GetControlInterface: IUnknown;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    constructor Create(AOwner : TComponent; CLSID : TGUID; iInstance : IOleObject );
                               reintroduce; overload;
    constructor Create(AOwner : TComponent; CLSID : TGUID ); reintroduce; overload;
    constructor Create(AOwner : TComponent; iInstance : IOleObject ); reintroduce; overload;
    property  ControlInterface: IUnknown read GetControlInterface;
    property AssignedGuid : TGUID read FCLSID;
  end;

  { Some useful global functions }

  { Returns true if 2 guids are same }
  function CompareGUIDs( iGuid1, iGuid2 : TGUID ) : Boolean;

  { This routine searches the registry for classes obtained from the com server.
     Values are returned in a string list of GUIDS }
  function GetClassesForComServer(const iFileName: string): TStringList;

implementation

uses
  ComObj, Registry;

resourcestring
  ResStr_CantReadRegistry = 'Failed to open registry key';


{ Returns true if 2 guids are same }
function CompareGUIDs( iGuid1, iGuid2 : TGUID ) : Boolean;
begin
  { Easiest way is to convert them to strings }
  Result := (GuidToString(iGuid1) = GuidToString(iGuid2));
end;

{ This routine searches the registry for classes obtained from the com server.
     Values are returned in a string list of GUIDS }
(*function GetClassesForComServer(const iFileName: string): TStringList;
var
  i : integer;
  lAllClasses : Tstringlist;
  lNewClasses : TStringList;
  lFoundServer : string;
  lRegistry : TRegistry;
  lShortFileName : string;
begin
  lRegistry := TRegistry.Create;
  lAllClasses := TStringList.Create;
  { Look in the Classes registration stuff}
  lNewClasses := TStringList.Create;
  try
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if lRegistry.OpenKeyReadOnly('Software\Classes\CLSID') then
    begin
      { Note that server paths seem to be stored in the registry as old 8.3 format
          for Delphi addins, but long paths for some others (vb).  Therefore we test
          short file path against short file path to be sure }
      lShortFileName := ExtractShortPathName(iFileName);
      lRegistry.GetkeyNames(lAllClasses);
      { Loop through all registered classes, looking for ones in the new server }
      for i := 0 to lAllClasses.Count-1 do
      begin
        lRegistry.CloseKey;
        if lRegistry.OpenKeyReadOnly('Software\Classes\CLSID\' +
                                        lAllClasses[i] + '\InprocServer32') then
        begin
          { Read the server - use default value }
          lFoundServer := lRegistry.ReadString('');
          { If its ours, then store the class }
          if (CompareText(ExtractShortPathName(lFoundServer), lShortFileName) = 0) then
            lNewClasses.Add(lAllClasses[i]);
        end;
      end;
    end else
      raise EOleToolException.Create(ResStr_CantReadRegistry);
  finally
    lRegistry.Free;
    lAllClasses.Free;
  end; // try..finally
  Result := lNewClasses;
end;*)


{ This routine searches the registry for classes obtained from the com server.
     Values are returned in a string list of GUIDS.  Caller is respoonsible for
     freeing this list }
function GetClassesForComServer(const iFileName: string): TStringList;
var
  lClasses : TStringList;
  lTypeLib : ITypeLib;
  lItemTypeInfo : ITypeInfo;
  lpTypeAttr : PTypeAttr;
  liIndex : integer;
begin
  lClasses := TStringList.Create;
  gpcAPIResultCheck(LoadTypeLib(PWideChar(WideString(iFilename)),lTypeLib));
  for liIndex := 0 to lTypeLib.GetTypeInfoCount-1 do begin
    gpcApiResultCheck(lTypeLib.GetTypeInfo(liIndex, lItemTypeInfo));
    gpcApiResultCheck(lItemTypeInfo.GetTypeAttr(lpTypeAttr));
    lClasses.Add(GuidToString(lpTypeAttr.guid));
  end;
  Result := lClasses;
end;

//==============================================================================
constructor TOLEProxy.Create(AOwner: TComponent; CLSID: TGUID);
begin
  FCLSID := CLSID;
  inherited Create(AOwner);
end;

//==============================================================================
constructor TOLEProxy.Create(AOwner: TComponent; CLSID: TGUID;
                                     iInstance : IOleObject);
begin
  FCLSID := CLSID;
  FOleObject := iInstance;
  inherited Create(AOwner);
end;


//==============================================================================
constructor TOLEProxy.Create(AOwner: TComponent; iInstance : IOleObject);
begin
  iInstance.GetUserClassID(FCLSID);
  FOleObject := iInstance;
  inherited Create(AOwner);
end;


//==============================================================================
procedure TOLEProxy.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  ControlData.ClassID := FCLSID;
end;


procedure TOLEProxy.CreateControl;
  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IUnknown;
  end;
begin
  if FIntf = nil then DoCreate;
end;


function TOLEProxy.GetControlInterface: IUnknown;
begin
  CreateControl;
  Result := FIntf;
end;



end.
