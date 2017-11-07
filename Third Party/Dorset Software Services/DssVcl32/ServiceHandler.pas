//==============================================================================
//  Unit: ServiceHandler
//
//  Implements :TServiceHandler
//
//  Description : Generic class for handling services
//
//  Created: 25/3/2003
//
//  Last Revision Details:
//    $Revision: 4 $
//    $Date: 8/05/09 10:12 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================
unit ServiceHandler;

interface

uses
  Windows, Forms, Controls, SvcMgr, WinSvc, Classes, SysUtils, Registry, ApiUtils,
  GeneralFunctions;

type
  EServiceError = class(Exception);

  TOnStatusUpdate = procedure(Sender: TObject; ServiceStatus: TServiceStatus) of object;

  TServiceHandler = class
  private
    FSCManager: SC_HANDLE;
    FSCService: SC_HANDLE;
    FServiceName: string;
    FOnStatusUpdate: TOnStatusUpdate;
    procedure GetServiceManager;
    procedure GetService;
  public
    constructor Create(const AServiceName: String); reintroduce;
    destructor Destroy; override;
    procedure EnableServiceIfDisabled(const AStartType: DWord);
    function GetStatus: DWORD;
    procedure OpenServiceConnection(const serviceName: String);
    function QueryStatus: TServiceStatus;
    function ServiceExists: Boolean;
    class procedure ServicesList(services: TStrings);
    procedure Start;
    function Stop: Boolean;
    property ServiceHandle: SC_HANDLE read FSCService;
    property OnStatusUpdate: TOnStatusUpdate read FOnStatusUpdate write FOnStatusUpdate;
  end;

function ServiceStatusToString(serviceStatus: DWORD): String;

implementation

resourcestring
  ResStr_ErrorOpeningService = 'Error opening ''%s'' service.';
  ResStr_ErrorOpeningManager = 'Error opening Service Manager.';
  ResStr_ServiceNotFound     = 'Service %s not found in the registry';

  ResStr_Stopped             = 'Stopped';
  ResStr_Start_Pending       = 'Start pending';
  ResStr_Stop_Pending        = 'Stop pending';
  ResStr_Running             = 'Running';
  ResStr_Continue_Pending    = 'Continue pending';
  ResStr_Pause_Pending       = 'Pause pending';
  ResStr_Paused              = 'Paused';
  ResStr_Unknown             = 'Status unknown';

const
  REG_KEY_SERVICES           = 'System\CurrentControlSet\Services\';
  REG_VAL_START              = 'Start';
  REG_REQUIRED_PRIVILEGES    = 'RequiredPrivileges';

{-------------------------------------------------------------------------------
}
function ServiceStatusToString(serviceStatus: DWORD): String;
begin
  case serviceStatus of
    SERVICE_STOPPED         : Result := ResStr_Stopped;
    SERVICE_START_PENDING   : Result := ResStr_Start_Pending;
    SERVICE_STOP_PENDING    : Result := ResStr_Stop_Pending;
    SERVICE_RUNNING         : Result := ResStr_Running;
    SERVICE_CONTINUE_PENDING: Result := ResStr_Continue_Pending;
    SERVICE_PAUSE_PENDING   : Result := ResStr_Pause_Pending;
    SERVICE_PAUSED          : Result := ResStr_Paused;
  else
    Result := ResStr_Unknown;
  end
end;  // ServiceStatusToString

{-------------------------------------------------------------------------------
  Constructor connects to the service.
}
constructor TServiceHandler.Create(const AServiceName: String);
begin
  inherited Create;
  FServiceName := AServiceName;
  GetServiceManager;
  GetService;
end;  // TServiceHandler.Create

{-------------------------------------------------------------------------------
  Closes the service manager objects.
}
destructor TServiceHandler.Destroy;
begin
  CloseServiceHandle(FSCService);
  CloseServiceHandle(FSCManager);
  inherited;
end;  // TServiceHandler.Destroy

{-------------------------------------------------------------------------------
  Prepares the Service manager handle.
}
procedure TServiceHandler.GetServiceManager;
begin
  // open a service manager
  FSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if FSCManager = 0 then
    raise EServiceError.Create(ResStr_ErrorOpeningManager);
end;  // TServiceHandler.GetServiceManager

{-------------------------------------------------------------------------------
  Gets a service object handle.
}
procedure TServiceHandler.GetService;
begin
  FSCService := OpenService(FSCManager, PChar(FServiceName), SERVICE_ALL_ACCESS);
  if FSCService = 0 then
    raise EServiceError.Create(Format(ResStr_ErrorOpeningService, [FServiceName]));
end;  // TServiceHandler.GetService

{-------------------------------------------------------------------------------
  Connects to a new service object.
}
procedure TServiceHandler.OpenServiceConnection(const serviceName: String);
begin
  if FSCService <> 0 then CloseServiceHandle(FSCService);
  FServiceName := serviceName;
  GetService;
end;  // TServiceHandler.OpenServiceConnection

{-------------------------------------------------------------------------------
  Checks a service exists on the machine by looking at the list of services
  in the registry. Restricted to services that can be started.
}
function TServiceHandler.ServiceExists: Boolean;
begin
  Result := False;
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access := KEY_READ;
      if OpenKeyReadOnly(REG_KEY_SERVICES + FServiceName) then
        Result := ValueExists(REG_VAL_START);
      CloseKey;
    finally
      Free;
    end;
end;  // TServiceHandler.ServiceExists

{-------------------------------------------------------------------------------
  Enables a disabled service. Sets the start type to the supplied parameter,
  e.g. SERVICE_AUTO_START.
}
procedure TServiceHandler.EnableServiceIfDisabled(const AStartType : DWord);
begin
  // Check service is not disabled, and if so, enabled it
  if not ServiceExists then
    raise EServiceError.Create(Format(ResStr_ServiceNotFound, [FServiceName]))
  else
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        OpenKey(REG_KEY_SERVICES + FServiceName, False);
        Access := KEY_READ + KEY_WRITE;
        if ReadInteger(REG_VAL_START) = SERVICE_DISABLED then
          ChangeServiceConfig(
              FSCService,              // hService
              SERVICE_NO_CHANGE,    // dwServiceType
              AStartType,           // dwStartType
              0,                    // dwErrorControl
              nil,                  // lpBinaryPathName
              nil,                  // lpLoadOrderGroup
              nil,                  // lpdwTagId
              nil,                  // lpDependencies
              nil,                  // lpServiceStartName
              nil,                  // lpPassword
              nil);                 // lpDisplayName
      finally
        Free;
      end;
end;  // TServiceHandler.EnableServiceIfDisabled

{-------------------------------------------------------------------------------
  Gets a service object handle.
}
function TServiceHandler.GetStatus: DWORD;
begin
{ Returns the status of the service. Maybe you want to check this
  more than once, so just call this function again.
  Results may be: SERVICE_STOPPED
                  SERVICE_START_PENDING
                  SERVICE_STOP_PENDING
                  SERVICE_RUNNING
                  SERVICE_CONTINUE_PENDING
                  SERVICE_PAUSE_PENDING
                  SERVICE_PAUSED   }
  Result := QueryStatus.dwCurrentState;
end;  // TServiceHandler.GetStatus

{-------------------------------------------------------------------------------
  Queries the status of the current service.
}
function TServiceHandler.QueryStatus: TServiceStatus;
begin
  QueryServiceStatus(ServiceHandle, Result);
  if Assigned(OnStatusUpdate) then OnStatusUpdate(Self, Result);
end;  // TServiceHandler.QueryStatus

{-------------------------------------------------------------------------------
  Starts a service.
}
procedure TServiceHandler.Start;
var
  arg: PChar;
begin
  arg:= nil;
  try
    gpcAPICheck(StartService(FSCService, 0, arg));
  except
    on E:EAPIError do
      // Ignore service already started error
      if E.ErrorCode <> 1056 then raise;
  end;
end;  // TServiceHandler.Start

{-------------------------------------------------------------------------------
  Stops a service.
}
function TServiceHandler.Stop: Boolean;
var
  serviceStatus: TServiceStatus;
  checkPoint: DWORD;
  startWait: DWORD;
  cur: TCursor;
begin
  cur := HourglassCursor;
  try
    if ControlService(FSCService, SERVICE_CONTROL_STOP, serviceStatus) then
      if QueryServiceStatus(FSCService, serviceStatus) then
        while SERVICE_STOPPED <> serviceStatus.dwCurrentState do
        begin
          // Cursor changed to arrow. Wait hourglass really.
          HourglassCursor;

          // For safety, get hold of CheckPoint.
          checkPoint := serviceStatus.dwCheckPoint;
          // Wait before asking status again.

          startWait := GetTickCount;
          // Application is global variable in BOTH Forms AND SvcMgr! Thanks Borland!!!
          while GetTickCount - startWait < serviceStatus.dwWaitHint do
            Forms.Application.ProcessMessages;

          // If can't check status again, something might have gone wrong.
          // But can't continue the loop.
          if not QueryServiceStatus(FSCService, serviceStatus) then
            Break;

          // Checkpoint didn't move since last query.
          // Don't want infinite loop either.
          if serviceStatus.dwCheckPoint < checkPoint then
            Break;
        end;
    // Result is determined by last value in serviceStatus.
    Result := SERVICE_STOPPED = serviceStatus.dwCurrentState;
  finally
    DefaultCursor(cur);
  end;
end;  // TServiceHandler.Stop

{-------------------------------------------------------------------------------
  Returns a list of services installed services. Since not all services can be
  "interacted" with, those with no "Start" value and those, under Vista, with
  a "RequiredPrivileges" value are excluded from the results.
  A more comprehensive check of each service's properties would need to be done
  to remove all those that shouldn't be touched.
}
class procedure TServiceHandler.ServicesList(services: TStrings);
var
  i: Integer;
begin
  services.Clear;
  Screen.Cursor := crHourglass;
  try
    with TRegistry.Create do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        Access  := KEY_READ;
        if OpenKeyReadOnly(REG_KEY_SERVICES) then
        begin
          GetKeyNames(services);
          CloseKey;
        end;

        for i := services.Count - 1 downto 0 do
          if OpenKeyReadOnly(REG_KEY_SERVICES + services[i]) then
          begin
            // Don't know how to "interact" with services with no "Start" value.
            // And those in Vista with the "RequiredPrivileges" value.
            if not ValueExists(REG_VAL_START) or
               ((GetOSVersion >= wvVista) and ValueExists(REG_REQUIRED_PRIVILEGES)) then
              services.Delete(i);
            CloseKey;
          end;
      finally
        Free;
      end;
  finally
    Screen.Cursor := crDefault;
  end;
end;  // TServiceHandler.ServicesList

end.
