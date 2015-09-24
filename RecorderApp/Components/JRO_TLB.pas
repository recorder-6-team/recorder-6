unit JRO_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 17/04/2002 15:12:52 from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: C:\Program Files\Common Files\System\ADO\msjro.dll (1)
// IID\LCID: {AC3B8B4C-B6CA-11D1-9F31-00C04FC29D52}\0
// Helpfile: C:\Program Files\Common Files\System\ADO\msjro.chm
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v2.6 ADODB, (C:\Program Files\Common Files\System\ADO\msado15.dll)
//   (3) v4.0 StdVCL, (\\ermintrude\windows\System32\STDVCL40.DLL)
// Errors:
//   Error creating palette bitmap of (TReplica) : Invalid GUID format
//   Error creating palette bitmap of (TJetEngine) : Invalid GUID format
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL, 
  ADODB_TLB;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  JROMajorVersion = 2;
  JROMinorVersion = 6;

  LIBID_JRO: TGUID = '{AC3B8B4C-B6CA-11D1-9F31-00C04FC29D52}';

  IID_IReplica: TGUID = '{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}';
  IID_Filters: TGUID = '{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}';
  IID_Filter: TGUID = '{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}';
  IID_IJetEngine: TGUID = '{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}';
  CLASS_Replica: TGUID = '{D2D139E3-B6CA-11D1-9F31-00C04FC29D52}';
  CLASS_JetEngine: TGUID = '{DE88C160-FF2C-11D1-BB6F-00C04FAE22DA}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum ReplicaTypeEnum
type
  ReplicaTypeEnum = TOleEnum;
const
  jrRepTypeNotReplicable = $00000000;
  jrRepTypeDesignMaster = $00000001;
  jrRepTypeFull = $00000002;
  jrRepTypePartial = $00000003;

// Constants for enum VisibilityEnum
type
  VisibilityEnum = TOleEnum;
const
  jrRepVisibilityGlobal = $00000001;
  jrRepVisibilityLocal = $00000002;
  jrRepVisibilityAnon = $00000004;

// Constants for enum UpdatabilityEnum
type
  UpdatabilityEnum = TOleEnum;
const
  jrRepUpdFull = $00000000;
  jrRepUpdReadOnly = $00000002;

// Constants for enum SyncTypeEnum
type
  SyncTypeEnum = TOleEnum;
const
  jrSyncTypeExport = $00000001;
  jrSyncTypeImport = $00000002;
  jrSyncTypeImpExp = $00000003;

// Constants for enum SyncModeEnum
type
  SyncModeEnum = TOleEnum;
const
  jrSyncModeIndirect = $00000001;
  jrSyncModeDirect = $00000002;
  jrSyncModeInternet = $00000003;

// Constants for enum FilterTypeEnum
type
  FilterTypeEnum = TOleEnum;
const
  jrFilterTypeTable = $00000001;
  jrFilterTypeRelationship = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IReplica = interface;
  IReplicaDisp = dispinterface;
  Filters = interface;
  FiltersDisp = dispinterface;
  Filter = interface;
  FilterDisp = dispinterface;
  IJetEngine = interface;
  IJetEngineDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Replica = IReplica;
  JetEngine = IJetEngine;


// *********************************************************************//
// Interface: IReplica
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E0-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  IReplica = interface(IDispatch)
    ['{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}']
    procedure Set_ActiveConnection(const ppconn: IDispatch); safecall;
    procedure _Set_ActiveConnection(ppconn: OleVariant); safecall;
    function  Get_ActiveConnection: IDispatch; safecall;
    function  Get_ConflictFunction: WideString; safecall;
    procedure Set_ConflictFunction(const pbstr: WideString); safecall;
    function  Get_ConflictTables: _Recordset; safecall;
    function  Get_DesignMasterId: OleVariant; safecall;
    procedure Set_DesignMasterId(pvar: OleVariant); safecall;
    function  Get_Priority: Integer; safecall;
    function  Get_ReplicaId: OleVariant; safecall;
    function  Get_ReplicaType: ReplicaTypeEnum; safecall;
    function  Get_RetentionPeriod: Integer; safecall;
    procedure Set_RetentionPeriod(pl: Integer); safecall;
    function  Get_Visibility: VisibilityEnum; safecall;
    procedure CreateReplica(const replicaName: WideString; const description: WideString; 
                            ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                            Priority: Integer; updatability: UpdatabilityEnum); safecall;
    function  GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool; safecall;
    procedure SetObjectReplicability(const objectName: WideString; const objectType: WideString; 
                                     replicability: WordBool); safecall;
    procedure MakeReplicable(const connectionString: WideString; columnTracking: WordBool); safecall;
    procedure PopulatePartial(const FullReplica: WideString); safecall;
    procedure Synchronize(const target: WideString; syncType: SyncTypeEnum; syncMode: SyncModeEnum); safecall;
    function  Get_Filters: Filters; safecall;
    property ActiveConnection: IDispatch write Set_ActiveConnection;
    property ConflictFunction: WideString read Get_ConflictFunction;
    property ConflictTables: _Recordset read Get_ConflictTables;
    property DesignMasterId: OleVariant read Get_DesignMasterId;
    property Priority: Integer read Get_Priority;
    property ReplicaId: OleVariant read Get_ReplicaId;
    property ReplicaType: ReplicaTypeEnum read Get_ReplicaType;
    property RetentionPeriod: Integer read Get_RetentionPeriod;
    property Visibility: VisibilityEnum read Get_Visibility;
    property Filters: Filters read Get_Filters;
  end;

// *********************************************************************//
// DispIntf:  IReplicaDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E0-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  IReplicaDisp = dispinterface
    ['{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}']
    property ActiveConnection: IDispatch writeonly dispid 1610743808;
    property ConflictFunction: WideString readonly dispid 1610743811;
    property ConflictTables: _Recordset readonly dispid 1610743813;
    property DesignMasterId: OleVariant readonly dispid 1610743814;
    property Priority: Integer readonly dispid 1610743816;
    property ReplicaId: OleVariant readonly dispid 1610743817;
    property ReplicaType: ReplicaTypeEnum readonly dispid 1610743818;
    property RetentionPeriod: Integer readonly dispid 1610743819;
    property Visibility: VisibilityEnum readonly dispid 1610743821;
    procedure CreateReplica(const replicaName: WideString; const description: WideString; 
                            ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                            Priority: Integer; updatability: UpdatabilityEnum); dispid 1610743822;
    function  GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool; dispid 1610743823;
    procedure SetObjectReplicability(const objectName: WideString; const objectType: WideString; 
                                     replicability: WordBool); dispid 1610743824;
    procedure MakeReplicable(const connectionString: WideString; columnTracking: WordBool); dispid 1610743825;
    procedure PopulatePartial(const FullReplica: WideString); dispid 1610743826;
    procedure Synchronize(const target: WideString; syncType: SyncTypeEnum; syncMode: SyncModeEnum); dispid 1610743827;
    property Filters: Filters readonly dispid 1610743828;
  end;

// *********************************************************************//
// Interface: Filters
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E2-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  Filters = interface(IDispatch)
    ['{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}']
    procedure Refresh; safecall;
    function  _NewEnum: IUnknown; safecall;
    function  Get_Count: Integer; safecall;
    function  Get_Item(Index: OleVariant): Filter; safecall;
    procedure Append(const TableName: WideString; FilterType: FilterTypeEnum; 
                     const FilterCriteria: WideString); safecall;
    procedure Delete(Index: OleVariant); safecall;
    property Count: Integer read Get_Count;
    property Item[Index: OleVariant]: Filter read Get_Item; default;
  end;

// *********************************************************************//
// DispIntf:  FiltersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E2-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  FiltersDisp = dispinterface
    ['{D2D139E2-B6CA-11D1-9F31-00C04FC29D52}']
    procedure Refresh; dispid 1610743808;
    function  _NewEnum: IUnknown; dispid -4;
    property Count: Integer readonly dispid 1610743810;
    property Item[Index: OleVariant]: Filter readonly dispid 0; default;
    procedure Append(const TableName: WideString; FilterType: FilterTypeEnum; 
                     const FilterCriteria: WideString); dispid 1610743812;
    procedure Delete(Index: OleVariant); dispid 1610743813;
  end;

// *********************************************************************//
// Interface: Filter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E1-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  Filter = interface(IDispatch)
    ['{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}']
    function  Get_TableName: WideString; safecall;
    function  Get_FilterType: FilterTypeEnum; safecall;
    function  Get_FilterCriteria: WideString; safecall;
    property TableName: WideString read Get_TableName;
    property FilterType: FilterTypeEnum read Get_FilterType;
    property FilterCriteria: WideString read Get_FilterCriteria;
  end;

// *********************************************************************//
// DispIntf:  FilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2D139E1-B6CA-11D1-9F31-00C04FC29D52}
// *********************************************************************//
  FilterDisp = dispinterface
    ['{D2D139E1-B6CA-11D1-9F31-00C04FC29D52}']
    property TableName: WideString readonly dispid 1610743808;
    property FilterType: FilterTypeEnum readonly dispid 1610743809;
    property FilterCriteria: WideString readonly dispid 1610743810;
  end;

// *********************************************************************//
// Interface: IJetEngine
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F63D980-FF25-11D1-BB6F-00C04FAE22DA}
// *********************************************************************//
  IJetEngine = interface(IDispatch)
    ['{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}']
    procedure CompactDatabase(const SourceConnection: WideString; const Destconnection: WideString); safecall;
    procedure RefreshCache(const Connection: _Connection); safecall;
  end;

// *********************************************************************//
// DispIntf:  IJetEngineDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F63D980-FF25-11D1-BB6F-00C04FAE22DA}
// *********************************************************************//
  IJetEngineDisp = dispinterface
    ['{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}']
    procedure CompactDatabase(const SourceConnection: WideString; const Destconnection: WideString); dispid 1610743808;
    procedure RefreshCache(const Connection: _Connection); dispid 1610743809;
  end;

// *********************************************************************//
// The Class CoReplica provides a Create and CreateRemote method to          
// create instances of the default interface IReplica exposed by              
// the CoClass Replica. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoReplica = class
    class function Create: IReplica;
    class function CreateRemote(const MachineName: string): IReplica;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TReplica
// Help String      : JRO Replica Class
// Default Interface: IReplica
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TReplicaProperties= class;
{$ENDIF}
  TReplica = class(TOleServer)
  private
    FIntf:        IReplica;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TReplicaProperties;
    function      GetServerProperties: TReplicaProperties;
{$ENDIF}
    function      GetDefaultInterface: IReplica;
  protected
    procedure InitServerData; override;
    procedure Set_ActiveConnection(const ppconn: IDispatch);
    procedure _Set_ActiveConnection(ppconn: OleVariant);
    function  Get_ActiveConnection: IDispatch;
    function  Get_ConflictFunction: WideString;
    procedure Set_ConflictFunction(const pbstr: WideString);
    function  Get_ConflictTables: _Recordset;
    function  Get_DesignMasterId: OleVariant;
    procedure Set_DesignMasterId(pvar: OleVariant);
    function  Get_Priority: Integer;
    function  Get_ReplicaId: OleVariant;
    function  Get_ReplicaType: ReplicaTypeEnum;
    function  Get_RetentionPeriod: Integer;
    procedure Set_RetentionPeriod(pl: Integer);
    function  Get_Visibility: VisibilityEnum;
    function  Get_Filters: Filters;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IReplica);
    procedure Disconnect; override;
    procedure CreateReplica(const replicaName: WideString; const description: WideString; 
                            ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                            Priority: Integer; updatability: UpdatabilityEnum);
    function  GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool;
    procedure SetObjectReplicability(const objectName: WideString; const objectType: WideString; 
                                     replicability: WordBool);
    procedure MakeReplicable(const connectionString: WideString; columnTracking: WordBool);
    procedure PopulatePartial(const FullReplica: WideString);
    procedure Synchronize(const target: WideString; syncType: SyncTypeEnum; syncMode: SyncModeEnum);
    property  DefaultInterface: IReplica read GetDefaultInterface;
    property ConflictTables: _Recordset read Get_ConflictTables;
    property DesignMasterId: OleVariant read Get_DesignMasterId write Set_DesignMasterId;
    property Priority: Integer read Get_Priority;
    property ReplicaId: OleVariant read Get_ReplicaId;
    property ReplicaType: ReplicaTypeEnum read Get_ReplicaType;
    property Visibility: VisibilityEnum read Get_Visibility;
    property Filters: Filters read Get_Filters;
    property ConflictFunction: WideString read Get_ConflictFunction write Set_ConflictFunction;
    property RetentionPeriod: Integer read Get_RetentionPeriod write Set_RetentionPeriod;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TReplicaProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TReplica
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TReplicaProperties = class(TPersistent)
  private
    FServer:    TReplica;
    function    GetDefaultInterface: IReplica;
    constructor Create(AServer: TReplica);
  protected
    procedure Set_ActiveConnection(const ppconn: IDispatch);
    procedure _Set_ActiveConnection(ppconn: OleVariant);
    function  Get_ActiveConnection: IDispatch;
    function  Get_ConflictFunction: WideString;
    procedure Set_ConflictFunction(const pbstr: WideString);
    function  Get_ConflictTables: _Recordset;
    function  Get_DesignMasterId: OleVariant;
    procedure Set_DesignMasterId(pvar: OleVariant);
    function  Get_Priority: Integer;
    function  Get_ReplicaId: OleVariant;
    function  Get_ReplicaType: ReplicaTypeEnum;
    function  Get_RetentionPeriod: Integer;
    procedure Set_RetentionPeriod(pl: Integer);
    function  Get_Visibility: VisibilityEnum;
    function  Get_Filters: Filters;
  public
    property DefaultInterface: IReplica read GetDefaultInterface;
  published
    property ConflictFunction: WideString read Get_ConflictFunction write Set_ConflictFunction;
    property RetentionPeriod: Integer read Get_RetentionPeriod write Set_RetentionPeriod;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoJetEngine provides a Create and CreateRemote method to          
// create instances of the default interface IJetEngine exposed by              
// the CoClass JetEngine. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoJetEngine = class
    class function Create: IJetEngine;
    class function CreateRemote(const MachineName: string): IJetEngine;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TJetEngine
// Help String      : JRO JetEngine Class
// Default Interface: IJetEngine
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TJetEngineProperties= class;
{$ENDIF}
  TJetEngine = class(TOleServer)
  private
    FIntf:        IJetEngine;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TJetEngineProperties;
    function      GetServerProperties: TJetEngineProperties;
{$ENDIF}
    function      GetDefaultInterface: IJetEngine;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IJetEngine);
    procedure Disconnect; override;
    procedure CompactDatabase(const SourceConnection: WideString; const Destconnection: WideString);
    procedure RefreshCache(const Connection: _Connection);
    property  DefaultInterface: IJetEngine read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TJetEngineProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TJetEngine
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TJetEngineProperties = class(TPersistent)
  private
    FServer:    TJetEngine;
    function    GetDefaultInterface: IJetEngine;
    constructor Create(AServer: TJetEngine);
  protected
  public
    property DefaultInterface: IJetEngine read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

resourcestring
  ResStr_CallConnect =  'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation';

class function CoReplica.Create: IReplica;
begin
  Result := CreateComObject(CLASS_Replica) as IReplica;
end;

class function CoReplica.CreateRemote(const MachineName: string): IReplica;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Replica) as IReplica;
end;

procedure TReplica.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D2D139E3-B6CA-11D1-9F31-00C04FC29D52}';
    IntfIID:   '{D2D139E0-B6CA-11D1-9F31-00C04FC29D52}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TReplica.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IReplica;
  end;
end;

procedure TReplica.ConnectTo(svrIntf: IReplica);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TReplica.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TReplica.GetDefaultInterface: IReplica;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, ResStr_CallConnect );
  Result := FIntf;
end;

constructor TReplica.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TReplicaProperties.Create(Self);
{$ENDIF}
end;

destructor TReplica.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TReplica.GetServerProperties: TReplicaProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TReplica.Set_ActiveConnection(const ppconn: IDispatch);
begin
  DefaultInterface.Set_ActiveConnection(ppconn);
end;

procedure TReplica._Set_ActiveConnection(ppconn: OleVariant);
begin
  DefaultInterface._Set_ActiveConnection(ppconn);
end;

function  TReplica.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function  TReplica.Get_ConflictFunction: WideString;
begin
  Result := DefaultInterface.Get_ConflictFunction;
end;

procedure TReplica.Set_ConflictFunction(const pbstr: WideString);
begin
  DefaultInterface.Set_ConflictFunction(pbstr);
end;

function  TReplica.Get_ConflictTables: _Recordset;
begin
  Result := DefaultInterface.Get_ConflictTables;
end;

function  TReplica.Get_DesignMasterId: OleVariant;
begin
  Result := DefaultInterface.Get_DesignMasterId;
end;

procedure TReplica.Set_DesignMasterId(pvar: OleVariant);
begin
  DefaultInterface.Set_DesignMasterId(pvar);
end;

function  TReplica.Get_Priority: Integer;
begin
  Result := DefaultInterface.Get_Priority;
end;

function  TReplica.Get_ReplicaId: OleVariant;
begin
  Result := DefaultInterface.Get_ReplicaId;
end;

function  TReplica.Get_ReplicaType: ReplicaTypeEnum;
begin
  Result := DefaultInterface.Get_ReplicaType;
end;

function  TReplica.Get_RetentionPeriod: Integer;
begin
  Result := DefaultInterface.Get_RetentionPeriod;
end;

procedure TReplica.Set_RetentionPeriod(pl: Integer);
begin
  DefaultInterface.Set_RetentionPeriod(pl);
end;

function  TReplica.Get_Visibility: VisibilityEnum;
begin
  Result := DefaultInterface.Get_Visibility;
end;

function  TReplica.Get_Filters: Filters;
begin
  Result := DefaultInterface.Get_Filters;
end;

procedure TReplica.CreateReplica(const replicaName: WideString; const description: WideString; 
                                 ReplicaType: ReplicaTypeEnum; Visibility: VisibilityEnum; 
                                 Priority: Integer; updatability: UpdatabilityEnum);
begin
  DefaultInterface.CreateReplica(replicaName, description, ReplicaType, Visibility, Priority, 
                                 updatability);
end;

function  TReplica.GetObjectReplicability(const objectName: WideString; const objectType: WideString): WordBool;
begin
  Result := DefaultInterface.GetObjectReplicability(objectName, objectType);
end;

procedure TReplica.SetObjectReplicability(const objectName: WideString; 
                                          const objectType: WideString; replicability: WordBool);
begin
  DefaultInterface.SetObjectReplicability(objectName, objectType, replicability);
end;

procedure TReplica.MakeReplicable(const connectionString: WideString; columnTracking: WordBool);
begin
  DefaultInterface.MakeReplicable(connectionString, columnTracking);
end;

procedure TReplica.PopulatePartial(const FullReplica: WideString);
begin
  DefaultInterface.PopulatePartial(FullReplica);
end;

procedure TReplica.Synchronize(const target: WideString; syncType: SyncTypeEnum; 
                               syncMode: SyncModeEnum);
begin
  DefaultInterface.Synchronize(target, syncType, syncMode);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TReplicaProperties.Create(AServer: TReplica);
begin
  inherited Create;
  FServer := AServer;
end;

function TReplicaProperties.GetDefaultInterface: IReplica;
begin
  Result := FServer.DefaultInterface;
end;

procedure TReplicaProperties.Set_ActiveConnection(const ppconn: IDispatch);
begin
  DefaultInterface.Set_ActiveConnection(ppconn);
end;

procedure TReplicaProperties._Set_ActiveConnection(ppconn: OleVariant);
begin
  DefaultInterface._Set_ActiveConnection(ppconn);
end;

function  TReplicaProperties.Get_ActiveConnection: IDispatch;
begin
  Result := DefaultInterface.Get_ActiveConnection;
end;

function  TReplicaProperties.Get_ConflictFunction: WideString;
begin
  Result := DefaultInterface.Get_ConflictFunction;
end;

procedure TReplicaProperties.Set_ConflictFunction(const pbstr: WideString);
begin
  DefaultInterface.Set_ConflictFunction(pbstr);
end;

function  TReplicaProperties.Get_ConflictTables: _Recordset;
begin
  Result := DefaultInterface.Get_ConflictTables;
end;

function  TReplicaProperties.Get_DesignMasterId: OleVariant;
begin
  Result := DefaultInterface.Get_DesignMasterId;
end;

procedure TReplicaProperties.Set_DesignMasterId(pvar: OleVariant);
begin
  DefaultInterface.Set_DesignMasterId(pvar);
end;

function  TReplicaProperties.Get_Priority: Integer;
begin
  Result := DefaultInterface.Get_Priority;
end;

function  TReplicaProperties.Get_ReplicaId: OleVariant;
begin
  Result := DefaultInterface.Get_ReplicaId;
end;

function  TReplicaProperties.Get_ReplicaType: ReplicaTypeEnum;
begin
  Result := DefaultInterface.Get_ReplicaType;
end;

function  TReplicaProperties.Get_RetentionPeriod: Integer;
begin
  Result := DefaultInterface.Get_RetentionPeriod;
end;

procedure TReplicaProperties.Set_RetentionPeriod(pl: Integer);
begin
  DefaultInterface.Set_RetentionPeriod(pl);
end;

function  TReplicaProperties.Get_Visibility: VisibilityEnum;
begin
  Result := DefaultInterface.Get_Visibility;
end;

function  TReplicaProperties.Get_Filters: Filters;
begin
  Result := DefaultInterface.Get_Filters;
end;

{$ENDIF}

class function CoJetEngine.Create: IJetEngine;
begin
  Result := CreateComObject(CLASS_JetEngine) as IJetEngine;
end;

class function CoJetEngine.CreateRemote(const MachineName: string): IJetEngine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JetEngine) as IJetEngine;
end;

procedure TJetEngine.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{DE88C160-FF2C-11D1-BB6F-00C04FAE22DA}';
    IntfIID:   '{9F63D980-FF25-11D1-BB6F-00C04FAE22DA}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TJetEngine.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IJetEngine;
  end;
end;

procedure TJetEngine.ConnectTo(svrIntf: IJetEngine);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TJetEngine.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TJetEngine.GetDefaultInterface: IJetEngine;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, ResStr_CallConnect );
  Result := FIntf;
end;

constructor TJetEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TJetEngineProperties.Create(Self);
{$ENDIF}
end;

destructor TJetEngine.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TJetEngine.GetServerProperties: TJetEngineProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TJetEngine.CompactDatabase(const SourceConnection: WideString; 
                                     const Destconnection: WideString);
begin
  DefaultInterface.CompactDatabase(SourceConnection, Destconnection);
end;

procedure TJetEngine.RefreshCache(const Connection: _Connection);
begin
  DefaultInterface.RefreshCache(Connection);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TJetEngineProperties.Create(AServer: TJetEngine);
begin
  inherited Create;
  FServer := AServer;
end;

function TJetEngineProperties.GetDefaultInterface: IJetEngine;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ADO',[TReplica, TJetEngine]);
end;

end.
