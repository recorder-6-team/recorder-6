{===============================================================================
  Unit:        BaseADODataModule

  Defines:     TdmBaseADO

  Description: Base class for any Delphi data module that uses ADO.
               Exposes a number of useful utility methods

  Created:     Jan 2004

  Last revision information:
    $Revision: 30 $
    $Date: 21/02/08 14:13 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseADODataModule;

interface

uses
  SysUtils, Classes, DB, Contnrs, Forms, Controls, Variants, ExceptionForm, ActiveX,
  ADODB_TLB, ADODB, Windows;

const
  PARAM_RECORDS_AFFECTED  = '@RecordsAffected';
  FOREIGN_KEY_CONSTRAINT_FAIL = 547;

type
  EBaseADODataModuleException = class(TExceptionPath);

  EUpdatedException = class(EBaseADODataModuleException)
  end;

  EReferentialIntegrityException = class(EUpdatedException)
  private
    FTableName: String;
    FColumn: String;
  public
    property TableName: String read FTableName;
    property Column: String read FColumn;
    constructor CreateNonCritical(const Message, TableName, Column: String);
  end;

  EDeletedException = class(EBaseADODataModuleException)
  end;

  EForeignKeyException = class(EBaseADODataModuleException);


  TAsyncSQLExecuteCompleteProc = procedure (ADataSet: TCustomADODataSet;
      const AError: ADODB.Error; var AEventStatus: TEventStatus) of object;

  {-----------------------------------------------------------------------------
    Method type of callback procedure for asynchronously loaded controls, where the data
    required is from a stored procedure returning the data in a single output parameter.
  }
  TAsyncExecuteCompleteProc = procedure (ATarget: TObject; AValue: Variant) of object;

  {-----------------------------------------------------------------------------
    Method type of callback procedure for asynchronously loaded controls, where the data
    required is from a stored procedure returning the data in a recordset.
  }
  TAsyncFetchCompleteProc = procedure (ATarget: TObject; ADataset: TCustomADODataset) of
      object;

  {-----------------------------------------------------------------------------
    Method type of callback procedure for asynchronously run stored procs that
        return no dataset or value.
  }
  TAsyncRunCompleteProc = procedure (ATarget: TObject; AError: ADODB.Error) of object;

  TdmBaseADO = class (TDataModule)
    Connection: TADOConnection;
  private
    FAsyncCommands: TThreadList;
    FASyncDatasetPool: TObjectList;
    FAsyncDatasets: TThreadList;
    FParametersCache: TStringList;
    FLog: TextFile;
    FLogAssigned: boolean;
    procedure ConnectionExecuteComplete(Connection: TADOConnection; RecordsAffected: Integer;
        const Error: ADODB.Error; var EventStatus: TEventStatus; const ACommand: _Command; const
        Recordset: _Recordset);
    procedure CreateObjects;
    procedure DatasetFetchComplete(DataSet: TCustomADODataSet; const Error: ADODB.Error; var
        EventStatus: TEventStatus);
  protected
    procedure CancelAllAsyncCommands;
    function GetAsyncDataset: TADODataset;
    procedure OpenConnection(const AConnectionString: String);
    procedure ReleaseAsyncDataset(ADataset: TADODataset);
    procedure SetScreenCursor;
    procedure SetupCommandParameters(AParameters: TParameters; AParams: Array of Variant);
        virtual;
    procedure SetupCommandProperties(ACommand: TADOCommand; AStoredProcName: String; AParams:
        Array of Variant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelAsyncCommands(ACallbackProc: TAsyncExecuteCompleteProc); overload;
    procedure CancelAsyncCommands(ACallbackProc: TAsyncFetchCompleteProc); overload;
    procedure CancelAsyncCommands(ACallbackProc: TAsyncRunCompleteProc); overload;
    function ExecuteSQL(const ASQLText: String; AReturnsRecords: Boolean = False): _Recordset;
    procedure GetAsyncData(const AStoredProcName: String; AParams: Array of Variant; const
        AOutputParamName: String; ATarget: TObject; ACallbackProc: TAsyncExecuteCompleteProc);
    function GetAsyncDatasetFromSQL(const ASQL: String; ACompleteCallbackProc:
        TAsyncSQLExecuteCompleteProc; AProgressCallbackProc: TFetchProgressEvent = nil): TADODataset;
    procedure GetAsyncRecordset(const AStoredProcName: String; AParams: Array of Variant;
        ATarget: TObject; ACallbackProc: TAsyncFetchCompleteProc);
    function GetRecordset(const AStoredProc: String; AParams: Array of Variant): _Recordset;
    function GetStoredProcOutputParam(const AStoredProcName: String; AParams: Array of Variant;
        const AOutputParamName: String): Variant;
    function GetXMLData(const AXMLRootName, AStoredProc: String; AParams: Array of Variant): 
        String;
    procedure Log(const AText: string);
    procedure RefreshParameters(const AProcName: string; AParameters: TParameters);
    procedure RunStoredProc(const AStoredProcName: String; AParams: Array of Variant);
    procedure RunAsyncStoredProc(const AStoredProcName: String; AParams: Array of Variant;
        ATarget: TObject; ACallbackProc: TAsyncRunCompleteProc);
    procedure RunDeleteStoredProc(const AStoredProcName: String; AParams: Array of Variant);
    function RunInsertStoredProc(const ATableName, AStoredProcName: String; AParams: Array of
        Variant; const AOutputParamName: String): Variant;
    procedure RunUpdateStoredProc(const AStoredProcName: String; AParams: Array of Variant);
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralFunctions, StrUtils, ComObj;

resourcestring
  ResStr_DeleteAlreadyUpdated =
    'The attempt to delete this record was unsuccessful because the record has been updated by another user.';
  ResStr_UpdateAlreadyUpdated =
    'The attempt to save your changes was unsuccessful because the record has been updated by another user.';
  ResStr_UpdateAlreadyDeleted =
    'The attempt to save your changes was unsuccessful because the record has been deleted by another user.';
  ResStr_RecordCannotBeDeleted = 'The record cannot be deleted because it has %s records linked to it.';
  ResStr_ForeignKeyConstraintFail = 'The data changes could not be saved to the database because the record is linked to a data item '+
      'that no longer exists.  It may have been deleted by another user.';
  ResStr_ExceptionInMethod = 'Exception occurred in method ';      

const
  // Transact-SQL error returned when TSEqual fails on comparing 2 timestamp values.
  FAILED_TIMESTAMP_CHECK = 532;
  REFERENTIAL_INTEGRITY_ERROR = 547; //changed for SQL server
  // Transact-SQL error returned when insert fails because of duplicate primary key.
  DUPLICATE_PRIMARY_KEY_ON_INSERT = 2627;
  // String returned from stored procedures on SQL Server 2005 with mismatching timestamps
  RECORD_CHANGED_MESSAGE = 'Record updated by another user'; {Do not localize}

type
  {-----------------------------------------------------------------------------
    Assistor classes for asynchronously loaded controls.
  }
  TAsyncAssistor = class (TObject)
  private
    FTarget: TObject;
  public
    property Target: TObject read FTarget write FTarget;
  end;

  TAsyncFetchAssistor = class (TAsyncAssistor)
  private
    FCallbackProc: TAsyncFetchCompleteProc;
    FDataset: TADODataset;
    procedure SetCallbackProc(const Value: TAsyncFetchCompleteProc);
  public
    destructor Destroy; override;
    property CallbackProc: TAsyncFetchCompleteProc read FCallbackProc write SetCallbackProc;
    property Dataset: TADODataset read FDataset write FDataset;
  end;

  TAsyncAssistorNoRecords = class(TAsyncAssistor)
  private
    FCommand: TADOCommand;
  public
    destructor Destroy; override;
    property Command: TADOCommand read FCommand write FCommand;
  end;

  TAsyncExecuteAssistor = class (TAsyncAssistorNoRecords)
  private
    FCallbackProc: TAsyncExecuteCompleteProc;
    FOutputParam: String;
  public
    property CallbackProc: TAsyncExecuteCompleteProc read FCallbackProc write FCallbackProc;
    property OutputParam: String read FOutputParam write FOutputParam;
  end;

  TAsyncRunAssistor = class (TAsyncAssistorNoRecords)
  private
    FCallbackProc: TAsyncRunCompleteProc;
    procedure SetCallbackProc(const Value: TAsyncRunCompleteProc);
  public
    property CallbackProc: TAsyncRunCompleteProc read FCallbackProc write SetCallbackProc;
  end;

  {-----------------------------------------------------------------------------
    Class to persist a list of parameters in the cache.
  }
  TCachedParameters = class (TObjectList)
  private
    procedure AddParam(AParam: TParameter);
  public
    constructor Create(AParameters: TParameters); reintroduce;
    procedure LoadParameterList(AParameters: TParameters);
  end;

  {-----------------------------------------------------------------------------
    Class to hold cached details of a single parameter.  This is required because a TParameter 
    cannot exist in isolation from a command object so is not suitable for caching.
  }
  TCachedParameter = class (TObject)
  private
    FAttributes: TParameterAttributes;
    FDataType: TDataType;
    FDirection: TParameterDirection;
    FName: WideString;
    FNumericScale: Byte;
    FPrecision: Byte;
    FSize: Integer;
  public
    property Attributes: TParameterAttributes read FAttributes write FAttributes;
    property DataType: TDataType read FDataType write FDataType;
    property Direction: TParameterDirection read FDirection write FDirection;
    property Name: WideString read FName write FName;
    property NumericScale: Byte read FNumericScale write FNumericScale;
    property Precision: Byte read FPrecision write FPrecision;
    property Size: Integer read FSize write FSize;
  end;


{-==============================================================================
    TdmBaseADO
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdmBaseADO.Create(AOwner: TComponent);
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  inherited Create(AOwner);

  FLogAssigned := False;
  CreateObjects;
  Connection.OnExecuteComplete := ConnectionExecuteComplete;
  FASyncDatasetPool := TObjectList.Create;
  FASyncDatasetPool.OwnsObjects := False;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.Create', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.Create

{-------------------------------------------------------------------------------
}
destructor TdmBaseADO.Destroy;
var
  lIdx: Integer;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  if Assigned(FParametersCache) then
    for lIdx := 0 to FParametersCache.Count-1 do
      FParametersCache.Objects[lIdx].Free;
  FParametersCache.Free;

  if Assigned(FAsyncCommands) and Assigned(FAsyncDatasets) then
    CancelAllAsyncCommands;

  FAsyncCommands.Free;
  FAsyncDatasets.Free;
  if FLogAssigned then
    CloseFile(FLog);
  FASyncDatasetPool.OwnsObjects := True;    
  FAsyncDatasetPool.Free;

  inherited Destroy;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.Destroy', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.Destroy

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.CancelAllAsyncCommands;
var
  I: Integer;
  lList: TList;
  lObject: TObject;
begin
  lList := FAsyncCommands.LockList;
  try
    for I := lList.Count - 1 downto 0 do
    begin
      lObject := TObject(lList[I]);
      lList.Delete(I);
      lObject.Free;
    end;
  finally
    FAsyncCommands.UnlockList;
  end;

  lList := FAsyncDatasets.LockList;
  try
    for I := lList.Count - 1 downto 0 do
    begin
      lObject := TObject(lList[I]);
      lList.Delete(I);
      lObject.Free;
    end;
  finally
    FAsyncDatasets.UnlockList;
    SetScreenCursor;
  end;
end;  // TdmBaseADO.CancelAllAsyncCommands

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.CancelAsyncCommands(ACallbackProc: 
    TAsyncExecuteCompleteProc);
var
  I: Integer;
  lList: TList;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lList := FAsyncCommands.LockList;
  try
    for I := lList.Count - 1 downto 0 do
      if TObject(lList[I]) is TAsyncExecuteAssistor then
        with TAsyncExecuteAssistor(lList[I]) do
          if Addr(CallbackProc) = Addr(ACallbackProc) then
          begin
            lList.Delete(I);
            Free;
          end;
  finally
    FAsyncCommands.UnlockList;
    SetScreenCursor;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.CancelAsyncCommands TAsyncExecuteCompleteProc', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.CancelAsyncCommands

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.CancelAsyncCommands(ACallbackProc:
    TAsyncFetchCompleteProc);
var
  I: Integer;
  lList: TList;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lList := FAsyncDatasets.LockList;
  try
    for I := lList.Count - 1 downto 0 do
      with TAsyncFetchAssistor(lList[I]) do
        if Addr(CallbackProc) = Addr(ACallbackProc) then
        begin
          lList.Delete(I);
          Free;
        end;
  finally
    FAsyncDatasets.UnlockList;
    SetScreenCursor;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do begin
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.CancelAsyncCommands TAsyncFetchCompleteProc', E)
    end;
  end;
  {$ENDIF}
end;  // TdmBaseADO.CancelAsyncCommands

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.CancelAsyncCommands(ACallbackProc: TAsyncRunCompleteProc);
var
  I: Integer;
  lList: TList;
begin
  lList := FAsyncCommands.LockList;
  try
    for I := lList.Count - 1 downto 0 do
      if TObject(lList[I]) is TAsyncRunAssistor then
        with TAsyncRunAssistor(lList[I]) do
          if Addr(CallbackProc) = Addr(ACallbackProc) then
          begin
            lList.Delete(I);
            Free;
          end;
  finally
    FAsyncCommands.UnlockList;
    SetScreenCursor;
  end;
end;  // TdmBaseADO.CancelAsyncCommands

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.ConnectionExecuteComplete(Connection: TADOConnection;
    RecordsAffected: Integer; const Error: ADODB.Error; var EventStatus: TEventStatus;
    const ACommand: _Command; const Recordset: _Recordset);
var
  I: Integer;
  lList: TList;
  lAssistor: TAsyncAssistorNoRecords;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lAssistor := nil;
  lList := FAsyncCommands.LockList;
  try
    for I := 0 to lList.Count - 1 do
    begin
      if ACommand = TAsyncAssistorNoRecords(lList[I]).Command.CommandObject then
      begin
        lAssistor := TAsyncAssistorNoRecords(lList[I]);
        lList.Delete(I);
        SetScreenCursor;
        Break;
      end;
    end;
  finally
    FAsyncCommands.UnlockList;
  end;

  if Assigned(lAssistor) then
    try
      if lAssistor is TAsyncExecuteAssistor then
        with TAsyncExecuteAssistor(lAssistor) do
        begin
          if stExecuting in Command.States then
            Command.Cancel;
          if Assigned(CallbackProc) then
            CallbackProc(
                Target,
                Command.Parameters.ParamByName(OutputParam).Value);
        end
      else if lAssistor is TAsyncRunAssistor then
        with TAsyncRunAssistor(lAssistor) do
          if Assigned(CallbackProc) then
            CallbackProc(Target, Error);
    finally
      lAssistor.Free;
    end;
  {$IFDEF DEBUG}
  except
    on E:Exception do begin
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.ConnectionExecuteComplete', E)
    end;
  end;
  {$ENDIF}
end;  // TdmBaseADO.ConnectionExecuteComplete

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.CreateObjects;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  FAsyncCommands := TThreadList.Create;
  FAsyncDatasets := TThreadList.Create;
  // Prepare a cache so we don't call Parameters.Refresh more than we need to
  FParametersCache := TStringList.Create;
  FParametersCache.Sorted := True;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.CreateObjects', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.CreateObjects

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.DatasetFetchComplete(DataSet: TCustomADODataSet; const
    Error: ADODB.Error; var EventStatus: TEventStatus);
var
  I: Integer;
  lList: TList;
  lAssistor: TAsyncFetchAssistor;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lAssistor := nil;
  lList := FAsyncDatasets.LockList;
  try
    for I := 0 to lList.Count - 1 do
      if Dataset = TAsyncFetchAssistor(lList[I]).Dataset then
      begin
        lAssistor := TAsyncFetchAssistor(lList[I]);
        lList.Delete(I);
        SetScreenCursor;
        Break;
      end;
  finally
    FAsyncDatasets.UnlockList;
  end;

  if Assigned(lAssistor) then
    try
      if Assigned(lAssistor.CallbackProc) then
        // Call procedure specified when dataset was requested.
        lAssistor.CallbackProc(lAssistor.Target, Dataset);
    finally
      ReleaseAsyncDataset(lAssistor.Dataset);
      lAssistor.Free;
    end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.DatasetFetchComplete', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.DatasetFetchComplete

{-------------------------------------------------------------------------------
}
function TdmBaseADO.ExecuteSQL(const ASQLText: String; AReturnsRecords: Boolean = False):
    _Recordset;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  if AReturnsRecords then
    Result := Connection.Execute(ASQLText, cmdText, [])
  else begin
    Connection.Execute(ASQLText, cmdText, [eoExecuteNoRecords]);
    Result := nil;
  end;

  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.ExecuteSQL', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.ExecuteSQL

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.GetAsyncData(const AStoredProcName: String; AParams: Array of Variant;
    const AOutputParamName: String; ATarget: TObject; ACallbackProc:
    TAsyncExecuteCompleteProc);
var
  lAsyncData: TAsyncExecuteAssistor;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  if AStoredProcName <> '' then begin
    // Setup object.
    lAsyncData := TAsyncExecuteAssistor.Create;
    lAsyncData.CallbackProc := ACallbackProc;
    lAsyncData.Target       := ATarget;
    lAsyncData.OutputParam  := AOutputParamName;
    lAsyncData.Command      := TADOCommand.Create(nil);
    // Setup Dataset properties.
    SetupCommandProperties(lAsyncData.Command, AStoredProcName, AParams);
    with lAsyncData.Command do begin
      Parameters.ParamByName(AOutputParamName).Direction := pdOutput;
      ExecuteOptions  := [eoAsyncExecute, eoExecuteNoRecords];
    end;
    // Add to list
    FAsyncCommands.Add(lAsyncData);
    SetScreenCursor;
    // Open dataset.
    lAsyncData.Command.Execute;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.GetAsyncData', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.GetAsyncData

{-------------------------------------------------------------------------------
  Retrieve a pooled ado dataset object available for reuse
}
function TdmBaseADO.GetAsyncDataset: TADODataset;
begin
  if FASyncDatasetPool.Count>0 then begin
    Result := TADODataset(FASyncDatasetPool[0]);
    FASyncDatasetPool.Delete(0);
  end
  else
    Result := TADODataset.Create(nil);
end;

{-------------------------------------------------------------------------------
}
function TdmBaseADO.GetAsyncDatasetFromSQL(const ASQL: String; ACompleteCallbackProc:
    TAsyncSQLExecuteCompleteProc; AProgressCallbackProc: TFetchProgressEvent = nil): TADODataset;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  Result := TADODataSet.Create(nil);
  if ASQL <> '' then begin
    with Result do begin
      Connection      := Self.Connection;
      CommandType     := cmdText;
      CommandTimeOut  := 0;
      CommandText     := ASQL;
      ExecuteOptions  := [eoAsyncExecute, eoAsyncFetchNonBlocking];
      CacheSize       := 10;
      CursorLocation  := clUseClient;
      CursorType      := ctStatic;
      OnFetchComplete := ACompleteCallbackProc;
      OnFetchProgress := AProgressCallbackProc;
    end;
    // Open dataset.
    Result.Open;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.GetAsyncDatasetFromSQL', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.GetAsyncDatasetFromSQL

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.GetAsyncRecordset(const AStoredProcName: String; AParams: Array of
    Variant; ATarget: TObject; ACallbackProc: TAsyncFetchCompleteProc);
var
  lAsyncData: TAsyncFetchAssistor;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  if AStoredProcName <> '' then begin
    if Screen.Cursor=crDefault then
      Screen.Cursor := crAppStart;
    // Setup object.
    lAsyncData := TAsyncFetchAssistor.Create;
    lAsyncData.CallbackProc := ACallbackProc;
    lAsyncData.Target       := ATarget;
    lAsyncData.Dataset      := GetAsyncDataset;
    // Setup Dataset properties. Can't use SetupCommandProperties, different ancestors!
    with lAsyncData.Dataset do begin
      Connection      := Self.Connection;
      CommandType     := cmdStoredProc;
      CommandTimeOut  := 0;
      CommandText     := AStoredProcName;
      RefreshParameters(AStoredProcName, Parameters);
      // But can use SetupCommandParameters though.
      SetupCommandParameters(Parameters, AParams);
      ExecuteOptions  := [eoAsyncExecute, eoAsyncFetchNonBlocking];
      CacheSize       := 10;
      CursorLocation  := clUseClient;
      CursorType      := ctStatic;
      OnFetchComplete := DatasetFetchComplete;
    end;
    // Add to list
    FAsyncDatasets.Add(lAsyncData);
    // Open dataset.
    lAsyncData.Dataset.Open;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.GetAsyncRecordset', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.GetAsyncRecordset

{-------------------------------------------------------------------------------
  Run an async stored proc that does not return anything
}
procedure TdmBaseADO.RunAsyncStoredProc(const AStoredProcName: String;
  AParams: array of Variant; ATarget: TObject; ACallbackProc: TAsyncRunCompleteProc);
var
  lAsyncData: TAsyncRunAssistor;
begin
  if AStoredProcName <> '' then begin
    // Setup object.
    lAsyncData := TAsyncRunAssistor.Create;
    lAsyncData.CallbackProc := ACallbackProc;
    lAsyncData.Target       := ATarget;
    lAsyncData.Command      := TADOCommand.Create(nil);
    // Setup Dataset properties.
    SetupCommandProperties(lAsyncData.Command, AStoredProcName, AParams);
    with lAsyncData.Command do
      ExecuteOptions  := [eoAsyncExecute, eoExecuteNoRecords];
    // Add to list
    FAsyncCommands.Add(lAsyncData);
    SetScreenCursor;
    // Open dataset.
    lAsyncData.Command.Execute;
  end;
end;

{-------------------------------------------------------------------------------
}
function TdmBaseADO.GetRecordset(const AStoredProc: String; AParams: Array of Variant):
    _Recordset;
var
  lCmd: TADOCommand;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProc, AParams);
    Result := lCmd.Execute;
  finally
    lCmd.Free;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.GetRecordset', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.GetRecordset

{-------------------------------------------------------------------------------
}
function TdmBaseADO.GetStoredProcOutputParam(const AStoredProcName: String; AParams: Array of
    Variant; const AOutputParamName: String): Variant;
var
  lCmd: TADOCommand;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProcName, AParams);
    lCmd.ExecuteOptions := [eoExecuteNoRecords];
    lCmd.Parameters.ParamByName(AOutputParamName).Direction := pdOutput;
    lCmd.Execute;
    Result := lCmd.Parameters.ParamByName(AOutputParamName).Value;
  finally
    lCmd.Free;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.GetStoredProcOutputParam', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.GetStoredProcOutputParam

{-------------------------------------------------------------------------------
  Returns a string representing the XML returned by the stored proc.
}
function TdmBaseADO.GetXMLData(const AXMLRootName, AStoredProc: String; AParams: Array of 
    Variant): String;
var
  lCmd: TADOCommand;
  lRecs: OleVariant;
  outStream: TStringStream;
  adapter: IStream;  // Need to declare TStreamAdapter as IStream. And no Free when done with it!
begin
  outStream := TStringStream.Create('');
  adapter := TStreamAdapter.Create(outStream, soReference);  // IStream
  lCmd := TADOCommand.Create(nil);

  try
    // No need to use SetupCommandProperties as we can't use cached parameters.
    // The Refresh MUST be called to get the Properties collection populated.
    with lCmd do begin
      Connection      := Self.Connection;
      CommandType     := cmdStoredProc;
      CommandTimeOut  := 0;
      CommandText     := AStoredProc;
      // Need a Refresh on Parameters or the Properties collection will be empty!!!!
      Parameters.Refresh;
      // Now populate the parameters properly.
      SetupCommandParameters(Parameters, AParams);

      // Set root node name
      Properties.Item['XML Root'].Value := AXMLRootName;
      // Link stream to command object.
      Properties.Item['Output Stream'].Value := adapter as IStream;

      // adExecuteStream cannot be used with the ADOCommand.Execute, Delphi doesn't
      // support it. But the CommandObject can.
      CommandObject.Execute(lRecs, EmptyParam, adExecuteStream);
    end;

    // Return the XML as a big string
    Result := outStream.DataString;
  finally
    lCmd.Free;
    outStream.Free;
  end;
end;  // TdmBaseADO.GetXMLData

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.OpenConnection(const AConnectionString: String);
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  with Connection do begin
    ConnectionString := AConnectionString;
    CommandTimeout   := 0;
    LoginPrompt      := False;
    OnExecuteComplete:= ConnectionExecuteComplete;
    Open;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.OpenConnection', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.OpenConnection

{-------------------------------------------------------------------------------
  Refreshes the parameters on the command object, using the cache of previously refreshed
      parameters where possible.
}
procedure TdmBaseADO.RefreshParameters(const AProcName: string; AParameters: TParameters);
var
  lIdx: Integer;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lIdx := FParametersCache.IndexOf(AProcName);
  if lIdx=-1 then
    // Refresh parameters and store in cache
    FParametersCache.AddObject(AProcName,
        TCachedParameters.Create(AParameters))
  else
    // Obtain parameters from cache
    TCachedParameters(FParametersCache.Objects[lIdx]).LoadParameterList(AParameters);
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.RefreshParameters', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.RefreshParameters

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.RunStoredProc(const AStoredProcName: String; AParams: Array of Variant);
var
  lCmd: TADOCommand;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  ExecuteSQL('SET ARITHABORT ON');
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProcName, AParams);
    lCmd.ExecuteOptions := [eoExecuteNoRecords];
    lCmd.Execute;
  finally
    lCmd.Free;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.RunStoredProc', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.RunStoredProc

{-------------------------------------------------------------------------------
  Sets the screen cursor to app start, and back again, when async queries are started and
      stopped.
}
procedure TdmBaseADO.SetScreenCursor;
var
  lCount: Integer;
  lList: TList;
begin
  lList := FAsyncCommands.LockList;
  try
    lCount := lList.Count;
    lList := FAsyncDatasets.LockList;
    try
      lCount := lCount + lList.Count;
    finally
      FAsyncDatasets.UnlockList;
    end;
  finally
    FAsyncCommands.UnlockList;
  end;
  
  if lCount > 0 then begin
    // running async, so ensure cursor is at least an AppStart
    if Screen.Cursor=crDefault then
      Screen.Cursor := crAppStart
  end
  else begin
    // no async queries, so reset cursor
    if Screen.Cursor=crAppStart then
      Screen.Cursor := crDefault
  end;
end;  // TdmBaseADO.SetScreenCursor

{-------------------------------------------------------------------------------
  Method to populate the parameters collection of a command object with the values given in
      the AParams array.  Override in descendats for project specific parameters.
}
procedure TdmBaseADO.SetupCommandParameters(AParameters: TParameters; AParams: Array of
    Variant);
var
  i: Integer;
  lValue: Variant;
  lParam: TParameter;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  for i := 0 to (Length(AParams) div 2) - 1 do begin
    // If parameter not found in stored proc list of parameters, skip it.
    lParam := AParameters.FindParam(AParams[i * 2]);
    if Assigned(lParam) then begin
      // If string is empty, send NULL to database. If field is foreign key, empty string
      // would break referential integrity!
      lValue := AParams[(i * 2) + 1];
      if ((VarType(lValue) and varOleStr > 0) or
          (VarType(lValue) and varString > 0)) and
         (VarToStr(lValue) = '') then
        lParam.Value := Null
      else
        lParam.Value := lValue;
    end;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.SetupCommandParameters', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.SetupCommandParameters

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.SetupCommandProperties(ACommand: TADOCommand; AStoredProcName: String; 
    AParams: Array of Variant);
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  with ACommand do begin
    Connection      := Self.Connection;
    CommandType     := cmdStoredProc;
    CommandTimeOut  := 0;
    CommandText     := AStoredProcName;
    RefreshParameters(AStoredProcName, Parameters);
    SetupCommandParameters(Parameters, AParams);
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TdmBaseADO.SetupCommandProperties', E)
  end;
  {$ENDIF}
end;  // TdmBaseADO.SetupCommandProperties

{-------------------------------------------------------------------------------
  Log an entry to a text file in the temp folder.  Provided as a debugging
      service.
}
procedure TdmBaseADO.Log(const AText: string);
begin
  if not FLogAssigned then begin
    AssignFile(FLog, GetWindowsTempDir + ExtractWithoutExt(Application.Exename)+'.txt');
    Rewrite(FLog);
    WriteLn(FLog, 'Log started ' + TimeToStr(now));
    FLogAssigned := True;
  end;
  WriteLn(FLog, AText);
  Flush(FLog);
end;

{-------------------------------------------------------------------------------
  Instead of freeing an ADODataset used for an async query, we pool up to 5 of
      them for re-use.
}
procedure TdmBaseADO.ReleaseAsyncDataset(ADataset: TADODataset);
begin
  if FASyncDatasetPool.Count<5 then begin
    ADataset.Close;
    FASyncDatasetPool.Add(ADataset);
  end else
    ADataset.Free;

  SetScreenCursor;
end;

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.RunDeleteStoredProc(const AStoredProcName: String; AParams: Array of
    Variant);
var
  lCmd: TADOCommand;

    function GetMessageField(const AMessage, Field: String): String;
    var
      lPos: Integer;
    begin
      lPos := Pos(Field + ' ''', AMessage);
      Result := Copy(AMessage, lPos + Length(Field) + 2, Length(AMessage));
      Result := LeftStr(Result, Pos(''',', Result)-1);
    end;

    // Retrieves the name of the related table from an error message
    function GetRelTableName(const AMessage: string): string;
    begin
      Result := GetMessageField(AMessage, 'table');
    end;

    // Name of the column from an error message
    function GetColumnName(const AMessage: String): String;
    begin
      Result := GetMessageField(AMessage, 'column');
    end;

begin
  ExecuteSQL('SET ARITHABORT ON');
  // we may need to parse the error message, so ensure that it will be in the
  // expected English form
  ExecuteSQL('SET LANGUAGE us_english');
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProcName, AParams);
    lCmd.ExecuteOptions := [eoExecuteNoRecords];
    try
      lCmd.Execute;
    except
      on E:EOleException do
        if Connection.Errors.Count > 0 then
          if (Connection.Errors[0].NativeError = FAILED_TIMESTAMP_CHECK)
              or (Connection.Errors[0].Description = RECORD_CHANGED_MESSAGE) then
            raise EUpdatedException.CreateNonCritical(ResStr_DeleteAlreadyUpdated)
          else if Connection.Errors[0].NativeError = REFERENTIAL_INTEGRITY_ERROR then
            raise EReferentialIntegrityException.CreateNonCritical(Format(
                ResStr_RecordCannotBeDeleted, [GetRelTableName(E.Message)]),
                GetRelTableName(E.Message),
                GetColumnName(E.Message))
          else
            raise // Error was caused by something else, raise it.
        else
          raise E;  // Not an error in Connection's Errors collection, better raise it then.
    end;
  finally
    lCmd.Free;
  end;
end;  // TdmGeneral.RunDeleteStoredProc 

{-------------------------------------------------------------------------------
}
function TdmBaseADO.RunInsertStoredProc(const ATableName, AStoredProcName: String; AParams:
    Array of Variant; const AOutputParamName: String): Variant;
var
  lCmd: TADOCommand;
begin
  ExecuteSQL('SET ARITHABORT ON');
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProcName, AParams);
    lCmd.ExecuteOptions := [eoExecuteNoRecords];
    lCmd.Parameters.ParamByName(AOutputParamName).Direction := pdOutput;
    try
      lCmd.Execute;
      Result := lCmd.Parameters.ParamByName(AOutputParamName).Value;
    except
      on E:EOleException do
        if Connection.Errors.Count > 0 then begin
          if Connection.Errors[0].NativeError = DUPLICATE_PRIMARY_KEY_ON_INSERT then begin
            // Get the database to fix itself
            RunStoredProc('spRepairLastKey', ['@TableName', ATableName]);
            // And try again.
            lCmd.Execute;
            Result := lCmd.Parameters.ParamByName(AOutputParamName).Value;
          end
          else if Connection.Errors[0].NativeError = FOREIGN_KEY_CONSTRAINT_FAIL then
            raise EForeignKeyException.CreateNonCritical(ResStr_ForeignKeyConstraintFail)
          else
            raise // Error was caused by something else, raise it.
        end
        else
          raise E;  // Not an error in Connection's Errors collection, better raise it then.
    end;
  finally
    lCmd.Free;
  end;
end;  // TdmGeneral.RunInsertStoredProc

{-------------------------------------------------------------------------------
}
procedure TdmBaseADO.RunUpdateStoredProc(const AStoredProcName: String; AParams: Array of
    Variant);
var
  lCmd: TADOCommand;
  lRecordsAffected: Integer;
  lParam: TParameter;
begin
  ExecuteSQL('SET ARITHABORT ON');
  lCmd := TADOCommand.Create(nil);
  try
    SetupCommandProperties(lCmd, AStoredProcName, AParams);
    lCmd.ExecuteOptions := [eoExecuteNoRecords];
    try
      lCmd.Execute(lRecordsAffected, EmptyParam);
      lParam := lCmd.Parameters.FindParam(PARAM_RECORDS_AFFECTED);
      if Assigned(lParam) then
        lRecordsAffected := lParam.Value;
      if lRecordsAffected = 0 then
        raise EDeletedException.CreateNonCritical(ResStr_UpdateAlreadyDeleted);
    except
      on E:EOleException do
        if Connection.Errors.Count > 0 then
          if (Connection.Errors[0].NativeError = FAILED_TIMESTAMP_CHECK)
          or (Connection.Errors[0].Description = RECORD_CHANGED_MESSAGE) then
            raise EUpdatedException.CreateNonCritical(ResStr_UpdateAlreadyUpdated)
          else
            raise // Error was caused by something else, raise it.
        else
          raise E;  // Not an error in Connection's Errors collection, better raise it then.
    end;
  finally
    lCmd.Free;
  end;
end;  // TdmGeneral.RunUpdateStoredProc

{-==============================================================================
    TAsyncFetchAssistor
===============================================================================}
{-------------------------------------------------------------------------------
}
destructor TAsyncFetchAssistor.Destroy;
begin
  if Assigned(Dataset) then begin
    if Assigned(Dataset.Recordset) and (Dataset.State = dsOpening) then begin
      Dataset.OnFetchComplete := nil;
      Dataset.Recordset.Cancel;
    end;
  end;
  inherited;
end;  // TAsyncFetchAssistor.Destroy

{-------------------------------------------------------------------------------
}
procedure TAsyncFetchAssistor.SetCallbackProc(const Value: TAsyncFetchCompleteProc);
begin
  FCallbackProc := Value;
end;  // TAsyncFetchAssistor.SetCallbackProc

{-==============================================================================
    TCachedParameters
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise object.  Loads parameter data from the supplied object.
}
constructor TCachedParameters.Create(AParameters: TParameters);
var
  lIdx: Integer;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  inherited Create;
  AParameters.Refresh;
  for lIdx := 0 to AParameters.Count-1 do
    AddParam(AParameters[lIdx]);
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TCachedParameters.Create', E)
  end;
  {$ENDIF}
end;  // TCachedParameters.Create

{-------------------------------------------------------------------------------
  Add a single parameter to the cached list.
}
procedure TCachedParameters.AddParam(AParam: TParameter);
var
  lCachedParameter: TCachedParameter;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  lCachedParameter := TCachedParameter.Create;
  with lCachedParameter do begin
    Attributes   := AParam.Attributes;
    DataType     := AParam.DataType;
    Direction    := AParam.Direction;
    Name         := AParam.Name;
    NumericScale := AParam.NumericScale;
    Precision    := AParam.Precision;
    Size         := AParam.Size;
  end;
  Add(lCachedParameter);
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TCachedParameters.AddParam', E)
  end;
  {$ENDIF}
end;  // TCachedParameters.AddParam

{-------------------------------------------------------------------------------
  Dumps the cached list of parameters into the supplied parameter object.
}
procedure TCachedParameters.LoadParameterList(AParameters: TParameters);
var
  lIdx: Integer;
  lCachedParameter: TCachedParameter;
begin
  {$IFDEF DEBUG}
  try
  {$ENDIF}
  AParameters.Clear;
  for lIdx := 0 to Count-1 do begin
    lCachedParameter := TCachedParameter(Items[lIdx]);
    with AParameters.AddParameter do begin
      Attributes   := lCachedParameter.Attributes;
      DataType     := lCachedParameter.DataType;
      Direction    := lCachedParameter.Direction;
      Name         := lCachedParameter.Name;
      NumericScale := lCachedParameter.NumericScale;
      Precision    := lCachedParameter.Precision;
      Size         := lCachedParameter.Size;
    end;
  end;
  {$IFDEF DEBUG}
  except
    on E:Exception do
      raise EBaseADODataModuleException.Create(ResStr_ExceptionInMethod +
        'TCachedParameters.LoadParameterList', E)
  end;
  {$ENDIF}
end;  // TCachedParameters.LoadParameterList

{-==============================================================================
 TAsyncRunAssistor
===============================================================================}
{-------------------------------------------------------------------------------
  Accessor method
}
procedure TAsyncRunAssistor.SetCallbackProc(const Value: TAsyncRunCompleteProc);
begin
  FCallbackProc := Value;
end;  // TAsyncRunAssistor.SetCallbackProc

{-==============================================================================
 TAsyncAssistorNoRecords
===============================================================================}
{-------------------------------------------------------------------------------
  Cleanup dataset
}
destructor TAsyncAssistorNoRecords.Destroy;
begin
  if Assigned(Command) then begin
    if Command.States <> [stClosed] then Command.Cancel;
    FreeAndNil(FCommand);
  end;

  inherited Destroy;
end;  // TAsyncAssistorNoRecords.Destroy

{-==============================================================================
    EReferentialIntegrityException
===============================================================================}
{-------------------------------------------------------------------------------
  Initialise a new instance of EReferentialIntegrityException.
}
constructor EReferentialIntegrityException.CreateNonCritical(
  const Message, TableName, Column: String);
begin
  inherited CreateNonCritical(Message);
  FTableName := TableName;
  FColumn := Column;
end;

end.
