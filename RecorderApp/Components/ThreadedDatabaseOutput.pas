//==============================================================================
//  Unit:        ThreadedDatabaseOutput
//
//  Description: Warapper class for TDatabaseOutput to allow it be used
//               from a separate thread.
//
//  Created using the Delphi ThreadedObject template
//
//  Author:      John Durman
//  Created:     19 May 2008
//
//  Last Revision Details:
//    $Revision: 3 $
//    $Date: 19/06/08 16:03 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2008
//
//==============================================================================

unit ThreadedDatabaseOutput;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF}, DataClasses, DatabaseOutput,
  DatabaseUtilities, TaskProgress, DatabaseAccessADO, SysUtils;

type
  TThreadedDatabaseOutput = class(TThread)
  private
    FDatabaseModule: TdmDatabase;         //--|
    FADBName: string;                     //  |-- Fields used to pass values through
    FKeyList: TEditableKeyList;           //  |     to the export routine.
    FInvalidKeys: TEditableKeyList;       //  |
    FWantObservations: boolean;           //  |
    FFromExportFilter: boolean;           //--|
    FChangeCustodian :boolean;            //--|-- Mantis 343
    FDatabaseOutput: TDatabaseOutput;     // Performs the export
    FCancelled: boolean;
    FSetStatus: TSetStatusEvent;
    FStatusString: string;
    FProgressBar: TTaskProgressBar;
    FProgress: integer;
    FException: Exception;
    procedure SetName;
    procedure SetCancelled(const Value: boolean);
    procedure SetStatusEvent(const iStatusString: String; iPrcMsgs: Boolean = true);
    procedure SetStatus;
    procedure SetProgressEvent(const iProgress: Integer; iPrcMsgs: Boolean = true);
    procedure SetProgress;
    procedure HandleException;
    procedure DoHandleException;
  public
    constructor Create(ADatabaseModule: TdmDatabase; const ADBName: String; const
        ASetStatusEvent: TSetStatusEvent; const AProgressBar: TTaskProgressBar;
        AKeyList, AInvalidKeys: TKeyList; ATerminated: TNotifyEvent; const
        AWantObservations: boolean; AFromExportFilter: boolean; AChangeCustodian :boolean);
    destructor Destroy; override;
    procedure Execute; override;
    property Cancelled : boolean read FCancelled write SetCancelled;
  end;

implementation

uses Forms, Messages, Dialogs, ExceptionForm, ApplicationSettings;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TThreadedDatabaseOutput.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TThreadedDatabaseOutput }

{-------------------------------------------------------------------------------
  Create a new instance of this class
-------------------------------------------------------------------------------}
constructor TThreadedDatabaseOutput.Create(ADatabaseModule: TdmDatabase; const
    ADBName: String; const ASetStatusEvent: TSetStatusEvent; const
    AProgressBar: TTaskProgressBar; AKeyList, AInvalidKeys: TKeyList;
    ATerminated: TNotifyEvent; const AWantObservations: boolean;
    AFromExportFilter: boolean; AChangeCustodian: boolean);
begin
  inherited Create(False);
  FDatabaseModule := ADatabaseModule;
  FADBName := ADBName;
  FProgressBar := AProgressBar;
  FSetStatus := ASetStatusEvent;
  // Use copies of the key lists to avoid memory overwrite errors
  FKeyList := TEditableKeyList.Create;
  FKeyList.Assign(AKeyList);
  FInvalidKeys := TEditableKeyList.Create;
  FInvalidKeys.Assign(AInvalidKeys);
  FWantObservations := AWantObservations;
  FFromExportFilter := AFromExportFilter;
  FChangeCustodian := AChangeCustodian;
  FreeOnTerminate := True;
  OnTerminate := ATerminated;
end;    // TThreadedDatabaseOutput.Create

{-------------------------------------------------------------------------------
  Clean-up objects owned by this class.
-------------------------------------------------------------------------------}
destructor TThreadedDatabaseOutput.Destroy;
begin
  FKeyList.Free;
  FInvalidKeys.Free;
  inherited;
end;    // TThreadedDatabaseOutput.Destroy

{-------------------------------------------------------------------------------
  Handle (on the main thread) any exception that's raised by this thread
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.DoHandleException;
begin
  // Cancel the mouse capture
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  // Now actually show the exception
  if (FException is TExceptionPath) and (not TExceptionPath(FException).Critical) then
    MessageDlg(FException.Message, mtInformation, [mbOK], 0)
  else if FException is Exception then
    Application.ShowException(FException)
  else
    SysUtils.ShowException(FException, nil);
  Terminate;
end;    // TThreadedDatabaseOutput.DoHandleException

{-------------------------------------------------------------------------------
  Start the thread.
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.Execute;
begin
  SetName;
  try
    FDatabaseOutput := TDatabaseOutput.Create(
      FDatabaseModule, FADBName, SetStatusEvent, SetProgressEvent);
    try
      FDatabaseOutput.UserAccessLevel := Integer(AppSettings.UserAccessLevel);
      FDatabaseOutput.ExportConfidentialOccurrences := AppSettings.ExportConfidentialOccurrences;
      FDatabaseOutput.CanExportSystemSupplied := AppSettings.CanExportSystemSupplied;
      FDatabaseOutput.UsingExportFilter := FFromExportFilter;
      FDatabaseOutput.Execute(FKeyList, FInvalidKeys, FWantObservations,FChangeCustodian);
    finally
      FDatabaseOutput.Free;
      FDatabaseOutput := nil;
    end;
  except
    HandleException;
  end;
end;    // TThreadedDatabaseOutput.Execute

{-------------------------------------------------------------------------------
  Ensure that exceptions are dealt with correctly
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;    // TThreadedDatabaseOutput.HandleException

{-------------------------------------------------------------------------------
  Property accessor
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetCancelled(const Value: boolean);
begin
  if Assigned(FDatabaseOutput) then FDatabaseOutput.Cancelled := Value;
end;    // TThreadedDatabaseOutput.SetCancelled

{-------------------------------------------------------------------------------
  Method created by Delphi to name the thread
    (including the call to RaiseException!)
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'DatabaseOutput';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;    // TThreadedDatabaseOutput.SetName

{-------------------------------------------------------------------------------
  Set the value of the specified progress bar
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetProgress;
begin
  FProgressBar.TaskPosition := FProgress;
end;    // TThreadedDatabaseOutput.SetProgress

{-------------------------------------------------------------------------------
  Set the value of the specified progress bar in a synchonised manner
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetProgressEvent(
  const iProgress: Integer; iPrcMsgs: Boolean);
begin
  FProgress := iProgress;
  Synchronize(SetProgress);
end;    // TThreadedDatabaseOutput.SetProgressEvent

{-------------------------------------------------------------------------------
  Set the status string, using the main thread
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetStatus;
begin
  if Assigned(FSetStatus) then FSetStatus(FStatusString);
end;    // TThreadedDatabaseOutput.SetStatus

{-------------------------------------------------------------------------------
  Call the specified SetStatusEvent method in a synchronised manner
-------------------------------------------------------------------------------}
procedure TThreadedDatabaseOutput.SetStatusEvent(
  const iStatusString: String; iPrcMsgs: Boolean);
begin
  FStatusString := iStatusString;
  Synchronize(SetStatus);
end;    // TThreadedDatabaseOutput.SetStatusEvent

end.
