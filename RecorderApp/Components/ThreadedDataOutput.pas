//==============================================================================
//  Unit:        ThreadedDataOutput
//
//  Description: Warapper class for TDataOutput to allow it be used
//               from a separate thread.
//
//  Created using the Delphi ThreadedObject template
//
//  Author:      John Durman
//  Created:     19 May 2008
//
//  Last Revision Details:
//    $Revision: 1 $
//    $Date: 21/05/08 11:23 $
//    $Author: Johndurman $
//
//  Copyright © Dorset Software Services Ltd, 2008
//
//==============================================================================

unit ThreadedDataOutput;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF}, DataClasses, DataOutput,
  SysUtils;

type
  TThreadedDataOutput = class(TThread)
  private
    FFileName: string;              //--|
    FKeyList: TEditableKeyList;     //--|
    FInvalidKeys: TEditableKeyList; //  |-- Fields used to pass values through
    FWantObservations: boolean;     //  |     to the export routine.
    FFromExportFilter: boolean;     //--|
    FDataOutput: TDataOutput;       // Performs the export
    FCancelled: boolean;
    FException: Exception;
    procedure SetName;
    procedure SetCancelled(const Value: boolean);
    procedure HandleException;
    procedure DoHandleException;
  public
    constructor Create(const AFileName: string; AKeyList: TEditableKeyList;
      AInvalidKeys: TKeyList; ATerminated: TNotifyEvent;
      const AWantObservations, AFromExportFilter: boolean);
    destructor Destroy; override;
    procedure Execute; override;
    property Cancelled : boolean read FCancelled write SetCancelled;
  end;

implementation

uses
  Forms, Messages, ExceptionForm, Dialogs;
  
{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TThreadedDataOutput.UpdateCaption;
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

{ TThreadedDataOutput }

{-------------------------------------------------------------------------------
  Create a new instance of this class
-------------------------------------------------------------------------------}
constructor TThreadedDataOutput.Create(const AFileName: string;
  AKeyList: TEditableKeyList; AInvalidKeys: TKeyList; ATerminated: TNotifyEvent;
  const AWantObservations, AFromExportFilter: boolean);
begin
  inherited Create(False);
  FFileName := AFileName;
  // Use copies of the key lists to avoid memory overwrite errors
  FKeyList := TEditableKeyList.Create;
  FKeyList.Assign(AKeyList);
  FInvalidKeys := TEditableKeyList.Create;
  FInvalidKeys.Assign(AInvalidKeys);
  FWantObservations := AWantObservations;
  FFromExportFilter := AFromExportFilter;
  FreeOnTerminate := True;
  OnTerminate := ATerminated;
end;    // TThreadedDataOutput.Create

{-------------------------------------------------------------------------------
  Clean-up objects owned by this class.
-------------------------------------------------------------------------------}
destructor TThreadedDataOutput.Destroy;
begin
  FKeyList.Free;
  FInvalidKeys.Free;
  inherited;
end;    // TThreadedDataOutput.Destroy

{-------------------------------------------------------------------------------
  Handle (on the main thread) any exception that's raised by this thread
-------------------------------------------------------------------------------}
procedure TThreadedDataOutput.DoHandleException;
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
end;    // TThreadedDataOutput.DoHandleException

{-------------------------------------------------------------------------------
  Start the thread.
-------------------------------------------------------------------------------}
procedure TThreadedDataOutput.Execute;
begin
  SetName;
  try
    FDataOutput := TDataOutput.Create(FFileName);
    try
      FDataOutput.Execute(FKeyList, FInvalidKeys, FWantObservations, FFromExportFilter);
    finally
      FDataOutput.Free;
      FDataOutput := nil;
    end;
  except
    HandleException;
  end;
end;    // TThreadedDataOutput.Execute

{-------------------------------------------------------------------------------
  Ensure that exceptions are dealt with correctly
-------------------------------------------------------------------------------}
procedure TThreadedDataOutput.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    // Don't show EAbort messages
    if not (FException is EAbort) then
      Synchronize(DoHandleException);
  finally
    FException := nil;
  end;
end;    // TThreadedDataOutput.HandleException

{-------------------------------------------------------------------------------
  Property accessor
-------------------------------------------------------------------------------}
procedure TThreadedDataOutput.SetCancelled(const Value: boolean);
begin
  if Assigned(FDataOutput) then FDataOutput.Cancelled := Value;
end;    // TThreadedDataOutput.SetCancelled

{-------------------------------------------------------------------------------
  Method created by Delphi to name the thread
    (including the call to RaiseException!)
-------------------------------------------------------------------------------}
procedure TThreadedDataOutput.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'DataOutput';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;    // TThreadedDataOutput.SetName

end.
