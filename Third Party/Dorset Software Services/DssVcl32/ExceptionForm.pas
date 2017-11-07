{*******************************************************************************
 Exception handler, with TExceptionPath class.
    Set the application exception handler to the one in this unit
    (MadExceptionHandler).
    Inherit all your exception classes from TExeptionPath.
    Saves critical exception info to a file, Error-nnnnnn0.txt in the public documents
    directory.  If you catch an exception, pass the information into the new
    exception constructor as the second parameter - this way entire paths
    of exceptions can be traced.
    Use CreateNonCritical to demote the exception message to non-critical.
    This tones down the exception handler form.

    NB/ This class no longer keeps a path to the exceptions since that is handled
    by MadExcept.
}


unit ExceptionForm;

{$B-} // ensure partial evaluation

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, VersionInfo, MadExcept;


type

  //----------------------------------------------------------------------------
  { Exception class }
  TExceptionPath = class(Exception)
    FCritical : boolean;
  public
    constructor Create(const Msg : string); overload;
    constructor Create(const Msg : string; iException : Exception); overload;
    constructor CreateNonCritical(const Msg : string); overload;
    constructor CreateNonCritical(const Msg : string; iException : Exception); overload;
    constructor CreateValidation(const Msg : string;
                                        iFocusControl : TWinControl); overload;
    constructor CreateValidation(const Msg : string; iException : Exception;
                                        iFocusControl : TWinControl); overload;
    property Critical : boolean read FCritical;
  end;  // TExceptionPath



resourcestring
  ResStr_CriticalError = 'A critical error has occurred in the %s application.  ' +
                         'Click the More Details button if you require further information.';

  ResStr_ExceptionOccuredInApp = 'Exception occurred in application %s at %s.';
  ResStr_MadExceptErrorMessage = 'An unhandled error has occurred in the application. '+
      'This error message has been copied to the clipboard and also will be saved to the '+
      'following file location after this dialog is closed:'+
      #13#10'''%s''.';
  ResStr_MadExceptErrorMessageXp = 'If you browse the folders in Windows Explorer, then because you '+
      'are running Windows XP you will see this folder displayed as:'+
      #13#10'''%s''.';
  ResStr_Documents = 'Documents';
  ResStr_SharedDocuments = 'Shared Documents';

var
  NonCriticalExceptions : TStringList;
  // configurable variable for the error message
  MstErrorMessage : string = ResStr_CriticalError;
  madExceptErrorPath: string;

// global validation procedures
procedure ValidateValue(const AValue: boolean; const AMsg: string); overload;
procedure ValidateValue(const AValue: boolean; const AMsg: string;
  const AControl: TWinControl); overload;

{$IFDEF madExcept}
procedure MadExceptionHandler(const exceptIntf : IMEException;
                              var handled      : boolean);
{$ENDIF}

//==============================================================================
implementation

uses GeneralFunctions, ComCtrls, DateUtils;


const
  { Set size of recent event log according to debug mode }
  {$IFDEF DEBUG}
  EVENT_LOG_SIZE = 30;
  {$ELSE}
  EVENT_LOG_SIZE = 8; // change this to increase event log size for error analysis
  {$ENDIF}


resourcestring
  { This error will never happen ! }
  ResStr_BadCall = 'Trying to construct the error handler form using the wrong' +
                 ' overloaded constructor.';
  ResStr_InfoSavedToFile =  'Information has been saved to the file %s';
  ResStr_Version = 'Version : %s';
  ResStr_ExceptionPath =  'Exception path:';
  ResStr_LastEventAction =  'Last event\actions: ';
  ResStr_OSInfo = 'Operating System : %s';
  ResStr_PhysicalMemInfo =  'Physical Memory available : %s';
  ResStr_DLLsLoaded = 'DLLs loaded:';
  ResStr_FileSaveError =  '<File not saved due to error>';


//==============================================================================
//==============================================================================
procedure ValidateValue(const AValue: boolean; const AMsg: string);
begin
  if not AValue then
    raise TExceptionPath.CreateNonCritical(AMsg);
end;  // ValidateValue


//==============================================================================
procedure ValidateValue(const AValue: boolean; const AMsg: string;
  const AControl: TWinControl);

    procedure MakeControlVisible;
    var
      lParent: TWinControl;
      lTabSheet: TTabSheet;
    begin
      if not AControl.CanFocus then begin
        // Control cannot receive focus.  If this is because it is on a hidden
        // tab sheet, bring it to the front.
        lParent := AControl.Parent;
        lTabSheet := nil;
        // Look up the containers, setting ALL page controls to the right tab
        while Assigned(lParent) do begin
          if (lParent is TPageControl) then begin
            if Assigned(lTabSheet) then
              (lParent as TPageControl).ActivePage := lTabSheet;
          end else
          begin
            if lParent is TTabSheet then
              lTabSheet := lParent as TTabSheet;
          end;
          lParent := lParent.Parent;
        end; // while
      end;
    end;

begin
  if not AValue then begin
    MakeControlVisible;
    if AControl.Visible then // safety in case hidden by COM addin
      AControl.SetFocus;
    raise TExceptionPath.CreateValidation(AMsg, AControl);
  end;
end;  // ValidateValue

//==============================================================================

//==============================================================================
{ TExceptionPath }
//==============================================================================

{ Create a critical exception at the root level }
constructor TExceptionPath.Create(const Msg: string);
begin
  FCritical := True;
  inherited Create(Msg);
end;  // TExceptionPath.Create (Message only)

//------------------------------------------------------------------------------
{ Create a critical secondary exception, passing in a pointer to the previous
     exception.  Use the previous exception path only if it is relevant
  DEPRECATED }
constructor TExceptionPath.Create(const Msg: string; iException: Exception);
begin
  FCritical := True;
  inherited Create(Msg);
end;  // TExceptionPath.Create (Message and exception)

//------------------------------------------------------------------------------
{ Create a root exception which is non-critical (ie just a nice message if it
     gets to the top level }
constructor TExceptionPath.CreateNonCritical(const Msg: string);
begin
  FCritical := False;
  inherited Create(Msg);
end;  // TExceptionPath.CreateNonCritical (Message only)

//------------------------------------------------------------------------------
{ Create a non-critical secondary exception, passing in a pointer to the previous
     exception.  Use the previous exception path only if it is relevant
  DEPRECATED }
constructor TExceptionPath.CreateNonCritical(const Msg: string; iException: Exception);
begin
  FCritical := False;
  inherited Create(Msg);
end;  // TExceptionPath.CreateNonCritical (Message and exception)

//------------------------------------------------------------------------------
{ Versions of CreateNonCritical which automatically set a control
    to focus - to prompt the user to enter correct data }
constructor TExceptionPath.CreateValidation(const Msg: string; iFocusControl: TWinControl);
begin
  CreateNonCritical(Msg);
  if iFocusControl <> nil then
    if iFocusControl.Visible and iFocusControl.Enabled then
      iFocusControl.SetFocus;
end;  // TExceptionPath.CreateValidation (Message and contol)

//------------------------------------------------------------------------------
{ And again }
constructor TExceptionPath.CreateValidation(const Msg: string;
  iException: Exception; iFocusControl: TWinControl);
begin
  CreateNonCritical(Msg, iException);
  if iFocusControl <> nil then
    if iFocusControl.Visible and iFocusControl.Enabled then
      iFocusControl.SetFocus;
end;  // TExceptionPath.CreateValidation (Message, exception and control)

{$IFDEF madExcept}

(**
 * Utility function to remove files older than a set date.
 *)
function PurgeFiles(const AFileMask : string; AOlderThanDays : word;
                    AFailOnError : boolean = false) : integer;
var rDirInfo : TSearchRec;
    iResult,iError : integer;
    dtFileDate,dtNow : TDateTime;
    sFilePath,sErrMess : string;
begin
  iResult := 0;
  dtNow := Date;
  sFilePath := ExtractFilePath(AFileMask);
  iError := FindFirst(AFileMask,faAnyFile,rDirInfo);

  // Itterate thru files found with mask
  while iError = 0 do begin
    // Eclude Directories
    if (rDirInfo.Name <> '.') and (rDirInfo.Name <> '..') and
       (rDirInfo.Attr and faDirectory <> faDirectory) then begin
      dtFileDate := FileDateToDateTime(rDirInfo.Time);

      // Does the file meet deletion days criteria ?
      if trunc(dtNow - dtFileDate) + 1 > AOlderThanDays then begin
        // Delete the file - raise exception if fail and AFailOnError set
        if not DeleteFile(sFilePath + rDirInfo.Name) and
           AFailOnError then begin
          sErrMess := 'PurgFiles() Failed on file' + #13#10 +
                      sFilePath + rDirInfo.Name + #13#10#13#10 +
                      SysErrorMessage(GetLastError);
          raise Exception.Create(sErrMess);
        end;

        inc(iResult);
      end;
    end;

    iError := FindNext(rDirInfo);
    if iError <> 0 then FindClose(rDirInfo);  // Release FindFirt Allocs
  end;

  Result := iResult;
end;

{-------------------------------------------------------------------------------
  Global exception handled for when Mad exceptions are installed
}
procedure MadExceptionHandler(const exceptIntf : IMEException;
                              var handled      : boolean);
var
  ts: TTimeStamp;
  msg: string;
begin
  if (exceptIntf.exceptObject is TExceptionPath) and (not TExceptionPath(exceptIntf.exceptObject).Critical) then begin
    MessageDlg(Exception(exceptIntf.exceptObject).message, mtInformation, [mbOk], 0);
    Handled := True;
  end else begin
    if (madExceptErrorPath<>'') then begin
      ts := DateTimeToTimeStamp(now);
      // Create a unique bug report file
      exceptIntf.BugReportFile := madExceptErrorPath + 'Error-' + IntToStr(ts.Date) + IntToStr(ts.Time)+'.txt';
      // purge error files older than one week
      PurgeFiles(madExceptErrorPath+'*.txt', 7);
    end;
    msg:= Format(ResStr_MadExceptErrorMessage, [exceptIntf.BugReportFile]);
    if (madExceptErrorPath<>'') and (GetOSName='Windows XP') then
      msg := msg + #13#10#13#10 + Format(ResStr_MadExceptErrorMessageXp, [
        StringReplace(exceptIntf.BugReportFile, '\' + ResStr_Documents + '\', '\' + ResStr_SharedDocuments + '\', [])
      ]);
    exceptIntf.ExceptMsg  := msg;
  end;
end; // MadExceptionHandler
{$ENDIF}


initialization
  NonCriticalExceptions := TStringList.Create;
  madExceptErrorPath := '';

finalization
  NonCriticalExceptions.Free;

end.
