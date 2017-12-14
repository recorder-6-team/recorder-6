//==============================================================================
//  Unit:        Mapifunctions
//
//  Implements:  functions
//
//  Description: A number of functions to simpleify interfacing with MAPI.DLL
//
//  Author:      John van Breda
//  Created:     21 Feb 2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 5 $
//    $Date: 4/01/08 11:09 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MapiDLLFunctions;

interface

uses
  SysUtils, Classes, Mapi, ExceptionForm, Registry;

const
  MAX_RECIPIENTS = 6;

type
  EMapiDLLError = class(TExceptionPath);

  TMapiFileDescArray = array[0..5] of TMapiFileDesc;
  PMapiFileDescArray = ^TMapiFileDescArray;

  TMapiRecipArray = array[0..MAX_RECIPIENTS-1] of TMapiRecipDesc;
  PMapiRecipArray = ^TMapiRecipArray;


  TSendOption = (soLogon, soDialog);  // soLogon - display logon to mapi prompt
                                      // soDialog - display the mail before sending
  TSendOptions = set of TSendOption;

  procedure MapiDllSendMail( const iSubject, iBodyText : string;
                          const iRecipients : array of string;
                          const iAttachments : array of string;
                          iOptions : TSendOptions = [soLogon];
                          AAppHandle: Cardinal=0 );
  procedure MapiDllSignOn;
  procedure MapiDllSignOff;
  function MapiDllCheckNameResolves( const iName : string ) : boolean;

  // internal functions
  procedure MapiCheck( iErrorNum : integer );
  function GetDefaultLogon(out oProfile : string) : boolean;
  procedure InitialiseRecipients( out oRecips : PMapiRecipArray;
                                const iRecipients : array of string );




implementation


var
  mSession : longint;

const
   MAPI_ERROR: array[0..26] of string = (
    'Success',
    'Aborted by user',
    'Mapi error',
    'Logon failure or Logon aborted',
    'Disk full',
    'Insufficient Memory',
    'Access denied',
    'Undocumented',
    'To many sessions',
    'To many files',
    'To many recipients',
    'Attachment not found',
    'Attachement open error',
    'Attachement write error',
    'Unknown recipient',
    'Illegal recipient type',
    'No Message error',
    'Invalid Message',
    'Text to large',
    'Invalid session',
    'type not supported',
    'Ambiguos recipient',
    'Message in use',
    'Mapi Network error',
    'Invalid edit fields',
    'Invalid recipient',
    'not supported'
    );

resourcestring
  ResStr_MapiError =  'MAPI Error %d: %s';
  ResStr_TooManyRecipient = 'Too many recipients specified';


//==============================================================================
{ Send a mail.  Recipients are provided through a string array, as are attachment
     paths - leave the array empty for no attachments.
     If the session is already signed on, then use it.  Otherwise creates a
     MAPI session specifically to send this mail. }
procedure MapiDllSendMail( const iSubject, iBodyText : string;
                        const iRecipients : array of string;
                        const iAttachments : array of string;
                          iOptions : TSendOptions = [soLogon];
                          AAppHandle: Cardinal=0 );
var
  lMapiMessage: TMapiMessage;
  pMapiFiles  : PMapiFileDescArray;
  pRecips     : PMapiRecipArray;
  lError      : Cardinal;
  lFlags      : Cardinal;
  i           : integer;
  lInitiallySignedOn : boolean;

begin
  if mSession = 0 then begin
    MapiDLLSignOn;
    lInitiallySignedOn := False;
  end else
    lInitiallySignedOn := True;

  InitialiseRecipients(pRecips, iRecipients);

  pMapiFiles  := PMapiFileDescArray(AllocMem((High(iAttachments)+1) * SizeOf(TMapiFileDesc)));
  for i := 0 to High(iAttachments) do begin
    // Filename w/ drive and path information
    pMapiFiles^[i].lpszPathName := pchar(iAttachments[i]);
    pMapiFiles^[i].nPosition    := $FFFFFFFF;
  end;

  with lMapiMessage do begin
    ulReserved := 0;
    lpszSubject := PChar(iSubject);
    lpszNoteText := PChar(iBodyText);
    lpszMessageType := nil;
    lpszDateReceived := nil;
    lpszConversationID := nil;
    flFlags := 0;
    lpOriginator := nil;
    nRecipCount := High(iRecipients)+1;
    lpRecips := @pRecips^;
    nFileCount := High(iAttachments)+1;
    lpFiles := @pMapiFiles^;
  end;
  if soLogon in iOptions then
    lFlags := MAPI_LOGON_UI
  else
    lFlags := 0;
  if soDialog in iOptions then
    lFlags := lFlags + MAPI_DIALOG;
  lError := MapiSendMail( mSession, AAppHandle, lMapiMessage, lFlags, 0);
  if lError <> SUCCESS_SUCCESS then
    raise EMapiDLLError.CreateNonCritical(Format(ResStr_MapiError, [lError, Mapi_Error[lError]]));
  if not lInitiallySignedOn then
    MapiDLLSignOff;
end;


//==============================================================================
function GetDefaultLogon(out oProfile : string) : boolean;
const
  KEYNAME = 'Software\Microsoft\Windows Messaging Subsystem\Profiles';
  VALUESTR = 'DefaultProfile';
var
  lReg : TRegistry;
begin
  Result := true;
  lReg := TRegistry.create;
  with lReg do
  try
     if OpenKey(KEYNAME, false) then
     begin
        try
           oProfile := ReadString(VALUESTR);
        except
           Result := false;
        end; // try
        CloseKey;
     end
     else
        Result := false;
  finally
     lReg.free;
  end;
end;


//==============================================================================
procedure MapiDllSignOn;
var
  lstProfile : string;
  lError : cardinal;
begin
  GetDefaultLogon(lstProfile);
  // try to logon with default profile
  lError := MapiLogOn(0, PChar(lstProfile), nil, 0, 0, @mSession);
  if lError <> 0 then
    // try to logon, allowing user to select logon
    MapiCheck(MapiLogOn(0, PChar(lstProfile), nil, MAPI_LOGON_UI, 0, @mSession));
end;


//==============================================================================
procedure MapiDllSignOff;
begin
  MapiCheck( MapiLogOff(mSession, 0, 0, 0) );
  mSession := 0;
end;


//==============================================================================
procedure MapiCheck( iErrorNum : integer );
begin
  if (iErrorNum <> SUCCESS_SUCCESS) then
    raise EMapiDLLError.Create(Format(ResStr_MapiError, [iErrorNum, Mapi_Error[iErrorNum]]));
end;

//==============================================================================
{ Initialise an array of recipient strings (addresses or display names are OK)
    into PMapiRecipArray, suitable for MAPI dll function calls.
    Will generate exceptions if Mapi errors occur. Max 6 recipients supported }
procedure InitialiseRecipients(out oRecips : PMapiRecipArray; const iRecipients : array of string );
var
  i : integer;
  lRecipDesc : pMapiRecipDesc;
begin
  if High(iRecipients) >= MAX_RECIPIENTS then
    raise EMapiDLLError.Create(ResStr_TooManyRecipient);
  oRecips  := PMapiRecipArray(AllocMem((High(iRecipients)+1) * SizeOf(TMapiRecipDesc)));
  for i := 0 to High(iRecipients) do begin
    MapiCheck( MapiResolveName(mSession, 0, PChar(pChar(iRecipients[i])),0, 0,
               lRecipDesc) );
    // Sets info about message recipient
    with oRecips^[i] do begin
      ulReserved := 0;
      ulRecipClass := MAPI_TO;
      lpszAddress := lRecipDesc.lpszAddress;
      lpszName := lRecipDesc.lpszName;
      ulEIDSize := 0;
      lpEntryID := lRecipDesc.lpEntryID;
    end;
  end;
end;


//==============================================================================
{ Function to check that  an email address resolves OK.  If not, then result
   is false.  Else, if there are other MAPI errors then an exception is raised }
function MapiDllCheckNameResolves( const iName : string ) : boolean;
var
  lRecipDesc : pMapiRecipDesc;
  lError : Cardinal;
begin
  lError := MapiResolveName(mSession, 0, PChar(iName),0, 0, lRecipDesc);
  Result := (lError = SUCCESS_SUCCESS);
  if (lError <> SUCCESS_SUCCESS) and (lError <> MAPI_E_UNKNOWN_RECIPIENT) then
    MapiCheck(lError);
end;

initialization
  mSession := 0; //  no session signed on



end.
