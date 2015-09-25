{===============================================================================
	Unit:        Functions

	Defines:

	Description:

	Model:

	Created:     March 2004

	Last revision information:
		$Revision: 6 $
		$Date: 23/02/09 16:56 $
		$Author: Andrewkemp $

===============================================================================}

unit Functions;

interface

uses
	Windows, SysUtils, StdCtrls, Forms, Dialogs, TextMessages, Registry, ADODB, ADOInt,
	JPeg, Graphics, Classes, SetupConstants;

function CharCheck(const ACode: String): Boolean;
function Scramble(const ACode: String): String;
function ValidateSiteID(const ASiteID, AVerificationKey: String): Boolean;
function ProgramFilesFolder: String;
procedure CheckPath(AEdit: TEdit; const ASpecificPath: String = '');
procedure CheckServices(RemoteServer: Boolean = False);
procedure ResetReadOnlyFlag(const APath: String); overload;
procedure ResetReadOnlyFlag(AFileList: TStringList); overload;

function FormatBytes(const AValue: Int64): String;
procedure LoadJPEGFromRes(const AResName: String; ADestPicture: TPicture);

procedure RegisterRecorder(const InstallationPath: String);


//==============================================================================
implementation

uses
  ApiUtils, ComObj;

{-------------------------------------------------------------------------------
}
function CharCheck(const ACode: String): Boolean;
var
	i: Integer;
begin
	Result := true;
	if Length(ACode) <> 8 then Result := false;

	for i := 1 to Length(ACode) do
		if not (ACode[i] in ['0'..'9', 'a'..'z', 'A'..'Z']) then begin
			Result := False;
			Exit;
		end;
end;  // CharCheck

{-------------------------------------------------------------------------------
}
function Scramble(const ACode: String): String;
var
	i: Integer;
	lTotal: Int64;
	lString: String;
	lSpare: String;
begin
	lTotal := 0;
	Result := '0000';
	// Function is not case sensitive.
	lString  := UpperCase(ACode);
	// Stores the built-in random number generator's seed.
	Randseed := 6527;

	//  Creates an integer total from the sum of (the ASCII values of each character
	//  in the Key Sting multiplied by a random umber in the ranger 0 to 501)
	for i := 1 to Length(lString) do
		lTotal := lTotal + ((Random(500) + 1) * Ord(lString[i]));

	// if lTotal is greater than FFFF we want to use the integer remainder of 1Total/4093
	if lTotal > 65535 then
		lTotal := lTotal mod 4093;

	//Convert to Hexadeciamal
	lSpare := IntToHex(lTotal, 4);
	// Swaps the order of the characters round in lSpare
	for i := 1 to 4 do
		Result[i] := lSpare[5 - i];
end;  // Scramble

{-------------------------------------------------------------------------------
}
function ValidateSiteID(const ASiteID, AVerificationKey: String): Boolean;
begin
	Result := False;
	// Checks the site id only contains numbers and letters.
	if CharCheck(ASiteID) then
		//Checks that the Key Code entered is the correct on for the Entered Key.
		Result := Scramble(ASiteID) = UpperCase(AVerificationKey);
end;  // ValidateSiteID

{-------------------------------------------------------------------------------
}
function ProgramFilesFolder: string;
begin
	// default as old Windows versions don't store it in the registry
	Result := 'C:\Program Files\';
	with TRegistry.Create do
		try
			Rootkey := HKEY_LOCAL_MACHINE;
			Access  := KEY_READ;
			if OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion') then begin
				if ValueExists('ProgramFilesDir') then
					Result := IncludeTrailingPathDelimiter(ReadString('ProgramFilesDir'));
			end;
		finally
			CloseKey;
			Free;
		end;
end;

{-------------------------------------------------------------------------------
 Description : Checks that SQL Server and MSDTC services are started
}
procedure CheckServices(RemoteServer: Boolean = False);
begin
	if not RemoteServer then
		if Win32Platform = VER_PLATFORM_WIN32_NT then begin
			WinExec(PChar('net start MSSQLSERVER'), SW_MINIMIZE);
			WinExec(PChar('net start MSDTC'), SW_MINIMIZE);
		end;
end;

{-------------------------------------------------------------------------------
	Check the content of the Edit box is an existing path, or check the value in optional
	parameter is a valid path.
}
procedure CheckPath(AEdit: TEdit; const ASpecificPath: String = '');
var
	lPath: String;
begin
	lPath := ASpecificPath;
	if lPath = '' then lPath := AEdit.Text;
	if not DirectoryExists(lPath) then begin
		MessageDlg(Format(ResStr_InvalidPathError, [lPath]), mtWarning, [mbOk], 0);
		AEdit.SetFocus;
		Abort;
	end;
end;

{-------------------------------------------------------------------------------
 Reset the readonly flag on files specified in the given list.
}
procedure ResetReadOnlyFlag(const APath: String);
var
  lSearchRec: TSearchRec;
begin
	if FindFirst(APath + '*.*', faReadOnly + faDirectory, lSearchRec) = 0 then
		repeat
			// Ignore DOS directory maps
			if (lSearchRec.Name <> '.') and (lSearchRec.Name <> '..') then begin
				// for directories, recurse into them
				if (lSearchRec.Attr and faDirectory) > 0 then
					ResetReadOnlyFlag(APath + lSearchRec.Name + '\')
				else
				// Remove ReadOnly flag if present
				if (lSearchRec.Attr and faReadOnly) = faReadOnly then
					FileSetAttr(APath + lSearchRec.Name, lSearchRec.Attr - faReadOnly);
			end;
			Application.ProcessMessages;
		until FindNext(lSearchRec) <> 0;
	// Clean up.
	FindClose(lSearchRec);
end;

{-------------------------------------------------------------------------------
 Recursively reset the readonly flag on files found at the specified location.
}
procedure ResetReadOnlyFlag(AFileList: TStringList);
var
  i: Integer;
begin
  for i := 0 to AFileList.Count - 1 do
    if FileExists(AFileList[i]) then
      if (FileGetAttr(AFileList[i]) and faReadOnly) = faReadOnly then begin
        FileSetAttr(AFileList[i], FileGetAttr(AFileList[i]) - faReadOnly);
        Application.ProcessMessages;
      end;
end;

//------------------------------------------------------------------------------
procedure LoadJPEGFromRes(const AResName: String; ADestPicture: TPicture);
var
	lResHandle: THandle;
	lMemHandle: THandle;
	lMemStream: TMemoryStream;
	lResPtr: PByte;
	lResSize: Longint;
	lJPEGImage: TJPEGImage;
begin
	lResHandle := FindResource(hInstance, PChar(AResName), 'JPEG');
	if lResHandle <> 0 then
	begin
		lMemStream := TMemoryStream.Create;
		lJPEGImage := TJPEGImage.Create;
		try
			lMemHandle := LoadResource(hInstance, lResHandle);
			if lMemHandle <> 0 then
			begin
				lResPtr := LockResource(lMemHandle);
				if lResPtr^ <> 0 then
				begin
					lResSize := SizeOfResource(hInstance, lResHandle);
					if lResSize <> 0 then
					begin
						lMemStream.SetSize(lResSize);
						lMemStream.Write(lResPtr^, lResSize);
						lMemStream.Seek(0, soFromBeginning);
						lJPEGImage.LoadFromStream(lMemStream);
						ADestPicture.Assign(lJPEGImage);
					end;
				end;
			end;
		finally
			lJPEGImage.Free;
			lMemStream.Free;
		end;
	end;
end;

//------------------------------------------------------------------------------
function FormatBytes(const AValue: Int64): String;
var
	lResult: Double;
begin
	lResult := AValue;
	// Only some bytes
	if lResult < 1024 then begin
		Result := Format('%.0f Bytes', [lResult]);
		Exit;
	end;

	// Up to some KBs
	lResult := AValue / 1024;
	// Less than a MB?
	if lResult < 1024 then begin
		Result := Format('%.2f KB', [lResult]);
		Exit;
	end;

	// Up to some MBs
	lResult := lResult / 1024;
	// Less than a GB?
	if lResult < 1024 then begin
		Result := Format('%.2f MB', [lResult]);
		Exit;
	end;

	// No going to prepare for more than GB, TB not so widespread (yet!)
	Result := Format('%.2f GB', [lResult / 1024]);
end;


{ ------------------------------------------------------------------------------
  Registers the Recorder executable and its add-in installation component.
}
procedure RegisterRecorder(const InstallationPath: String);
begin
  WinExecAndWait32(
      InstallationPath + STR_RECORDER_MAIN_EXE + ' /regserver',
      InstallationPath,
      SW_SHOW);

  RegisterComServer(InstallationPath + STR_ADDIN_INSTALLER_DLL);
end;


end.

