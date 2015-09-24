unit VersionInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ShellAPI,
  Forms, Dialogs, Registry;

type
  TTimeOfWhat = ( ftCreationTime, ftLastAccessTime, ftLastWriteTime );

  PFixedFileInfo = ^TFixedFileInfo;
  TFixedFileInfo = record
     dwSignature       : DWORD;
     dwStrucVersion    : DWORD;
     wFileVersionMS    : WORD;  // Minor Version
     wFileVersionLS    : WORD;  // Major Version
     wProductVersionMS : WORD;  // Build Number
     wProductVersionLS : WORD;  // Release Version
     dwFileFlagsMask   : DWORD;
     dwFileFlags       : DWORD;
     dwFileOS          : DWORD;
     dwFileType        : DWORD;
     dwFileSubtype     : DWORD;
     dwFileDateMS      : DWORD;
     dwFileDateLS      : DWORD;
  end; // TFixedFileInfo

  function GetFileVersion(const FileName :String ): string;
  function GetFileTime(const FileName : String; ComparisonType : TTimeOfWhat ): TDateTime;
  function AddinVersion(const FileName: string): string;


implementation

function GetFileInfo( const FileName :String ) : TFixedFileInfo;
var
  dwHandle, dwVersionSize : DWORD;
  strSubBlock             : String;
  pTemp                   : Pointer;
  pData                   : Pointer;
begin
   strSubBlock := '\';

   // get version information values
   dwVersionSize := GetFileVersionInfoSize( PChar( FileName ), // pointer to filename string
                                            dwHandle );        // pointer to variable to receive zero

   // if GetFileVersionInfoSize is successful
   if dwVersionSize <> 0 then
   begin
      GetMem( pTemp, dwVersionSize );
      try
         if GetFileVersionInfo( PChar( FileName ),             // pointer to filename string
                                dwHandle,                      // ignored
                                dwVersionSize,                 // size of buffer
                                pTemp ) then                   // pointer to buffer to receive file-version info.

            if VerQueryValue( pTemp,                           // pBlock     - address of buffer for version resource
                              PChar( strSubBlock ),            // lpSubBlock - address of value to retrieve
                              pData,                           // lplpBuffer - address of buffer for version pointer
                              dwVersionSize ) then             // puLen      - address of version-value length buffer
               Result := PFixedFileInfo( pData )^;
      finally
         FreeMem( pTemp );
      end; // try
   end; // if dwVersionSize
end;

function GetFileVersion(const FileName :String ): string;
var lFileInfo: TFixedFileInfo;
begin
     Result := '';
     lFileInfo := GetFileInfo(FileName);
     {If lFileInfo <> nil then}
        with lFileInfo do
             Result := IntToStr(wFileVersionLS) + '.' +
                       IntToStr(wFileVersionMS) + '.' +
                       IntToStr(wProductVersionLS) + '.' +
                       IntToStr(wProductVersionMS);
end;

function myGetFileTime( const FileName : String; ComparisonType : TTimeOfWhat ) : TFileTime;
var
   FileTime, LocalFileTime : TFileTime;
   hFile                   : THandle;
begin
   // initialize TFileTime record in case of error
   Result.dwLowDateTime := 0;
   Result.dwHighDateTime := 0;
   hFile := FileOpen( FileName, fmShareDenyNone );
   try
      if hFile <> 0 then
      begin
         case ComparisonType of
            ftCreationTime   : Windows.GetFileTime( hFile, @FileTime, nil, nil );
            ftLastAccessTime : Windows.GetFileTime( hFile, nil, @FileTime, nil );
            ftLastWriteTime  : Windows.GetFileTime( hFile, nil, nil, @FileTime );
         end; // case FileTimeOf

         // Change the file time to local time
         FileTimeToLocalFileTime( FileTime, LocalFileTime );
         Result := LocalFileTime;
      end; // if hFile <> 0
   finally
      FileClose( hFile );
   end; // try
end;

function GetFileTime( const FileName : String; ComparisonType : TTimeOfWhat ): TDateTime;
var
   SystemTime : TSystemTime;
   FileTime   : TFileTime;
begin
   FileTime := myGetFileTime( FileName, ComparisonType );
   if FileTimeToSystemTime( FileTime, SystemTime ) then
      // Convert to TDateTime and return
      Result := SystemTimeToDateTime( SystemTime )
   else
      Result := 0.0;
end;

// Get the string to display for a Rec2K addin
function AddinVersion(const FileName: string): string;
var lReg:  TRegistry;
    lPath, lFile: string;
begin
  {Get addin path from registry}
  lReg := TRegistry.Create;
  try
     lReg.OpenKey('Software\Dorset Software\Recorder 6\Settings',False);
     lPath := lReg.ReadString('Addin Path');
  finally
     lReg.Free;
  end;
  lFile := lPath + FileName;
  Result := ' Version ' + GetFileVersion(lFile) +
            ' of ' + DateToStr(GetFileTime(lFile, ftLastWriteTime)) + '.';
end;

end.
