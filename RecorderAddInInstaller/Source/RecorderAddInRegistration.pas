{===============================================================================
  Unit:         RecorderAddInRegistration

  Defines:      TRecorderAddinRegistration

  Description:  A COM component that can be used to install or remove Recorder
                add-ins.

                Most of this code was moved from Recorder's TdlgAddIn class.

                Designed for use with COM elevation on versions of Windows from
                Vista onwards.  When running on earlier versions of Windows, it
                behaves the same way that Recorder did before this code was
                extracted.

  Created:      February 2009

  Last revision information:
    $Revision: 2 $
    $Date: 19/02/09 11:03 $
    $Author: Andrewkemp $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
unit RecorderAddInRegistration;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, RecorderAddinInstaller_TLB;

type
  { ----------------------------------------------------------------------------
    A COM component that can be used to install or remove Recorder add-ins.
  }
  TRecorderAddinRegistration = class(TTypedComObject,
      IRecorderAddinRegistration)
  private
    FMessages: WideString;
    procedure CopyBitmaps(const FromPath, ToPath: String);
    function RegisterClassesInternally(
        iClassList: TStringList;
        const iFileName, iSourceFile: String): Boolean;
  protected {IRecorderAddInRegistration}
    function IsElevated: WordBool; stdcall;
    function Install(const FileName, Path: WideString; DoCopy: WordBool): HResult; stdcall;
    function Remove(ClassID: TGUID): HResult; stdcall;
    function Get_Messages(out Value: WideString): HResult; stdcall;
  end;


implementation

uses
  ComServ, SysUtils, Dialogs, Controls, Registry, OleTools, Recorder2000_TLB,
  PrivilegedObjectFactory, PrivilegedComObj;

resourcestring
  ResStr_InstallProblem =
      'The addin could not be installed for the following reason: ';
  ResStr_InstallCopyFailedStandalone = 'The addin could not be installed because the file could not be copied to '+
      'the destination folder because you do not have the required permissions or because you have '+
      'just uninstalled the addin and Recorder still has the addin file open. If this is the case then '+
      'restarting Recorder and installing the addin again should allow the file to be copied, or if there '+
      'is not a new version of the addin available you can try installing it from the Recorder '+
      'Addins directory.';
  ResStr_InstallCopyFailedNetwork = 'The addin could not be installed because the file could not be copied to '+
      'the destination folder. This could be because the addin is in use on another machine on the network, '+
      'or you do not have the required permissions, or because you have '+
      'just uninstalled the addin and Recorder still has the addin file open. If this is the case then '+
      'restarting Recorder and installing the addin again should allow the file to be copied, or if there '+
      'is not a new version of the addin available you can try installing it from the Recorder '+
      'Addins directory.';

const
  APPID_RecorderAddInInstaller: TGUID =
      '{89103B41-1FCB-4EED-BB6C-A51D413317AE}';

  RecorderNameResourceId = '101';
      // Identifies a string resource in the DLL that contains Recorder's
      // application name; see 'ApplicationName.RC' and 'ApplicationName.RES'.
  
  REG_KEY_RECORDER = '\Software\Dorset Software\Recorder 6';
  REG_KEY_ADDIN = REG_KEY_RECORDER + '\Installed Addins';
  REG_INSTALLED = 'Installed';
  REG_CLASS_ID  = 'ClsId';

procedure InternalRegister(const iClassID: string; iRegistry: TRegistry;
  iName: string); forward;
function GetLibraryName(const iFileName: String): String; forward;
  

{-------------------------------------------------------------------------------
  Is the component elevated?

  Always True on versions of Windows earlier than Vista.
}
function TRecorderAddInRegistration.IsElevated: WordBool;
begin
  Result := PrivilegedComObj.IsElevated;
end;

{-------------------------------------------------------------------------------
  Installs the Recorder add-in contained in the specified file into the
  given path.
}
function TRecorderAddinRegistration.Install(
  const FileName, Path: WideString; DoCopy: WordBool): HResult;
var
  lAddInFilePath: String;
  lNewClasses: TStringList;
  reg: TRegistry;
begin
  if not IsElevated then
  begin
    Result := E_ACCESSDENIED;
    Exit;
  end;
  Result := S_OK;
  
  try
    { First move the files to the \Addin and \Addin images folder }
    if CompareText(ExtractShortPathName(ExtractFilePath(FileName)),
                   ExtractShortPathName(Path)) <> 0 then
    begin
      if DoCopy then begin
        // overwrite the existing file, since Recorder should have confirmed any file overwrites
        // before calling this addin
        if not CopyFileW(
            PWideChar(FileName),
            PWideChar(Path + ExtractFileName(FileName)),
            False) then begin
          // copy failed
          Result := E_FAIL;
          // need to know if on networked Recorder, as reason for failure different
          reg :=  TRegistry.Create;
          try
            reg.RootKey := HKEY_LOCAL_MACHINE;
            reg.OpenKeyReadOnly('SOFTWARE\Dorset Software\Recorder 6\');
            if reg.ReadBool('Standalone') then
              FMessages := FMessages + #13#10 + ResStr_InstallCopyFailedStandalone
            else
              FMessages := FMessages + #13#10 + ResStr_InstallCopyFailedNetwork;
          finally
            reg.Free;
          end;
          Exit; // give up
        end;
      end;
    end; // if different folder
    CopyBitmaps(ExtractFilePath(FileName), Path);
    lAddInFilePath := Path + ExtractFileName(FileName);
    { Register the COM Server }
    RegisterComServer(lAddInFilePath);
    { Now read the class ids from the dll }
    lNewClasses := GetClassesForComServer(lAddinFilePath);
    try
      if not RegisterClassesInternally(lNewClasses, lAddinFilePath, FileName) then
        result := E_FAIL;
    finally
      lNewClasses.Free;
    end;
  except
    on E: Exception do
    begin
      FMessages := FMessages + #13#10 + E.Message;
      Result := E_FAIL;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Removes the specified add-in from Recorder's list.
}
function TRecorderAddInRegistration.Remove(ClassID: TGUID): HResult;
var
  Registry: TRegistry;
  InstalledClasses: TStringList;
  I: Integer;
begin
  if not IsElevated then
  begin
    Result := E_ACCESSDENIED;
    Exit;
  end;
  Result := S_OK;
    
  Registry := TRegistry.Create;
  InstalledClasses := TStringList.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKeyReadOnly(REG_KEY_ADDIN);
    Registry.GetKeyNames(InstalledClasses);

    for I := 0 to InstalledClasses.Count - 1 do
    begin
      Registry.CloseKey;
      Registry.OpenKeyReadOnly(REG_KEY_ADDIN + '\' + InstalledClasses[i]);
      { If this is the registered class }
      if Registry.ReadString(REG_CLASS_ID) = GuidToString(ClassID) then
      begin
        Registry.CloseKey;
        Registry.OpenKeyReadOnly(REG_KEY_ADDIN);
        { Remove the relevant key }
        if not Registry.DeleteKey(InstalledClasses[i]) then
          Result := E_FAIL;
      end;
    end;
  finally
    Registry.Free;
    InstalledClasses.Free;
  end; // try..finally
end;

{ ------------------------------------------------------------------------------
  Gets any messages that were recorded during the last attempt to register
  add-ins.
}
function TRecorderAddInRegistration.Get_Messages(
  out Value: WideString): HResult;
begin
  Value := FMessages;
  Result := S_OK;
end;

{ ------------------------------------------------------------------------------
  Copies any bitmaps in the supplied path into the addins images directory
}
procedure TRecorderAddInRegistration.CopyBitmaps(
  const FromPath, ToPath: String);
var
  lSearchRec: TSearchRec;
begin
  { If a file found }
  if FindFirst(FromPath + '*.bmp', faAnyFile, lSearchRec) = 0 then
    repeat
      CopyFile(
          PChar(lSearchRec.Name),
          PChar(ToPath + 'Images\' + ExtractFileName(lSearchRec.Name)),
          True);
    until FindNext(lSearchRec) <> 0;
end;

{-------------------------------------------------------------------------------
  Performs the necessary work to put registration entries for Recorder 2000
  addins according to the class list.  Returns true if at least one class
  is successfully installed.
  iSourceFile is passed to the addin as a parameter of install, in case there
  are any other files required in the install folder.
}
function TRecorderAddInRegistration.RegisterClassesInternally(
    iClassList: TStringList; const iFileName, iSourceFile: String): Boolean;
var
  i : integer;
  lRegistry : TRegistry;
  lRecorderIntf : IRecorderAddin;
begin
  FMessages := '';
  Result := False; // default - no valid objects
  lRegistry := TRegistry.Create;
  lRegistry.RootKey := HKEY_LOCAL_MACHINE;
  try
    for i := 0 to iClassList.Count-1 do
    begin
      try
        lRecorderIntf := CreateComObject(StringToGuid(iClassList[i])) as IRecorderAddin;
        lRecorderIntf.Install(iSourceFile);
        InternalRegister(
            iClassList[i], lRegistry, GetLibraryName(iFileName) + '.' + lRecorderIntf.Name);
        Result := true; // succesful installation
      except
        { Not a valid Addin, so ignore class }
        on EIntfCastError do ;
        on E:EOleSysError do
          if EOleSysError(E).ErrorCode <> -2147221164 then begin  // Class not registered
            FMessages := FMessages + #13#10 + E.Message;
            InternalRegister(
                iClassList[i], lRegistry, GetLibraryName(iFileName) + '.' + lRecorderIntf.Name);
          end;
      end; // try..except
    end;
  finally
    lRegistry.Free;
  end; // try..finally
end;

{-------------------------------------------------------------------------------
  Sets up the internal registry settings required for a single class.  If
  already present, set installed to "1".  The registry object is set to
  HKEY_LOCAL_MACHINE with no key set.
}
procedure InternalRegister(const iClassID: string; iRegistry: TRegistry;
  iName: string);
begin
  { Allow creation of the key if not present }
  with iRegistry do
  begin
    OpenKey(REG_KEY_ADDIN + '\' + iName, True);
    WriteString(REG_CLASS_ID, iClassID);
    WriteString(REG_INSTALLED, '1');
    CloseKey;
  end; // with
end;

{-------------------------------------------------------------------------------
  Extracts the filename (no extension) from a full file name including path
}
function GetLibraryName(const iFileName: String): String;
begin 
  Result := ChangeFileExt(ExtractFileName(iFileName), '');
end;


initialization
  TPrivilegedObjectFactory.Create(
      APPID_RecorderAddInInstaller,
      RecorderNameResourceId,
      ComServer,
      TRecorderAddinRegistration,
      Class_RecorderAddinRegistration,
      ciMultiInstance,
      tmApartment);
end.
