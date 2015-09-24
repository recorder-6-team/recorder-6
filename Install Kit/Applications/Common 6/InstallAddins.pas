{-------------------------------------------------------------------------------
  Unit:        InstallAddins.pas

  Defines:     TInstallAddin

  Description: Install and register addins on target machine. Display and update
               progress info at the same time.

  Created:     March 2003

  Last revision information:
    $Revision: 10 $
    $Date: 5/05/09 13:46 $
    $Author: Pauldavies $

-------------------------------------------------------------------------------}

unit InstallAddins;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, StdCtrls, ComCtrls, Registry,
  OleTools, ActiveX, ComObj, Recorder2000_TLB, Settings, TextMessages, APIUtils,
  SetupConstants, GeneralFunctions;

type
  EAddinInstallError = class(Exception);

  TInstallAddins = class
  private
    FSettings   : TSettings;
    FProgress   : TProgressBar;
    FCurrentFile: TLabel;
    FAddinPath  : String;
    FCancelled  : Boolean;
    function DoRegistration(const AFileName: String): Boolean;
    function GetLibraryName(const AFileName: String): String;
    procedure InstallAddinsInFile(const AFileName: String);
    procedure InternalRegister(const AClassID: String; ARegistry: TRegistry;
      const AName: String);
    function RegisterClassesInternally(AClassList: TStringList;
      const AFileName, ASourceFile: String): Boolean;
    procedure UnRegisterRecorderTypeLib;
    procedure SetProgress(const APosition: Integer);
    procedure SetLabel(const ACaption: String);
  public
    constructor Create(ASettings: TSettings; ALabel: TLabel; AProgressBar: TProgressBar);
    procedure Cancel;
    procedure FixAddinButtons;
    procedure InstallAddins;
    procedure UninstallAddins(AddinsList: TStrings);
  end;

//==============================================================================
implementation

//==============================================================================
{ TInstallAddin }
//------------------------------------------------------------------------------
constructor TInstallAddins.Create(ASettings: TSettings; ALabel: TLabel;
  AProgressBar: TProgressBar);
begin
  FSettings    := ASettings;
  FProgress    := AProgressBar;
  FCurrentFile := ALabel;
  FCancelled   := False;
end;

//------------------------------------------------------------------------------
procedure TInstallAddins.InstallAddins;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_WRITE;
      if OpenKeyReadOnly(REG_KEY_R6) then begin
        // Read addin path created by install shield
        FAddinPath := ReadString(REG_ADDIN_PATH);

        // Ensure addin path exists
        FSettings.ForceFolders(FAddinPath);
        CloseKey;

        // Setup temporary environment for DLLs used by addins, so they can be found.
        SetEnvironmentVariable(PChar('PATH'), PChar(FAddinPath));

        // if addin key already present, addins are already installed
        if not OpenKeyReadOnly(REG_KEY_R6_ADDINS) then begin
          // Addins are not installed - try to create folder in registry
          if OpenKey(REG_KEY_R6_ADDINS, True) then
            InstallAddinsInFile(STR_DEFAULT_ADDINS)
          else
            MessageDlg(ResStr_AddinsNotInstalled, mtWarning, [mbOk], 0);
        end else
          MessageDlg(ResStr_AddinsAlreadyInstalled, mtInformation, [mbOk], 0);
        CloseKey;
      end else begin
        MessageDlg(ResStr_AddinsNotInstalled, mtWarning, [mbOk], 0);
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Install any addins listed in the given file (inside the addins dir).
  Used to install default addins.
}
procedure TInstallAddins.InstallAddinsInFile(const AFileName: String);
var i                      : Integer;
    lAddinList, lNewClasses: TStringList;
begin
  if FileExists(FAddinPath + AFileName) then begin
    // Default addins are specified, so install them
    lAddinList := TStringList.Create;
    try
      lAddinList.LoadFromFile(FAddinPath + AFileName);
      if Assigned(FProgress) then FProgress.Max := lAddinList.Count;
      SetProgress(0);

      for i := 0 to (lAddinList.Count - 1) do
      begin
        if FCancelled then Exit;

        try
          SetLabel(lAddinList[i]);
          if (lAddinList[i] <> '') and FileExists(FAddinPath + lAddinList[i]) then
            if DoRegistration(FAddinPath + lAddinList[i]) then
            begin
              { Now read the class ids from the dll }
              lNewClasses := GetClassesForComServer(FAddinPath + lAddinList[i]);
              RegisterClassesInternally(lNewClasses,
                                        FAddinPath + lAddinList[i],
                                        FAddinPath + lAddinList[i]);
              // Log full path + name
              FSettings.AddAddinName(FAddinPath + lAddinList[i]);
            end;
          SetProgress(i + 1);
          Application.ProcessMessages;
        except
          on E:Exception do
            MessageDlg(
                Format(ResStr_AddinInstallError, [lAddinList[i]]) + E.Message, mtError, [mbOk], 0);
        end;  // try
      end;  // for
    finally
      lAddinList.Free;
    end; // try
  end;
end;  // InstallAddinsInFile

{-------------------------------------------------------------------------------
  20/02/2003
  Unregister the Recorder Type libraries after unregistering addins.
  Modified to use WinExec32 and the /unregserver option to properly remove ALL registry
  entries.
}
procedure TInstallAddins.UnRegisterRecorderTypeLib;
var
  fileName: WideString;
begin
  fileName := ExtractFilePath(Copy(FAddinPath, 1, Length(FAddinPath) - 2)) + STR_RECORDER_MAIN_EXE;
  WinExecAndWait32(fileName + ' /unregserver', ExtractFilePath(fileName), SW_SHOW);
end;

//------------------------------------------------------------------------------
procedure TInstallAddins.UninstallAddins(AddinsList: TStrings);
var i: Integer;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if OpenKeyReadOnly(REG_KEY_R6) then begin
        // Read addin path
        FAddinPath := ReadString(REG_ADDIN_PATH);
        CloseKey;
      end else begin
        MessageDlg(ResStr_AddinsNotInstalled, mtWarning, [mbOk], 0);
      end;
    finally
      Free;
    end;

  // Setup temporary environment for DLLs used by addins, so they can be found.
  SetEnvironmentVariable(PChar('PATH'), PChar(FAddinPath));
  Application.ProcessMessages;

  for i := 0 to AddinsList.Count - 1 do begin
    try
      UnregisterComServer(AddinsList[i]);
    except
      on Exception do ; // ignore exceptions, where just trying to get rid of it
    end;
    Application.ProcessMessages;
  end;
  UnRegisterRecorderTypeLib;
  Application.ProcessMessages;
end;

{-------------------------------------------------------------------------------
  Try and register the COM server.  Returns true if sucessful, otherwise false
  and displays an error
}
function TInstallAddins.DoRegistration(const AFileName: String): Boolean;
begin
  Result := False; // default
  try
    RegisterComServer(AFileName);
    Result := True;
  except
    on E:Exception do
      MessageDlg(Format(ResStr_CannotRegisterAddin, [AFileName]) + E.Message, mtInformation, [mbOK], 0);
  end;
end;  // DoRegistration

{-------------------------------------------------------------------------------
  Performs the necessary work to put registration entries for Recorder 2000
  addins according to the class list.  Returns true if at least one class
  is successfully installed.
  ASourceFile is passed to the addin as a parameter of install, in case there
  are any other files required in the install folder.
}
function TInstallAddins.RegisterClassesInternally(AClassList: TStringList;
  const AFileName, ASourceFile: String): Boolean;
var i            : Integer;
    lRegistry    : TRegistry;
    lRecorderIntf: IRecorderAddin;
begin
  Result := False; // default - no valid objects
  try
    lRegistry := TRegistry.Create;
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    lRegistry.Access  := KEY_WRITE;
    try
      for i := 0 to AClassList.Count - 1 do begin
        try
          try
            lRecorderIntf := CreateComObject(StringToGuid(AClassList[i])) as IRecorderAddin;
          except
            on EOleSysError do Continue;
          end;
          lRecorderIntf.Install(ASourceFile);
          InternalRegister(AClassList[i], lRegistry,
                           GetLibraryName(AFileName) + '.' + lRecorderIntf.Name);
          Result := true; // succesful installation
          Application.ProcessMessages;
        except
          { Not a valid Addin, so ignore class }
          on EIntfCastError do;
        end; // try..except
      end;
    finally
      lRegistry.Free;
    end; // try..finally
  except
    on E:Exception do
      MessageDlg(
          Format(ResStr_AddinInternalRegistrationError, [AFileName]) + E.Message, mtWarning, [mbOk], 0);
  end;
end;  // RegisterClassesInternally

{-------------------------------------------------------------------------------
  Sets up the internal registry settings required for a single class.  If
  already present, set installed to "1".  The registry object is set to
  HKEY_LOCAL_MACHINE with no key set.
}
procedure TInstallAddins.InternalRegister(const AClassID: String; ARegistry: TRegistry;
  const AName: String);
begin
  { Allow creation of the key if not present }
  with ARegistry do begin
    OpenKey(REG_KEY_R6_ADDINS + '\' + AName, True);
    WriteString('ClsID', AClassID);
    WriteString('Installed', '1');
    CloseKey;
  end; // with
end;  // InternalRegister

{-------------------------------------------------------------------------------
  Extracts the filename (no extension) from a full file name including path
}
function TInstallAddins.GetLibraryName(const AFileName: String): String;
var lFileName, lFileExt: String;
begin
  lFileName := ExtractFileName(AFileName);
  lFileExt  := ExtractFileExt(AFilename);
  { Lop off the extension }
  Result := Copy(lFileName, 1, Length(lFileName) - Length(lFileExt));
end;  // GetLibraryName

{-------------------------------------------------------------------------------
  14/02/2003
  Ensures that addin buttons still point to the correct action, because the
  number of actions changes in this version.
  This also deletes some redundant registry settings.
}
procedure TInstallAddins.FixAddinButtons;
var lValues   : TStringList;
    i, lBtnIdx: Integer;
begin
  if FCancelled then Exit;

  lValues := TStringList.Create;
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(REG_KEY_R6_SETTINGS + '\Standard', False) then begin
        GetValueNames(lValues);
        for i := 0 to lValues.Count - 1 do begin
          if (Length(lValues[i]) < 3) and (GetDataType(lValues[i]) = rdInteger) then begin
            lBtnIdx := ReadInteger(lValues[i]);
            // If a com action (beyond end of standard action list) then mark up
            // This maintains alignment so future upgrades don't break it
            if lBtnIdx >= 40 then
              WriteString(lValues[i], 'Addin:' + IntToStr(lBtnIdx - 41));
          end;
          Application.ProcessMessages;
        end;
      end;
      // Delete some redundant settings
      if OpenKey(REG_KEY_R6_SETTINGS, False) then begin
        DeleteValue('Search All Checklists');
        DeleteValue('Map Dataset System');
        DeleteValue('CD Database Path');
        DeleteValue('Checklist Source');
      end;
    finally
      CloseKey;
      Free;
      lValues.Free;
    end; // try
end;

//------------------------------------------------------------------------------
procedure TInstallAddins.SetProgress(const APosition: Integer);
begin
  if Assigned(FProgress) then begin
    FProgress.Position := APosition;
    Application.ProcessMessages;
  end;
end;

procedure TInstallAddins.SetLabel(const ACaption: String);
begin
  if Assigned(FCurrentFile) then begin
    FCurrentFile.Caption := ACaption;
    FCurrentFile.Refresh;
  end;
end;

procedure TInstallAddins.Cancel;
begin
  FCancelled := True;
end;

//------------------------------------------------------------------------------
end.
