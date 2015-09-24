{===============================================================================
  Unit:        RemovePage

  Defines:     TfraRemove

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 4 $
    $Date: 11/02/09 15:33 $
    $Author: Ericsalmon $

===============================================================================}

unit RemovePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls, ComCtrls, VCLZip, VCLUnzip;

type
  TfraRemove = class(TBasePage)
    Animation: TAnimate;
    lblInformation: TLabel;
    lblArchiving: TLabel;
    lblFiles: TLabel;
    lblRegistry: TLabel;
    lblShortcuts: TLabel;
    lblStepArchive: TLabel;
    lblStepFiles: TLabel;
    lblStepRegistry: TLabel;
    lblStepShortcuts: TLabel;
    pbZip: TProgressBar;
    pnlFiles: TPanel;
  private
    FZipTool: TVCLZip;
    procedure ArchiveDatabase;
    procedure RemoveFiles;
    procedure RemoveRegistryEntries;
    procedure RemoveShortcuts;
    procedure ZipTotalPercentDone(Sender: TObject; Percent: Integer);
  protected
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure Cancel; override;
    function Execute: Boolean; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  RemoveCompletePage, SetupConstants, Functions, GeneralFunctions, ShortCuts,
  ApiUtils, Registry, RemovalSettings;

{-==============================================================================
    TfraRemove
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraRemove.ArchiveDatabase;
begin
  FZipTool := TVCLZip.Create(nil);
  try
    lblStepArchive.Caption := STR_ARROW;
    lblArchiving.Font.Style := [fsBold];
    Refresh;
    // Setup component ready for zipping the database
    with FZipTool do begin
      if not SameText(ExtractFileExt(TRemovalSettings(Settings).ArchiveZipName), '.zip')
          then
        ZipName := TRemovalSettings(Settings).ArchiveZipName + '.zip'
      else
        // Name of zip file, includes the .zip extension.
        ZipName := TRemovalSettings(Settings).ArchiveZipName;
  
      Recurse    := False;
      StorePaths := False;
      PackLevel  := 9;
      DoProcessMessages := True;
      OnTotalPercentDone := ZipTotalPercentDone;
      // Add file name to list of files to zip (only NBNData and NBNDict)
      FilesList.Add(TRemovalSettings(Settings).MigrationAccessDBPath + ACCESS_MAIN_DB);
      if FileExists(TRemovalSettings(Settings).MigrationAccessDBPath + ACCESS_DICT_DB)
          then
        FilesList.Add(TRemovalSettings(Settings).MigrationAccessDBPath + ACCESS_DICT_DB);
  
      if FileExists(ZipName) then DeleteFile(ZipName);
      Zip;
    end;
  finally
    FreeAndNil(FZipTool);
    lblStepArchive.Caption := STR_TICK;
    lblArchiving.Font.Style := [];
    Refresh;
  end;
end;  // TfraRemove.ArchiveDatabase 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.Cancel;
begin
  if Assigned(FZipTool) then FZipTool.CancelTheOperation;
  Application.ProcessMessages;
end;  // TfraRemove.Cancel 

{-------------------------------------------------------------------------------
}
function TfraRemove.Execute: Boolean;
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    if TRemovalSettings(Settings).ArchiveDatabase then begin
      Animation.CommonAVI := aviCopyFile;
      Animation.Active := True;
      ArchiveDatabase;
      Animation.Active := False;
    end;
    // Start animation, something to look at...
    Animation.CommonAVI := aviRecycleFile;
    Animation.Active := True;
    RemoveShortCuts;
    RemoveFiles;
    RemoveRegistryEntries;
    Sleep(1000);
  finally
    DefaultCursor(lCursor);
    // Enough of that now
    Animation.Active := False;
  end;
  Application.ProcessMessages;
  Result := True;
end;  // TfraRemove.Execute 

{-------------------------------------------------------------------------------
}
function TfraRemove.GetNext: TBasePageClass;
begin
  Result := TfraRemoveComplete;
end;  // TfraRemove.GetNext 

{-------------------------------------------------------------------------------
}
function TfraRemove.GetResourceImage: String;
begin
  Result := ResImg_Remove;
end;  // TfraRemove.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.LoadContent;
begin
  inherited;
  if Settings.OSVersion >= wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileCopy);
    Animation.Active := True;
  end;
  // No archiving requested, move panel with all other labels to hide the archiving bit.
  if not TRemovalSettings(Settings).ArchiveDatabase then
    pnlFiles.Top := lblArchiving.Top - 8;
end;  // TfraRemove.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.RemoveFiles;
begin
  lblStepFiles.Caption := STR_ARROW;
  lblFiles.Font.Style := [fsBold];
  Refresh;
  Sleep(500);
  
  // Remove files and folders.
  with TRemovalSettings(Settings) do
    if R2K2InstallPath <> '' then begin
      ResetReadOnlyFlag(R2K2InstallPath);
      RemoveFileOrFolder(R2K2InstallPath);
    end;
  
  lblFiles.Font.Style := [];
  lblStepFiles.Caption := STR_TICK;
end;  // TfraRemove.RemoveFiles 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.RemoveRegistryEntries;
begin
  lblStepRegistry.Caption := STR_ARROW;
  lblRegistry.Font.Style := [fsBold];
  Refresh;
  Sleep(500);
  
  with TRegistry.Create do
    try
      // Delete user's settings.
      RootKey := HKEY_CURRENT_USER;
      Access  := KEY_WRITE;
      if KeyExists(REG_KEY_JNCC) then DeleteKey(REG_KEY_JNCC);
      // Delete machine's settings.
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_ALL_ACCESS;
      if KeyExists(REG_KEY_JNCC) then DeleteKey(REG_KEY_JNCC);
      // And a few other entries.
      if KeyExists(REG_KEY_R2K2UNINSTALLER) then DeleteKey(REG_KEY_R2K2UNINSTALLER);
      CloseKey;
    finally
      Free;
    end;
  
  lblRegistry.Font.Style := [];
  lblStepRegistry.Caption := STR_TICK;
end;  // TfraRemove.RemoveRegistryEntries 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.RemoveShortcuts;
begin
  lblStepShortcuts.Caption := STR_ARROW;
  lblShortcuts.Font.Style := [fsBold];
  Refresh;
  Sleep(500);
  
  with TShortcuts.Create(R2K2_START_MENU) do
    try
      RemoveShortcuts([R2K2_PROGRAM_LINK, R2K2_GUIDE_LINK]);
    finally
      Free;
    end;
  
  lblShortcuts.Font.Style := [];
  lblStepShortcuts.Caption := STR_TICK;
end;  // TfraRemove.RemoveShortcuts 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.SaveContent;
begin
  inherited;
  if Assigned(FZipTool) then FZipTool.Free;
end;  // TfraRemove.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraRemove.ZipTotalPercentDone(Sender: TObject; Percent: Integer);
begin
  pbZip.Position := Percent;
end;  // TfraRemove.ZipTotalPercentDone 

end.

