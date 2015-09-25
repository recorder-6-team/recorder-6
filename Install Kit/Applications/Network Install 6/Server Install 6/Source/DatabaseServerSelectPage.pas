{===============================================================================
  Unit:        DatabaseServerSelectPage

  Defines:     TfraDatabaseServerSelect

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 15 $
    $Date: 6/03/09 11:52 $
    $Author: Pauldavies $

===============================================================================}

unit DatabaseServerSelectPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, RestrictedEdits, Registry,
  ExtCtrls, BaseSQLExpressSettingsPage;

type
  TfraDatabaseServerSelect = class(TfraBaseSQLExpressSettings)
    cmbInstances: TComboBox;
    lblInfo: TLabel;
    lblInstanceName: TLabel;
    lblWarning: TLabel;
    lblInfoNoDetect: TLabel;
    eInstance: TEdit;
    lblDropDownInfo: TLabel;
    procedure CheckChange(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  InstallFolderPage, MigrateWelcomePage, LoginPage, Settings, SetupConstants,
  TextMessages, GeneralFunctions;

{-==============================================================================
    TfraDatabaseServerSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraDatabaseServerSelect.CheckChange(Sender: TObject);
begin
  inherited;
  if Loading then Exit;
  CheckTrustedConnectionMode(cmbInstances.Text, lblWarning);
  ChangedContent;
end;  // TfraDatabaseServerSelect.CheckChange 

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetConfirmCancel: Boolean;
begin
  Result := Settings.FoldersCreated.Count <> 0;
end;  // TfraDatabaseServerSelect.GetConfirmCancel

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetHasNext: Boolean;
begin
  Result := (eInstance.Visible and (eInstance.Text <> '')) or
            (cmbInstances.Visible and (cmbInstances.Text<>''));
end;  // TfraDatabaseServerSelect.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraDatabaseServerSelect.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetNext: TBasePageClass;
var
  instanceName: String;
begin
  Result := TfraLogin;

  if eInstance.Visible then instanceName := eInstance.Text;
  if cmbInstances.Visible then instanceName := cmbInstances.Text;
  if not ValidInstanceName(instanceName) then begin
    Result := nil;
    Exit;
  end;
end;  // TfraDatabaseServerSelect.GetNext

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetPrevious: TBasePageClass;
begin
  if Settings.Mode = moMigrate then
    Result := TfraMigrateWelcome
  else
    Result := TfraInstallFolder;
end;  // TfraDatabaseServerSelect.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraDatabaseServerSelect.GetResourceImage: String;
begin
  Result := ResImg_SQLExpress;
end;  // TfraDatabaseServerSelect.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraDatabaseServerSelect.LoadContent;
var
  lCanDetectInstances: Boolean;
  i: integer;
begin
  inherited;
  lCanDetectInstances := Win32Platform = VER_PLATFORM_WIN32_NT;

  // In case the OS is Win9x/ME, can't find network instances, so let user type it in.
  lblInfoNoDetect.Visible := not lCanDetectInstances;
  eInstance.Visible := not lCanDetectInstances;
  eInstance.Text := Settings.ServerName;

  // Modern-ish OS, list instances.
  lblInfo.Visible := lCanDetectInstances;
  cmbInstances.Visible := lCanDetectInstances;
  lblDropDownInfo.Visible := lCanDetectInstances;
  
  with cmbInstances do begin
    if Settings.Mode = moMigrate then
      // Only want local instances, no network stuff. AccessDB must be local for transfer.
      Items.Assign(Settings.LocalInstanceNames)
    else begin
      // Allow any instance,
      Items.Assign(Settings.LocalInstanceNames);
      for i := 0 to Settings.SQLInstanceNames.Count-1 do begin
        if Items.IndexOf(Settings.SQLInstanceNames[i])=-1 then
          Items.Add(Settings.SQLInstanceNames[i]);
      end;
    end;

    // If nothing uet set for default existing instance, select first one.
    ItemIndex := Items.IndexOf(Settings.ServerName);
    if (ItemIndex = -1) and (Items.Count > 0) then ItemIndex := 0;
  end;
  CheckTrustedConnectionMode(cmbInstances.Text, lblWarning);
  ChangedContent;
end;  // TfraDatabaseServerSelect.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraDatabaseServerSelect.SaveContent;
begin
  inherited;
  if eInstance.Visible then
    Settings.ServerName := eInstance.Text
  else
    Settings.ServerName := cmbInstances.Text;
  Settings.TrustedLogin := lblWarning.Visible;
end;  // TfraDatabaseServerSelect.SaveContent 

end.

