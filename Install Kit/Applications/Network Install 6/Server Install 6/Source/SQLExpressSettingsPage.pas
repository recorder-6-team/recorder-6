{===============================================================================
  Unit:        SQLExpressSettingsPage

  Defines:     TfraSQLExpressSettings

  Description:

  Model:       Server Install 6.mpb

  Created:     March 2009

  Last revision information:
    $Revision: 6 $
    $Date: 14/07/09 11:38 $
    $Author: Ericsalmon $

===============================================================================}

unit SQLExpressSettingsPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, RestrictedEdits, Registry,
  ExtCtrls, BaseSQLExpressSettingsPage;

type
  TfraSQLExpressSettings = class(TfraBaseSQLExpressSettings)
    dlgFolder: TFolderBrowser;
    eNewInstanceName: TRestrictedEdit;
    lblExistingInstancePlural: TLabel;
    lblExistingInstanceSingular: TLabel;
    lblNewInstanceName: TLabel;
    lblNameInUse: TLabel;
    pnlControls: TPanel;
    lblNoSpace: TLabel;
    eServerPath: TEdit;
    btnServerPath: TButton;
    lblInstanceDir: TLabel;
    lblNotLocal: TLabel;
    lblEnterPassword: TLabel;
    lblConditions: TLabel;
    lblPassword: TLabel;
    ePassword: TEdit;
    ePasswordConfirm: TEdit;
    lblPasswordConfirm: TLabel;
    procedure CheckChange(Sender: TObject);
    procedure eServerPathChange(Sender: TObject);
    procedure btnServerPathClick(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    function GetNextCaption: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure ValidateContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  Settings, SetupConstants, TextMessages, InstallationPage, GeneralFunctions;

resourcestring
  ResStr_SQLInstallFolderMissing =
      'The selected SQL Express install folder does not exist. Create now?';

{-==============================================================================
    TfraSQLExpressSettings
===============================================================================}

{-------------------------------------------------------------------------------
}
procedure TfraSQLExpressSettings.CheckChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraSQLExpressSettings.CheckChange 

{-------------------------------------------------------------------------------
}
function TfraSQLExpressSettings.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraSQLExpressSettings.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraSQLExpressSettings.GetHasNext: Boolean;
begin
  result := ValidInstanceName(eNewInstanceName.Text) and
            ServerPathIsValid(eServerPath.Text, lblNotLocal, lblNoSpace) and
            ValidPassword(ePassword.Text, ePasswordConfirm.Text);
end;  // TfraSQLExpressSettings.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraSQLExpressSettings.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraSQLExpressSettings.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraSQLExpressSettings.GetNext: TBasePageClass;
begin
  Result := TfraInstallation;
end;  // TfraSQLExpressSettings.GetNext 

{-------------------------------------------------------------------------------
  The caption to display on the next button.
}
function TfraSQLExpressSettings.GetNextCaption: String;
begin
  Result := ResStr_InstallCaption;
end;  // TfraSQLExpressSettings.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraSQLExpressSettings.GetResourceImage: String;
begin
  Result := ResImg_SQLExpress;
end;  // TfraSQLExpressSettings.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraSQLExpressSettings.LoadContent;
begin
  inherited;
  with Settings do begin
    lblExistingInstanceSingular.Visible := LocalInstanceNames.Count = 1;
    lblExistingInstancePlural.Visible   := LocalInstanceNames.Count > 1;

    if LocalInstanceNames.Count = 0 then
      pnlControls.Top := lblExistingInstancePlural.Top + 4;

    eNewInstanceName.Text := eNewInstanceName.Text;
    eServerPath.Text      := NewInstancePath;
  end;
  ChangedContent;
end;  // TfraSQLExpressSettings.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraSQLExpressSettings.SaveContent;
begin
  inherited;
  with Settings do begin
    SpecifyOwnInstanceName := True;
    NewInstanceName        := eNewInstanceName.Text;

    SpecifyOwnInstancePath := not (eServerPath.Text = '');
    NewInstancePath        := eServerPath.Text;

    Settings.Password := ePassword.Text;
  end;
end;  // TfraSQLExpressSettings.SaveContent

{-------------------------------------------------------------------------------
  When the server path is changed, checks whether the server path is valid (so as
  to hide/display labels), then sets the content as changed.
}
procedure TfraSQLExpressSettings.eServerPathChange(Sender: TObject);
begin
  inherited;
  ServerPathIsValid(eServerPath.Text, lblNotLocal, lblNoSpace);
  ChangedContent;
end;

{-------------------------------------------------------------------------------
  Opens a folder dialog to allow the user to select a file path.
}
procedure TfraSQLExpressSettings.btnServerPathClick(Sender: TObject);
begin
  inherited;
  if DirectoryExists(eServerPath.Text) then
    dlgFolder.Folder := eServerPath.Text;

  if dlgFolder.Execute then
  begin
    eServerPath.Text     := dlgFolder.Folder;
    eServerPath.Modified := True;
    ServerPathIsValid(eServerPath.Text, lblNotLocal, lblNoSpace);
    ChangedContent;
  end;
end;  

{-------------------------------------------------------------------------------
  Validates the controls on this page.
}
procedure TfraSQLExpressSettings.ValidateContent;
begin
  // Gives the user a chance to create the folder they selected, if it does not
  // already exist.
  // Only allow moving on to next page if folder gets created, or already exists.
  if not Settings.FolderExists(eServerPath.Text, True, True, ResStr_SQLInstallFolderMissing) then
    Abort;
end;  // TfraSQLExpressSettings.ValidateContent

end.

