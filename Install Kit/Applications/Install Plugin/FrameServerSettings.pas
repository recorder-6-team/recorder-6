{-------------------------------------------------------------------------------
  Unit:        FrameServerSettings.pas

  Defines:     TfraServerSettings

  Description: Additional information for install.

  Created:     February 2003

  Last revision information:
    $Revision: 8 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameServerSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, RestrictedEdits, ExtCtrls, TextMessages,
  Settings, FolderBrowser;

type
  TfraServerSettings = class(TPageFrame)
    cbInstanceName: TCheckBox;
    lblName: TLabel;
    eInstanceName: TRestrictedEdit;
    cbDataDir: TCheckBox;
    eDataDir: TEdit;
    btnFolderBrowse: TButton;
    lblPath: TLabel;
    Label1: TLabel;
    FolderBrowser: TFolderBrowser;
    Label4: TLabel;
    procedure eInstanceNameCheckText(Sender: TObject; const iText: String;
      var ioAccept: Boolean);
    procedure cbInstanceNameClick(Sender: TObject);
    procedure eInstanceNameChange(Sender: TObject);
    procedure eDataDirChange(Sender: TObject);
    procedure cbDataDirClick(Sender: TObject);
    procedure btnFolderBrowseClick(Sender: TObject);
  private
    FJustCreated: Boolean;
    procedure SetInstanceNameState;
    procedure SetPathState;
    procedure SetNextButtonState;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
    function GetNextButtonCaption : string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameCutOffYear, FrameServerFiles, FrameNewOrExisting, FrameInstallation;

//==============================================================================
{ TfraServerSettings }
//------------------------------------------------------------------------------
constructor TfraServerSettings.Create(AOwner: TComponent);
begin
  inherited;
  FJustCreated := true;
end;

{-------------------------------------------------------------------------------
  Description : Validate the instance name as it is typed to ensure that it is
              valid for SQL Server
  Created : 25/02/2003 }
procedure TfraServerSettings.eInstanceNameCheckText(Sender: TObject;
  const iText: String; var ioAccept: Boolean);
var
  lCharPos : integer;
begin
  inherited;
  // loop through each char checking they are valid
  for lCharPos := 1 to Length(iText) do
    if not(iText[lCharPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      ioAccept := False;
end;

//------------------------------------------------------------------------------
function TfraServerSettings.GetNextFrame: TPageFrameClass;
var lNextFrameOk: Boolean;
begin
  lNextFrameOk := true;
  if cbDataDir.Checked then
    if FJustCreated then  // We want to stop the prompt, unless user clicks "Install"
      FJustCreated := false
    else
      lNextFrameOk:= Settings.FolderExists(eDataDir.Text);

  if lNextFrameOk then
    Result := TfraInstallation
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
function TfraServerSettings.GetPrevFrame: TPageFrameClass;
begin
  if Settings.ServerList.Count > 0 then
    Result := TfraNewOrExisting
  else if Settings.InstallMode = imServer then
    Result := TfraServerFiles
  else
    Result := TfraCutOffYear;
end;

//------------------------------------------------------------------------------
procedure TfraServerSettings.cbInstanceNameClick(Sender: TObject);
begin
  inherited;
  SetInstanceNameState;
  Settings.SpecifyOwnInstanceName := cbInstanceName.Checked;
  SetNextButtonState;
end;

{-------------------------------------------------------------------------------
  Description : Enable/disable the instance name edit box controls according to
              the checkbox state
  Created : 25/02/2003 }
procedure TfraServerSettings.SetInstanceNameState;
begin
  lblName.Enabled := cbInstanceName.Checked;
  eInstanceName.Enabled := cbInstanceName.Checked;
end;

{-------------------------------------------------------------------------------
  Description : Enable/disable the path edit box controls according to the
              checkbox state
  Created : 25/02/2003 }
procedure TfraServerSettings.SetPathState;
begin
  lblPath.Enabled := cbDataDir.Checked;
  eDataDir.Enabled := cbDataDir.Checked;
  btnFolderBrowse.Enabled := cbDataDir.Checked;
  Refresh;
end;

{-------------------------------------------------------------------------------
  Description : Trap the assignment of the settings object to the frame so the
              initial page state can be configured
  Created : 25/02/2003 }
procedure TfraServerSettings.SetSettings(const Value: TSettings);
begin
  inherited;
  if Settings.LocalInstancesPresent then begin
    cbInstanceName.Checked := True;
    cbInstanceName.Enabled := False;
  end else
  // Win98 seems not to always like a named instance without a default one first
  // So, we force a default instance if no local ones exist.
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    cbInstanceName.Visible := false;
    eInstanceName.Visible  := false;
    lblName.Visible        := false;
    cbDataDir.Top          := 116;
    lblPath.Top            := 146;
    eDataDir.Top           := 144;
    btnFolderBrowse.Top    := 144;
    Settings.SpecifyOwnInstanceName := false;
  end else
    cbInstanceName.Checked := Settings.SpecifyOwnInstanceName;

  eInstanceName.Text := Settings.InstanceName;
  cbDataDir.Checked  := Settings.SpecifyOwnDataDir;
  eDataDir.Text      := Settings.DataDir;
  SetInstanceNameState;
end;

//------------------------------------------------------------------------------
procedure TfraServerSettings.eInstanceNameChange(Sender: TObject);
begin
  inherited;
  Settings.InstanceName := eInstanceName.Text;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraServerSettings.eDataDirChange(Sender: TObject);
begin
  inherited;
  Settings.DataDir := eDataDir.Text;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraServerSettings.cbDataDirClick(Sender: TObject);
begin
  inherited;
  SetPathState;
  Settings.SpecifyOwnDataDir := cbDataDir.Checked;
  SetNextButtonState;
end;


//------------------------------------------------------------------------------
procedure TfraServerSettings.btnFolderBrowseClick(Sender: TObject);
begin
  inherited;
  if FolderBrowser.Execute then
    eDataDir.Text := FolderBrowser.Folder;
end;

{-------------------------------------------------------------------------------
  25/02/2003
  Enable or disable the Next button according to whether we have enough info to
  proceed
}
procedure TfraServerSettings.SetNextButtonState;
begin
  if Assigned(NextButton) then begin
    if (cbInstanceName.Checked and
        ((eInstanceName.Text = '') or Settings.ServerIsLocal(eInstanceName.Text))) or
       (cbDataDir.Checked and (eDataDir.Text = '')) then
    begin
      NextButton.Enabled := False;
      FJustCreated       := False;
    end else
      NextButton.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraServerSettings.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
  // set focus only if user input required
  if not NextButton.Enabled then begin
    if eInstanceName.Enabled then
      eInstanceName.SetFocus
    else if eDataDir.Enabled then
      eDataDir.SetFocus
  end;
end;

//------------------------------------------------------------------------------
function TfraServerSettings.GetNextButtonCaption: string;
begin
  Result := ST_INSTALL_CAPTION;
end;

//------------------------------------------------------------------------------
end.
