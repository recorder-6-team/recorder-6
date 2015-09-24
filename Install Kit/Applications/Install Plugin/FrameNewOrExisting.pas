{-------------------------------------------------------------------------------
  Unit:        FrameNewOrExisting.pas

  Defines:     TfraNewOrExisting

  Description: Additional information for install.

  Created:     February 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameNewOrExisting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, Settings, TextMessages;

type
  TfraNewOrExisting = class(TPageFrame)
    GroupBox1: TGroupBox;
    rbInstallNew: TRadioButton;
    rbInstallExisting: TRadioButton;
    cmbServerName: TComboBox;
    Label4: TLabel;
    procedure rbInstallOptionClick(Sender: TObject);
    procedure cmbServerNameChange(Sender: TObject);
  private
    procedure SetNextButtonState;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value : TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
  end;

//==============================================================================
implementation

uses
  FrameServerFiles, FrameCutOffYear, FrameServerSettings, FrameInstallation,
  FrameInstallFolder;

{$R *.dfm}

//==============================================================================
{ TfraNewOrExisting }
//------------------------------------------------------------------------------
function TfraNewOrExisting.GetNextFrame: TPageFrameClass;
begin
  if rbInstallNew.Checked then
    Result := TfraServerSettings
  else
    Result := TfraInstallation;
end;

//------------------------------------------------------------------------------
function TfraNewOrExisting.GetPrevFrame: TPageFrameClass;
begin
  case Settings.InstallMode of
    imServer  : Result := TfraServerFiles;
    imUpgrade : Result := TfraInstallFolder;
  else
    Result := TfraCutOffYear;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraNewOrExisting.rbInstallOptionClick(Sender: TObject);
begin
  inherited;
  cmbServerName.Enabled := rbInstallExisting.Checked;
  Settings.NewServer := rbInstallNew.Checked;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraNewOrExisting.SetSettings(const Value: TSettings);
begin
  inherited;
  Settings.GetLocalInstanceNames;

  cmbServerName.Items.Assign(Settings.ServerList);
  if Settings.NewServer then
    rbInstallNew.Checked := True
  else
    rbInstallExisting.Checked := True;
  cmbServerName.ItemIndex := cmbServerName.Items.IndexOf(Settings.ServerName);
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraNewOrExisting.cmbServerNameChange(Sender: TObject);
begin
  inherited;
  Settings.ServerName := cmbServerName.Text;
  SetNextButtonState;
end;

{-------------------------------------------------------------------------------
  Description : Trap the assignment of the next button so we can set its initial
              enabled state correctly
  Created : 25/02/2003 }
procedure TfraNewOrExisting.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
end;

{-------------------------------------------------------------------------------
  Description : Set the state of the next button to disabled if we don't have
              enough information to proceed
  Created : 25/02/2003 }
procedure TfraNewOrExisting.SetNextButtonState;
begin
  if Assigned(NextButton) then begin
    NextButton.Enabled := not (rbInstallExisting.Checked and
                               (cmbServerName.Text = ''));
    // By default
    NextButton.Caption := ST_NEXT_CAPTION;
    // Set caption to "Install" if using existing server
    if (rbInstallExisting.Checked) and (cmbServerName.Text <> '') then
      NextButton.Caption := ST_INSTALL_CAPTION;
  end;
end;

//------------------------------------------------------------------------------
end.
