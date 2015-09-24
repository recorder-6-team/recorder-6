{-------------------------------------------------------------------------------
  Unit:        FrameUninstallSelect.pas

  Defines:     TfraUninstallSelect

  Description: Allows user to select which features to remove, beside the
               application files and settings. MSDE and System Components
               options are only displayed if indicator flags are found in the
               install log file.

  Created:     April 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/07/09 9:05 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameUninstallSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, StdCtrls;

type
  TfraUninstallSelect = class(TPageFrame)
    lblTitle: TLabel;
    chkSQLInstance: TCheckBox;
    lblInformation: TLabel;
    procedure chkSQLInstanceClick(Sender: TObject);
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetNextButton(const Value: TButton); override;
    procedure SetSettings(const Value: TSettings); override;
  end;

//------------------------------------------------------------------------------
implementation

{$R *.dfm}

uses
  FrameWelcome, FrameUninstall;

//------------------------------------------------------------------------------
{ TfraUninstallSelect }

function TfraUninstallSelect.GetNextFrame: TPageFrameClass;
begin
  Result := TfraUninstall;
end;

//------------------------------------------------------------------------------
function TfraUninstallSelect.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraWelcome;
end;

//------------------------------------------------------------------------------
procedure TfraUninstallSelect.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := True;
end;

//------------------------------------------------------------------------------
procedure TfraUninstallSelect.SetSettings(const Value: TSettings);
begin
  inherited;
  if Settings.SQLExpressInstallState = isInstalled then begin
    if Settings.InstalledInstanceName = '' then
      chkSQLInstance.Caption := 'SQL Express Instance (local).'
    else
      chkSQLInstance.Caption := 'SQL Express Instance (' + Settings.InstalledInstanceName + ').';
    chkSQLInstance.Enabled := True;
    chkSQLInstance.Checked := True;
  end else
    chkSQLInstance.Enabled := False;
end;

//------------------------------------------------------------------------------
procedure TfraUninstallSelect.chkSQLInstanceClick(Sender: TObject);
begin
  inherited;
  if chkSQLInstance.Checked then Settings.SQLExpressInstallState := isInstalled
                            else Settings.SQLExpressInstallState := isNotInstalled;
end;

end.
