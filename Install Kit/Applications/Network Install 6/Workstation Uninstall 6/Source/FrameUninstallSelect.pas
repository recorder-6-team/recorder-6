{===============================================================================
  Unit:        FrameUninstallSelect.pas

  Defines:     TfraUninstallSelect

  Description: Allows user to select which features to remove, beside the
               application files and settings. MSDE and System Components
               options are only displayed if indicator flags are found in the
               install log file.

  Model:       Workstation Uninstall 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/02/09 16:55 $
    $Author: Ericsalmon $

===============================================================================}

unit FrameUninstallSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, StdCtrls;

type
  TfraUninstallSelect = class(TPageFrame)
    chkSysComp: TCheckBox;
    Label1: TLabel;
    Label4: TLabel;
    lblSysCompInfo: TLabel;
    procedure chkSysCompClick(Sender: TObject);
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetNextButton(const Value: TButton); override;
    procedure SetSettings(const Value: TSettings); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  FrameWelcome, FrameUninstall;

{-==============================================================================
    TfraUninstallSelect
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraUninstallSelect.chkSysCompClick(Sender: TObject);
begin
  inherited;
  Settings.RemoveSystemComponents := chkSysComp.Checked;
end;  // TfraUninstallSelect.chkSysCompClick 

{-------------------------------------------------------------------------------
}
function TfraUninstallSelect.GetNextFrame: TPageFrameClass;
begin
  Result := TfraUninstall;
end;  // TfraUninstallSelect.GetNextFrame 

{-------------------------------------------------------------------------------
}
function TfraUninstallSelect.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraWelcome;
end;  // TfraUninstallSelect.GetPrevFrame 

{-------------------------------------------------------------------------------
}
procedure TfraUninstallSelect.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := True;
end;  // TfraUninstallSelect.SetNextButton 

{-------------------------------------------------------------------------------
}
procedure TfraUninstallSelect.SetSettings(const Value: TSettings);
begin
  inherited;
  if Settings.SystemComponentsInstalled then begin
    chkSysComp.Enabled := True;
    chkSysComp.Checked := True;
  end else
    chkSysComp.Enabled := False;
end;  // TfraUninstallSelect.SetSettings 

end.

