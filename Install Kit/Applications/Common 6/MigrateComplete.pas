{===============================================================================
  Unit:        MigrateComplete

  Defines:     TfraMigrateComplete

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 6 $
    $Date: 11/02/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit MigrateComplete;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraMigrateComplete = class(TBasePage)
    chkLaunch: TCheckBox;
    lblInfoRecords: TLabel;
    lblInformation: TLabel;
    lblInfoLocations: TLabel;
    lblInfoNames: TLabel;
    lblInfoReferences: TLabel;
    lblInfoSurveys: TLabel;
    lblInfoTaxa: TLabel;
    lblInfoBiotopes: TLabel;
    lblBioOccs: TLabel;
    lblLocations: TLabel;
    lblNames: TLabel;
    lblReferences: TLabel;
    lblSurveys: TLabel;
    lblTaxOccs: TLabel;
    lblTotalRecords: TLabel;
    lblUpgradeErrors: TLabel;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetIsFinal: Boolean; override;
    function GetNextCaption: String; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  GeneralFunctions, MigrationSettings, SetupConstants, TextMessages, Settings;

{-==============================================================================
    TfraMigrateComplete
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraMigrateComplete.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraMigrateComplete.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraMigrateComplete.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraMigrateComplete.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraMigrateComplete.GetIsFinal: Boolean;
begin
  Result := True;
end;  // TfraMigrateComplete.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TfraMigrateComplete.GetNextCaption: String;
begin
  Result := ResStr_MainMenuCaption;
end;  // TfraMigrateComplete.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TfraMigrateComplete.GetResourceImage: String;
begin
  Result := ResImg_Completion;
end;  // TfraMigrateComplete.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateComplete.LoadContent;
begin
  inherited;
  with TMigrationSettings(Settings) do begin
    lblUpgradeErrors.Visible := MigrationErrors;
    lblTotalRecords.Caption  := MigratedRecords;
    lblLocations.Caption     := IntToStr(MigratedLocations);
    lblNames.Caption         := IntToStr(MigratedNames);
    lblReferences.Caption    := IntToStr(MigratedReferences);
    lblSurveys.Caption       := IntToStr(MigratedSurveys);
    lblTaxOccs.Caption       := IntToStr(MigratedTaxOccs);
    lblBioOccs.Caption       := IntToStr(MigratedBioOccs);
    chkLaunch.Visible        := (InstallType <> itServer);
  end;
end;  // TfraMigrateComplete.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateComplete.SaveContent;
begin
  inherited;
  if chkLaunch.Checked and chkLaunch.Visible then
    if Settings.InstallType = itWorkstation then
      ShellFile(Settings.RootFolder + STR_RECORDER_SPLASH_EXE)
    else
      ShellFile(Settings.InstallFolder + STR_RECORDER_SPLASH_EXE);
end;  // TfraMigrateComplete.SaveContent

end.


