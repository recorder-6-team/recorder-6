{===============================================================================
  Unit:        SiteInfoPage

  Defines:     TfraSiteInfo

  Description:

  Model:       Server Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 6 $
    $Date: 14/07/09 11:37 $
    $Author: Ericsalmon $

===============================================================================}

unit SiteInfoPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls;

type
  TfraSiteInfo = class(TBasePage)
    btnChange: TButton;
    eSiteID: TEdit;
    eVerificationKey: TEdit;
    lblInformationNewSite: TLabel;
    lblInformation: TLabel;
    lblKey: TLabel;
    lblChangeSiteID: TLabel;
    lblSiteIDExisting: TLabel;
    lblNote: TLabel;
    lblConfirmSiteID: TLabel;
    lblSiteID: TLabel;
    pnlConfirmSiteID: TPanel;
    pnlEnterSiteID: TPanel;
    procedure btnChangeClick(Sender: TObject);
    procedure eSiteIDChange(Sender: TObject);
    procedure eSiteIDKeyPress(Sender: TObject; var Key: Char);
    procedure eVerificationKeyChange(Sender: TObject);
    procedure InfoCheck(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  InstallFolderPage, ServerSelectPage, SetupConstants, Functions;

{-==============================================================================
    TfraSiteInfo
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.btnChangeClick(Sender: TObject);
begin
  inherited;
  pnlEnterSiteID.Visible := True;
  pnlConfirmSiteID.Visible := False;
  ChangedContent;
end;  // TfraSiteInfo.btnChangeClick 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.eSiteIDChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraSiteInfo.eSiteIDChange 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.eSiteIDKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  // Keep #8, backspace allowed.
  if not (Key in [#8, '0'..'9', 'A'..'Z', 'a'..'z']) then
    Key := #0;
end;  // TfraSiteInfo.eSiteIDKeyPress 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.eVerificationKeyChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraSiteInfo.eVerificationKeyChange 

{-------------------------------------------------------------------------------
}
function TfraSiteInfo.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraSiteInfo.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraSiteInfo.GetHasNext: Boolean;
begin
  if pnlEnterSiteID.Visible and
     ((eSiteID.Text = '') or (eVerificationKey.Text = '') or
      (Length(eSiteID.Text) <> 8) or (Length(eVerificationKey.Text) <> 4)) then
    Result := False
  else
    Result := pnlConfirmSiteID.Visible or ValidateSiteID(eSiteID.Text,
        eVerificationKey.Text);
end;  // TfraSiteInfo.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraSiteInfo.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraSiteInfo.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraSiteInfo.GetNext: TBasePageClass;
begin
  Result := TfraServerSelect;
end;  // TfraSiteInfo.GetNext 

{-------------------------------------------------------------------------------
}
function TfraSiteInfo.GetResourceImage: String;
begin
  Result := ResImg_SiteInfo;
end;  // TfraSiteInfo.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.InfoCheck(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraSiteInfo.InfoCheck 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.LoadContent;
begin
  inherited;
  eSiteID.Text := Settings.SiteID;
  eVerificationKey.Text := Settings.VerificationKey;
  lblConfirmSiteID.Caption := Settings.SiteID;
  
  pnlEnterSiteID.Visible   := (Settings.SiteID = '') or not Settings.RegistrySiteID;
  pnlConfirmSiteID.Visible := not pnlEnterSiteID.Visible;
end;  // TfraSiteInfo.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraSiteInfo.SaveContent;
begin
  inherited;
  if pnlEnterSiteID.Visible then begin
    Settings.SiteID := eSiteID.Text;
    Settings.VerificationKey := eVerificationKey.Text;
    Settings.RegistrySiteID := False;
  end;
end;  // TfraSiteInfo.SaveContent 

end.
