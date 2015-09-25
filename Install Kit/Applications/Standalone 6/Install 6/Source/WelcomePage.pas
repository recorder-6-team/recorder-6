{===============================================================================
  Unit:        IntroductionPage

  Defines:     TfraIntroduction

  Description: Presents the options available to the user for the standalone
               install of Recorder 6

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 5 $
    $Date: 3/07/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit WelcomePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, ExtCtrls, Htmlview;

resourcestring
  ResStr_WelcomeText =
      '<HTML><BODY>'
      + '<P>Welcome to the Recorder 6 setup program. This programme will install'
      +   ' Recorder 6 onto your machine. </P>'
      + '<P>It is strongly recommended that you exit all Windows programs before'
      +   ' running the setup.</P>'
      + '<P>Recorder 6 has the following system requirements:'
      + '<UL>'
      +   '<LI>Internet Explorer version 5 or above.<BR><BR></LI>'
      +   '<LI>512MB RAM on Windows Vista or Windows 7.<BR><BR></LI>'
      +   '<LI>128MB RAM on Windows XP.<BR><BR></LI>'
      +   '<LI>64MB RAM on Windows 2000.<BR><BR></LI>'  
      + '</UL>'
      + '<P>To install the programme you will also need Administrative rights on'
      +   ' this machine.</P>'
      + '<P>If you do not already have Recorder 2002 installed on this machine, you'
      +   ' will need your SiteID and verification key.  These are shown on your'
      +   ' licence agreement.</P>'
      + '<P><I>Click Next to continue...</I></P>'
      + '</BODY></HTML>';

type
  TfraWelcome = class (TBasePage)
    hvInfo: THTMLViewer;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  InstallFolderPage, SetupConstants;

{-==============================================================================
    TfraWelcome
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraWelcome.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraWelcome.GetConfirmCancel

{-------------------------------------------------------------------------------
}
function TfraWelcome.GetHasNext: Boolean;
begin
  Result := True;
end;  // TfraWelcome.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraWelcome.GetHasPrevious: Boolean;
begin
  Result := Settings.FoldersCreated.Count = 0;
end;  // TfraWelcome.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraWelcome.GetNext: TBasePageClass;
begin
  Result := TfraInstallFolder;
end;  // TfraWelcome.GetNext 

{-------------------------------------------------------------------------------
}
function TfraWelcome.GetResourceImage: String;
begin
  Result := ResImg_Welcome;
end;  // TfraWelcome.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TfraWelcome.LoadContent;
begin
  hvInfo.LoadFromString(ResStr_WelcomeText);
end;  // TfraWelcome.LoadContent

end.
