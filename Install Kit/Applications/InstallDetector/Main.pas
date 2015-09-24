{-------------------------------------------------------------------------------
  Unit: Main

  Implements:   TfrmMain

  Description: Main form for Installation Detector.  Allows the user to select
               a network or standalone install.

  Author:      John van Breda
  Created:     11/06/2002

  Last Revision Details:
    $Revision: 4 $
    $Date: 24/04/03 12:23 $
    $Author: Ericsalmon $

  $History: main.pas $
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 24/04/03   Time: 12:23
//  Updated in $/JNCC/Development/Install Kit/Applications/InstallDetector

//  *****************  Version 3  *****************
//  User: Johnvanbreda Date: 7/02/03    Time: 9:07
//  Updated in $/JNCC/Development/Install Kit/Applications/InstallDetector
//  Image loads from disk file
//
//  *****************  Version 2  *****************
//  User: Johnvanbreda Date: 3/01/03    Time: 13:54
//  Updated in $/JNCC/Development/Install Kit/Applications/InstallDetector
//  Local admin rights required to install on local machine
//
//  *****************  Version 1  *****************
//  User: Johnvanbreda Date: 12/06/02   Time: 2:07p
//  Created in $/JNCC/Installation Kit/InstallDetector
//  First version of InstallDetector tool

-------------------------------------------------------------------------------}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, StdCtrls, ExtCtrls, HotLabel, EasyShell, APIUtils,
  GeneralFunctions;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    imgLogo: TImage;
    lblNetwork: THotLabel;
    lblStandalone: THotLabel;
    lblGettingStarted: THotLabel;
    lblBrowse: THotLabel;
    lblExit: THotLabel;
    procedure lblExitClick(Sender: TObject);
    procedure lblStandaloneMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblStandaloneClick(Sender: TObject);
    procedure lblNetworkClick(Sender: TObject);
    procedure lblBrowseClick(Sender: TObject);
    procedure lblGettingStartedClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.DFM}

//------------------------------------------------------------------------------
constructor TfrmMain.Create(AOwner: TComponent);
var lFileName: String;
begin
  inherited;
  if not IsAdmin then
    lblStandalone.HotColor := clBtnFace;
  lFileName := ExtractFilePath(Application.ExeName) + 'System\InstallDetectorLogo.bmp';
  if FileExists(lFileName) then
    imgLogo.Picture.LoadFromFile(lFileName);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.lblExitClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.lblStandaloneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TLabel(Sender).Font.Color := clMaroon;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.imgLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblStandalone.Font.Color := clBlack;
  lblNetwork.Font.Color := clBlack;
  lblGettingStarted.Font.Color := clBlack;
  lblBrowse.Font.Color := clBlack;
  lblExit.Font.Color := clBlack;
end;

//------------------------------------------------------------------------------
{ Call the standalone install kit when the label is clicked }
procedure TfrmMain.lblStandaloneClick(Sender: TObject);
begin
  if IsAdmin then begin
    WinExec32('"' + ExtractFilePath(Application.ExeName) + 'System\InstallPlugin.exe" /standalone',
              ExtractFilePath(Application.ExeName) + 'System\', SW_SHOWNORMAL);
    Close;
  end else
    MessageDlg('You cannot install Recorder on this machine as you are not an ' +
               'administrator of this machine.  Please ask your network administrator '+
               'for assistance.', mtWarning, [mbOk], 0);
end;

//------------------------------------------------------------------------------
{ Call the server install kit when the label is clicked }
procedure TfrmMain.lblNetworkClick(Sender: TObject);
begin
  WinExec32('"' + ExtractFilePath(Application.ExeName) + 'System\InstallPlugin.exe" /server',
            ExtractFilePath(Application.ExeName) + 'System\', SW_SHOWNORMAL);
  Close;
end;

//------------------------------------------------------------------------------
{ Allow the user to browser the CD }
procedure TfrmMain.lblBrowseClick(Sender: TObject);
begin
  ShellFile(ExtractFilePath(Application.ExeName));
end;

//------------------------------------------------------------------------------
{ Call the getting started guide when the label is clicked }
procedure TfrmMain.lblGettingStartedClick(Sender: TObject);
begin
  ShellFile(ExtractFilePath(Application.ExeName) + 'Getting Started\Recorder Getting Started Guide.chm');
end;

//------------------------------------------------------------------------------
end.

