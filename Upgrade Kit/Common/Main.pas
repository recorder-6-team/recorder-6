{===============================================================================
  Unit:         Main

  Defines:      TfrmMain

  Description:  Progress splash screen for Recorder database upgrades.

  Created:      29/5/2003

  Last revision information:
    $Revision: 11 $
    $Date: 6/07/09 10:14 $
    $Author: Ericsalmon $

===============================================================================}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Settings, LoginFrame, Buttons, BaseFrameUnit,
  GeneralFunctions, ExtCtrls, Functions, XPMenu,KeyCheckFrame;

resourcestring
  ResStr_RecorderUpdated =
      'Recorder upgrade complete.';

  ResStr_RecorderUpdatedNoDBUpdate =
      'Recorder upgrade complete.  No database updates were required.';

type
  {-----------------------------------------------------------------------------
    Upgrader main form.
    Each step of the process is displayed by embedding a frame onto the dialog.
  }
  TfrmMain = class (TForm)
    btnCancel: TBitBtn;
    btnProceed: TBitBtn;
    imgBackdrop: TImage;
    procedure btnCancelClick(Sender: TObject);
    procedure btnProceedClick(Sender: TObject);
  private
    FCurrentFrame: TBaseFrame;
    FSettings: TSettings;
    FXPMenu: TXPMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EmbedCurrentFrame;
    procedure SetApplicationTitle(const title: String);
    property Settings: TSettings read FSettings;
  end;

var
  frmMain: TfrmMain;

//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
  Initialises the form.  Embeds the first frame onto the form (the licensing frame).
}
constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSettings     := TSettings.Create;
  FCurrentFrame := TfraLogin.Create(Self);
  LoadJPEGFromRes('Backdrop', imgBackDrop.Picture);

  FXPMenu := TXPMenu.Create(Self);
  FXPMenu.XPControls := FXPMenu.XPControls - [xcCombo, xcGroupBox];
  FXPMenu.Active := True;
end;  // TfrmMain.Create

{-------------------------------------------------------------------------------
  Frees owned objects. 
}
destructor TfrmMain.Destroy;
begin
  FSettings.Free;
  
  inherited Destroy;
end;  // TfrmMain.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;  // TfrmMain.btnCancelClick 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.btnProceedClick(Sender: TObject);
var
  lLastFrame: TBaseFrame;
  canProceed: boolean;
begin
  canProceed := true;
  FCurrentFrame.Validate(FSettings, canProceed);
  if canProceed then begin
    btnProceed.Enabled := False;
    btnCancel.Enabled  := False;
    // apply the settings and move to the next frame
    FCurrentFrame.ApplySettings(FSettings);
    lLastFrame    := FCurrentFrame;
    FCurrentFrame := lLastFrame.CreateNextFrame(Self);
    lLastFrame.Free;
    if Assigned(FCurrentFrame) then
      try
        EmbedCurrentFrame;
        btnProceed.Enabled := true;
        btnCancel.Enabled  := true;
      except
        on EAbort do Close;
      end // try
    else begin
      if Settings.DBUpdated then
        ShowInformation(ResStr_RecorderUpdated)
      else
        ShowInformation(ResStr_RecorderUpdatedNoDBUpdate);
      Close;
    end;
  end;
end;  // TfrmMain.btnProceedClick

{-------------------------------------------------------------------------------
  Embeds the current frame onto the form and calls its execute method. 
}
procedure TfrmMain.EmbedCurrentFrame;
begin
  with FCurrentFrame do begin
    Parent := Self;
    SetBounds(168, 0, 328, 312);
    Execute(FSettings);
  end;
end;  // TfrmMain.EmbedCurrentFrame 

{-------------------------------------------------------------------------------
  Allow for internationalisation of the application title. Set the form caption too.
}
procedure TfrmMain.SetApplicationTitle(const title: String);
begin
  Application.Title := title;
  Caption           := title;
end;

end.

