{-------------------------------------------------------------------------------
  Unit:        FrameBase.pas

  Defines:     TPageFrame        Base class for all frames in application.
               TPageFrameClass   

  Description: Base class for the wizard's pages. Implements basic and common
               functionality.

  Created:     February 2003

  Last revision information:
    $Revision: 6 $
    $Date: 27/04/09 9:38 $
    $Author: Simonwood $

-------------------------------------------------------------------------------}

unit FrameBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Settings, StdCtrls, TextMessages;

const
  IID_EXECUTEPAGE: TGUID = '{A4E49FDD-E2AA-4311-9676-D49D6EE045F6}';

type
  TPageFrameClass = class of TPageFrame;

  TPageFrame = class(TFrame)
  private
    FSettings: TSettings;
    FPrevButton: TButton;
    FNextButton: TButton;
    FCancelled: Boolean;
  protected
    function GetIsFinal: Boolean; virtual;
    function GetNextButtonCaption: string; virtual;
    function GetNextFrame: TPageFrameClass; virtual;
    function GetPrevFrame: TPageFrameClass; virtual;
    procedure SetSettings(const Value: TSettings); virtual;
    procedure SetNextButton(const Value: TButton); virtual;
    procedure SetPrevButton(const Value: TButton); virtual;
    procedure CancelFrame; virtual;
    procedure DoPreProcess; virtual;
    procedure DoPostProcess; virtual;
  public
    procedure Cancel;
    procedure PreProcess;
    procedure PostProcess;
    property Settings : TSettings read FSettings write SetSettings;
    property NextButton : TButton read FNextButton write SetNextButton;
    property PrevButton : TButton read FPrevButton write SetPrevButton;
    property Cancelled: Boolean read FCancelled;
    property NextFrame: TPageFrameClass read GetNextFrame;
    property PrevFrame: TPageFrameClass read GetPrevFrame;
    property NextButtonCaption: string read GetNextButtonCaption;
    property IsFinal: Boolean read GetIsFinal;
  end;

  { Pages must implement this if they run a task as soon as they are shown }
  IExecutePage = interface
    ['{A4E49FDD-E2AA-4311-9676-D49D6EE045F6}']
    procedure Execute;
  end;

//------------------------------------------------------------------------------
implementation

{$R *.dfm}

//------------------------------------------------------------------------------
{ TPageFrame }

procedure TPageFrame.Cancel;
begin
  FCancelled := True;
  CancelFrame;
end;

//------------------------------------------------------------------------------
procedure TPageFrame.CancelFrame;
begin
  // Default behaviour when cancelling, skip to next frame
  if Assigned(NextButton) then begin
    Forms.Application.ProcessMessages;
    NextButton.Enabled := true;
    NextButton.Click;
  end;
end;

//------------------------------------------------------------------------------
procedure TPageFrame.DoPostProcess;
begin
  // Override when needed.
end;

procedure TPageFrame.DoPreProcess;
begin
  // Override when needed.
end;

function TPageFrame.GetIsFinal: Boolean;
begin
  Result := False;
end;

function TPageFrame.GetNextButtonCaption: string;
begin
  Result := ResStr_NextCaption;
end;

function TPageFrame.GetNextFrame: TPageFrameClass;
begin
  Result := nil;
end;

function TPageFrame.GetPrevFrame: TPageFrameClass;
begin
  Result := nil;
end;

procedure TPageFrame.PostProcess;
begin
  DoPostProcess;
end;

procedure TPageFrame.PreProcess;
begin
  DoPreProcess;
end;

procedure TPageFrame.SetNextButton(const Value: TButton);
begin
  FNextButton := Value;
end;

procedure TPageFrame.SetPrevButton(const Value: TButton);
begin
  FPrevButton := Value;
end;

procedure TPageFrame.SetSettings(const Value: TSettings);
begin
  FSettings := Value;
end;

//------------------------------------------------------------------------------
end.
