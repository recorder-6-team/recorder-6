//==============================================================================
//  Unit:        Cascade
//
//  Implements:  TdlgCascade
//
//  Description:
//
//  Author:      Ricky Shrestha
//  Created:     16 Jan 2008
//
//  Last Revision Details:
//    $Revision: 9 $
//    $Date: 22/04/09 14:39 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit Cascade;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, OnlineHelp;

type
  TCascadeType = (ctNothing, ctCascadeAll, ctCascadeEqual);

  TdlgCascade = class(TForm)
    pnlChoose: TGroupBox;
    rbDoNothing: TRadioButton;
    rbCascadeAll: TRadioButton;
    rbCascadeEqual: TRadioButton;
    ckbUpdateSamples: TCheckBox;
    bbOK: TImageListButton;
    procedure bbOKClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
  private
    FChosenOption: TCascadeType;
  public
    constructor CreateDialog(AOwner:TComponent; const AChangedDetails: string;
      const AForSample, AMultipleSamples: boolean); overload;
    property ChosenOption: TCascadeType read FChosenOption;
  end;

{-------------------------------------------------------------------------------
}
implementation

{$R *.dfm}

uses
  GeneralFunctions;

resourcestring
  ResStr_ChooseOption = 'Please choose one of the option to continue.';
  ResStr_DoNothing    = 'Do Nothing';
  ResStr_UpdateAll    =
      'Update all the child samples and determinations to match the survey event''s updated %s';
  ResStr_UpdateEqual  =
      'Update the child sample and determinations to match the survey event''s '
      + 'updated %s but only if the values previously matched between the sample '
      + 'and survey event or determination';

  ResStr_UpdateAll_ForSample   =
      'Update the parent event and child determinations to match the sample''s '
      + 'updated %s';
  ResStr_UpdateEqual_ForSample =
      'Update the parent event and child determinations to match the sample''s '
      + 'updated %s but only if the values previously matched between the sample '
      + 'and survey event or determination';
  ResStr_CascadeSampleChanges = 'Cascade Sample Changes';

{-------------------------------------------------------------------------------
}
constructor TdlgCascade.CreateDialog(AOwner:TComponent; const AChangedDetails: string;
  const AForSample, AMultipleSamples: boolean);
begin
  inherited Create(AOwner);

  rbDoNothing.Caption := ResStr_DoNothing;
  rbDoNothing.Checked := true;

  if (AForSample) then
  begin
    rbCascadeAll.Caption   := Format(ResStr_UpdateAll_ForSample, [AChangedDetails]);
    rbCascadeEqual.Caption := Format(ResStr_UpdateEqual_ForSample, [AChangedDetails]);
  end else begin
    rbCascadeAll.Caption   := Format(ResStr_UpdateAll, [AChangedDetails]);
    rbCascadeEqual.Caption := Format(ResStr_UpdateEqual, [AChangedDetails]);
  end;
  ckbUpdateSamples.Visible := AForSample and AMultipleSamples;
  if AForSample then
    Caption := ResStr_CascadeSampleChanges;

  Self.HelpContext := IDH_CASCADINGUPDATE;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgCascade.bbOKClick(Sender: TObject);
begin
  if rbDoNothing.Checked then
    FChosenOption := ctNothing
  else if rbCascadeAll.Checked then
    FChosenOption := ctCascadeAll
  else if rbCascadeEqual.Checked then
    FChosenOption := ctCascadeEqual
  else
    ShowInformation(ResStr_ChooseOption);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgCascade.RadioButtonClick(Sender: TObject);
begin
  ckbUpdateSamples.Enabled := not rbDoNothing.Checked;
end;

end.
