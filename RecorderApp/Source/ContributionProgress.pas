//==============================================================================
//  Unit:        ContributionProgress
//
//  Implements:  TfrmContributionProgress
//
//  Description:
//
//  Author:      John van Breda
//  Created:     27 Feb 2002
//
//  Changes:     Eric Salmon 13/06/02
//               Database export.
//
//  Last Revision Details:
//    $Revision: 5 $
//    $Date: 12/12/07 13:23 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ContributionProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DataOutput, DatabaseOutput, ExtCtrls;

type
  TfrmContributionProgress = class(TForm)
    lblWait: TLabel;
    btnCancel: TBitBtn;
    lblStage: TLabel;
    Image1: TImage;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDataOutput: TDataOutput;
    FCancelled: boolean;
    FDBOutput: TDatabaseOutput;   // store this so we can cancel it
    procedure SetScheme(const Value: string);
    procedure SetStage(const Value: string);
    procedure SetDataOutput(const Value: TDataOutput);
    procedure SetDBOutput(const Value: TDatabaseOutput);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    property Stage : string write SetStage;
    property Scheme : string write SetScheme;
    property DataOutput : TDataOutput read FDataOutput write SetDataOutput;
    property DatabaseOutput: TDatabaseOutput read FDBOutput write SetDBOutput;
    property Cancelled : boolean read FCancelled;
  end;

var
  frmContributionProgress: TfrmContributionProgress;

//==============================================================================
implementation

resourcestring
  ResStr_Wait = 'Please wait, preparing contribution to %s';

{$R *.DFM}

{ TfrmContributionProgress }

//==============================================================================
constructor TfrmContributionProgress.Create(AOwner: TComponent);
begin
  inherited;
  FCancelled := False;
end;

//==============================================================================
{ On close, just hide the form.  Cancel the data output control if it is
    running }
procedure TfrmContributionProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

//==============================================================================
procedure TfrmContributionProgress.SetDataOutput(const Value: TDataOutput);
begin
  FDataOutput := Value;
end;

//==============================================================================
procedure TfrmContributionProgress.SetDBOutput(const Value: TDatabaseOutput);
begin
  FDBOutput := Value;
end;

//==============================================================================
procedure TfrmContributionProgress.SetScheme(const Value: string);
begin
  lblWait.Caption := Format(ResStr_Wait, [Value]);
end;

//==============================================================================
procedure TfrmContributionProgress.SetStage(const Value: string);
begin
  lblStage.Caption := Value;
end;

//==============================================================================
{ Click Cancel also cancels data output object }
procedure TfrmContributionProgress.btnCancelClick(Sender: TObject);
begin
  FCancelled := True;
  if Assigned(DataOutput) then
    DataOutput.Cancelled := true;
  if Assigned(DatabaseOutput) then
    DatabaseOutput.Cancelled := true;
end;

//==============================================================================
end.
