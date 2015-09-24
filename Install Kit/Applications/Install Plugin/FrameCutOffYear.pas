{-------------------------------------------------------------------------------
  Unit:        FrameCutOffYear.pas

  Defines:     TfraCutOffYear

  Description: Additional information for install.

  Created:     28/02/2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameCutOffYear;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Settings, FrameBase, StdCtrls;

type
  TfraCutOffYear = class(TPageFrame)
    Label4: TLabel;
    lblCutOffYear: TLabel;
    eCutOffYear: TEdit;
    Label1: TLabel;
    procedure eCutOffYearChange(Sender: TObject);
    procedure eCutOffYearKeyPress(Sender: TObject; var Key: Char);
  private
    procedure SetNextButtonState;
    function ValidateCutOffYear: Boolean;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
  end;

//==============================================================================
implementation

uses
  FrameSpatialRef, FrameNewOrExisting, FrameInstallation;

{$R *.dfm}

//==============================================================================
{ TfraCutOffYear }
//------------------------------------------------------------------------------
function TfraCutOffYear.GetNextFrame: TPageFrameClass;
begin
  if Settings.InstallMode = imWorkstation then
    Result := TfraInstallation
  else
    Result := TfraNewOrExisting;
end;

//------------------------------------------------------------------------------
function TfraCutOffYear.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraSpatialRef;
end;

//------------------------------------------------------------------------------
procedure TfraCutOffYear.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraCutOffYear.SetSettings(const Value: TSettings);
begin
  inherited;
  eCutoffYear.Text := IntToStr(Settings.CutOffYear);
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraCutOffYear.eCutOffYearChange(Sender: TObject);
var lDate: TDateTime;
    d, m, y : word;
begin
  inherited;
  if ValidateCutOffYear then begin
    // ensure a 4 digit date
    lDate := StrToDate('1/1/' + eCutOffYear.Text);
    DecodeDate(lDate, y, m, d);
    Settings.CutOffYear := y;
  end;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraCutOffYear.SetNextButtonState;
begin
  if Assigned(NextButton) then
    NextButton.Enabled := ValidateCutOffYear;
end;

//------------------------------------------------------------------------------
function TfraCutOffYear.ValidateCutOffYear: Boolean;
var lNow, lDate: TDateTime;
    i, lYear   : Integer;
begin
  Result := false;
  if eCutOffYear.Text <> '' then begin
    // Check only digits are used
    for i := 1 to Length(eCutOffYear.Text) do
      if not (eCutOffYear.Text[i] in ['0'..'9']) then begin
        Result := false;
        Exit;
      end;

    lNow := Now;
    lYear := StrToInt(eCutOffYear.Text);
    if lYear > 0 then begin
      lDate := StrToDate('1/1/' + eCutOffYear.Text);
      Result := lDate <= lNow;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraCutOffYear.eCutOffYearKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  // Need to include backspace in here too.
  if not (Key in [ #8, '0'..'9']) then
    Key := #0;
end;

//------------------------------------------------------------------------------
end.
