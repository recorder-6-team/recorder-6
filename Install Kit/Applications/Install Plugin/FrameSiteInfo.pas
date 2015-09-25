{-------------------------------------------------------------------------------
  Unit:        FrameSiteInfo.pas

  Defines:     TfraSiteInfo

  Description: Additional information for install. 

  Created:     February 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameSiteInfo;

interface
                                    
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, StdCtrls, DB, ADODB, ComObj;

type
  TfraSiteInfo = class(TPageFrame)
    Label2: TLabel;
    Label1: TLabel;
    lblSiteID: TLabel;
    eSiteID: TEdit;
    Label5: TLabel;
    eVerificationKey: TEdit;
    Label4: TLabel;
    procedure InfoCheck(Sender: TObject);
    procedure eSiteIDKeyPress(Sender: TObject; var Key: Char);
    procedure eSiteIDChange(Sender: TObject);
    procedure eVerificationKeyChange(Sender: TObject);
  private
    procedure SetNextButtonState;
    function CharCheck(const ACode: String): Boolean;
    function Scramble(const ACode: String): String;
    function ValidateSiteID: boolean;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
  public
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameInstallFolder, FrameServerFiles, FrameSpatialRef;

//==============================================================================
{ TfraSiteInfo }
//------------------------------------------------------------------------------
function TfraSiteInfo.GetNextFrame: TPageFrameClass;
begin
  if Settings.InstallMode = imServer then
    Result := TfraServerFiles
  else
    Result := TfraSpatialRef;
end;

//------------------------------------------------------------------------------
function TfraSiteInfo.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraInstallFolder;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.SetSettings(const Value: TSettings);
begin
  inherited;
  eSiteID.Text          := Settings.SiteID;
  eVerificationKey.Text := Settings.VerificationKey;
  SetNextButtonState;
  eSiteID.SetFocus;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.SetNextButtonState;
begin
  if Assigned(NextButton) then
    if (eSiteID.Text = '') or (eVerificationKey.Text = '') or
       (Length(eSiteID.Text) <> 8) or (Length(eVerificationKey.Text) <> 4) then
      NextButton.Enabled := false
    else
      NextButton.Enabled := ValidateSiteID;
end;

{-------------------------------------------------------------------------------
  Event triggered when text changes on both edit boxes, and when leaving either
  of them.
}
procedure TfraSiteInfo.InfoCheck(Sender: TObject);
begin
  inherited;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.eSiteIDChange(Sender: TObject);
begin
  inherited;
  Settings.SiteID := eSiteID.Text;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.eVerificationKeyChange(Sender: TObject);
begin
  inherited;
  Settings.VerificationKey := eVerificationKey.Text;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSiteInfo.eSiteIDKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  // Keep #8, backspace allowed.
  if not (Key in [#8, '0'..'9', 'A'..'Z', 'a'..'z']) then
    Key := #0;
end;

//------------------------------------------------------------------------------
{ Returns true if the site id validates against the verification key }
function TfraSiteInfo.ValidateSiteID: boolean;
begin
  Result := false;
  // Checks the site id only contains numbers and letters.
  if CharCheck(eSiteID.Text) then
    //Checks that the Key Code entered is the correct on for the Entered Key.
    Result := Scramble(eSiteID.Text) = UpperCase(eVerificationKey.Text);
end;

//------------------------------------------------------------------------------
// Function checks to make sure that the input Key is 8 characters long and
// only contains integers and letters
function TfraSiteInfo.CharCheck(const ACode: String): Boolean;
var i: Integer;
begin
  Result := true;
  if Length(ACode) <> 8 then Result := false;

  for i := 1 to Length(ACode) do
    if not (ACode[i] in ['0'..'9', 'a'..'z', 'A'..'Z']) then begin
      Result := false;
      Exit;
    end;
end;

//------------------------------------------------------------------------------
// Function to Calclate a Key Code for a Key.
function TfraSiteInfo.Scramble(const ACode: String): String;
var
  i : Integer;
  lTotal : Int64;  // Changed from Cardinal type to remove compiler warning
  lString : String;
  lSpare : String;
begin
  lTotal := 0;
  Result := '0000';
  // Function is not case sensitive.
  lString  := UpperCase(ACode);
  // Stores the built-in random number generator's seed.
  Randseed := 6527;

  //  Creates an integer total from the sum of (the ASCII values of each character
  //  in the Key Sting multiplied by a random umber in the ranger 0 to 501)
  for i := 1 to Length(lString) do
    lTotal := lTotal + ((Random(500) + 1) * Ord(lString[i]));

  // if lTotal is greater than FFFF we want to use the integer remainder of 1Total/4093
  if lTotal > 65535 then
    lTotal := lTotal mod 4093;

  //Convert to Hexadeciamal
  lSpare := IntToHex(lTotal, 4);
  // Swaps the order of the characters round in lSpare
  for i := 1 to 4 do
    Result[i] := lSpare[5 - i];
end;

//------------------------------------------------------------------------------
end.
