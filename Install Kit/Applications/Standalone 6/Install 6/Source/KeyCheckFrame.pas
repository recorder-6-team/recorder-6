unit KeyCheckFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseFrameUnit,Settings, UpgradeFrame,strutils;

resourcestring
  ResStr_FormatWrong = 'The Licence key is not in the correct format. ';
  ResStr_InvalidKey = 'The Licence key entered is not valid for this upgrade. ';
  ResStr_Key_Required = 'A licence key is required to install this upgrade';
  ResStr_Licence_Word = 'DINTYGRID';

type
  TfraKeyCheck = class(TBaseFrame)
    Panel1: TPanel;
    edLicenceKey: TEdit;
    edVersion: TEdit;
 private
    { Private declarations }
    function LicenceScramble: String;
    function GetFinancialYear : integer;
    function CharCheck(const ACode: String): Boolean;
    procedure InternalValidate(ASettings: TSettings; var ACanProceed: boolean);

 public
    { Public declarations }
    function CreateNextFrame(AOwner: TComponent): TBaseFrame; override;
    procedure Execute(ASettings: TSettings); override;
    procedure Validate(ASettings: TSettings; var ACanProceed: boolean); override;
  end;

implementation

{$R *.dfm}
 {-------------------------------------------------------------------------------
}
function TfraKeyCheck.CreateNextFrame(AOwner: TComponent): TBaseFrame;
begin
  Result := TfraUpgrade.Create(AOwner);
end;

function TfraKeyCheck.LicenceScramble: String;
var
	i: Integer;
	lTotal: Int64;
	lString: String;
	lSpare: String;
  lFinancialYear : integer;
begin
	lTotal := 0;
	Result := '0000';
	// Function is not case sensitive.
	lString  := ResStr_Licence_Word;
  lFinancialYear := GetFinancialYear;
  // Get the FinancialYear
  //Not using a Random number so that algorithm does not depend on delphi
	//  Creates an integer total from the sum of (the ASCII values of each character
	//  in the Key Sting multiplied by the adjusted financial year
  for i := 1 to Length(lString) do
		lTotal := lTotal + (lFinancialYear * Ord(lString[i]));

	// if lTotal is greater than FFFF we want to use the integer remainder of 1Total/4093
	if lTotal > 65535 then
		lTotal := lTotal mod 4093;

	//Convert to Hexadeciamal
	lSpare := IntToHex(lTotal, 4);
	// Swaps the order of the characters round in lSpare
	for i := 1 to 4 do
		Result[i] := lSpare[5 - i];

end;  // LicenceScramble


function TfraKeyCheck.GetFinancialYear : integer;
var
  myDate : TDateTime;
  myYear, myMonth, myDay : Word;
begin
  // Set up the myDate to be now
  myDate := date;
  DecodeDate(myDate, myYear, myMonth, myDay);
  if MyMonth < 4 then
     MyYear := MyYear - 1 ;
  Result := MyYear - 1700;

end;

function TfraKeyCheck.CharCheck(const ACode: String): Boolean;
var
	i: Integer;
begin
    Result := true;
	  if Length(ACode) <> 8 then Result:= false;
    for i := 1 to length(Acode) do
		  if NOT (ACode[i] in ['0'..'9', 'A'..'Z']) then begin
		    Result := false;
		    Exit;
		  end;

end;  // CharCheck

procedure TfraKeyCheck.Execute(ASettings: TSettings);
begin
  edVersion.Text := ResStr_Key_Required;
  edLicencekey.Text := '';
  edLicencekey.SetFocus;
  // does nothing
end;

procedure TfraKeyCheck.Validate(ASettings: TSettings; var ACanProceed: boolean);
begin
  InternalValidate(ASettings, ACanProceed);
end;

procedure TfraKeyCheck.InternalValidate(ASettings: TSettings; var ACanProceed: boolean);
var
lscrambled : string;
begin
  ACanProceed := false;
  if CharCheck(edLicenceKey.text) then begin
    lScrambled := LicenceScramble;
    showmessage (lScrambled);
      if (pos(lScrambled,edLicenceKey.Text) > 0) OR (edLicenceKey.text  = ResStr_Licence_Word) then begin
        ACanProceed := true;
      end else
        MessageDlg(ResStr_InvalidKey, mtInformation, [mbOk], 0);
  end else
      MessageDlg(ResStr_FormatWrong, mtInformation, [mbOk], 0);

end;



end.
