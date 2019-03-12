unit LicenceKeyGenerator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TLicenceKey = class(TForm)
    edYear: TEdit;
    Label1: TLabel;
    LicenceKey: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    function GetFinancialYear : integer;
    function LicenceScramble: String;

  public

  end;

var
  LicenceKey: TLicenceKey;

implementation

function TLicenceKey.LicenceScramble: String;
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
	lString  := 'DINTYGRID';
  lFinancialYear := strtoint(edYear.text) - 1700;
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

{$R *.dfm}
function TLicenceKey.GetFinancialYear : integer;
var
  myDate : TDateTime;
  myYear, myMonth, myDay : Word;
begin
  // Set up the myDate to be now
  myDate := date;
  DecodeDate(myDate, myYear, myMonth, myDay);
  if MyMonth < 4  then
     MyYear := MyYear -1 ;
  Result := MyYear - 1700;

end;
procedure TLicenceKey.Button1Click(Sender: TObject);
begin
  LicenceKey.caption := LicenceScramble;
end;

end.
