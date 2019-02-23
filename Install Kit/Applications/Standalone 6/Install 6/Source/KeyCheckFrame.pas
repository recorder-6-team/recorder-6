unit KeyCheckFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BaseFrameUnit,Settings, UpgradeFrame,strutils;

resourcestring
  ResStr_LengthWrong = 'The Licence key is 8 characters upper case alpha numeric. ';
  ResStr_InvalidKey = 'The Licence key entered is not valid for this upgrade. ';
  ResStr_InvalidLicenceFile = 'Licence.txt is corrupt.';
  ResStr_Key_Required = 'A licence key is required to install this upgrade';

const
  Contained = 40;

type
  TfraKeyCheck = class(TBaseFrame)
    Panel1: TPanel;
    edLicenceKey: TEdit;
    edVersion: TEdit;
    procedure edVersionClick(Sender: TObject);
    procedure edLicenceKeyChange(Sender: TObject);
  private
    { Private declarations }
    function Scramble(const ACode: String): String;
    function CharCheck(const ACode: String): Boolean;
    procedure InternalValidate(ASettings: TSettings; var ACanProceed: boolean);
    function GetSeed : integer;
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

function TfraKeyCheck.Scramble(const ACode: String): String;
var
	i: Integer;
	lTotal: Int64;
	lString: String;
	lSpare: String;
begin
	lTotal := 0;
	Result := '0000';
	// Function is not case sensitive.
	lString  := UpperCase(ACode);
	// Stores the built-in random number generator's seed.
  // seed comes from the version file
	Randseed := GetSeed;

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
end;  // Scramble

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
licenceFile: TStringList;
containedIn: string;
lscrambled : string;
begin
  ACanProceed := false;
  try
  licenceFile := TStringList.Create;
    if CharCheck(edLicenceKey.text) then begin
      if FileExists(ExtractFilePath(Application.ExeName) + 'LicenceKey.txt') then begin
        licenceFile.LoadFromFile(ExtractFilePath(Application.ExeName) + 'LicenceKey.txt');
        containedIn := licenceFile[0];
        if length(Containedin) = Contained then begin
          lScrambled := scramble(leftStr(edLicenceKey.Text,8));
          if pos(lScrambled,containedIn) > 0 then begin
            ACanProceed := true;
          end else
            MessageDlg(ResStr_InvalidKey, mtInformation, [mbOk], 0);
        end else
            MessageDlg(ResStr_InvalidLicenceFile, mtInformation, [mbOk], 0);
        end;
    end else
      MessageDlg(ResStr_LengthWrong, mtInformation, [mbOk], 0);
  finally
    licenceFile.free;
  end;
end;

procedure TfraKeyCheck.edVersionClick(Sender: TObject);
begin
  inherited;
  if edVersion.Text = 'DINTY' then
    edVersion.Text := scramble(edLicenceKey.text);

end;

procedure TfraKeyCheck.edLicenceKeyChange(Sender: TObject);
begin
  inherited;
  edVersion.text := ResStr_Key_Required;
end;

function  TfraKeyCheck.GetSeed : integer;
var versionFile: TStringList;
begin
  Result := 7067;
  versionFile := TStringList.Create;
  if FileExists(ExtractFilePath(Application.ExeName) + 'VersionControl.txt') then begin
    versionFile.LoadFromFile(ExtractFilePath(Application.ExeName) + 'VersionControl.txt');
    Result := strtoint(versionFile[2]);
  end;
  versionFile.free;
end;

end.
