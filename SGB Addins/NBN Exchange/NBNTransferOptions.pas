//------------------------------------------------------------------------------
// Stuart Ball, JNCC
// July 2007
//
// Option form. Prompts the user what to do with confidential and zero
// abundance records. Allows user to select which fields to export.
//
// The main portion of the code determined whether or not the user has the
// necessary authority to decide whether to export confidential records.
// If they don't, the option to choose is disabled and confidential records
// are NOT exported.
//------------------------------------------------------------------------------
unit NBNTransferOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Registry, ADODB, Recorder2000_TLB, ComObj,
  CheckLst;

type
  TformOptions = class(TForm)
    cbZeroAbundance: TCheckBox;
    cbConfidential: TCheckBox;
    cbRecorders: TCheckBox;
    cbDeterminer: TCheckBox;
    cbSampleType: TCheckBox;
    bOK: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    bCancel: TButton;
    clbMeasurements: TCheckListBox;
    Label1: TLabel;
    cbComment: TCheckBox;
    cbSubstrate: TCheckBox;
    rgSiteName: TRadioGroup;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    function Get_Confidential: boolean;
    function Get_DetNames: boolean;
    function Get_Recorders: boolean;
    function Get_SampleType: boolean;
    function Get_ZeroAbundance: boolean;
    function GetMeasuremetType(Index: integer): boolean;
    function GetMeasuremetLabel(Index: integer): ansistring;
    function GetMeasurementTypeCount: integer;
    function Get_Comment: boolean;
    function Get_Substrate: boolean;
    function GetSampleLocationNameFirst: boolean;
  public
    { Public declarations }
    property ZeroAbundance: boolean read Get_ZeroAbundance;
    property Confidential: boolean read Get_Confidential;
    property RecorderNames: boolean read Get_Recorders;
    property DetNames: boolean read Get_DetNames;
    property SampleType: boolean read Get_SampleType;
    property Comment: boolean read Get_Comment;
    property Substrate: boolean read Get_Substrate;
    property SampleLocationNameFirst: boolean read GetSampleLocationNameFirst;
    property MeasurementTypeSelected[Index: integer]: boolean read GetMeasuremetType;
    property MeasurementType[Index: integer]: ansistring read GetMeasuremetLabel;
    property MeasurementTypeCount: integer read GetMeasurementTypeCount;
    procedure AddMeasurementType(const sItem: ansistring);
    procedure ClearMeasurementTypes;
  end;

var
  formOptions: TformOptions;

implementation

{$R *.dfm}

const
  RegistryKey = 'Software\Dorset Software\Recorder 6\Settings';

{ TformOptions }

procedure TformOptions.AddMeasurementType(const sItem: ansistring);
begin
  clbMeasurements.Items.Add(sItem);
end;

procedure TformOptions.ClearMeasurementTypes;
begin
  clbMeasurements.Items.Clear;
end;

//------------------------------------------------------------------------------
// We need to decide whether the option to allow the export of confidential
// data should be available. This depends on two things:
//
// 1. Is the option selected? (stored in the registry)
// 2. Does the current user have a high enough security level?
//    - A user's security level is in the USER table and we can get the
//      current user's ID (NAME_KEY) from the IRecorder2000 interface
//    - The min level to allow export of confidential data is in the SETTING
//      table in the row with NAME = "LevelConf"
//------------------------------------------------------------------------------
procedure TformOptions.FormActivate(Sender: TObject);
var oReg: TRegistry;
    oRec2000: IRecorder2000;
    oQuery: TADOQuery;
    iAllowConfidential, iSecurity, iMinLevel: integer;
    sSQL: ansistring;
    i: integer;
begin
  // Initialise so that access WILL NOT be allowed by default
  iSecurity := 0;
  iMinLevel := 5;

  // first we check in the registry to see if the option to allow
  // export of confidential records has been set
  oReg := TRegistry.Create;
  try
    oReg.OpenKey(RegistryKey, False);  // HKLU
    iAllowConfidential := oReg.ReadInteger('Export Confidential Occurrences');
  finally
    oReg.Free;
  end;

  // if it has, we need to check whether the user is at a high
  // enough security level
  if iAllowConfidential > 0 then
  begin
    // we need this to get the connection string for the database
    // and the current user's ID
    oRec2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;

    oQuery := TADOQuery.Create(self);
    try
      oQuery.ConnectionString := oRec2000.ConnectionString;

      // First we get the current user's security level
      sSQL := Format('SELECT SECURITY_LEVEL FROM [USER] WHERE (NAME_KEY = ''%s'')',
                     [oRec2000.CurrentSettings.UserIDKey]);
      oQuery.SQL.Clear;
      oQuery.SQL.Add(sSQL);
      oQuery.Open;
      if oQuery.RecordCount > 0 then
         iSecurity := oQuery.FieldByName('SECURITY_LEVEL').AsInteger;
      oQuery.Close;

      // then we get the Min Security level from the SETTING table
      sSQL := 'SELECT DATA FROM SETTING WHERE (NAME = ''LevelConf'')';
      oQuery.SQL.Clear;
      oQuery.SQL.Add(sSQL);
      oQuery.Open;
      if oQuery.RecordCount > 0 then
         iMinLevel := oQuery.FieldByName('DATA').AsInteger;
      oQuery.Close;
    finally
      oQuery.Free;
    end;
  end;

  // we can now decide whther to enable the checkbox for the
  // confidential records export option
  cbConfidential.Enabled := (iSecurity >= iMinLevel);
  cbConfidential.Checked := False; // make sure option is off by default

  // default measurment type to not selected
  if clbMeasurements.Count > 0 then
    for i := 0 to clbMeasurements.Count - 1 do
      clbMeasurements.Checked[i] := False;
end;

//------------------------------------------------------------------------------
// These are the functions to return results to the caller
//------------------------------------------------------------------------------
function TformOptions.GetMeasurementTypeCount: integer;
begin
  Result := clbMeasurements.Count;
end;

function TformOptions.GetMeasuremetLabel(Index: integer): ansistring;
begin
  if (Index >= 0) and (Index < clbMeasurements.Count) then
    Result := clbMeasurements.Items[Index]
  else
    Result := '';
end;

function TformOptions.GetMeasuremetType(Index: integer): boolean;
begin
  if (Index >= 0) and (Index < clbMeasurements.Count) then
    Result := clbMeasurements.Checked[Index]
  else
    Result := False;
end;

function TformOptions.GetSampleLocationNameFirst: boolean;
begin
  Result := (rgSiteName.ItemIndex = 0);
end;

function TformOptions.Get_Comment: boolean;
begin
  Result := cbComment.Checked;
end;

function TformOptions.Get_Confidential: boolean;
begin
  Result := cbConfidential.Checked;
end;

function TformOptions.Get_DetNames: boolean;
begin
  Result := cbDeterminer.Checked;
end;

function TformOptions.Get_Recorders: boolean;
begin
  Result := cbRecorders.Checked;
end;

function TformOptions.Get_SampleType: boolean;
begin
  Result := cbSampleType.Checked;
end;

function TformOptions.Get_Substrate: boolean;
begin
  Result := cbSubstrate.Checked;
end;

function TformOptions.Get_ZeroAbundance: boolean;
begin
  Result := cbZeroAbundance.Checked;
end;

end.
