//================================================================================
// Author:  Stuart Ball     stuart.ball@jncc.gov.uk
// Date:    May 2009
// Purpose: Object to manage the insertion of entries into Sample_Admin_Area
//--------------------------------------------------------------------------------
//================================================================================
unit SampleAdminArea;

interface

uses
  Windows, ADODB, Recorder2000_TLB, VC_check, SysUtils;

type
  ESampleAdminArea = class(EXception);

  TSampleAdminArea = class(TObject)
  private
    FConnection:   TADOConnection;
    FQuery:        TADOQuery;
    FAdded:        Longint;
    FRecorder2000: IRecorder2000;
    FEnteredBy:    string;
    FSiteID:       string;
    function Get_EnteredBy: string;
    function Get_Key: string;
    function Get_Query: TADOQuery;
    function Get_SiteID: string;
    function Get_VCKey(const vc: byte): string;
    function NeedsAdding(const SampleKey, VCKey: string): boolean;
  protected
    property EnteredBy: string read Get_EnteredBy;
    property SiteID: string read Get_SiteID;
    property ADOQuery: TADOQuery read Get_Query;
    property NewKey: string read Get_Key;
  public
    Constructor Create(Connection: TADOConnection; Rec2000: IRecorder2000);
    Destructor Destroy; override;
    property nAdded: Longint read FAdded;
    procedure Insert(infoCheck: TInfoToCheck);
  end;

implementation

Const
  // the keys of the Admin_Area entries for the 112 GB vice-counties
  VC_KEYS: array[1..112] of string = ('NBNSYS0000000869',
                                      'NBNSYS0000000871',
                                      'NBNSYS0000000872',
                                      'NBNSYS0000000873',
                                      'NBNSYS0000000874',
                                      'NBNSYS0000000875',
                                      'NBNSYS0000000876',
                                      'NBNSYS0000000877',
                                      'NBNSYS0000000878',
                                      'NBNSYS0000000879',
                                      'NBNSYS0000000880',
                                      'NBNSYS0000000881',
                                      'NBNSYS0000000882',
                                      'NBNSYS0000000883',
                                      'NBNSYS0000000884',
                                      'NBNSYS0000000885',
                                      'NBNSYS0000000886',
                                      'NBNSYS0000000887',
                                      'NBNSYS0000000888',
                                      'NBNSYS0000000889',
                                      'NBNSYS0000000890',
                                      'NBNSYS0000000891',
                                      'NBNSYS0000000892',
                                      'NBNSYS0000000893',
                                      'NBNSYS0000000894',
                                      'NBNSYS0000000895',
                                      'NBNSYS0000000896',
                                      'NBNSYS0000000897',
                                      'NBNSYS0000000898',
                                      'NBNSYS0000000899',
                                      'NBNSYS0000000900',
                                      'NBNSYS0000000901',
                                      'NBNSYS0000000902',
                                      'NBNSYS0000000903',
                                      'NBNSYS0000000904',
                                      'NBNSYS0000000905',
                                      'NBNSYS0000000906',
                                      'NBNSYS0000000907',
                                      'NBNSYS0000000908',
                                      'NBNSYS0000000909',
                                      'NBNSYS0000000910',
                                      'NBNSYS0000000911',
                                      'NBNSYS0000000912',
                                      'NBNSYS0000000913',
                                      'NBNSYS0000000914',
                                      'NBNSYS0000000915',
                                      'NBNSYS0000000916',
                                      'NBNSYS0000000917',
                                      'NBNSYS0000000918',
                                      'NBNSYS0000000919',
                                      'NBNSYS0000000920',
                                      'NBNSYS0000000921',
                                      'NBNSYS0000000922',
                                      'NBNSYS0000000923',
                                      'NBNSYS0000000924',
                                      'NBNSYS0000000925',
                                      'NBNSYS0000000926',
                                      'NBNSYS0000000927',
                                      'NBNSYS0000000928',
                                      'NBNSYS0000000929',
                                      'NBNSYS0000000930',
                                      'NBNSYS0000000931',
                                      'NBNSYS0000000932',
                                      'NBNSYS0000000933',
                                      'NBNSYS0000000934',
                                      'NBNSYS0000000935',
                                      'NBNSYS0000000936',
                                      'NBNSYS0000000937',
                                      'NBNSYS0000000938',
                                      'NBNSYS0000000939',
                                      'NBNSYS0000000940',
                                      'NBNSYS0000000941',
                                      'NBNSYS0000000942',
                                      'NBNSYS0000000943',
                                      'NBNSYS0000000944',
                                      'NBNSYS0000000945',
                                      'NBNSYS0000000946',
                                      'NBNSYS0000000947',
                                      'NBNSYS0000000948',
                                      'NBNSYS0000000949',
                                      'NBNSYS0000000950',
                                      'NBNSYS0000000951',
                                      'NBNSYS0000000952',
                                      'NBNSYS0000000953',
                                      'NBNSYS0000000954',
                                      'NBNSYS0000000955',
                                      'NBNSYS0000000956',
                                      'NBNSYS0000000968',
                                      'NBNSYS0000000957',
                                      'NBNSYS0000000958',
                                      'NBNSYS0000000959',
                                      'NBNSYS0000000960',
                                      'NBNSYS0000000961',
                                      'NBNSYS0000000962',
                                      'NBNSYS0000000963',
                                      'NBNSYS0000000964',
                                      'NBNSYS0000000965',
                                      'NBNSYS0000000966',
                                      'NBNSYS0000000967',
                                      'NBNSYS0000000969',
                                      'NBNSYS0000000970',
                                      'NBNSYS0000000971',
                                      'NBNSYS0000000972',
                                      'NBNSYS0000000973',
                                      'NBNSYS0000000974',
                                      'NBNSYS0000000975',
                                      'NBNSYS0000000976',
                                      'NBNSYS0000000977',
                                      'NBNSYS0000000978',
                                      'NBNSYS0000000979',
                                      'NBNSYS0000000980',
                                      'NBNSYS0000000981');


{ TSampleAdminArea }

//--------------------------------------------------------------------------------
// We need access to the database connection and to Recorder
//--------------------------------------------------------------------------------
constructor TSampleAdminArea.Create(Connection: TADOConnection; Rec2000: IRecorder2000);
begin
  FConnection := Connection;
  FRecorder2000 := Rec2000;
  // initialise local variables
  FAdded := 0;
  FEnteredBy := '';
  FSiteID := '';
end;

//--------------------------------------------------------------------------------
// Free memory we allocated
//--------------------------------------------------------------------------------
destructor TSampleAdminArea.Destroy;
begin
  if FQuery <> nil then
    FQuery.Free;
  inherited;
end;

//--------------------------------------------------------------------------------
// Get the Current user's ID from Recorder
//--------------------------------------------------------------------------------
function TSampleAdminArea.Get_EnteredBy: string;
begin
  if FEnteredBy = '' then
    FEnteredBy := FRecorder2000.CurrentSettings.UserIDKey;
  Result := FEnteredBy;
end;

//--------------------------------------------------------------------------------
// Get the next key for the Sample_Admin_Areas table from Recorder
//--------------------------------------------------------------------------------
function TSampleAdminArea.Get_Key: string;
begin
  Result := FRecorder2000.GetNextKey('Sample_Admin_Areas');
end;

//--------------------------------------------------------------------------------
// Set up a query on the database
//--------------------------------------------------------------------------------
function TSampleAdminArea.Get_Query: TADOQuery;
begin
  if FQuery = nil then
  begin
    FQuery := TADOQuery.Create(nil);
    FQuery.Connection := FConnection;
  end;
  Result := FQuery;
end;

//--------------------------------------------------------------------------------
// Get the copy's SiteId from Recorder
//--------------------------------------------------------------------------------
function TSampleAdminArea.Get_SiteID: string;
begin
  if FSiteID = '' then
    with ADOQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT DATA FROM SETTING WHERE NAME = ''SiteID''');
      Open;
      if RecordCount > 0 then
        FSiteID := FieldByName('DATA').AsString;
      Close;
    end;
  Result := FSiteID;
end;

//--------------------------------------------------------------------------------
// Get the key of the Admin_Area representing this vc number
//--------------------------------------------------------------------------------
function TSampleAdminArea.Get_VCKey(const vc: byte): string;
begin
  Result := '';
  if (vc >= Low(VC_KEYS)) and (vc <= High(VC_KEYS)) then
    Result := VC_KEYS[vc]
  else
    raise ESampleAdminArea.CreateFmt('Vice county number %d is not valid', [vc]);
end;

//--------------------------------------------------------------------------------
// Do the insertion
//--------------------------------------------------------------------------------
procedure TSampleAdminArea.Insert(infoCheck: TInfoToCheck);
var theSQL, vcKey: string;
    i: byte;
begin
  for i := 0 to High(infoCheck.VCs) do
  begin
    // get the key for the VC
    vcKey := Get_VCKey(infoCheck.VCs[i]);
    // check whether this entry already exists
    // we don't want to duplicate entries!
    if NeedsAdding(infoCheck.Key, vcKey) then
    begin
      theSQL := Format('INSERT INTO Sample_Admin_Areas ' +
                  '(Sample_Admin_Areas_Key, Admin_Area_Key, Sample_Key, Entered_By, Custodian) ' +
                  'VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'');',
                  [NewKey, vcKey, infoCheck.Key, EnteredBy, SiteID]);
      with ADOQuery do
      begin
        SQL.Clear;
        SQL.Add(theSQL);
        try
          ExecSQL;
          inc(FAdded);
        except
          raise ESampleAdminArea.CreateFmt('Could not insert Sample_Admin_Areas record for Sample %s',
                   [infoCheck.Key]);
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------
// Check whether there is already an entry for this SAMPLE/VC combination
// Returns TRUE if the entry is NOT found
//--------------------------------------------------------------------------------
function TSampleAdminArea.NeedsAdding(const SampleKey, VCKey: string): boolean;
var theSQL: string;
begin
  Result := False;
  theSQL := Format('SELECT Sample_Admin_Areas_Key FROM Sample_Admin_Areas ' +
            'WHERE (Admin_Area_Key = ''%s'') AND (Sample_Key = ''%s'')',
            [VCKey, SampleKey]);
  with ADOQuery do
  begin
    SQL.Clear;
    SQL.Add(theSQL);
    Open;
    Result := (RecordCount = 0); // if the count is zero the entry was not found
    Close;
  end;
end;

end.
