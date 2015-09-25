{===============================================================================
  Unit:        Recorder2000Validation

  Defines:     TRecorder2000Validation

  Description:

  Model:       <none>

  Created:

  Last revision information:
    $Revision: 50 $
    $Date: 7.04.10 14:38 $
    $Author: Andrewkemp $

===============================================================================}

unit Recorder2000Validation;

interface

uses
  Windows, ActiveX, ComObj, Classes, SysUtils, Db, Recorder2000_TLB,
  ValidationData, SpatialRefFuncs, VagueDate, DataClasses, COMClasses, Variants,
  SpatialComAddins, Forms, ADODB, ADODB_TLB, ADOInt, ADOX_TLB, StdValLib_TLB, Registry, VersionInfo;

resourcestring
  ResStr_AddinDescription =
      'Provide the standard validation rules for Recorder '+
      'when importing external records.'#13#10'Version: %s'#13#10'Date: %s';
  ResStr_AddinName = 'Recorder 6 Standard Validation';
  ResStr_Status    = 'Performing external validation checks (%s)...';

type
  TRecorder2000Validation = class(TAutoObject, IRecorderAddIn, IValidation, IValidation6)
  private
    FValidationFailureList: TComKeyList;
    FCurrentEntityType    : String;
    FErrorItemNo          : integer;
    FSurveyErrors         : TStringList;
    FEventErrors          : TStringList;
    FSampleErrors         : TStringList;
    FTaxOccErrors         : TStringList;
    FBioOccErrors         : TStringList;
    FIndividualErrors     : TStringList;
    FOrganisationErrors   : TStringList;
    FReferenceErrors      : TStringList;

    FDesignationErrors    : TStringList;
    FBoundaryErrors       : TStringList;
    FUseErrors            : TStringList;
    FTenureErrors         : TStringList;
    FAimErrors            : TStringList;
    FDamageOccErrors      : TStringList;
    FCommsErrors          : TStringList;
    FAssocsErrors         : TStringList;
    FAddressesErrors      : TStringList;

    FConnection           : TADOConnection;
    FInvalidItems         : TEditableKeyList;
    FValidationData       : TdmValidation;

    FSpatialAddins        : TSpatialComAddins;

    FDateString: String;
    FAddinPath: string;

    FTables: TStringList;
    FCancelled: Boolean;
    FBaseDate: string;
    FZeroDate: string;
    procedure AddError(var AErrors: string; const AErrorToAdd: string);
    procedure ClearErrorLists;
    procedure ConnectToAccessDatabase(const ADBName: String);
    function ErrorForCurrentEntityItem: WideString;
    function GetExactDateFromField(AField: TField; const AType: String): String; overload;
    function GetExactDateFromField(recordset: _Recordset; const specifier: String = ''): String; overload;
    function GetTableData(const ATableName, AJoinClause, AFieldList, AFilter: String;
        filtered: Boolean; const ASortFields: String = '';
        const ATableNamePrefix: String = ''): _Recordset; overload;
    function GetTableData(const ATableName, AFieldList, AFilter: String;
        filtered: boolean): _Recordset; overload;
    function GetVagueDateFromField(recordset: _Recordset; const specifier: String = ''): TVagueDate;
    procedure ShutDownAccessDatabase;
    procedure RefreshScreen(var ACount: Integer);
    { main entity validations }
    function ValidateWholeDatabase: Integer;
    procedure ValidateSurveys(filtered: Boolean = False);
    procedure ValidateEvents(filtered: Boolean = False);
    procedure ValidateSamples(filtered: Boolean = False);
    procedure ValidateTaxOccs(filtered: Boolean = False);
    procedure ValidateBioOccs(filtered: Boolean = False);
    procedure ValidateLocations(filtered: Boolean = False);
    procedure ValidateFeatures(filtered: Boolean = False);
    procedure ValidateIndividuals(filtered: Boolean = False);
    procedure ValidateOrganisations(filtered: Boolean = False);
    procedure ValidateReferences(filtered: Boolean = False);
    procedure ValidateComms(filtered: Boolean = False);
    procedure ValidateAddresses(filtered: Boolean = False);
    procedure ValidateAssocs(filtered: Boolean = False);
    { Location related entity validations }
    procedure ValidateBoundaries(filtered: Boolean = False);
    procedure ValidateDesignations(filtered: Boolean = False);
    procedure ValidateTenures(filtered: Boolean = False);
    procedure ValidateUses(filtered: Boolean = False);
    { Feature related entity validations }
    procedure ValidateAims(filtered: Boolean = False);
    procedure ValidateDamageOccs(filtered: Boolean = False);
    function AllocateResources: IRecorder2000;
    procedure ReleaseResources;
    procedure ValidateOccurrences(const AOccType: String; AErrors: TStringList;
        filtered: Boolean = False);
    function GetBaseDate: string;
    function GetZeroDate: string;
    procedure IncludeObservationsHierarchy(AKeys: TStringList);
    procedure ValidateCommonEventSampleFields(const ATable, ADate, ASpatialRef,
        ASpatialRefSystem, ALocationKey, ALocationDesc: string; var AErrors: string);
    property ZeroDate: string read GetZeroDate;
    property BaseDate: string read GetBaseDate;
  protected
    {Declare IRecorderAddIn methods here}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {Declare IValidation methods here}
    function Validate(const iPath: WideString): Integer; safecall;
    function Get_KeyList: IKeyList; safecall;
    function Get_ErrorString(iIndex: Integer): WideString; safecall;
    {Declare IValidation6 methods here}
    procedure Cancel; safecall;
    function ValidateAll(const connection: ADODB_TLB._Connection): Integer; safecall;
    function ValidateKeyList(const Connection: ADODB_TLB._Connection;
      const KeyList: IKeyList): Integer; safecall;
  public
    {use initialize to create field level objects freed in the destructor }
    destructor Destroy; override;
    procedure Initialize; override;
  end;

const
  // TN_ for Table Name.
  TN_SURVEY             = 'Survey';
  TN_SURVEY_EVENT       = 'Survey_Event';
  TN_SAMPLE             = 'Sample';
  TN_OCCURRENCE         = '%s_Occurrence';
  TN_TAXON_OCCURRENCE   = 'Taxon_Occurrence';
  TN_BIOTOPE_OCCURRENCE = 'Biotope_Occurrence';
  TN_LOCATION           = 'Location';
  TN_FEATURE            = 'Location_Feature';
  TN_INDIVIDUAL         = 'Individual';
  TN_ORGANISATION       = 'Organisation';
  TN_REFERENCE          = 'Reference';

  TN_DESIGNATION        = 'Location_Designation';
  TN_BOUNDARY           = 'Location_Boundary';
  TN_USE                = 'Location_Use';
  TN_TENURE             = 'Tenure';
  TN_AIM                = 'Management_Aim';
  TN_DAMAGE_OCCURRENCE  = 'Damage_Occurrence';
  TN_COMMUNICATION      = 'Communication';
  TN_ASSOCIATION        = 'Name_Relation';
  TN_ADDRESS            = 'Address';

  //----------------------------------------------------------------------------
  // Vague date field names, without prefixes.
  VAGUE_DATE_START = 'Vague_Date_Start';
  VAGUE_DATE_END   = 'Vague_Date_End';
  VAGUE_DATE_TYPE  = 'Vague_Date_Type';

  //----------------------------------------------------------------------------
  FIELDS_ADDRESS =
      'Address_Key, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type';
  FIELDS_AIM =
      'Management_Aim_Key, Agreement_Date';
  FIELDS_ASSOCIATION =
      'Name_Relation_Key, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type';
  FIELDS_BOUNDARY =
      'Location_Boundary_Key, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type';
  FIELDS_COMMUNICATION =
      'Communication_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type';
  FIELDS_DAMAGE_OCCURRENCE =
      'Damage_Occurrence_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type';
  FIELDS_DESIGNATION =
      'Designation_Key, Date_From, Date_To';
  FIELDS_SURVEY_EVENT =
      'Survey_Key, Survey_Event_Key, Location_Key, Spatial_Ref, Spatial_Ref_System, '
      + 'Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Location_Name';
  FIELDS_FEATURE =
      'Location_Feature_Key';
  FIELDS_INDIVIDUAL =
      'Name_Key, '
      + 'Born_Vague_Date_Start, Born_Vague_Date_End, Born_Vague_Date_Type, '
      + 'Died_Vague_Date_Start, Died_Vague_Date_End, Died_Vague_Date_Type';
  FIELDS_LOCATION =
      'Location_Key';
  FIELDS_ORGANISATION =
      'Name_Key, '
      + 'Founded_Vague_Date_Start, Founded_Vague_Date_End, Founded_Vague_Date_Type, '
      + 'Ended_Vague_Date_Start, Ended_Vague_Date_End, Ended_Vague_Date_Type';
  FIELDS_REFERENCE =
      'Source_Key, '
      + 'Year_Vague_Date_Start, Year_Vague_Date_End, Year_Vague_Date_Type';
  FIELDS_SAMPLE =
      'S.Survey_Event_Key, S.Sample_Key, S.Location_Key, S.Spatial_Ref, S.Spatial_Ref_System, '
      + 'S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, S.Location_Name, '
      + 'SE.Survey_Key, SE.Location_Key AS Event_Location_Key';
  JOINS_SAMPLE = ' S INNER JOIN Survey_Event SE ON SE.Survey_Event_Key=S.Survey_Event_Key';
  FIELDS_SURVEY =
      'Survey_Key, SW_Spatial_Ref, NE_Spatial_Ref, Spatial_Ref_System, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type, '
      + 'Op_From_Vague_Date_Start, Op_From_Vague_Date_End, Op_From_Vague_Date_Type, '
      + 'Op_To_Vague_Date_Start, Op_To_Vague_Date_End, Op_To_Vague_Date_Type';
  FIELDS_OCCURRENCE =
      'XO.Sample_Key, XO.%s_Occurrence_Key, XD.%s_Determination_Key, '
      + 'XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Type, '
      + 'S.Vague_Date_Start AS S_Vague_Date_Start, S.Vague_Date_End AS S_Vague_Date_End, '
      + 'S.Vague_Date_Type AS S_Vague_Date_Type';
  FIELDS_TENURE =
      'Tenure_Key, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type';
  FIELDS_USE =
      'Location_Use_Key, '
      + 'From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type, '
      + 'To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type';

  SORT_FIELDS_OCCURRENCE =
      'XO.Sample_Key, XO.%s_Occurrence_Key, XD.%s_Determination_Key, '
      + 'XD.Vague_Date_Start, XD.Vague_Date_End, XD.Vague_Date_Type';
  SORT_FIELDS_SAMPLE =
      'S.Survey_Event_Key, S.Sample_Key, S.Location_Key, S.Spatial_Ref, S.Spatial_Ref_System, '
      + 'S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type, S.Location_Name, '
      + 'SE.Survey_Key, SE.Location_Key';

  SQL_ALL      = 'SELECT %s FROM %s ORDER BY %s ';
  SQL_FILTERED = 'SELECT %s FROM %s WHERE %s IN (SELECT RecordKey FROM #Templist) ORDER BY %s';

  SQL_FILTER_BIOTOPE_OCCURRENCE = 'Biotope_Occurrence_Key';
  SQL_FILTER_EVENT              = 'Survey_Event_Key';
  SQL_FILTER_FEATURE            = 'Location_Feature_Key';
  SQL_FILTER_LOCATION           = 'Location_Key';
  SQL_FILTER_NAME               = 'Name_Key';
  SQL_FILTER_SAMPLE             = 'Sample_Key';
  SQL_FILTER_SOURCE             = 'Source_Key';
  SQL_FILTER_SURVEY             = 'Survey_Key';
  SQL_FILTER_OCCURRENCE         = 'XO.%s_Occurrence_Key';

//==============================================================================
implementation

uses
  ComServ, GeneralFunctions, Constants, SQLConstants;

const
  // Jet 4.0, allow use of ADOX to use Catalog object for Access databases
  JET_4_0  = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=';

{===============================================================================
 TRecorder2000Validation
===============================================================================}
procedure TRecorder2000Validation.Initialize;
begin
  ShortDateFormat := 'dd/MM/yyyy';
  FSurveyErrors       := TStringList.Create;
  FEventErrors        := TStringList.Create;
  FSampleErrors       := TStringList.Create;
  FTaxOccErrors       := TStringList.Create;
  FBioOccErrors       := TStringList.Create;
  FIndividualErrors   := TStringlist.Create;
  FOrganisationErrors := TStringlist.Create;
  FReferenceErrors    := TStringlist.Create;

  FDesignationErrors  := TStringList.Create;
  FBoundaryErrors     := TStringList.Create;
  FUseErrors          := TStringList.Create;
  FTenureErrors       := TStringList.Create;
  FAimErrors          := TStringList.Create;
  FDamageOccErrors    := TStringList.Create;
  FCommsErrors        := TStringList.Create;
  FAssocsErrors       := TStringList.Create;
  FAddressesErrors    := TStringList.Create;

  // Get list of tables in database, so we easily know what's available.
  FTables               := TStringList.Create;
  FTables.CaseSensitive := False;

  FInvalidItems := TEditableKeyList.Create;
  FInvalidItems.SetTable(MIXED_DATA);
  // Saves having to do it all over the place. Once is enough.
  FDateString := DateToStr(Date);
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKeyReadOnly(REG_KEY_RECORDER);
    FAddinPath := ReadString('Addin Path');
    CloseKey;
  finally
    Free;
  end;
end;  // Initialize

//==============================================================================
destructor TRecorder2000Validation.Destroy;
begin
  FInvalidItems.Free;
  FSurveyErrors.Free;
  FEventErrors.Free;
  FSampleErrors.Free;
  FTaxOccErrors.Free;
  FBioOccErrors.Free;
  FIndividualErrors.Free;
  FOrganisationErrors.Free;
  FReferenceErrors.Free;

  FDesignationErrors.Free;
  FBoundaryErrors.Free;
  FUseErrors.Free;
  FTenureErrors.Free;
  FAimErrors.Free;
  FDamageOccErrors.Free;
  FCommsErrors.Free;
  FAssocsErrors.Free;
  FAddressesErrors.Free;

  FTables.Free;
  inherited Destroy;
end;  // Destroy

{-------------------------------------------------------------------------------
  Appends the error to add to the list of errors, inserting 2 line feeds if
     necessary
}
procedure TRecorder2000Validation.AddError(var AErrors: string; const AErrorToAdd: string);
begin
  if AErrors='' then
    AErrors := AErrorToAdd
  else
    AErrors := AErrors + #13#10#13#10 + AErrorToAdd;
end;

{-------------------------------------------------------------------------------
  Clears all error lists. Must be done before validation, either whole database
  or from keylist, starts
}
procedure TRecorder2000Validation.ClearErrorLists;
begin
  FAddressesErrors.Clear;
  FAssocsErrors.Clear;
  FBioOccErrors.Clear;
  FCommsErrors.Clear;
  FEventErrors.Clear;
  FAimErrors.Clear;
  FDamageOccErrors.Clear;
  FIndividualErrors.Clear;
  FDesignationErrors.Clear;
  FBoundaryErrors.Clear;
  FUseErrors.Clear;
  FTenureErrors.Clear;
  FOrganisationErrors.Clear;
  FReferenceErrors.Clear;
  FSampleErrors.Clear;
  FSurveyErrors.Clear;
  FTaxOccErrors.Clear;
end;

//==============================================================================
function TRecorder2000Validation.ErrorForCurrentEntityItem: WideString;
begin
  if CompareText(FCurrentEntityType, TN_SURVEY) = 0 then
    Result := FSurveyErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_SURVEY_EVENT) = 0 then
    Result := FEventErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_SAMPLE) = 0 then
    Result := FSampleErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_TAXON_OCCURRENCE) = 0 then
    Result := FTaxOccErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_BIOTOPE_OCCURRENCE) = 0 then
    Result := FBioOccErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_INDIVIDUAL) = 0 then
    Result := FIndividualErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_ORGANISATION) = 0 then
    Result := FOrganisationErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_REFERENCE) = 0 then
    Result := FReferenceErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_DESIGNATION) = 0 then
    Result := FDesignationErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_BOUNDARY) = 0 then
    Result := FBoundaryErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_USE) = 0 then
    Result := FUseErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_TENURE) = 0 then
    Result := FTenureErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_AIM) = 0 then
    Result := FAimErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_DAMAGE_OCCURRENCE) = 0 then
    Result := FDamageOccErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_COMMUNICATION) = 0 then
    Result := FCommsErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_ASSOCIATION) = 0 then
    Result := FAssocsErrors[FErrorItemNo]
  else
  if CompareText(FCurrentEntityType, TN_ADDRESS) = 0 then
    Result := FAddressesErrors[FErrorItemNo];
end;  // ErrorForCurrentEntityItem

//==============================================================================
function TRecorder2000Validation.Get_Description: WideString;
begin
  Result := Format(ResStr_AddinDescription,
      [GetFileVersion(FAddinPath + 'StdValLib.dll'),
      FormatDateTime('d mmm yyyy',
      GetFileDate(FAddinPath + 'StdValLib.dll'))]);
end;

//==============================================================================
function TRecorder2000Validation.Get_ErrorString(iIndex: Integer): WideString;
var
  lEntityType: String;
  lCurrentKey: IKeyItem;
begin
  lCurrentKey := (FValidationFailureList.GetKeyItem(iIndex));
  lEntityType := lCurrentKey.KeyField2;
  if iIndex = 0 then
  begin
    FCurrentEntityType := lEntityType; // First item in the list
    FErrorItemNo := 0;
  end else
  if CompareText(lEntityType, FCurrentEntityType) <> 0 then  // entity changes
  begin
    FCurrentEntityType := lEntityType;
    FErrorItemNo := 0;
  end else
    FErrorItemNo := FErrorItemNo + 1;

  Result := ErrorForCurrentEntityItem;
end;  // Get_ErrorString

//==============================================================================
function TRecorder2000Validation.Get_ImageFileName: WideString;
begin
  Result := '';
end;

//==============================================================================
function TRecorder2000Validation.Get_KeyList: IKeyList;
begin
  Result := FValidationFailureList as IKeyList;
end;

//==============================================================================
function TRecorder2000Validation.Get_Name: WideString;
begin
  Result := ResStr_AddinName;
end;

//==============================================================================
procedure TRecorder2000Validation.Install(const iInstalledFilePath: WideString);
begin

end;

//==============================================================================
procedure TRecorder2000Validation.ConnectToAccessDatabase(const ADBName: String);
begin
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionString := JET_4_0 + ADBName;
  FConnection.LoginPrompt := False;
  FConnection.Open;
end;  // ConnectToAccessDatabase

//==============================================================================
procedure TRecorder2000Validation.ShutDownAccessDatabase;
begin
  FConnection.Close;
  FConnection.Free;
  FConnection := nil;
end;  // ShutDownAccessDatabase

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.AllocateResources: IRecorder2000;
begin
  FSpatialAddins := TSpatialComAddins.Create(Class_Recorder2000Validation);
  SetCommAddInLink(FSpatialAddins.SpatialAddins, FSpatialAddins.SpatialAddinIntfs);

  FConnection.GetTableNames(FTables, False);

  { We need to repoint the validation data module to our import database }
  FValidationData := TdmValidation.Create(nil);
  FValidationData.SetDatabase(FConnection);

  Result := CreateOleObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
end;  // AllocateResources

{-------------------------------------------------------------------------------
}
procedure TRecorder2000Validation.ReleaseResources;
begin
  FValidationData.Free;
  FSpatialAddins.Free;
  FValidationData := nil;
  FSpatialAddins  := nil;
end;  // ReleaseResources

//==============================================================================
function TRecorder2000Validation.Validate(const iPath: WideString): Integer;
begin
  ConnectToAccessDatabase(iPath);
  try
    Result := ValidateWholeDatabase;
  finally
    ShutDownAccessDatabase;
  end;
end;  // Validate

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.ValidateAll(const connection: ADODB_TLB._Connection): Integer;
begin
  // DO NOT free FConnection, as this would also close the _connection and all linked datasets!!!
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionObject := connection as ADOInt._Connection;
  Result := ValidateWholeDatabase;
  Application.ProcessMessages;
end;  // ValidateAll

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.ValidateWholeDatabase: Integer;
var
  lRecorder: IRecorder2000;
const
  STEPS = 13;
begin
  Result := -1;

  ClearErrorLists;
  lRecorder := AllocateResources;
  lRecorder.RecorderMainForm.StartProgressBar;
  try
    ValidateSurveys;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 1 * 100 div STEPS;
    ValidateEvents;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 2 * 100 div STEPS;
    ValidateSamples;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 3 * 100 div STEPS;
    ValidateTaxOccs;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 4 * 100 div STEPS;
    ValidateBioOccs;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 5 * 100 div STEPS;
    ValidateLocations;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 6 * 100 div STEPS;
    ValidateFeatures;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 7 * 100 div STEPS;
    ValidateIndividuals;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 8 * 100 div STEPS;
    ValidateComms;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 9 * 100 div STEPS;
    ValidateAssocs;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 10 * 100 div STEPS;
    ValidateAddresses;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 11 * 100 div STEPS;
    ValidateOrganisations;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 12 * 100 div STEPS;
    ValidateReferences;
    if FCancelled then Exit;
    lRecorder.RecorderMainForm.Progress := 13 * 100 div STEPS;
  finally
    FValidationFailureList := TComKeyList.Create(FInvalidItems);
    lRecorder.RecorderMainForm.StopProgressBar;
    lRecorder.RecorderMainForm.StatusText := '';
    Application.ProcessMessages;
  end;
  Result := FValidationFailureList.Get_ItemCount;
end;  // ValidateWholeDatabase

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.ValidateKeyList(const connection: ADODB_TLB._Connection;
  const keyList: IKeyList): Integer;
var
  i: Integer;
  key, tableName: String;
  isMixed: Boolean;
  recorder: IRecorder2000;
  sortedKeys: TStringList;
  processedKeyCount: integer;

  procedure ProcessCurrentTable;
  begin
    if SameText(tableName, TN_SURVEY)             then ValidateSurveys(true)       else
    if SameText(tableName, TN_SURVEY_EVENT)       then ValidateEvents(true)        else
    if SameText(tableName, TN_SAMPLE)             then ValidateSamples(true)       else
    if SameText(tableName, TN_TAXON_OCCURRENCE)   then ValidateTaxOccs(true)       else
    if SameText(tableName, TN_BIOTOPE_OCCURRENCE) then ValidateBioOccs(true)       else
    if SameText(tableName, TN_LOCATION)           then ValidateLocations(true)     else
    if SameText(tableName, TN_FEATURE)            then ValidateFeatures(true)      else
    if SameText(tableName, TN_INDIVIDUAL)         then ValidateIndividuals(true)   else
    if SameText(tableName, TN_COMMUNICATION)      then ValidateComms(true)         else
    if SameText(tableName, TN_ASSOCIATION)        then ValidateAssocs(true)        else
    if SameText(tableName, TN_ADDRESS)            then ValidateAddresses(true)     else
    if SameText(tableName, TN_ORGANISATION)       then ValidateOrganisations(true) else
    if SameText(tableName, TN_REFERENCE)          then ValidateReferences(true);
  end;

begin
  Result := -1;
  ClearErrorLists;
  processedKeyCount := 0;

  // DO NOT free FConnection, as this would also close the _connection and all linked datasets!!!
  FConnection := TADOConnection.Create(nil);
  FConnection.ConnectionObject := connection as ADOInt._Connection;
  recorder := AllocateResources;
  recorder.RecorderMainForm.StartProgressBar;
  sortedKeys := TStringList.Create;
  try
    tableName := keyList.TableName;
    isMixed   := SameText(tableName, MIXED_DATA);
    // transfer the keys to a string list so we can sort it by table
    for i := 0 to keyList.ItemCount - 1 do begin
      if isMixed then
        tableName := keyList.GetKeyItem(i).KeyField2
      else
        tableName := keyList.TableName;
      sortedKeys.Add(tableName + '=' + keyList.GetKeyItem(i).KeyField1)
    end;
    sortedKeys.Sorted := true;
    sortedKeys.Duplicates := dupIgnore;
    IncludeObservationsHierarchy(sortedKeys);

    tableName := '';
    for i := 0 to sortedKeys.Count - 1 do begin
      if sortedKeys.Names[i] <> tableName then begin
        // starting to process a new table, so process the last table and prepare a new templist of keys
        ProcessCurrentTable;
        processedKeyCount := i;
        if isMixed then tableName := sortedKeys.Names[i];
        FConnection.Execute(SQL_TEMPLIST_DROP, cmdText, []);
        FConnection.Execute(SQL_TEMPLIST_CREATE, cmdText, []);
      end;
      key := sortedKeys.ValueFromIndex[i];
      FConnection.Execute(Format(SQL_TEMPLIST_INSERT, [key]), cmdText, []);
      recorder.RecorderMainForm.Progress := (i + processedKeyCount) * 50 div sortedKeys.Count;
    end;
    ProcessCurrentTable; // handle the last table in the list
    recorder.RecorderMainForm.Progress := 100;
    Application.ProcessMessages;
  finally
    FValidationFailureList := TComKeyList.Create(FInvalidItems);
    recorder.RecorderMainForm.StopProgressBar;
    recorder.RecorderMainForm.StatusText := '';
    ReleaseResources;
    sortedKeys.Free;
    FConnection.Execute(SQL_TEMPLIST_DROP, cmdText, []);
  end;
  Result := FValidationFailureList.Get_ItemCount;
end;  // ValidateKeyList

{-------------------------------------------------------------------------------
  Convert a vague date field to its string representation.  If the date is
  imprecise, return an empty string so it avoids validation.
}
function TRecorder2000Validation.GetExactDateFromField(AField: TField; const AType: String): String;
var
  lType: String;
begin
  lType := Trim(AType);
  // Unknown date, Month, Season or empty type, return empty String
  if (lType = 'U') or (lType = 'M') or (lType = 'S') or (lType = '') then
    Result := ''
  else
    Result := AField.Text;
end;  // GetExactDateFromField

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.GetVagueDateFromField(recordset: _Recordset;
  const specifier: String = ''): TVagueDate;
var
  lPrefix: String;
begin
  // if a specified supplied, it must have an underscore after it
  if specifier = '' then
    lPrefix := ''
  else
    lPrefix := specifier + '_';

  with recordset do
    if VarIsNull(Fields[lPrefix + VAGUE_DATE_TYPE].Value) then
      Result.DateTypeString := ''
    else begin
      if not VarIsNull(Fields[lPrefix + VAGUE_DATE_START].Value) then
        Result.StartDate := Fields[lPrefix + VAGUE_DATE_START].Value;
      if not VarIsNull(Fields[lPrefix + VAGUE_DATE_END].Value) then
        Result.EndDate := Fields[lPrefix + VAGUE_DATE_END].Value;
      Result.DateTypeString := Fields[lPrefix + VAGUE_DATE_TYPE].Value;
    end;
end;

{-------------------------------------------------------------------------------
}
function TRecorder2000Validation.GetExactDateFromField(recordset: _Recordset;
  const specifier: String = ''): String;
var
  lVagueDate: TVagueDate;
begin
  lVagueDate := GetVagueDateFromField(recordset, specifier);
  if (lVagueDate.DateTypeString = 'U') or (lVagueDate.DateTypeString = 'M') or
     (lVagueDate.DateTypeString = 'S') or (lVagueDate.DateTypeString = '') then
    Result := ''
  else
    Result := VagueDateToString(lVagueDate);
end;  // GetExactDateFromField

//==============================================================================
procedure TRecorder2000Validation.ValidateAddresses(filtered: Boolean = False);
var
  lAddressKey: TKeyString;
  lAddressError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_ADDRESS, FIELDS_ADDRESS, SQL_FILTER_NAME, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lAddressKey            := TKeyString(Fields['Address_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'From');
        lToDateStart           := GetExactDateFromField(rs, 'To');
        lAddressError          := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lAddressError, lDateError);
        if lAddressError <> '' then begin
          FInvalidItems.AddItem(lAddressKey, TN_ADDRESS);
          FAddressesErrors.Add(lAddressError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateAddresses

//==============================================================================
procedure TRecorder2000Validation.ValidateAims(filtered: Boolean = False);
var
  lCount: Integer;
  rs: _Recordset;
begin
  // FAimErrors cleared in ValidateFeatures.

  rs := GetTableData(TN_AIM, FIELDS_AIM, SQL_FILTER_FEATURE, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        if VarToDateTime(Fields['Agreement_Date'].Value) > Date then
        begin
          FInvalidItems.AddItem(TKeyString(Fields['Management_Aim_Key'].Value), TN_AIM);
          FAimErrors.Add(InvalidDate('The Agreement Date ', False, False));
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateAims

//==============================================================================
procedure TRecorder2000Validation.ValidateAssocs(filtered: Boolean = False);
var
  lAssocKey: TKeyString;
  lAssocError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_ASSOCIATION, FIELDS_ASSOCIATION, SQL_FILTER_NAME, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lAssocKey              := TKeyString(Fields['Name_Relation_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'From');
        lToDateStart           := GetExactDateFromField(rs, 'To');
        lAssocError            := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lAssocError, lDateError);
        if lAssocError <> '' then begin
          FInvalidItems.AddItem(lAssocKey, TN_ASSOCIATION);
          FAssocsErrors.Add(lAssocError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateAssocs

//==============================================================================
procedure TRecorder2000Validation.ValidateBioOccs(filtered: Boolean = False);
begin
  ValidateOccurrences('BIOTOPE', FBioOccErrors, filtered);
end;  // ValidateBioOccs

//==============================================================================
procedure TRecorder2000Validation.ValidateBoundaries(filtered: Boolean = False);
var
  lBoundaryKey: TKeyString;
  lBoundaryError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  // FBoundaryErrors cleared in ValidateLocations
  rs := GetTableData(TN_BOUNDARY, FIELDS_BOUNDARY, SQL_FILTER_LOCATION, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lBoundaryKey           := TKeyString(Fields['Location_Boundary_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'From');
        lToDateStart           := GetExactDateFromField(rs, 'To');
        lBoundaryError         := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lBoundaryError, lDateError);
        if lBoundaryError <> '' then begin
          FInvalidItems.AddItem(lBoundaryKey, TN_BOUNDARY);
          FBoundaryErrors.Add(lBoundaryError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateBoundaries

//==============================================================================
procedure TRecorder2000Validation.ValidateComms(filtered: Boolean = False);
var
  lCommKey: TKeyString;
  lCommError: String;
  lDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_COMMUNICATION, FIELDS_COMMUNICATION, SQL_FILTER_NAME, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lCommKey               := TKeyString(Fields['Communication_Key'].Value);
        lDateStart             := GetExactDateFromField(rs);
        lCommError             := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lDateStart, '', lCommError, lDateError);
        if lCommError <> '' then begin
          FInvalidItems.AddItem(lCommKey, TN_COMMUNICATION);
          FCommsErrors.Add(lCommError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateComms

//==============================================================================
procedure TRecorder2000Validation.ValidateDamageOccs(filtered: Boolean = False);
var
  lDamageOccKey: TKeyString;
  lDamageOccError: String;
  lDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  // FDamageErrors cleared in ValidateFeatures
  rs := GetTableData(TN_DAMAGE_OCCURRENCE, FIELDS_DAMAGE_OCCURRENCE, SQL_FILTER_FEATURE, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lDamageOccKey          := TKeyString(Fields['Damage_Occurrence_Key'].Value);
        lDateStart             := GetExactDateFromField(rs);
        lDamageOccError        := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lDateStart, '', lDamageOccError, lDateError);
        if lDamageOccError <> '' then begin
          FInvalidItems.AddItem(lDamageOccKey, TN_DAMAGE_OCCURRENCE);
          FDamageOccErrors.Add(lDamageOccError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateDamageOccs

//==============================================================================
procedure TRecorder2000Validation.ValidateDesignations(filtered: Boolean = False);
var
  lDesignationKey: TKeyString;
  lDesignationError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;

  function ConvertZeroDatesToBaseDate(const ADate: string): string;
  begin
    if AnsiSameText(ADate, ZeroDate) then
      Result := BaseDate
    else
      Result := ADate;
  end;

begin
  // FDesignationErrors cleared in ValidateLocations
  rs := GetTableData(TN_DESIGNATION, FIELDS_DESIGNATION, SQL_FILTER_LOCATION, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lDesignationKey        := TKeyString(Fields['Designation_Key'].Value);
        lFromDateStart         := ConvertZeroDatesToBaseDate(VarToStr(Fields['Date_From'].Value));
        lToDateStart           := ConvertZeroDatesToBaseDate(VarToStr(Fields['Date_To'].Value));
        lDesignationError      := '';
        lDateError.DateFrom    := InvalidDate('The Start Date ', False, False);
        lDateError.DateTo      := InvalidDate('The End Date ', False, False);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToRealDates(lFromDateStart, lToDateStart, lDesignationError, lDateError);
        if lDesignationError <> '' then begin
          FInvalidItems.AddItem(lDesignationKey, TN_DESIGNATION);
          FDesignationErrors.Add(lDesignationError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateDesignations

//==============================================================================
procedure TRecorder2000Validation.ValidateEvents(filtered: Boolean = False);
var
  lEventKey, lSurveyKey, lEventLocationKey: TKeyString;
  lEventError, lEventRef, lEventRefSystem, lEventLocDesc: String;
  lEventDateStart: String;
  lValidResult: TValidationResult;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_SURVEY_EVENT, FIELDS_SURVEY_EVENT, SQL_FILTER_EVENT, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lEventKey         := TKeyString(Fields['Survey_Event_Key'].Value);
        lSurveyKey        := TKeyString(Fields['Survey_Key'].Value);
        lEventLocationKey := TKeyString(VarToStr(Fields['Location_Key'].Value));
        lEventRef         := VarToStr(Fields['Spatial_Ref'].Value);
        lEventRefSystem   := Trim(VarToStr(Fields['Spatial_Ref_System'].Value));
        lEventDateStart   := GetExactDateFromField(rs);
        lEventLocDesc     := VarToStr(Fields['Location_Name'].Value);
        lEventError       := '';
        try
          try
            lValidResult := FValidationData.CheckEventInSurvey(lSurveyKey,
                   lEventRef, lEventRefSystem, lEventLocationKey, True);
            if not lValidResult.Success then
              lEventError := lValidResult.Message;
          except on ESpatialRefError do
            lEventError := ResStr_SystemUnknown;
          end; // try

          if not FValidationData.CheckEventDateAgainstSurvey(lSurveyKey,
                 StringToVagueDate(lEventDateStart), True) then
            AddError(lEventError, ResStr_EventDateAgainstSurvey);

          ValidateCommonEventSampleFields(ResStr_Event, lEventDateStart,
              lEventRef, lEventRefSystem, lEventLocationKey, lEventLocDesc,
              lEventError);
        except
          on E:Exception do
            lEventError := E.Classname + ': ' + E.Message;
        end; // try except
        if lEventError <> '' then begin
          FInvalidItems.AddItem(lEventKey, TN_SURVEY_EVENT);
          FEventErrors.Add(lEventError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateEvents

//==============================================================================
procedure TRecorder2000Validation.ValidateFeatures(filtered: Boolean = False);
begin
  if GetTableData(TN_FEATURE, FIELDS_FEATURE, SQL_FILTER_FEATURE, filtered) = nil then Exit;

  ValidateAims(filtered);
  ValidateDamageOccs(filtered);
end;

//==============================================================================
procedure TRecorder2000Validation.ValidateIndividuals(filtered: Boolean = False);
var
  lIndividualKey: TKeyString;
  lIndividualError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_INDIVIDUAL, FIELDS_INDIVIDUAL, SQL_FILTER_NAME, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lIndividualKey         := TKeyString(Fields['Name_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'Born');
        lToDateStart           := GetExactDateFromField(rs, 'Died');
        lIndividualError       := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lIndividualError, lDateError);
        if lIndividualError <> '' then begin
          FInvalidItems.AddItem(lIndividualKey, TN_INDIVIDUAL);
          FIndividualErrors.Add(lIndividualError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateIndividuals

//==============================================================================
procedure TRecorder2000Validation.ValidateLocations(filtered: Boolean = False);
begin
  if GetTableData(TN_LOCATION, FIELDS_LOCATION, SQL_FILTER_LOCATION, filtered) = nil then Exit;

  ValidateBoundaries(filtered);
  ValidateTenures(filtered);
  ValidateUses(filtered);
  ValidateDesignations(filtered);
end;  // ValidateLocations

//==============================================================================
procedure TRecorder2000Validation.ValidateOrganisations(filtered: Boolean = False);
var
  lOrganisationKey: TKeyString;
  lOrganisationError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_ORGANISATION, FIELDS_ORGANISATION, SQL_FILTER_NAME, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lOrganisationKey       := TKeyString(Fields['Name_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'Founded');
        lToDateStart           := GetExactDateFromField(rs, 'Ended');
        lOrganisationError     := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lOrganisationError, lDateError);
        if lOrganisationError <> '' then begin
          FInvalidItems.AddItem(lOrganisationKey, TN_ORGANISATION);
          FOrganisationErrors.Add(lOrganisationError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateOrganisations

//==============================================================================
procedure TRecorder2000Validation.ValidateReferences(filtered: Boolean = False);
var
  lReferenceKey: TKeyString;
  lReferenceError: String;
  lDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_REFERENCE, FIELDS_REFERENCE, SQL_FILTER_SOURCE, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lReferenceKey          := TKeyString(Fields['Source_Key'].Value);
        lDateStart             := GetExactDateFromField(rs, 'Year');
        lReferenceError        := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lDateStart, '', lReferenceError, lDateError);
        if lReferenceError <> '' then begin
          FInvalidItems.AddItem(lReferenceKey, TN_REFERENCE);
          FReferenceErrors.Add(lReferenceError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateReferences

//==============================================================================
procedure TRecorder2000Validation.ValidateSamples(filtered: Boolean = False);
var
  lSampleKey, lEventKey, lSampleLocationKey: TKeyString;
  lSampleError, lSampleRef, lSampleRefSystem: String;
  lSampleDateStart, lSampleLocDesc: String;
  lSurveyKey, lEventLocationKey: string;
  lValidResult: TValidationResult;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_SAMPLE, JOINS_SAMPLE, FIELDS_SAMPLE, SQL_FILTER_SAMPLE,
      filtered, SORT_FIELDS_SAMPLE);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lSampleKey         := TKeyString(Fields['Sample_Key'].Value);
        lEventKey          := TKeyString(Fields['Survey_Event_Key'].Value);
        lSurveyKey         := Fields['Survey_Key'].Value;
        lEventLocationKey  := VarToStr(Fields['Event_Location_Key'].Value);
        lSampleLocationKey := TKeyString(VarToStr(Fields['Location_Key'].Value));
        lSampleRef         := VarToStr(Fields['Spatial_Ref'].Value);
        lSampleRefSystem   := Trim(VarToStr(Fields['Spatial_Ref_System'].Value));
        lSampleDateStart   := GetExactDateFromField(rs);
        lSampleLocDesc     := VarToStr(Fields['Location_Name'].Value);
        lSampleError       := '';

        try
          lValidResult := FValidationData.CheckSampleInEvent(lEventKey, lSampleRef,
                                                             lSampleLocationKey,
                                                             lSampleRefSystem,
                                                             lEventLocationKey,
                                                             lSurveyKey);
          if not lValidResult.Success then
            lSampleError := lValidResult.Message;

          if not FValidationData.CheckSampleDateAgainstEvent(lEventKey,
                                     StringToVagueDate(lSampleDateStart)) then
            AddError(lSampleError, ResStr_SampleDateAgainstEvent);

          ValidateCommonEventSampleFields(ResStr_Sample, lSampleDateStart,
              lSampleRef, lSampleRefSystem, lSampleLocationKey, lSampleLocDesc,
              lSampleError);
        except
          on E:Exception do
            lSampleError := E.Classname + ': ' + E.Message;
        end; // try except
        if lSampleError <> '' then begin
          FInvalidItems.AddItem(lSampleKey, TN_SAMPLE);
          FSampleErrors.Add(lSampleError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateSamples

//==============================================================================
procedure TRecorder2000Validation.ValidateSurveys(filtered: Boolean = False);
var
  lSurveyKey: TKeyString;
  lBBResult: TValidBoundingBox;
  lSWCorner, lNECorner, lSystem, lSurveyError: String;
  lFromDateStart, lToDateStart: String;
  lDateVal: TValidationResult;
  lCount: Integer;
  rs: _Recordset;
begin
  rs := GetTableData(TN_SURVEY, FIELDS_SURVEY, SQL_FILTER_SURVEY, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lSurveyKey     := TKeyString(Fields['Survey_Key'].Value);
        lSWCorner      := VarToStr(Fields['SW_Spatial_Ref'].Value);
        lNECorner      := VarToStr(Fields['NE_Spatial_Ref'].Value);
        lSystem        := Trim(VarToStr(Fields['Spatial_Ref_System'].Value));
        lFromDateStart := GetExactDateFromField(rs, 'From');
        lToDateStart   := GetExactDateFromField(rs, 'To');
        lSurveyError   := '';
        if ((lSWCorner = '') and (lNECorner <> '')) or
           ((lSWCorner <> '') and (lNECorner = ''))then
        begin
          lSurveyError := ResStr_SurveyBoundingBox;
        end else
        if ((lSWCorner <> '') and (lNECorner <> '')) then begin
          lBBResult := CheckBoundingBox(lSWCorner, lNECorner, lSystem, lSystem);
          if not lBBResult.Valid then begin
            lSurveyError := lBBResult.Error;
          end;
        end;
        lDateVal := ValidateSurveyDates(
            VagueDateToString(GetVagueDateFromField(rs, 'From')),
            VagueDateToString(GetVagueDateFromField(rs, 'To')),
            VagueDateToString(GetVagueDateFromField(rs, 'Op_From')),
            VagueDateToString(GetVagueDateFromField(rs, 'Op_To')));
        if not lDateVal.Success then
          AddError(lSurveyError, lDateVal.Message);

        if lSurveyError <> '' then begin
          FInvalidItems.AddItem(lSurveyKey, TN_SURVEY);
          FSurveyErrors.Add(lSurveyError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateSurveys

//==============================================================================
procedure TRecorder2000Validation.ValidateTaxOccs(filtered: Boolean = False);
begin
  ValidateOccurrences('TAXON', FTaxOccErrors, filtered);
end;  // ValidateTaxOccs

//==============================================================================
procedure TRecorder2000Validation.ValidateTenures(filtered: Boolean = False);
var
  lTenureKey: TKeyString;
  lTenureError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  // FTenureErrors cleared in ValidateLocations
  rs := GetTableData(TN_TENURE, FIELDS_TENURE, SQL_FILTER_LOCATION, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lTenureKey             := TKeyString(Fields['Tenure_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'From');
        lToDateStart           := GetExactDateFromField(rs, 'To');
        lTenureError           := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lTenureError, lDateError);
        if lTenureError <> '' then begin
          FInvalidItems.AddItem(lTenureKey, TN_TENURE);
          FTenureErrors.Add(lTenureError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateTenures

//==============================================================================
procedure TRecorder2000Validation.ValidateUses(filtered: Boolean = False);
var
  lUseKey: TKeyString;
  lUseError: String;
  lFromDateStart, lToDateStart: String;
  lDateError: TDateError;
  lCount: Integer;
  rs: _Recordset;
begin
  // FUseErrors cleared in ValidateLocations
  rs := GetTableData(TN_USE, FIELDS_USE, SQL_FILTER_LOCATION, filtered);
  if rs = nil then Exit;
  with rs do
    try
      lCount := 0;
      while not (Eof or FCancelled) do begin
        lUseKey                := TKeyString(Fields['Location_Use_Key'].Value);
        lFromDateStart         := GetExactDateFromField(rs, 'From');
        lToDateStart           := GetExactDateFromField(rs, 'To');
        lUseError              := '';
        lDateError.DateFrom    := Format(ResStr_FromDate, [FDateString]);
        lDateError.DateTo      := Format(ResStr_ToDate, [FDateString]);
        lDateError.DateCompare := ResStr_CompareDate;
        ValidateFromToVagueDates(lFromDateStart, lToDateStart, lUseError, lDateError);
        if lUseError <> '' then begin
          FInvalidItems.AddItem(lUseKey, TN_USE);
          FUseErrors.Add(lUseError);
        end;
        MoveNext;
        RefreshScreen(lCount);
      end;
    finally
      Close;
    end;
end;  // ValidateUses

//==============================================================================
{ Returns data from the requested table, if it exists in the DB. }
function TRecorder2000Validation.GetTableData(const ATableName, AFieldList, AFilter: String;
    filtered: boolean): _Recordset;
begin
  Result := GetTableData(ATableName, '', AFieldList, AFilter, filtered);
end;

{-------------------------------------------------------------------------------
  Retrieve the data from a table that forms the input for validation.
}
function TRecorder2000Validation.GetTableData(const ATableName, AJoinClause,
    AFieldList, AFilter: String; filtered: Boolean; const ASortFields: String = '';
    const ATableNamePrefix: String = ''): _Recordset;
var
  lSortFields: string;
begin
  if ASortFields='' then
    lSortFields := AFieldList
  else
    lSortFields := ASortFields;
  Result := nil;
  if FTables.IndexOf(ATableName) > -1 then begin
    if filtered then
      Result := FConnection.Execute(
          Format(SQL_FILTERED, [AFieldList, ATableNamePrefix + ATableName + AJoinClause, AFilter, lSortFields]),
          cmdText, [])
    else
      Result := FConnection.Execute(
          Format(SQL_ALL, [AFieldList, ATableNamePrefix + ATableName + AJoinClause, lSortFields]),
          cmdText, []);
    with (CreateOLEObject('Recorder2000.AutoApplicationSettings') As IRecorder2000).RecorderMainForm do
      StatusText := Format(ResStr_Status, [ReadableFormat(ATableName)]);
    Application.ProcessMessages;
  end;
end;  // GetTableData

{-------------------------------------------------------------------------------
}
procedure TRecorder2000Validation.RefreshScreen(var ACount: Integer);
begin
  Inc(ACount);
  if ACount mod 500 = 0 then
    Application.ProcessMessages;
end;

//==============================================================================
procedure TRecorder2000Validation.Cancel;
begin
  FCancelled := True;
end;

{-------------------------------------------------------------------------------
  A generic validation routing for the occurrences (taxon or biotope)
}
procedure TRecorder2000Validation.ValidateOccurrences(const AOccType: String;
    AErrors: TStringList; filtered: Boolean = False);
var
  occurrenceKey, sampleKey, error: String;
  count: integer;
  datesCheck: TValidOccDates;
  sampleDate, detDate: TVagueDate;
  rs: _Recordset;
  tableName: string;
const
  SQL_EXTRA_JOINS =
      ' XO INNER JOIN %s_Determination XD ON XD.%s_Occurrence_Key = XO.%s_Occurrence_Key)'
      + ' INNER JOIN Sample S ON S.Sample_Key = XO.Sample_Key';
begin
  tableName := Format(TN_OCCURRENCE, [AOccType]);
  rs := GetTableData(
      tableName,
      Format(SQL_EXTRA_JOINS, [AOccType, AOccType, AOccType]),
      Format(FIELDS_OCCURRENCE, [AOccType, AOccType]),
      Format(SQL_FILTER_OCCURRENCE, [AOccType]),
      filtered,
      Format(SORT_FIELDS_OCCURRENCE, [AOccType, AOccType]), '(');

  if rs = nil then Exit;

  with rs do
  begin
    count := 0;
    occurrenceKey := '';
    error         := '';

    while not (Eof or FCancelled) do
    begin
      if occurrenceKey <> TKeyString(Fields[AOccType + '_Occurrence_Key'].Value) then begin
        if error <> '' then begin
          FInvalidItems.AddItem(occurrenceKey, tableName);
          AErrors.Add(error);
        end;
        occurrenceKey := TKeyString(Fields[AOccType + '_Occurrence_Key'].Value);
        error         := '';
      end;

      sampleKey  := TKeyString(Fields['Sample_Key'].Value);
      detDate    := GetVagueDateFromField(rs);
      sampleDate := GetVagueDateFromField(rs, 'S');
      try
        datesCheck := FValidationData.CheckDeterminationDate(
            sampleKey,
            TKeyString(Fields[AOccType + '_Determination_Key'].Value),
            sampleDate,
            detDate);
        if not datesCheck.Valid then
          if error = '' then error := datesCheck.Message
                        else error := error + #13#10#13#10 + datesCheck.Message;
      except
        on E:Exception do
          error := E.Classname + ': ' + E.Message;
      end;

      MoveNext;
      RefreshScreen(count);
    end;

    // Don't forget the last one.
    if error <> '' then begin
      FInvalidItems.AddItem(occurrenceKey, tableName);
      AErrors.Add(error);
    end;
  end;
end;  // ValidateTaxOccs

//==============================================================================
function TRecorder2000Validation.GetBaseDate: string;
begin
  if (FBaseDate = '') then
    FBaseDate := Format('30%s12%s1899', [DateSeparator, DateSeparator]);
  Result := FBaseDate;
end;

//==============================================================================
function TRecorder2000Validation.GetZeroDate: string;
begin
  if (FZeroDate = '') then
    FZeroDate := Format('00%s00%s00', [TimeSeparator, TimeSeparator]);
  Result := FZeroDate;
end;

{-------------------------------------------------------------------------------
  Ensures that when items in the observations hierarchy are validated, the
      parents are also validated (e.g. samples, events and surveys)
}
procedure TRecorder2000Validation.IncludeObservationsHierarchy(AKeys: TStringList);

  // grabs the list of parents for a set of nodes lower in the hierarchy
  procedure Include(const AParentTable, AParentKey, AChildTable, AChildKey: string);
  var
    lIndex: integer;
  begin
    // find the first of the block of keys which apply to the child table
    lIndex := AKeys.IndexOfName(AChildTable);
    // build a table containing all the keys from the child table we need, so we can select
    // the distinct parent keys
    FConnection.Execute(SQL_TEMPLIST_CREATE, cmdText, []);
    try
      while SameText(AKeys.Names[lIndex], AChildTable) do begin
        FConnection.Execute(Format(SQL_TEMPLIST_INSERT, [AKeys.ValueFromIndex[lIndex]]), cmdText, []);
        lIndex := lIndex + 1;
        if lIndex >= AKeys.Count then
          break; // from loop
      end;
      with FConnection.Execute(Format('SELECT DISTINCT %s FROM %s WHERE %s IN ' +
          '(SELECT RecordKey FROM #TempList)',
          [AParentKey, AChildTable, AChildKey])) do begin
        // If it doesn't already exist in the list, add the parent keys
        while not EOF do begin
          AKeys.Add(AParentTable + '=' + Fields[0].Value);
          MoveNext;
        end;
      end;
    finally
      FConnection.Execute(SQL_TEMPLIST_DROP, cmdText, []);
    end;
  end;

begin
  // if there are no samples to validate
  if AKeys.IndexOfName(TN_SAMPLE)=-1 then begin
    // but there are occurrences
    if (AKeys.IndexOfName(TN_TAXON_OCCURRENCE)<>-1) then
      Include(TN_SAMPLE, 'SAMPLE_KEY', TN_TAXON_OCCURRENCE, 'TAXON_OCCURRENCE_KEY');
    if (AKeys.IndexOfName(TN_BIOTOPE_OCCURRENCE)<>-1) then
      Include(TN_SAMPLE, 'SAMPLE_KEY', TN_BIOTOPE_OCCURRENCE, 'BIOTOPE_OCCURRENCE_KEY');
  end;
  // if there are no events but there are samples, include the events
  if (AKeys.IndexOfName(TN_SURVEY_EVENT)=-1) and (AKeys.IndexOfName(TN_SAMPLE)<>-1) then
    Include(TN_SURVEY_EVENT, 'SURVEY_EVENT_KEY', TN_SAMPLE, 'SAMPLE_KEY');
  // if there are no surveys but there are events, include the surveys
  if (AKeys.IndexOfName(TN_SURVEY)=-1) and (AKeys.IndexOfName(TN_SURVEY_EVENT)<>-1) then
    Include(TN_SURVEY, 'SURVEY_KEY', TN_SURVEY_EVENT, 'SURVEY_EVENT_KEY');
end;

{-------------------------------------------------------------------------------
  Provide some generic rules that can be applied to the date and locality
    information for both events and spatial references
}
procedure TRecorder2000Validation.ValidateCommonEventSampleFields(const ATable,
    ADate, ASpatialRef, ASpatialRefSystem, ALocationKey, ALocationDesc: string;
    var AErrors: string);
var
  lValidRef: TValidSpatialRef;
begin
  if not CheckVagueDate(ADate) then
    AddError(AErrors, Format(ResStr_InvalidVagueDateValue, [FDateString]));

  if (ASpatialRef<>'') and (ASpatialRefSystem<>'') then begin
    lValidRef := ValidSpecificSpatialRef(ASpatialRef, ASpatialRefSystem);
    if not lValidRef.Valid then
      AddError(AErrors, lValidRef.Error + #13#10 + ResStr_InvalidSpatialRef);

    if (ALocationKey <> '') and (not FValidationData.CheckSRefInLocation
          (ALocationKey, ASpatialRef, ASpatialRefSystem)) then
      AddError(AErrors, ResStr_SRefNotInLocation);
  end
  else if (ASpatialRef<>'') and (ASpatialRefSystem='') then
    AddError(AErrors, ResStr_SpatialRefSystemMissing);

  if not FValidationData.CheckEventLocationInfo
         (ASpatialRef, ALocationKey, ALocationDesc) then
    AddError(AErrors, Format(ResStr_LocationInfoMissing, [ATable]));
end;

//==============================================================================
initialization
  TAutoObjectFactory.Create(ComServer,
                            TRecorder2000Validation,
                            Class_Recorder2000Validation,
                            ciMultiInstance,
                            tmApartment);
end.
