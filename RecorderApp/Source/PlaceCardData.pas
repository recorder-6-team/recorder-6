//==============================================================================
//  Unit:        PlaceCardData
//
//  Implements:  TdmPlaceCard
//
//  Description: Implements data access functionality for the Place Card details
//               screen.
//
//  Author:      Ian Addis
//  Created:     1 June 1999
//
//  Changes:     Eric Salmon - 8 March 2002
//               Cleaned up the CleanUpTable code, uses only one stringlist
//               where the first item specifies the field name, and the rest
//               being the keys.
//
//  Last Revision Details:
//    $Revision: 107 $
//    $Date: 8/04/10 17:17 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit PlaceCardData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, JNCCDatasets, ExceptionForm, DataClasses, StdCtrls, SpatialRef,
  Constants, ADODB, LocationInfoFrame, Variants;

const
  ORIGINAL_DETERMINATION_TYPE_KEY = 'NBNSYS0000000011';
  ORIGINAL_DETERMINER_ROLE_KEY    = 'NBNSYS0000000003';
  DETERMINATION_CONFIRMATION_ROLE_KEY = 'NBNSYS0000000001';
  ORIGINAL_DETERMINER_ROLE_VALIDATION_COMPETENCY = 1;
  COUNT_MEASUREMENT_UNIT_KEY      = 'NBNSYS0000000009';
  RECORDER_RECORDER_ROLE_KEY      = 'NBNSYS0000000002';
  NONE_ABUNDANCE_QUALIFIER_KEY    = 'NBNSYS0000000004';

resourcestring
  ResStr_UnableToDelete = 'Unable to delete from table - ';
  ResStr_UnknownProblem =
      'An unknown problem has occurred cleaning up table %s in the place card datamodule.';
  ResStr_Of = 'of';

const
  COL_CODE_NUMBER           = 'Code Number';
  COL_COMMENT               = 'Comment';
  COL_COMMON_NAME           = 'Common Name';
  COL_COUNT                 = 'Count';
  COL_DATE_OF_DETERMINATION = 'Date of Determination';
  COL_DETERMINER            = 'Determiner';
  COL_RECORD_TYPE           = 'Record Type';
  COL_PROVENANCE            = 'Provenance';
  COL_SUBSTRATE             = 'Substrate';
  COL_SPATIAL_REF           = 'Spatial Ref';
  COL_BIOTOPE               = 'Biotope';

  // and additional 'fixed columns'
  COL_SCIENTIFIC_NAME       = 'Scientific Name';
  COL_COUNT_OF              = 'Count of';
  COL_ACCURACY              = 'Accuracy';

resourcestring
  // Resourcestrings for the list of columns
  ResStr_ColumnCodeNumber          = 'Code Number';
  ResStr_ColumnComment             = 'Comment';
  ResStr_ColumnCommonName          = 'Common Name';
  ResStr_ColumnCount               = 'Count';
  ResStr_ColumnDateOfDetermination = 'Date of Determination';
  ResStr_ColumnDeterminer          = 'Determiner';
  ResStr_ColumnRecordType          = 'Record Type';
  ResStr_ColumnProvenance          = 'Provenance';
  ResStr_ColumnSubstrate           = 'Substrate';
  ResStr_ColumnSpatialRef          = 'Spatial Ref';
  ResStr_ColumnBiotope             = 'Biotope';

  // and additional 'fixed columns'
  ResStr_ColumnScientificName      = 'Scientific Name';
  ResStr_ColumnCountOf             = 'Count of';
  ResStr_ColumnAccuracy            = 'Accuracy';

type
  ERecordCardDataError = class(TExceptionPath);
  ETableCleanupError = class(ERecordCardDataError);
  ENoKeyFieldSupplied = class(ETableCleanupError);
  EUnableToDeleteFromTable = class(ETableCleanupError);

  TMeasurementKeys = class(TKeyData)
  private
    FDataType: Char;
    FRestrictedValues: TStringList;
  protected
    procedure SetItemAdditional(const Value: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RefreshRestrictedValues;
    property DataType: Char read FDataType write FDataType;
    property RestrictedValues: TStringList read FRestrictedValues;
  end;

  // Used to remember original column name after it's been renamed
  TColumnName = class
  private
    FOriginalName: String;
    FIsStandard: Boolean;
    FMeasurementKeys: TMeasurementKeys;
    procedure SetMeasurementKeys(const qualifierKey, unitKey, dataType: String);
  public
    constructor Create(const AName: String; AStandard: Boolean);
    destructor Destroy; override;
    property OriginalName: String read FOriginalName;
    property IsStandard: Boolean read FIsStandard;
    property MeasurementKeys: TMeasurementKeys read FMeasurementKeys;
  end;

  TdmPlaceCard = class(TBaseDataModule)
    qryCheckEvent: TJNCCQuery;
    qryCheckRecorders: TJNCCQuery;
    dsSampleType: TDataSource;
    tblRecordType: TJNCCTable;
    dsRecordType: TDataSource;
    tblSubstrate: TJNCCTable;
    dsSubstrate: TDataSource;
    qryAdminAreas: TJNCCQuery;
    qryTaxonTLIKey: TJNCCQuery;
    qryBiotopeBLIKey: TJNCCQuery;
    qryTaxaNames: TJNCCQuery;
    qryCleanup: TJNCCQuery;
    qrySampleType: TJNCCQuery;
    qryTaxaDetails: TJNCCQuery;
    qryInsertSample: TJNCCQuery;
    qryInsertBiotopeOccur: TJNCCQuery;
    qryInsertTaxonOccur: TJNCCQuery;
    qryInsertTaxOccurData: TJNCCQuery;
    qryInsertSurveyEvent: TJNCCQuery;
    qryCheckDictionary: TJNCCQuery;
    qryCheckSample: TJNCCQuery;
    qryInsertSampleData: TJNCCQuery;
  private
    FMappedColumns: TStringList;
    function DoBiotopeCheck(var ABLIKey: TKeyString; var ABiotope: String): Boolean;
    function DoAdminAreaCheck(var AAKey: TKeyString; var AAdminArea: String; ATypeKey: TKeyString): Boolean;
    function HasSameSampleData(ASampleKey: string; AMeasurementControls: TStringList): Boolean;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function CheckBiotope(var ABLIKey: TKeyString; EBiotope: TEdit): Boolean; overload;
    function CheckBiotope(var ABLIKey: TKeyString; var ABiotope: String): Boolean; overload;
    function CheckAdminArea(var AAKey: TKeyString; EAdminArea: TEdit; ATypeKey: TKeyString): Boolean; overload;
    function CheckReference(var ARefKey: TKeyString; EReference: TEdit): Boolean;
    function CheckSurveyEventRecorders(iRecorders: TStrings;
      iSEKey: TKeyString; iSERKeys: TStringList): Boolean;
    procedure CleanupTable(const iTableName: String; iFields: TStringList);
    function FindTaxon(const iFindTaxon: string): TKeyString;
    function GetAdminAreas(const ALocationKey: TKeyString): String;
    function GetBiotopeListItemKey(const ABiotopeKey: TKeyString): TKeyString;
    function GetGlyphIndex(const iItemName: string): integer;
    function GetSurveyEventKey(const ASurveyKey:TKeyString; const iLocationKey : TKeyString;
             const iLocationName: String; const iSpatialRef: TSpatialRef;
             iRecorders: TStrings; const iDate: String; iSERKeys: TStringList): TKeyString;
    procedure InitTaxaNames(iTaxonKeys: TStrings);
    procedure InitTaxaNamesInRange(iTaxonKeys : TStrings; const iStart, iEnd: integer);
    procedure StartQryTaxaDetails(const AKey: string);
    procedure InitLocalityParams(const AParameters: TParameters; ALocInfoFrame:
        TfraLocationInfo);
    procedure NullSpatialParameters(AParameters: TParameters);
    procedure SetLocationKeyParameter(AParam: TParameter; ALocationInfoFrame:
        TfraLocationInfo);
    function CheckTaxonDictionary(iTaxonKey: String): Boolean;
    function GetSampleKey(DateStart, DateEnd: Integer;
      const DateType, SurveyKey, SampleType, LocationName: string;
      ASpatialRef: TSpatialRef; AMeasurementControls: TStringList): string;
    procedure SetColumnNames(standardColumns, measurementColumns: TStrings);
    function IndexOfMappedColumn(const originalName: String; standard: Boolean): Integer;
    function MappedColumnNameFromObject(obj: TObject): String;
    property MappedColumns: TStringList read FMappedColumns;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, VagueDate, FormActions, HierarchyNodes,
  SpatialRefFuncs, Find, DatabaseAccessADO;

//==============================================================================
{ TdmPlaceCard }
//==============================================================================
constructor TdmPlaceCard.Create(AOwner: TComponent);
begin
  inherited;
  //Set parameter types of queries now as they don't seem to load properly from
  //dfm
  with qryInsertSurveyEvent do
  begin
    Parameters.ParamByName('SURVEY_EVENT_KEY').DataType      := ftString;
    Parameters.ParamByName('Vague_Date_Start').DataType      := ftInteger;
    Parameters.ParamByName('Vague_Date_End').DataType        := ftInteger;
    Parameters.ParamByName('Vague_Date_Type').DataType       := ftString;
    Parameters.ParamByName('Spatial_Ref').DataType           := ftString;
    Parameters.ParamByName('Spatial_Ref_System').DataType    := ftString;
    Parameters.ParamByName('Spatial_Ref_Qualifier').DataType := ftString;
    Parameters.ParamByName('Lat').DataType                   := ftFloat;
    Parameters.ParamByName('Long').DataType                  := ftFloat;
    Parameters.ParamByName('Location_Key').DataType          := ftString;
    Parameters.ParamByName('Survey_Key').DataType            := ftString;
    Parameters.ParamByName('Location_Name').DataType         := ftString;
    Parameters.ParamByName('Entered_By').DataType            := ftString;
  end;
  with qryInsertTaxOccurData do
  begin
    Parameters.ParamByName('SiteID').DataType                    := ftString;
    Parameters.ParamByName('Taxon_Occurrence_Key').DataType      := ftString;
    Parameters.ParamByName('Accuracy').DataType                  := ftString;
    Parameters.ParamByName('Data').DataType                      := ftString;
    Parameters.ParamByName('Measurement_Qualifier_Key').DataType := ftString;
    Parameters.ParamByName('Measurement_Unit_Key').DataType      := ftString;
    Parameters.ParamByName('Entered_By').DataType                := ftString;
  end;
  with qryInsertSample do
  begin
    Parameters.ParamByName('Sample_Key').DataType            := ftString;
    Parameters.ParamByName('Sample_Reference').DataType      := ftString;
    Parameters.ParamByName('Vague_Date_Start').DataType      := ftInteger;
    Parameters.ParamByName('Vague_Date_End').DataType        := ftInteger;
    Parameters.ParamByName('Vague_Date_Type').DataType       := ftString;
    Parameters.ParamByName('Spatial_Ref').DataType           := ftString;
    Parameters.ParamByName('Spatial_Ref_System').DataType    := ftString;
    Parameters.ParamByName('Spatial_Ref_Qualifier').DataType := ftString;
    Parameters.ParamByName('Lat').DataType                   := ftFloat;
    Parameters.ParamByName('Long').DataType                  := ftFloat;
    Parameters.ParamByName('Outstanding_Card').DataType      := ftBoolean;
    Parameters.ParamByName('Sample_Type_Key').DataType       := ftString;
    Parameters.ParamByName('Location_Key').DataType          := ftString;
    Parameters.ParamByName('Survey_Event_Key').DataType      := ftString;
    Parameters.ParamByName('Location_Name').DataType         := ftString;
    Parameters.ParamByName('Comment').DataType               := ftString;
    Parameters.ParamByName('Entered_By').DataType            := ftString;
  end;
  qryTaxaDetails.ParseSQL := False;
  FMappedColumns := TStringList.Create;
end;  // Create

//==============================================================================
destructor TdmPlaceCard.Destroy;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FMappedColumns.Count - 1 do
    FMappedColumns.Objects[i].Free;
  FMappedColumns.Free;
end;

//==============================================================================
function TdmPlaceCard.GetGlyphIndex(const iItemName: string): integer;
var liIdx: integer;
begin
  Result := -1;
  liIdx  :=  0;
  with dmFormActions.SampleTypeList do
  begin
    while (liIdx < Count) and (TSampleType(Items[liIdx]).ShortName <> iItemName) do
      Inc(liIdx);

    if liIdx <= Count - 1 then //clearly found a match.
      Result := liIdx;
  end;
end;  // GetGlyphIndex

//==============================================================================
function TdmPlaceCard.GetAdminAreas(const ALocationKey: TKeyString): String;
var
  lTempString: String;
begin
  //Add each admin area to the string
  lTempString := '';
  with qryAdminAreas do
  begin
    Parameters.ParamByName('Key').Value := ALocationKey;
    Open;
    while not Eof do
    begin
      //Add seperator
      if lTempString <> '' then
        lTempString:= lTempString + ', ';
      if FieldByName('Short_Code').AsString <> '' then
        lTempString := lTempString + FieldByName('SHORT_CODE').AsString + ', ';
      lTempString := lTempString + FieldByName('ITEM_NAME').AsString;
      Next;
    end;
    Close;
  end;
  Result:= lTempString;
end;  // GetAdminAreas

//==============================================================================
function TdmPlaceCard.GetBiotopeListItemKey(const ABiotopeKey:TKeyString):TKeyString;
begin
  Result := '';
  with qryBiotopeBLIKey do begin
    Parameters.ParamByName('KeyParameter').Value := ABiotopeKey;
    Open;
    First;
    if not Eof then Result := FieldByName('Biotope_List_Item_Key').AsString;
    Close;
  end;
end;  // GetBiotopeListItemKey

//==============================================================================
function TdmPlaceCard.CheckBiotope(var ABLIKey: TKeyString; EBiotope: TEdit): Boolean;
var lstBiotope: String;
    lKey      : TKeyString;
begin
  lstBiotope := EBiotope.Text;
  lKey       := ABLIKey;
  Result     := DoBiotopeCheck(lKey, lstBiotope);
  // Reset eBiotope text first, as this triggers a call to the OnChange handler
  EBiotope.Text := lstBiotope;
  // Now we can set the Key
  ABLIKey := lKey;
end;  // CheckBiotope

//------------------------------------------------------------------------------
function TdmPlaceCard.CheckBiotope(var ABLIKey: TKeyString; var ABiotope: String): Boolean;
begin
  Result := DoBiotopeCheck(ABLIKey, ABiotope);
end;  // CheckBiotope

//==============================================================================
function TdmPlaceCard.CheckAdminArea(var AAKey: TKeyString; EAdminArea: TEdit; ATypeKey: TKeyString): Boolean;
var lstAdminArea: String;
    lKey      : TKeyString;
begin
  lstAdminArea := EAdminArea.Text;
  lKey       := AAKey;
  Result     := DoAdminAreaCheck(lKey, lstAdminArea, ATypeKey);
  // Reset eBiotope text first, as this triggers a call to the OnChange handler
  EAdminArea.Text := lstAdminArea;
  // Now we can set the Key
  AAKey := lKey;
end;  // CheckAdminArea

//------------------------------------------------------------------------------
function TdmPlaceCard.DoBiotopeCheck(var ABLIKey: TKeyString; var ABiotope: String): Boolean;
var lFind: TdlgFind;
begin
  Result := true;
  lFind  := TdlgFind.CreateDialog(nil, ResStr_FindBiotopeTerm, ftBiotope);
  with lFind do
    try
      if FindUnique(ABiotope) then begin
        ABLIKey  := ItemKey;
        ABiotope := ItemText;
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOk then begin
          ABLIKey  := ItemKey;
          ABiotope := ItemText;
        end else
          Result := False;
      end else begin
        MessageDlg(ResStr_BiotopeItems, mtInformation, [mbOK], 0);
        Result := False;
      end;
    finally
      Release;
    end;
end;  // DoBiotopeCheck

//------------------------------------------------------------------------------
function TdmPlaceCard.DoAdminAreaCheck(var AAKey: TKeyString; var AAdminArea: String; ATypeKey: TKeyString): Boolean;
var lFind: TdlgFind;
begin
  Result := true;
  lFind  := TdlgFind.CreateSearchAdminType(nil, ResStr_FindAdminArea, ATypeKey);
  with lFind do
    try
      if FindUnique(AAdminArea) then begin
        AAKey  := ItemKey;
        AAdminArea := ItemText;
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal = mrOk then begin
          AAKey  := ItemKey;
          AAdminArea := ItemText;
        end else
          Result := False;
      end else begin
        MessageDlg(ResStr_AdminAreaItems, mtInformation, [mbOK], 0);
        Result := False;
      end;
    finally
      Release;
    end;
end;  // DoAdminAreaCheck

//==============================================================================
function TdmPlaceCard.CheckReference(var ARefKey:TKeyString; EReference:TEdit):boolean;
var lFind: TdlgFind;
begin
  Result := true;
  lFind  := TdlgFind.CreateDialog(nil, ResStr_FindDocument, ftReference);
  with lFind do
    try
      if FindUnique(eReference.Text) then begin
        ARefKey         := ItemKey;
        EReference.Text := dmGeneralData.GetReferenceText(ARefKey);
      end else
      if not eSearchText.NoSourceItems then begin
        if ShowModal=mrOk then begin
          ARefKey         := ItemKey;
          EReference.Text := dmGeneralData.GetReferenceText(ARefKey);
        end else
          Result := false;
      end else begin
        Result := false;
        MessageDlg(ResStr_NoReferenceItems, mtInformation, [mbOK], 0);
      end;
    finally
      Release;
    end;
end; // CheckReference

//==============================================================================
function TdmPlaceCard.GetSurveyEventKey(const ASurveyKey:TKeyString;
         const iLocationKey : TKeyString; const iLocationName: String;
         const iSpatialRef: TSpatialRef; iRecorders: TStrings; const iDate: String;
         iSERKeys: TStringList): TKeyString;
var lDate:string;
begin
  Result := '';
  lDate  := VagueDatetoString(StringtoVagueDate(iDate));

  //Try to find existing survey event which matches the current survey, location name and SpatialRef.SpatialRef
  with qryCheckEvent do begin
    Parameters.ParamByName('Survey').Value := ASurveyKey;
    if iLocationKey = '' then
      SQL[3] := 'AND SE.SPATIAL_REF = ''' + iSpatialRef.EnteredRef + ''''
    else
      SQL[3] := 'AND SE.LOCATION_KEY = ''' + iLocationKey + '''';
    SQL[3] := SQL[3]
        + ' AND (SE.LOCATION_NAME = ''' + StringReplace(iLocationName, #39, #39#39, [rfReplaceAll])
        + ''' OR SE.LOCATION_NAME IS NULL) ORDER BY SE.LOCATION_NAME DESC';
    Open;
    //Now locate any survey events which match with eDate
    while (not Eof) and (Result = '') do
    begin
      if FieldByName('VAGUE_DATE_START').Text = lDate then
      begin
        //Now locate the recorders for the current survey event and match them with lbRecorders
        if CheckSurveyEventRecorders(iRecorders, FieldByName('SURVEY_EVENT_KEY').AsString, iSERKeys) then
	        Result := FieldByName('SURVEY_EVENT_KEY').AsString;
      end;
      Next;
    end;
    Close;
  end;
end;  // GetSurveyEventKey

//==============================================================================
function TdmPlaceCard.CheckSurveyEventRecorders(iRecorders: TStrings;
  iSEKey: TKeyString; iSERKeys: TStringList): boolean;
var
  lKeys: TStringList;
  i: Integer;
  iMatches: integer;
begin
  //Set pessimistic result
  Result := False;
  //Get list of keys to search for
  lKeys := TStringList.Create;
  try
    iSERKeys.Clear;
    with iRecorders do
      for i := 0 to Count - 1 do
        lKeys.Add(TKeyData(Objects[i]).ItemKey);

    with qryCheckRecorders do
    begin
      Parameters.ParamByName('Key').Value := iSEKey;
      Open;
      Last;
      //Check the number of recorders
      if RecordCount=lKeys.Count then
      begin
        First;
        iMatches := 0;
        while not Eof do begin
          i := lKeys.IndexOf(FieldByName('NAME_KEY').AsString);
          if i <> -1 then
            Inc(iMatches);
          //Build the SERKeys list to pass out.
          iSERKeys.Add(FieldByName('SE_RECORDER_KEY').AsString);
          Next;
        end;
        //If the key list is empty, we have found all the keys
        if iMatches = Recordcount then
          Result := True;
      end;
      Close;
    end;
  finally
    lKeys.Free;
  end;
end;  // CheckSurveyEventRecorders

//==============================================================================
{ Each key on iTaxonKeys should be wrapped in single quotes }
procedure TdmPlaceCard.InitTaxaNames(iTaxonKeys: TStrings);
begin
  with qryTaxaNames do begin
    Close;
    SQL[SQL.Count - 2] := iTaxonKeys.CommaText;
    Open;
  end;
end;  // InitTaxaNames

//==============================================================================
{ InitTaxaNamesInRange is just like the InitTaxaNames except it initialises for
    a selected range of the supplied keys, rather than the entire list }
procedure TdmPlaceCard.InitTaxaNamesInRange(iTaxonKeys: TStrings; const iStart,
  iEnd: integer);
var liIdx  : Integer;
    lstKeys: String;
begin
  with qryTaxaNames do
  begin
    Close;
    lstKeys := '';
    // This loop will give us [,'NBN...','NBN...','NBN...']
    for liIdx := iStart to iEnd do
      lstKeys := lstKeys + ',' + iTaxonKeys[liIdx];
    SQL[SQL.Count - 2] := Copy(lstKeys, 2, Length(lstKeys));  // To skip the first ','
    Open;
  end;
end;  // InitTaxaNamesInRange

//==============================================================================
procedure TdmPlaceCard.CleanupTable(const iTableName: String; iFields: TStringList);
var lstSQL: String;
    i     : Integer;
begin
  with qryCleanup do begin
    Close;
    SQL.Clear;
    lstSQL := Format('DELETE FROM %s WHERE %s IN (''%s''', [iTableName, iFields[0], iFields[1]]);
    // Generate SQL for other values
    for i := 2 to iFields.Count - 1 do lstSQL := lstSQL + ',''' + iFields[i] + '''';
    lstSQL := lstSQL + ')';

    SQL.Add(lstSQL);
    try
      ExecSQL;
    except
      on E:Exception do
        if (E.ClassType = EDatabaseError) then
          raise ETableCleanupError.CreateNonCritical(ResStr_UnableToDelete + iTableName, E)
        else
          raise ERecordCardDataError.Create(Format (ResStr_UnknownProblem, [iTableName]), E);
    end;
  end;
end;  // CleanupTable

//==============================================================================
{ Use the FindUnique function of the Find dialog to determine if we need to
  show the dialog itself. }
function TdmPlaceCard.FindTaxon(const iFindTaxon: string): TKeyString;
var ldlgFind: TdlgFind;
begin
  ldlgFind := TdlgFind.CreateDialog(nil, True, ResStr_FindSpecies, ftTaxon);
  with ldlgFind do begin
    try
      if iFindTaxon <> '' then StoreSearchText(iFindTaxon)
                          else StoreSearchText('');

      // If there is a unique item, get it, otherwise, we have to show the dialog
      if FindUnique(iFindTaxon) then
        Result := ItemKey
      else
      if ShowModal = mrOK then Result := ItemKey
                          else Result := '';
    finally
      Release;
    end;
  end; // with lFind
end; // FindTaxon

{-------------------------------------------------------------------------------
  Set up spatial reference parameters
}
procedure TdmPlaceCard.InitLocalityParams(const AParameters: TParameters;
    ALocInfoFrame: TfraLocationInfo);
var
  lLatLong: TLatLong;
begin
  with ALocInfoFrame.eSpatialRef do
    if EnteredRef<>'' then begin
      AParameters.ParamByName('SPATIAL_REF').Value           := EnteredRef;
      AParameters.ParamByName('SPATIAL_REF_SYSTEM').Value    := EnteredSystem;
      AParameters.ParamByName('SPATIAL_REF_QUALIFIER').Value := Qualifier;
      lLatLong := ConvertToLatLong(EnteredRef, EnteredSystem);
      AParameters.ParamByName('LAT').Value                   := lLatLong.Lat;
      AParameters.ParamByName('LONG').Value                  := lLatLong.Long;
    end
    else
      NullSpatialParameters(AParameters);
  SetLocationKeyParameter(AParameters.ParamByName('LOCATION_KEY'), ALocInfoFrame);
  AParameters.ParamByName('LOCATION_NAME').Value := ALocInfoFrame.eLocationName.Text;
end;

{-------------------------------------------------------------------------------
  Clears values from the spatial reference parameters of a query
}
procedure TdmPlaceCard.NullSpatialParameters(AParameters: TParameters);
begin
  AParameters.ParamByName('SPATIAL_REF').Value           := Null;
  AParameters.ParamByName('SPATIAL_REF_SYSTEM').Value    := Null;
  AParameters.ParamByName('SPATIAL_REF_QUALIFIER').Value := Null;
  AParameters.ParamByName('Lat').Value                   := Null;
  AParameters.ParamByName('Long').Value                  := Null;
end;

{-------------------------------------------------------------------------------
  Sets the value of the parameter for location keys (either an event or sample)
}
procedure TdmPlaceCard.SetLocationKeyParameter(AParam: TParameter;
    ALocationInfoFrame: TfraLocationInfo);
begin
  if ALocationInfoFrame.eLocation.Key <> '' then
    AParam.Value := ALocationInfoFrame.eLocation.Key
  else
    AParam.Value := Null;
end;                                                                          

//==============================================================================
procedure TdmPlaceCard.StartQryTaxaDetails(const AKey: string);
begin
  with qryTaxaDetails.SQL do
    Strings[5] := '''' + AKey + '''';
end;  // StartQryTaxaDetails

//==============================================================================
function TdmPlaceCard.CheckTaxonDictionary(iTaxonKey: String): Boolean;
begin
  Result := True;
  with qryCheckDictionary do
  begin
    Parameters.ParamByName('TaxonList').Value := iTaxonKey;
    Open;
    while not Eof do begin
      Result := FieldByName('Allow_Data_Entry').Value;
      Next;
    end;
    Close;
  end;
end;

//==============================================================================
function TdmPlaceCard.GetSampleKey(DateStart, DateEnd: Integer;
  const DateType, SurveyKey, SampleType, LocationName: string;
  ASpatialRef: TSpatialRef; AMeasurementControls: TStringList): string;
var
  lLatLong: TLatLong;
begin
  Result := '';
  with qryCheckSample do
  begin
    Parameters.ParamByName('Start').Value        := DateStart;
    Parameters.ParamByName('End').Value          := DateEnd;
    Parameters.ParamByName('DateType').Value     := DateType;
    Parameters.ParamByName('SurveyKey').Value    := SurveyKey;
    Parameters.ParamByName('SampleType').Value   := SampleType;
    Parameters.ParamByName('SessionStart').Value := AppSettings.SessionStartTime;
    if ASpatialRef.EnteredSystem <> ResStr_SystemUnknown then
      lLatLong := ConvertToLatLong(ASpatialRef.EnteredRef, ASpatialRef.EnteredSystem);
    Parameters.ParamByName('Lat').Value          := lLatLong.Lat;
    Parameters.ParamByName('Long').Value         := lLatLong.Long;
    Parameters.ParamByName('LocationName').Value := LocationName;
    Open;
    while not Eof do
    begin
      if HasSameSampleData(FieldByName('Sample_Key').AsString, AMeasurementControls) then
        Result := FieldByName('Sample_Key').AsString;
      Next;
    end;
    Close;
  end;
end;

(**
 * Checks whether an existing sample found in the DB that we are about to save against
 * actually has the same sample data as the data we are about to post
 *)
function TdmPlaceCard.HasSameSampleData(ASampleKey: string; AMeasurementControls: TStringList): boolean;
var i: integer;
  defTokens: TStringList;
  queryInClause: string;
  sampleDataCount: integer;
begin
  result := true; // default
  defTokens := TStringList.Create;
  defTokens.Delimiter := ':';
  try
    // If something to test against
    if AMeasurementControls.Count>0 then
    begin
      queryInClause := '';
      sampleDataCount := 0;
      // Build an SQL Statement that can select out all the existing measurements which
      // match the ones we have to submit for our sample
      for i:= 0 to AMeasurementControls.Count-1 do begin
        defTokens.delimitedText := AMeasurementControls[i];
        if (defTokens[0]='sample') and (TEdit(AMeasurementControls.Objects[i]).Text<>'') then begin
          if queryInClause<>'' then
            queryInClause := queryInClause + ', ';
          queryInClause := queryInClause + '''' + defTokens[2] + '.' + defTokens[3] + '.' + defTokens[4] +
            '=' + TEdit(AMeasurementControls.Objects[i]).Text + '''';
          sampleDataCount := sampleDataCount+1;
        end;
      end;
      if queryInClause<>'' then begin
        with dmGeneralData.qryAllPurpose do begin
          Sql.Text := Format('SELECT * FROM Sample_Data WHERE Sample_Key=''%s'' ' +
              'AND Measurement_Unit_Key+''.''+Measurement_Qualifier_Key+''.''+Accuracy+''=''+Data in (%s)',
              [ASampleKey, queryInClause]);
          Open;
          // If the query found a match for every measurement we want to post, then the sample is OK to use.
          result := RecordCount = sampleDataCount;
        end;
      end
      else
        // has no sample data to test
        result := true;
    end;
  finally
    defTokens.Free;
  end;
end;

//==============================================================================
procedure TdmPlaceCard.SetColumnNames(standardColumns, measurementColumns: TStrings);
var
  col: TColumnName;

  function AddToList(list: TStrings; const original, translated: String;
    standard: Boolean = True): TColumnName;
  begin
    Result := TColumnName.Create(original, standard);
    FMappedColumns.AddObject(translated, Result);
    if Assigned(list) then
      list.AddObject(translated, Result);
  end;

begin
  if Assigned(standardColumns)    then standardColumns.Clear;
  if Assigned(measurementColumns) then measurementColumns.Clear;

  // Want to map translated to originals. So "ResStr=ORIGINAL"
  // Columns available for selection
  AddToList(standardColumns, COL_CODE_NUMBER          , ResStr_ColumnCodeNumber);
  AddToList(standardColumns, COL_COMMENT              , ResStr_ColumnComment);
  AddToList(standardColumns, COL_COMMON_NAME          , ResStr_ColumnCommonName);
  AddToList(standardColumns, COL_COUNT                , ResStr_ColumnCount);
  AddToList(standardColumns, COL_DATE_OF_DETERMINATION, ResStr_ColumnDateOfDetermination);
  AddToList(standardColumns, COL_DETERMINER           , ResStr_ColumnDeterminer);
  AddToList(standardColumns, COL_RECORD_TYPE          , ResStr_ColumnRecordType);
  AddToList(standardColumns, COL_PROVENANCE           , ResStr_ColumnProvenance);
  AddToList(standardColumns, COL_SUBSTRATE            , ResStr_ColumnSubstrate);
  AddToList(standardColumns, COL_SPATIAL_REF          , ResStr_ColumnSpatialRef);
  AddToList(standardColumns, COL_BIOTOPE              , ResStr_ColumnBiotope);

  // Fixed columns. Should NOT be available for selection.
  AddToList(nil, COL_SCIENTIFIC_NAME, ResStr_ColumnScientificName);
  AddToList(nil, COL_COUNT_OF       , ResStr_ColumnCountOf);
  AddToList(nil, COL_ACCURACY       , ResStr_ColumnAccuracy);

  // Get the measurement column names
  with dmDatabase.GetRecordset('usp_MeasurementColumns_Get', ['@TranslatedOf', ResStr_Of]) do
  begin
    while not Eof do begin
      col := AddToList(
          measurementColumns,
          Fields['OriginalColumnName'].Value,
          Fields['ColumnName'].Value,
          False);
      col.SetMeasurementKeys(Fields['Measurement_Qualifier_Key'].Value,
                             Fields['Measurement_Unit_Key'].Value,
                             Fields['Data_Type'].Value);
      MoveNext;
    end;
    Close;
  end;
end;

//==============================================================================
function TdmPlaceCard.IndexOfMappedColumn(const originalName: String; standard: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;
  with FMappedColumns do
    for i := 0 to Count - 1 do
      if SameText(TColumnName(Objects[i]).OriginalName, originalName) and
         (TColumnName(Objects[i]).IsStandard = standard) then
      begin
        Result := i;
        Exit;
      end;
end;

//==============================================================================
function TdmPlaceCard.MappedColumnNameFromObject(obj: TObject): String;
var
  idx: Integer;
begin
  Result := '';
  idx := FMappedColumns.IndexOfObject(obj);
  if idx <> -1 then
    Result := FMappedColumns[idx];
end;

{-------------------------------------------------------------------------------
 TColumnName }
{-------------------------------------------------------------------------------
}
constructor TColumnName.Create(const AName: String; AStandard: Boolean);
begin
  FOriginalName := AName;
  FIsStandard := AStandard;
end;

{-------------------------------------------------------------------------------
}
destructor TColumnName.Destroy;
begin
  if Assigned(FMeasurementKeys) then FMeasurementKeys.Free;
end;

{-------------------------------------------------------------------------------
}
procedure TColumnName.SetMeasurementKeys(const qualifierKey, unitKey, dataType: String);
begin
  if not Assigned(FMeasurementKeys) then FMeasurementKeys := TMeasurementKeys.Create;
  FMeasurementKeys.ItemKey := qualifierKey;
  FMeasurementKeys.ItemAdditional := unitKey;
  FMeasurementKeys.DataType := dataType[1];
end;

{-------------------------------------------------------------------------------
 TMeasurementKeys }
{-------------------------------------------------------------------------------
}
constructor TMeasurementKeys.Create;
begin
  FRestrictedValues := TStringList.Create;
end;

{-------------------------------------------------------------------------------
}
destructor TMeasurementKeys.Destroy;
begin
  FRestrictedValues.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TMeasurementKeys.SetItemAdditional(const Value: String);
begin
  inherited;
  RefreshRestrictedValues;
end;

{-------------------------------------------------------------------------------
}
procedure TMeasurementKeys.RefreshRestrictedValues;
begin
  FRestrictedValues.Clear;
  with dmDatabase.GetRecordset(
      'usp_MeasurementUnitValue_Select_ForMeasurementUnit',
      ['@Key', ItemAdditional]) do
  begin
    while not Eof do begin
      FRestrictedValues.Add(Fields['Data'].Value);
      MoveNext;
    end;
    Close;
  end;
end;


end.
