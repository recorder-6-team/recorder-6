{ ------------------------------------------------------------------------------
//  Unit:         Exporter
//
//                Copyright © Dorset Software Services Ltd, 2002
//
//  Implements:   TExporter,
//                EExporterError
//
//  Description:  DMAP Export Add-In for Recorder 2000.
//
//  Author:       Paul Thomas
//  Created:      2000-04-19 (?)
//
//  Revision:     $Revision: 5 $
//                $Date: 10/02/03 15:36 $
//                $Author: Andrewkemp $
// --------------------------------------------------------------------------- }
unit Exporter;

interface

uses
  SysUtils, Windows, ActiveX, ComObj, Recorder2000_TLB, Classes, JNCCDatasets,
  AdoDB;

type
  EExporterError = class(Exception);

  TListType = (ltMixed, ltTaxon, ltBiotope, ltLocation, ltName, ltSurvey,
               ltEvent, ltSample);

  TCommaSeparatedKeys = record  //Deals with mixed lists
    Keys1 : string;  //for mixed lists these will be biotope occurrence keys.
    Keys2 : string;  //for mixed lists these will be taxon occurrence keys.
    ListType : TListType;
  end;

  TExporter = class(TAutoObject,
                    IRecorderAddIn,
                    IFilter,
                    IExportFilter,
                    IExportByColumn)
  protected
    FMeasurementUnitKey : WideString;
    FNumberOfRanges : integer;
    FMinValue : Double;
    FMaxValue : Double;
    FTypeErrors : Boolean;
    FDMapExportQuery: TJNCCQuery;
    FConnection : TADOConnection;
    FRecorder : IRecorder2000;
    procedure OpenDatabase;
    procedure CloseDatabase;
    function BuildCommaSeparatedList(const iItemsToExport : IKeyList) : TCommaSeparatedKeys;
    function BuildSelect(const iBiotope : boolean) : string;
    function BuildFrom(const iListType : TListType; iBiotope : boolean) : string;
    function BuildPref(const iBiotope : boolean) : string;
    function BuildWhere(const iListType : TListType; const iKeys : string;
             const iBiotope : Boolean) : string;
    function BuildMeasurement(const iBiotope : boolean) : string;
    function BuildSort(const iBiotope : boolean) : string;

    procedure PopulateExportLists(keys: TCommaSeparatedKeys;
                                  NamList: TStringList;
                                  DisList: TStringList);
    procedure ExportOccurrences(const iBiotope: boolean;
              const iListType : TListType; const iKeys : string;
              var ioNAMFile, ioDISFile : TStringList; var ioIndex : integer);
    function ProcessSpatialReference(const ref: string;
                                     const system: string): string;
    function ProcessLuxReference(const ref: string): string;
    function TrimFirstWord(const iString : string) : string;
    function AddLeadingBrackets(const iString : string) : string;
    function SetRangeValue(const iValue : double) : integer;
    function RemoveExistingExt(const iFile : string) : string;
  protected { IRecorderAddIn }
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
  protected { IFilter }
    function Get_DefaultFileExtension: WideString; safecall;
  protected { IExportFilter }
    function ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool; safecall;
    function Get_LastExportError: WideString; safecall;
  protected { IExportByColumn }
    function Get_MeasurementUnitKey: WideString; safecall;
    procedure Set_MeasurementUnitKey(const Value: WideString); safecall;
    function Get_NumberOfRanges: Integer; safecall;
    procedure Set_NumberOfRanges(Value: Integer); safecall;
    function Get_MinValue: Double; safecall;
    procedure Set_MinValue(Value: Double); safecall;
    function Get_MaxValue: Double; safecall;
    procedure Set_MaxValue(Value: Double); safecall;
  end; { TExporter }




implementation

uses
  ComServ, Dialogs, DataClasses, Registry, DMAPServer_TLB;

const
  DIS_FILE_EXT='.dis';
  NAM_FILE_EXT='.nam';

  EST_DB_MISSING = '%s Name not in registry - export cannot be performed';
  EST_ADD_VALUE = 'An error occurred writing additional column data.';
  EST_UNKNOWN_LIST = 'Unknown list type (%s).';
  EST_CREATE_FILE = 'Failed to create DMAP export file.'#10#10
                    + 'Please ensure that you have specified a valid path to'
                    + ' which you have write access.';
  EST_WRITE_FILE = 'An error occurred while writing to a DMAP export'
                   + ' file.'#10#10
                   + 'Please ensure that there is enough free space on the'
                   + ' disc, and that the drive is still connected.';
  EST_UNEXPECTED = 'An unexpected error occurred while creating the DMAP'
                   + ' export files.'#10#10'%s: %s.';

  FROM = 'From ';
  BIOTOPE_SELECT = 'SELECT BIOTOPE.BIOTOPE_KEY AS KEY_FIELD,'
                   + ' BIOTOPE.SHORT_TERM AS NAME_FIELD,'
                   + ' SAMPLE.SPATIAL_REF, SAMPLE.SPATIAL_REF_SYSTEM,'
                   + ' SAMPLE.LAT, SAMPLE.LONG,'
                   + ' SAMPLE.VAGUE_DATE_START, SAMPLE.VAGUE_DATE_END,'
                   + ' SAMPLE.VAGUE_DATE_TYPE';
  TAXON_SELECT =   'SELECT TAXON.TAXON_KEY AS KEY_FIELD,'
                   + ' TAXON.ITEM_NAME AS NAME_FIELD,'
                   + ' SAMPLE.SPATIAL_REF, SAMPLE.SPATIAL_REF_SYSTEM,'
                   + ' SAMPLE.LAT, SAMPLE.LONG,'
                   + ' SAMPLE.VAGUE_DATE_START, SAMPLE.VAGUE_DATE_END,'
                   + ' SAMPLE.VAGUE_DATE_TYPE';
  BIO_MEAS_SELECT =', BIOTOPE_OCCURRENCE_DATA.DATA';
  TAX_MEAS_SELECT =', TAXON_OCCURRENCE_DATA.DATA';
  BIOTOPE_SORT =   'ORDER BY BIOTOPE.SHORT_TERM, SAMPLE.SPATIAL_REF';
  TAXON_SORT =     'ORDER BY TAXON.ITEM_NAME, SAMPLE.SPATIAL_REF';
  BIOTOPE_WHERE =  'BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY IN ';
  TAXON_WHERE =    'TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY IN ';
  LOCATION_WHERE = 'SAMPLE.LOCATION_KEY IN ';
  BIO_DET_WHERE =  'BIOTOPE_DETERMINATION.DETERMINER IN ';
  TAX_DET_WHERE =  'TAXON_DETERMINATION.DETERMINER IN ';
  SURVEY_WHERE =   'SURVEY.SURVEY_KEY IN ';
  EVENT_WHERE =    'SURVEY_EVENT.SURVEY_EVENT_KEY IN ';
  SAMPLE_WHERE =   'SAMPLE.SAMPLE_KEY IN ';
  TAX_OCC_JOIN =   'SAMPLE INNER JOIN TAXON_OCCURRENCE ON ' +
                   'SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY) INNER JOIN ' +
                   'TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY) INNER JOIN ' +
                   'TAXON_LIST_ITEM ON TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY = TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY) INNER JOIN ' +
                   'TAXON_VERSION ON TAXON_VERSION.TAXON_VERSION_KEY = TAXON_LIST_ITEM.TAXON_VERSION_KEY) INNER JOIN ' +
                   'TAXON ON TAXON.TAXON_KEY = TAXON_VERSION.TAXON_KEY';
  BIO_OCC_JOIN =   'SAMPLE INNER JOIN BIOTOPE_OCCURRENCE ON ' +
                   'SAMPLE.SAMPLE_KEY = BIOTOPE_OCCURRENCE.SAMPLE_KEY) INNER JOIN ' +
                   'BIOTOPE_DETERMINATION ON BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_DETERMINATION.BIOTOPE_OCCURRENCE_KEY) INNER JOIN ' +
                   'BIOTOPE_LIST_ITEM ON BIOTOPE_LIST_ITEM.BIOTOPE_LIST_ITEM_KEY = BIOTOPE_DETERMINATION.BIOTOPE_LIST_ITEM_KEY) INNER JOIN ' +
                   'BIOTOPE ON BIOTOPE.BIOTOPE_KEY = BIOTOPE_LIST_ITEM.BIOTOPE_KEY';
  TAX_DATA_JOIN =  ') INNER JOIN TAXON_OCCURRENCE_DATA ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_OCCURRENCE_DATA.TAXON_OCCURRENCE_KEY';
  BIO_DATA_JOIN =  ') INNER JOIN BIOTOPE_OCCURRENCE_DATA ON BIOTOPE_OCCURRENCE.BIOTOPE_OCCURRENCE_KEY = BIOTOPE_OCCURRENCE_DATA.BIOTOPE_OCCURRENCE_KEY';
  EVENT_JOIN =     'SURVEY_EVENT INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY) ';
  SURVEY_JOIN =    'SURVEY INNER JOIN SURVEY_EVENT ON SURVEY.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY) ';
  BIO_PREF =       'WHERE BIOTOPE_DETERMINATION.PREFERRED = 1 AND ';
  TAX_PREF =       'WHERE TAXON_DETERMINATION.PREFERRED = 1 AND ';
  TAX_MEASUREMENT =' AND TAXON_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY = ';
  BIO_MEASUREMENT =' AND BIOTOPE_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY = ';

type
  TLatLong = class(TAutoObject, ILatLong)
  protected
    FLatitude: Double;
    FLongitude: Double;
  protected { ILatLong }
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function Get_ErrorMsg: WideString; safecall;
  public
    constructor Create(latitide: Double; longitude: Double);
  end; { TLatLong }


{ TExporter }

//==============================================================================

//counts number of closing brackets and adds the same number of opening brackets
//to the front.
function TExporter.AddLeadingBrackets(const iString: string): string;
var
  lPos, lStringLen, lCount, lIndex : integer;
  lSearchString, lResult : string;
begin
  lResult := '';
  lCount := 0;
  lSearchString := iString;
  lStringLen := Length(lSearchString);
  while lStringLen > 0 do begin
    lPos := Pos(')', lSearchString);
    if lPos > 0 then begin
      lCount := lCount + 1;
      lSearchString := Copy(lSearchString, lPos + 1, lStringLen - lPos);
      lStringLen := Length(lSearchString);
    end else
      lStringLen := 0;
  end;
  for lIndex := 1 to lCount do
    lresult := lresult + '(';
  Result := lResult + iString;
end;

//==============================================================================
function TExporter.BuildCommaSeparatedList(
  const iItemsToExport: IKeyList): TCommaSeparatedKeys;
var
  ltfLookAtKeyField2 : boolean;
  lCurrentKeyItem : IKeyItem;
  lTableName : string;
  lIndex : integer;
  lListType : TListType;
begin
  Result.Keys1 := '';
  Result.Keys2 := '';
  ltfLookAtKeyField2 := False;
  lTableName := iItemsToExport.Get_TableName;

  if CompareText(lTableName, MIXED_DATA) = 0 then begin
    //dealing with biotope and Taxon occurrences
    ltfLookAtKeyField2 := True;
    lListType := ltMixed;
  end

  else if CompareText(lTableName, 'Taxon_Occurrence') = 0 then
    lListType := ltTaxon

  else if CompareText(lTableName, 'Biotope_Occurrence') = 0 then
    lListType := ltBiotope

  else if CompareText(lTableName, 'Location') = 0 then
    lListType := ltLocation

  else if CompareText(lTableName, 'Name') = 0 then
    lListType := ltName

  else if CompareText(lTableName, 'Survey') = 0  then
    lListType := ltSurvey

  else if CompareText(lTableName, 'Survey_Event') = 0 then
    lListType := ltEvent

  else if CompareText(lTableName, 'Sample') = 0 then
    lListType := ltSample

  else
    raise EExporterError.CreateFmt(EST_UNKNOWN_LIST, [lTableName]);

  for lIndex := 0 to iItemsToExport.Get_ItemCount - 1 do begin
    lCurrentKeyItem := iItemsToExport.GetKeyItem(lIndex);
    if ltfLookAtKeyField2 then begin
      if CompareText(lCurrentKeyItem.KeyField2, 'Biotope_Occurrence') = 0 then begin
        if Result.Keys1 <> '' then
          Result.Keys1 := Result.Keys1 + ', ';
        Result.Keys1 := Result.Keys1 + '''' + lCurrentKeyItem.KeyField1 + '''';
      end else if CompareText(lCurrentKeyItem.KeyField2, 'Taxon_Occurrence') = 0 then begin
        if Result.Keys2 <> '' then
          Result.Keys2 := Result.Keys2 + ', ';
        Result.Keys2 := Result.Keys2 + '''' + lCurrentKeyItem.KeyField1 + '''';
      end;
    end else begin
      if Result.Keys1 <> '' then
        Result.Keys1 := Result.Keys1 + ', ';
      Result.Keys1 := Result.Keys1 + '''' + lCurrentKeyItem.KeyField1 + '''';
    end;
  end;
  Result.ListType := lListType;
end;

//==============================================================================
function TExporter.BuildFrom(const iListType: TListType; iBiotope : boolean): string;
var
  lResult : string;
begin
  lresult := '';
  case iListType of
  ltEvent:
    begin
    lresult := EVENT_JOIN;
    end;
  ltSurvey:
    begin
    lresult := SURVEY_JOIN;
    lresult := lResult + TrimFirstWord(EVENT_JOIN);
    end;
  end;
  if iBiotope then begin
    if lresult <> '' then
      lresult := lresult + TrimFirstWord(BIO_OCC_JOIN)
    else
      lresult := lresult + BIO_OCC_JOIN;
  end else begin
    if lresult <> '' then
      lresult := lresult + TrimFirstWord(TAX_OCC_JOIN)
    else
      lresult := lresult + TAX_OCC_JOIN;
  end;
  if FMeasurementUnitKey <> '' then begin
    if iBiotope then
      lResult := lResult + BIO_DATA_JOIN
    else
      lResult := lResult + TAX_DATA_JOIN;
  end;
  result := FROM + AddLeadingBrackets(lResult);
end;

//==============================================================================
function TExporter.BuildMeasurement(const iBiotope: boolean): string;
begin
  if iBiotope then
    result := BIO_MEASUREMENT + '''' + FMeasurementUnitKey + ''''
  else
    result := TAX_MEASUREMENT + '''' + FMeasurementUnitKey + '''';
end;

//==============================================================================
function TExporter.BuildPref(const iBiotope: boolean): string;
begin
  if iBiotope then
    result := BIO_PREF
  else
    result := TAX_PREF;
end;

//==============================================================================
function TExporter.BuildSelect(const iBiotope: boolean): string;
begin
  result := '';
  if iBiotope then begin
    result := BIOTOPE_SELECT;
    if FMeasurementUnitKey <> '' then
      result := result + BIO_MEAS_SELECT;
  end else begin
    result := TAXON_SELECT;
    if FMeasurementUnitKey <> '' then
      result := result + TAX_MEAS_SELECT;
  end;
end;

//==============================================================================
function TExporter.BuildSort(const iBiotope: boolean): string;
begin
  result := '';
  if iBiotope then
    result := BIOTOPE_SORT
  else
    result := TAXON_SORT;
end;

//==============================================================================
function TExporter.BuildWhere(const iListType : TListType; const iKeys : string;
             const iBiotope : Boolean): string;
begin
  result := '';
  if iKeys <> '' then begin
    case iListType of
    ltBiotope:  result := BIOTOPE_WHERE + '(' + iKeys + ')';
    ltTaxon:    result := TAXON_WHERE + '(' + iKeys + ')';
    ltLocation: result := LOCATION_WHERE + '(' + iKeys + ')';
    ltName:
    begin
      if iBiotope then
        result := BIO_DET_WHERE + '(' + iKeys + ')'
      else
        result := TAX_DET_WHERE + '(' + iKeys + ')';
    end;
    ltSurvey:   result := SURVEY_WHERE + '(' + iKeys + ')';
    ltEvent:    result := EVENT_WHERE + '(' + iKeys + ')';
    ltSample:   result := SAMPLE_WHERE + '(' + iKeys + ')';
    end;
  end;
end;

//==============================================================================
function TExporter.ExportFile(const iItemsToExport: IKeyList;
  const iDestinationFile: WideString): WordBool;
// This function actually produces the *.DIS and *.NAM files
// required by DMAP
var
  lslDISFile, lslNAMFile: TStringList;
  lKeys: TCommaSeparatedKeys;

begin
  FRecorder := CreateComObject(CLASS_AutoApplicationSettings) as IRecorder2000;
  FTypeErrors := False;
  result := false;
  lslNAMFile := nil;
  lslDISFile:=TStringList.Create;
  try
    lslNAMFile:=TStringList.Create;
    try
      lKeys := BuildCommaSeparatedList(iItemsToExport);
      PopulateExportLists(lKeys, lslNAMFile, lslDISFile);

      lslDISFile.SaveToFile(RemoveExistingExt(iDestinationFile) + DIS_FILE_EXT);
      lslNAMFile.SaveToFile(RemoveExistingExt(iDestinationFile) + NAM_FILE_EXT);
      result:=true;
      if FTypeErrors then
        MessageDlg('Certain items could not be exported due to type conversion errors.', mtInformation, [mbOK], 0)
      else
        MessageDlg('Successfully completed DMap export.', mtInformation, [mbOK], 0);
    except
      on EFCreateError do
        MessageDlg(EST_CREATE_FILE, mtWarning, [mbOk], 0);
      on EWriteError do
        MessageDlg(EST_WRITE_FILE, mtWarning, [mbOk], 0);
      on e: Exception do
        MessageDlg(Format(EST_UNEXPECTED, [e.ClassName, e.Message]),
                   mtWarning, [mbOK], 0);
    end;
  finally
    lslNAMFile.Free;
    lslDISFile.Free;
  end;
end;

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
procedure TExporter.PopulateExportLists(keys: TCommaSeparatedKeys;
                                        NamList: TStringList;
                                        DisList: TStringList);
var
  index: Integer;
  listtype: TListType;

begin
  index := 0;
  listtype := keys.ListType;
  OpenDatabase;
  try
    case listtype of
      ltMixed:
      begin
        { Biotope and Taxon Occurrences respectively }
        ExportOccurrences(True, ltBiotope, keys.Keys1, NamList, DisList, index);
        ExportOccurrences(False, ltTaxon, keys.Keys2, NamList, DisList, index);
      end;

      ltLocation, ltName, ltSurvey, ltEvent, ltSample:
      begin
        { Biotope and Taxon Occurrences respectively }
        ExportOccurrences(True, listtype, keys.Keys1, NamList, DisList, index);
        ExportOccurrences(False, listtype, keys.Keys1, NamList, DisList, index);
      end;

      ltBiotope:
        { Biotope Occurrences }
        ExportOccurrences(True, listtype, keys.Keys1, NamList, DisList, index);

      ltTaxon:
        { Taxon Occurrences }
        ExportOccurrences(False, listtype, keys.Keys1, NamList, DisList, index);
    end;
  finally
    CloseDatabase;
  end;
  DisList.add('-1');
end;

//==============================================================================
procedure TExporter.ExportOccurrences(const iBiotope: boolean;
          const iListType : TListType; const iKeys : string;
          var ioNAMFile, ioDISFile : TStringList; var ioIndex : integer);
var
  lName, lSpatialRefString : string;
  lItemToAdd, lAddValue, lDataValue : string;
  ltfAdd : Boolean;
  recorderfns: IRecorderFunctions;
  refsystem: string;
  spatialref: ILatLong;

begin
  if iKeys = '' then Exit; { nothing to export }
  recorderfns :=Frecorder.RecorderFunctions;
  refsystem := recorderfns.SpatialRefSystem;

  with FDMapExportQuery do begin
    sql.Clear;
    sql.add(BuildSelect(iBiotope));
    sql.add(BuildFrom(iListType, iBiotope));
    sql.add(BuildPref(iBiotope));
    sql.add(BuildWhere(iListType, iKeys, iBiotope));
    if FMeasurementUnitKey <> '' then
      sql.add(BuildMeasurement(iBiotope));
    sql.add(BuildSort(iBiotope));
    Open;
    try
      first;
      lName := '';
      while not eof do begin
        ltfAdd := True;
        if refsystem = FieldByName('SPATIAL_REF_SYSTEM').AsString then
          // already have spatial reference in correct form
          lSpatialRefString := FieldByName('SPATIAL_REF').AsString
        else begin
          spatialref := TLatLong.Create(FieldByName('LAT').AsFloat,
                                        FieldByName('LONG').AsFloat);
          lSpatialRefString := recorderfns.EncodeSpatialRef(spatialref);
        end;
        
        lItemToAdd := ProcessSpatialReference(lSpatialRefString,
                                              refsystem);

        if FMeasurementUnitKey <> '' then begin
          lDataValue := FieldByName('DATA').AsString;
          try
            lAddValue := IntToStr(SetRangeValue(StrToFloat(lDataValue)));
            if lAddValue <> '' then
              lItemToAdd := lItemToAdd + ' ' + lAddValue;
          except
            on E:EConvertError do begin
              ltfAdd := False;
              FTypeErrors := True;
            end else
              raise EExporterError.Create(EST_ADD_VALUE);
          end;
        end;
        if ltfAdd then begin
          if lName <> FieldByName('NAME_FIELD').AsString then begin
            inc(ioIndex);
            lName := FieldByName('NAME_FIELD').AsString;
            ioNAMFile.add(Inttostr(ioIndex) + ' ' + lName);
            if ioIndex > 1 then ioDISFile.add('-1');
            ioDISFile.add(InttoStr(ioIndex));
          end;  //if
          ioDISFile.add(lItemToAdd);
        end;
        next;
      end;
    finally
      close;
    end;
  end;
end;

{ ------------------------------------------------------------------------------
// Format a reference (given in the specified format) ready for export.
// --------------------------------------------------------------------------- }
function TExporter.ProcessSpatialReference(const ref: string;
                                           const system: string): string;
begin
  if system = 'LUX' then
    Result := ProcessLuxReference(ref)

  else if (system = 'OSGB') or
          (system = 'OSNI') or
          (system = 'LUGR') then
    Result := StringReplace(ref, ' ', '', [rfReplaceAll]) { remove spaces }
    
  else
    Result := ref;
end;

{ ------------------------------------------------------------------------------
// Format a Gauss-Luxembourg spatial reference ready for export.
// --------------------------------------------------------------------------- }
function TExporter.ProcessLuxReference(const ref: string): string;

    { integer divided by 1000, to exactly 3 decimal places }
    function Shift(ref: Integer): string;
    begin
      Result := FloatToStrF(ref / 1000, ffFixed, 15, 3);
    end;

var
  commapos: Integer;
  ref1, ref2: Integer;
  oldsep: Char;

begin
  commapos := Pos(',', ref);
  ref1 := StrToInt(Copy(ref, 1, commapos - 1));
  ref2 := StrToInt(Copy(ref, commapos + 1, Length(ref) - commapos));
  { shift references by 3 decimal places, forcing decimal separator }
  oldsep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Result := Shift(ref1) + ',' + Shift(ref2);
  finally
    DecimalSeparator := oldsep;
  end;
end;

//==============================================================================

function TExporter.Get_DefaultFileExtension: WideString;
begin
  result := '';
end;

//==============================================================================

function TExporter.Get_Description: WideString;
begin
  result := 'COM object which provides the export functionality from Recorder'
            + ' 2000 to DMAP';
end;

//==============================================================================

function TExporter.Get_ImageFileName: WideString;
begin
  result := '';
end;

//==============================================================================

function TExporter.Get_LastExportError: WideString;
begin
  result := '';
end;

//==============================================================================

function TExporter.Get_MaxValue: Double;
begin
  result := FMaxValue;
end;

//==============================================================================
function TExporter.Get_MeasurementUnitKey: WideString;
begin
  result := FMeasurementUnitKey;
end;

//==============================================================================

function TExporter.Get_MinValue: Double;
begin
  result := FMinValue;
end;

//==============================================================================
function TExporter.Get_Name: WideString;
begin
  result := 'DMAP export';
end;

//==============================================================================

function TExporter.Get_NumberOfRanges: Integer;
begin
  result := FNumberOfRanges;
end;

//==============================================================================
procedure TExporter.Install(const iInstalledFilePath: WideString);
begin

end;

//==============================================================================
procedure TExporter.OpenDatabase;
var
  reg: TRegistry;
  lstDatabaseName: string;
  lstServerName : string;
begin
  reg := TRegistry.Create;
  try
    reg.Rootkey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('Software\JNCC\Recorder') then begin
      if reg.ValueExists('Database Name') then
        lstDatabaseName := reg.ReadString('Database Name')
      else
        raise EExporterError.Create(Format(EST_DB_MISSING, ['Database']));
      if reg.ValueExists('Server Name') then
        lstServerName := reg.ReadString('Server Name')
      else
        raise EExporterError.Create(Format(EST_DB_MISSING, ['Server']));
      reg.CloseKey;
    end;  // if reg.OpenKey...
  finally
    reg.Free;
  end;
  FDmapExportQuery := TJNCCQuery.Create(nil);
  FConnection := TADOConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.ConnectionString := FRecorder.ConnectionString;
  FConnection.Open;
  FRecorder.SetApplicationSecurity(FConnection.ConnectionObject);
  FDMapExportQuery.Connection := FConnection;
end;  // OpenDatabase

//==============================================================================
procedure TExporter.CloseDatabase;
begin
  FDMapExportQuery.Close;
  FConnection.Close;
  FDMapExportQuery.Free;
  FDMapExportQuery := nil;
  FConnection.Free;
  FConnection := nil;
end;

//==============================================================================
function TExporter.RemoveExistingExt(const iFile: string): string;
var
  lPos : integer;
  lResult, lRemainder : string;
begin
  lResult := '';
  lRemainder := iFile;
  lPos := Pos('.', lRemainder);
  if lPos = 0 then
    Result := lRemainder
  else begin
    while lPos > 0 do begin
      lResult := lResult + Copy(lRemainder, 1, lPos);
      lRemainder := Copy(lRemainder, lPos + 1, length(lRemainder) - lPos);
      lPos := Pos('.', lRemainder);
    end;

    //Check if not a DMap file
    if (CompareText(lRemainder, Copy(DIS_FILE_EXT, 2, length(DIS_FILE_EXT) - 1)) <> 0) and
       (CompareText(lRemainder, Copy(NAM_FILE_EXT, 2, length(NAM_FILE_EXT) - 1)) <> 0) then
      lResult := lResult + lRemainder;

    //Check if ends in .
    if lResult[length(lResult)] = '.' then
      lResult := Copy(lResult, 1, length(lResult) - 1);

    Result := lResult;
  end;
end;

{ Function to calculate from a data value, which range (1 - FNumberOfRanges) that
    the value should be exported as.  For example, values 1,2,3,4,5,6,7,8,9 will
    export as 1,1,1,2,2,2,3,3,3 if there are 3 ranges }
function TExporter.SetRangeValue(const iValue: double): integer;
var
  lTotalRange, lSingleRange: extended;
begin
  lTotalRange := FMaxValue - FMinValue;
  lSingleRange := lTotalRange / (FNumberOfRanges-1);
  Result := Round((iValue - FMinValue) / lSingleRange) + 1;
  if Result<1 then
    Result := 1
  else if Result > FNumberOfRanges then
    Result := FNumberOfRanges;
end;

//==============================================================================
procedure TExporter.Set_MaxValue(Value: Double);
begin
  FMaxValue := Value;
end;

//==============================================================================
procedure TExporter.Set_MeasurementUnitKey(const Value: WideString);
begin
  FMeasurementUnitKey := Value;
end;

//==============================================================================
procedure TExporter.Set_MinValue(Value: Double);
begin
  FMinValue := Value;
end;

//==============================================================================
procedure TExporter.Set_NumberOfRanges(Value: Integer);
begin
  FNumberOfRanges := Value;
end;

//==============================================================================
function TExporter.TrimFirstWord(const iString: string): string;
var
  lPos : integer;
begin
  lpos := Pos(' ', iString);
  result := Copy(iString, lPos+1, length(iString) - lPos);
end;



{ TLatLong }

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
constructor TLatLong.Create(latitide, longitude: Double);
begin
  FLatitude := latitide;
  FLongitude := longitude;
end;

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
function TLatLong.Get_ErrorMsg: WideString;
begin
  Result := '';
end;

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
function TLatLong.Get_Latitude: Double;
begin
  Result := FLatitude;
end;

{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
function TLatLong.Get_Longitude: Double;
begin
  Result := FLongitude;
end;


{ ------------------------------------------------------------------------------
// --------------------------------------------------------------------------- }
initialization
  TAutoObjectFactory.Create(ComServer,
                            TExporter,
                            Class_Exporter,
                            ciMultiInstance,
                            tmApartment);
end.
