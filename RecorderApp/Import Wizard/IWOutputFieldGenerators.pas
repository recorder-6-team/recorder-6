{===============================================================================
  Unit:         IWOutputFieldGenerators.pas

  Implements:   Various concrete TOutputFieldGenerator subclasses.

  Description:  Output field generator implementations.

  Model:        ImportWizardBackend.mpb

  Created:      June 2004

  Last Revision Details:
    $Revision: 49 $
    $Date: 21/03/13 15:39 $
    $Author: Michaelcaptain $

  Copyright © Environment and Heritage Service, 2004

===============================================================================}
unit IWOutputFieldGenerators;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ADOInt, DataClasses, IWTableRule, ExceptionForm;

type
  {-----------------------------------------------------------------------------
    NBN key which may be partially specified by
    the imported data.
  }
  TKeyFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
    function GeneratedKeySQL(Tables: TTableSource; const SiteId: String):
        String; virtual;
    function RecordIdKeySQL(Tables: TTableSource;
        const SiteColumn, RecordColumn: String;
        IsMapMateKey: Boolean): String; virtual;
  end;

  {-----------------------------------------------------------------------------
    Spatial reference for a sample.
  }
  TSpatialReferenceFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Location.
  }
  TLocationFieldGenerator = class(TOutputFieldGenerator)
  protected
    function LocationKey(Fields: Fields; UseViceCounty: Boolean): OLEVariant;
        virtual;
    function LocationKeySQL(Tables: TTableSource; UseViceCounty: Boolean):
        String; virtual;
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Survey key, supplied by the user.
  }
  TSurveyKeyFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String; override;
  end;

  {-----------------------------------------------------------------------------
    Spatial reference for a survey event.
  }
  TSurveyEventSpatialReferenceFieldGenerator = class(
      TSpatialReferenceFieldGenerator)
  protected
    FLocationGenerator: TLocationFieldGenerator;
  public
    constructor Create(Owner: TOutputField); override;
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Vague date, either obtained from the import
    file or supplied by the user.
  }
  TDateFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Vague date. If the user put an empty value for the import, will take
    the vague date from the date column instead.
    }
  TDeterminationDateFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Vague date. If the user put an empty value for the import, will take
    the vague date from the date column instead.
  }
  TReviewDateFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Determiner Preferred
  }
  TDeterminerPreferredFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String; override;
  end;
  {-----------------------------------------------------------------------------
    Custodian.
  }
  TCustodianFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String; override;
  end;
  {-----------------------------------------------------------------------------
    Current user, current date.
  }
  TCurrentStatusFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String; override;
  end;

  {-----------------------------------------------------------------------------
    Record type.
  }
  TRecordTypeKeyFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Individual key identifying the determiner,
    either obtained from the import file or
    supplied by the user.
  }
  TDeterminerFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Individual key identifying the reviewer
    always obtained from the import file.
  }
  TReviewerFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Individual key identifying the determiner role,
    either obtained from the import file or
    set to 'Original Recorder'.
  }
  TDeterminerRoleFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Individual key identifying the reviewer role,
    either obtained from the import file or
    set to default.
  }
  TReviewerRoleFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Individual key identifying the determination type,
    either obtained from the import file or
    set to 'Unconfirmed'.
  }
  TDeterminationTypeFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    Individual key identifying the review type,
    either obtained from the import file or
    set to 'Unconfirmed'.
  }
  TReviewTypeFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;
  {-----------------------------------------------------------------------------
    For Reviewer (second determiner) get the species, but if the column is blank
    or not uses then uses the main species.
  }
  TReviewSpeciesFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Taxon occurrence key within the same sample
    for the associated species.
  }
  TAssociatedOccurenceKeyFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant; override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Abundance accuracy, either from the "Abundance Accuracy" column type (if
    mapped) or as determined by the parser for the "Abundance Data" column type.
  }
  TAbundanceAccuracyFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant;
        override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;

  {-----------------------------------------------------------------------------
    Observer(s) or TempObserver(s).
  }
  TObserverFieldGenerator = class(TOutputFieldGenerator)
  public
    function Value(Fields: Fields; FieldIndex: Integer): OLEVariant;
        override;
    function GeneratingSQL(FieldIndex: Integer; Tables: TTableSource): String;
        override;
  end;


implementation

uses
  Variants, DateUtils, DatabaseAccessADO, SpatialRefFuncs, ApplicationSettings,
  IWParsers, VagueDate, IWConstants, Sql;

var
  // allow location spatial reference results sets to be cached
  mCachedLocationSRefRecordset: _Recordset;
  mCachedLocationKey: string;


{-==============================================================================
    TKeyFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TKeyFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
var
  RecordNo: Integer;
begin
  RecordNo := Fields['Record_No'].Value;
  
  if ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY) then
    Result := dmDatabase.GetStoredProcOutputParam(
        'usp_ImportWizard_NextKey_MapMate',
        ['@table_name', TableRule.TableName,
         '@field_name', OutputField.Name,
         '@record_no', RecordNo],
        '@key')
  else if ColumnMapping.KeyIsMapped(CT_KEY_RECORD_ID) then
    Result := dmDatabase.GetStoredProcOutputParam(
        'usp_ImportWizard_NextKey_RecordID',
        ['@table_name', TableRule.TableName,
         '@field_name', OutputField.Name,
         '@record_no', RecordNo],
        '@key')
  else if ColumnMapping.KeyIsMapped(CT_KEY_SITE_ID) then
    Result := dmDatabase.GetStoredProcOutputParam(
        'uspImportWizard_NextKey_SiteID',
        ['@table_name', TableRule.TableName,
         '@record_no', RecordNo],
        '@key')
  else
    Result := dmDatabase.GetStoredProcOutputParam(
        'spNextKey',
        ['@TableName', TableRule.TableName],
        '@Key');
end;  // TKeyFieldGenerator.Value 

{ ------------------------------------------------------------------------------
}
function TKeyFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;  
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY) then
    Result := RecordIdKeySQL(Tables, CT_KEY_MAPMATE_KEY, CT_KEY_MAPMATE_KEY, True)
  else if ColumnMapping.KeyIsMapped(CT_KEY_RECORD_ID) then
    Result := RecordIdKeySQL(Tables, CT_KEY_SITE_ID, CT_KEY_RECORD_ID, False)
  else if ColumnMapping.KeyIsMapped(CT_KEY_SITE_ID) then
    Result := GeneratedKeySQL(
        Tables,
        FieldValueSQL(Tables, CT_KEY_SITE_ID, FLD_SITEID))
  else
    Result := GeneratedKeySQL(Tables, 'DEFAULT');
end;

{ ------------------------------------------------------------------------------
}
function TKeyFieldGenerator.GeneratedKeySQL(Tables: TTableSource;
    const SiteId: String): String;
begin
  // TODO: this does not do the right thing if there are multiple output
  //       records for a single @generated record
  Result := Format(
      'dbo.ufn_MakeKey(@base_key, %0:s, %1:s)',
      [Tables.QualifiedField('@generated', 'Generated_Id'), SiteId])
end;

{ ------------------------------------------------------------------------------
}
function TKeyFieldGenerator.RecordIdKeySQL(Tables: TTableSource;
    const SiteColumn, RecordColumn: String; IsMapMateKey: Boolean): String;
var
  SiteId: String;
  RecordId: String;

  { ----------------------------------------------------------------------------
    An SQL expression giving the current maximum of the unique index part of
    the record key for the current site id and record id (see TSD 1.18.2.11)
    in the output table, #<TableName>.  This expression will evaluate to NULL
    if there are no such records in #<TableName>.
    Also consider whether it's for a MapMate Key or not.
  }
  function BaseUniqueIndexSQL: String;
  begin
    if IsMapMateKey then
      Result := '(SELECT MAX(SUBSTRING(' + OutputField.Name + ', 9, 1))'
          + ' FROM #' + OutputField.TableRule.TableName
          + ' WHERE ' + OutputField.Name
          + ' LIKE ' + SiteId + ' + ''_'' + RIGHT(' + RecordId + ', 7))'
    else
      Result := '(SELECT MAX(SUBSTRING(' + OutputField.Name + ', 9, 2))'
          + ' FROM #' + OutputField.TableRule.TableName
          + ' WHERE ' + OutputField.Name
          + ' LIKE ' + SiteId + ' + ''__'' + RIGHT(' + RecordId + ', 6))';
  end;

begin
  // Use @generated to keep values consistent..
  SiteId   := Tables.QualifiedField('@generated', FLD_SITEID);
  RecordId := Tables.QualifiedField('@generated', FLD_RECORDID);

  // MapMate Key has 'E' padding instead of '0'. Also, RecordId can be up ,
  // to 7 characters long so that needs to be taken into account.
  if IsMapMateKey then
    Result := '(SELECT ' + SiteId + #10#9#9
            + ' + CASE'#10#9#9#9
                + 'WHEN ' + BaseUniqueIndexSQL + ' IS NULL THEN '#10#9#9#9#9
                    // The first key should be "unspoiled", only the next ones get the increment.
                    + 'CASE WHEN COUNT(*) = 0 THEN ' + RecordId + #10#9#9#9#9
                    + 'ELSE dbo.ufn_Base36Sum(''0'', COUNT(*)) + RIGHT(' + RecordId + ', 7)'#10#9#9#9#9
                    + 'END'#10#9#9#9
                + 'ELSE dbo.ufn_Base36Sum('#10#9#9#9#9
                    + BaseUniqueIndexSQL + ','#10#9#9#9#9
                    + '1 + COUNT(*))'#10#9#9#9
                    + '+ RIGHT(' + RecordId + ', 7)'#10#9#9
            + 'END'#10#9
        + 'FROM @generated AS p'#10#9
        + 'WHERE p.Site_ID = ' + SiteId + #10#9
        + 'AND p.Record_ID = ' + RecordId + #10#9
        + 'AND p.Generated_Id < '
        + Tables.QualifiedField('@generated', 'Generated_Id') + ')'
  else
    Result := '(SELECT ' + SiteId + #10#9#9
        + '+ RIGHT(''00'''
            + ' + CASE WHEN ' + BaseUniqueIndexSQL + ' IS NULL'#10#9#9#9
                + 'THEN dbo.ufn_Base36Sum(''0'', COUNT(*))'#10#9#9#9
                + 'ELSE dbo.ufn_Base36Sum('#10#9#9#9#9
                    + BaseUniqueIndexSQL + ','#10#9#9#9#9
                    + '1 + COUNT(*))'#10#9#9#9
              + 'END, 2)'#10#9#9
        + '+ RIGHT(' + RecordId + ', 6)'#10#9
        + 'FROM @generated AS p'#10#9
        + 'WHERE p.Site_ID = ' + SiteId + #10#9
        + 'AND p.Record_ID = ' + RecordId + #10#9
        + 'AND p.Generated_Id < '
        + Tables.QualifiedField('@generated', 'Generated_Id') + ')';
end;

{-==============================================================================
    TSpatialReferenceFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSpatialReferenceFieldGenerator.Value(Fields: Fields; FieldIndex:
    Integer): OLEVariant;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE) then
    if FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_SPATIALREF) <> '' then
      case FieldIndex of
        0: Result := FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_SPATIALREF);
        1: Result := FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_SYSTEM);
        2: Result := FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_LAT);
        3: Result := FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_LONG);
        4: Result := FieldValue(Fields, CT_KEY_GRID_REFERENCE, FLD_QUALIFIER);
      else
        InvalidFieldIndex(FieldIndex);
      end
    else
      Result := Null
  else if UserSuppliedData.SingleSpatialRef <> '' then
    case FieldIndex of
      0: Result := UserSuppliedData.SingleSpatialRef;
      1: Result := UserSuppliedData.SingleSpatialRefSystem;
      2: Result := ConvertToLatLong(
                      UserSuppliedData.SingleSpatialRef,
                      UserSuppliedData.SingleSpatialRefSystem).Lat;
      3: Result := ConvertToLatLong(
                      UserSuppliedData.SingleSpatialRef,
                      UserSuppliedData.SingleSpatialRefSystem).Long;
      4: Result := UserSuppliedData.SingleSpatialRefQualifier;
    else
      InvalidFieldIndex(FieldIndex);
    end
  else
    Result := Null;
end;  // TSpatialReferenceFieldGenerator.Value 

{ ------------------------------------------------------------------------------
}
function TSpatialReferenceFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
var
  Field: String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE) then
  begin
    Result := 'CASE WHEN '
        + FieldValueSQL(Tables, CT_KEY_GRID_REFERENCE, FLD_SPATIALREF)
        + ' IS NOT NULL THEN ';
    case FieldIndex of
      0: Field := FLD_SPATIALREF;
      1: Field := FLD_SYSTEM;
      2: Field := FLD_LAT;
      3: Field := FLD_LONG;
      4: Field := FLD_QUALIFIER;
    else
      InvalidFieldIndex(FieldIndex);
    end;
    Result := Result
        + FieldValueSQL(Tables, CT_KEY_GRID_REFERENCE, Field)
        + ' END';
  end
  else if UserSuppliedData.SingleSpatialRef <> '' then
    case FieldIndex of
      0: Result := TransactSqlLiteral(UserSuppliedData.SingleSpatialRef);
      1: Result := TransactSqlLiteral(UserSuppliedData.SingleSpatialRefSystem);
      2: Result := TransactSqlLiteral(ConvertToLatLong(
                      UserSuppliedData.SingleSpatialRef,
                      UserSuppliedData.SingleSpatialRefSystem).Lat);
      3: Result := TransactSqlLiteral(ConvertToLatLong(
                      UserSuppliedData.SingleSpatialRef,
                      UserSuppliedData.SingleSpatialRefSystem).Long);
      4: Result := TransactSqlLiteral(
                      UserSuppliedData.SingleSpatialRefQualifier);
    else
      InvalidFieldIndex(FieldIndex);
    end
  else
    Result := 'NULL';
end;


{-==============================================================================
    TLocationFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
  Location key, either obtained from the import file or supplied 
  by the user. 
}
function TLocationFieldGenerator.LocationKey(Fields: Fields;
    UseViceCounty: Boolean): OLEVariant;
begin
  Result := Null;
  
  if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION) then
    Result := FieldValue(Fields, CT_KEY_LOCATION, FLD_DATA)
  else
  if (UserSuppliedData.SingleLocationKey <> '')
      and (not (ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER)
      or ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE))) then
    Result := UserSuppliedData.SingleLocationKey;

  if (VarType(Result) = varNull) // not matched, or not present
      and UseViceCounty
      and ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER) then
    Result := FieldValue(Fields, CT_KEY_VICE_COUNTY_NUMBER, FLD_KEY);
end;  // TLocationFieldGenerator.LocationKey 

{ ------------------------------------------------------------------------------
  SQL expression that gives the location key, either obtained from the import
  file or supplied by the user. 
}
function TLocationFieldGenerator.LocationKeySQL(Tables: TTableSource;
    UseViceCounty: Boolean): String;
var
  MappedViceCountySQL: String;
begin
  Result := 'NULL';
  
  if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION) then
    Result := FieldValueSQL(Tables, CT_KEY_LOCATION, FLD_DATA)
  else
  if (UserSuppliedData.SingleLocationKey <> '')
      and (not (ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER)
      or ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE))) then
    Result := TransactSQLLiteral(UserSuppliedData.SingleLocationKey);

  if UseViceCounty and ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER) then
  begin
    MappedViceCountySQL :=
        FieldValueSQL(Tables, CT_KEY_VICE_COUNTY_NUMBER, FLD_KEY);
    if Result = 'NULL' then
      Result := MappedViceCountySQL
    else
      Result := 'CASE WHEN ' + Result + ' IS NOT NULL THEN '
          + Result
          + ' ELSE '
          + MappedViceCountySQL
          + ' END';
  end;
end;

{-------------------------------------------------------------------------------
}
function TLocationFieldGenerator.Value(Fields: Fields; FieldIndex: Integer): OLEVariant;
var
  locationName: String;
  location: String;
begin
  case FieldIndex of
    0: Result := LocationKey(Fields, True);
    1:
      begin
        Result := Null;
        if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION_NAME)
           or ColumnMapping.KeyIsMapped(CT_KEY_LOCATION) then
        begin
          if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION_NAME) then
          begin
            locationName := VarToStr(dmDatabase.GetStoredProcOutputParam(
                  'usp_ImportWizard_GetLocationLocationName',
                  ['@record_No', Fields['Record_No'].Value],
                  '@location_name'));
          end;
          if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION) then
          begin
            location := VarToStr(dmDatabase.GetStoredProcOutputParam(
                  'usp_ImportWizard_GetLocationName',
                  ['@record_No', Fields['Record_No'].Value, '@loc_key', LocationKey(Fields, False)],
                  '@location_name'));
          end;
          // At this stage, either Location or LocationName is available.
          // No LocationName available, use Location
          if (locationName = '') then
            Result := location //VI 17143
          else
          // No Location, use LocationName
          if (location = '') then
            Result := locationName
          else
          // Both available, concatenate if no LocationKey, otherwise, just LocationName
          if ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER) then
            Result := location + ', ' + locationName
          else
            if LocationKey(Fields,true)=Null then
              Result := location + ', ' + locationName
          else
            Result := locationName
        end else
        if not (ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER)
           or ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE)) then
          Result := UserSuppliedData.SingleLocationName;
      end;
  else
    InvalidFieldIndex(FieldIndex);
  end;
end;  // TLocationFieldGenerator.Value

{ ------------------------------------------------------------------------------
}
function TLocationFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;

    function ColumnDataFieldValue(Key: String): String;
    begin
      if not ColumnMapping.KeyIsMapped(Key) then
        Result := 'NULL'
      else
        Result := Tables.QualifiedField('#master', Key + '_' + FLD_DATA);
    end;
    
begin
  case FieldIndex of
    0: Result := LocationKeySQL(Tables, True);
    1:
      begin
        Result := 'NULL';
        if ColumnMapping.KeyIsMapped(CT_KEY_LOCATION_NAME)
           or ColumnMapping.KeyIsMapped(CT_KEY_LOCATION) then
        begin
          Result := 'dbo.ufn_GetImportWizardLocationName('
              + ColumnDataFieldValue(CT_KEY_LOCATION_NAME) + ', '
              + ColumnDataFieldValue(CT_KEY_LOCATION) + ', '
              + LocationKeySQL(Tables, False) + ', ';

          if ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER) then
            Result := Result + '1)'
          else
            Result := Result + 'CASE WHEN ' + LocationKeySQL(Tables, True) + ' IS NULL'
                + ' THEN 1 ELSE 0 END)';
        end else
        if not (ColumnMapping.KeyIsMapped(CT_KEY_VICE_COUNTY_NUMBER)
           or ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE)) then
          Result := TransactSQLLiteral(UserSuppliedData.SingleLocationName);
      end;
  else
    InvalidFieldIndex(FieldIndex);
  end;
end;

{-==============================================================================
    TSurveyEventSpatialReferenceFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSurveyEventSpatialReferenceFieldGenerator.Create(Owner:
    TOutputField);
begin
  inherited;
  FLocationGenerator := TLocationFieldGenerator.Create(Owner);
end;  // TSurveyEventSpatialReferenceFieldGenerator.Create


{-------------------------------------------------------------------------------
}
function TSurveyEventSpatialReferenceFieldGenerator.Value(Fields: Fields;
    FieldIndex: Integer): OLEVariant;
var
  lLocationKey: OLEVariant;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE) then
    Result := inherited Value(Fields, FieldIndex)
  else
  begin
    // Here we behave as though the samples in the survey event have
    // different grid references, since it is hard to tell at this point.
    // Any values that are incorrectly set are updated after table rule
    // processing has finished.
    lLocationKey := FLocationGenerator.Value(Fields, 0);
    if VarType(lLocationKey) = varNull then
      Result := Null
    else begin
      if lLocationKey <> mCachedLocationKey then begin
        mCachedLocationSRefRecordset := dmDatabase.GetRecordset(
          'usp_ImportWizard_GetLocationSpatialRef',
          ['@location_key', lLocationKey]);
        mCachedLocationKey := lLocationKey;
      end;
      with mCachedLocationSRefRecordset do
        if Eof then
          Result := Null
        else
          if VarToStr(Fields['Spatial_Ref'].Value) <> '' then
            case FieldIndex of
              0: Result := VarToStr(Fields['Spatial_Ref'].Value);
              1: Result := Fields['Spatial_Ref_System'].Value;
              2: Result := Fields['Lat'].Value;
              3: Result := Fields['Long'].Value;
              4: Result := Fields['Spatial_Ref_Qualifier'].Value;
            else
              InvalidFieldIndex(FieldIndex);
            end
          else
            Result := Null;
    end;
  end;
end;  // TSurveyEventSpatialReferenceFieldGenerator.Value

{ ------------------------------------------------------------------------------
}
function TSurveyEventSpatialReferenceFieldGenerator.GeneratingSQL(
    FieldIndex: Integer; Tables: TTableSource): String;
var
  LocationKeySQL: String;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE) then
    Result := inherited GeneratingSQL(FieldIndex, Tables)
  else
  begin
    // Here we behave as though the samples in the survey event have
    // different grid references, since it is hard to tell at this point.
    // Any values that are incorrectly set are updated after table rule
    // processing has finished.
    LocationKeySQL := FLocationGenerator.GeneratingSQL(0, Tables);
    if LocationKeySQL = 'NULL' then
      Result := 'NULL'
    else
    begin
      Result := '(SELECT CASE WHEN ISNULL(Spatial_Ref, '''') <> '''' THEN ';

      case FieldIndex of
        0: Result := Result + 'Spatial_Ref';
        1: Result := Result + 'Spatial_Ref_System';
        2: Result := Result + 'Lat';
        3: Result := Result + 'Long';
        4: Result := Result + 'Spatial_Ref_Qualifier';
      else
        InvalidFieldIndex(FieldIndex);
      end;

      Result := Result + ' END'
          + ' FROM Location'
          + ' WHERE Location_Key = ' + LocationKeySQL + ')';
    end;
  end;
end;


{-==============================================================================
    TDateFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}

function TDateFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
var
  lDate: TVagueDate;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then begin
    case FieldIndex of
      0: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := TransactSqlLiteral(Trunc(lDate.StartDate));
      1: Result := TransactSqlLiteral(Trunc(lDate.EndDate));
      2: Result := TransactSqlLiteral(lDate.DateTypeString);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;

{--------------------------------------------------------------------------
}
function TDateFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
var
  lDate: TVagueDate;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then
    case FieldIndex of
      0: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := Trunc(lDate.StartDate);
      1: Result := Trunc(lDate.EndDate);
      2: Result := lDate.DateTypeString;
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;  // TDateFieldGenerator.Value

{-==============================================================================
    TDeterminationDateFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDeterminationDateFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
var
  lDate: TVagueDate;
  lDateResult: String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_DETERMINATION_DATE) then begin
    case FieldIndex of
      0:
      begin
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATESTART);
        Result := FieldValueSQL(Tables, CT_KEY_DETERMINATION_DATE, FLD_DATESTART);
        Result := 'CASE WHEN ' + Result + ' <> -1 THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
      1:
      begin
        Result := FieldValueSQL(Tables, CT_KEY_DETERMINATION_DATE, FLD_DATEEND);
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATEEND);
        Result := 'CASE WHEN ' + Result + ' <> -1 THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
      2:
      begin
        Result := FieldValueSQL(Tables, CT_KEY_DETERMINATION_DATE, FLD_DATETYPE);
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATETYPE);
        Result := 'CASE WHEN ' + Result + ' <> ''Z'' THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
    else
        InvalidFieldIndex(FieldIndex);
    end;
  end
  else if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then begin
    case FieldIndex of
      0: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := TransactSqlLiteral(Trunc(lDate.StartDate));
      1: Result := TransactSqlLiteral(Trunc(lDate.EndDate));
      2: Result := TransactSqlLiteral(lDate.DateTypeString);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;

{--------------------------------------------------------------------------
}
function TDeterminationDateFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
var
  lDate: TVagueDate;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_DETERMINATION_DATE) then begin
    Result := FieldValue(Fields, CT_KEY_DETERMINATION_DATE, FLD_DATESTART);
    if (Result <> -1) then begin
      case FieldIndex of
        0: Result := FieldValue(Fields, CT_KEY_DETERMINATION_DATE, FLD_DATESTART);
        1: Result := FieldValue(Fields, CT_KEY_DETERMINATION_DATE, FLD_DATEEND);
        2: Result := FieldValue(Fields, CT_KEY_DETERMINATION_DATE, FLD_DATETYPE);
      else
        InvalidFieldIndex(FieldIndex);
      end
    end
    else begin
      case FieldIndex of
        0: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATESTART);
        1: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATEEND);
        2: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATETYPE);
      else
        InvalidFieldIndex(FieldIndex);
      end
    end
  end
  else if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then
    case FieldIndex of
      0: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := Trunc(lDate.StartDate);
      1: Result := Trunc(lDate.EndDate);
      2: Result := lDate.DateTypeString;
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;  // TDeterminationDateFieldGenerator.Value

{-==============================================================================
    TReviewDateFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TReviewDateFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
var
  lDate: TVagueDate;
  lDateResult: String;
  lDateField : string;
begin
 if ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_DATE) then
    lDatefield := CT_KEY_REVIEW_DATE
  else
   lDatefield := CT_KEY_DETERMINATION_DATE;

  if ColumnMapping.KeyIsMapped(lDateField) then begin
    case FieldIndex of
      0:
      begin
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATESTART);
        Result := FieldValueSQL(Tables, lDateField, FLD_DATESTART);
        Result := 'CASE WHEN ' + Result + ' <> -1 THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
      1:
      begin
        Result := FieldValueSQL(Tables, lDateField, FLD_DATEEND);
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATEEND);
        Result := 'CASE WHEN ' + Result + ' <> -1 THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
      2:
      begin
        Result := FieldValueSQL(Tables, lDateField, FLD_DATETYPE);
        lDateResult := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATETYPE);
        Result := 'CASE WHEN ' + Result + ' <> ''Z'' THEN '
        + Result
        + ' ELSE '
        + lDateResult
        + ' END';
      end;
    else
        InvalidFieldIndex(FieldIndex);
    end;
  end
  else if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then begin
    case FieldIndex of
      0: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValueSQL(Tables, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := TransactSqlLiteral(Trunc(lDate.StartDate));
      1: Result := TransactSqlLiteral(Trunc(lDate.EndDate));
      2: Result := TransactSqlLiteral(lDate.DateTypeString);
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;

{--------------------------------------------------------------------------
}
function TReviewDateFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
var
  lDate: TVagueDate;
  lDateField: string;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_DATE) then
    lDatefield := CT_KEY_REVIEW_DATE
  else
    lDatefield := CT_KEY_DETERMINATION_DATE;

  if ColumnMapping.KeyIsMapped(lDateField) then begin
    Result := FieldValue(Fields, lDateField, FLD_DATESTART);
    if (Result <> -1) then begin
      case FieldIndex of
        0: Result := FieldValue(Fields, lDateField, FLD_DATESTART);
        1: Result := FieldValue(Fields, lDateField, FLD_DATEEND);
        2: Result := FieldValue(Fields, lDateField, FLD_DATETYPE);
      else
        InvalidFieldIndex(FieldIndex);
      end
    end
    else begin
      case FieldIndex of
        0: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATESTART);
        1: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATEEND);
        2: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATETYPE);
      else
        InvalidFieldIndex(FieldIndex);
      end
    end
  end
  else if ColumnMapping.KeyIsMapped(CT_KEY_DATE) then
    case FieldIndex of
      0: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATESTART);
      1: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATEEND);
      2: Result := FieldValue(Fields, CT_KEY_DATE, FLD_DATETYPE);
    else
      InvalidFieldIndex(FieldIndex);
    end
  else
  begin
    lDate := StringToVagueDate(UserSuppliedData.SingleDate);
    case FieldIndex of
      0: Result := Trunc(lDate.StartDate);
      1: Result := Trunc(lDate.EndDate);
      2: Result := lDate.DateTypeString;
    else
      InvalidFieldIndex(FieldIndex);
    end;
  end;
end;  // TReviewDateFieldGenerator.Value


{-==============================================================================
    TSurveyKeyFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TSurveyKeyFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
begin
  Result := TransactSqlLiteral(UserSuppliedData.SurveyKey);
end;

{--------------------------------------------------------------------------
}
function TSurveyKeyFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  Result := UserSuppliedData.SurveyKey;
end;  // TSurveyKeyFieldGenerator.Value

{-==============================================================================
    TCustodianFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCustodianFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_SITE_ID) then
    Result := FieldValueSQL(Tables, CT_KEY_SITE_ID, FLD_SITEID)
  else if ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY) then
    Result := 'LEFT('
        + FieldValueSQL(Tables, CT_KEY_MAPMATE_KEY, FLD_SITEID)
        + ',8)'
  else
    Result := TransactSqlLiteral(AppSettings.SiteID);
end;

{-------------------------------------------------------------------------------
}
function TCustodianFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_SITE_ID) then
    Result := FieldValue(Fields, CT_KEY_SITE_ID, FLD_SITEID)
  else if ColumnMapping.KeyIsMapped(CT_KEY_MAPMATE_KEY) then
    Result := Copy(FieldValue(Fields, CT_KEY_MAPMATE_KEY, FLD_SITEID), 1, 8)
  else
    Result := AppSettings.SiteID;
end;  // TCustodianFieldGenerator.Value
 /////
{-==============================================================================
    TCustodianFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDeterminerPreferredFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
begin
  if NOT ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER_PREFERRED) then
    Result := TransactSqlLiteral(1)
  else
    Result :=  FieldValueSQL(Tables, CT_KEY_REVIEWER_PREFERRED, FLD_BOOLEAN) + ' ^ 1'
end;

{-------------------------------------------------------------------------------
}
function TDeterminerPreferredFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if NOT ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER_PREFERRED) then
    Result := 1
  else 
    Result := not(FieldValue(Fields, CT_KEY_REVIEWER_PREFERRED, FLD_BOOLEAN))

end;  // TDeterminerPreferredFieldGenerator


{-==============================================================================
    TCurrentStatusFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCurrentStatusFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
begin
   case FieldIndex of
    0: Result := TransactSqlLiteral(AppSettings.UserID);
    1: Result := 'GETDATE()';
  else
    InvalidFieldIndex(FieldIndex);
  end;
end;

{--------------------------------------------------------------------------
}
function TCurrentStatusFieldGenerator.Value(Fields: Fields; FieldIndex:
    Integer): OLEVariant;
begin
  case FieldIndex of
    0: Result := AppSettings.UserID;
    1: Result := Today;
  else
    InvalidFieldIndex(FieldIndex);
  end;
end;  // TCurrentStatusFieldGenerator.Value

{-==============================================================================
    TRecordTypeKeyFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TRecordTypeKeyFieldGenerator.GeneratingSQL(FieldIndex: Integer;
  Tables: TTableSource): String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_RECORD_TYPE) then
    Result := FieldValueSQL(Tables, CT_KEY_RECORD_TYPE, FLD_DATA)
  else if ColumnMapping.KeyIsMapped(CT_KEY_BRC_SOURCE) then
    Result := FieldValueSQL(Tables, CT_KEY_BRC_SOURCE, FLD_KEY)
  else
    Result := TransactSqlLiteral(UserSuppliedData.TermKey['Record_Type']);
end;

{-------------------------------------------------------------------------------
}
function TRecordTypeKeyFieldGenerator.Value(Fields: Fields; FieldIndex:
    Integer): OLEVariant;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_RECORD_TYPE) then
    Result := FieldValue(Fields, CT_KEY_RECORD_TYPE, FLD_DATA)
  else if ColumnMapping.KeyIsMapped(CT_KEY_BRC_SOURCE) then
    Result := FieldValue(Fields, CT_KEY_BRC_SOURCE, FLD_KEY)
  else
    Result := UserSuppliedData.TermKey['Record_Type'];
end;  // TRecordTypeKeyFieldGenerator.Value

{-==============================================================================
    TDeterminerFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDeterminerFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  Result := Null;
  if ColumnMapping.KeyIsMapped(CT_KEY_DETERMINER) then
    Result := FieldValue(Fields, CT_KEY_DETERMINER, FLD_DATA);
  if (VarType(Result)=VarNull) then begin
    if ColumnMapping.KeyIsMapped(CT_KEY_OBSERVER) then
    begin
      if UserSuppliedData.UseSpecificDeterminer then
        Result := UserSuppliedData.SingleDeterminerKey
      else
        Result := FieldValue(Fields, CT_KEY_OBSERVER, FLD_NAME);
    end
    else if ColumnMapping.KeyIsMapped(CT_KEY_TEMPOBSERVER) then
    begin
      if UserSuppliedData.UseSpecificDeterminer then
        Result := UserSuppliedData.SingleDeterminerKey
      else
        Result := TransactSQLLiteral(AppSettings.IWTempNameKey)
    end else
      Result := UserSuppliedData.SingleObserverKey;
  end;
end;  // TDeterminerFieldGenerator.Value

{ ------------------------------------------------------------------------------
}
function TDeterminerFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;
var
  MappedDeterminerSQL: String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_OBSERVER) then
  begin
    if UserSuppliedData.UseSpecificDeterminer then
      Result := TransactSQLLiteral(UserSuppliedData.SingleDeterminerKey)
    else
      // first listed observer key
      Result := '(SELECT n.Match_Key'
          + ' FROM #CT_' + CT_KEY_OBSERVER + ' AS o'
          + ' LEFT JOIN #Names AS n'
          + ' ON n.Import_Value = o.name'
          + ' WHERE o.Record_No = '
          + Tables.QualifiedField('@generated', 'Record_No')
          + ' AND o.Position = 0)';
  end
  else if ColumnMapping.KeyIsMapped(CT_KEY_TEMPOBSERVER) then
  begin
    if UserSuppliedData.UseSpecificDeterminer then
      Result := TransactSQLLiteral(UserSuppliedData.SingleDeterminerKey)
    else
      Result := TransactSQLLiteral(AppSettings.IWTempNameKey)
  end else
    Result := TransactSQLLiteral(UserSuppliedData.SingleObserverKey);
  if ColumnMapping.KeyIsMapped(CT_KEY_DETERMINER) then
  begin
    MappedDeterminerSQL := FieldValueSQL(Tables, CT_KEY_DETERMINER, FLD_DATA);
    Result := 'CASE WHEN ' + MappedDeterminerSQL + ' IS NOT NULL THEN '
        + MappedDeterminerSQL
        + ' ELSE '
        + Result
        + ' END';
  end;
end;
 {-==============================================================================
    TReviewerFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TReviewerFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  Result := Null;
  if ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER) then
    Result := FieldValue(Fields, CT_KEY_REVIEWER, FLD_DATA);

end;  // TReviewerFieldGenerator.Value

{ ------------------------------------------------------------------------------
}
function TReviewerFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;
var
  MappedReviewerSQL: String;
begin
  Result := 'NULL';
  if ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER) then
  begin
    MappedReviewerSQL := FieldValueSQL(Tables, CT_KEY_REVIEWER, FLD_DATA);
    Result := 'CASE WHEN ' + MappedReviewerSQL + ' IS NOT NULL THEN '
        + MappedReviewerSQL
        + ' ELSE '
        + Result
        + ' END';
  end;
end;
{-==============================================================================
    TDeterminerRoleFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDeterminerRoleFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_DETERMINER_ROLE) then
    Result := 'NBNSYS0000000003'
  else
  begin
    Result := FieldValue(Fields, CT_KEY_DETERMINER_ROLE, FLD_DATA);
    if VarIsNull(Result) then Result := 'NBNSYS0000000003'
  end
end;

{ ------------------------------------------------------------------------------
}
function TDeterminerRoleFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_DETERMINER_ROLE) then
    Result := TransactSQLLiteral('NBNSYS0000000003')
  else
    Result := 'ISNULL('
        + FieldValueSQL(Tables, CT_KEY_DETERMINER_ROLE, FLD_DATA) + ', '
        + TransactSQLLiteral('NBNSYS0000000003') + ')'
end;

{-==============================================================================
    TReviewerRoleFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TReviewerRoleFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER_ROLE) then
    Result := 'NBNSYS0000000005'
  else
  begin
    Result := FieldValue(Fields, CT_KEY_REVIEWER_ROLE, FLD_DATA);
    if VarIsNull(Result) then Result := 'NBNSYS0000000005'
  end
end;

{ ------------------------------------------------------------------------------
}
function TReviewerRoleFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_REVIEWER_ROLE) then
    Result := TransactSQLLiteral('NBNSYS0000000005')
  else
    Result := 'ISNULL('
        + FieldValueSQL(Tables, CT_KEY_REVIEWER_ROLE, FLD_DATA) + ', '
        + TransactSQLLiteral('NBNSYS0000000005') + ')'
end;


{-==============================================================================
    TDeterminationTypeFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TDeterminationTypeFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_DETERMINATION_TYPE) then
    Result := 'NBNSYS0000000011'
  else
  begin
    Result := FieldValue(Fields, CT_KEY_DETERMINATION_TYPE, FLD_DATA);
    if VarIsNull(Result) then Result := 'NBNSYS0000000011';
  end;
end;

{ ------------------------------------------------------------------------------
}
function TDeterminationTypeFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;
begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_DETERMINATION_TYPE) then
    Result := TransactSQLLiteral('NBNSYS0000000011')
  else
    Result := 'ISNULL('
        + FieldValueSQL(Tables, CT_KEY_DETERMINATION_TYPE, FLD_DATA) + ', '
        + TransactSQLLiteral('NBNSYS0000000011') + ')'
end;
{-==============================================================================
    TReviewTypeFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TReviewTypeFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;

begin
 if not ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_TYPE) then
    Result := 'NBNSYS0000000007'
  else
  begin
    Result := FieldValue(Fields, CT_KEY_REVIEW_TYPE, FLD_DATA);
    if VarIsNull(Result) then Result := 'NBNSYS0000000007';
  end;
end;

{ ------------------------------------------------------------------------------
}
function TReviewTypeFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;

begin
  if not ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_TYPE) then
    Result := TransactSQLLiteral('NBNSYS0000000007')
  else
    Result := 'ISNULL('
        + FieldValueSQL(Tables, CT_KEY_REVIEW_TYPE, FLD_DATA) + ', '
        + TransactSQLLiteral('NBNSYS0000000007') + ')'
end;
{-==============================================================================
    TReviewSpeciesFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TReviewSpeciesFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;

begin
 if ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_SPECIES) then
 begin
   Result := FieldValue(Fields, CT_KEY_REVIEW_SPECIES, FLD_DATA);
 end;
 if VarIsNull(Result) then Result := FieldValue(Fields, CT_KEY_SPECIES, FLD_DATA)

end;

{ ------------------------------------------------------------------------------
}
function TReviewSpeciesFieldGenerator.GeneratingSQL(FieldIndex: Integer;
    Tables: TTableSource): String;

begin
 if not ColumnMapping.KeyIsMapped(CT_KEY_REVIEW_SPECIES) then
   Result := '(' + FieldValueSQL(Tables, CT_KEY_SPECIES, FLD_DATA) + ')'
 else
   Result := 'ISNULL('
        + FieldValueSQL(Tables, CT_KEY_REVIEW_SPECIES, FLD_DATA) + ', '
        + FieldValueSQL(Tables, CT_KEY_SPECIES, FLD_DATA)+ ')'
end;

{-==============================================================================
    TAssociatedOccurenceKeyFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
}
function TAssociatedOccurenceKeyFieldGenerator.Value(Fields: Fields;
    FieldIndex: Integer): OLEVariant;
begin
  Result := dmDatabase.GetStoredProcOutputParam(
      'usp_ImportWizard_GetAssociatedOccurrence',
      ['@record_no', Fields['Record_No'].Value],
      '@taxon_occurrence_key');
end;  // TAssociatedOccurenceKeyFieldGenerator.Value

{ ------------------------------------------------------------------------------
}
function TAssociatedOccurenceKeyFieldGenerator.GeneratingSQL(
    FieldIndex: Integer; Tables: TTableSource): String;
begin
  Result := '(SELECT TOP 1 o.Taxon_Occurrence_Key'
      + ' FROM #RN_Sample AS s'
      + ' INNER JOIN #Taxon_Occurrence AS o'
      + ' ON o.Sample_Key = s.Sample_Key'
      + ' INNER JOIN #Taxon_Determination AS d'
      + ' ON d.Taxon_Occurrence_Key = o.Taxon_Occurrence_Key'
      + ' WHERE s.Record_No = ' + Tables.QualifiedField('#master', 'Record_No')
      + ' AND d.Taxon_List_Item_Key = '
      + FieldValueSQL(Tables, CT_KEY_ASSOCIATED_SPECIES, FLD_DATA) + ')';
end;

{-==============================================================================
    TAbundanceAccuracyFieldGenerator
===============================================================================}
{-------------------------------------------------------------------------------
  If the "Abundance Accuracy" column type is mapped, then returns the value
  from that field; otherwise returns the accuracy parsed from the "Abundance
  Data" column.
}
function TAbundanceAccuracyFieldGenerator.Value(
  Fields: Fields; FieldIndex: Integer): OLEVariant;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_ABUNDANCE_ACCURACY) then
    Result := FieldValue(Fields, CT_KEY_ABUNDANCE_ACCURACY, FLD_DATA)
  else
    Result := FieldValue(Fields, CT_KEY_ABUNDANCE_DATA, FLD_ACCURACY);
end;

{-------------------------------------------------------------------------------
  If the "Abundance Accuracy" column type is mapped, then returns SQL to
  select the values from that field; otherwise returns SQL to select the
  accuracy that was parsed from the "Abundance Data" column.
}
function TAbundanceAccuracyFieldGenerator.GeneratingSQL(
  FieldIndex: Integer; Tables: TTableSource): String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_ABUNDANCE_ACCURACY) then
    Result := FieldValueSQL(Tables, CT_KEY_ABUNDANCE_ACCURACY, FLD_DATA)
  else
    Result := FieldValueSQL(Tables, CT_KEY_ABUNDANCE_DATA, FLD_ACCURACY);
end;

{-==============================================================================
    TObserverFieldGenerator
===============================================================================}
{ ------------------------------------------------------------------------------
}
function TObserverFieldGenerator.Value(Fields: Fields; FieldIndex: Integer):
    OLEVariant;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_OBSERVER) then
    Result := FieldValue(Fields, CT_KEY_OBSERVER, FLD_NAME)
  else if ColumnMapping.KeyIsMapped(CT_KEY_TEMPOBSERVER) then
    Result := TransactSQLLiteral(AppSettings.IWTempNameKey)
  else
    Result := TransactSqlLiteral(UserSuppliedData.SingleObserverKey);
end;

{ ------------------------------------------------------------------------------
}
function TObserverFieldGenerator.GeneratingSQL(
    FieldIndex: Integer; Tables: TTableSource): String;
begin
  if ColumnMapping.KeyIsMapped(CT_KEY_OBSERVER) then
    Result := FieldValueSQL(Tables, CT_KEY_OBSERVER, FLD_NAME)
  else if ColumnMapping.KeyIsMapped(CT_KEY_TEMPOBSERVER) then
    Result := TransactSQLLiteral(AppSettings.IWTempNameKey)
  else
    Result := TransactSqlLiteral(UserSuppliedData.SingleObserverKey);
end;


initialization
  RegisterOutputFieldGenerators(
      [TKeyFieldGenerator,
       TSpatialReferenceFieldGenerator,
       TLocationFieldGenerator,
       TSurveyKeyFieldGenerator,
       TSurveyEventSpatialReferenceFieldGenerator,
       TDateFieldGenerator,
       TDeterminationDateFieldGenerator,
       TReviewDateFieldGenerator,
       TCustodianFieldGenerator,
       TCurrentStatusFieldGenerator,
       TRecordTypeKeyFieldGenerator,
       TDeterminerFieldGenerator,
       TDeterminerPreferredFieldGenerator,
       TReviewerFieldGenerator,
       TDeterminerRoleFieldGenerator,
       TReviewerRoleFieldGenerator,
       TDeterminationTypeFieldGenerator,
       TReviewTypeFieldGenerator,
       TReviewSpeciesFieldGenerator,
       TAssociatedOccurenceKeyFieldGenerator,
       TAbundanceAccuracyFieldGenerator,
       TObserverFieldGenerator]);
end.
