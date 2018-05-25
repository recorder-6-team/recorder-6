//============================================================================
//  Unit: ReportGenerator
//
//  Implements: TReportGenerator, TAttribute, TJoinSQLData, TAttributeSQLData
//
//  Description: Class that provides a TAttributes object to expose the attributes
//               available to the report wizard. After a selection of attributes
//               has been made, the TAttribute is returned to this class
//               to determine which attributes are required for the report.
//               The stored data for each selected attribute is used to populate the
//               TSQLJoinData and TSQLAttributeData objects with the relevent SQL.
//               After pairing up each TSQLJoinData object to one or many
//               TSQLAttributeData objects, the resulting SQL is executed on the
//               server to populate the report table on a column by column basis.
//
//  Author: Ben Collier
//  Created: 25/11/2002
//
//  Last Revision Details:
//    $Revision: 33 $
//    $Date: 10/02/09 10:24 $
//    $Author: Pauldavies $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit ReportGenerator;

interface

uses Classes, Db, ADODB, ReportSQLData, checklst, contnrs, Sysutils, Dialogs,
     controls, forms, ExceptionForm, ComObj, ReportWizardSettings,
     DatabaseAccessADO, Variants;

resourcestring
  ResStr_RptGenTaxon                = 'Taxon';
  ResStr_RptGenBiotope              = 'Biotope';
  ResStr_RptGenSample               = 'Sample';
  ResStr_RptGenDesignationSets      = 'Designation Sets';
  ResStr_RptGenLocation             = 'Location';
  ResStr_RptGenMeasurements         = 'Measurements';
  ResStr_RptGenSampleMeasurements   = 'Sample Measurements';
  ResStr_RptGenLocationMeasurements = 'Location Measurements';

  ResStr_RptGenAllDesignations      = 'All Designations';
  ResStr_RptGenUserFilteredDesignations = 'Selected Designations';
  ResStr_RptGenDesignationLink      = '%s - %s';

  ResStr_RptGenShortNames           = 'Short Names';
  ResStr_RptGenLongNames            = 'Long Names';
  ResStr_RptGenKind                 = 'Kind';
  ResStr_RptGenStatusAbbreviation   = 'Status Abbreviation';
  ResStr_RptGenIsDesignated         = 'Taxon Designated';

  ResStr_RptInvalidFormat           = 'Invalid output format.';

type
  TAttributeType = (atAttribute, atMeasurement, atDesignation);

  TSortType = (stAsc, stDesc, stNone);

//==============================================================================
// Description: Class holding the attribute data and metadata for all fields required
//              by the parent attribute
//
// Author: Ben Collier
// Created: 28/11/2002
//------------------------------------------------------------------------------
type
  TAttribute = class
  private
    FKey: string;
    FDisplayname: string;
    FMeasurementContextTable: string;
    FDesignationParameters: string;
    FGroup: string;
    FAttrType: TAttributeType;
    FAttributeFields: TObjectList;
    FSelected: boolean;
    FPosition: Integer;
    FSort: TSortType;
    FSortOrder: integer;
    FSourceTable: String;
    procedure SetSelected(const Value: boolean);
    procedure SetPosition(const Value: Integer);
    procedure SetSort(const Value: TSortType);
    procedure SetSortOrder(const Value: integer);
    procedure SetSourceTable(const Value: String);
  public
    constructor Create(const AKey, ADisplayName, AGroup, ASourceTable: string; AAttrType : TAttributeType);
    destructor Destroy; override;
    property Key: string read FKey;                              //REPORT_ATTRIBUTE_KEY or MEASUREMENT_QUALIFIER_KEY
    property Name: string read FDisplayName;                     //Attribute's Display Name
    property MeasurementContextTable: string read FMeasurementContextTable //Table from where
                                      write FMeasurementContextTable; //measurement data is found
    property DesignationParameters: string read FDesignationParameters //The parameters to use
                                      write FDesignationParameters;   // in ufn_GetDesignations
    property Group: string read FGroup;                          //Group that attribute belongs to
    property Selected: boolean read FSelected write SetSelected; //Attribute has been selected
    property Position: Integer read FPosition write SetPosition; //Posiiton of the attribute in the table
    property Sort : TSortType read FSort write SetSort;
    property SortOrder : integer read FSortOrder write SetSortOrder;
    property AttrType: TAttributeType read FAttrType;                        //Attribute or Measurement Attribute
    property AttributeFields: TObjectList read FAttributeFields;   //Fields required by attribute
    property SourceTable : String read FSourceTable write SetSourceTable;
  end;

  TPolygonInfo = class
  private
    FMapKey: String;
    FLayerKey: String;
    FPolygonId: Integer;
  public
    constructor Create(const AMapKey, ALayerKey: String; APolygonId: Integer);
    property MapKey: String read FMapKey;
    property LayerKey: String read FLayerKey;
    property PolygonId: Integer read FPolygonId;
  end;

  TReportGenerator = class
  private
    FConnection: TADOConnection;
    FdsMeasurements: TADODataSet;
    FReportTableName: string;
    FSQLData: TObjectList;
    FAttributes: TStringList;
    FGroups : TStringlist;
    FStatusStepSize : Double;
    FCurrentStep: integer;
    FReportType: Char;
    FPolygonSelection: TObjectList;
    FDesignationTypes: TStrings;
    FDesignationSets: TStrings;
    procedure AddGroups(const APath: string);
    procedure ClearAttributeSelection;
    procedure ReadAttributeData;
    procedure DropReportTable;
    procedure CreateReportTable(AstFilterSQL : String);
    procedure CreateReportTableIndexes;
    procedure PopulateSQLObjects;
    procedure PopulateTableKeys;
    procedure PopulateAttributes;
    procedure ExecuteSQL(const istSQL: string; itfStoredProc: boolean = false);
    procedure ReadMeasurementData(const istITEM_GROUP, istMeasurementContextTable: string);
    procedure ReadDesignationData;
    function ReturnMeasurementJOINSQL(const istMeasurementContextTable: string): string;
    function GetSelectAttributes: string;
    function GetSortAttributes: string;
    function GetSortAttributeCount: integer;
    procedure SetCurrentStep(const Value: integer);
    procedure RemoveRestrictedRecords(ASettings: TReportWizardSettings); 
    procedure AddDesignationAttribute(const title, keyValue: string;
        outputIndex: integer; designationSetKey, designationTypeKeys: string);
  public
    constructor Create(iConnection : TADOConnection; desSets: TStrings);
    destructor Destroy; override;
    procedure AddToPolygonSelection(const mapKey, layerKey: String; polygonId: Integer);
    procedure PrepareTable (AReportType : Char; ASettings : TReportWizardSettings);
    procedure PrepareData;
    function HasSelectedAttribute : boolean;
    procedure Clear;
    property Attributes: TStringList read FAttributes;           //Data for all Attributes
    property Groups : TStringList read FGroups; // list of groups attributes are in
    property TableName : string read FReportTableName;
    property SelectAttributes : string read GetSelectAttributes;
    property SortAttributes : string read GetSortAttributes;
    property SortAttributeCount : integer read GetSortAttributeCount;
    function GetAttributeByName(AstName : String): TAttribute;
    property CurrentStep : integer read FCurrentStep write SetCurrentStep;
    property ReportType : Char read FReportType write FReportType;
    property PolygonSelection: TObjectList read FPolygonSelection;
    property DesignationTypes: TStrings read FDesignationTypes write FDesignationTypes;
    property DesignationSets: TStrings read FDesignationSets write FDesignationSets;
  end;

//==============================================================================
// Description: Class holding the metadata for all fields required by the parent
//              attribute
//
// Author: Ben Collier
// Created: 02/12/2002
//------------------------------------------------------------------------------
type
  TAttributeField = class
  private
    FstFieldName: string;
    FstFieldType: string;
    FstFieldSize: string;
    function GetIsMemoField: Boolean;
  public
    constructor Create(const AFieldName, AFieldType, AFieldSize: string);
    property FieldName: string read FstFieldName;
    property FieldType: string read FstFieldType;
    property FieldSize: string read FstFieldSize;
    property IsMemoField: Boolean read GetIsMemoField;
  end;

//==============================================================================
implementation

uses
  GeneralFunctions, StrUtils, Maintbar, GenFuncs, Constants, Wizard, DataClasses;

const
  ST_MEMO_TYPE = 'text';  // Type for memo fields

  SQL_SELECT_ATTRIBUTES = 'SELECT RA.REPORT_ATTRIBUTE_KEY, RA.ITEM_GROUP, ' +
                          '  RA.ITEM_NAME, REPORT_FIELD.FIELD_ITEM_NAME, ' +
                          '  RA.ATTRIBUTE_SQL, ' +
                          '  RA.REPORT_JOIN_KEY, RA.REPORT_WHERE_KEY, ' +
                          '  RA.SOURCE_TABLE, ' +
                          '  REPORT_FIELD.FIELD_TYPE, REPORT_FIELD.FIELD_SIZE, REPORT_JOIN.JOIN_SQL, ' +
                          '  REPORT_WHERE.WHERE_SQL ' +
                          'FROM REPORT_ATTRIBUTE RA ' +
                          'LEFT JOIN REPORT_FIELD ON RA.REPORT_ATTRIBUTE_KEY = REPORT_FIELD.REPORT_ATTRIBUTE_KEY ' +
                          'LEFT JOIN REPORT_JOIN ON RA.REPORT_JOIN_KEY = REPORT_JOIN.REPORT_JOIN_KEY ' +
                          'LEFT JOIN REPORT_WHERE ON RA.REPORT_WHERE_KEY = REPORT_WHERE.REPORT_WHERE_KEY ';

resourcestring
  ResStr_ErrorWhileExecSQL =  'An error occurred executing the following SQL: '#13#10 + '%s';

//==============================================================================
// Description: Initialise the Connection and Dataset to be used by this class,
//              and the DataSource to be returned to the caller.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
constructor TReportGenerator.Create(iConnection: TADOConnection; desSets: TStrings);
begin
  DesignationSets := desSets;

  FConnection := iConnection;
  FConnection.CommandTimeout := 0;
  FdsMeasurements := TADODataSet.Create(nil);
  FGroups := TStringList.Create;   // distinct list of attribute groups
  FGroups.Sorted := True;
  FGroups.Duplicates := dupIgnore;

  ReadAttributeData;  //Populate FAttributes with data from database
                      //Populate FAttributes with measurement data from the following tables:
  ReadMeasurementData(ResStr_RptGenTaxon, TN_TAXON_OCCURRENCE_DATA);
  ReadMeasurementData(ResStr_RptGenBiotope, TN_BIOTOPE_OCCURRENCE_DATA);
  ReadMeasurementData(ResStr_RptGenSample, TN_SAMPLE_DATA);
  ReadMeasurementData(ResStr_RptGenLocation, TN_LOCATION_DATA);
  ReadDesignationData;
  //Set the name of the temporary report table
  FReportTableName := '"#REPORT_OUTPUT"';

  FPolygonSelection := TObjectList.Create;
end;

//==============================================================================
// Description: Tidy up
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
destructor TReportGenerator.Destroy;
  var i : integer;
begin
  FdsMeasurements.Free;
  FSQLData.Free;
  if Assigned(FAttributes) then begin
    for i := 0 to FAttributes.Count -1 do FAttributes.Objects[i].Free;
    FAttributes.Free;
  end;
  FPolygonSelection.Free;
  
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
}
procedure TReportGenerator.Clear;
begin
  ClearAttributeSelection;
  PolygonSelection.Clear;
end;

{===============================================================================
 Description : Prepares the empty Report Output table on the connection.
 Created : 05/12/2002 }
procedure TReportGenerator.PrepareTable(AReportType : Char; ASettings: TReportWizardSettings);
begin
  ReportType := AReportType;
  //Drop the report table if it already exists for this connection
  DropReportTable;
  //Create the report table
  CreateReportTable(ASettings.FilterSQL);
  //Create the Indexes on the report table
  CreateReportTableIndexes;
  //Clear out records that are confidential, unchecked etc
  RemoveRestrictedRecords(ASettings);
end;

{-------------------------------------------------------------------------------
  Remove unchecked, invalid, zero or confidential records as appropriate.
  Note that it seems faster to do this afterwards than to filter them out
  first, because there are no indexes on these fields which causes table scans.
}
procedure TReportGenerator.RemoveRestrictedRecords(ASettings: TReportWizardSettings);
var
  lSql: string;
  lWhereClause: string;

    procedure AddToWhereClause(const AClause: string);
    begin
      if lWhereClause='' then
        lWhereClause := 'WHERE '
      else
        lWhereClause := lWhereClause + ' OR ';
      lWhereClause := lWhereClause + '(' + AClause + ')';
    end;

begin
  lSql := 'DELETE R '+
         'FROM #Report_Output R '+
         'INNER JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=R.Occurrence_Key '+
         '	AND R.Type=''T'' ';
  lWhereClause := '';
  if not ASettings.ShowConfidential then
    AddToWhereClause('XO.Confidential=1');
  if not ASettings.ShowUnchecked then
    AddToWhereClause('XO.Checked<>1');
  if not ASettings.ShowInvalid then
    AddToWhereClause('XO.Verified=1');
  if not ASettings.ShowZeroAbundance then
    AddToWhereClause('XO.Zero_abundance=1');
  // If there is any restriction to apply, then do it
  if lWhereClause<>'' then
    ExecuteSQL(lSQL+lWhereClause);
end;


//==============================================================================
// Description: Bulk of the calling code. Accepts the TAttribute object containing
//              the attributes required by the report wizard, retrieves the data
//              into the #Report_Output table.
// Requirements: The #REPORT_OUTPUT table must already exist for this connection
//               by calling PrepareTable.  Then the caller must populate the
//               occurrence key and type fields.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.PrepareData;
begin
  //Use the selected objects in the TAttribute objectlist to populate the required
  //TJoinSQLData and TAttributeSQLData objects
  PopulateSQLObjects;
  //Populate the five key fields in the report table
  PopulateTableKeys;
  //Populate all selected attributes based upon the key values
  PopulateAttributes;
end;

//==============================================================================
// Description: Returns true if there is at least one selected attribute
//
// Created: 25/11/2002
//------------------------------------------------------------------------------
function TReportGenerator.HasSelectedAttribute : boolean;
var
  liIndex : integer;
begin
  Result := False;
  for liIndex := 0 to FAttributes.Count-1 do begin
    if TAttribute(FAttributes.Objects[liIndex]).Selected then begin
      Result := True;
      break; // from loop
    end;
  end;
end;

//==============================================================================
// Description: Clears the Selected flag from every attribute
//
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.ClearAttributeSelection;
var
  liAttrIndex : integer;
begin
  for liAttrIndex := 0 to FAttributes.Count-1 do
  begin
    TAttribute(FAttributes.Objects[liAttrIndex]).Selected := False;
    TAttribute(FAttributes.Objects[liAttrIndex]).Sort := stNone;
  end;
end;

//==============================================================================
// Description: Populate the FAttributes object with attributes from the database
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.ReadAttributeData();
var
  lstLastAttributeKey: String;
  lstFieldSizeOrNull : String;
  lAttribute         : TAttribute;
  lRs                : _Recordset;
begin
  lAttribute := nil;
  lstLastAttributeKey := '';
  if not FConnection.Connected then FConnection.Open;

  lRs := dmDatabase.ExecuteSQL(SQL_SELECT_ATTRIBUTES+
                                'ORDER BY RA.ITEM_GROUP, RA.ITEM_NAME, RA.REPORT_WHERE_KEY',
                                true);
  //Populate dataset with sql data
  FAttributes := TStringList.Create;
  FAttributes.Sorted := false;
  FAttributes.Duplicates := dupAccept;

  //Add each attribute to the FAttributes Object along with field metadata
  while not lRs.Eof do
  begin
    AddGroups(VarToStr(lRs.Fields['ITEM_GROUP'].Value));
    // Check the field size value and set local var to NULL accordingly
    // Saves having to unecessarily repeat code
    if VarIsNull(lRs.Fields['FIELD_SIZE'].Value) then
      lstFieldSizeOrNull := 'NULL'
    else
      lstFieldSizeOrNull := lRs.Fields['FIELD_SIZE'].Value;

    if (lstLastAttributeKey <> lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value) then
    begin
      lstLastAttributeKey := lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value;
      // Add a new TAttribute object for each new REPORT_ATTRIBUTE_KEY
      lAttribute := TAttribute.Create(
            lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value,
            lRs.Fields['ITEM_NAME'].Value,
            VarToStr(lRs.Fields['ITEM_GROUP'].Value),
            VarToStr(lRs.Fields['SOURCE_TABLE'].Value),
            atAttribute);
      lAttribute.AttributeFields.Add(TAttributeField.Create(
            lRs.Fields['FIELD_ITEM_NAME'].Value,
            lRs.Fields['FIELD_TYPE'].Value,
            lstFieldSizeOrNull));
      FAttributes.AddObject(lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value, lAttribute);
    end else begin
      // Add a new TAttributeField object to the existing TAttribute object
      lAttribute.AttributeFields.Add(TAttributeField.Create(
                lRs.Fields['FIELD_ITEM_NAME'].Value,
                lRs.Fields['FIELD_TYPE'].Value,
                lstFieldSizeOrNull));
    end;
    lRs.MoveNext;
  end;
end;

{-------------------------------------------------------------------------------
  Using the path indicated by an attribute, finds the folders that are inferred
     by this path and add them to the groups list.
}
procedure TReportGenerator.AddGroups(const APath: string);
var
  lIdx: integer;
begin
  lIdx := 1;
  // Scan the path for each parent path and add it to the groups
  while lIdx <= Length(APath) do
  begin
    if APath[lIdx]='\' then
      FGroups.Add(Copy(APath, 1, lIdx-1));
    Inc(lIdx);
  end;
  FGroups.Add(APath);
end;

//==============================================================================
// Description: Populate the FAttributes object with attributes that exist for
//              measurements made within the scope of the initial selection.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.ReadMeasurementData(const istITEM_GROUP,
  istMeasurementContextTable: string);
var
  lcmdMeasurements: TADOCommand;
  liPosition: integer;
  lUsedNames         : TStringList;
  lAttrName          : string;
begin
  FGroups.Add(istItem_Group);

  if not FConnection.Connected then FConnection.Open;

  lcmdMeasurements:= TADOCommand.Create(nil);
  lcmdMeasurements.Connection := FConnection;
  //Setup the command object to return all sql data related to the attributes
  lcmdMeasurements.CommandType := cmdStoredProc;
  lcmdMeasurements.CommandText := 'spRptMeasurementByContextName';
  lcmdMeasurements.Parameters.Refresh;

  //Set the context group (MEASUREMENT_CONTEXT.CONTEXT_NAME) for which we are interested.
  lcmdMeasurements.Parameters[1].Value := istITEM_GROUP;
  // list of names so we can ensure columns are unique
  lUsedNames := TStringList.Create;
  lUsedNames.Sorted := True;
  try
    //Populate dataset with sql data
    FdsMeasurements.Recordset := lcmdMeasurements.Execute;

    //Add each measurement attribute to the FAttributes object
    while not FdsMeasurements.EOF do
    begin
      // Check attribute name is unique
      lAttrName := FdsMeasurements.FieldByName('Description').AsString;
      if lUsedNames.IndexOf(lAttrName)=-1 then
        lUsedNames.AddObject(lAttrName, Pointer(1))
      else begin
        lUsedNames.Objects[lUsedNames.IndexOf(lAttrName)] :=
            Pointer(Integer(lUsedNames.Objects[lUsedNames.IndexOf(lAttrName)])+1);
        lAttrName := lAttrName + ' ' + IntToStr(
            Integer(lUsedNames.Objects[lUsedNames.IndexOf(lAttrName)]));
      end;
      liPosition := FAttributes.AddObject(FdsMeasurements.FieldByName('MEASUREMENT_QUALIFIER_KEY').AsString,
                      TAttribute.Create(FdsMeasurements.FieldByName('MEASUREMENT_QUALIFIER_KEY').AsString,
                                        lAttrName,
                                        istITEM_GROUP, '',
                                        atMeasurement));

      TAttribute(FAttributes.Objects[liPosition]).AttributeFields.Add(TAttributeField.Create(
          lAttrName, 'VARCHAR', '65'));
      //Store the table name where the context data will be joind from
      TAttribute(FAttributes.Objects[liPosition]).MEASUREMENTCONTEXTTABLE := istMeasurementContextTable;
      //end;
      FdsMeasurements.Next;
    end;
  finally
    if assigned(lcmdMeasurements) then lcmdMeasurements.Free;
    lUsedNames.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Populate the FAttributes object with attributes for the designations.
}
procedure TReportGenerator.ReadDesignationData;
var
  i, j: integer;
  keyValue: String;
begin
  // Creates attributes for All designations of the five different types.
  for i := 1 to 5 do
  begin
    AddDesignationAttribute(ResStr_RptGenAllDesignations,
                            Format('TEMPDATA%.8d', [i - 1]),
                            i,
                            'NULL',
                            'NULL');
  end;

  // Creates attributes for the filtered designations of the five types.
  for i := 1 to 5 do
  begin
    AddDesignationAttribute(ResStr_RptGenUserFilteredDesignations,
                            Format('TEMPDATA%.8d', [i + 4]),
                            i,
                            'NULL',
                            '%s');
  end;

  // Creates attributes for all the designation sets for each of the five types.
  for i := 0 to DesignationSets.Count - 1 do
    for j := 1 to 5 do
    begin
      keyValue := TKeyData(DesignationSets.Objects[i]).ItemKey;

      AddDesignationAttribute(DesignationSets[i],
                              keyValue,
                              j,
                              Format('''%s''', [keyValue]),
                              'NULL');
    end;
end;

{-------------------------------------------------------------------------------
  Creates a new attribute for a Taxon Designation, and adds it to the attributes list.
}
procedure TReportGenerator.AddDesignationAttribute(const title, keyValue: string;
    outputIndex: integer; designationSetKey, designationTypeKeys: string);
var
  lAttrName, outputFormat: String;
  newAttribute: TAttribute;
begin
  // Gets the name of the output format for this output index.
  case outputIndex of
    1: outputFormat := ResStr_RptGenShortNames;
    2: outputFormat := ResStr_RptGenLongNames;
    3: outputFormat := ResStr_RptGenKind;
    4: outputFormat := ResStr_RptGenStatusAbbreviation;
    5: outputFormat := ResStr_RptGenIsDesignated;
  else
    raise Exception.Create(ResStr_RptInvalidFormat);
  end;

  lAttrName := Format(ResStr_RptGenDesignationLink, [title, outputFormat]);

  newAttribute := TAttribute.Create(keyValue, lAttrName, title, '', atDesignation);

  FAttributes.AddObject(keyValue, newAttribute);

  newAttribute.AttributeFields.Add(TAttributeField.Create(lAttrName, 'VARCHAR', '1000'));
  newAttribute.DesignationParameters :=
        Format('%d, %s, %s', [outputIndex, designationSetKey, designationTypeKeys]);
end;

//==============================================================================
// Description: Drops the temp report table if it already exists for the
// current connection
//
// Author: Ben Collier
// Created: 03/12/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.DropReportTable;
begin
  ExecuteSQL(Format('IF OBJECT_ID(''tempdb.dbo.%s'') IS NOT NULL DROP TABLE %s',
                    [FReportTableName, FReportTableName]));
end;

//==============================================================================
// Description: Build up the required SQL to create the report table. Also read
//              in all SQL into the TJoinSQLData and TAttributeSQLData objects.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.CreateReportTable(AstFilterSQL : String);
var
  liCounter, liInnerCounter: integer;
  lstCreateTableSQL: string;
  lstNullFields : String;
  lCurrentAttribute: TAttribute;
  lstTextAlter : TStringList;
  i : integer;
begin
  //Create the key fields in the table
  lstCreateTableSQL := AstFilterSQL;
  lstTextAlter := TStringList.Create();
  try
    lstNullFields := ', Convert(char(16), null) collate SQL_Latin1_General_CP1_CI_AS as SURVEY_KEY, '
        + 'Convert(char(16), null) collate SQL_Latin1_General_CP1_CI_AS as SURVEY_EVENT_KEY, '
        + 'Convert(char(16), null) collate SQL_Latin1_General_CP1_CI_AS as SAMPLE_KEY, '
        + 'Convert(char(16), null) collate SQL_Latin1_General_CP1_CI_AS as LIST_ITEM_KEY ';
      //Loop through all Selected Attribute table fields and create SQL script to create report table
      for liCounter := 0 to (FAttributes.Count -1) do
        if TAttribute(FAttributes.Objects[liCounter]).Selected or
           (TAttribute(FAttributes.Objects[liCounter]).Sort<>stNone) then
        begin
          lCurrentAttribute := TAttribute(FAttributes.Objects[liCounter]);

          //Loop through all attribute table fields
          for liInnerCounter := 0 to (lCurrentAttribute.AttributeFields.Count -1) do
          begin
            With TAttributeField(lCurrentAttribute.AttributeFields.Items[liInnerCounter]) do
            begin
              //Add the field to the report table
              if (CompareText(FieldType, 'Text')<> 0) then
              begin
                lstNullFields := lstNullFields +
                 ', Convert(' + FieldType;
                if (CompareText(FieldType, 'char') = 0) or
                    (CompareText(FieldType, 'varchar') = 0) then
                  lstNullFields := lstNullFields + '(' + FieldSize + ')';

                lstNullFields := lstNullFields + ', Null) ';
                if (CompareText(FieldType, 'char') = 0) or
                    (CompareText(FieldType, 'varchar') = 0) then
                  lstNullFields := lstNullFields + 'Collate SQL_Latin1_general_CP1_CI_AS';
                lstNullFields := lstNullFields + ' as ' +
                    FormatWithSquareBrackets(FieldName);
              end
              else
              begin
                //Text fields cannot be used with Select Distinct, so have to alter
                //the data type of these fields later.
                lstNullFields := lstNullFields + ', Convert(char(1),Null) as ' + FormatWithSquareBrackets(FieldName);
                lstTextAlter.Add('Alter table ' + FReportTableName +
                    ' Alter Column ' + FormatWithSquareBrackets(FieldName) +
                    ' Text Collate SQL_Latin1_General_CP1_CI_AS');
              end;
            end;
          end;
        end;
    lstCreateTableSQL := TrimInsideStr(lstCreateTableSQL);
    lstCreateTableSQL := StringReplace(lstCreateTableSQL, 'AS Type From', 'AS Type' + lstNullFields + ' From', [rfReplaceAll, rfIgnoreCase]);
    //Want Select Into only before the first from. (if there is a union)
    lstCreateTableSQL := StringReplace (lstCreateTableSQL, ' From ', ' into ' + FReportTableName + ' From ', []);
    //Execute the SQL to create the report table
    ExecuteSQL(lstCreateTableSQL);
    //Make sure the collation on the primary key columns is OK
    ExecuteSQL('Alter table ' + FReportTableName + ' ALTER COLUMN OCCURRENCE_KEY ' +
              ' char(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT Null ');
    ExecuteSQL('Alter table ' + FReportTableName +
              ' ALTER COLUMN [TYPE] char(1) COLLATE SQL_LATIN1_General_CP1_CI_AS NOT Null ');
    for i := 0 to lstTextAlter.Count - 1 do
      ExecuteSQL(lstTextAlter[i]);
  finally
    lstTextAlter.Free;
  end;
end;

//==============================================================================
// Description: Create indexes on the report table keys.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.CreateReportTableIndexes();
begin
  //Do not create primary key as the primary key object is visible to all connections
  //made by the current user
  ExecuteSQL('CREATE UNIQUE CLUSTERED INDEX IX_OCCURRENCE_KEY ON ' + FReportTableName +' (OCCURRENCE_KEY, [TYPE])');
  //Other indexes are now created within the stored procedures
end;

//==============================================================================
// Description: Using the selected attributes in the FAttributes, populate the
//              TJoinSQLData and TAttributeSQLData objects with the required SQL.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.PopulateSQLObjects;
var
  liCounter: integer;
  lstCurrentJoinKey: string;
  liPosition: integer;
  lstLastJoinKey: string;
  lstLastWhereKey: string;
  lstLastAttributeKey: string;
  lRs                : _Recordset;
  designationParameters: string;
  designationTypeKeyList: string;
  i: Integer;
  joinSQLData: TJoinSQLData;
  attribute: TAttribute;
begin
  liPosition := -1;
  lRs := dmDatabase.ExecuteSQL(SQL_SELECT_ATTRIBUTES+
                    'ORDER BY RA.REPORT_JOIN_KEY, RA.REPORT_WHERE_KEY',
                    true);
  //Initialise var
  lstLastJoinKey := '';
  lstLastWhereKey := '';

  //Create SQLData object to hold all data as attributes are paired up with
  //their required joins and where clauses
  FSQLData := TObjectList.Create;

  while not lRs.Eof do
  begin
    if (lstLastAttributeKey <> lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value) then
    begin
    //Loop through all attributes that were populated from the REPORT_ATTRIBUTE
    //table in the database and add their SQL data to FSQLData
      for liCounter := 0 to (FAttributes.Count -1) do
        // include selected AND sort fields
        if TAttribute(FAttributes.Objects[liCounter]).Selected
           or (TAttribute(FAttributes.Objects[liCounter]).Sort<>stNone) then
        begin
          if (TAttribute(FAttributes.Objects[licounter]).AttrType = atAttribute) and
             (TAttribute(FAttributes.Objects[licounter]).KEY =
               lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value) then
          //If the current attribute has been selected then include it in the report table
          begin
            //Set the value of the attribute's join key
            if lRs.Fields['REPORT_JOIN_KEY'].Value = '' then
              lstCurrentJoinKey := 'NULL'
            else
              lstCurrentJoinKey := lRs.Fields['REPORT_JOIN_KEY'].Value;

            if (lstLastJoinKey <> lstCurrentJoinKey) or
                (lstLastJoinKey<>VarToStr(lRs.Fields['REPORT_WHERE_KEY'].Value)) then
            begin
            //If the join key has changed then create a new JoinSQLData object
            //containing a new AttributeSQLData object
              liPosition := FSQLData.Add(TJoinSQLData.Create(
                  lstCurrentJoinKey,
                  lRs.Fields['JOIN_SQL'].Value));
              TJoinSQLData(FSQLData[liPosition]).Attributes.Add(TAttributeSQLData.Create(
                  lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value,
                  lRs.Fields['ATTRIBUTE_SQL'].Value,
                  lRs.Fields['ITEM_NAME'].Value,
                  VarToStr(lRs.Fields['WHERE_SQL'].Value)));
            end
            else
            //If the current join key is the same as the last then add a new AttributeSQLData
            //object to the existing JoinSQLData object
              TJoinSQLData(FSQLData[liPosition]).Attributes.Add(TAttributeSQLData.Create(
                  lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value,
                  lRs.Fields['ATTRIBUTE_SQL'].Value,
                  lRs.Fields['ITEM_NAME'].Value,
                  lRs.Fields['WHERE_SQL'].Value));

             //Set current join key as the last join key and advance the dataset to the next record
            lstLastJoinKey := lstCurrentJoinKey;
            lstLastWhereKey := VarToStr(lRs.Fields['REPORT_WHERE_KEY'].Value);
            //Exit for-loop now that a match has been found
            break;
          end;
        end;
    end;
    lstLastAttributeKey := lRs.Fields['REPORT_ATTRIBUTE_KEY'].Value;
    lRs.MoveNext;
  end;

  //Loop through all measurement attributes (NOT populated from the REPORT_ATTRIBUTE)
  //table and add their SQL data to FSQLData
  for liCounter := 0 to (FAttributes.Count -1) do
    if TAttribute(FAttributes.Objects[liCounter]).Selected then
    begin
      if (TAttribute(FAttributes.Objects[licounter]).AttrType = atMeasurement) then
      begin
        liPosition := FSQLData.Add(TJoinSQLData.Create(
            TAttribute(FAttributes.Objects[licounter]).MEASUREMENTCONTEXTTABLE,
            ReturnMeasurementJOINSQL(TAttribute(FAttributes.Objects[licounter]).MEASUREMENTCONTEXTTABLE)));

        TJoinSQLData(FSQLData[liPosition]).Attributes.Add(TAttributeSQLData.Create(
            TAttribute(FAttributes.Objects[licounter]).KEY,
            Format('#REPORT_OUTPUT."%s" = %s.DATA + '' '' + MEASUREMENT_UNIT.Short_name', [
                        StringReplace(TAttribute(FAttributes.Objects[licounter]).NAME, '"', '""', [rfReplaceAll]),
                        TAttribute(FAttributes.Objects[licounter]).MEASUREMENTCONTEXTTABLE
            ]),
            TAttribute(FAttributes.Objects[licounter]).NAME,
            Format(' %s.MEASUREMENT_QUALIFIER_KEY=''%s''',
                        [TAttribute(FAttributes.Objects[licounter]).MEASUREMENTCONTEXTTABLE,
                        TAttribute(FAttributes.Objects[licounter]).KEY])));
      end else if (TAttribute(FAttributes.Objects[licounter]).AttrType = atDesignation) then
      begin
        // Creates the join data and adds it to the SQL Data.
        joinSQLData := TJoinSQLData.Create(TN_TAXON_LIST_ITEM,
                                ReturnMeasurementJOINSQL(TN_TAXON_LIST_ITEM));
        FSQLData.Add(joinSQLData);


        // Gets the additional parameters to pass to the user function
        designationParameters := TAttribute(FAttributes.Objects[licounter]).FDesignationParameters;

        // If there is a space for the list of filtered designations, insert them.
        if Pos('%s', designationParameters) > 0 then
        begin
          if Assigned(DesignationTypes) and (DesignationTypes.Count > 0) then
          begin
            // Gets a list of all the designation types that are being filtered on.
            designationTypeKeyList :=
                TWizardKeyObject(DesignationTypes.Objects[0]).Key;
            for i := 1 to DesignationTypes.Count - 1 do
            begin
              designationTypeKeyList := designationTypeKeyList +
                  ',' + TWizardKeyObject(DesignationTypes.Objects[i]).Key;
            end;
            designationTypeKeyList := Format('''%s''', [designationTypeKeyList]);
          end else begin
            // If none are being filtered on, display all designations.
            designationTypeKeyList := 'NULL';
          end;
          // Add the keys into the parameters.
          designationParameters := Format(designationParameters, [designationTypeKeyList]);
        end;

        // Gets the attribute at this point.
        attribute := TAttribute(FAttributes.Objects[licounter]);

        // Adds the attribute to the join.
        joinSQLData.Attributes.Add(TAttributeSQLData.Create(
            attribute.KEY,
            Format('#REPORT_OUTPUT."%s" = ' +
                       'dbo.ufn_GetDesignations(#REPORT_OUTPUT.LIST_ITEM_KEY, %s)' +
                       'WHERE #REPORT_OUTPUT.TYPE = ''T''',
                   [attribute.NAME,
                    designationParameters]),
            attribute.NAME,
            ''));
      end;
    end;
  //one step for each attribute,
  //3 for the putting in taxon keys,
  //2 for the sample keys & list Item keys
  //2 each for the Survey and Survey event keys
  //1 for the final select.
  FStatusStepSize := 100 /(FSQLData.Count + 10);
  CurrentStep :=3;
end;

//==============================================================================
// Description: Return the relevent JOIN for each type of measurement context
//
// Author: Ben Collier
// Created: 02/12/2002
//------------------------------------------------------------------------------
function TReportGenerator.ReturnMeasurementJOINSQL(const istMeasurementContextTable: string): string;
begin
  if istMeasurementContextTable = TN_TAXON_OCCURRENCE_DATA then
    Result := 'FROM #REPORT_OUTPUT LEFT JOIN TAXON_OCCURRENCE_DATA LEFT JOIN MEASUREMENT_UNIT ' +
	       'ON TAXON_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY = MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY ' +
               'ON #REPORT_OUTPUT.OCCURRENCE_KEY = TAXON_OCCURRENCE_DATA.TAXON_OCCURRENCE_KEY ' +
               'AND #Report_Output.Type = ''T'''
  else if istMeasurementContextTable = TN_BIOTOPE_OCCURRENCE_DATA then
    Result := 'FROM #REPORT_OUTPUT LEFT JOIN BIOTOPE_OCCURRENCE_DATA	LEFT JOIN MEASUREMENT_UNIT ' +
	      'ON BIOTOPE_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY = MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY ' +
              'ON #REPORT_OUTPUT.OCCURRENCE_KEY = BIOTOPE_OCCURRENCE_DATA.BIOTOPE_OCCURRENCE_KEY ' +
              'AND #Report_Output.Type = ''B'''
  else if istMeasurementContextTable = TN_LOCATION_DATA then
    Result := 'FROM #REPORT_OUTPUT LEFT JOIN SAMPLE LEFT JOIN LOCATION_DATA LEFT JOIN MEASUREMENT_UNIT ' +
              'ON LOCATION_DATA.MEASUREMENT_UNIT_KEY = MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY ' +
	      'ON SAMPLE.LOCATION_KEY = LOCATION_DATA.LOCATION_KEY ' +
              'ON #REPORT_OUTPUT.SAMPLE_KEY = SAMPLE.SAMPLE_KEY'
  else if istMeasurementContextTable = TN_SAMPLE_DATA then
    Result := 'FROM #REPORT_OUTPUT LEFT JOIN SAMPLE_DATA LEFT JOIN MEASUREMENT_UNIT ' +
              'ON SAMPLE_DATA.MEASUREMENT_UNIT_KEY = MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY ' +
              'ON #REPORT_OUTPUT.SAMPLE_KEY = SAMPLE_DATA.SAMPLE_KEY'
end;

{===============================================================================
 Description : Returns a comma separated list of the selected attributes,
               suitable for inclusion in a select statement
 Created : 05/12/2002 }
function TReportGenerator.GetSelectAttributes: string;
var
  liAttrIndex, liFieldIndex : integer;
begin
  Result := 'OCCURRENCE_KEY, '
      + 'TYPE, '
      + 'SURVEY_KEY, '
      + 'SURVEY_EVENT_KEY, '
      + 'SAMPLE_KEY, '
      + 'LIST_ITEM_KEY ';
  for liAttrIndex := 0 to FAttributes.Count-1 do
    with TAttribute(FAttributes.Objects[liAttrIndex]) do
      if Selected then
        for liFieldIndex :=0 to AttributeFields.Count -1 do
          With TAttributeField(AttributeFields[liFieldIndex]) do
            if CompareText(RightStr(FieldName, 4), 'time') = 0 then
              AddToCommaSeparatedList(Result, 'Convert(Char(8), ' +
                  FormatWithSquareBrackets(FieldName) + ', 8) as ' +
                  FormatWithSquareBrackets(FieldName))
            else
              AddToCommaSeparatedList(Result, FormatWithSquareBrackets(FieldName));
end;

{===============================================================================
 Description : Returns a comma separated list of the sort attributes,
               suitable for inclusion in a select statement
 Created : 05/12/2002 }
function TReportGenerator.GetSortAttributes: string;
var
  lSortList : TStringlist; // used to sort the sorts by precedence
  liIndex : Integer;
  lstItem : string;
    procedure AddToResult(const stItem : string);
    begin
      if Result = '' then
        Result := stItem
      else
        Result := Result + ', ' + stItem;
    end;
begin
  lSortList := TStringList.Create;
  for liIndex := 0 to FAttributes.Count-1 do begin
    with TAttribute(FAttributes.Objects[liIndex]) do begin
      if Sort<>stNone then begin
        // first character of the string is used to alpha sort by sort precedence.
        lstItem := Chr(SortOrder + Ord('a')) + ' "' + TAttributeField(AttributeFields.Items[0]).FieldName + '"';
        if Sort=stDesc then
          lstItem := lstItem + ' DESC';
        lSortList.Add(lstItem);
      end;
    end;
  end;
  lSortList.Sort;
  Result := ''; // default no sort
  // build sort clause
  for liIndex := 0 to lSortList.count-1 do
    AddToResult(Copy(lSortList[liIndex], 2, 255));  // drop sort character from start of string
  lSortList.Free;
end;


{===============================================================================
 Description : Return the number of sorted attributes
 Created : 5/12/2002 }
function TReportGenerator.GetSortAttributeCount: integer;
var
  liIndex: integer;
begin
  Result := 0;
  for liIndex := 0 to FAttributes.Count-1 do
    if TAttribute(FAttributes.Objects[liIndex]).Sort<>stNone then
      Inc(Result);
end;


//==============================================================================
// Description: Obtain the other foriegn key values from the database and insert
//              them into the report table.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.PopulateTableKeys;
begin
  ExecuteSQL('spRptPopulateListItemKeysANDSampleKeys ''' + ReportType + '''', False);
  CurrentStep := CurrentStep + 2;
  ExecuteSQL('spRptPopulateSurveyEventKeys', True);
  CurrentStep := CurrentStep + 2;
  ExecuteSQL('spRptPopulateSurveyKeys', True);
  CurrentStep := CurrentStep + 2;
end;


//==============================================================================
// Description: For each combination of a set of TAttributesSQLData and TJoinSQLData, run the
//              SQL on the database to populate their attribute columns in the report table
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.PopulateAttributes;
var
  liJoinSQLDataCounter: integer;
  lFAttributesQLDataCounter: integer;
  lCurrentJoinSQLData: TJoinSQLData;
  lCurrentAttributeSQLData: TAttributeSQLData;
  lstPopulateAttributeSQL: string;
begin
  lCurrentAttributeSQLData := nil;
  //Loop through each JoinSQLData in the SQLData ObjectList to build up the SQL
  for liJoinSQLDataCounter := 0 to (FSQLData.Count - 1) do
  begin
    lstPopulateAttributeSQL := 'UPDATE ' + FReportTableName + ' SET ';

    lCurrentJoinSQLData := TJoinSQLData(FSQLData.Items[liJoinSQLDataCounter]);

    //Loop through each AttributeSQLData in the current JoinSQLData object and
    //concatenate each attribute SQL
    for lFAttributesQLDataCounter := 0 to (lCurrentJoinSQLData.Attributes.Count -1) do
    begin
      lCurrentAttributeSQLData := TAttributeSQLData(lCurrentJoinSQLData.Attributes.Items[lFAttributesQLDataCounter]);
      lstPopulateAttributeSQL := lstPopulateAttributeSQL + lCurrentAttributeSQLData.AttributeSQL;

      if lFAttributesQLDataCounter < (lCurrentJoinSQLData.Attributes.Count -1) then
        lstPopulateAttributeSQL := lstPopulateAttributeSQL + ', '
      else
        lstPopulateAttributeSQL := lstPopulateAttributeSQL + ' ';
    end;

    //If the attribute has a join key then concatenate the WHERE statement if it exists
    if Not (lCurrentJoinSQLData.JoinSQL = 'NULL') then
    begin
      lstPopulateAttributeSQL := lstPopulateAttributeSQL + lCurrentJoinSQLData.JoinSQL;
      if Not (lCurrentAttributeSQLData.WhereSQL = '') then
        lstPopulateAttributeSQL := lstPopulateAttributeSQL + ' WHERE ' + lCurrentAttributeSQLData.WhereSQL;
    end;

    //Populate the current attribute field
    ExecuteSQL(lstPopulateAttributeSQL);
    CurrentStep := CurrentStep + 1;
  end;
end;

function TReportGenerator.GetAttributeByName(AstName: String): TAttribute;
  var i : integer;
begin
  for i := 0 to Attributes.Count - 1 do
    if TAttribute(Attributes.Objects[i]).Name = AstName then
    begin
      Result := TAttribute(Attributes.Objects[i]);
      Exit;
    end;
  Result := nil;
end;

//==============================================================================
// Description: Method to execute a given SQL string on the SQL server.
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
procedure TReportGenerator.ExecuteSQL(const istSQL: string; itfStoredProc: boolean = false);
var
  lcmdExecuteSQL: TADOCommand;
  lHoldCursor : TCursor;
begin
  lHoldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  lcmdExecuteSQL := TADOCommand.Create(nil);
  try
    lcmdExecuteSQL.CommandTimeout := 0;
    lcmdExecuteSQL.ParamCheck := false;    
    if not FConnection.Connected then FConnection.Open;
    lcmdExecuteSQL.Connection := FConnection;

    if itfStoredProc then
      lcmdExecuteSQL.CommandType := cmdStoredProc
    else
      lcmdExecuteSQL.CommandType := cmdText;

    lcmdExecuteSQL.CommandText := istSQL;
    lcmdExecuteSQL.Execute;
  finally
    Screen.Cursor := lHoldCursor;
    lcmdExecuteSQL.Free;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TReportGenerator.AddToPolygonSelection(const mapKey, layerKey: String; polygonId: Integer);
begin
  FPolygonSelection.Add(TPolygonInfo.Create(mapKey, layerKey, polygonId));
end;

{-------------------------------------------------------------------------------
}
//==============================================================================
//==============================================================================
{ TAttribute }
//==============================================================================
constructor TAttribute.Create(const AKey, ADisplayName, AGroup, ASourceTable: string;
  AAttrType : TAttributeType);
begin
  inherited Create;
  FKey := AKey;
  FDisplayName := ADisplayName;
  FGroup := AGroup;
  FMeasurementContextTable := '';
  FDesignationParameters := '';
  FSourceTable := ASourceTable;
  FAttrType := AAttrType;
  FSelected := False;
  FAttributeFields := TObjectList.Create;
  FSort := stNone;
end;

destructor TAttribute.Destroy;
begin
  FAttributeFields.Free;
  inherited;
end;

// Accessor method
procedure TAttribute.SetSelected(const Value: boolean);
begin
  FSelected := Value;
end;

//Accessor method
procedure TAttribute.SetPosition(const Value: Integer);
begin
  FPosition := Value;
end;

// Accessor method
procedure TAttribute.SetSort(const Value: TSortType);
begin
  FSort := Value;
  if FSort=stNone then
    FSortOrder:=-1;
end;

// Accessor method
procedure TAttribute.SetSortOrder(const Value: integer);
begin
  FSortOrder := Value;
end;

//==============================================================================
//==============================================================================
{ TAttributeField }
//==============================================================================
constructor TAttributeField.Create(const AFieldName, AFieldType, AFieldSize: string);
begin
  inherited Create;
  FstFieldName := AFieldName;
  FstFieldType := AFieldType;
  FstFieldSize := AFieldSize;
end;

function TAttributeField.GetIsMemoField: Boolean;
begin
  Result := SameText(FieldType, ST_MEMO_TYPE);
end;

//==============================================================================
procedure TReportGenerator.SetCurrentStep(const Value: integer);
begin
  FCurrentStep := Value;
  frmMain.SetProgress(Round(FCurrentStep * FStatusStepSize))
end;

procedure TAttribute.SetSourceTable(const Value: String);
begin
  FSourceTable := Value;
end;

//==============================================================================
//==============================================================================
{ TPolygonInfo }
//==============================================================================
constructor TPolygonInfo.Create(const AMapKey, ALayerKey: String; APolygonId: Integer);
begin
  inherited Create;
  FMapKey := AMapKey;
  FLayerKey := ALayerKey;
  FPolygonId := APolygonId;
end;

end.
