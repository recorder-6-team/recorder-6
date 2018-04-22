{===============================================================================
  Unit:        ReportSQL.pas

  Description: Contains the classes for the ReportSQL section. Also contains
                TReportFile.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 40 $
    $Date: 2/04/09 16:35 $
    $Author: Pauldavies $
===============================================================================}

unit CRReportSQL;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, SMIBase, SMI2TXT,
  Forms, Dialogs, XMLIntf, XMLDoc, DB, ExceptionForm, DBClient, Grids, CRColumns,
  CRCSVTables, CRCommonClasses, ADODB, Constants, CRExternalFilterFiles;

type
  EReportSQLException = class(TExceptionPath)
  end;

  TWhereClauseComponent = class(TObject)
  end;

  // method to callback when dialog completed - to trigger grid display
  TCallbackMethod = procedure of object;

  TDynamicArray = array of Integer;

  TClauseText = class(TWhereClauseComponent)
  private
    FText: String;
  public
    constructor Create(const AText: string); reintroduce; overload;
    property Text: String read FText;
  end;

  TWhereClauseComponents = class(TList)
  private
    function GetItems(AIndex: Integer): TWhereClauseComponent;
    procedure SetItems(AIndex: Integer; AItem: TWhereClauseComponent);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TWhereClauseComponent): Integer;
    procedure Clear; override;
    function Extract(AItem: TWhereClauseComponent): TWhereClauseComponent;
    function First: TWhereClauseComponent;
    function IndexOf(AItem: TWhereClauseComponent): Integer;
    procedure Insert(AIndex: Integer; AItem: TWhereClauseComponent);
    function Last: TWhereClauseComponent;
    function Remove(AItem: TWhereClauseComponent): Integer;
    property Items[AIndex: Integer]: TWhereClauseComponent read GetItems write SetItems;
        default;
  end;

  TWhereClause = class(TObject)
  private
    FComponents: TWhereClauseComponents;
    FKeyType: TKeyType;
    FAddinKeyType: String;
    FPartialOverlap: Boolean;
    function GetInputParam(Index: Integer): TInputParam;
    function GetInputParamCount: Integer;
    function ReadKeyType(ANode: IXMLNode): TKeyType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddWhereClauseComponents(ANode: IXMLNode);
    procedure ReadXML(ANode: IXMLNode);
    property Components: TWhereClauseComponents read FComponents;
    property InputParam[Index: Integer]: TInputParam read GetInputParam;
    property InputParamCount: Integer read GetInputParamCount;
    property KeyType: TKeyType read FKeyType;
    property AddinKeyType: String read FAddinKeyType;
    property PartialOverlap: Boolean read FPartialOverlap;
  end;

  TWhereClauses = class(TList)
  private
    function GetInputParam(Index: Integer): TInputParam;
    function GetInputParamCount: Integer;
    function GetItems(AIndex: Integer): TWhereClause;
    procedure SetItems(AIndex: Integer; AItem: TWhereClause);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TWhereClause): Integer;
    procedure Clear; override;
    function Extract(AItem: TWhereClause): TWhereClause;
    function First: TWhereClause;
    function IndexOf(AItem: TWhereClause): Integer;
    procedure Insert(AIndex: Integer; AItem: TWhereClause);
    function Last: TWhereClause;
    function Remove(AItem: TWhereClause): Integer;
    property InputParam[Index: Integer]: TInputParam read GetInputParam;
    property InputParamCount: Integer read GetInputParamCount;
    property Items[AIndex: Integer]: TWhereClause read GetItems write SetItems; default;
  end;

//////////////////////////////////////////////////////////////////////////////
// Created by KJG
// 17/11/2004
/////////////////////////////////////////////////////////////////////////////
  TOrderByClauseComponent = class(TObject)
  end;

  TOrderByClauseComponents = class(TList)
  private
    function GetItems(AIndex: Integer): TOrderByClauseComponent;
    procedure SetItems(AIndex: Integer; AItem: TOrderByClauseComponent);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TOrderByClauseComponent): Integer;
    procedure Clear; override;
    function Extract(AItem: TOrderByClauseComponent): TOrderByClauseComponent;
    function First: TOrderByClauseComponent;
    function IndexOf(AItem: TOrderByClauseComponent): Integer;
    procedure Insert(AIndex: Integer; AItem: TOrderByClauseComponent);
    function Last: TOrderByClauseComponent;
    function Remove(AItem: TOrderByClauseComponent): Integer;
    property Items[AIndex: Integer]: TOrderByClauseComponent read GetItems write SetItems;
        default;
  end;

  TOrderByClause = class(TObject)
  private
    FName: String;
    FField: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadXML(ANode: IXMLNode);
    property Field: String read FField write FField;
    property Name: String read FName write FName;
  end;

  TOrderByClauses = class(TList)
  private
    FSelected: integer;
    function GetItems(AIndex: Integer): TOrderByClause;
    procedure SetItems(AIndex: Integer; AItem: TOrderByClause);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TOrderByClause): Integer;
    procedure Clear; override;
    function Extract(AItem: TOrderByClause): TOrderByClause;
    function First: TOrderByClause;
    function IndexOf(AItem: TOrderByClause): Integer;
    function FindByValue( AFindString: String): Integer;
    procedure Insert(AIndex: Integer; AItem: TOrderByClause);
    function Last: TOrderByClause;
    function Remove(AItem: TOrderByClause): Integer;
    property Items[AIndex: Integer]: TOrderByClause read GetItems write SetItems; default;
    property Selected: integer read FSelected write FSelected;
  end;
//////////////////////////////////////////////////////////////////////////////
  TReportSQL = class(TObject)
  private
    FSQL: String;
    FWhereClauses: TWhereClauses;
    FOrderByClauses: TOrderByClauses;
    FOutputTableOrderByClauses: TOrderByClauses;
    procedure AddWhereClauses(ANode: IXMLNode);
    procedure AddOrderByClause(ANode: IXMLNode; list: TOrderByClauses);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadXML(ANode: IXMLNode);
    property SQL: String read FSQL;
    property WhereClauses: TWhereClauses read FWhereClauses;
    property OrderByClauses: TOrderByClauses read FOrderByClauses;
    property OutputTableOrderByClauses: TOrderByClauses read FOutputTableOrderByClauses;
  end;

  TClauseCondition = class(TWhereClauseComponent)
  private
    FCSVDataType: TCSVDataType;
    FField: String;
    FInputParam: TInputParam;
    FName: String;
    FOperator: TOperator;
    procedure CheckMakesSense;
    function GetInputParam: TInputParam;
    function ReadCSVDataType(ANode: IXMLNode): TCSVDataType;
    function ReadDataType(ANode: IXMLNode): TParamDataType;
    function ReadOperator(ANode: IXMLNode): TOperator;
    procedure Validate(ANode: IXMLNode; ADataType: TParamDataType);
  public
    destructor Destroy; override;
    procedure AddConditionOptions(ANode: IXMLNode);
    procedure ReadXML(ANode: IXMLNode);
    property CSVDataType: TCSVDataType read FCSVDataType;
    property Field: String read FField;
    property InputParam: TInputParam read GetInputParam;
    property Name: String read FName;
    property Operator: TOperator read FOperator;
  end;

  TBaseXmlQueryFile = class(TObject)
  private
    FCancelCallback: TCallbackMethod;
    FClientDataSet: TClientDataSet;
    FConnection: TADOConnection;
    FCSVTables: TCSVTables;
    FDescription: String;
    FDisplayCallback: TCallbackMethod;
    FHasParamsDialog: Boolean;
    FItemKey: String;
    FKeyType: TKeyType;
    FMenuPath: String;
    FReportSQL: TReportSQL;
    FSMImportFromText: TSMImportFromText;
    FTitle: String;
    FTemplate: string;
    FExternalFilterTables: TExternalFilterTables;
    function AddParamValues(const ASQLString: String): String;
    function BuildBoundingBox(AClause: TClauseCondition): string;
    function BuildClauseCondition(AClause: TClauseCondition): String;
    function BuildCSVString(AClause: TClauseCondition): String;
    procedure BuildCSVTables;
    procedure BuildExternalFilterTables;
    function BuildDateString(AClause: TClauseCondition): String;
    function BuildNormalString(AClause: TClauseCondition): String;
    function BuildVagueDateString(AClause: TClauseCondition): String;
    function BuildSpatialRefString(AClause: TClauseCondition): string;
    function BuildWhereClause: String;
    function BuildOrderByClause: String;
    function BuildOutputTableOrderByClause: String;
    function ConvertOperator(AOperator: TOperator): String;
    procedure CreateStructure(Sender: TObject; Columns: TSMIColumns);
    function GetCurrentWhereClause: TWhereClause;
    function GetInputParam(Index: Integer): TInputParam;
    function GetInputParamCount: Integer;
    procedure GetParams;
    function GetRowData(ACSVDataType: TCSVDataType): String;
    function GetWhereClauseIndex: Integer;
    function InOrNotIn(AOperator: TOperator): string;
    procedure Initialise;
    function RequiresQuotes(AType: TCSVDataType): Boolean;
    procedure SetItemKey(const Value: String);
  protected
    function BuildGridSquareRange(AClause: TClauseCondition): string;
    function BuildSQLString: String; virtual; abstract;
    function ReadExtraNode(ANode: IXMLNode): Boolean; virtual; abstract;
    procedure PopulateTable; virtual; abstract;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    procedure BuildReport;
    function BuildTable(ADisplayCallback, ACancelCallback:
        TCallbackMethod): Boolean;
    procedure CancelReport;
    procedure DropTempTables;
    procedure ReadXML(ANode: IXMLNode);
    property CSVTables: TCSVTables read FCSVTables;
    property Description: String read FDescription;
    property HasParamsDialog: Boolean read FHasParamsDialog;
    property InputParam[Index: Integer]: TInputParam read GetInputParam;
    property InputParamCount: Integer read GetInputParamCount;
    property ItemKey: String read FItemKey write SetItemKey;
    property KeyType: TKeyType read FKeyType write FKeyType;
    property MenuPath: String read FMenuPath;
    property ReportSQL: TReportSQL read FReportSQL write FReportSQL;
    property Template: string read FTemplate;
    property Title: String read FTitle;
    property Connection: TADOConnection read FConnection;
    property CurrentWhereClause: TWhereClause read GetCurrentWhereClause;
    property ExternalFilterTables: TExternalFilterTables read FExternalFilterTables;
  end;

  TReportFile = class(TBaseXmlQueryFile)
  private
    FReportQuery: TADOQuery;
    FReportColumns: TReportColumns;
  protected
    function BuildSQLString: String; override;
    function ReadExtraNode(ANode: IXMLNode): Boolean; override;
    procedure PopulateTable; override;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    function BuildTableForReport(AQuery: TADOQuery; ADisplayCallback, ACancelCallback:
        TCallbackMethod): Boolean;
    property ReportColumns: TReportColumns read FReportColumns;
  end;

  TBatchUpdateFile = class(TBaseXmlQueryFile)
  private
    FCountRecordset: _Recordset;
    FFilterRecordset: _Recordset;
  protected
    function BuildSQLString: String; override;
    function ReadExtraNode(ANode: IXMLNode): Boolean; override;
    procedure PopulateTable; override;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    property CountRecordset: _Recordset read FCountRecordset;
    property FilterRecordset: _Recordset read FFilterRecordset;
  end;

implementation

uses
  CRFrmParametersUnit, DatabaseAccessADO, Variants, CRConstants,ResourceStrings,
  VagueDate, GeneralFunctions, ComObj, StrUtils, SpatialRefFuncs,
  ApplicationSettings, ADOInt;

resourcestring
  ResStr_InvalidOpForBoundingBox =  'Invalid operator for bounding box parameter';
  ResStr_InvalidOpForGridRange =  'Invalid operator for Gridsquarerange parameter - must be equal or not equal';
  ResStr_NodeNotRecognised =  'The node %s is not recognised in this context.';
  ResStr_BatchUpdateError = 'The following error occurred while attempting to run the batch update SQL:' +
      #13#10 + #13#10 + '%s';


{-==============================================================================
    TClauseText
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TClauseText.Create(const AText: string);
begin
  FText := AText;
end;  // TClauseText.Create

{-==============================================================================
    TWhereClause
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TWhereClause.Create;
begin
  inherited;
  FComponents := TWhereClauseComponents.Create;
  FPartialOverlap := True;
end;  // TWhereClause.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TWhereClause.Destroy;
begin
  FComponents.Free;
  inherited;
end;  // TWhereClause.Destroy

{-------------------------------------------------------------------------------
  Add a ClauseCondition or a ClauseText object to the FComponents list.
}
procedure TWhereClause.AddWhereClauseComponents(ANode: IXMLNode);
var
  lWhereClauseComponent: TWhereClauseComponent;
  lIndex: Integer;
begin
  if ANode.NodeName = EL_CLAUSECONDITION then begin
    lWhereClauseComponent := TClauseCondition.Create;
    lIndex := FComponents.Add(lWhereClauseComponent);
    TClauseCondition(FComponents.Items[lIndex]).ReadXML(ANode);
  end else if ANode.NodeName = '#text' then begin
    lWhereClauseComponent := TClauseText.Create(ANode.NodeValue);
    FComponents.Add(lWhereClauseComponent);
  end else
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_ProblemParsingSection, ['WhereClauseComponent']));
end;  // TWhereClause.AddWhereClauseComponents

{-------------------------------------------------------------------------------
  Accessor method - gets the InputParam from the ClauseCondition object.
}
function TWhereClause.GetInputParam(Index: Integer): TInputParam;
var
  i, lClauseConditionIndex: Integer;
begin
  lClauseConditionIndex := 0;
  Result := nil;
  for i := 0 to FComponents.Count - 1 do begin
    if FComponents.Items[i] is TClauseCondition then
      if Index = lClauseConditionIndex then begin
        Result := TClauseCondition(FComponents.Items[i]).InputParam;
        Break;
      end
      else
        Inc(lClauseConditionIndex);
  end;
end;  // TWhereClause.GetInputParam

{-------------------------------------------------------------------------------
  Get the number of parameters.
}
function TWhereClause.GetInputParamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FComponents.Count - 1 do
    if (FComponents.Items[i] is TClauseCondition) then inc(Result);
end;  // TWhereClause.GetInputParamCount

{-------------------------------------------------------------------------------
  Convert the string containing the KeyType read from the XML file into a value of the
      TKeyType enumerated type.
}
function TWhereClause.ReadKeyType(ANode: IXMLNode): TKeyType;
begin
  with ANode do begin
    if      LowerCase(Attributes[AT_KEYTYPE]) = 'default' then
      Result := ktDefault
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'name' then
      Result := ktName
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'location' then
      Result := ktLocation
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'gridsquarerange' then
      Result := ktGridSquareRange
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'spatialref' then
      Result := ktSpatialRef
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'taxon' then
      Result := ktTaxon
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'biotope' then
      Result := ktBiotope
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'admin' then
      Result := ktAdminArea
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'survey' then
      Result := ktSurvey
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'event' then
      Result := ktSurveyEvent
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'sample' then
      Result := ktSample
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'taxon_occurrence' then
      Result := ktTaxonOccurrence
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'biotope_occurrence' then
      Result := ktBiotopeOccurrence
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'samplesinpolygon' then
      Result := ktSamplesInPolygon
    else if LowerCase(Attributes[AT_KEYTYPE]) = 'locationsinpolygon' then
      Result := ktLocationsInPolygon
    else begin
      // It's an addin defined keytype, so we need to store the name
      // so we know which addin defined keytype it is
      FAddinKeyType := LowerCase(Attributes[AT_KEYTYPE]);
      Result := ktAddin;
    end;
  end;
end;  // TWhereClause.ReadKeyType

{-------------------------------------------------------------------------------
  Reads the WhereClause XML node for the report file.
}
procedure TWhereClause.ReadXML(ANode: IXMLNode);
var
  i: Integer;
begin
  with ANode do begin
    if HasAttribute(AT_KEYTYPE) then FKeyType := ReadKeyType(ANode);
    if HasAttribute(AT_INCLUDEPARTIALOVERLAP) then
      FPartialOverlap := not SameText(Attributes[AT_INCLUDEPARTIALOVERLAP], 'no');
    for i := 0 to ChildNodes.Count - 1 do begin
      if (ChildNodes.Nodes[i].NodeName = EL_CLAUSECONDITION) or
         (ChildNodes.Nodes[i].NodeName = '#text') then
        AddWhereClauseComponents(ChildNodes.Nodes[i])
      else
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
            Format(ResStr_ProblemParsingSection, ['WhereClause']));
    end;
  end;
end;  // TWhereClause.ReadXML     

{-==============================================================================
    TReportSQL
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TReportSQL.Create;
begin
  inherited;
  if Assigned(dmDatabase) then
     dmDatabase.Log('TReportSQL.Create');
  FWhereClauses := TWhereClauses.Create;
  FOrderByClauses := TOrderByClauses.Create;
  FOutputTableOrderByClauses := TOrderByClauses.Create;  
end;  // TReportSQL.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TReportSQL.Destroy;
begin
  FWhereClauses.Free;
  FOrderByClauses.Free;
  FOutputTableOrderByClauses.Free;  
  if Assigned(dmDatabase) then
     dmDatabase.Log('TReportSQL.Destroy');
  inherited;
end;  // TReportSQL.Destroy

{-------------------------------------------------------------------------------
  Add a where clause to the WhereClauses list.
}
procedure TReportSQL.AddWhereClauses(ANode: IXMLNode);
var
  lWhereClause: TWhereClause;
  lIndex: Integer;
begin
  if ANode.NodeName = EL_WHERECLAUSE then begin
    lWhereClause := TWhereClause.Create;
    lIndex := FWhereClauses.Add(lWhereClause);
    FWhereClauses.Items[lIndex].ReadXML(ANode);
  end else
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_ProblemParsingSection, ['WhereClauses']));
end;  // TReportSQL.AddWhereClauses

{-------------------------------------------------------------------------------
  Add an order by clause to the OrderByClauses or OutputTableOrderByClauses list.
}
procedure TReportSQL.AddOrderByClause(ANode: IXMLNode; list: TOrderByClauses);
var
  lOrderByClause: TOrderByClause;
begin
  lOrderByClause := TOrderByClause.Create;
  lOrderByClause.ReadXML(ANode);
  list.Add(lOrderByClause);
end;  // TReportSQL.AddOrderByClause

{-------------------------------------------------------------------------------
  Read the SQL or the where clauses in from the XML nodes.
}
procedure TReportSQL.ReadXML(ANode: IXMLNode);
var
  i: Integer;
begin
  with ANode do begin
    for i := 0 to ChildNodes.Count - 1 do begin
      if ChildNodes.Nodes[i].NodeName = '#text' then
        FSQL := ChildNodes[i].NodeValue
      else if ChildNodes.Nodes[i].NodeName = EL_WHERECLAUSE then
        AddWhereClauses(ChildNodes.Nodes[i])
      else if ChildNodes.Nodes[i].NodeName = EL_ORDERBYCLAUSE then
        AddOrderByClause(ChildNodes.Nodes[i], FOrderByClauses)
      else if ChildNodes.Nodes[i].NodeName = EL_OUTPUTTABLEORDERBYCLAUSE then
        AddOrderByClause(ChildNodes.Nodes[i], FOutputTableOrderByClauses)
      else
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
            Format(ResStr_ProblemParsingSection, ['WhereClauses']));
    end;
  end;
end;  // TReportSQL.ReadXML

{-==============================================================================
    TClauseCondition
===============================================================================}
{-------------------------------------------------------------------------------
  Destructor.
}
destructor TClauseCondition.Destroy;
begin
  FInputParam.Free;
  inherited;
end;  // TClauseCondition.Destroy

{-------------------------------------------------------------------------------
  Add a Condition Option to the Input Parameter.
}
procedure TClauseCondition.AddConditionOptions(ANode: IXMLNode);
var
  lConditionOption: TConditionOption;
  lIndex: Integer;
begin
  if ANode.NodeName = EL_CONDITIONOPTION then begin
    lConditionOption := TConditionOption.Create;
    lIndex := FInputParam.Options.Add(lConditionOption);
    FInputParam.Options.Items[lIndex].ReadXML(ANode);
  end else
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_ProblemParsingSection, ['ConditionOptions']));
end;  // TClauseCondition.AddConditionOptions

{-------------------------------------------------------------------------------
  Ensure that the settings in the clause condition make sense
}
procedure TClauseCondition.CheckMakesSense;
begin
  if (FOperator<>opEqual) and (FInputParam.EntryCount<>1) then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_XMLReportEntryCountMustUseEqual, [FName]));
  if (not (FOperator in [opEqual, opNotEqual])) and (FInputParam.DataType in
      [dtVagueDate, dtLocation, dtGridSquareRange, dtSpatialRef,
      dtLocation, dtBoundingBox, dtTaxon, dtName, dtIndividual, dtOrganisation]) then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_XMLReportDataTypeMustUseEqual, [FName]));
  if (FInputParam.DataType in [dtCSVFile, dtDate]) and (FInputParam.EntryCount<>1) then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
      Format(ResStr_XMLReportDataTypeMustUseEqual, [FName]));
  if ((not (FInputParam.DataType in [dtTaxon, dtLocation, dtIndividual, dtOrganisation, dtName]))
      or (FInputParam.EntryCount <> -1)) and (FInputParam.UseRucksack <> urNo) then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
      Format(ResStr_XMLReportParamInvalidUseRucksack, [FName]));
end;

{-------------------------------------------------------------------------------
  Return the input parameter.
}
function TClauseCondition.GetInputParam: TInputParam;
begin
  Result := FInputParam;
end;  // TClauseCondition.GetInputParam

{-------------------------------------------------------------------------------
  Convert the string containing the DataType read from the XML file into a value of the
      TCSVDataType enumerated type.
}
function TClauseCondition.ReadCSVDataType(ANode: IXMLNode): TCSVDataType;
begin
  with ANode do begin
    if      LowerCase(Attributes[AT_DATATYPE]) = 'text' then Result := csvdtText
    else if LowerCase(Attributes[AT_DATATYPE]) = 'number' then Result := csvdtNumber
    else if LowerCase(Attributes[AT_DATATYPE]) = 'date' then Result := csvdtDate
    else
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_InvalidCSVDateType, [Attributes[AT_DATATYPE]]));
  end;
end;  // TClauseCondition.ReadCSVDataType

{-------------------------------------------------------------------------------
  Convert the string containing the Type read from the XML file into a value of the
      TParamDataType enumerated type.
}
function TClauseCondition.ReadDataType(ANode: IXMLNode): TParamDataType;
begin
  with ANode do begin
    if      LowerCase(Attributes[AT_TYPE]) = 'text' then Result := dtText
    else if LowerCase(Attributes[AT_TYPE]) = 'number' then Result := dtNumber
    else if LowerCase(Attributes[AT_TYPE]) = 'date' then Result := dtDate
    else if LowerCase(Attributes[AT_TYPE]) = 'vaguedate' then Result := dtVagueDate
    else if LowerCase(Attributes[AT_TYPE]) = 'truefalse' then Result := dtTrueFalse
    else if LowerCase(Attributes[AT_TYPE]) = 'csvfile' then Result := dtCSVFile
    else if LowerCase(Attributes[AT_TYPE]) = 'optionset' then Result := dtOptionSet
    else if LowerCase(Attributes[AT_TYPE]) = 'location' then Result := dtLocation
    else if LowerCase(Attributes[AT_TYPE]) = 'taxon' then Result := dtTaxon
    else if LowerCase(Attributes[AT_TYPE]) = 'name' then Result := dtName
    else if LowerCase(Attributes[AT_TYPE]) = 'individual' then Result := dtIndividual
    else if LowerCase(Attributes[AT_TYPE]) = 'organisation' then Result := dtOrganisation
    else if LowerCase(Attributes[AT_TYPE]) = 'boundingbox' then Result := dtBoundingBox
    else if LowerCase(Attributes[AT_TYPE]) = 'gridsquarerange' then Result := dtGridSquareRange
    else if LowerCase(Attributes[AT_TYPE]) = 'spatialref' then Result := dtSpatialRef
    else if LowerCase(Attributes[AT_TYPE]) = 'samplesinpolygon' then Result := dtSamplesInPolygon
    else if LowerCase(Attributes[AT_TYPE]) = 'locationsinpolygon' then Result := dtLocationsInPolygon
    else if LowerCase(Attributes[AT_TYPE]) = 'currentuserid' then Result := dtCurrentUserID
    else
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_InvalidDateType, [Attributes[AT_TYPE]]));
  end;
end;  // TClauseCondition.ReadDataType

{-------------------------------------------------------------------------------
  Convert the string containing the Operator read from the XML file into a value of the
      TOperator enumerated type.
}
function TClauseCondition.ReadOperator(ANode: IXMLNode): TOperator;
begin
  with ANode do begin
    if      LowerCase(Attributes[AT_OPERATOR]) = 'equal' then Result := opEqual
    else if LowerCase(Attributes[AT_OPERATOR]) = 'not equal' then Result := opNotEqual
    else if LowerCase(Attributes[AT_OPERATOR]) = 'like' then Result := opLike
    else if LowerCase(Attributes[AT_OPERATOR]) = 'greater than equal' then Result :=
        opGreaterThanEqual
    else if LowerCase(Attributes[AT_OPERATOR]) = 'greater than' then Result := opGreaterThan
    else if LowerCase(Attributes[AT_OPERATOR]) = 'less than equal' then Result :=
        opLessThanEqual
    else if LowerCase(Attributes[AT_OPERATOR]) = 'less than' then Result := opLessThan
    else if LowerCase(Attributes[AT_OPERATOR]) = 'in' then Result := opIn
    else
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_InvalidOperator, [Attributes[AT_OPERATOR]]));
  end;
end;  // TClauseCondition.ReadOperator

{-------------------------------------------------------------------------------
  Reads the ClauseCondition XML node for the report file.
}
procedure TClauseCondition.ReadXML(ANode: IXMLNode);
var
  i: Integer;
  lType: TParamDataType;
begin
  with ANode do begin
    if HasAttribute(AT_TYPE) then
      lType := ReadDataType(ANode)
    else
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterMissing, ['Type', 'Condition']));

    // Now that we know the 'type' we can ensure all the details required
    // by the node are present in the xml. If not, the report generation
    // will be abandoned.
    Validate(ANode, lType);

    if HasAttribute(AT_NAME) then FName := Attributes[AT_NAME];
    if HasAttribute(AT_FIELD) then FField := Attributes[AT_FIELD];
    if HasAttribute(AT_OPERATOR) then FOperator := ReadOperator(ANode);

    // Create input parameter using 'type' value obtained earlier
    if not Assigned(FInputParam) then
    begin
      FInputParam := TInputParam.Create(lType, FName, nil);
      FInputParam.ReadAttributes(ANode);
    end;
    // If DataType is dtCSVFile, need to read in the type of data for the parameter.
    if FInputParam.DataType = dtCSVFile then FCSVDataType := ReadCSVDataType(ANode);

    // The node does have options
    if ChildNodes.Count <> 0 then
      for i := 0 to ChildNodes.Count - 1 do begin
        if (ChildNodes.Nodes[i].NodeName = EL_CONDITIONOPTION) then
          AddConditionOptions(ChildNodes.Nodes[i])
        else
          raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
              Format(ResStr_ProblemParsingSection, ['ClauseCondition']));
      end;
    CheckMakesSense;
  end;
end;  // TClauseCondition.ReadXML

{-------------------------------------------------------------------------------
  Validate the contents of the node to see that all of the required attributes are present.
}
procedure TClauseCondition.Validate(ANode: IXMLNode; ADataType: TParamDataType);
begin
  with ANode do begin
    if (not HasAttribute(AT_NAME)) then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterMissing, ['Name', 'Condition']));
    if (not HasAttribute(AT_OPERATOR)) and (not (ADataType in
        [dtCSVFile, dtSamplesInPolygon, dtLocationsInPolygon])) then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterMissing, ['Operator', 'Condition']));
    if (not HasAttribute(AT_DATATYPE)) and (ADataType = dtCSVFile) then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterMissing, ['DataType', 'Condition']));
    if (not HasAttribute(AT_FIELD)) and (ADataType <> dtVagueDate) then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterMissing, ['Field', 'Condition']));
    if  HasAttribute(AT_Default) and  (not (ADataType in [dtText,dtOptionSet,dtTrueFalse])) then
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ParameterNotSupported, ['Default', 'Condition']));
  end;
end;  // TClauseCondition.Validate

{-==============================================================================
    TBaseXmlQueryFile
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TBaseXmlQueryFile.Create(AConnection: TADOConnection);
begin
  inherited Create;
  if Assigned(dmDatabase) then
     dmDatabase.Log('Create TBaseXmlQueryFile');
  FConnection := AConnection;
  FCSVTables := TCSVTables.Create(FConnection);
  FReportSQL := TReportSQL.Create;
  FSMImportFromText := TSMImportFromText.Create(Application);
  FSMImportFromText.Options := FSMImportFromText.Options - [soShowMessage];
  FSMImportFromText.OnCreateStructure := CreateStructure;
  FClientDataSet := TClientDataSet.Create(Application);
  FHasParamsDialog := false;
  FExternalFilterTables := TExternalFilterTables.Create(FConnection);
  Initialise;
end;  

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TBaseXmlQueryFile.Destroy;
begin
  FCSVTables.Free;
  FReportSQL.Free;
  FClientDataSet.Free;
  FSMImportFromText.Free;
  FExternalFilterTables.Free;
  if Assigned(dmDatabase) then
     dmDatabase.Log('Destroy TBaseXmlQueryFile');
  inherited;
end;  

{-------------------------------------------------------------------------------
  Produces a string made up of the contents of a CSV file. The report will then be able to use
      this to see if a column contains values that are contained in the CSV file.
}
function TBaseXmlQueryFile.BuildCSVString(AClause: TClauseCondition): String;
begin
  FSMImportFromText.SourceFileName := AClause.InputParam.Values[0];
  FSMImportFromText.Execute;
  Result := '';
  with FClientDataSet do begin
    if not (BOF and EOF) then
      First;
    while not EOF do begin
      Result := Result + GetRowData(AClause.CSVDataType);
      Next;
      if not EOF then Result := Result + ', ';
    end;
  end;
  Result := AClause.Field + ' IN (' + Result + ')';
end;  // TBaseXmlQueryFile.BuildCSVString

{-------------------------------------------------------------------------------
  Creates a temp table for each specified CSV table and populates it with the content of the
      CSV file.
}
procedure TBaseXmlQueryFile.BuildCSVTables;
var
  i: Integer;
begin
  for i := 0 to FCSVTables.Count-1 do
    FCSVTables[i].BuildTable;
end;  // TBaseXmlQueryFile.BuildCSVTables

{-------------------------------------------------------------------------------
  Creates a temp table for each specified external filter and populates it
    with the content of the external filter file.
}
procedure TBaseXmlQueryFile.BuildExternalFilterTables;
var
  i: Integer;
begin
  for i := 0 to Pred(FExternalFilterTables.Count) do
    FExternalFilterTables[i].BuildTable;
end;    // TBaseXmlQueryFile.BuildExternalFilterTables

{-------------------------------------------------------------------------------
  This method decides which other methods to call so that they can generate strings that will
      ultimately form the SQL code that is sent to the database to generate the report.
}
function TBaseXmlQueryFile.BuildClauseCondition(AClause: TClauseCondition): String;
begin
  // default - for inactive or substituted input params
  Result := '';
  if AClause.InputParam.Active then begin
    if AClause.InputParam.SubstituteText='' then
      case AClause.InputParam.DataType of
        dtVagueDate : Result := BuildVagueDateString(AClause);
        dtCSVFile   : Result := BuildCSVString(AClause);
        dtDate      : Result := BuildDateString(AClause);
        dtText,
        dtTrueFalse,
        dtOptionSet,
        dtNumber,
        dtLocation,
        dtTaxon,
        dtName,
        dtIndividual,
        dtOrganisation  : Result := BuildNormalString(AClause);
        dtGridSquareRange : Result := BuildGridSquareRange(AClause);
        dtSpatialRef : Result := BuildSpatialRefString(AClause);
        dtBoundingBox : Result := BuildBoundingBox(AClause);
        dtSamplesInPolygon : Result := AClause.Field + ' IN (' + SQL_SAMPLES_FOR_POLYGON + ')';
        dtLocationsInPolygon : Result := AClause.Field + ' IN (' + SQL_SAMPLES_FOR_POLYGON + ')';
        dtCurrentUserID :
          begin
            AClause.InputParam.Values.Clear;
            AClause.InputParam.Values.Add(QuotedStr(AppSettings.UserID));
            Result := BuildNormalString(AClause);
          end;
      else
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
            Format(ResStr_UnsuitableValue, ['DataType']));
      end
  end;
end;  // TBaseXmlQueryFile.BuildClauseCondition  

{-------------------------------------------------------------------------------
  Uses a datediff function to compare 2 dates and determine if they are equal.
}
function TBaseXmlQueryFile.BuildDateString(AClause: TClauseCondition): String;
begin
  Result := 'DATEDIFF(d, ''30 Dec 1899'', ' + AClause.Field + ')';
  if AClause.InputParam.Values.Count=1 then
    Result := Result + ' ' + ConvertOperator(AClause.Operator) + ' ' + AClause.InputParam.Values[0]
  else
    Result := Result + ' ' + InOrNotIn(AClause.Operator) + ' (' +
        StringListToCommaSepList(AClause.InputParam.Values) + ')';
end;  // TReportFile.BuildDateString

{-------------------------------------------------------------------------------
  Creates the SQL clause required to do a grid square range filter
}
function TBaseXmlQueryFile.BuildGridSquareRange(AClause: TClauseCondition): string;
var
  lField: string;
begin
  // if no field specified, or just an alias, then no need to append underscore to get lat/long
  if (AClause.Field='') or (RightStr(AClause.Field, 1)='.') then
    lField := AClause.Field
  else
    lField := AClause.Field + '_';
  lField := lField + 'SPATIAL_REF';
  Result := lField + InorNotIn(AClause.Operator) + '(' +
      StringListToCommaSepList(AClause.InputParam.Values) + ')';
end;

{-------------------------------------------------------------------------------
}
function TBaseXmlQueryFile.AddParamValues(const ASQLString: String): String;   
var
  lPos: Integer;
  lClauseNumber: Integer;
  lParamNumber: Integer;
  lParamContent: string;
begin
  Result := ASQLString;
  //  KJG 9/12/2004
  //  Added to make substitution of the substitute text with the actual supplied
  //  parameter value throughout the SQL which has been generated
  for lClauseNumber := 0 to FReportSQL.WhereClauses.Count-1 do
  begin
    with FReportSQL.WhereClauses[lClauseNumber] do
    for lParamNumber := 0 to InputParamCount - 1 do
    begin
      if InputParam[lParamNumber].SubstituteText <>'' then
      begin
        lParamContent := StringListToCommaSepList(InputParam[lParamNumber].Values);
        lPos := Pos( InputParam[lParamNumber].SubstituteText, Result);
        while lPos <> 0 do
        begin
          Result := leftstr(Result, lPos-1) + lParamContent +
                  RightStr(Result, Length(Result)
                    -(lPos+Length(InputParam[lParamNumber].SubstituteText)-1));
          lPos := Pos(InputParam[lParamNumber].SubstituteText, Result);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Creates the SQL clause required to do a grid square range filter
}
function TBaseXmlQueryFile.BuildBoundingBox(AClause: TClauseCondition): string;
var
  lLT, lGT: string;
  lPos: integer;

    function GetCoord(APos: integer; const ACoordName: string): string;
    var
      lBox: TStringList;
    begin
      lBox := TStringList.Create;
      try
        lBox.CommaText := AClause.InputParam.Values[APos];
        Result := lBox.Values[ACoordName];
      finally
        lBox.Free;
      end;
    end;

begin
  // We swap the LessThan/GreaterThan bits of SQL if using Not Equal
  case AClause.Operator of
    opEqual: begin
        lLt := '<=';
        lGt := '>=';
      end;
    opNotEqual: begin
        lLt := '>=';
        lGt := '<=';
      end;
    else
      raise EReportSQLException.Create(ResStr_InvalidOpForBoundingBox);
  end;
  Result := '(';
  // build a block of tests for each group of 4, i.e. each bounding box
  for lPos := 0 to AClause.InputParam.Values.Count-1 do begin
    if Result<>'(' then Result := Result + ' OR ';
    Result := Result + '(' + AClause.Field + 'Lat' + lGt + GetCoord(lPos, 'SWLat') +
        ' AND ' + AClause.Field + 'Lat' + lLt + GetCoord(lPos, 'NELat') +
        ' AND ' + AClause.Field + 'Long' + lGt + GetCoord(lPos, 'SWLong') +
        ' AND ' + AClause.Field + 'Long' + lLt + GetCoord(lPos, 'NELong') + ')';
  end;
  Result := Result + ')';
end;

{-------------------------------------------------------------------------------
  Builds a standard string using the field name, operator and value to compare it with.
}
function TBaseXmlQueryFile.BuildNormalString(AClause: TClauseCondition): String;
var
  lValue: String;
begin
  lValue := VarToStr(StringListToCommaSepList(AClause.InputParam.Values));
  if AClause.Operator=opLike then begin
    if AClause.InputParam.DataType<>dtText then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          ResStr_InvalidLikeType);
    // prepare parameter for like query - replace * with % and then insert additional % at end
    lValue := StringReplace(lValue, '*', '%', [rfReplaceAll]);
    if RightStr(lValue,1)='''' then
      lValue := LeftStr(lValue, Length(lValue)-1) + '%''';
  end;
  if AClause.InputParam.EntryCount=1 then
    Result := AClause.Field + ' ' + ConvertOperator(AClause.Operator) + ' ' + lValue
  else
    Result := AClause.Field + ' ' + InOrNotIn(AClause.Operator) + ' (' + lValue + ')';
end;  // TBaseXmlQueryFile.BuildNormalString 

{-------------------------------------------------------------------------------
  Builds the where clause SQL for a spatial reference
}
function TBaseXmlQueryFile.BuildSpatialRefString(AClause: TClauseCondition): string;
var
  lFieldPrefix: string;
  lSRef: TLatLong;
  i: integer;
begin
  // if no field specified, or just an alias, then no need to append underscore to get lat/long
  if (AClause.Field='') or (RightStr(AClause.Field, 1)='.') then
    lFieldPrefix := AClause.Field
  else
    lFieldPrefix := AClause.Field + '_';
  if AClause.InputParam.Values.Count>1 then
    Result := '('
  else
    Result := '';
  for i := 0 to AClause.InputParam.Values.Count-1 do begin
    lSRef := ConvertToLatLong(AClause.InputParam.Values[i], AppSettings.SpatialRefSystem);
    Result := Result +
        '(ROUND('+ lFieldPrefix + 'LAT,' + IntToStr(LAT_LONG_SIGFIGS) +
        ')=ROUND(' + FloatToStr(lSRef.Lat) + ', ' + IntToStr(LAT_LONG_SIGFIGS) + ')'+
        ' AND ROUND('+ lFieldPrefix + 'LONG,' + IntToStr(LAT_LONG_SIGFIGS) +
        ')=ROUND(' + FloatToStr(lSRef.Long) + ', ' + IntToStr(LAT_LONG_SIGFIGS) + '))';
    if i<AClause.InputParam.Values.Count-1 then
      Result := Result + ' OR ';
  end;
  if AClause.InputParam.Values.Count>1 then
    Result := Result + ')';
end;

{-------------------------------------------------------------------------------
  This public method is called from the main form. It causes the ParameterEntry form to appear
      if necessary. It then causes the temporary tables to be generated for the CSV files and
      then causes the report to be generated.
}
function TBaseXmlQueryFile.BuildTable(ADisplayCallback,
    ACancelCallback: TCallbackMethod): Boolean;
begin
  if (FReportSQL.OrderByClauses.Count>1) and (FReportSQL.OutputTableOrderByClauses.Count>1) then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_ProblemParsingSection, ['OrderByClauses']));
  Result := True;
  FDisplayCallback := ADisplayCallback;
  FCancelCallback := ACancelCallback;
  //  We only have one OrderBy clause and no other params
  if (FReportSQL.OrderByClauses.Count = 1) then
    FReportSQL.OrderByClauses.Selected := 0;
  if (FReportSQL.OutputTableOrderByClauses.Count = 1) then
    FReportSQL.OutputTableOrderByClauses.Selected := 0;
  //  Have we at least one param or order by clause to choose - display dialog
  if (InputParamCount>0) or (FReportSQL.OrderByClauses.Count > 1) or
      (FReportSQL.OutputTableOrderByClauses.Count > 1) then begin
    FHasParamsDialog := true;
    GetParams;
  end
  else
    BuildReport;
end;  // TBaseXmlQueryFile.BuildTable

{-------------------------------------------------------------------------------
  Build the string used to compare vague dates and ranges of vague dates.
}
function TBaseXmlQueryFile.BuildVagueDateString(AClause: TClauseCondition): String;
var
  lVagueDate: TVagueDate;
  lPrefix: String;
  i: integer;
begin
  Result := '';
  with AClause do begin
    if Operator in [opEqual, opNotEqual] then begin
      if (Field='') or (RightStr(Field, 1)='.') then
        lPrefix := Field
      else
        lPrefix := Field + '_';
      if Operator = opNotEqual then
        Result := Result + 'NOT ';
      if InputParam.Values.Count>1 then
        Result := Result + '(';
      for i := 0 to InputParam.Values.Count-1 do begin
        lVagueDate := StringToVagueDate(InputParam.Values[i]);
        if SameText(lVagueDate.DateTypeString, 'U') then
          Result := Result + '1=1' // we need something, otherwise the SQL structure will break
        else if lVagueDate.DateTypeString[1]='-' then
          // Date is UP TO a given date, so ignore start
          Result := Result + lPrefix + 'VAGUE_DATE_END <= ' + FloatToStr(lVagueDate.EndDate)
        else if lVagueDate.DateTypeString[Length(lVagueDate.DateTypeString)]='-' then
          // Date is FROM a given date, so ignore end
          Result := Result + lPrefix + 'VAGUE_DATE_START' + ' >= ' + FloatToStr(lVagueDate.StartDate)
        else
          Result := Result +
                  '(' + lPrefix + 'VAGUE_DATE_START' + ' >= ' + FloatToStr(lVagueDate.StartDate)
                  + ' AND ' +
                  lPrefix + 'VAGUE_DATE_END <= ' + FloatToStr(lVagueDate.EndDate) + ')';
        if i<InputParam.Values.Count-1 then
          Result := Result + ' OR ';
      end;
      if InputParam.Values.Count>1 then
        Result := Result + ')';
    end else
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          ResStr_VagueDateOperators);
  end;
end;  // TBaseXmlQueryFile.BuildVagueDateString

{-------------------------------------------------------------------------------
  Builds the where clause.
}
function TBaseXmlQueryFile.BuildWhereClause: String;
var
  i: Integer;
begin
  Result := '';
  //  First build where clauses with their params
  with CurrentWhereClause do begin
    for i := 0 to FComponents.Count - 1 do
      if FComponents.Items[i] is TClauseCondition then
        Result := Result + BuildClauseCondition(TClauseCondition(FComponents.Items[i]))
      else if FComponents.Items[i] is TClauseText then
        if (FKeyType = ktDefault) or
            (Pos('%s', TClauseText(FComponents.Items[i]).Text)=0) then
          Result := Result + TClauseText(FComponents.Items[i]).Text
        else
          Result := Result + Format(TClauseText(FComponents.Items[i]).Text, [ItemKey]);
  end;
end;  // TBaseXmlQueryFile.BuildWhereClause

//  KJG 18/11/2004
{-------------------------------------------------------------------------------
  Builds the ORDER BY clause.
}
function TBaseXmlQueryFile.BuildOrderByClause: String;
begin
  Result := ' ';
  with FReportSQL.OrderByClauses do begin
    //  First make sure there is one to be added
    if Count > 0 then begin
      //  Use the Selected parameter value
      result := result + Items[Selected].FField;
    end
  end;
end;  // TBaseXmlQueryFile.BuildOrderByClause

{-------------------------------------------------------------------------------
  Builds the ORDER BY that can be run AFTER building the output table.
}
function TBaseXmlQueryFile.BuildOutputTableOrderByClause: String;
begin
  Result := ' ';
  with FReportSQL.OutputTableOrderByClauses do begin
    //  First make sure there is one to be added
    if Count > 0 then begin
      //  Use the Selected parameter value
      result := result + Items[Selected].FField;
    end
  end;
end;  // TBaseXmlQueryFile.BuildOutputTableOrderByClause

{-------------------------------------------------------------------------------
  Prepare the CSV tables then run the report query.  Public because it
    can be triggered by the params dialog.
}
procedure TBaseXmlQueryFile.BuildReport;
var
  lCursor: TCursor;
begin
  try
    lCursor := HourglassCursor;
    try
      BuildCSVTables;
      BuildExternalFilterTables;
      PopulateTable;
      if Assigned(FDisplayCallback) then
        FDisplayCallback;
    finally
      DefaultCursor(lCursor);
    end; // try
  except
    on E:EOLEException do
      raise EReportSQLException.CreateNonCritical(Format(ResStr_SQLError, [E.Message]));
  end;
end;

{-------------------------------------------------------------------------------
  Method called by the cancel button handler on the params dialog to cancel
    the report
}
procedure TBaseXmlQueryFile.CancelReport;
begin
  if Assigned(FCancelCallback) then
    FCancelCallback;
end;

{-------------------------------------------------------------------------------
  Convert values of the TOperator enumerated type into actual operators that can be used in
      SQL code.
}
function TBaseXmlQueryFile.ConvertOperator(AOperator: TOperator): String;
begin
  case AOperator of
    opEqual             : Result := '=';
    opNotEqual          : Result := '<>';
    opLike              : Result := 'like';
    opGreaterThanEqual  : Result := '>=';
    opGreaterThan       : Result := '>';
    opLessThanEqual     : Result := '<=';
    opLessThan          : Result := '<';
    opIn                : Result := 'IN';
  else
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_UnsuitableValue, ['Operator']));
  end;
end;  // TBaseXmlQueryFile.ConvertOperator

{-------------------------------------------------------------------------------
  Create the structure of FClientDataSet so that the data from the CSV files can be put into
      temporary tables.
}
procedure TBaseXmlQueryFile.CreateStructure(Sender: TObject; Columns: TSMIColumns);
var
  i: Integer;
begin
  FClientDataset.FieldDefs.Clear;

  for i := 0 to Columns.Count-1 do begin
    with FClientDataset.FieldDefs.AddFieldDef do begin
      Name := Columns[i].FieldName;
      DataType := ftMemo;
      //Mantis 212
      Size := 255;
    end;
  end;

  FClientDataset.CreateDataset;
end;  // TBaseXmlQueryFile.CreateStructure

{-------------------------------------------------------------------------------
  Drop the temporary tables. This includes the #REPORT_OUTPUT table and any tables generated
      as a result of the CSV files. One slight problem is that if this drop tables method goes
      wrong, an exception will be thrown and the 'try..finally' on the main form will run.
      Part of the 'finally' code actually causes this method to be called again in an attempt
      to tidy up. However, it is unlikely that this method will go wrong, and it is necessary
      for this method to be called in the 'finally' in case it goes wrong in much more likely
      circumstances elsewhere.
}
procedure TBaseXmlQueryFile.DropTempTables;
var
  lIdx: Integer;
begin
  try
    // Note that the #Report_Output table is left available.
    for lIdx := 0 to FCSVTables.Count - 1 do begin
      TCSVTable(FCSVTables.Items[lIdx]).DropTable;
    end;
  except on E:EOLEException do
    raise EReportSQLException.CreateNonCritical(Format(ResStr_SQLError, [E.Message]));
  end;
end;  // TBaseXmlQueryFile.DropTempTables

{-------------------------------------------------------------------------------
  Accessor to retrieve the currently active where clause
}
function TBaseXmlQueryFile.GetCurrentWhereClause: TWhereClause;
begin
  Result := FReportSQL.WhereClauses.Items[GetWhereClauseIndex];
end;


{-------------------------------------------------------------------------------
  Accessor method - allows access to CSV table, external filter and SQL input parameters.

  KJG 18/11/2004
  This works for the WhereClauses only
}
function TBaseXmlQueryFile.GetInputParam(Index: Integer): TInputParam;
var
  CSVCount, WhereCount: integer;
begin
  CSVCount := FCSVTables.InputParamCount;
  if Index > CSVCount - 1 then begin
    WhereCount := CurrentWhereClause.InputParamCount;
    if Index > CSVCount + WhereCount - 1 then
      Result := FExternalFilterTables.InputParam[Index - CSVCount - WhereCount]
    else
      Result := CurrentWhereClause.InputParam[Index - CSVCount]
  end else begin
    Result := FCSVTables.InputParam[Index];
  end;    // if Index > CSVCount - 1
end;  // TBaseXmlQueryFile.GetInputParam

{-------------------------------------------------------------------------------
  Count the number of parameters contained by the CSVTables and ReportSQL
}
function TBaseXmlQueryFile.GetInputParamCount: Integer;
begin
  Result := FCSVTables.InputParamCount + CurrentWhereClause.InputParamCount
            + FExternalFilterTables.InputParamCount;
end;  // TBaseXmlQueryFile.GetInputParamCount

{-------------------------------------------------------------------------------
  Display the Enter Parameters screen and request a value for each parameter.
      Updates the parameter values.
}
procedure TBaseXmlQueryFile.GetParams;
begin
  with TFrmParameters.Create(nil) do begin
    SetReportFile(self);
    Show;
  end;
end;  // TBaseXmlQueryFile.GetParams

{-------------------------------------------------------------------------------
  There may be values of different types in the CSV file that is being read. The XML file
      stated what data type we are interested in. The following ensures only values of the
      correct type are returned.
}
function TBaseXmlQueryFile.GetRowData(ACSVDataType: TCSVDataType): String;
var
  lIdx: Integer;
  lValue: String;

  function AddToString(AValue: String): string;
  begin
    Result := '';

    if lIdx <> 0 then Result := Result + ', ';
    // Text and Date values require delimiters around them. Numbers don't.
    if RequiresQuotes(ACSVDataType) then
      Result := Result + '''';
    Result := Result + StringReplace(lValue, '''', '''''', [rfReplaceAll]);
    if RequiresQuotes(ACSVDataType) then
      Result := Result + '''';
  end;

begin
  Result := '';
  with FClientDataSet do
    for lIdx := 0 to FieldCount - 1 do begin
      lValue := Fields.Fields[lIdx].Text;

      case ACSVDataType of
        csvdtText   : if not (IsFloat(lValue) and IsDate(lValue)) then Result := Result +
            AddToString(lValue);
        csvdtNumber : if IsFloat(lValue) then Result := Result + AddToString(lValue);
        csvdtDate   : if IsDate(lValue) then Result := Result + AddToString(lValue);
      else
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
            Format(ResStr_UnsuitableValue, ['CSVDataType']));
      end;
    end;
end;  // TBaseXmlQueryFile.GetRowData

{-------------------------------------------------------------------------------
  Retrieve the index of the currently active where clause.
}
function TBaseXmlQueryFile.GetWhereClauseIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FReportSQL.WhereClauses.Count - 1 do begin
    if TWhereClause(FReportSQL.WhereClauses.Items[i]).FKeyType = FKeyType then
      Result := i;
  end;
  if Result = -1 then
    raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
        Format(ResStr_UnsuitableValue, ['KeyType']));
end;  // TBaseXmlQueryFile.GetWhereClauseIndex

{-------------------------------------------------------------------------------
  For IN clause based filters, the Operator is handled differently
}
function TBaseXmlQueryFile.InOrNotIn(AOperator: TOperator): string;
begin
  case AOperator of
    opEqual: result := ' IN ';
    opNotEqual: result := ' NOT IN ';
  else
    raise EReportSQLException.Create(ResStr_InvalidOpForGridRange);
  end;
end;

{-------------------------------------------------------------------------------
  Initialise the FSMImportFromText object.
}
procedure TBaseXmlQueryFile.Initialise;
begin
  with FSMImportFromText do begin
    AnimatedStatus := False;
    FieldDelimiter := fdComma;
    RowFirst := 2;
    DataSet := FClientDataSet;
  end;
end;  // TBaseXmlQueryFile.Initialise

{-------------------------------------------------------------------------------
  Reads the ReportFile XML node for the report file.
}
procedure TBaseXmlQueryFile.ReadXML(ANode: IXMLNode);
var
  i: Integer;
begin
  FCSVTables.Clear;
  if ANode.HasAttribute(AT_MENUPATH) then
    FMenuPath := ANode.Attributes[AT_MENUPATH];
  if ANode.HasAttribute(AT_DESCRIPTION) then
    FDescription := ANode.Attributes[AT_DESCRIPTION];
  if ANode.HasAttribute(AT_TITLE) then
    FTitle := ANode.Attributes[AT_TITLE];
  if ANode.HasAttribute(AT_TEMPLATE) then
    FTemplate := ANode.Attributes[AT_TEMPLATE];
  for i := 0 to ANode.ChildNodes.Count - 1 do begin
    if ANode.ChildNodes.Nodes[i].NodeName = EL_CSVTABLES then
      FCSVTables.ReadXML(ANode.ChildNodes.Nodes[i])
    else if ANode.ChildNodes.Nodes[i].NodeName = EL_EXTERNAL_FILTER_FILES then
      FExternalFilterTables.ReadXML(ANode.ChildNodes.Nodes[i])
    else if ANode.ChildNodes.Nodes[i].NodeName = EL_REPORTSQL then
      FReportSQL.ReadXML(ANode.ChildNodes.Nodes[i])
    else if not ReadExtraNode(ANode.ChildNodes.Nodes[i]) then
      raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_ProblemParsingSection, [ANode.NodeName]) + #13#10 +
          Format(ResStr_NodeNotRecognised, [ANode.ChildNodes.Nodes[i].NodeName]));
  end;
end;  // TBaseXmlQueryFile.ReadXML

{-------------------------------------------------------------------------------
  Method used to determine if quotes should be added to the SQL string, depending on the data
      type.
}
function TBaseXmlQueryFile.RequiresQuotes(AType: TCSVDataType): Boolean;
begin
  Result := AType in [csvdtText, csvdtDate];
end;  // TBaseXmlQueryFile.RequiresQuotes

{-------------------------------------------------------------------------------
  Item key to report, used when the file is linked to a specific data item (i.e. not via Run
      menu).
}
procedure TBaseXmlQueryFile.SetItemKey(const Value: String);
begin
  FItemKey := Value;
end;  // TBaseXmlQueryFile.SetItemKey

{-==============================================================================
    TReportFile
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
  constructor TReportFile.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);

  FReportColumns := TReportColumns.Create;
end;  // TReportFile.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TReportFile.Destroy;
begin
  FReportColumns.Free;

  inherited Destroy;
end;  // TReportFile.Destroy   

{-------------------------------------------------------------------------------
  This method joins the main SQL code from the XML file with the code generated by the
      contents of the where clause. This data is inserted into a temporary table.
}
function TReportFile.BuildSQLString: String;
var
  lPos: Integer;
  lStartPos: Integer;
  lFromFound, lTempListFound : string;
begin
  Result := FReportSQL.SQL + BuildWhereClause + BuildOrderByClause;
  // We want to put the results into a temporary table using 'SELECT .. INTO'.
  // The 'INTO' part goes after the SELECT statements and before the FROM.
  // Note we only modify the LAST query
  //  KJG 19/11/2004
  //  But first check if we have a union statement as this requires the FIRST
  //  Select to be the INTO
  //
  lStartPos := Pos( 'union', Result);
  
  //  We didn't find a union so we can start at the end of the input string
  if lStartPos = 0 then
    lStartPos := Length(Result);

  for lPos := lStartPos downto 1 do begin
    lFromFound := Trim(Copy(Result, lPos, 5));
    lTempListFound := Trim(Copy(Result, lPos+5, 10)); //Checking if it is from #TempList
    if SameText(lFromFound, 'FROM') and (lTempListFound <> '#TempList') then
    begin
      Result := LeftStr(Result, lPos-1) + 'INTO #REPORT_OUTPUT FROM' +
          RightStr(Result, Length(Result)-lPos-3);
      break;
    end;
  end;
  Result := AddParamValues(Result);
end;  // TReportFile.BuildSQLString

{-------------------------------------------------------------------------------
}
function TReportFile.BuildTableForReport(AQuery: TADOQuery; ADisplayCallback, ACancelCallback:
        TCallbackMethod): Boolean;
begin
  FReportQuery := AQuery;
  Result := BuildTable(ADisplayCallback, ACancelCallback);
end;

{-------------------------------------------------------------------------------
  Populates the main report temp table.
}
procedure TReportFile.PopulateTable;
begin
  // Ensure that if for some reason the table already exists, it is dropped.
  FConnection.Execute(Format(SQL_DROPTEMPTABLE, ['#REPORT_OUTPUT', '#REPORT_OUTPUT']),
      cmdText, [eoExecuteNoRecords]);
  // This will execute the SQL code and insert the results into a temporary table.
  FConnection.Execute(BuildSQLString, cmdText, [eoExecuteNoRecords]);
  FReportQuery.SQL.Text := SQL_SELECT_TEMP_TABLE;
  if FReportSQL.OutputTableOrderByClauses.Count>0 then
    FReportQuery.SQL.Text := FReportQuery.SQL.Text + #13#10 + BuildOutputTableOrderByClause;
  FReportQuery.Open;
end;  // TReportFile.PopulateTable

{-------------------------------------------------------------------------------
}
function TReportFile.ReadExtraNode(ANode: IXMLNode): Boolean;
begin
  if ANode.NodeName = EL_REPORTCOLUMNS then begin
    FReportColumns.ReadXML(ANode);
    Result := True;
  end else
    Result := False;
end;  // TReportFile.ReadExtraNode

{-==============================================================================
    TBatchUpdateFile
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TBatchUpdateFile.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;  // TBatchUpdateFile.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TBatchUpdateFile.Destroy;
begin
  inherited Destroy;
end;  // TBatchUpdateFile.Destroy
  
{-------------------------------------------------------------------------------
}
function TBatchUpdateFile.BuildSQLString: String;
var
  lWhereClause: String;
  lOrderByClause: String;
  lPos: Integer;
begin
  lWhereClause := BuildWhereClause;
  lOrderByClause := BuildOrderByClause;
  Result := FReportSQL.SQL;

  lPos := Pos(BU_WHERECLAUSEMARKER, Result);
  if lPos=0 then
    // Report does not specify location of the WHERE clause(s) so revert to
    // normal behaviour of sticking the clause at the end of the SQL
    Result := Result + lWhereClause + lOrderByClause
  else begin
    while lPos <> 0 do
    begin
      Result := leftstr(Result, lPos-1) + lWhereClause +
          RightStr(Result, Length(Result)
          -(lPos+Length(BU_WHERECLAUSEMARKER)-1));
      lPos := Pos(BU_WHERECLAUSEMARKER, Result);
    end;
    Result := AddParamValues(Result);
  end;
end;

{-------------------------------------------------------------------------------
}
function TBatchUpdateFile.ReadExtraNode(ANode: IXMLNode): Boolean;
begin
  Result := False;
end;  // TBatchUpdateFile.ReadExtraNode

{-------------------------------------------------------------------------------
}
procedure TBatchUpdateFile.PopulateTable;
var
  lRS: _Recordset;
  lVariant: OLEVariant;
  i: Integer;
  lFieldNames: TStringList;
begin
  lFieldNames := TStringList.Create;
  AppSettings.BatchUpdateConnection.BeginTrans;
  AppSettings.HasUncommittedUpdates := True;
  try
    lRS := AppSettings.BatchUpdateConnection.Execute(BuildSQLString);
  except on E:Exception do
    begin
      AppSettings.BatchUpdateConnection.RollbackTrans;
      AppSettings.HasUncommittedUpdates := False;
      lFieldNames.Free;
      raise EReportSQLException.CreateNonCritical(Format(ResStr_BatchUpdateError, [E.Message]));
    end;
  end;
  
  try
    while lRS <> nil do begin
      lFieldNames.Clear;
      for i := 0 to lRS.Fields.Count - 1 do
        lFieldNames.Add(lRS.Fields[i].Name);
      if (lFieldNames.Count = 1) and (UpperCase(lFieldNames[0]) = 'COUNT') then
        FCountRecordset := lRS
      else if (lFieldNames.Count >= 2) and (UpperCase(lFieldNames[0]) = 'TABLE')
          and (UpperCase(lFieldNames[1]) = 'KEY') then
        FFilterRecordset := lRS;

      lRS := lRS.NextRecordset(lVariant);
    end;
  finally
    lFieldNames.Free;
  end;
end;

{-==============================================================================
    TWhereClauseComponents
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TWhereClauseComponents.Create;
begin
  inherited Create;
end;  // TWhereClauseComponents.Create

{-------------------------------------------------------------------------------
}
destructor TWhereClauseComponents.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TWhereClauseComponents.Destroy

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.Add(AItem: TWhereClauseComponent): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TWhereClauseComponents.Add

{-------------------------------------------------------------------------------
}
procedure TWhereClauseComponents.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TWhereClauseComponents.Clear

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.Extract(AItem: TWhereClauseComponent): TWhereClauseComponent;
begin
  Result := TWhereClauseComponent(inherited Extract(AItem));
end;  // TWhereClauseComponents.Extract

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.First: TWhereClauseComponent;
begin
  Result := TWhereClauseComponent(inherited First);
end;  // TWhereClauseComponents.First

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.GetItems(AIndex: Integer): TWhereClauseComponent;
begin
  Result := TWhereClauseComponent(inherited Items[AIndex]);
end;  // TWhereClauseComponents.GetItems

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.IndexOf(AItem: TWhereClauseComponent): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TWhereClauseComponents.IndexOf

{-------------------------------------------------------------------------------
}
procedure TWhereClauseComponents.Insert(AIndex: Integer; AItem: TWhereClauseComponent);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TWhereClauseComponents.Insert

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.Last: TWhereClauseComponent;
begin
  Result := TWhereClauseComponent(inherited Last);
end;  // TWhereClauseComponents.Last

{-------------------------------------------------------------------------------
}
function TWhereClauseComponents.Remove(AItem: TWhereClauseComponent): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TWhereClauseComponents.Remove

{-------------------------------------------------------------------------------
}
procedure TWhereClauseComponents.SetItems(AIndex: Integer; AItem: TWhereClauseComponent);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TWhereClauseComponents.SetItems

{-==============================================================================
    TWhereClauses
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TWhereClauses.Create;
begin
  inherited Create;
end;  // TWhereClauses.Create

{-------------------------------------------------------------------------------
}
destructor TWhereClauses.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TWhereClauses.Destroy

{-------------------------------------------------------------------------------
}
function TWhereClauses.Add(AItem: TWhereClause): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TWhereClauses.Add

{-------------------------------------------------------------------------------
}
procedure TWhereClauses.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TWhereClauses.Clear

{-------------------------------------------------------------------------------
}
function TWhereClauses.Extract(AItem: TWhereClause): TWhereClause;
begin
  Result := TWhereClause(inherited Extract(AItem));
end;  // TWhereClauses.Extract

{-------------------------------------------------------------------------------
}
function TWhereClauses.First: TWhereClause;
begin
  Result := TWhereClause(inherited First);
end;  // TWhereClauses.First

{-------------------------------------------------------------------------------
  A Where clause may have multiple input parameters. There may be multiple Where clauses also.
      This method is used to retrieve the correct input parameter.
}
function TWhereClauses.GetInputParam(Index: Integer): TInputParam;
var
  i, lWhereClauseIndex: Integer;
begin
  lWhereClauseIndex := Index;
  Result := nil;

  for i := 0 to Count - 1 do begin
    if TWhereClause(Items[i]).InputParamCount < lWhereClauseIndex then
      lWhereClauseIndex := lWhereClauseIndex - TWhereClause(Items[i]).InputParamCount
    else begin
      Result := Items[i].InputParam[lWhereClauseIndex];
      Break;
    end;
  end;
end;  // TWhereClauses.GetInputParam

{-------------------------------------------------------------------------------
  There may be multiple where clauses. Retrieve the number of Input Params for each where
      clause.
}
function TWhereClauses.GetInputParamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + TWhereClause(Items[i]).InputParamCount;
end;  // TWhereClauses.GetInputParamCount

{-------------------------------------------------------------------------------
}
function TWhereClauses.GetItems(AIndex: Integer): TWhereClause;
begin
  Result := TWhereClause(inherited Items[AIndex]);
end;  // TWhereClauses.GetItems

{-------------------------------------------------------------------------------
}
function TWhereClauses.IndexOf(AItem: TWhereClause): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TWhereClauses.IndexOf

{-------------------------------------------------------------------------------
}
procedure TWhereClauses.Insert(AIndex: Integer; AItem: TWhereClause);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TWhereClauses.Insert

{-------------------------------------------------------------------------------
}
function TWhereClauses.Last: TWhereClause;
begin
  Result := TWhereClause(inherited Last);
end;  // TWhereClauses.Last

{-------------------------------------------------------------------------------
}
function TWhereClauses.Remove(AItem: TWhereClause): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TWhereClauses.Remove

{-------------------------------------------------------------------------------
}
procedure TWhereClauses.SetItems(AIndex: Integer; AItem: TWhereClause);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TWhereClauses.SetItems

//////////////////////////////////////////////////////////////////////////
// KJG added 17/11/2004
//
{-==============================================================================
    TOrderByClauseComponents
===============================================================================}
{-------------------------------------------------------------------------------
}


constructor TOrderByClauseComponents.Create;
begin
  inherited Create;
end;  // TOrderByClauseComponents.Create

{-------------------------------------------------------------------------------
}
destructor TOrderByClauseComponents.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TOrderByClauseComponents.Destroy

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.Add(AItem: TOrderByClauseComponent): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TOrderByClauseComponents.Add

{-------------------------------------------------------------------------------
}
procedure TOrderByClauseComponents.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TOrderByClauseComponents.Clear

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.Extract(AItem: TOrderByClauseComponent): TOrderByClauseComponent;
begin
  Result := TOrderByClauseComponent(inherited Extract(AItem));
end;  // TOrderByClauseComponents.Extract

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.First: TOrderByClauseComponent;
begin
  Result := TOrderByClauseComponent(inherited First);
end;  // TOrderByClauseComponents.First

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.GetItems(AIndex: Integer): TOrderByClauseComponent;
begin
  Result := TOrderByClauseComponent(inherited Items[AIndex]);
end;  // TOrderByClauseComponents.GetItems

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.IndexOf(AItem: TOrderByClauseComponent): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TOrderByClauseComponents.IndexOf

{-------------------------------------------------------------------------------
}
procedure TOrderByClauseComponents.Insert(AIndex: Integer; AItem: TOrderByClauseComponent);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TOrderByClauseComponents.Insert

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.Last: TOrderByClauseComponent;
begin
  Result := TOrderByClauseComponent(inherited Last);
end;  // TOrderByClauseComponents.Last

{-------------------------------------------------------------------------------
}
function TOrderByClauseComponents.Remove(AItem: TOrderByClauseComponent): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TOrderByClauseComponents.Remove

{-------------------------------------------------------------------------------
}
procedure TOrderByClauseComponents.SetItems(AIndex: Integer; AItem: TOrderByClauseComponent);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TOrderByClauseComponents.SetItems

{-==============================================================================
    TOrderByClauses
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TOrderByClauses.Create;
begin
  inherited Create;
end;  // TOrderByClauses.Create

{-------------------------------------------------------------------------------
}
destructor TOrderByClauses.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TOrderByClauses.Destroy

{-------------------------------------------------------------------------------
}
function TOrderByClauses.Add(AItem: TOrderByClause): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TOrderByClauses.Add

{-------------------------------------------------------------------------------
}
procedure TOrderByClauses.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TOrderByClauses.Clear

{-------------------------------------------------------------------------------
}
function TOrderByClauses.Extract(AItem: TOrderByClause): TOrderByClause;
begin
  Result := TOrderByClause(inherited Extract(AItem));
end;  // TOrderByClauses.Extract

{-------------------------------------------------------------------------------
}
function TOrderByClauses.First: TOrderByClause;
begin
  Result := TOrderByClause(inherited First);
end;  // TOrderByClauses.First


{-------------------------------------------------------------------------------
}
function TOrderByClauses.GetItems(AIndex: Integer): TOrderByClause;
begin
  Result := TOrderByClause(inherited Items[AIndex]);
end;  // TOrderByClauses.GetItems

{-------------------------------------------------------------------------------
}
function TOrderByClauses.IndexOf(AItem: TOrderByClause): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TOrderByClauses.IndexOf
{-------------------------------------------------------------------------------
  KJG 18/11/2004
  Finds the OrderBy with the VALUE given in the string parameter
  Returns the index (0 up) or -1 if not found
}

function TOrderByClauses.FindByValue(AFindString: String): Integer;
var
  i : integer;
begin
  result := -1;
  for i:=0 to count-1 do begin
    if Items[i].FField = AFindString then
      result := i;
  end;

end;

{-------------------------------------------------------------------------------
}
procedure TOrderByClauses.Insert(AIndex: Integer; AItem: TOrderByClause);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TOrderByClauses.Insert

{-------------------------------------------------------------------------------
}
function TOrderByClauses.Last: TOrderByClause;
begin
  Result := TOrderByClause(inherited Last);
end;  // TOrderByClauses.Last

{-------------------------------------------------------------------------------
}
function TOrderByClauses.Remove(AItem: TOrderByClause): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TOrderByClauses.Remove

{-------------------------------------------------------------------------------
}
procedure TOrderByClauses.SetItems(AIndex: Integer; AItem: TOrderByClause);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TOrderByClauses.SetItems





constructor TOrderByClause.Create;
begin
  inherited;
end;  // TOrderByClause.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TOrderByClause.Destroy;
begin
  inherited;
end;  // TOrderByClause.Destroy



{-------------------------------------------------------------------------------
  Reads the OrderByClause XML node for the report file.
}
procedure TOrderByClause.ReadXML(ANode: IXMLNode);
begin
  with ANode do begin

        FName := Attributes[AT_NAME];
        FField := Attributes[AT_SQL];
  end;
end;  // TOrderByClause.ReadXML

// End 17/11/2004
///////////////////////////////////////////////////////////////////////////////
end.
