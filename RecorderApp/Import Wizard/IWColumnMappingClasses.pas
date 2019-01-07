{===============================================================================
  Unit:        IWColumnMappingClasses

  Defines:     TColumnMapping
               TColumnType
                 TLocationInfoColumnType
                 TTaxonOccurrenceDataColumnType
               TImportFile
               TMatchRule
               TRelatedType

  Description:

  Model:

  Last revision information:                               
    $Revision: 90 $
    $Date: 21/03/13 15:17 $
    $Author: Michaelcaptain $

===============================================================================}

unit IWColumnMappingClasses;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ExceptionForm, IWParsers, Contnrs, DB, ComboListID, IWUserSuppliedData,
  ADODB, IWConstants, StrUtils, VagueDate;

resourcestring
  ResStr_DateNotValid = 'Date supplied is not recognised as a valid date or vague date.';
  ResStr_DeterminationDateNotValid = 'Determination date supplied is not recognised as a valid date or vague date.';
  ResStr_ReviewDateNotValid = 'Review date supplied is not recognised as a valid date or vague date.';
  ResStr_FieldRequired =
      'A value is specified in the %s column, so an associated value is required in the %s column.';
  ResStr_OneFieldRequired =
      'A value is required in the %s column or the %s column.';
  ResStr_ValueRequired =
      'A value is required in the %s column';
  ResStr_DeterminationDateBeforeSampleDate =
      'The determination date cannot be before the sample date.';
  ResStr_ReviewDateBeforeSampleDate =
      'The review date cannot be before the sample date.';
  ResStr_ClassNotParser = 'Class %s is does not implement IImportWizardParser.';
  ResStr_ColumnConflict =
      'Column type %s cannot be selected at the same time as column type %s.';
  ResStr_ColumnDependency = 'Column type %s requires that column type %s is also selected.';
  ResStr_ColumnTypeClassLoadFailure =
      'A column type could not be loaded from the '
      + 'database for key value %s.  The error is described as:'#13#10'%s';
  ResStr_ColumnTypeMissing = 'A column type was not found for key %s.';
  ResStr_ColumnTypeClassUnknown = 'The column type class %s is not recognised.';
  ResStr_ColumnTypeRequired = 'Column type %s must be selected.';
  ResStr_InvalidFieldName = 'Field name not recognised: %s';
  ResStr_ImportFileGenerationFailed =
      'Failed to generate import table ''%s''.'
      + ' The import job was aborted.'#10#10
      + 'The error that occurred was:'#10#10'%s'#10#10
      + 'This is probably due to an error in the definition of the import'
      + ' process.';
  ResStr_ParsingColumn         = 'Parsing column %s';
  ResStr_ParsingData           = 'Parsing data...';
  ResStr_ProcessingTable       = 'Processing data - %s';
  ResStr_CreatingDatabase      = 'Creating temporary database';
  ResStr_CreatingDatabaseTable = 'Creating temporary database - %s';
  ResStr_InitMatching          = 'Initialising matching process...';
  ResStr_PreparingData         = 'Preparing data tables...';
  ResStr_AllPreferredLists = 'All preferred lists';
  ResStr_AllPreferredTaxa = 'All taxa';
  ResStr_Virtual_Organism  = 'All organisms';
  ResStr_AllRecommendedTaxa = 'All recommended taxa';
  ResStr_RestoreUnconfirmed =
      'There are some uncommitted matches remaining '
      + 'from a previous session. Do you wish to restore them?';
  ResStr_SaveUnconfirmed =
      'Some or all of the matches have not yet been '
      + 'saved to the database. Do you want to retain these for the next session?';
  ResStr_MeasurementColumnCaption = '%s of %s (%s)';

type
  TRelationshipType = (rtRequires, rtConflictsWith, rtFieldDependencyColumnRequired,
      rtFieldDependency, rtRequiredBy, rtOneRequired);

  TMatchControlType = (ctLinkedEdit, ctIDComboBox);

  EImportFile = class(TExceptionPath);

  TMatchRule = class;
  EMatchRule = class(TExceptionPath);
  EColumnMappingClass = class(TExceptionPath);

  TParseErrorEvent = procedure (Sender: TObject; const FieldName: String; Error: Boolean) of object;
  TProgressEvent   = procedure (const Progress: Integer; ProcessMessages: Boolean) of object;
  TStatusEvent     = procedure (const Status: String; ProcessMessages: Boolean) of object;
  TPredicate       = function: Boolean of object;

  {-----------------------------------------------------------------------------
    Simple class to store the relationship between 2 column types.
  }
  TRelatedType = class(TObject)
  private
    FColumnTypeKey: String;
    FRelationshipType: TRelationshipType;
  public
    constructor Create(AColumnTypeKey: string; ARelationshipType: TRelationshipType); reintroduce;
    property ColumnTypeKey: String read FColumnTypeKey;
    property RelationshipType: TRelationshipType read FRelationshipType;
  end;

  {-----------------------------------------------------------------------------
    A column type.
  }
  TColumnType = class(TPersistent)
  private
    FCommonlyUsed: Boolean;
    FFieldMatchRuleKeys: TStringList;
    FFieldType: String;
    FKey: String;
    FMaximumLength: Integer;
    FName: String;
    FParsedFieldLists: TStringList;
    FParsedValueLists: TStringList;
    FParser: TImportWizardParser;
    FParserClassName: String;
    FRelatedTypes: TObjectList;
    FRequired: Boolean;
    FTermListTable: String;
    FLastPosition: Integer;
    FUseOldImportWizard: Boolean;
    procedure ParserCallback(Sender: TObject; Position: Integer; const Field: string;
        const Value: Variant);
  protected
    constructor Create; virtual;
    function GetFieldMatchRuleKeys(Index: Integer): String; virtual;
    function GetParser: TImportWizardParser; virtual;
    function GetQualification: String; virtual;
    function GetRelatedTypeCount: Integer; virtual;
    function GetRelatedTypes(Index: Integer): TRelatedType; virtual;
    procedure InternalRefreshTermLists; virtual;
    procedure LoadFieldMatchRuleKeys; virtual;
    procedure LoadRelatedTypes;
  public
    destructor Destroy; override;
    procedure AddParsedRecords(AParser: TImportWizardParser; const ARecordNo: Integer;
        const AValue: String); virtual;
    class procedure LoadTypes(const TypeID: String; TypeList: TObjectList;
        UseOldImportWizard: Boolean); virtual;
    procedure ParseField(const Value: String; CallBack: TFieldParserEvent);
    procedure RefreshTermLists;
    property CommonlyUsed: Boolean read FCommonlyUsed;
    property FieldMatchRuleKeys[Index: Integer]: String read GetFieldMatchRuleKeys;
    property FieldType: String read FFieldType;
    property Key: String read FKey;
    property Name: String read FName;
    property Parser: TImportWizardParser read GetParser;
    property Qualification: String read GetQualification;
    property RelatedTypeCount: Integer read GetRelatedTypeCount;
    property RelatedTypes[Index: Integer]: TRelatedType read GetRelatedTypes;
    property Required: Boolean read FRequired;
    property TermListTable: String read FTermListTable;
  end;

  TTaxonOccurrenceDataColumnType = class(TColumnType)
  private
    FDataQualifier: String;
    FDataType: String;
    FDataUnit: String;
  protected
    function GetParser: TImportWizardParser; override;
    function GetQualification: String; override;
    procedure InternalRefreshTermLists; override;
  public
    class procedure LoadTypes(const TypeID: string; TypeList: TObjectList;
        UseOldImportWizard: Boolean); override;
    property DataQualifier: String read FDataQualifier;
    property DataType: String read FDataType;
    property DataUnit: String read FDataUnit;
  end;

  TLocationInfoColumnType = class(TColumnType)
  private
    FGridRefMapped: Boolean;
    FLocationMapped: Boolean;
    FLocationNameMapped: Boolean;
  protected
    function GetParser: TImportWizardParser; override;
  public
    class procedure LoadTypes(const TypeID: string; TypeList: TObjectList;
        UseOldImportWizard: Boolean); override;
    procedure UpdateFlags(gridRef, location, locationName: Boolean);
    property GridRefMapped: Boolean read FGridRefMapped write FGridRefMapped;
    property LocationMapped: Boolean read FLocationMapped write FLocationMapped;
    property LocationNameMapped: Boolean read FLocationNameMapped write FLocationNameMapped;
  end;

  TColumnMapping = class(TObject)
  private
    FErrors: TStringList;
    FIsDirty: Boolean;
    FTermFieldsRequired: TObjectList;
    FIsTempSurvey: Boolean;
    function GetTermFieldsRequired(Index: Integer): TColumnType;
    function GetTermFieldsRequiredCount: Integer;
    function GetColumnCount: integer;
    function GetColumnTitleByName(AFieldName: string): string;
    procedure SetColumnTitleByName(AFieldName: string; const Value: string);
    function GetColumnTitle(AIndex: integer): string;
    procedure SetColumnTitle(AIndex: integer; const Value: string);
    procedure UpdateLocationInfoColumns;
  protected
    FColumns: TStringList;
    FColumnTitles: TStringList;
    FTypes: TObjectList;
    function GetTypeByKey(const AKey: string): TColumnType; virtual;
    function GetTypeCount: Integer; virtual;
    function GetTypes(Index: Integer): TColumnType; virtual;
    procedure LoadTypes(UseOldImportWizard: Boolean); virtual;
    procedure SetIsDirty(Value: Boolean); virtual;
    procedure UnmapType(TypeIndex: Integer); overload; virtual;
    procedure UnmapType(AType: TColumnType); overload; virtual;
    procedure Validate; virtual;
    property IsDirty: Boolean read FIsDirty write SetIsDirty;
  public
    constructor Create(DataSource: TDataSet; UseOldImportWizard: Boolean);
    destructor Destroy; override;
    function ColumnTypeByKey(AKey: string): TColumnType; overload; virtual;
    function ColumnTypeByKey(AKey, AQualification: string): TColumnType; overload; virtual;
    function IsMapped(FieldName: string): Boolean; overload; virtual;
    function IsMapped(ColumnType: TColumnType): Boolean; overload; virtual;
    function KeyIsMapped(const ColumnTypeKey: String): Boolean; virtual;
    procedure LoadColumns(DataSource: TDataSet); virtual;
    procedure MapAutomatically; virtual;
    procedure MapColumn(const AFieldName: string; AType: TColumnType); virtual;
    function MappedColumn(AType: TColumnType): String;
    function MappedType(const FieldName: string): TColumnType; virtual;
    procedure RefreshErrors; virtual;
    procedure RefreshRequiredTerms; virtual;
    procedure RefreshTermLists; virtual;
    procedure ResetColumnTitle(AIndex: integer);
    procedure UnmapColumn(const FieldName: string); virtual;
    function ValidateFieldDependency(AType: TColumnType; ADataset: TDataset): String; virtual;
    property Errors: TStringList read FErrors;
    property TermFieldsRequired[Index: Integer]: TColumnType read GetTermFieldsRequired;
    property TermFieldsRequiredCount: Integer read GetTermFieldsRequiredCount;
    property TypeCount: Integer read GetTypeCount;
    property Types[Index: Integer]: TColumnType read GetTypes;
    property ColumnCount: integer read GetColumnCount;
    property ColumnTitleByName[AFieldName: string]: string
             read GetColumnTitleByName write SetColumnTitleByName;
    property ColumnTitle[AIndex: integer]: string
             read GetColumnTitle write SetColumnTitle;
  end;

  TImportFile = class(TObject)
  private
    FColumnMapping: TColumnMapping;
    FImportDatabase: String;
    FLargeFields: TStringList;
    FLastParsedValues: TStringList;
    FMatchRules: TStringList;
    FOnParseErrorChanged: TParseErrorEvent;
    FOnStatusChanged: TStatusEvent;
    FOnProgress: TProgressEvent;
    FAbortPredicate: TPredicate;
    FOutputTables: TObjectList;
    FTables: TStringList;
    FUserSuppliedData: TUserSuppliedData;
    function GroupChecksum(ColumnType: TColumnType; RecordNo: Integer): String;
    procedure RecordParsedValues(Sender: TObject; Position: Integer; const Field: string;
        const Value: Variant);
    function RequiresQuotes(AType: string): Boolean;
  protected
    FDataSource: TDataSet;
    FTableRuleIndex: Integer;
    FTableRuleCount: Integer;
    FMultiRecordTableTypes: TObjectList;
    function Aborted: Boolean; virtual;
    function AddMatchRule(const AKey: string): TMatchRule; virtual;
    procedure CreateImportTable(columnType: TColumnType); virtual;
    procedure CreateImportTables; virtual;
    function CreateTableRule(const Key: String): TObject; virtual;
    procedure ApplyTableRulePostProcessing; virtual;
    procedure DoParseErrorChanged(Sender: TObject; const FieldName: String; Error: Boolean);
    procedure DropTemporaryTable(const Name: String); virtual;
    procedure InternalDropWorkingTables; virtual;
    procedure DropOutputTables; virtual;
    procedure GenerateImportTable(Connection: TADOConnection; OutputTableIndex: Integer); virtual;
    function GetValueToParse(const AFieldName: string;
        AColumnType: TColumnType): string; virtual;
    function GetMasterColumn(AColumnType: TColumnType; AWithFieldType: Boolean): String; virtual;
    function GetMasterColumns(AWithFieldType: Boolean = False): String; virtual;
    function GetMasterValue(const AFieldName: String; AColumnType: TColumnType): String; virtual;
    function GetMasterValues: String; virtual;
    function GetMatchRuleCount: Integer; virtual;
    function GetMatchRules(Index: Integer): TMatchRule; virtual;
    procedure LoadData; virtual;
    procedure PopulateChecksumTables; virtual;
    procedure PopulateMasterTable; virtual;
    procedure PopulateMultiRecordTables; virtual;
    procedure TableRuleProgress(const Progress: Integer; ProcessMessages: Boolean); virtual;
    procedure UpdateStatus(const Status: String); virtual;
    procedure UpdateProgress(Progress: Integer); virtual;
    property LargeFields: TStringList read FLargeFields;
    property LastParsedValues: TStringList read FLastParsedValues;
  public
    constructor Create(DataSource: TDataSet; LargeFields: TStringList;
        UserSuppliedData: TUserSuppliedData; UseOldImportWizard: Boolean);
    destructor Destroy; override;
    procedure ClearMatchRules;
    procedure DropWorkingTables; virtual;
    procedure ApplyTableRules; virtual;
    procedure GenerateImportDatabase; virtual;
    function GetParseError(const FieldName: string): String; virtual;
    procedure InitialiseMatching; virtual;
    function MatchRuleByKey(const AKey: String): TMatchRule; virtual;
    procedure ParseColumn(const FieldName: string); virtual;
    procedure ParseRelatedColumns(AColumn: TColumnType;
        IncludeSelf: Boolean=False); virtual;
    procedure ParseColumnList(AList: TStringList); virtual;
    function ParseField(const FieldName: String): Boolean; virtual;
    procedure PrepareData; virtual;
    property ColumnMapping: TColumnMapping read FColumnMapping;
    property ImportDatabase: String read FImportDatabase;
    property MatchRuleCount: Integer read GetMatchRuleCount;
    property MatchRules[Index: Integer]: TMatchRule read GetMatchRules;
    property UserSuppliedData: TUserSuppliedData read FUserSuppliedData;
  published
    property OnParseErrorChanged: TParseErrorEvent read FOnParseErrorChanged
        write FOnParseErrorChanged;
    property OnStatusChanged: TStatusEvent read FOnStatusChanged write FOnStatusChanged;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property AbortPredicate: TPredicate read FAbortPredicate write FAbortPredicate;
  end;

  TMatchRule = class(TObject)
  private
    FChecklistsSelectProcedure: String;
    FControlType: TMatchControlType;
    FImportedDataInsertSql: String;
    FKey: String;
    FKeyToCaptionProcedure: String;
    FMatchProcedure: String;
    FName: String;
    FNewEntryProcedure: String;
    FUpdateNotesProcedure: String;
    FDisplayNotesProcedure: String;
    FDetailedNotesProcedure: String;
    FRecordMatchesProcedure: String;
    FRememberedMatchesProcedure: String;
    FRequiresChecklist: Boolean;
    FSearchType: Integer;
    FSequence: Integer;
    FSetMatchProcedure: String;
    FTableCreateSql: String;
    FTermlistSelectProcedure: String;
    FFields: TStringList;
    FExcludeUnmatchedProcedure: String;
    function GetIsSpeciesRule: Boolean;
  protected
    procedure ApplyRememberedMatches(ChecklistKey: string = ''); virtual;
    procedure CreateTable; virtual;
    procedure PopulateCombo(ACombo: TIDComboBox; const AProc: String);
    property ImportedDataInsertSql: String read FImportedDataInsertSql;
    property MatchProcedure: String read FMatchProcedure;
    property RecordMatchesProcedure: String read FRecordMatchesProcedure;
    property RememberedMatchesProcedure: String read FRememberedMatchesProcedure;
    property SetMatchProcedure: String read FSetMatchProcedure;
    property TableCreateSql: String read FTableCreateSql;
   public
    constructor Create(const AKey: string); reintroduce;
    destructor Destroy; override;
    function CheckDuplicates: TStringList;
    function ConvertKeyToCaption(const AKey: String): String;
    procedure LoadImportedData(ColumnType: TColumnType; const AParseFieldName: string); virtual;
    procedure MakeNewEntry(const ImportValue: String); virtual;
    procedure MatchRecords(ChecklistKey: string = ''); virtual;
    procedure PopulateChecklistCombo(ACombo: TIDComboBox);
    procedure PopulateTermListCombo(ACombo: TIDComboBox);
    procedure RecordMatches(ChecklistKey: string = ''); virtual;
    procedure SetMatch(const ImportValue: String; const MatchKey: string;
        const ChecklistKey: string = ''); virtual;
    procedure Cancel;
    property ControlType: TMatchControlType read FControlType;
    property Key: String read FKey;
    property Name: String read FName;
    property NewEntryProcedure: String read FNewEntryProcedure;
    property UpdateNotesProcedure: String Read FUpdateNotesProcedure;
    property DisplayNotesProcedure: String Read FDisplayNotesProcedure;
    property DetailedNotesProcedure: String Read FDetailedNotesProcedure;
    property RequiresChecklist: Boolean read FRequiresChecklist;
    property SearchType: Integer read FSearchType;
    property Sequence: Integer read FSequence;
    property IsSpeciesRule: Boolean read GetIsSpeciesRule;
    function RemoveUnmatched : Integer;
    property ExcludeUnmatchedProcedure: String read FExcludeUnmatchedProcedure;
  end;


//==============================================================================
implementation

uses
  ApplicationSettings, GeneralFunctions, DatabaseAccessADO, Variants,
  IWSettings, IWTableRule, Sql, Maintbar, ComObj;

type
  TColumnTypeClass = class of TColumnType;

const
  SQL_MATCHTABLE_INSERT =
      'INSERT INTO #%s (Import_Value, Match_Count) '+
      'SELECT DISTINCT SRC.[%s], 0 '+
      'FROM [%s] AS SRC '+
      'LEFT JOIN #%s AS DEST ON DEST.Import_Value=SRC.[%s] '+
      'WHERE DEST.Import_Value IS NULL '+
      'AND SRC.[%s] IS NOT NULL '+
      'AND RTRIM(LTRIM(SRC.[%s])) <> ''''';

  SQL_DUPLICATES =
      'SELECT SRC.Record_No, DEST.Import_Value, DEST.Match_Key FROM ' +
      '[%s] AS SRC LEFT JOIN #%s AS DEST ON SRC.[%s]' +
      ' = DEST.Import_Value ORDER BY SRC.Record_No';
  USP_RECOVERABLE_SPECIES      = 'usp_IWCheckRecoverableMatches_Species';
  USP_MATCHRECOVERED_SPECIES   = 'usp_IWMatchRecovered_Species';
  USP_MATCHCLEAR_SPECIES       = 'usp_IWMatchClear_Species';
  USP_CHECKUNCONFIRMED_SPECIES = 'usp_IWCheckUnconfirmedMatches_Species';

  SQL_MASTER_HAS_GRID_REF =
      'SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS ' +
      'WHERE OBJECT_ID(''tempdb..'' + table_name)=OBJECT_ID(''tempdb..#Master'')' +
      'and COLUMN_NAME like ''System0100000001_Spatial_ref%''';
  SQL_VIRTUAL_ORGANISM_EXISTS = 'Select * FROM Taxon_List WHERE Taxon_List_Key' +
      ' = ''VIRTUAL_ORGANISM''';
{-==============================================================================
    TRelatedType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TRelatedType.Create(AColumnTypeKey: string; ARelationshipType: TRelationshipType);
begin
  inherited Create;
  FColumnTypeKey    := AColumnTypeKey;
  FRelationshipType := ARelationshipType;
end;  // TRelatedType.Create

{-==============================================================================
    TMatchRule
===============================================================================}
{-------------------------------------------------------------------------------
  Create object and initialise field data by loading from the database.
}
constructor TMatchRule.Create(const AKey: string);
begin
  inherited Create;
  FKey := AKey;
  with dmDatabase.GetRecordset('usp_IWMatchRule_Select', ['@Key', AKey]) do begin
    FSequence                   := Fields['Sequence'].Value;
    FName                       := Fields['Item_Name'].Value;
    FControlType                := TMatchControlType(Fields['Control_Type'].Value);
    FImportedDataInsertSQL      := VarToStr(Fields['Imported_Data_Insert_Sql'].Value);
    FRememberedMatchesProcedure := VarToStr(Fields['Remembered_Matches_Procedure'].Value);
    FMatchProcedure             := VarToStr(Fields['Match_Procedure'].Value);
    FRecordMatchesProcedure     := VarToStr(Fields['Record_Matches_Procedure'].Value);
    FNewEntryProcedure          := VarToStr(Fields['New_Entry_Procedure'].Value);
    FRequiresChecklist          := Fields['Requires_Checklist'].Value;
    FSetMatchProcedure          := VarToStr(Fields['Set_Match_Procedure'].Value);
    FTableCreateSQL             := VarToStr(Fields['Table_Create_Sql'].Value);
    FUpdateNotesProcedure       := VarToStr(Fields['Update_Notes_Procedure'].Value);
    FDisplayNotesProcedure      := VarToStr(Fields['Display_Notes_Procedure'].Value);
    FDetailedNotesProcedure     := VarToStr(Fields['Detailed_Notes_Procedure'].Value);
    FKeyToCaptionProcedure      := VarToStr(Fields['Key_To_Caption_Procedure'].Value);
    FSearchType                 := Fields['Search_Type'].Value;
    FChecklistsSelectProcedure  := VarToStr(Fields['Checklists_Select_Procedure'].Value);
    FTermListSelectProcedure    := VarToStr(Fields['Termlist_Select_Procedure'].Value);
    FExcludeUnmatchedProcedure  := VarToStr(Fields['Exclude_Unmatched_Procedure'].Value);
  end;
  FFields := TStringList.Create;
  CreateTable;
end;  // TMatchRule.Create
 
{-------------------------------------------------------------------------------
}
destructor TMatchRule.Destroy;
begin
  dmDatabase.ExecuteSQL(
      'IF Object_Id(''tempdb..#' + Name + ''') IS NOT NULL'
      + #10'DROP TABLE #' + Name);

  FFields.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
}
procedure TMatchRule.ApplyRememberedMatches(ChecklistKey: string = '');
var
  unconfirmedMatches : Boolean;
begin
  if IsSpeciesRule then
  begin
    // Checks the DB for temporary matches for this checklist which are not
    // currently shown in the table. If they are found, gives the user the
    // option of either loading them or removing them.
    unconfirmedMatches := Boolean(
        dmDatabase.GetStoredProcOutputParam(USP_RECOVERABLE_SPECIES      ,
                                            ['@ChecklistKey', ChecklistKey,
                                             '@UserID',       AppSettings.UserID,
                                             '@FoundMatches', 0],
                                            '@FoundMatches'));

    if unconfirmedMatches then
      if MessageDlg(ResStr_RestoreUnconfirmed, mtWarning, [mbYes, mbNo], 0) = mrYes then
        dmDatabase.RunStoredProc(USP_MATCHRECOVERED_SPECIES,
                                 ['@ChecklistKey', ChecklistKey,
                                  '@UserID',       AppSettings.UserID])
      else
        dmDatabase.RunStoredProc(USP_MATCHCLEAR_SPECIES,
                                 ['@UserID',       AppSettings.UserID,
                                  '@ChecklistKey', ChecklistKey]);
  end;
  dmDatabase.RunStoredProc(RememberedMatchesProcedure, ['@ChecklistKey', ChecklistKey]);
end;  // TMatchRule.ApplyRememberedMatches

{-------------------------------------------------------------------------------
  Look for cases where 2 items have been matched to the same thing, in the
      same record. These are probably mistakes. E.g. if J Smith and john smith
      are observers for one record, they are probably different people so
      shouldn't be linked to the same one.
}
function TMatchRule.CheckDuplicates: TStringList;
var
  lRS: _Recordset;
  lValue, lKey, lookup: String;
  lRecno: integer;
  lRecords, lDuplicates: TStringList;
  j, lIdx: Integer;
begin
  lRecNo := -1;
  lRecords := TStringList.Create;
  lDuplicates := TStringList.Create;
  for j := 0 to FFields.Count - 1 do begin
    lRS := dmDatabase.ExecuteSQL(
        Format(SQL_DUPLICATES, [FFields.Names[j], Name, FFields.ValueFromIndex[j]]), True);
    while not lRS.EOF do begin
      // lRecords is the list of things associated with this record. Clear it when we
      // move to another record.
      if lRS.Fields['Record_No'].Value <> lRecNo then
        lRecords.Clear;

      lValue := VarToStr(lRS.Fields['Import_Value'].Value);
      lKey := VarToStr(lRS.Fields['Match_Key'].Value);
      lRecNo := lRS.Fields['Record_No'].Value;

      // check if we've found this key before by using a string list with format
      // key.ct_table.recno=value (e.g. TESTDATA00000005.#CT_SYSTEM0100000004.3=J Brown). We can check to see
      // if there are duplicate keys matched to this record number simply by
      // using IndexOfName on key.recno
      lookup := lKey + '.' + FFields.Names[j] + '.' + IntToStr(lRecNo);
      if lRecords.IndexOfName(lookup)=-1 then
        lRecords.Add(lookup +  '=' + lValue)
      else begin
        // got a duplicate
        // lDuplicates is a list of name-value pairs, key-record no.
        // Each pair is associated with a string list containing the duplicate values
        // assigned to that key and record no.
        if lDuplicates.IndexOf(lKey + '=' + IntToStr(lRecNo)) < 0 then
          lIdx := lDuplicates.AddObject(lKey + '=' + IntToStr(lRecNo), TStringList.Create)
        else
          lIdx := lDuplicates.IndexOf(lKey + '=' + IntToStr(lRecNo));
        with TStringList(lDuplicates.Objects[lIdx]) do begin
          // If this is the first duplicate we find, we need to add both duplicates.
          // If not, previous duplicates will already be in the list.
          if IndexOf(lRecords.Values[lookup]) < 0 then
            Add(lRecords.Values[lookup]);
          Add(lValue);
        end;
      end;
      lRS.MoveNext;
    end;
  end;
  lRecords.Free;
  Result := lDuplicates;
end;

{-------------------------------------------------------------------------------
}
function TMatchRule.ConvertKeyToCaption(const AKey: String): String;
begin
  Result := VarToStr(dmDatabase.GetStoredProcOutputParam(FKeyToCaptionProcedure,
                                                         ['@Key', AKey], '@Caption'));
end;  // TMatchRule.ConvertKeyToCaption

{-------------------------------------------------------------------------------
}
procedure TMatchRule.CreateTable;
begin
  dmDatabase.ExecuteSQL(FTableCreateSQL);
end;  // TMatchRule.CreateTable

{-------------------------------------------------------------------------------
}
procedure TMatchRule.LoadImportedData(ColumnType: TColumnType; const AParseFieldName: string);
var
  lTableName: String;
  lFieldName: String;
begin
  lTableName := '';
  if Assigned(ColumnType.Parser) then
    if not ColumnType.Parser.SingleRecord then begin
      lTableName := '#CT_' + ColumnType.Key;
      lFieldName := AParseFieldName;
    end;
  if lTableName = '' then begin
    lTableName := '#Master';
    lFieldName := ColumnType.Key + '_' + AParseFieldName;
  end;

  dmDatabase.ExecuteSQL(Format(SQL_MATCHTABLE_INSERT,
                               [Name, lFieldName, lTableName, Name,
                               lFieldName, lFieldName, lFieldName]));
  if FImportedDataInsertSQL <> '' then
    dmDatabase.ExecuteSQL(FImportedDataInsertSQL);

  FFields.Add(lTableName + '=' + lFieldName);
end;  // TMatchRule.LoadImportedData

{-------------------------------------------------------------------------------
}
function TMatchRule.RemoveUnmatched : Integer;
begin
  Result := dmDatabase.GetStoredProcOutputParam(ExcludeUnmatchedProcedure,
                                            [],
                                            '@RowsDeleted');
end;

{-------------------------------------------------------------------------------
}
procedure TMatchRule.MakeNewEntry(const ImportValue: String);
begin
  dmDatabase.RunStoredProc(NewEntryProcedure, ['@ImportValue', ImportValue,
                                                 '@EnteredBy', AppSettings.UserID]);
end;  // TMatchRule.MakeNewEntry

{-------------------------------------------------------------------------------
}
procedure TMatchRule.MatchRecords(ChecklistKey: string = '');
begin
  If not AppSettings.IgnoreRememberedMatches then
     ApplyRememberedMatches(ChecklistKey);

  dmDatabase.RunStoredProc(FMatchProcedure, ['@ChecklistKey', ChecklistKey]);
end;  // TMatchRule.MatchRecords

{-------------------------------------------------------------------------------
}
procedure TMatchRule.PopulateChecklistCombo(ACombo: TIDComboBox);
begin
  ACombo.Add(ResStr_AllPreferredTaxa, '');
  if not AppSettings.UsePreferredTaxa then
    PopulateCombo(ACombo, FChecklistsSelectProcedure)
end;  // TMatchRule.PopulateChecklistCombo 

{-------------------------------------------------------------------------------
}
procedure TMatchRule.PopulateCombo(ACombo: TIDComboBox; const AProc: String);
begin
  if AProc = '' then Exit; // or raise error?

  with dmDatabase.GetRecordset(AProc, []) do
    try
      while not Eof do begin
        ACombo.Add(Fields['Item_Name'].Value, VarToStr(Fields['Item_Key'].Value));
        MoveNext;
      end;
    finally
      Close;
    end;
end;  // TMatchRule.PopulateCombo

{-------------------------------------------------------------------------------
  Cancels the match.
}
procedure TMatchRule.Cancel;
var
  unconfirmedMatches: Boolean;
  dialogResult: Integer;
begin
  if IsSpeciesRule then
  begin
    // When the user cancels the wizard on the species page, gives them the
    // option of whether to delete or keep the temporary matches.
    unconfirmedMatches := Boolean(dmDatabase.GetStoredProcOutputParam(
        USP_CHECKUNCONFIRMED_SPECIES,
        ['@UserID',       AppSettings.UserID,
         '@FoundMatches', 0],
        '@FoundMatches'));
    if unconfirmedMatches then
    begin
      dialogResult := MessageDlg(ResStr_SaveUnconfirmed, mtCustom, [mbYes, mbNo, mbCancel], 0);
      if dialogResult = mrNo then
        dmDatabase.RunStoredProc(USP_MATCHCLEAR_SPECIES, ['@UserID', AppSettings.UserID])
      else if dialogResult <> mrYes then
        Abort;
    end;
  end
end;

{-------------------------------------------------------------------------------
}
procedure TMatchRule.PopulateTermListCombo(ACombo: TIDComboBox);
begin
  PopulateCombo(ACombo, FTermlistSelectProcedure);
end;  // TMatchRule.PopulateTermListCombo 

{-------------------------------------------------------------------------------
}
procedure TMatchRule.RecordMatches(ChecklistKey: string = '');
begin
  if IsSpeciesRule then
    dmDatabase.RunStoredProc(FRecordMatchesProcedure, ['@UserID', AppSettings.UserID])
  else
    dmDatabase.RunStoredProc(FRecordMatchesProcedure, []);
end;  // TMatchRule.RecordMatches

{-------------------------------------------------------------------------------
}
procedure TMatchRule.SetMatch(const ImportValue: String; const MatchKey: string;
    const ChecklistKey: string = '');
begin
  If IsSpeciesRule then
    dmDatabase.RunStoredProc(SetMatchProcedure, ['@ImportValue',  ImportValue,
                                                 '@MatchKey',     MatchKey,
                                                 '@UserID',       AppSettings.UserID,
                                                 '@ChecklistKey', ChecklistKey])
  else
    dmDatabase.RunStoredProc(SetMatchProcedure, ['@ImportValue', ImportValue,
                                                 '@MatchKey',    MatchKey]);
end;  // TMatchRule.SetMatch

{-------------------------------------------------------------------------------
  Returns true if this is a species match rule
}
function TMatchRule.GetIsSpeciesRule: Boolean;
begin
  Result := CompareText(FChecklistsSelectProcedure, 'usp_TaxonDataEntryLists_Select') = 0;
end;

{-==============================================================================
    TColumnType
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TColumnType.Create;
begin
  inherited;
  FRelatedTypes     := TObjectList.Create(True);
  FParsedValueLists := TStringList.Create;
  FParsedFieldLists := TStringList.Create;
  FLastPosition     := -1;
end;  // TColumnType.Create

{-------------------------------------------------------------------------------
}
destructor TColumnType.Destroy;
begin
  FFieldMatchRuleKeys.Free;
  FRelatedTypes.Free;
  FParsedFieldLists.Free;
  FParsedValueLists.Free;
  FParser.Free;
  inherited;
end;  // TColumnType.Destroy

{-------------------------------------------------------------------------------
  Using the parser, breaks down the value into its sub-records and inserts them
  into the appropriate table.
}
procedure TColumnType.AddParsedRecords(AParser: TImportWizardParser; const ARecordNo: Integer;
    const AValue: String);
var
  i: Integer;
  sql: String;
begin
  FParsedFieldLists.Clear;
  FParsedValueLists.Clear;
  FLastPosition := -1;
  AParser.ParseField(AValue, ParserCallback);

  // Tweak to allow this method to be used to validate as well as inserting data.
  if not AParser.Failed then
  begin
    // Build up a single statement to run, instead of separate ones.
    sql := '';
    for i := 0 to FParsedFieldLists.Count - 1 do
    begin
      FParsedFieldLists[i] := 'Record_No, Position, IW_Qualification, '
          + FParsedFieldLists[i];
      FParsedValueLists[i] := IntToStr(ARecordNo) + ', '
          + IntToStr(i) + ', '
          + '''' + Qualification + ''', '
          + FParsedValueLists[i];
      sql := sql
          + 'INSERT INTO #CT_' + FKey + '(' + FParsedFieldLists[i] + ')'
          + ' VALUES (' + FParsedValueLists[i] + ')'#10;
    end;
    if sql <> '' then
      dmDatabase.ExecuteSQL(sql);
  end;
end;  // TColumnType.AddParsedRecords

{-------------------------------------------------------------------------------
  Accessor for the match rule keys for a field index.  Returns '' if none available for the
      field index.
}
function TColumnType.GetFieldMatchRuleKeys(Index: Integer): String;
begin
  if not Assigned(FFieldMatchRuleKeys) then LoadFieldMatchRuleKeys;
  Result := FFieldMatchRuleKeys[Index];
end;  // TColumnType.GetFieldMatchRuleKeys 

{-------------------------------------------------------------------------------
  Retrieves a parser, instantiating it on demand. 
}
function TColumnType.GetParser: TImportWizardParser;
begin
  if not Assigned(FParser) and (FParserClassname <> '') then
  begin
    FParser := TImportWizardParserClass(GetClass(FParserClassname)).Create;
    FParser.UseOldImportWizard := FUseOldImportWizard;
    if FParser is TTextParser then
      TTextParser(FParser).MaximumLength := FMaximumLength;
  end;
  // Note that return result could still be nil
  Result := FParser;
end;  // TColumnType.GetParser

{-------------------------------------------------------------------------------
}
function TColumnType.GetQualification: String;
begin
  Result := '';
end;  // TColumnType.GetQualification 

{-------------------------------------------------------------------------------
}
function TColumnType.GetRelatedTypeCount: Integer;
begin
  Result := FRelatedTypes.Count;
end;  // TColumnType.GetRelatedTypeCount 

{-------------------------------------------------------------------------------
}
function TColumnType.GetRelatedTypes(Index: Integer): TRelatedType;
begin
  Result := TRelatedType(FRelatedTypes[Index]);
end;  // TColumnType.GetRelatedTypes

{-------------------------------------------------------------------------------
}
procedure TColumnType.InternalRefreshTermLists;
begin
  // Override when needed.
end;  // TColumnType.InternalRefreshTermLists

{-------------------------------------------------------------------------------
}
procedure TColumnType.LoadFieldMatchRuleKeys;
var
  I: Integer;
begin
  if Assigned(FFieldMatchRuleKeys) then
    FFieldMatchRuleKeys.Clear
  else
    FFieldMatchRuleKeys := TStringList.Create;

  if not Assigned(Parser) then
    FFieldMatchRuleKeys.Add('')
  else for I := 0 to Parser.FieldCount - 1 do
    FFieldMatchRuleKeys.Add('');

  with dmDatabase.GetRecordset(
      'usp_IWColumnTypeMatchRules_Select',
      ['@column_type_key', Key]) do
    while not Eof do
    begin
      FFieldMatchRuleKeys[Fields['Field_Index'].Value] :=
          Fields['IW_Match_Rule_Key'].Value;
      MoveNext;
    end;
end;  // TColumnType.LoadFieldMatchRuleKeys

{-------------------------------------------------------------------------------
}
procedure TColumnType.LoadRelatedTypes;
begin
  with dmDatabase.GetRecordset('usp_IWColumnTypeRelationships_Select',
      ['@Key', Key]) do begin
    if not EOF then begin
      MoveFirst;
      while not EOF do begin
        FRelatedTypes.Add(TRelatedType.Create(
            Fields['Related_IW_Column_Type_Key'].Value, Fields['Relationship_Type'].Value));
        MoveNext;
      end; // while
    end;
  end; // with
end;  // TColumnType.LoadRelatedTypes

{-------------------------------------------------------------------------------
  Add TColumnType objects to the given list for the specified
  type ID.
}
class procedure TColumnType.LoadTypes(const TypeID: String; TypeList: TObjectList;
    UseOldImportWizard: Boolean);
var
  lInstance: TColumnType;
begin
  with dmDatabase.GetRecordset('usp_IWColumnType_Select', ['@Key', TypeID]) do begin
    if RecordCount = 0 then
      raise EColumnMappingClass.Create(format(ResStr_ColumnTypeMissing, [TypeID]));
    lInstance := TColumnType.Create;
    lInstance.FKey                := TypeID;
    lInstance.FName               := Fields['Item_Name'].Value;
    lInstance.FRequired           := Fields['Required'].Value;
    lInstance.FCommonlyUsed       := Fields['Commonly_Used'].Value;
    lInstance.FParserClassName    := VarToStr(Fields['Parser_Class_Name'].Value);
    lInstance.FTermListTable      := VarToStr(Fields['Term_List_Table'].Value);
    lInstance.FFieldType          := VarToStr(Fields['Field_Type'].Value);
    lInstance.FUseOldImportWizard := UseOldImportWizard;
    if Fields['Maximum_Length'].Value <> Null then
      lInstance.FMaximumLength := Fields['Maximum_Length'].Value;
    lInstance.LoadRelatedTypes;
    TypeList.Add(lInstance);
  end;
end;  // TColumnType.LoadTypes

{-------------------------------------------------------------------------------
}
procedure TColumnType.ParseField(const Value: String; CallBack: TFieldParserEvent);
begin
  if Assigned(Parser) then
    Parser.ParseField(Value, nil);
end;  // TColumnType.ParseField

{-------------------------------------------------------------------------------
}
procedure TColumnType.RefreshTermLists;
begin
  InternalRefreshTermLists;
end;  // TColumnType.RefreshTermLists

{-------------------------------------------------------------------------------
  Callback method for the parser.  Builds lists of fields and values for each position.
}
procedure TColumnType.ParserCallback(Sender: TObject; Position: Integer; const Field: string;
    const Value: Variant);
var
  I: Integer;
begin
  if Position <> FLastPosition then
  begin
    FLastPosition := Position;
    FParsedValueLists.Add('');
    FParsedFieldLists.Add('');
  end;
  I := FParsedFieldLists.Count - 1;

  if FParsedValueLists[I] <> '' then
    FParsedValueLists[I] := FParsedValueLists[I] + ', ';
  FParsedValueLists[I] := FParsedValueLists[I] + TransactSqlLiteral(Value);
  if FParsedFieldLists[I] <> '' then
    FParsedFieldLists[I] := FParsedFieldLists[I] + ', ';
  FParsedFieldLists[I] := FParsedFieldLists[I] + Field;
end;  // TColumnType.ParserCallback

{-==============================================================================
    TTaxonOccurrenceDataColumnType
===============================================================================}
{-------------------------------------------------------------------------------
}
function TTaxonOccurrenceDataColumnType.GetParser: TImportWizardParser;
begin
  if not Assigned(FParser) and (FParserClassname <> '') then
  begin
    FParser := TImportWizardParserClass(GetClass(FParserClassname)).Create
        as TImportWizardParser;
    FParser.UseOldImportWizard := FUseOldImportWizard;
    if FParser is TTaxonDataParser then begin
      TTaxonDataParser(FParser).UnitKey      := FDataUnit;
      TTaxonDataParser(FParser).QualifierKey := FDataQualifier;
    end;
  end;
  // Note that return result could still be nil
  Result := FParser;
end;  // TTaxonOccurrenceDataColumnType.GetParser

{-------------------------------------------------------------------------------
  Qualification of column type to make it unique.
}
function TTaxonOccurrenceDataColumnType.GetQualification: String;
begin
  Result := FDataQualifier + FDataUnit;
end;  // TTaxonOccurrenceDataColumnType.GetQualification

{-------------------------------------------------------------------------------
  Ensure the restricted data values are up to date.
}
procedure TTaxonOccurrenceDataColumnType.InternalRefreshTermLists;
begin
  if Parser is TTaxonDataParser then
    TTaxonDataParser(Parser).RefreshRestrictedValues;
end;  // TTaxonOccurrenceDataColumnType.InternalRefreshTermLists

{-------------------------------------------------------------------------------
  Add one instance to the list for each valid combination
  of Qualifier, Type, and Unit.
}
class procedure TTaxonOccurrenceDataColumnType.LoadTypes(const TypeID: string; TypeList: TObjectList;
    UseOldImportWizard: Boolean);
var
  lInstance: TTaxonOccurrenceDataColumnType;
  lFieldType: String;
  lParserClassName: String;
begin
  // Find out information that applies to all data column types
  with dmDatabase.GetRecordset('usp_IWColumnType_Select', ['@Key', TypeID]) do begin
    if RecordCount = 0 then
      raise EColumnMappingClass.Create(format(ResStr_ColumnTypeMissing, [TypeID]));
    lParserClassName := VarToStr(Fields['Parser_Class_Name'].Value);
    lFieldType       := VarToStr(Fields['Field_Type'].Value);
  end;
  // loop to get all the combinations
  with dmDatabase.GetRecordset('usp_TaxonOccurrenceDataTypes_Select', []) do begin
    while not EOF do begin
      lInstance := TTaxonOccurrenceDataColumnType.Create;
      lInstance.FKey                := TypeID;
      lInstance.FName               := Format(ResStr_MeasurementColumnCaption, [
        Fields['MeasurementType'].Value,
        Fields['Qualifier'].Value,
        Fields['Unit'].Value
      ]);
      lInstance.FRequired           := False;
      lInstance.FCommonlyUsed       := False;
      lInstance.FParserClassName    := lParserClassName;
      lInstance.FTermListTable      := '';
      lInstance.FFieldType          := lFieldType;
      lInstance.FDataQualifier      := Fields['Measurement_Qualifier_Key'].Value;
      lInstance.FDataType           := Fields['Measurement_Type_Key'].Value;
      lInstance.FDataUnit           := Fields['Measurement_Unit_Key'].Value;
      lInstance.FUseOldImportWizard := UseOldImportWizard;
      TypeList.Add(lInstance);
      MoveNext;
    end;
  end;
end;  // TTaxonOccurrenceDataColumnType.LoadTypes

{-==============================================================================
    TLocationInfoColumnType
===============================================================================}
{-------------------------------------------------------------------------------
}
function TLocationInfoColumnType.GetParser: TImportWizardParser;
var
  lClassname: string;
begin
  lClassname := FParserClassname;
  // If GR column is not there or if it is there and both the Location and Location name
  // are not there then it must containa  value.
  // If the Location Name is there and the other wto columns are not then it
  // must contain a value.
  // If the Location is there, but the other two are not then it must containa value.
   if Key = CT_KEY_GRID_REFERENCE then
    if GridRefMapped and (LocationNameMapped or LocationMapped) then
      FParserClassname := 'TSpatialRefParser'
    else
      FParserClassname := 'TRequiredSpatialRefParser';

   if Key = CT_KEY_LOCATION_NAME then
    if LocationNameMapped and not LocationMapped and not GridRefMapped then
      FParserClassname := 'TRequiredTextParser'
   else
      FParserClassname := 'TTextParser';

   if Key = CT_KEY_LOCATION then
    if LocationMapped and not GridRefMapped and not LocationNameMapped then
      FParserClassname := 'TRequiredTextParser'
    else
      FParserClassname := 'TTextParser';

  if not Assigned(FParser) and (FParserClassname<>'') then
  begin
    FParser := TImportWizardParserClass(GetClass(FParserClassname)).Create;
    FParser.UseOldImportWizard := FUseOldImportWizard;
    if FParser is TTextParser then
      TTextParser(FParser).MaximumLength := FMaximumLength;
  end
  else
  if lClassname <> FParserClassname then begin
    if Assigned(FParser) then
      FParser.Free;
    FParser := TImportWizardParserClass(GetClass(FParserClassname)).Create;
    FParser.UseOldImportWizard := FUseOldImportWizard;
    if FParser is TTextParser then
      TTextParser(FParser).MaximumLength := FMaximumLength;
  end;
  // Note that return result could still be nil
  Result := FParser;
end;  // TLocationInfoColumnType.GetParser

{-------------------------------------------------------------------------------
  Add TColumnType objects to the given list for the specified
  type ID.
}
class procedure TLocationInfoColumnType.LoadTypes(const TypeID: String; TypeList: TObjectList;
    UseOldImportWizard: Boolean);
var
  lInstance: TLocationInfoColumnType;
begin
  with dmDatabase.GetRecordset('usp_IWColumnType_Select', ['@Key', TypeID]) do begin
    if RecordCount = 0 then
      raise EColumnMappingClass.Create(format(ResStr_ColumnTypeMissing, [TypeID]));
    lInstance := TLocationInfoColumnType.Create;
    lInstance.FKey                := TypeID;
    lInstance.FName               := Fields['Item_Name'].Value;
    lInstance.FRequired           := Fields['Required'].Value;
    lInstance.FCommonlyUsed       := Fields['Commonly_Used'].Value;
    lInstance.FParserClassName    := VarToStr(Fields['Parser_Class_Name'].Value);
    lInstance.FTermListTable      := VarToStr(Fields['Term_List_Table'].Value);
    lInstance.FFieldType          := VarToStr(Fields['Field_Type'].Value);
    lInstance.FUseOldImportWizard := UseOldImportWizard;
    if Fields['Maximum_Length'].Value <> Null then
      lInstance.FMaximumLength := Fields['Maximum_Length'].Value;
    lInstance.LoadRelatedTypes;
    TypeList.Add(lInstance);
  end;
end;  // TLocationInfoColumnType.LoadTypes

{-------------------------------------------------------------------------------
}
procedure TLocationInfoColumnType.UpdateFlags(gridRef, location, locationName: Boolean);
begin
  GridRefMapped      := gridRef;
  LocationMapped     := location;
  LocationNameMapped := locationName;
end;  // TLocationInfoColumnType.UpdateFlags

{-==============================================================================
    TColumnMapping
===============================================================================}
{-------------------------------------------------------------------------------
  Initialisation.
  The given data set must have at least one row.
}
constructor TColumnMapping.Create(DataSource: TDataSet; UseOldImportWizard: Boolean);
begin
  FColumns := TStringList.Create;
  FColumnTitles := TStringList.Create;
  FErrors := TStringList.Create;
  FTypes := TObjectList.Create(True);
  FTermFieldsRequired := TObjectList.Create(False);
  LoadColumns(DataSource);
  LoadTypes(UseOldImportWizard);
end;  // TColumnMapping.Create

{-------------------------------------------------------------------------------
}
destructor TColumnMapping.Destroy;
begin
  FColumns.Free;
  FColumnTitles.Free;
  FErrors.Free;
  FTermFieldsRequired.Free;
  FTypes.Free;
  
  inherited Destroy;
end;  // TColumnMapping.Destroy

{-------------------------------------------------------------------------------
}
function TColumnMapping.ColumnTypeByKey(AKey: string): TColumnType;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to TypeCount - 1 do
    if Types[i].Key = AKey then begin
      Result := Types[i];
      Break;
    end;
end;  // TColumnMapping.ColumnTypeByKey

{-------------------------------------------------------------------------------
  Retrieve a column type from the list by key, and qualification (used for
      measurement columns
}
function TColumnMapping.ColumnTypeByKey(AKey, AQualification: string):
    TColumnType;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to TypeCount - 1 do
    if (Types[i].Key = AKey) and (Types[i].Qualification = AQualification) then begin
      Result := Types[i];
      Break;
    end;
end;  // TColumnMapping.ColumnTypeByKey
 
{-------------------------------------------------------------------------------
}
function TColumnMapping.GetTermFieldsRequired(Index: Integer): TColumnType;
begin
  Result := TColumnType(FTermFieldsRequired[Index]);
end;  // TColumnMapping.GetTermFieldsRequired 

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetTermFieldsRequiredCount: Integer;
begin
  Result := FTermFieldsRequired.Count;
end;  // TColumnMapping.GetTermFieldsRequiredCount

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetColumnCount: integer;
begin
  Result := FColumns.Count;
end;

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetColumnTitleByName(AFieldName: string): string;
begin
  Result := FColumnTitles.Values[AFieldName];
end;

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.SetColumnTitleByName(AFieldName: string; const Value: string);
begin
  FColumnTitles.Values[AFieldName] := Value;
end;

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetColumnTitle(AIndex: integer): string;
begin
  Result := FColumnTitles.Values[FColumnTitles.Names[AIndex]];
end;

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.SetColumnTitle(AIndex: integer;
  const Value: string);
begin
  FColumnTitles.Values[FColumnTitles.Names[AIndex]] := Value;
end;

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetTypeByKey(const AKey: string): TColumnType;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FTypes.Count-1 do
    if TColumnType(FTypes[i]).Key=AKey then
      Result := TColumnType(FTypes[i]);
end;  // TColumnMapping.GetTypeByKey

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetTypeCount: Integer;
begin
  Result := FTypes.Count;
end;  // TColumnMapping.GetTypeCount

{-------------------------------------------------------------------------------
}
function TColumnMapping.GetTypes(Index: Integer): TColumnType;
begin
  Result := TColumnType(FTypes[Index]);
end;  // TColumnMapping.GetTypes

{-------------------------------------------------------------------------------
}
function TColumnMapping.IsMapped(FieldName: string): Boolean;
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOf(FieldName);
  if lIndex=-1 then
    raise EColumnMappingClass.Create(Format(ResStr_InvalidFieldName, [FieldName]));
  Result := Assigned(FColumns.Objects[lIndex]);
end;  // TColumnMapping.IsMapped 

{-------------------------------------------------------------------------------
}
function TColumnMapping.IsMapped(ColumnType: TColumnType): Boolean;
begin
  Result := FColumns.IndexOfObject(ColumnType)<>-1;
end;  // TColumnMapping.IsMapped 

{-------------------------------------------------------------------------------
}
function TColumnMapping.KeyIsMapped(const ColumnTypeKey: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FColumns.Count - 1 do
    if Assigned(FColumns.Objects[I]) then
      if TColumnType(FColumns.Objects[I]).Key = ColumnTypeKey then
      begin
        Result := True;
        Break;
      end;
end;  // TColumnMapping.KeyIsMapped

{-------------------------------------------------------------------------------
  Populate FColumns from the first row of the given data set.
}
procedure TColumnMapping.LoadColumns(DataSource: TDataSet);
var
  i: Integer;
begin
  FColumns.Clear;
  FColumnTitles.Clear;
  for i := 0 to Datasource.Fields.Count-1 do begin
    FColumns.Add(Datasource.Fields[i].FieldName);
    // Name value pair in case we need to change the title
    FColumnTitles.Add(Datasource.Fields[i].FieldName + '=' +
                      Datasource.Fields[i].FieldName);
  end;
end;  // TColumnMapping.LoadColumns

{-------------------------------------------------------------------------------
  Populate Types[] from the database.
}
procedure TColumnMapping.LoadTypes(UseOldImportWizard: Boolean);
var
  lClass: TColumnTypeClass;
  lCurrentSurveyTemp: boolean;
  // Skip inclusion of Observer/TempObserver column for non-temp/temp imports resp.
  // and Determination Date column for the old import wizard.

  function IsIncludedColumn(ColumnKey: String) : Boolean;
  begin
    Result := True;
    if (FIsTempSurvey and (ColumnKey = CT_KEY_OBSERVER)) then
      Result := False;
    if (not FIsTempSurvey and (ColumnKey = CT_KEY_TEMPOBSERVER)) then
      Result := False;
    if (UseOldImportWizard and (ColumnKey = CT_KEY_DETERMINATION_DATE)) then
      Result := False;
  end;
begin
  //CurrentChange
  if (not UseOldImportWizard) then begin
    lCurrentSurveyTemp := dmDatabase.GetStoredProcOutputParam('usp_Survey_TempSurvey',
                                            ['@Key', AppSettings.IWSurveyKey],
                                            '@Value');
    AppSettings.IWTempNameKey := dmDatabase.GetStoredProcOutputParam('usp_Setting_Get',
                                            ['@Name', 'TempName'],
                                            '@Value');
    if not lCurrentSurveyTemp then begin
      FIsTempSurvey := False;
      AppSettings.IWTempNameKey := '';
    end else
      FIsTempSurvey := True;

  end else begin
    AppSettings.IWTempNameKey := '';
    FIsTempSurvey := False;
  end;
  AppSettings.IWIsTempSurvey := FIsTempSurvey;
  // Instantiate each column type
  with dmDatabase.GetRecordset('usp_IWColumnTypes_Select', []) do begin
    if RecordCount>0 then begin
      MoveFirst;
      while not EOF do begin
        try
          lClass := TColumnTypeClass(GetClass(Fields['Class_Name'].Value));
          if not Assigned(lClass) then
            raise EColumnMappingClass.Create(Format(ResStr_ColumnTypeClassUnknown,
                                                    [Fields['Class_Name'].Value]));
          if IsIncludedColumn(Fields['IW_Column_Type_Key'].Value) then
            lClass.LoadTypes(Fields['IW_Column_Type_Key'].Value, FTypes, UseOldImportWizard);
        except
          on E:Exception do
            ShowInformation(Format(
                ResStr_ColumnTypeClassLoadFailure,
                [Fields['IW_Column_Type_Key'].Value, E.Message]));
        end;
        MoveNext;
      end;
    end;
  end;
end;  // TColumnMapping.LoadTypes

{-------------------------------------------------------------------------------
  For each column, look for matching column types.
  If exactly one match is found then map the column to that 
  type. 
}
procedure TColumnMapping.MapAutomatically;
var
  i: Integer;
  lColumnType: TColumnType; 
begin
  IsDirty := True;
  // Assign all unmapped columns automatically, ignore any that match twice
  for i := 0 to FColumns.Count - 1 do
    if not Assigned(FColumns.Objects[i]) then begin
      with dmDatabase.GetRecordset('usp_ColumnTitleMatches_Select',
           ['@ColumnTitle', FColumnTitles.Values[FColumnTitles.Names[i]]]) do
        if RecordCount = 1 then begin
          lColumnType := GetTypeByKey(Fields['IW_Column_Type_Key'].Value);
          if not IsMapped(lColumnType) then
            FColumns.Objects[i] := lColumnType;
        end;
    end;
  UpdateLocationInfoColumns;
end;  // TColumnMapping.MapAutomatically

{-------------------------------------------------------------------------------
  Map the specified column onto the specified type, replacing
  any existing mappings.
}
procedure TColumnMapping.MapColumn(const AFieldName: string; AType: TColumnType);
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOf(AFieldName);
  if lIndex=-1 then
    raise EColumnMappingClass.Create(Format(ResStr_InvalidFieldName, [AFieldName]));
  IsDirty := True;
  UnmapType(AType);
  FColumns.Objects[lIndex] := AType;

  if AType is TLocationInfoColumnType then UpdateLocationInfoColumns;
end;  // TColumnMapping.MapColumn

{-------------------------------------------------------------------------------
  Returns the field name of the mapped column.
}
function TColumnMapping.MappedColumn(AType: TColumnType): String;
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOfObject(AType);
  if lIndex>-1 then
    Result := FColumns[lIndex]
  else
    Result := '';
end;  // TColumnMapping.MappedColumn 

{-------------------------------------------------------------------------------
  Mapped type for the specified field, or nil if not mapped. 
}
function TColumnMapping.MappedType(const FieldName: string): TColumnType;
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOf(FieldName);
  if lIndex=-1 then
    raise EColumnMappingClass.Create(Format(ResStr_InvalidFieldName, [FieldName]));
  Result := TColumnType(FColumns.Objects[lIndex]);
end;  // TColumnMapping.MappedType 

{-------------------------------------------------------------------------------
  Refresh the list of errors regarding inter-column dependencies. 
}
procedure TColumnMapping.RefreshErrors;
var
  lTypeIdx: Integer;
  i: Integer;
begin
  FErrors.Clear;
  for lTypeIdx := 0 to FTypes.Count-1 do begin
    with TColumnType(FTypes[lTypeIdx]) do begin
      if IsMapped(TColumnType(FTypes[lTypeIdx])) then begin
        for i := 0 to RelatedTypeCount-1 do
          if RelatedTypes[i].RelationshipType in [rtRequires, rtFieldDependencyColumnRequired] then
          begin
            if not IsMapped(ColumnTypeByKey(RelatedTypes[i].ColumnTypeKey)) then
              FErrors.Add(Format(ResStr_ColumnDependency,
                  [Name, ColumnTypeByKey(RelatedTypes[i].ColumnTypeKey).Name]));
          end else
          if RelatedTypes[i].RelationshipType = rtConflictsWith then
          begin
            if IsMapped(ColumnTypeByKey(RelatedTypes[i].ColumnTypeKey)) then
              FErrors.Add(Format(ResStr_ColumnConflict,
                  [Name, ColumnTypeByKey(RelatedTypes[i].ColumnTypeKey).Name]));
          end;
      end
      else if Required and (TermListTable = '') then
        FErrors.Add(Format(ResStr_ColumnTypeRequired, [Name]));
    end;
  end;
end;  // TColumnMapping.RefreshErrors

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.RefreshRequiredTerms;
var
  I, J: Integer;
  lConflicts: Boolean;

    // Check if a type is always required, or required because of another mapping
    function IsRequired(AType: TColumnType): boolean;
    var
      lIdx: integer;
    begin
      Result := AType.Required;
      if not Result then
        for lIdx := 0 to AType.RelatedTypeCount-1 do begin
          if AType.RelatedTypes[lIdx].RelationshipType = rtRequiredBy then
            Result := Result or KeyIsMapped(AType.RelatedTypes[lIdx].ColumnTypeKey);
        end;
    end; // try

begin
  FTermFieldsRequired.Clear;
  for I := 0 to TypeCount-1 do
    if (Types[I].TermListTable <> '') and (not IsMapped(Types[I])) and
       IsRequired(Types[i]) then
    begin
      lConflicts := False;
      for J := 0 to Types[I].RelatedTypeCount - 1 do
        if Types[I].RelatedTypes[J].RelationshipType = rtConflictsWith then
          if KeyIsMapped(Types[I].RelatedTypes[J].ColumnTypeKey) then
          begin
            lConflicts := True;
            Break;
          end;
      if not lConflicts then FTermFieldsRequired.Add(Types[I]);
    end;
end;  // TColumnMapping.RefreshRequiredTerms

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.RefreshTermLists;
var
  i: Integer;
begin
  for i := 0 to FColumns.Count - 1 do
    if Assigned(FColumns.Objects[i]) then
      TColumnType(FColumns.Objects[i]).RefreshTermLists;
end;  // TColumnMapping.RefreshTermLists

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.ResetColumnTitle(AIndex: integer);
begin
  // reset the name of the column to its original field name
  FColumnTitles[AIndex] := FColumns[AIndex] + '=' + FColumns[AIndex];
end;

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.SetIsDirty(Value: Boolean);
begin
  FIsDirty := Value;
end;  // TColumnMapping.SetIsDirty

{-------------------------------------------------------------------------------
  Remove any mapping for the specified column. 
}
procedure TColumnMapping.UnmapColumn(const FieldName: string);
var
  lIndex: Integer;
  mappedTypeIsLocation: Boolean;
begin
  mappedTypeIsLocation := MappedType(FieldName) is TLocationInfoColumnType;
  lIndex := FColumns.IndexOf(FieldName);
  if lIndex=-1 then
    raise EColumnMappingClass.Create(Format(ResStr_InvalidFieldName, [FieldName]));
  IsDirty := True;
  FColumns.Objects[lIndex] := nil;
  if mappedTypeIsLocation then UpdateLocationInfoColumns;
end;  // TColumnMapping.UnmapColumn

{-------------------------------------------------------------------------------
  Remove any mapping for the specified column type by index.
}
procedure TColumnMapping.UnmapType(TypeIndex: Integer);
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOfObject(FTypes[TypeIndex]);
  if lIndex>-1 then
    FColumns.Objects[lIndex] := nil;
end;  // TColumnMapping.UnmapType

{-------------------------------------------------------------------------------
  Remove any mapping for the specified column type.
}
procedure TColumnMapping.UnmapType(AType: TColumnType);
var
  lIndex: Integer;
begin
  lIndex := FColumns.IndexOfObject(AType);
  if lIndex>-1 then
    FColumns.Objects[lIndex] := nil;
end;  // TColumnMapping.UnmapType

{-------------------------------------------------------------------------------
}
procedure TColumnMapping.UpdateLocationInfoColumns;
var
  gridRefMapped: Boolean;
  locationMapped: Boolean;
  locationNameMapped: Boolean;
begin
  gridRefMapped      := KeyIsMapped(CT_KEY_GRID_REFERENCE);
  locationMapped     := KeyIsMapped(CT_KEY_LOCATION);
  locationNameMapped := KeyIsMapped(CT_KEY_LOCATION_NAME);

  TLocationInfoColumnType(ColumnTypeByKey(CT_KEY_LOCATION)).UpdateFlags(
      gridRefMapped, locationMapped, locationNameMapped);

  TLocationInfoColumnType(ColumnTypeByKey(CT_KEY_GRID_REFERENCE)).UpdateFlags(
      gridRefMapped, locationMapped, locationNameMapped);

  TLocationInfoColumnType(ColumnTypeByKey(CT_KEY_LOCATION_NAME)).UpdateFlags(
      gridRefMapped, locationMapped, locationNameMapped);
end;  // TColumnMapping.UpdateLocationInfoColumns

{-------------------------------------------------------------------------------
  Check whether the mappings defined are valid, based on the
  required types and conflicting types for each mapped column
  type.
  Updates ErrorCount and Errors.
}
procedure TColumnMapping.Validate;
begin
  // TODO: perform validation
  IsDirty := False;
end;  // TColumnMapping.Validate

{-------------------------------------------------------------------------------
  Checks that if a column type relationship requires that a field has a matching value in
      another field, then the other value is populated.
  Returns the name of the required column if failed.
}
function TColumnMapping.ValidateFieldDependency(AType: TColumnType; ADataset: TDataset): String;
var
  i: Integer;
  lRelType: TRelatedType;
  lColIdx: Integer;
  lValue: String;
  lRelatedColumn: TColumnType;
  lRelatedValue: String;
  lDeterminationDate,lReviewDate: TVagueDate;
  lDate: TVagueDate;

  function GetValue(key: String): String;
  var
    colType: TColumnType;
  begin
    colType := ColumnTypeByKey(key);
    Result := ADataset.FieldByName(FColumns[FColumns.IndexOfObject(colType)]).AsString;
  end;

begin
  Result := '';
  lColIdx := FColumns.IndexOfObject(AType);
  if lColIdx > -1 then begin
    lValue := ADataset.FieldByName(FColumns[lColIdx]).AsString;
    for i := 0 to AType.RelatedTypeCount - 1 do begin
      lRelType := AType.RelatedTypes[i];
      lRelatedColumn := ColumnTypeByKey(lRelType.ColumnTypeKey);
      lColIdx := FColumns.IndexOfObject(lRelatedColumn);
      if lColIdx > -1 then begin
        lRelatedValue := ADataset.FieldByName(FColumns[lColIdx]).AsString;
        //workinghere
        if (lRelType.RelationshipType in
            [rtRequires, rtFieldDependencyColumnRequired, rtFieldDependency]) then
        begin
          // A value is require in the other field, so check it exists
          if (lValue <> '') and (lRelatedValue = '') then
            Result := Format(ResStr_FieldRequired, [AType.Name, lRelatedColumn.Name])
          else if (AType.Key = CT_KEY_REVIEW_DATE) and (lRelType.RelationshipType = rtFieldDependencyColumnRequired) then
          begin
            if (Trim(lValue) = '') then
              lValue := lRelatedValue;
            if not IsVagueDate(lRelatedValue) then
              Result := ''
            else if not IsVagueDate(lValue) then
              Result := ResStr_ReviewDateNotValid
            else begin
              lReviewDate := StringToVagueDate(lValue);
              lDate := StringToVagueDate(lRelatedValue);
              if IsVagueDateInVagueDate(lDate, lReviewDate) then
                Result := ResStr_ReviewDateBeforeSampleDate
              else if AreVagueDatesEqual(lDate, lReviewDate) then
              else if (CompareVagueDateToVagueDate(lReviewDate, lDate) < 0) then
                Result := ResStr_ReviewDateBeforeSampleDate;
            end;
          end
          // Validates the Determination Date column against the Date column
          // If the determination date is empty but the date isn't, not invalid -
          // we copy from the date column later in IWOutputFieldGenerators

          else if (AType.Key = CT_KEY_DETERMINATION_DATE) then
          begin
            if (Trim(lValue) = '') then
              lValue := lRelatedValue;
            if not IsVagueDate(lRelatedValue) then
              // We can't compare with the value from the "Date" column if it
              // is itself invalid, but in this case the "Date" column will be
              // marked as invalid so don't complain here as well (this
              // behaviour is consistent with TLargeImportFile.ParseData's
              // ParseMultiFieldColumns procedure).
              Result := ''
            else if not IsVagueDate(lValue) then
              Result := ResStr_DeterminationDateNotValid
            else begin
              lDeterminationDate := StringToVagueDate(lValue);
              lDate := StringToVagueDate(lRelatedValue);
              if IsVagueDateInVagueDate(lDate, lDeterminationDate) then
                Result := ResStr_DeterminationDateBeforeSampleDate
              else if AreVagueDatesEqual(lDate, lDeterminationDate) then
              else if (CompareVagueDateToVagueDate(lDeterminationDate, lDate) < 0) then
                Result := ResStr_DeterminationDateBeforeSampleDate;
            end;
          end;
        end else
        if (lRelType.RelationshipType = rtOneRequired) then begin
          // It's all a bit weird with the location stuff, but they asked for it...
          // GridRef and LocationName work kinda together.
          If (AType is TLocationInfoColumnType) then begin
            with TLocationInfoColumnType(AType) do begin
              if GridRefMapped and not LocationMapped and LocationNameMapped then begin
                if (GetValue(CT_KEY_GRID_REFERENCE) = '') and
                  (GetValue(CT_KEY_LOCATION_NAME) = '') then
                   Result := Format(ResStr_OneFieldRequired,
                   [ColumnTypeByKey(CT_KEY_GRID_REFERENCE).Name,
                   ColumnTypeByKey(CT_KEY_LOCATION_NAME).Name]);
              end else
              if not GridRefMapped and  LocationMapped and LocationNameMapped then begin
                if (GetValue(CT_KEY_LOCATION_NAME) = '') and
                   (GetValue(CT_KEY_LOCATION) = '') then
                    Result := Format(ResStr_ValueRequired,
                    [ColumnTypeByKey(CT_KEY_LOCATION).Name]);
              end else
              if GridRefMapped and LocationMapped and not LocationNameMapped then begin
                if (GetValue(CT_KEY_GRID_REFERENCE) = '') and
                   (GetValue(CT_KEY_LOCATION) = '') then
                    Result := Format(ResStr_ValueRequired,
                    [ColumnTypeByKey(CT_KEY_LOCATION_NAME).Name])
              end else
              if  GridRefMapped and LocationMapped and LocationNameMapped then begin
               if  (GetValue(CT_KEY_GRID_REFERENCE)= '') and
                   (GetValue(CT_KEY_LOCATION_NAME)= '') and
                   (GetValue(CT_KEY_LOCATION)= '') then
                    Result := Format(ResStr_ValueRequired,
                    [ColumnTypeByKey(CT_KEY_GRID_REFERENCE).Name])

              end;
            end; // end of with
           end else
          // One or the other required.
          if (lValue = '') and  (lRelatedValue = '') then
            Result := Format(ResStr_OneFieldRequired, [AType.Name, lRelatedColumn.Name]);
        end;
      end;
    end;
  end;
end;  // TColumnMapping.ValidateFieldDependency

{-==============================================================================
    TImportFile
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TImportFile.Create(DataSource: TDataSet; LargeFields: TStringList;
    UserSuppliedData: TUserSuppliedData; UseOldImportWizard: Boolean);
begin
  inherited Create;
  FDatasource            := Datasource;
  FLargeFields           := LargeFields;
  FUserSuppliedData      := UserSuppliedData;
  FMatchRules            := TStringList.Create;
  FColumnMapping         := TColumnMapping.Create(Datasource, UseOldImportWizard);
  FTables                := TStringList.Create;
  FMultiRecordTableTypes := TObjectList.Create(False);
  FOutputTables          := TObjectList.Create;
  FLastParsedValues      := TStringList.Create;
end;  // TImportFile.Create

{-------------------------------------------------------------------------------
}
destructor TImportFile.Destroy;
begin
  DropWorkingTables;
  FOutputTables.Free;
  FMultiRecordTableTypes.Free;
  FColumnMapping.Free;
  ClearMatchRules;
  FMatchRules.Free;
  FTables.Free;
  FLastParsedValues.Free;
  inherited Destroy;
end;  // TImportFile.Destroy

{-------------------------------------------------------------------------------
}
function TImportFile.Aborted: Boolean;
begin
  if not Assigned(AbortPredicate) then
    Result := False
  else
    Result := AbortPredicate;
end;  // TImportFile.Aborted

{-------------------------------------------------------------------------------
}
procedure TImportFile.ClearMatchRules;
var
  i: Integer;
begin
  for i := 0 to FMatchRules.Count - 1 do
    TMatchRule(FMatchRules.Objects[i]).Free;
  FMatchRules.Clear;
end;

{-------------------------------------------------------------------------------
  Adds a match rule to the internal list and returns it.  Only 1 of each rule is kept.
}
function TImportFile.AddMatchRule(const AKey: string): TMatchRule;
var
  lIdx: Integer;
begin
  lIdx := FMatchRules.IndexOf(AKey);
  if lIdx=-1 then begin
    Result := TMatchRule.Create(AKey);
    // Insert rule in the right place to follow proper sequence.
    for lIdx := 0 to MatchRuleCount - 1 do
      if Result.Sequence < MatchRules[lIdx].Sequence then
      begin
        FMatchRules.InsertObject(lIdx, AKey, Result);
        Exit;
      end;
    // Get here if have to add at the end.
    FMatchRules.AddObject(AKey, Result);
  end else
    Result := TMatchRule(FMatchRules.Objects[lIdx]);
end;  // TImportFile.AddMatchRule

{-------------------------------------------------------------------------------
  Instantiates the table rule object for the given key.
}
function TImportFile.CreateTableRule(const Key: String): TObject;
begin
  Result := TTableRule.Create(Key, Self);
end;

{-------------------------------------------------------------------------------
}
procedure TImportFile.ApplyTableRulePostProcessing;
begin
  with dmDatabase.GetRecordset(
      'usp_ImportWizard_PostTableRuleProcedures_Select', 
      []) do
    while not Eof do
    begin
      dmDatabase.RunStoredProc(Fields['Procedure_Name'].Value, []);
      MoveNext;
    end;
end;  // TImportFile.ApplyTableRulePostProcessing

{-------------------------------------------------------------------------------
}
procedure TImportFile.ApplyTableRules;
var
  I: Integer;
  lApply: Boolean;
  lDestroy: Boolean;
  lRule: TTableRule;
  lNamesSeen: TStringList;
  lCursor: TCursor;
begin
  // This has to be done after the matching to avoid errors.
  PopulateChecksumTables;
  DropOutputTables;
  lNamesSeen := TStringList.Create;
  lCursor    := AppStartCursor;
  try
    lNamesSeen.Sorted := True;
    with dmDatabase.GetRecordset('usp_IWTableRules_Select', []) do
    begin
      FTableRuleCount := RecordCount;
      FTableRuleIndex := 0;

      while not (Eof or Aborted) do
      begin
        lApply   := True;
        lDestroy := True;
        lRule    := TTableRule(CreateTableRule(Fields['IW_Table_Rule_Key'].Value));
        try
          UpdateStatus(Format(ResStr_ProcessingTable, [ReadableFormat(lRule.TableName)]));

          for I := 0 to lRule.RequiredFieldCount - 1 do
            if not (ColumnMapping.KeyIsMapped(lRule.RequiredFields[I]) or Aborted) then
            begin
              lApply := False;
              Break;
            end;

          if Aborted then
            lDestroy := True
          else
          if lApply then
          begin
            if lNamesSeen.IndexOf(lRule.TableName) = -1 then
            begin
              lNamesSeen.Add(lRule.TableName);
              FOutputTables.Add(lRule);
              lDestroy := False;
            end;
            lRule.OnProgress := TableRuleProgress;
            lRule.Apply;
          end;
        finally
          if lDestroy then lRule.Free;
        end;
        TableRuleProgress(100, True);
        Inc(FTableRuleIndex);
        MoveNext;
      end;
    end;  // with
  finally
    lNamesSeen.Free;
    DefaultCursor(lCursor);
  end;
  if not Aborted then ApplyTableRulePostProcessing;
end;  // TImportFile.ApplyTableRules

{-------------------------------------------------------------------------------
  Create temporary tables to hold parsed imported data prior to
  matching.

  The tables created are:

      #master
          -- has a column 'Record_No INT'.
          -- has primary key (Record_No)
          -- has a column for each mapped column type without
             a parser; the column's name and data type are
             given by the column type's Field property.
          -- has a column for each parsed field from a single
             record parser associated with a mapped column type;
             the column is named by joining the name of the
             parsed field onto the end of the column type key
             with a '_', and the data type is as defined for the
             parsed field.

      #CT_<ColumnType.Key>
          -- one table for each mapped column type that is
             associated with a multi-record parser.
          -- has a column 'Record_No INT'.
          -- has a column 'Position INT'.
          -- has primary key (Record_No, Position)
          -- has a column for each parsed field associated with
             the parser; the column has the name and data type
             defined for the parsed field.

      #CS_<ColumnType.Key>
          -- one table for each mapped column type that is
             associated with a multi-record parser.
          -- has a column 'Record_No INT'
          -- has a column 'Checksum INT'
}
procedure TImportFile.CreateImportTables;
var
  lColumns: String;
  i: Integer;
  lColumnType: TColumnType;
begin
  lColumns := GetMasterColumns(True);
  dmDatabase.ExecuteSQL('CREATE TABLE #Master (' + lColumns + ')');
  FTables.Add('#Master');

  // Find and process each matched column where the parser potentially creates
  // several records
  for i := 0 to FDatasource.FieldCount-1 do begin
    lColumnType := ColumnMapping.MappedType(FDatasource.Fields[i].FieldName);
    if Assigned(lColumnType) then begin
      if Assigned(lColumnType.Parser) then begin
        if not lColumnType.Parser.SingleRecord then begin
          // field is potentially parsed into several records, so need a
          // separate table, created later
          if FMultiRecordTableTypes.IndexOf(lColumnType)=-1 then
            FMultiRecordTableTypes.Add(lColumnType);
        end;
      end;
    end;
  end;

  // Now create tables for parsers that have multiple records
  for i := 0 to FMultiRecordTableTypes.Count - 1 do
    CreateImportTable(TColumnType(FMultiRecordTableTypes[i]));
end;  // TImportFile.CreateImportTables

{-------------------------------------------------------------------------------
}
procedure TImportFile.CreateImportTable(columnType: TColumnType);
var
  lTable: String;
  lColumns: String;
  i: Integer;
begin
  lColumns := #10#9'Record_No INT NOT NULL, '
      + #10#9'Position INT NOT NULL, '
      + #10#9'IW_Qualification VARCHAR(100) NOT NULL ';
  with columnType.Parser do
    for i := 0 to FieldCount - 1 do
      if (Pos('char', LowerCase(Fields[i].DataType)) <> 0) or
         (Pos('text', LowerCase(Fields[i].DataType)) <> 0) then
        AddToCommaSeparatedList(lColumns, #10#9 + Fields[i].Name + ' ' + Fields[i].DataType +
                                          ' COLLATE SQL_Latin1_General_CP1_CI_AS')
      else
        AddToCommaSeparatedList(lColumns, #10#9 + Fields[i].Name + ' ' + Fields[i].DataType);

  // Always run the CREATE statement (with the existence check first, obviously)
  lTable := '#CT_' + columnType.Key;
  dmDatabase.ExecuteSQL(
      'IF Object_Id(''tempdb..' + lTable + ''') IS NULL'
      + #10#9'CREATE TABLE ' + lTable + ' ('
      + lColumns
      + #10#9' PRIMARY KEY (Record_No, Position, IW_Qualification))');
  if FTables.IndexOf(lTable) = -1 then
    FTables.Add(lTable);

  // Always run the CREATE statement (with the existence check first, obviously)
  lTable := '#CS_' + columnType.Key;
  dmDatabase.ExecuteSQL(
      'IF Object_Id(''tempdb..' + lTable + ''') IS NULL'
      + #10#9'CREATE TABLE ' + lTable +
      ' (Record_No INT, Checksum INT)');
  if FTables.IndexOf(lTable) = -1 then
    FTables.Add(lTable);
end;  // TImportFile.CreateImportTable

{-------------------------------------------------------------------------------
}
procedure TImportFile.DropTemporaryTable(const Name: String);
var
  lSql: String;
  i: Integer;
begin
  lSql := 'IF NOT object_id(''tempdb..#' + Name + ''') IS NULL'
      + ' DROP TABLE #' + Name;
  dmDatabase.ExecuteSql(lSql);

  // Remove table name from list.
  i := FTables.IndexOf('#' + Name);
  if i > 0 then FTables.Delete(i);
end;

{-------------------------------------------------------------------------------
}
procedure TImportFile.DropOutputTables;
var
  I: Integer;
begin
  if Assigned(FOutputTables) then
  begin
    for I := 0 to FOutputTables.Count - 1 do
      with FOutputTables[I] as TTableRule do
      begin
        DropTemporaryTable(TableName);
        DropTemporaryTable('RN_' + TableName);
      end;
    FOutputTables.Clear;
  end;
end;  // TImportFile.DropOutputTables

{-------------------------------------------------------------------------------
}
procedure TImportFile.InternalDropWorkingTables;
var
  I: Integer;
  tempTables: TStringList;
begin
  DropTemporaryTable('Master');
  DropTemporaryTable('IW_Group');

  tempTables := TStringList.Create;
  try
    tempTables.Sorted     := True;
    tempTables.Duplicates := dupIgnore;

    if Assigned(ColumnMapping) then
      for I := 0 to ColumnMapping.TypeCount - 1 do
        with ColumnMapping.Types[I] do
        begin
          // The check for table existence is done in the DROP statement.
          // No need to get a parser to repeat that check (especially taxon data ones
          // as each query the DB for restricted values).
          tempTables.Add('CT_' + Key);
          tempTables.Add('CS_' + Key);
        end;

    if Assigned(FMatchRules) then
      for I := 0 to MatchRuleCount - 1 do
        if Assigned(MatchRules[i]) then
          tempTables.Add(MatchRules[I].Name);

    for i := 0 to tempTables.Count - 1 do
      DropTemporaryTable(tempTables[i]);
  finally
    tempTables.Free;
  end;
  FTables.Clear;
end;  // TImportFile.InternalDropWorkingTables

{-------------------------------------------------------------------------------
}
procedure TImportFile.DropWorkingTables;
begin
  InternalDropWorkingTables;
  DropOutputTables;
end;  // TImportFile.DropWorkingTables

{-------------------------------------------------------------------------------
}
procedure TImportFile.GenerateImportDatabase;
var
  lDb: TADOConnection;
  I: Integer;
begin
  try
    UpdateStatus(ResStr_CreatingDatabase);
    UpdateProgress(0);
    FImportDatabase :=
        UniqueFileName(GetWindowsTempDir + TEMP_DB_FOLDER, 'IMP');
    lDb := dmDatabase.CreateImportExportDatabase(FImportDatabase);
    try
      // Finished with #Master and all #CT_/#CS_ tables.
      InternalDropWorkingTables;

      for I := 0 to FOutputTables.Count - 1 do
      begin
        if Aborted then Break;
        UpdateStatus(Format(
            ResStr_CreatingDatabaseTable,
            [ReadableFormat(TTableRule(FOutputTables[I]).TableName)]));
        GenerateImportTable(lDb, I);
      end;
    finally
      lDb.Free;
    end;
    
  finally
    UpdateStatus('');
    UpdateProgress(0);
  end;
end;  // TImportFile.GenerateImportDatabase

{-------------------------------------------------------------------------------
}
procedure TImportFile.GenerateImportTable(Connection: TADOConnection; OutputTableIndex: Integer);
var
  I: Integer;
  lRule: TTableRule;
  lColumnList: String;
  lSql: String;
  lValue: String;
begin
  lRule := FOutputTables[OutputTableIndex] as TTableRule;
  dmDatabase.CreateImportExportTable(Connection, lRule.TableName);

  lColumnList := '';
  for I := 0 to lRule.OutputFieldCount - 1 do
  begin
    if I > 0 then lColumnList := lColumnList + ', ';
    { must use icky, non-standard, Microsoft form to quote field names
      since Access cannot cope with the correct notation }
    lColumnList := lColumnList + '[' + lRule.OutputFields[I].Name + ']';
  end;

  lSql := 'SELECT ' + lColumnList + ' FROM #' + lRule.TableName;
  with dmDatabase.ExecuteSql(lSql, True) do
    while not Eof do
    begin
      if Aborted then Break;

      lSql := '';

      for I := 0 to Fields.Count - 1 do
      begin
        lValue := AccessSqlLiteral(Fields[I].Value);

        if SameText(lRule.OutputFields[I].DataType, 'DATETIME') then
          lValue := StringReplace(lValue, DateSeparator, '/', [rfReplaceAll]);

        lSql := lSql
            + IfThen(lSql = '', '', ', ')
            + IfThen(lValue = '''''', 'NULL', lValue); // Change quoted empty string to NULL
      end;

      lSql := 'INSERT INTO ' + lRule.TableName + ' ('
          + lColumnList + ')'
          + ' VALUES ('
          + lSql + ')';
      try
        Connection.Execute(lSql);
      except
        on E: Exception do
          raise EImportFile.CreateNonCritical(Format(
              ResStr_ImportFileGenerationFailed,
              [lRule.TableName, E.Message]));
      end;

      UpdateProgress(
          (100 * OutputTableIndex
          + (100 * Integer(AbsolutePosition) div RecordCount))
          div FOutputTables.Count);

      MoveNext;
    end;
end;  // TImportFile.GenerateImportTable

{-------------------------------------------------------------------------------
  Retrieves the SQL for one column (or more if field is parsed into multiple) in
  the #Master table.
}
function TImportFile.GetMasterColumn(AColumnType: TColumnType; AWithFieldType: Boolean): String;
var
  i: Integer;
  lColumn: String;
begin
  Result := '';
  // One value, one column.
  if (AColumnType.FieldType <> '') and (not Assigned(AColumnType.Parser)) then
  begin
    Result := AColumnType.Key;
    if AWithFieldType then Result := Result + ' ' + AColumnType.FieldType;
  end else
  // One value, possibly multiple fields.
  if Assigned(AColumnType.Parser) then
  begin
    if AColumnType.Parser.SingleRecord then
    begin
      for i := 0 to AColumnType.Parser.FieldCount - 1 do
      begin
        lColumn := AColumnType.Key + '_' + AColumnType.Parser.Fields[i].Name;
        if AWithFieldType then
          if (Pos('char', LowerCase(AColumnType.Parser.Fields[i].DataType)) <> 0) or
             (Pos('text', LowerCase(AColumnType.Parser.Fields[i].DataType)) <> 0) then
            lColumn := lColumn + ' ' + AColumnType.Parser.Fields[i].DataType
                               + ' COLLATE SQL_Latin1_General_CP1_CI_AS'
          else
            lColumn := lColumn + ' ' + AColumnType.Parser.Fields[i].DataType;
        AddToCommaSeparatedList(Result, lColumn);
      end;

    end;
  end;
end;  // TImportFile.GetMasterColumn

{-------------------------------------------------------------------------------
  Retrieves the SQL for the list of columns in the #Master table.
}
function TImportFile.GetMasterColumns(AWithFieldType: Boolean = False): String;
var
  i: Integer;
  lColumnType: TColumnType;
  lColumn: String;
begin
  Result := 'Record_No';
  if AWithFieldType then
    Result := Result + ' INT PRIMARY KEY';
  // Find and process each matched column
  for i := 0 to FDatasource.FieldCount - 1 do begin
    lColumnType := ColumnMapping.MappedType(FDatasource.Fields[i].FieldName);
    if Assigned(lColumnType) then begin
      lColumn := GetMasterColumn(lColumnType, AWithFieldType);
      AddToCommaSeparatedList(Result, lColumn);
    end;
  end;

end;  // TImportFile.GetMasterColumns

{-------------------------------------------------------------------------------
  Gets the value to be passed to the parser of the given column type for the
  specified field in the current record of the import data.
}
function TImportFile.GetValueToParse(const AFieldName: string;
  AColumnType: TColumnType): string;

  function ValueByKey(const Key: string): string;
  var
    lFieldName: string;
  begin
    with ColumnMapping do
      lFieldName := MappedColumn(ColumnTypeByKey(Key));
    Result := FDataSource.FieldByName(lFieldName).AsString;
  end;

begin
  Result := FLargeFields.Values[
      AFieldName + ',' + FDatasource.FieldByName(FLD_ROWID).AsString];

  // if field value too large for client dataset, load from our own string list
  if Result = '' then
    Result := FDatasource.FieldByName(AFieldName).AsString;

  if AColumnType.Parser is TSpatialRefParser then
  begin
    if ColumnMapping.KeyIsMapped(CT_KEY_SPATIAL_SYSTEM) then
      Result := Result + '|' + ValueByKey(CT_KEY_SPATIAL_SYSTEM);
  end
  else if AColumnType.Parser is TSpatialRefSystemParser then
  begin
    if ColumnMapping.KeyIsMapped(CT_KEY_GRID_REFERENCE) then
      Result := ValueByKey(CT_KEY_GRID_REFERENCE) + '|' + Result;
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve the list of data to insert into a row in the #Master table.
}
function TImportFile.GetMasterValue(const AFieldName: String; AColumnType: TColumnType): String;
var
  i: Integer;
  value: String;
begin
  Result := '';
  FLastParsedValues.Clear;
  value := GetValueToParse(AFieldName, AColumnType);

  if (AColumnType.FieldType <> '') and (not Assigned(AColumnType.Parser)) then
  begin
    if RequiresQuotes(AColumnType.FieldType) then
    begin
      FLastParsedValues.Text := QuotedStr(value);
      Result := QuotedStr(value);
    end;
  end else
  if Assigned(AColumnType.Parser) then
  begin
    if AColumnType.Parser.SingleRecord then
    begin
      // Will populate FLastParsedValues.
      AColumnType.Parser.ParseField(value, RecordParsedValues);

      if not AColumnType.Parser.Failed then
        // Multiple values to concatenate before returning.
        for i := 0 to AColumnType.Parser.FieldCount - 1 do begin
          if RequiresQuotes(AColumnType.Parser.Fields[i].DataType) then
            FLastParsedValues[i] := QuotedStr(FLastParsedValues[i]);
          AddToCommaSeparatedList(Result, FLastParsedValues[i]);
        end;
    end;
  end;
end;  // TImportFile.GetMasterValue

{-------------------------------------------------------------------------------
  Retrieve the list of data to insert into a row in the #Master table.
}
function TImportFile.GetMasterValues: String;
var
  i: Integer;
  lColumnType: TColumnType;
begin
  Result := FDatasource.FieldByName(FLD_ROWID).AsString;
  // Find and process each matched column
  for i := 0 to FDatasource.FieldCount - 1 do begin
    lColumnType := ColumnMapping.MappedType(FDatasource.Fields[i].FieldName);
    if Assigned(lColumnType) then
      // Add value(s) to existing ones.
      AddToCommaSeparatedList(
          Result,
          GetMasterValue(FDataSource.Fields[i].FieldName, lColumnType));
  end;
end;  // TImportFile.GetMasterValues

{-------------------------------------------------------------------------------
}
function TImportFile.GetMatchRuleCount: Integer;
begin
  Result := FMatchRules.Count;
end;  // TImportFile.GetMatchRuleCount 

{-------------------------------------------------------------------------------
}
function TImportFile.GetMatchRules(Index: Integer): TMatchRule;
begin
  Result := (FMatchRules.Objects[Index]) as TMatchRule;
end;  // TImportFile.GetMatchRules 

{-------------------------------------------------------------------------------
}
procedure TImportFile.DoParseErrorChanged(Sender: TObject; const FieldName: String; Error: Boolean);
begin
  if Assigned(FOnParseErrorChanged) then
    FOnParseErrorChanged(Sender, FieldName, Error);
end;  // TImportFile.DoParseErrorChanged

{-------------------------------------------------------------------------------
  Gets the validation error message (if available) for the specified field in
  the current record. Does *not* raise the ParseErrorChanged event.
}
function TImportFile.GetParseError(const FieldName: string): String;
var
  lMappedType: TColumnType;
  lParser: TImportWizardParser;
begin
  Result := '';
  if ColumnMapping.IsMapped(FieldName) then
  begin
    lMappedType := ColumnMapping.MappedType(FieldName);
    lParser := lMappedType.Parser;

    if Assigned(lParser) then
    begin
      lParser.ParseField(
          GetValueToParse(FieldName, lMappedType),
          nil);

      if lParser.Failed then Result := lParser.LastErrorMessage;
    end;

    if Result = '' then
    begin
      Result := ColumnMapping.ValidateFieldDependency(lMappedType, FDatasource);
    end
  end
end;  // TImportFile.GetParseError

{-------------------------------------------------------------------------------
  Populate MatchRules from mapped column types.
}
procedure TImportFile.InitialiseMatching;
var
  i: Integer;
  lColumnType: TColumnType;
  j: Integer;
begin
  frmMain.SetStatus(ResStr_InitMatching);
  try
    for i := 0 to FDatasource.FieldCount-1 do begin
      lColumnType := ColumnMapping.MappedType(FDatasource.Fields[i].FieldName);
      if Assigned(lColumnType) then
        if Assigned(lColumnType.Parser) then
          for j := 0 to lColumnType.Parser.FieldCount-1 do
            if lColumnType.FieldMatchRuleKeys[j]<>'' then begin
              with AddMatchRule(lColumnType.FieldMatchRuleKeys[j]) do
                LoadImportedData(lColumnType, lColumnType.Parser.Fields[j].Name);
            end;
      frmMain.SetProgress(i * 100 div FDatasource.FieldCount);
    end;

  finally
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;  // TImportFile.InitialiseMatching

{-------------------------------------------------------------------------------
  Load data from the import source into temporary tables
  as produced by CreateImportTables.  Assumes that the data
  will parse with no errors. 
}
procedure TImportFile.LoadData;
begin
  FDatasource.DisableControls;
  try
    PopulateMasterTable;
    PopulateMultiRecordTables;
  finally
    FDatasource.EnableControls;
  end;
end;  // TImportFile.LoadData

{-------------------------------------------------------------------------------
}
function TImportFile.MatchRuleByKey(const AKey: String): TMatchRule;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to MatchRuleCount - 1 do
    if MatchRules[I].Key = AKey then
    begin
      Result := MatchRules[I];
      Break;
    end;
end;  // TImportFile.MatchRuleByKey 

{-------------------------------------------------------------------------------
}
procedure TImportFile.ParseColumn(const FieldName: string);
begin
  if ColumnMapping.IsMapped(FieldName) then
    ParseRelatedColumns(ColumnMapping.MappedType(FieldName), True);
end;  // TImportFile.ParseColumn

{-------------------------------------------------------------------------------
  Forces a parse of all columns that have relationships from the given column
  (optionally including that column itself).
}
procedure TImportFile.ParseRelatedColumns(AColumn: TColumnType;
  IncludeSelf: Boolean = False);
var
  I: Integer;
  lRelatedColumn: TColumnType;
  lColumns: TStringList;
  lRelatedKey: String;
begin
  lColumns := TStringList.Create;
  try
    if IncludeSelf then
      lColumns.Add(ColumnMapping.MappedColumn(AColumn));
    
    for I := 0 to AColumn.RelatedTypeCount - 1 do
    begin
      lRelatedKey := AColumn.RelatedTypes[I].ColumnTypeKey;
      if ColumnMapping.KeyIsMapped(lRelatedKey) then begin
        lRelatedColumn := ColumnMapping.ColumnTypeByKey(lRelatedKey);
        lColumns.Add(ColumnMapping.MappedColumn(lRelatedColumn));
      end;
    end;
    ParseColumnList(lColumns);
  finally
    lColumns.Free;
  end;
end;  // TImportFile.ParseRelatedColumns

{-------------------------------------------------------------------------------
}
procedure TImportFile.ParseColumnList(AList: TStringList);
var
  lCursor: TCursor;
  i: integer;
begin
  if AList.Count > 0 then
  begin
    lCursor := HourglassCursor;
    try
      frmMain.SetStatus(ResStr_ParsingData);
      frmMain.SetProgress(0);
      with FDatasource do begin
        DisableControls;
        try
          First;
          // loop through the dataset once, as this is slow
          while not EOF do begin
            // parse every column required at each record
            for i := 0 to AList.Count - 1 do
              ParseField(AList[i]);
            Next;
            frmMain.SetProgress(RecNo * 100 div RecordCount);
          end;
          First;
        finally
          EnableControls;
        end;
      end;
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    finally
      DefaultCursor(lCursor);
    end; // try
  end;
end;  // TImportFile.ParseColumnList

{-------------------------------------------------------------------------------
}
function TImportFile.ParseField(const FieldName: String): Boolean;
var
  lErrorFound: Boolean;
  lValue: Variant;
  lMappedType: TColumnType;
begin
  lErrorFound := False;
  if ColumnMapping.IsMapped(FieldName) then
  begin
    lMappedType := ColumnMapping.MappedType(FieldName);
    lValue := GetValueToParse(FieldName, lMappedType);

    if Assigned(lMappedType.Parser) then
    begin
      lMappedType.ParseField(lValue, nil);
      lErrorFound := lMappedType.Parser.Failed;
    end;
    if not lErrorFound then
      // Check for a field level dependency
      lErrorFound := ColumnMapping.ValidateFieldDependency(lMappedType, FDatasource) <> '';
    DoParseErrorChanged(Self, FieldName, lErrorFound);
  end;
  Result := not lErrorFound;
end;  // TImportFile.ParseField

{-------------------------------------------------------------------------------
  Fills the data in the #Master table.
}
procedure TImportFile.PopulateMasterTable;
var
  lMasterColumns: string;
begin
  lMasterColumns := GetMasterColumns;
  frmMain.SetStatus(ResStr_PreparingData);
  try
    with FDatasource do begin
      if not (EOF and BOF) then First;
      while not EOF do begin
        dmDatabase.ExecuteSQL('INSERT INTO #Master (' + lMasterColumns + ') ' +
                              'VALUES (' + GetMasterValues + ')');
        Next;
        frmMain.SetProgress(RecNo * 50 div RecordCount);
      end; // while
    end;
  finally
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;  // TImportFile.PopulateMasterTable

{-------------------------------------------------------------------------------
  Populates the output tables for column types that are parsed into more than
  one record.
}
procedure TImportFile.PopulateMultiRecordTables;
var
  i: Integer;
  lColType: TColumnType;
  lValue: String;
begin
  try
    if not (FDatasource.EOF and FDatasource.BOF) then FDatasource.First;
    while not FDatasource.EOF do begin
      for i := 0 to FMultiRecordTableTypes.Count - 1 do begin
        lColType := TColumnType(FMultiRecordTableTypes[i]);
        if ColumnMapping.IsMapped(lColType) then begin
          // Make sure we get the actual value, not a trimmed down version of it!!!!
          lValue := FLargeFields.Values[
              ColumnMapping.MappedColumn(lColType) + ',' +
              FDatasource.FieldByName(FLD_ROWID).AsString];
          if lValue = '' then
            lValue := FDataSource.FieldByName(ColumnMapping.MappedColumn(lColType)).AsString;

          lColType.AddParsedRecords(
              lColType.Parser,
              FDatasource.FieldByName(FLD_ROWID).AsInteger,
              lValue);
        end;
      end;
      FDatasource.Next;
      // second half of progress bar
      frmMain.SetProgress(FDatasource.RecNo * 50 div FDatasource.RecordCount + 50);
    end; // while
  finally
    frmMain.SetProgress(0);
    frmMain.SetStatus('');
  end;
end;  // TImportFile.PopulateMultiRecordTables

{-------------------------------------------------------------------------------
  Populates the tables containing the checksums
}
procedure TImportFile.PopulateChecksumTables;
var
  i: Integer; 
  lColType: TColumnType;
  lRecNo: Integer;
begin
  try
    if not (FDatasource.EOF and FDatasource.BOF) then FDatasource.First;
    while not FDatasource.EOF do begin 
      for i := 0 to FMultiRecordTableTypes.Count - 1 do begin
        lColType := TColumnType(FMultiRecordTableTypes[i]);
        lRecNo := FDatasource.FieldByName(FLD_ROWID).AsInteger;
        dmDatabase.ExecuteSQL('INSERT INTO #CS_' + lColType.Key
            + ' (Record_No, Checksum) VALUES (' + IntToStr(lRecNo)
            + ', ' + GroupChecksum(lColType, lRecNo) + ')');
      end;
      FDatasource.Next;
    end;
  finally
    frmMain.SetProgress(0);
    frmMain.SetStatus('');
  end;
end;

{-------------------------------------------------------------------------------
  Hash of the individual values in the given column of the
  specified import record.
}
function TImportFile.GroupChecksum(ColumnType: TColumnType; RecordNo: Integer):
    String;
var
  I: Integer;
  lField: TParsedField;
  lFields: String;
  lTables: String;
  lSql: String;
  lAliases: Integer;
  lValue: Variant;
  
  function NextAlias: String;
  begin
    Result := 't' + IntToStr(lAliases);
  end;
  
begin
  lFields := '';
  lTables := '#CT_' + ColumnType.Key + ' AS ct';

  for I := 0 to ColumnType.Parser.FieldCount - 1 do
  begin
    if I > 0 then lFields := lFields + ', ';
    lField := ColumnType.Parser.Fields[I];
    if ColumnType.FieldMatchRuleKeys[I] = '' then
      lFields := lFields + lField.Name
    else
    begin
      lFields := lFields + NextAlias + '.Match_Key + ' + NextAlias + '.Match_Value';;
      lTables := lTables + ' LEFT JOIN #'
          + MatchRuleByKey(ColumnType.FieldMatchRuleKeys[I]).Name
          + ' AS ' + NextAlias
          + ' ON ' + NextAlias + '.Import_Value = ct.' + lField.Name;
      Inc(lAliases);
    end;
  end;
  lSql := 'SELECT CHECKSUM_AGG(CHECKSUM(' + lFields + '))'
      + ' FROM ' + lTables
      + ' WHERE ct.Record_No = ' + IntToStr(RecordNo)
      + ' AND ct.IW_Qualification = ''' + ColumnType.Qualification + '''';

  lValue := dmDatabase.ExecuteSql(lSql, True).Fields[0].Value;
  if lValue = Null then
    Result := 'null'
  else
    Result := lValue;
end;  // TImportFile.GroupChecksum

{-------------------------------------------------------------------------------
  Prepares the tables that hold the import data, and populate them.
}
procedure TImportFile.PrepareData;
begin
  CreateImportTables;
  LoadData;
end;  // TImportFile.PrepareData

{-------------------------------------------------------------------------------
  Callback from the parser, records the values in FLastParsedValues.
}
procedure TImportFile.RecordParsedValues(Sender: TObject; Position: Integer; const Field:
    string; const Value: Variant);
var
  lDecSeparator: char;
begin
  // ensure floats convert with decimal point, as they are being sent to SQL Server
  lDecSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    FLastParsedValues.Add(Value);
  finally
    DecimalSeparator := lDecSeparator;
  end; // try
end;  // TImportFile.RecordParsedValues

{-------------------------------------------------------------------------------
  Return true if a data type is for a field that requires quote delimiters in SQL.
}
function TImportFile.RequiresQuotes(AType: string): Boolean;
begin
  Result := (Pos('char', AType) > 0) or
            (Pos('text', AType) > 0) or
            (Pos('date', AType) > 0);
end;  // TImportFile.RequiresQuotes

{-------------------------------------------------------------------------------
}
procedure TImportFile.TableRuleProgress(const Progress: Integer;
    ProcessMessages: Boolean);
begin
  UpdateProgress((100 * FTableRuleIndex + Progress) div FTableRuleCount);
end;  // TImportFile.TableRuleProgress

{-------------------------------------------------------------------------------
}
procedure TImportFile.UpdateStatus(const Status: String);
begin
  if Assigned(OnStatusChanged) then OnStatusChanged(Status, True);
end;  // TImportFile.UpdateStatus

{-------------------------------------------------------------------------------
}
procedure TImportFile.UpdateProgress(Progress: Integer);
begin
  if Assigned(OnProgress) then OnProgress(Progress, True);
end;  // TImportFile.UpdateProgress


initialization
  RegisterClasses([TColumnType, TTaxonOccurrenceDataColumnType, TLocationInfoColumnType]);

end.
