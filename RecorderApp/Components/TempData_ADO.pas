//==============================================================================
//  Unit:        TempData_ADO
//
//  Implements:  TTempData
//               TMessage
//               TTermDetail
//
//  Description: Builds a temporary database for the import data, using an
//               already instantiated XMLDoc hierarchy.  This is not a datamodule,
//               but it is 'data-aware'.
//
//               TMessage
//               A TKeyList class which can store invalid records and the
//               associated message.
//
//               TTermDetail
//               Class to hold 1 term's definition. Basically just separates out
//               a term item tag into its constituent tags/data for easy access.
//               Tags are used for term, longName and definition since they
//               could potentially contain CDATA.
//
//  Author:      John van Breda
//  Created:     21 June 1999
//
//  Changes:     Eric Salmon - 04/04/2002
//               Adapted for ADO
//
//  Last Revision Details:
//    $Revision: 27 $
//    $Date: 26/05/10 17:17 $
//    $Author: Andrewkemp $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit TempData_ADO;

interface

uses
  Windows, Sysutils, Classes, JNCCXMLDoc, JNCCDatasets, MarkupDocs, Forms, Db,
  ExceptionForm, Dialogs, DataClasses, Relationships_ADO, TaskProgress, XMLData,
  GeneralData, Constants, ApiUtils, ComObj, GenFuncs, SpatialRefFuncs, XMLTypes,
  DBMerger, ADODB, DatabaseAccessADO, TablePriorityList_ADO, DatabaseUtilities;

resourcestring
  ResStr_UnexpectedTable =
      'Unexpected table ''%s'' in import file. Import aborted.';

type
  ETempDataError = class(TExceptionPath);

  EQualifierFreeTerm = class(TExceptionPath);

  TGetBoolean = function: Boolean of object;

  { a TKeyList class which can store invalid records and the message }
  TMessage = class(TExtendedInfo)
    Msg: String;
  end;

  TInvalidDataList = class(TExtendedKeyList);

  { Class to hold 1 term's definition. Basically just separates out a term item
      tag into its constituent tags/data for easy access.  Tags are used for
      term, longName and definition since they could potentially contain CDATA }
  TTermDetail = class
  private
    FTermKey: TKeyString;
    FTerm: TTag;
    FLongName: TTag;
    FDefinition: TTag;
    FEnteredBy: TKeyString;
    FEntryDate: TDateTime;
    FChangedBy: TKeyString;
    FChangedDate: TDateTime;
    FSystemSuppliedData: Boolean;
    FLocalKey: Boolean;
  public
    constructor Create(iTermTag: TTag);
    property TermKey: TKeyString read FTermKey;
    property Term: TTag read FTerm;
    property LongName: TTag read FLongName;
    property Definition: TTag read FDefinition;
    property EnteredBy: TKeyString read FEnteredBy;
    property EntryDate: TDateTime read FEntryDate;
    property ChangedBy: TKeyString read FChangedBy;
    property ChangedDate: TDateTime read FChangedDate;
    property SystemSuppliedData: Boolean read FSystemSuppliedData;
    property LocalKey: Boolean read FLocalKey;
  end;  // TTermDetail

  TTempData = class
  private
    FXMLDoc: TXMLDoc;
    FTablesCreated: TStringList;
    FLinkedTablesCreated: TStringList;
    FDatabasePath : String;
    FLocalKeys: TStringlist; // name value pairs of tableName + local key to reassign
    FCreatedKeys: TStringList;// name value pairs to keep track of keys we create as we process doc
    FProgressBar: TTaskProgressBar;
    FSetStatus  : TSetStatusEvent;  // Used to pass method reference on to DBMerger.
    FTagsProcessed: Integer;
    FTotalRecords: Integer;
    FInvalidDataList: TStringlist;
    FInvalidTagList: TList;
    FOldDateFormat: String;
    FOldTimeFormat: String;
    FOpenTablePool: TStringList; // now cache all tables as still open
    FDatabase: TADOConnection;
    FGetCancelled: TGetBoolean;
    FRebuildTaxonIndex: Boolean;
    procedure HandleQualifier(const iQualifier: String; iTable: TJNCCTable);
    function GetTypeKey(iQuery: TJNCCQuery; const iLinkTable, iKey: String): String;
    procedure SetProgress(const iProgress: Integer; iPrcMsgs: Boolean = true);
  protected
    procedure CheckAndSetTableSubtypeFlag(iStartTable: TJNCCTable; iTag: TTag);
    procedure CheckForMissingEventLink(iTag: TTag; iTable: TJNCCTable);
    procedure CheckLocationKey(const iTable, iLastTable: TJNCCTable);
    procedure CheckMeasurementIntegrity(iTag: TTag; iTable: TJNCCTable);
    function CheckPrimaryKeyJoin(iRelationships: TRelationshipArray;
      iMasterTable, iDetailTable: String): String;
    procedure CheckTableNames;
    function CheckTagForKeyValue(iTag: TTag; iKeyField: String;
       iFieldArray: TStringArray): TKeyString;
    procedure CleanupTable(const iTable, iLastTable: TJNCCTable);
    function CompareWithArray(iText: String; iArray: TStringArray): Integer;
    function FindSeparator(const iString: String): Char; // count so we can set progress bar
    function GetFieldName(iTag: TTag; iTable: TJNCCTable; iDefaultFieldName: String): String;
    function GetNewTable(const iName: String; iTag: TTag): TJNCCTable;
    function GetNextKey(const iTableName: String): String;

    procedure HandleDateInput(iTag: TTag; iTable: TJNCCTable);
    function HandleFreeTerm(iTag: TTag; iTable: TJNCCTable): Boolean;
    procedure InitLists;
    procedure LocateAndEditTerm(iTermTable: TJNCCTable; iKey: TKeyString);
    procedure PopulateBlankForeignKeys(const iTable, iLastTable: TJNCCTable);
    procedure PopulateBlankPrimaryKey(const iTable, iLastTable: TJNCCTable);
    procedure PopulateForeignField(iForeignTable: TJNCCTable; const iForeignfield: String;
      iMasterTable: TJNCCTable);
    procedure PopulateNewTable(iTag: TTag; iTable: TJNCCTable; const iSpecifier: String);
    procedure PopulateTableField(iTag: TTag; iTable: TJNCCTable; const iSpecifier: String);
    procedure PopulateTag(iParentTag: TTag; const iStartTable: TJNCCTable; const iSpecifier: String);
    procedure PopulateTermlists;
    procedure ReadAttributes(iTag: TTag; iTable: TJNCCTable; const iSpecifier: String);
    procedure ReassignLocalKeys;
    procedure RecordIfInvalid(iTable: TJNCCTable; iE: Exception);
    function SafeStrToDate(const iString: String): TDateTime;
    function SafeStrToTime(const iString: String): TDateTime;
    procedure SetupSQLToResetKey(iQuery: TJNCCQuery;
      const iTableName: String; const lOldKey, lNewKey: TKeyString);
    procedure StoreLocalKey(iTag: TTag; const iTableName: String);
    procedure SwitchKeyInMainTable(const iTable: String; iQuery: TJNCCQuery;
      const iOldKey, iNewKey: TKeyString);
    procedure TagToField(iTable: TJNCCTable; const iFieldName: String; iTag: TTag);
    procedure TagToUnknownField(iTable: TJNCCTable; iTag: TTag; const iSpecifier: String);
    procedure TransferAdditionalFields(iTermTag: TTag; iTable: TJNCCTable);
    procedure WriteFieldToDB(iTable: TJNCCTable; const iFieldName, iText: String);
    procedure WriteTermToDB(iTerm: TTermDetail; iTable: TJNCCTable;
      const iAllSystemSupplied, iPrimaryKey: String);
    { Manage a pool of table objects ready for use }
    function GetTableObject(const iTableName: String): TJNCCTable;
    function NewTableObject(const iTableName: String): TJNCCTable;
    procedure RebuildPrimaryKeys;
    procedure ReleaseTableObject(iTable: TJNCCTable);
  public
    constructor Create(iXMLDoc: TXMLDoc; iProgressBar: TTaskProgressBar;
      ASetStatus: TSetStatusEvent; iGetCancelled: TGetBoolean);
    destructor Destroy; override;
    procedure ConnectTo(const iDBPath: String);
    function CopyRecordsIntoMainDB(iDuplicateList, iExclusionList: TStringList): Integer; overload;
    function CopyRecordsIntoMainDB(iDuplicateList, iExclusionList: TStringList;
      var AImportRejectsFile: TFileName): Integer; overload;
    procedure FinaliseProcessing;
    procedure ProcessDocumentChunk(iStartTag: TTag);
    property TablesCreated: TStringList read FTablesCreated;
    property DatabasePath: String read FDatabasePath;
    property Database: TADOConnection read FDatabase;
    property InvalidDataList: TStringList read FInvalidDataList;
    property InvalidTagList: TList read FInvalidTagList;
    property RebuildTaxonIndex: Boolean read FRebuildTaxonIndex;
  end;  // TTempData

//==============================================================================
implementation

uses
  StrUtils, ApplicationSettings, MergeDataData;

const
  FORMAT            = 'format';
  VAGUE             = 'vague';
  VAGUE_FIELD       = 'VAGUE_DATE_START';
  NORMAL_DATE_FIELD = 'DATE';
  ALPHANUMERICS     = ['a'..'z','A'..'Z','0'..'9'];
  TERM_LIST_ITEM    = 'TERMLIST_ITEM';

resourcestring
  { Error messages }
  ResStr_NoContent        = 'There is no content markup in this document - data cannot be imported.';
  ResStr_InvalidBoolean   = 'The value is not recognised for Boolean fields: ';
  ResStr_NoSeperator      = 'No valid date separator was found in the text ';
  ResStr_PrimaryKeyMissing= 'The primary key markup for the following table does not contain a valid key: ';
  ResStr_KeyStream        = 'Binary data cannot be used for a key value in markup ';
  ResStr_NotTable         = 'The table structure cannot be copied as the table is not present: ';
  ResStr_DatabaseLocked   = 'The temporary import database is locked and cannot be deleted.';
  ResStr_ExistingFreeTerm = 'Problem occurred scanning for existing free term ';
  ResStr_DateNoFormat     = 'Date tag found with no format attribute.';
  ResStr_FieldMissing     = 'The following field could not be found :';
  ResStr_TablePopulate    = 'Error occurred during population of import table ';
  ResStr_TermListPopulate = 'Error occurred during population of termlists ';
  ResStr_PostRelocate     = 'A problem occurred relocating a posted record in import table ';
  ResStr_NoTableDef       = 'No table definition could be found through DAO for ';
  ResStr_RecordMissing    = 'A record is missing in import table ';
  ResStr_InvalidTermList  = 'Invalid Termlist';
  ResStr_QualifierAndUnit = 'The qualifier and unit each specify a different measurement type';



//==============================================================================
{ TTempData }
//==============================================================================
{ Constructor for TTempData.  Initiates reading of the XML document into the
     temporary database }
constructor TTempData.Create(iXMLDoc: TXMLDoc; iProgressBar: TTaskProgressBar;
  ASetStatus: TSetStatusEvent; iGetCancelled: TGetBoolean);
begin
  FProgressBar := iProgressBar;
  FSetStatus   := ASetStatus;
  FGetCancelled:= iGetCancelled;
  { Ensure we are using standard date format }
  FOldDateFormat := ShortDateFormat;
  ShortDateFormat := DATE_FORMAT;
  FOldTimeFormat := LongTimeFormat;
  LongTimeFormat := TIME_FORMAT;
  inherited Create;
  InitLists;
  FTotalRecords := 0;
  FXMLDoc := iXMLDoc;
  { Allow for the initialisation we have just done }
  FTagsProcessed := 0;
end;  // Create

//==============================================================================
{ Connect directly to an existing import database without using an XML Doc.  To
    do this we must create the 'TablesCreated' list as this is built duing
    XML doc reading }
procedure TTempData.ConnectTo(const iDBPath: String);
  //----------------------------------------------------------------------------
  procedure CountRecordsInAccessDatabase;
  var i: Integer;
  begin
    with TADOQuery.Create(nil) do
      try
        Connection := FDatabase;
        for i := 0 to FTablesCreated.Count - 1 do begin
          SQL.Text := 'SELECT Count(*) FROM [' + FTablesCreated[i] + ']';
          Open;
          Inc(FTotalRecords, Fields[0].AsInteger);
          Close;
        end;
      finally
        Free;
      end;
  end;  // CountRecordsInAccessDatabase
  //----------------------------------------------------------------------------
begin
  FDatabasePath := iDBPath;
  if FXMLDoc <> nil then
    FDatabase := dmDatabase.CreateImportExportDatabase(iDBPath)
  else
    FDatabase := dmDatabase.ConnectToAccessDatabase(iDBPath);

  FDatabase.Open;
  FDatabase.GetTableNames(FTablesCreated, false);
  CheckTableNames;

  // When importing from XML, FTotalRecords is calculated as the XML is processed.
  // Not so from an existing Access DB, so do it here.
  CountRecordsInAccessDatabase;
end;  // ConnectTo

{+------------------------------------------------------------------------------
  Ensure that the import database contains no unexpected table names.
  Throws a non-critical exception if an offensive table is found.
}
procedure TTempData.CheckTableNames;
var
  I: Integer;
begin
  for I := 0 to FTablesCreated.Count - 1 do
    if not dmDatabase.GetStoredProcOutputParam(
        'usp_ImportWizard_CheckTableName',
        ['@table_name', FTablesCreated[I]],
        '@is_expected')
    then
      raise ETempDataError.CreateNonCritical(
          SysUtils.Format(ResStr_UnexpectedTable, [FTablesCreated[I]]));
end;  // TTempData.CheckTableNames

//==============================================================================
{ Destructor cleans up DAO stuff, and the temporary file }
destructor TTempData.Destroy;
var
  i: Integer;
begin
  FTablesCreated.Free;
  FLinkedTablesCreated.Free;
  FLocalKeys.Free;
  FCreatedKeys.Free;
  FInvalidDataList.Free;
  FOpenTablePool.Free;
  { Invalid tags are not freed as we go along, so user can browse them }
  for i := 0 to FInvalidTagList.Count-1 do
    TObject(FInvalidTagList[i]).Free;
  FInvalidTagList.Free;
  ShortDateFormat := FOldDateFormat;
  ShortTimeFormat := FOldTimeFormat;

  if Assigned(FDatabase) then begin
    if FDatabase.Connected then FDatabase.Close;
    FDatabase.Free;
  end;              
  inherited Destroy;
end;  // Destroy

//==============================================================================
{ Procedure to initiate copying of a chunk of XML into the temporary import
     database.  Start tag is either the content tag, or a survey event tag }
procedure TTempData.ProcessDocumentChunk(iStartTag: TTag);
begin
  if dmNBNXML.CheckIsTable(iStartTag.Name) then
    PopulateNewTable(iStartTag, nil, '')
  else
    PopulateTag(iStartTag, nil, '');
end;  // ProcessDocumentChunk

//==============================================================================
{ Finalise stuff when an XML doc has been converted into a database file.
     Includes handling termlists and local keys }
procedure TTempData.FinaliseProcessing;
var
  i: Integer;
  lTaskIndex: Integer;
begin
  lTaskIndex := -1;// default to remove warning
  if FLocalKeys.Count>0 then
    lTaskIndex := FProgressBar.EmbedTask(0, 30);
  if not FGetCancelled then
  begin
    PopulateTermlists;
    if FLocalKeys.Count>0 then
    begin
      lTaskIndex := FProgressBar.FinishAndEmbedNext(lTaskIndex,30,100);
      ReassignLocalKeys;
      FProgressBar.FinishTask(lTaskIndex);
    end;
  end;
  { Clean up the table pool as no longer required }
  for i := 0 to FOpenTablePool.Count-1 do
    TJNCCTable(FOpenTablePool.Objects[i]).Free;
  FOpenTablePool.Clear;
  // Clean up linked tables
  for i := 0 to FLinkedTablesCreated.Count-1 do
    dmDatabase.RemoveLinkedTable(FDatabase, 'SQLSERVER_' + FLinkedTablesCreated[i]);
  FLinkedTablesCreated.Clear;
  // Force the Access db cache to flush, otherwise validation might fail
  FDatabase.Close;
  FDatabase.Open;
end;  // FinaliseProcessing

//==============================================================================
{ Initialise all list variables - called by the constructor }
procedure TTempData.InitLists;
begin
  FTablesCreated := TStringList.Create;
  FTablesCreated.Sorted := True; // so the duplicate data display is in nice order
  FLinkedTablesCreated := TStringList.Create;
  FLinkedTablesCreated.Sorted := True;
  FLocalKeys := TStringList.Create;
  FLocalKeys.Sorted := True;
  FLocalKeys.Duplicates := dupIgnore;  // only need to process each local key once
  FCreatedKeys := TStringList.Create;
  { create a list to store invalid items }
  FInvalidDataList := TStringList.Create;
  FInvalidDataList.Sorted := true;
  FInvalidDataList.Duplicates := dupAccept;
  FInvalidTagList := TList.Create;
  FOpenTablePool := TStringList.Create;
  FOpenTablePool.Capacity := 20;
end;  // InitLists

//==============================================================================
{   Handles recursive building of the records being imported.
      The iParentTag is the tag object from which we are reading data - the
    initial call to this method starts with the content tag and recurses within.
    The iStartTable is the recordset we are adding data to - starts off nil. }
procedure TTempData.PopulateTag(iParentTag: TTag; const iStartTable: TJNCCTable;
  const iSpecifier: String);
var
  lTag: TTag;
  lTagIndex: Integer;
  lSpecifier: String;
  lQualifier: String; // qualifier processed at end since it must link to same unit/type as rest of record
begin
  lQualifier := '';
  if iParentTag.NestedTags.Count > 0 then
  begin
    { Iterate through the tags nested inside the current tag }
    for lTagIndex := 0 to iParentTag.NestedTags.Count-1 do
    begin
      lTag := TTag(iParentTag.NestedTags[lTagIndex]);
      if iStartTable <> nil then
        CheckAndSetTableSubtypeFlag(iStartTable, lTag);
      { Update progress bar }
      Inc(FTagsProcessed);
      FProgressBar.TaskPosition := FTagsProcessed * 100 div FXMLDoc.ContentTagCount;
      { Check for escape key }
      Application.ProcessMessages;
      if FGetCancelled then
        Exit;
      try
        if not lTag.IsInvalid then
        begin    // tag ok so recurse into it
          { Read the tag's specifier if present, otherwise use previous specifier }
          if lTag.HasSpecifier then
            lSpecifier := lTag.Specifier
          else
            lSpecifier := iSpecifier;
          { If a table, must set up a new table to put the tag info into }
          if dmNBNXML.CheckIsTable(lTag.Name) then
            try
              PopulateNewTable(lTag, iStartTable, lSpecifier)
            except
              on E:EQualifierFreeTerm do // catch qualifier free terms and process later
                lQualifier := E.Message; // message is the short_name of the qualifier we need
            end // try
          else if lTag.Name = 'qualifier' then
            lQualifier := lTag.Text   // handle this later
          else// use the table we are already in
          begin
            if iStartTable<>nil then
              PopulateTableField(lTag, iStartTable, lSpecifier)
            else // recurse to find first nested table tag
              PopulateTag(lTag, nil, lSpecifier);
          end;
          if lTag.Name='measurement_unit' then
            CheckMeasurementIntegrity(TTag(lTag.Parent), iStartTable);
          { Stop if cancel pressed - required due to recursion }
          if FGetCancelled then
            Exit;
        end;
      except
        on EQualifierFreeTerm do
          raise;  // catch this at higher level
        on E:Exception do begin
          lTag.InvalidateTag(e.Message);
        end;
      end;
      if lTag.isInvalid then begin
        // record tag as invalid item
        FInvalidTagList.Add(lTag);
        iStartTable.Cancel;
        Exit; // from procedure - once one tag invalid ignore the rest
      end;
    end; // For
    if lQualifier<>'' then
      HandleQualifier(lQualifier, iStartTable);
  end else
    { single field specified directly inside table tag }
    if iStartTable <> nil then // safety - could be empty content tag
      TagToUnknownField(iStartTable, iParentTag, iSpecifier);
end;  // PopulateTag

//==============================================================================
{ When a unit key is specified for measurements, checks that the unit and the
    qualifier match to the same type.  If not, then the tag is invalidated.
    If any data is missing, then the tag is accepted }
procedure TTempData.CheckMeasurementIntegrity(iTag: TTag; iTable: TJNCCTable);
var
  lQuery: TJNCCQuery;
  lQualifierType, lUnitType: String;
begin
  lQuery := TJNCCQuery.Create(nil);
  try
    lQualifierType := GetTypeKey(lQuery, 'measurement_qualifier',
                                 iTable.FieldByName('measurement_qualifier_key').AsString);
    lUnitType      := GetTypeKey(lQuery, 'measurement_unit',
                                 iTable.FieldByName('measurement_unit_key').AsString);
    if (lQualifierType <> '') and (lUnitType <> '') and (lUnitType <> lQualifierType) then
      iTag.InvalidateTag(ResStr_QualifierAndUnit);
  finally
    lQuery.Free
  end;
end;  // CheckMeasurementIntegrity

//==============================================================================
{ Qualifiers in measurements have been deprecated and replaced with measurement
     qualifier.  However, we still need to handle these by treating them as
     a measurement qualifier free term.
     This function also handles a measurement qualifier free term }
procedure TTempData.HandleQualifier(const iQualifier: String; iTable: TJNCCTable);
var
  lQuery: TJNCCQuery;
  lFoundTerm: String;
  lTypeKey: String;
begin
  lFoundTerm := '';
  lQuery := TJNCCQuery.Create(nil);
  try
    lTypeKey := GetTypeKey(lQuery, 'measurement_unit', iTable.FieldByName('measurement_unit_key').AsString);
    with lQuery do begin
      ParseSQL := false;
      Connection := FDatabase;  // the temp import database
      SQL.Text := 'SELECT Measurement_Qualifier_key '#13#10 +
          'FROM Measurement_Qualifier'#13#10 +
          'WHERE short_name=''' + iQualifier+ ''' AND Measurement_Type_Key = ''' + lTypeKey + '''';

      if FTablesCreated.IndexOf('Measurement_Qualifier')<>-1 then begin
        Open;
        if RecordCount>0 then
          lFoundTerm := FieldByName('Measurement_Qualifier_key').AsString;
        Close;
      end; // if
      if lFoundTerm='' then begin
        { No term match, so check in main database, via linked tables }
        dmDatabase.CreateLinkedTable(FDatabase, 'Measurement_Qualifier', 'SQLSERVER_Measurement_Qualifier');
        SQL[1] := 'FROM SQLSERVER_Measurement_Qualifier';
        Open;
        if RecordCount>0 then
          lFoundTerm := FieldByName('Measurement_Qualifier_key').AsString;
        Close;
        dmDatabase.RemoveLinkedTable(FDatabase, 'SQLSERVER_Measurement_Qualifier');
      end;
    end; // with lQuery
    if lFoundTerm = '' then begin
      { Need to add a new term }
      with dmGeneralData.IdGenerator do
        lFoundTerm := GetNextID(GetCurrentKey('MEASUREMENT_QUALIFIER'));
      FCreatedKeys.Add('MEASUREMENT_QUALIFIER' + '=' + lFoundTerm); // so last keys are updated after import
      { May need to add measurement qualifier table to import db }
      if FTablesCreated.IndexOf('MEASUREMENT_QUALIFIER') = -1 then
      begin
        dmDatabase.CreateImportExportTable(FDatabase, 'MEASUREMENT_QUELIFIER');
        { Remember we've got it for next time }
        FTablesCreated.Add('MEASUREMENT_QUALIFIER');
      end;
      { And now add the new record }
      lQuery.SQL.Text := 'INSERT INTO Measurement_Qualifier (Measurement_Qualifier_Key, Short_Name, ' +
                         'Measurement_Type_Key, Entered_By, System_Supplied_Data) '#13#10 +
                         'VALUES (''' + lFoundTerm + ''', ''' + iQualifier + ''', ' +
                         '''' + lTypeKey + ''', ''' + AppSettings.UserID + ''', False)';
      lQuery.ExecSQL;
    end;
    { Link term the DATA record }
    iTable.FieldByName('Measurement_Qualifier_key').AsString := lFoundTerm
  finally
    lQuery.Free;
  end;
end;  // HandleQualifier

//==============================================================================
{ Find an already created type key from the unit or qualifier key we know.
    iQuery is just an empty TQuery instance we can use }
function TTempData.GetTypeKey(iQuery: TJNCCQuery; const iLinkTable, iKey: String): String;
begin
  Result := '';
  with iQuery do begin
    Connection := FDatabase;  // the temp import database
    SQL.Text := 'SELECT Measurement_Type_Key '#13#10 +
        'FROM ' + iLinkTable + #13#10 +
        'WHERE ' + iLinkTable + '_Key =''' + iKey + '''';

    if FTablesCreated.IndexOf(iLinkTable)<>-1 then begin
      Open;
      if RecordCount>0 then
        Result := FieldByName('Measurement_Type_Key').AsString;
      Close;
    end; // if
    if Result='' then begin
      { No term match, so check in main database, via linked tables }
      if FLinkedTablesCreated.IndexOf(iLinkTable)=-1 then begin
        // create linked table if required
        dmDatabase.CreateLinkedTable(FDatabase, iLinkTable, 'SQLSERVER_' + iLinkTable);
        FLinkedTablesCreated.Add(iLinkTable);
      end;
      SQL[1] := 'FROM SQLSERVER_' + iLinkTable;  // Main DB table
      Open;
      if RecordCount>0 then
        Result := FieldByName('Measurement_Type_Key').AsString;
      Close;
    end;
  end; // with lQuery
end;  // GetTypeKey

//==============================================================================
{ If the tag is for a source_file or a reference, then sets the internal
    flag as appropriate in the start table (must be source).
    If the tag is for an organisation or an individual, then sets the
    organisation flag as appropriate in the start table (must be name) }
procedure TTempData.CheckAndSetTableSubtypeFlag(iStartTable: TJNCCTable;
  iTag: TTag);
var lstTableName: String;
begin
  lstTableName := iStartTable.TableName;
  if CompareText(iTag.Name, 'INDIVIDUAL')=0 then begin
    if CompareText(lstTableName, 'NAME')=0 then
      iStartTable.FieldByName('ORGANISATION').AsBoolean := False;
  end
  else if CompareText(iTag.Name, 'ORGANISATION')=0 then begin
    if CompareText(lstTableName, 'NAME')=0 then
      iStartTable.FieldByName('ORGANISATION').AsBoolean := True;
  end
  else if CompareText(iTag.Name, 'REFERENCE')=0 then begin
    if CompareText(lstTableName, 'SOURCE')=0 then
      iStartTable.FieldByName('INTERNAL').AsBoolean := True;
  end
  else if CompareText(iTag.Name, 'SOURCE_FILE')=0 then begin
    if CompareText(lstTableName, 'SOURCE')=0 then
      iStartTable.FieldByName('INTERNAL').AsBoolean := False;
  end;
end;  // CheckAndSetInternalSourceFlag

//==============================================================================
{ Finds the appropriate table for the tag, and makes sure we have a copy of this
    table in our temp output database.  Initiates the reading of the attributes
    and nested tags into this table.  Return value is the primary key value
    of the table record as output. }
procedure TTempData.PopulateNewTable(iTag: TTag; iTable: TJNCCTable;
  const iSpecifier: String);
var
  lNewTable: TJNCCTable;
  lNewTableName: String;
begin
  try
    if dmNBNXML.CheckIsRealTable(iTag.Name) then
      lNewTableName := iTag.Name
    else
      lNewTableName := dmNBNXML.GetTableFromSpecialElement(dmNBNXML.GetDatasetTable(iTable), iTag.Name);

    lNewTable := GetNewTable(lNewTableName, iTag);
    { Join this table to the previous 'master' table }
    PopulateBlankForeignKeys(lNewTable, iTable);
    { and the special case for survey_events handled as a separate block of data }
    CheckForMissingEventLink(iTag, lNewTable);
    try
      { Read the attribute information from the tag into the table }
      ReadAttributes(iTag, lNewTable, iSpecifier);
      { Recursive call into the tag we are processing }
      PopulateTag(iTag, lNewTable, iSpecifier);
    finally
      { and clean up the table (if not already cancelled) }
      if (lNewTable.State = dsInsert) or (lNewTable.State = dsEdit) then
        CleanupTable(lNewTable, iTable)
      else if lNewTable.State = dsBrowse then
        {Check master table has foreign key populated}
        PopulateBlankForeignKeys(iTable, lNewTable);
      ReleaseTableObject(lNewTable); // mark it as available for use
    end;// try..finally
  except // additional error info
    on EQualifierFreeTerm do
          raise;  // catch this at higher level
    on E:Exception do
      raise ETempDataError.Create(ResStr_TablePopulate + lNewTableName, E);
  end;
end;  // PopulateNewTable

//==============================================================================
{ When tidying up a survey event record, we must check that the survey key
    can be populated from the available information.  If no survey key was
    supplied in the import file, it is necessary to generate one here, insert
    it into the XML document model, and insert it into the survey event table.
    Then, when the survey XML is processed it will have the correct key to join }
procedure TTempData.CheckForMissingEventLink(iTag: TTag; iTable: TJNCCTable);
var
  lSurveyKey: TKeyString;
  lKeyTag: TTag;
begin
  if iTag.Name='survey_event' then
  begin
    if TTag(TTag(iTag.Parent).NestedTags[0]).Name = 'survey_key' then
      { Key is available, so link it up }
      iTable.FieldByName('survey_key').AsString := TTag(TTag(iTag.Parent).NestedTags[0]).Text
    else begin
      { No key available so generate one }
      lSurveyKey := GetNextKey('survey');
      iTable.FieldByName('survey_key').AsString := lSurveyKey;
      lKeyTag := TTag.CreateDummySurveyKey(iTag.Parent, iTag.Parent.Document, lSurveyKey);
      TTag(iTag.Parent).NestedTags.Insert(0, lKeyTag);
    end;
  end;
end;  // CheckForMissingEventLink

//==============================================================================
{ Puts tag information into a table we have already started to populate }
procedure TTempData.PopulateTableField(iTag: TTag; iTable: TJNCCTable; const iSpecifier: String);
begin
  if iTable <> nil then // we must have a valid table, else ignore
  begin
    if not HandleFreeTerm(iTag, iTable) then
    begin
      { Read the attribute information from the tag into the table - must
          be before we read the fields }
      ReadAttributes(iTag, iTable, iSpecifier);
      { If we have a valid field, update it }
      { First - special date handling }
      if iTag.Name = GENERIC_DATE then
        HandleDateInput(iTag, iTable)
      else
        { Put tag data into the appropriate field in the table }
        TagToUnknownField(iTable, iTag, iSpecifier);
      { Recursive call into the child we are processing }
      PopulateTag(iTag, iTable, iSpecifier);
    end; // if not handlefreeterm
  end;
end;  // PopulateTableField

//==============================================================================
{ Where we don't yet know the exact field name that a tag corresponds to, this
    procedure first finds the field, then copies the tag data into it.  The
    field name can be a direct match for the tag name, or the specifier or
    a special xml element can be used to locate it. }
procedure TTempData.TagToUnknownField(iTable: TJNCCTable; iTag: TTag; const iSpecifier: String);
var
  lSpecialElement: TSpecialElementItem;
  i: Integer;
  lFieldArray: TStringArray;
begin
  SetLength(lFieldArray, 0);
  if iTable.FieldDefs.IndexOf(iTag.Name) <> -1 then
    TagToField(iTable, iTag.Name, iTag)
  else if iTable.FieldDefs.IndexOf(iSpecifier + '_' + iTag.Name)<>-1 then
    TagToField(iTable, iSpecifier + '_' + iTag.Name, iTag)
  else
  begin // try special xml elements for field translations
    lSpecialElement := dmNBNXML.FindSpecialElement(iTag.Name, TRANSLATE_FIELD);
    if (lSpecialElement<> nil) then
    begin
      lFieldArray := dmNBNXML.ParseSemiColonString(lSpecialElement.Data);
      for i := 0 to High(lFieldArray) do
        if iTable.FieldDefs.IndexOf(lFieldArray[i]) <> -1 then
        begin
          TagToField(iTable, lFieldArray[i], iTag);
          break;// from for loop as we have matched a field
        end else if lFieldArray[i] = '*' then // indicates primary key
        begin
          TagToField(iTable, dmDatabase.GetPrimaryKey(iTable.TableName, False), iTag);
          break;// from for loop as we have matched a field
        end;
    end;
  end;
end;  // TagToUnknownField

//==============================================================================
{ When we find a free term, we need to look for an existing record in the main
     db.  If found, then we need to copy the records across and link up the
     keys correctly.  If not, we need to fill in the minimum required fields. }
function TTempData.HandleFreeTerm(iTag: TTag; iTable: TJNCCTable): Boolean;
var
  lFoundInImport: Boolean;
begin
  Result := False;
  if (iTag.Name = 'free_term') then // ignore all other tags
  begin
    if (iTag.Parent is TTag) then
      if TTag(iTag.Parent).Name = 'measurement_qualifier' then
        raise EQualifierFreeTerm.Create(iTag.Text);  // handle it in the data table, not qualifier table
    try
      Result := True;
      { Free term not in main DB, but is it one we just imported? }
      with dmNBNXML.qryFindExistingFreeTerm do
      begin
        { Reuse this query, but on the temp import DB. Find a matching term already imported }
        Connection := FDatabase;
        try
          lFoundInImport := dmNBNXML.ExistingFreeTerm(iTable.TableName,
                                                      iTag.Text, iTable, True);
        finally
          dmDatabase.SetDatabaseLocal([dmNBNXML.qryFindExistingFreeTerm]);
        end; // try..finally
      end;  // with lqryImportedTerm

      if not lFoundInImport then // finally try the main DB
        if not dmNBNXML.ExistingFreeTerm(iTable.TableName, iTag.Text, iTable, False) then
        begin
          iTable.FieldbyName('short_name').AsString := iTag.Text;
          iTable.FieldbyName('entered_by').AsString := Appsettings.UserID;
          iTable.FieldbyName('entry_date').AsDateTime := Now;
        end;
    except
      on E:Exception do
        raise ETempDataError.Create(ResStr_ExistingFreeTerm + iTag.Text, E);
    end;
  end;
end;  // HandleFreeTerm

//==============================================================================
{ If we already have a table of this name in our temp database, then a table
    object instance is created, linked to it and returned.  Otherwise, a new
    table definition is added via DAO, and the object instance returned.  The
    resulting table is opened and an empty record appended, or an
    existing record edited depending on the presence of the record indicated
    by the primary key in the input Tag. If the table is appended, then a dummy
    record is created in a 'like' table in the main db, and the defaults are
    copied from there. }
function TTempData.GetNewTable(const iName: String; iTag: TTag): TJNCCTable;
begin
  if FTablesCreated.IndexOf(iName) = -1 then
  begin
    dmDatabase.CreateImportExportTable(FDatabase, iName);
    { Remember we've got it for next time }
    FTablesCreated.Add(iName);
  end;
  { Set up the result table }
  Result := GetTableObject(iName);
  Result.Append;
end;  // GetNewTable

//==============================================================================
{ Looks for a key field name in the supplied tag.  This is either a fieldname
     match with the tag name, or a match indicated by the special xml element -
     the possible field values for the special xml element are broken down into
     the iFieldArray parameter. }
function TTempData.CheckTagForKeyValue(iTag: TTag; iKeyField: String;
         iFieldArray: TStringArray): TKeyString;
var
  i: Integer;
begin
  Result := ''; // default - keyvalue not found
  if CompareText(iTag.Name, iKeyField)=0 then
    Result := iTag.Text
  else
  begin // check for a special element conversion
    for i := 0 to High(iFieldArray) do
    begin
      { If fieldname required matches one of the translation fields, or
          translation indicates tag always represents primary key }
      if (CompareText(iFieldArray[i], iKeyField)=0) or (iFieldArray[i]='*') then
      begin
        Result := iTag.Text;
        break; // from loop
      end;
    end; // for
  end;
end;  // CheckTagForKeyValue

//==============================================================================
{ Reads information (String or stream) from a tag instance into the field
    specified in the dataset }
procedure TTempData.TagToField(iTable: TJNCCTable; const iFieldName: String;
  iTag: TTag);
var
  lCDATAStrings: TStringList;
  lBlobStream : TADOBlobStream;
begin
  { First, If a CData section and a BLOB field }
  if (iTag.Stream <> nil) and (iTable.FieldByName(iFieldName).IsBlob) then
  begin
    { Assign data from a stream }
    if tiLocalKey in iTag.TagInfo then
      raise ETempDataError.Create(ResStr_KeyStream + iTag.Name);

    lBlobStream := TADOBlobStream.Create(TBlobField(iTable.FieldByName(iFieldName)), bmWrite);
    iTag.Stream.Position := 0;
    lBlobStream.CopyFrom(iTag.Stream, iTag.Stream.Size);
    lBlobStream.Free;
  end
  else if iTag.Stream<>nil then // a CDATA section but not a blob field
  begin
    iTag.Stream.Position := 0;
    { Need to read stream into a String list, then read the text into the DB }
    lCDATAStrings := TStringList.Create;
    try
      lCDATAStrings.LoadFromStream(iTag.Stream);
      WriteFieldToDB(iTable, iFieldName, lCDATAStrings.Text);
    finally
      lCDATAStrings.Free;
    end; // try..finally
  end else // normal tag
  begin
    { If a local key, then we must remember it so we can reassign to an nbn key
      later.  Don't store the foreign key references to local keys though. }
    if (tiLocalKey in iTag.TagInfo) and
       (CompareText(iFieldName, dmDatabase.GetPrimaryKey(iTable.TableName, True)) = 0) then
      StoreLocalKey(iTag, iTable.TableName);
    WriteFieldToDB(iTable, iFieldName, iTag.Text);
  end;
end;  // TagToField

//==============================================================================
{ Utility to post a table.  Has some safety checks built in to ensure keys are
     correct.   }
procedure TTempData.CleanupTable(const iTable, iLastTable: TJNCCTable);
var
  lState: TDataSetState;
begin
  PopulateBlankPrimaryKey(iTable, iLastTable);
  if iTable <> nil then
  begin
    if CompareText(iTable.TableName, 'SAMPLE')=0 then
      CheckLocationKey(iTable, iLastTable);
    try
      { Don't record term references in the main content section as invalid -
            the real term data comes later. Also, no identification records as
            these aren't imported }
      if dmNBNXML.IsTermTable(iTable) then
      begin
        if iTable.FieldByName(TERM_SHORT).IsNull then
          iTable.Cancel;
      end else
        if dmNBNXML.IsIdentification(iTable) then
          iTable.Cancel;
      lState := iTable.State;
      if (lState=dsEdit) or (lState=dsInsert) then
      begin
        iTable.Post;
        Inc(FTotalRecords);
      end;
    except on E:Exception do
      begin
        RecordIfInvalid(iTable, E);
        iTable.Cancel;
      end;
    end;
  end;
end;  // CleanupTable

//==============================================================================
procedure TTempData.RecordIfInvalid(iTable: TJNCCTable; iE: Exception);
var
  lPrimaryKey: TPrimaryKey;
  lInvalidText: TMessage;
  lstTableName: String;
begin
  lstTableName := iTable.TableName;
  lPrimaryKey := dmDatabase.SplitPrimaryKey(lstTableName);
  lInvalidText := TMessage.Create;
  lInvalidText.Msg := iE.Message;
  { If we have a key }
  if iTable.FieldByName(lPrimaryKey.Key1).AsString<>'' then
  begin
    if FInvalidDataList.IndexOf(lstTableName + '=' +
                                iTable.FieldByName(lPrimaryKey.Key1).AsString) = -1 then
      FInvalidDataList.AddObject(lstTableName + '=' +
                                 iTable.FieldByName(lPrimaryKey.Key1).AsString, lInvalidText);
  end else
    FInvalidDataList.AddObject(lstTableName + '=Unknown', lInvalidText);
end;  // RecordIfInvalid

//==============================================================================
{ If a sample is imported with no location key, try and use the location key for
    the event to populate the data.  This facilitates reporting later on.  For
    this to work, iLastTable must be survey_event }
procedure TTempData.CheckLocationKey(const iTable, iLastTable: TJNCCTable);
const
  LOC_KEY = 'LOCATION_KEY';
begin
  if CompareText(iLastTable.TableName, 'SURVEY_EVENT')=0 then
  begin
    { Test we have valid fields }
    if (iTable.FieldList.IndexOf(LOC_KEY)<>-1) and
       (iLastTable.FieldList.IndexOf(LOC_KEY)<>-1) then
      { Test we have a missing location key, and we have one to copy over }
      if iTable.FieldByName(LOC_KEY).IsNull and
         (not iLastTable.FieldByName(LOC_KEY).IsNull) then
        { Copy it over }
        iTable.FieldByName(LOC_KEY).AsString := iLastTable.FieldByName(LOC_KEY).AsString;
  end;
end;  // CheckLocationKey

//==============================================================================
{ PopulateBlankForeignKeys - checks if the primary key of a parent 'master'
     table matches one of the fields in this table, and populates the
     field to create the relationship as appropriate. }
procedure TTempData.PopulateBlankForeignKeys(const iTable, iLastTable: TJNCCTable);
var
  lRelationships: TRelationshipArray;
  lJoinField: String;
begin
  SetLength(lRelationships, 0); // remove compiler warning
  if (iTable<>nil) and (iLastTable<>nil) then
  begin
    { First check if related to the table which the previous primary key was
       reported for }
    lRelationships := dmDatabase.Relationships.FindRelationsBetweenTables(
                          iLastTable.TableName, iTable.TableName);
    { If there is a single relationship, we can use that relationship to populate
         any missing foreign key fields in the current record.  Therefore, nested
         elements don't need to re-specify their parent's key }
    if High(lRelationships) = 0 then
    begin
      lJoinField := lRelationships[0].Fields[0].DetailName;
      PopulateForeignField(iTable, lJoinField, iLastTable);
    end else //No master-detail, so try other way round for one-one relationships
    begin
      lRelationships := dmDatabase.Relationships.FindRelationsBetweenTables(
                            iTable.TableName, iLastTable.TableName);
      if High(lRelationships) >= 0 then
      begin
        { Check - there must be a relationship between the current table's
            primary key and the detail tables primary keys }
        lJoinField := CheckPrimaryKeyJoin(lRelationships, iTable.TableName, iLastTable.TableName);
        if lJoinField <> '' then
          PopulateForeignField(iTable, lJoinField, iLastTable);
      end;
    end;
  end; { tables not nil }
end;  // PopulateBlankForeignKeys

//==============================================================================
procedure TTempData.PopulateForeignField(iForeignTable: TJNCCTable;
  const iForeignfield: String; iMasterTable: TJNCCTable);
var
  lMasterField: String;
begin
  if iForeignTable.FieldByName(iForeignfield).AsString = '' then
  begin
    lMasterField := dmDatabase.GetPrimaryKey(iMasterTable.TableName, False);
    { First check we have a primary key to join to }
    if iMasterTable.FieldByName(lMasterField).Text = '' then
      iMasterTable.FieldByName(lMasterField).AsString := GetNextKey(iMasterTable.TableName);
    { Then join the foreign key to this key }
    iForeignTable.FieldByName(iForeignfield).AsString := iMasterTable.FieldByName(lMasterField).Text;
  end;
end;  // PopulateForeignField

//==============================================================================
{ If a record comes in which does not specify a primary key, then we must build
    one ourselves }
procedure TTempData.PopulateBlankPrimaryKey(const iTable, iLastTable: TJNCCTable);
var
  lRelationships: TRelationshipArray;
  lstFieldName : String;
begin
  SetLength(lRelationships, 0);
  if (iTable<>nil) then // safety
  begin
    { If no primary key in the newly imported record, build a new one (they are
      often optional in the DTD).  If we are looking at a join table (a multi-
      field key) then the key should not be built in this way. }
    try
      lstFieldName := dmDatabase.GetPrimaryKey(iTable.TableName, False);
      if iTable.FieldByName(lstFieldName).AsString='' then
        iTable.FieldByName(lstFieldName).AsString := GetNextKey(iTable.TableName);
    except
      on EMultiFieldKey do ; // nothing - a join table
    end; // try..except
    { Now write the primary key value back into any related fields in our start
        table, if necessary }
    if iLastTable<>nil then
    begin
      lRelationships := dmDatabase.Relationships.FindRelationsBetweenTables(
                            iTable.TableName, iLastTable.TableName);
      // MAY NEED TO COPE WITH 1-1 joins, multiple relationships
      if (High(lRelationships)>-1) and
         (iLastTable.FieldByName(lRelationships[0].Fields[0].DetailName).AsString = '') then
        iLastTable.FieldByName(lRelationships[0].Fields[0].DetailName).AsString :=
           iTable.FieldByName(lRelationships[0].Fields[0].MasterName).AsString;
    end; // if iLastTable<>nil
  end; // if iTable<>nil
end;  // PopulateBlankPrimaryKey

//==============================================================================
{ Reads the attribute information into the table from a tag }
procedure TTempData.ReadAttributes(iTag: TTag; iTable: TJNCCTable; const iSpecifier: String);
var
  i: Integer;
  lName: String;
  lSpecialElement: TSpecialElementItem;
begin
  for i := 0 to iTag.AttributeData.Count-1 do
  begin
    lName := iTag.AttributeData.Names[i]; // the potential field name
    { Check if the attribute exists - first try real name, then specifier + name,
        then any special xml elements }
    if iTable.FieldDefs.IndexOf(lName) <> -1 then
      WriteFieldToDB(iTable, lName, iTag.AttributeData.Values[lName])
    else if iTable.FieldDefs.IndexOf(iSpecifier+'_'+lName) <> -1 then
      WriteFieldToDB(iTable, iSpecifier+'_'+lName, iTag.AttributeData.Values[lName])
    else
    begin
      lSpecialElement := dmNBNXML.FindSpecialElement(lName, TRANSLATE_FIELD);
      if (lSpecialElement<> nil) then
        if iTable.FieldDefs.IndexOf(lSpecialElement.Data) <> -1 then
          WriteFieldToDB(iTable, lSpecialElement.Data, iTag.AttributeData.Values[lName]);
    end; // if
  end; // for i
end;  // ReadAttributes

//==============================================================================
{ Writes a single String into a DB field.  Allows stuff like yes/no fields to be
     written into booleans etc. }
procedure TTempData.WriteFieldToDB(iTable: TJNCCTable; const iFieldName,
  iText: String);
var
  lOrigRefType: String;
begin
  case iTable.FieldByName(iFieldName).DataType of
    ftBoolean :
        if Uppercase(iText) = 'YES' then
          iTable.FieldByName(iFieldName).AsBoolean := True
        else if Uppercase(iText) = 'NO' then
          iTable.FieldByName(iFieldName).AsBoolean := False
        else
          raise ETempDataError.Create(ResStr_InvalidBoolean + iText);
    ftDateTime :
        begin
          if Pos(Uppercase(TIME), Uppercase(iFieldName))=0 then // not time
            iTable.FieldByName(iFieldName).AsDateTime := SafeStrToDate(iText)
          else // time field
            iTable.FieldByName(iFieldName).AsDateTime := SafeStrToTime(iText);
        end;
    ftMemo :
        iTable.FieldByName(iFieldName).AsString := iText;
    else
        if not AnsiEndsText('SPATIAL_REF', iFieldName) then
          iTable.FieldByName(iFieldName).Text := iText
        else begin  // make sure we try the XML's specified spatial reference type
          lOrigRefType := SpatialRefFuncs.GetCurrentSpatialRefSystem;
          SpatialRefFuncs.SetCurrentSpatialRefSystem(
                       iTable.FieldByName('SPATIAL_REF_SYSTEM').AsString); // temporary change
          iTable.FieldByName(iFieldName).AsString := iText;
          SpatialRefFuncs.SetCurrentSpatialRefSystem(lOrigRefType);
        end;
  end; // case
end;  // WriteFieldToDB

//==============================================================================
{ Basically a String to date function, but will work with any separator, as long
     as it is consistent }
function TTempData.SafeStrToDate(const iString: String): TDateTime;
var
  lOldSeparator: char;
begin
  lOldSeparator := DateSeparator; // remember current settings
  DateSeparator := FindSeparator(iString);
  Result := StrToDate(iString); // convert
  DateSeparator := lOldSeparator; // restore original settings
end;  // SafeStrToDate

//==============================================================================
{ A String to time function that will work with any separator }
function TTempData.SafeStrToTime(const iString: String): TDateTime;
var
  lOldSeparator: char;
begin
  lOldSeparator := TimeSeparator; // remember current settings
  TimeSeparator := FindSeparator(iString);
  Result := StrToTime(iString); // convert
  TimeSeparator := lOldSeparator; // restore original settings
end;  // SafeStrToTime

//==============================================================================
{ Locate the separator used for a date or time String }
function TTempData.FindSeparator(const iString: String): Char;
var
  i: Integer;
begin
  { First find the first non-alphanumeric character }
  Result := #0; // default - not found
  i:=1;
  while i <= Length(iString) do
  begin
    case iString[i] of
      '0'..'9', 'a'..'z', 'A'..'Z', ' ':  // not a separator
        Inc(i);
    else
        Result := iString[i];
        break; // from while loop
    end; //case
  end;
  if Result = #0 then
    raise ETempDataError.Create(ResStr_NoSeperator + iString);
end;  // FindSeparator

//==============================================================================
{ Checks if there is a join in the relationship array between the primary
     keys of the two tables.  Note - the joins we are looking at come from the
     main DB, not the import database.  If a join is found, then the master
     field name is returned.  Note, we have to allow for multiple field keys-
     as long as one of the fields is joined we are OK. }
function TTempData.CheckPrimaryKeyJoin(iRelationships: TRelationshipArray;
  iMasterTable, iDetailTable: String): String;
var
  lMasterKey, lDetailKey: String; // field names
  i: Integer;
  lMasterFields, lDetailFields: TStringArray;
  lIndex: Integer;
begin
  lMasterKey := dmDatabase.GetPrimaryKey(iMasterTable, True);
  lDetailKey := dmDatabase.GetPrimaryKey(iDetailTable, True);
  lMasterFields := dmNBNXML.ParseSemiColonString(lMasterKey);
  lDetailFields := dmNBNXML.ParseSemiColonString(lDetailKey);
  Result := ''; // default - no valid join
  for i := 0 to High(iRelationships) do  // check all relationships in array
  begin
    lIndex := CompareWithArray(iRelationships[i].Fields[0].MasterName, lMasterFields);
    if (lIndex <> -1) and
       (CompareWithArray(iRelationships[i].Fields[0].DetailName, lDetailFields) <> -1) then
    begin
      Result := lMasterFields[lIndex];
      break;  // from loop as we have  found it
    end;
  end;
end;  // CheckPrimaryKeyJoin

//==============================================================================
{ CompareWithArray - a utility function to return True if a String exists in a
     String array.  Returns the array index, or -1 if not found }
function TTempData.CompareWithArray(iText: String; iArray: TStringArray): Integer;
var
  i: Integer;
begin
  Result := -1; // default
  for i := 0 to High(iArray) do
    if iText = iArray[i] then
    begin
      Result := i;
      Break; // from for loop - found a match
    end;
end;  // CompareWithArray

//==============================================================================
{ StoreLocalKey keeps a list of all local keys which have been found in the
     document, so that we can reassign the key values later }
procedure TTempData.StoreLocalKey(iTag: TTag; const iTableName: String);
begin
  {Add tablename=key value to list}
  FLocalKeys.Add(iTableName + '=' + iTag.Text);
end;  // StoreLocalKey

//==============================================================================
{ Locates the next key value for a record to be added to a table.  If the key
    value is for a table we have already added data into the temporary import
    DB, then we must ensure we don't reuse the same key value }
function TTempData.GetNextKey(const iTableName: String): String;
var
  lPrevCreatedKeyIndex: Integer;
begin
  { Need a new key - check if we have already assigned one to this table }
  lPrevCreatedKeyIndex := FCreatedKeys.IndexOfName(iTableName);
  { Read the next key value accordingly and keep our own list of last key
      values so we don't get the same value again next time }
  if lPrevCreatedKeyIndex = -1 then // can use main DB to build new key
  begin
    with dmGeneralData.IdGenerator do
      Result := GetNextID(GetCurrentKey(iTableName));
    FCreatedKeys.Add(iTableName + '=' + Result);
  end
  else // use our own last created key for the table
  begin
    Result := dmGeneralData.IdGenerator.GetNextID(FCreatedKeys.Values[iTableName]);
    FCreatedKeys.Values[iTableName] := Result;
  end;
end;  // GetNextKey

//==============================================================================
{ Manage the input of a generic Date style tag }
procedure TTempData.HandleDateInput(iTag: TTag; iTable: TJNCCTable);
var
  lFormat: String;
  lFieldName: String;
  lDate: TDateTime;
begin
  { first, find the format tag }
  if iTag.AttributeData.IndexOfName('format') = -1 then
    raise ETempDataError.Create(ResStr_DateNoFormat);
  lFormat := iTag.AttributeData.Values['format'];
  { If vague date, find the vague_date_start field }
  if CompareText(lFormat, VAGUE)=0 then
  begin
    lFieldName := GetFieldName(iTag, iTable, VAGUE_FIELD);
    iTable.FieldByName(lFieldName).Text := iTag.Text; // ok as vague dates always standard format
  end
  else   { else the date field }
  begin
    lFieldName := GetFieldName(iTag, iTable, NORMAL_DATE_FIELD);
    { temporarily use specified date format }
    ShortDateFormat := lFormat;
    lDate := SafeStrToDate(iTag.Text);
    iTable.FieldByName(lFieldName).AsDateTime := lDate;
    ShortDateFormat := DATE_FORMAT;
  end;
end;  // HandleDateInput

//==============================================================================
{ Return the field name for a date tag in a table.  Uses the specifier for the
     tag if necessary. }
function TTempData.GetFieldName(iTag: TTag; iTable: TJNCCTable;
         iDefaultFieldName: String): String;
begin
  if (iTag.HasSpecifier) and (iTag.Specifier<>'') then
  begin
    Result := iTag.Specifier + '_' + iDefaultFieldName;
    if iTable.FieldList.IndexOf(Result)=-1 then
      if iTable.FieldList.IndexOf(iTag.Specifier)<>-1 then
        Result := iTag.Specifier;
  end
  else
    Result := iDefaultFieldName;
  if iTable.FieldList.IndexOf(Result)=-1 then
    raise ETempDataError.Create(ResStr_FieldMissing + Result);
end;  // GetFileName

//==============================================================================
{ Copys all records into the main db, unless they are one of the exclusions,
     Returns the number of records copied }
function TTempData.CopyRecordsIntoMainDB(iDuplicateList, iExclusionList: TStringList): Integer;
var lDummyFile: TFileName;
begin
  Result := CopyRecordsIntoMainDB(iDuplicateList, iExclusionList, lDummyFile);
end;

//==============================================================================
{ Copys all records into the main db, unless they are one of the exclusions,
     Returns the number of records copied, and return details of the file
     containing the list of rejection errors}
function TTempData.CopyRecordsIntoMainDB(iDuplicateList, iExclusionList: TStringList;
  var AImportRejectsFile: TFileName): Integer;
var lDBMerger: TDBMerger;
begin
  { Recreate primary keys in temp db }
  RebuildPrimaryKeys;
  { Minimise memory consumption }
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF);
  lDBMerger := TDBMerger.Create(FDatabase.ConnectionString, SetProgress, FSetStatus);
  try
    Result := lDBMerger.DoMerge(iDuplicateList, iExclusionList,
        FTablesCreated, FTotalRecords, AImportRejectsFile);
    FRebuildTaxonIndex := lDBMerger.RebuildTaxonIndex;
  finally
    lDBMerger.Free;
  end;
end;  // CopyRecordsIntoMainDB

//==============================================================================
{ Because Access corruption can occur on our created primary keys, we remove
      and re-add every one before copying into main db.  Otherwise SQL filters
      do not always work }
procedure TTempData.RebuildPrimaryKeys;
var i: Integer;
begin
  if Assigned(FDatabase) then
    for i := 0 to FTablesCreated.Count - 1 do
      dmDatabase.RebuildAccessTablePrimaryKey(FDatabase, FTablesCreated[i]);
end;  // RebuildPrimaryKeys

//==============================================================================
{ Handle building of data from the termlists section of the XML document }
procedure TTempData.PopulateTermlists;
var
  lTermlistsTag, lSingleListTag, lItemTag: TTag;
  lTermListName: String;
  lListIndex, lTermIndex: Integer;
  lTermTable: TJNCCTable;
  lPrimaryKey: String;
  lAllSystemSupplied: String; // Yes, No or blank (each item has individual setting }
  lTermDetail: TTermDetail;
  lErrorMessage: TMessage;
begin
  try
    try
      lTermListsTag := FXMLDoc.GetTagByName('termlists');
    except on
      EDTDNotComplete do
        Exit; // no termlist section to import
    end;
    for lListIndex := 0 to lTermListsTag.NestedTags.Count-1 do
    begin
      FProgressBar.TaskPosition := lListIndex * 100 div lTermListsTag.NestedTags.Count;
      lSingleListTag := lTermListsTag.NestedTags[lListIndex];
      lTermListName := lSingleListTag.AttributeData.Values['term_list_name'];
      if not dmNBNXML.CheckIsRealTable(lTermListName) then begin
        lErrorMessage := TMessage.Create;
        lErrorMessage.Msg := ResStr_NotTable;
        FInvalidDataList.AddObject(lTermListName + '=' + ResStr_InvalidTermList, lErrorMessage);
      end else begin
        lTermTable := GetNewTable(lTermListName, lSingleListTag);
        try
          lPrimaryKey := dmDatabase.GetPrimaryKey(lTermListName, False);
          lAllSystemSupplied := lSingleListTag.AttributeData.Values['system_supplied_data'];
          for lTermIndex := 0 to lSingleListTag.NestedTags.Count-1 do
          begin
            lItemTag := lSingleListTag.NestedTags[lTermIndex];
            if Uppercase(lItemTag.Name) = TERM_LIST_ITEM then
            begin
              { Read the definition of the term }
              lTermDetail := TTermDetail.Create(lItemTag);
              try
                { Locate or append to the import termlist table as appropriate }
                LocateAndEditTerm(lTermTable, lTermDetail.TermKey);
                WriteTermToDB(lTermDetail, lTermTable, lAllSystemSupplied, lPrimaryKey);
                { If a local key then remember it }
                if lTermDetail.LocalKey then
                  FLocalKeys.Add(lTermListName + '=' + lTermDetail.TermKey);
                TransferAdditionalFields(lItemTag, lTermTable);
                lTermTable.Post;
                Inc(FTotalRecords);
              finally
                lTermDetail.Free;
              end; // try..finally
            end;
          end; // for lTermIndex
        finally
          lTermTable.Free;
        end;
        FProgressBar.TaskPosition := 100;
      end;
    end; // for lListIndex
  except
    on E:Exception do // add error information
      raise ETempDataError.Create(ResStr_TermListPopulate + lTermListName, E);
  end; // try..except
end;  // PopulateTermList

//==============================================================================
{ Scans through the term table for a key match.  If found, then the record is
    edited, otherwise a new record is appended }
procedure TTempData.LocateAndEditTerm(iTermTable: TJNCCTable; iKey: TKeyString);
var lPrimaryKey: TPrimaryKey;
begin
  lPrimaryKey := dmDatabase.SplitPrimaryKey(iTermTable.TableName);
  if iTermTable.Locate(lPrimaryKey.Key1, iKey, []) then
    iTermTable.Edit
  else
    iTermTable.Append;
end;  // LocateAndEditTerm

//==============================================================================
{ Find any non-standard fields (additional_field) in the tag, and copy the data
     across to the DB record. }
procedure TTempData.TransferAdditionalFields(iTermTag: TTag; iTable: TJNCCTable);
var
  i: Integer;
  lFieldTag: TTag;
  lFieldName: String;
  lNewTable: TJNCCTable;
begin
  for i := 0 to iTermTag.NestedTags.Count-1 do
  begin
    if TTag(iTermTag.NestedTags[i]).Name = 'additional_field' then
    begin
      lFieldTag := TTag(iTermTag.NestedTags[i]);
      lFieldName := lFieldTag.AttributeData.Values['specifier'];
      if (iTable.FieldList.IndexOf(lFieldName)<>-1) then
      begin // appropriate field found
        TagToField(iTable, lFieldName, lFieldTag);
      end else if lFieldName = 'measurement_context_key' then
      begin // Deal with the hardcoded measurement_concept_keys.
        lNewTable := GetNewTable('measurement_type_context', lFieldTag);
        PopulateBlankForeignKeys(lNewTable, iTable);
        WriteFieldToDB(lNewTable, 'measurement_type_key', iTable.FieldByName('measurement_type_key').AsString);
        // Procedure is expecting 'yes'/'no' rather than 'true'/'false' so cannot
        // do this directly.
        if iTable.FieldByName('SYSTEM_SUPPLIED_DATA').AsBoolean then
          WriteFieldToDB(lNewTable, 'SYSTEM_SUPPLIED_DATA', 'YES')
        else
          WriteFieldToDB(lNewTable, 'SYSTEM_SUPPLIED_DATA', 'NO');
        TagToField(lNewTable, 'measurement_context_key', lFieldTag);
        lNewTable.Post;
        Inc(FTotalRecords)
      end;
    end; // if additional field
  end; // for
end;  // TransferAdditionalFields

//==============================================================================
{ WriteTermToDB
    Writes a single termdetail record into a provided DB record.  The provided
    DB record must be a valid termlist.  iAllSystemSupplied is a String, either
    'Yes', 'No', or ''.  If set, this overrides the SystemSuppliedData of the
    current item.   }
procedure TTempData.WriteTermToDB(iTerm: TTermDetail; iTable: TJNCCTable;
  const iAllSystemSupplied, iPrimaryKey: String);
begin
  iTable.FieldByName(iPrimaryKey).AsString := iTerm.TermKey;
  TagToField(iTable, 'SHORT_NAME', iTerm.Term);
  { Check optional tags are present }
  if iTerm.LongName <> nil then
    TagToField(iTable, 'LONG_NAME', iTerm.LongName);
  if iTerm.Definition <> nil then
    TagToField(iTable, 'DESCRIPTION', iTerm.Definition);
  iTable.FieldByName('ENTERED_BY').AsString := iTerm.EnteredBy;
  iTable.FieldByName('ENTRY_DATE').AsDateTime := iTerm.EntryDate;
  { Note - we check the optional fields are present }
  if iTerm.ChangedBy <> '' then
  begin
    if iTable.FieldList.IndexOf('CHANGED_BY')<>-1 then
      iTable.FieldByName('CHANGED_BY').AsString := iTerm.ChangedBy;
    if iTable.FieldList.IndexOf('CHANGED_DATE')<>-1 then
      iTable.FieldByName('CHANGED_DATE').AsDateTime := iTerm.ChangedDate;
  end;
  { Handle cases where an entire termlist is imported as system supplied }
  if iAllSystemSupplied <> '' then
  begin
    if iTable.FieldList.IndexOf('SYSTEM_SUPPLIED_DATA')<>-1 then
      iTable.FieldByName('SYSTEM_SUPPLIED_DATA').Text := iAllSystemSupplied;
  end else // not system_supplied at entire list level, so use local item setting
     if iTable.FieldList.IndexOf('SYSTEM_SUPPLIED_DATA')<>-1 then
      iTable.FieldByName('SYSTEM_SUPPLIED_DATA').AsBoolean := iTerm.SystemSuppliedData;
end;  // WriteTermToDB

//==============================================================================
{ Once the temporary import DB is fully populated, we must identify all local
    keys in the database, and reassign them to new NBN keys.  The foreign keys
    must be altered to point to the new key - this process is identical to the
    merge data process so we reuse the data module }
procedure TTempData.ReassignLocalKeys;
var
  i: Integer;
  lMergeData: TdmMergeData;
  lQuery: TJNCCQuery;
  lOldKey, lNewKey: TKeyString;
  lTaskIndex: Integer;
begin
  { Create a merge data module to point to the import db }
  lMergeData := TdmMergeData.Create(nil, FDatabasePath);
  lMergeData.DeleteSources := False; // no need to remove original record for this use of the merge module
  lQuery := TJNCCQuery.Create(nil);
  lQuery.Connection := FDatabase;  // the temp import database
  try
    for i := 0 to FLocalKeys.Count-1 do
    begin
      lTaskIndex := FProgressBar.EmbedTask((i * 100 div FLocalKeys.Count),
                                            ((i+1) * 100 div FLocalKeys.Count));
      try
        { Take part after equals - can't use values as names not unique }
        lOldKey := Copy(FLocalKeys[i], Pos('=', FLocalKeys[i])+1, 255);
        { Read a new key using the item's table }
        lNewKey := GetNextKey(FLocalKeys.Names[i]);
        SwitchKeyInMainTable(FLocalKeys.Names[i], lQuery, lOldKey, lNewKey);
        { And then all the foreign keys }
        lMergeData.ProcessMergeButLeavePreferred(FLocalKeys.Names[i], lOldKey, lNewKey);
        FProgressBar.TaskPosition := 100;
      finally
        FProgressBar.FinishTask(lTaskIndex);
      end; // try..finally
    end;
  finally
    lMergeData.Free;
    lQuery.Free;
  end; // try..finally
end;

//==============================================================================
{ Merge process relies on the existance of the destination record in the main
    table, so do the actual switch }
procedure TTempData.SwitchKeyInMainTable(const iTable: String; iQuery: TJNCCQuery;
  const iOldKey, iNewKey: TKeyString);
begin
  SetupSQLToResetKey(iQuery, iTable, iOldKey, iNewKey);
  { Reassign the key }
  try
    iQuery.ExecSQL;
  except on E:EOleException do;
    // if simple table not found error then OK to ignore
  end;
  { Recurse to handle special 1-1 joins }
  if LowerCase(iTable)='name' then
  begin
    SwitchKeyInMainTable('individual', iQuery, iOldKey, iNewKey);
    SwitchKeyInMainTable('organisation', iQuery, iOldKey, iNewKey);
  end else if LowerCase(iTable)='source' then
  begin
    SwitchKeyInMainTable('reference', iQuery, iOldKey, iNewKey);
    SwitchKeyInMainTable('source_file', iQuery, iOldKey, iNewKey);
  end;
end;

//==============================================================================
{ Set up a query SQL to allow one key value to be reassigned to another }
procedure TTempData.SetupSQLToResetKey(iQuery: TJNCCQuery; const iTableName: String;
  const lOldKey, lNewKey: TKeyString);
var
  lPrimaryKey: String;
begin
  lPrimaryKey := dmDatabase.GetPrimaryKey(iTableName, False);
  with iQuery.SQL do
  begin
    Clear;
    Add('UPDATE ' + iTableName);
    Add('SET ' + lPrimaryKey + '=''' + lNewKey +'''');
    Add('WHERE ' + lPrimaryKey + '=''' + lOldKey +'''');
  end;
end;

//==============================================================================
{ To avoid heap fragmentation and excessive time creating and freeing tables,
    we pool the tables that have been finished with and reuse them.  If none
    available in the pool, then create a new one. }
function TTempData.GetTableObject(const iTableName: String): TJNCCTable;
var
  lIndex: Integer;
begin
  lIndex := FOpenTablePool.IndexOf(UpperCase(iTableName));
  if lIndex <> -1 then
  begin
    Result := TJNCCTable(FOpenTablePool.Objects[lIndex]);
    FOpenTablePool.Delete(lIndex);
  end
  else
    Result := NewTableObject(iTableName);
end;

//==============================================================================
{ When a table is finished with, rather than freeing it we store it for future
    use.  If a frequently used table, then we keep it open }
procedure TTempData.ReleaseTableObject(iTable: TJNCCTable);
begin
  FOpenTablePool.AddObject(Uppercase(iTable.TableName), iTable)
end;

//==============================================================================
{ No tables available in our pools, so must create a new object }
function TTempData.NewTableObject(const iTableName: String): TJNCCTable;
begin
  Result := TJNCCTable.Create(nil);
  Result.Connection := FDatabase;
  Result.TableName := iTableName;
  Result.Open;
end;

//==============================================================================
//==============================================================================
//------------------------------------------------------------------------------
{ TTermDetail }
//------------------------------------------------------------------------------

{ Constructor - reads all sub-tag info into the relevant class fields }
constructor TTermDetail.Create(iTermTag: TTag);
var
  i: Integer;
begin
  for i := 0 to iTermTag.NestedTags.Count-1 do
  begin
    if CompareText(TTag(iTermTag.NestedTags[i]).Name, 'term_key') = 0 then
    begin
      FTermKey := TTag(iTermTag.NestedTags[i]).Text;
      FLocalKey := (tiLocalKey in TTag(iTermTag.NestedTags[i]).TagInfo);
    end
    else if CompareText(TTag(iTermTag.NestedTags[i]).Name, 'term') = 0 then
      FTerm := TTag(iTermTag.NestedTags[i])
    else if CompareText(TTag(iTermTag.NestedTags[i]).Name, 'long_name') = 0 then
      FLongName := TTag(iTermTag.NestedTags[i])
    else if CompareText(TTag(iTermTag.NestedTags[i]).Name, 'term_definition') = 0 then
      FDefinition := TTag(iTermTag.NestedTags[i])
  end;
  FEnteredBy := iTermTag.AttributeData.Values['entered_by'];
  FEntryDate := StrToDate(iTermTag.AttributeData.Values['entry_date']);
  if iTermTag.AttributeData.Values['changed_by']<>'' then
  begin
    FChangedBy := iTermTag.AttributeData.Values['changed_by'];
    if iTermTag.AttributeData.Values['changed_date']<>'' then
      FChangedDate := StrToDate(iTermTag.AttributeData.Values['changed_date'])
  end;
  { Note, any value other than Yes or No is non-valid, so treat as No. }
  FSystemSuppliedData := (iTermTag.AttributeData.Values['system_supplied_data']='Yes');
end;

{-------------------------------------------------------------------------------
  Progress callback for DBMerger
}
procedure TTempData.SetProgress(const iProgress: Integer;
  iPrcMsgs: Boolean);
begin
  FProgressBar.Position := iProgress;
  FProgressBar.Refresh;
end;

end.
