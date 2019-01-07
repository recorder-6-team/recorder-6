//==============================================================================
//  Unit:        XMLData
//
//  Implements:  TdmNBNXML
//               TSpecialElementItem
//
//  Description:
//               TSpecialElementItem
//               Helper class for special element items from the special xml
//               elements table.
//
//  Author:      John van Breda
//  Created:     21 June 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 55 $
//    $Date: 4/04/08 15:16 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit XMLData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, JNCCDatasets, Dataclasses, ExceptionForm, JNCCXMLDoc, XMLTypes, BaseData,
  Constants, Relationships_ADO, ADODB, DatabaseAccessADO;

resourcestring
  ResStr_Cancelled = 'Export has been cancelled.';

type
  EXMLDataError = class(TExceptionPath);

  { A special element item from the special xml element table }
  TSpecialElementItem = class
    Name : string;
    ElementType : char;
    Data : string;
  end;
  
  TRelationshipDirection = (rdMasterDetail, rdDetailMaster, rdUnrelated);
  ERelationsProblem = class(EXMLDataError);

  TStringArray = array of string;

  TdmNBNXML = class(TBaseDataModule)
    qrySpecialXMLElements: TJNCCQuery;
    qryLocalDTDText: TJNCCQuery;
    qrySpecialTables: TJNCCQuery;
    qryFindExistingFreeTerm: TJNCCQuery;
    qryTermListDesc: TJNCCQuery;
    qryRelatedOccurrences: TJNCCQuery;
    qryListItem: TJNCCQuery;
  private
    { Private declarations }
    FKeyList : TEditableKeyList;  // list of all items we will need to output
    FInvalidKeys: TEditableKeyList;
    FRestrictedData : TStringList; //  a list of keylists - table name is string
    FAlreadyOutputData : TStringList;
    FDocument : TXMLDoc;
    FWantObservations : boolean;
    FLogInfo : TStringList;  // this is the info we don't want to lose
    FSpecialElements : TStringList;
    FTermsToOutput : TStringList;
    FOutputItemCount : integer;
    FTermlists : TStringList;
    FUnusedTables : TStringList;
    FRestrictListItems: boolean; // number of records- just for progress info
    FQueryPool : TList; // Stores list of queries
    {$IFDEF DEBUG}
    FSQLRun : TStringList;
    {$ENDIF}
    procedure SetWantObservations(const Value: boolean);
    procedure MakeRestrictorTable(const iTableName, iTableRef: string);
    procedure RestrictItem(const iTableName, iTableRef, iKey: string);
    procedure SetRestrictListItems(const Value: boolean);
    procedure PrepareOptimisations;
  protected
    procedure AddJoinToQuery(const iMaster, iDetail: string;
           ioQuery: TJNCCQuery; iDetailKey : TKeyString);
    function SearchForTableContainer( const iName : string ): TElement;
    function GetTableContainerForCurrentTable(iKeyList: TEditableKeyList;
      var iIndex: integer; iQuery: TJNCCQuery) : string;
    function ItemTableIdentifier(const iTableName : string): string;
    function CheckRelationshipDirection( const iTable1, iTable2 : string )
                                         : TRelationshipDirection;
    procedure AddItemsToRestrictedData(iQuery : TJNCCQuery);
    procedure AddRecordToRestrictedDataLists(iQuery : TJNCCQuery);
    procedure PreloadSpecialElements;
    procedure CopyRecord( iSource, iDest : TDataset );
    function FindSpecifierInRelations(iRelations: TRelationshipArray;
      iSearchMaster: boolean; iSpecifiers: TStringArray;
      var ioTableName: string; const iExactMatch: boolean): boolean;
    function IsConfidential( iDataset : TDataset ): boolean;
    function IsInvalid(iDataset: TDataset): Boolean;
    function IsRestrictedByHierarchy(iDataset: TDataset;
      const iTableName, iKeyField: string): boolean;
    function IsOccurrenceTable(const iTableName: string): boolean;
    function IsDictListItemTable(const iTableName: string): boolean;
    function IsDeterminationTable( const iTableName : string ) : boolean;
    function IsUnchecked( iDataset : TDataset ) : boolean;
    function CheckListItem(iDataset: TDataset): boolean;
  public
    { Public declarations }
    constructor Create( iDocument : TXMLDoc ); reintroduce;
    destructor Destroy; override;
    function RestrictedData(iDataset: TDataset; iFromExportFilter: Boolean): boolean;
    function AlreadyOutput(const iTableName, iPrimaryKey : string): Boolean;
    function InvalidRecord(const tableName, key: String): Boolean;
    function GetDatasetTable( iDataset : TDataset ): string;
    function GetSpecialTableElement( iDataset : TDataset ): string;
    function GetTableFromSpecialElement( const iSourceTableName,
                                               iElementName : string ) : string;
    function CheckIsTable( const iName : string ) : boolean;
    function CheckIsRealTable( const iName : string ) : boolean;
    function GetContainerElementListForTable( var iIndex : integer ) : TKeyList;
    procedure RememberOutput( const iTableName, iKeyValue : string ); overload;
    procedure RememberOutput( iDataset : TDataset ); overload;
    procedure StoreTerm( iDataset : TDataset ); overload;
    procedure StoreTerm( const iTableName, iKey : string ); overload;
    procedure StoreIfForeignKey( iDataset : TDataset; const iFieldName : string);
    procedure Log( const iText : string ); overload;
    procedure Log( const iText : string; iException : Exception ); overload;
    function ParseSemiColonString( iList : string ) : TStringArray;
    function NameContainsSpecifier( const iName : string;
                                  const iSpecifiers: array of string;
                                  const iExactMatch : boolean ): boolean;
    procedure ReplaceSpecialTable(var iTableName: string;
                                      const iSpecifier: string;
                                      const iSourcetableNAme : string );
    function FindSpecialElement( iName: string;
                             iType : char ): TSpecialElementItem;
    procedure HandleSpecialFields(var ioFieldName: string;
                    iRecord: TDataset);
    function ExistingFreeTerm( const iTableName, iFreeTerm : string;
             iDataset : TDataset; iLocate : boolean) : boolean;
    function IsTermTable( const iDataset : TDataset ): boolean;
    function GetTermlistDescription( const iListName : string ) : string;
    function IsIdentification(const iTable : TJNCCTable) : boolean;
    function IsLocalSpeciesAccount( iDataset : TDataset ) : boolean;
    procedure ClearRestrictedData;
    function IsRestrictedDictListItem(const iTableName: string): boolean;
    property KeyList : TEditableKeyList read FKeyList;
    property InvalidKeys: TEditableKeyList read FInvalidKeys;
    property WantObservations : boolean read FWantObservations
                                             write SetWantObservations;
    property TermsToOutput : TStringList read FTermsToOutput;
    property RestrictListItems : boolean read FRestrictListItems write SetRestrictListItems;
    function GetQueryObject( const iRecord : TRecordIdentifier ): TDataset;
    procedure ReleaseQueryObject(iQuery: TDataset);
    function IsTermlistTable(const iTagName: string): boolean;
    function IsTableUnused(const iTagName: string): boolean;    
  end;

var
  dmNBNXML: TdmNBNXML;

//==============================================================================
implementation

uses MarkupDocs, GeneralData, ApplicationSettings, Maintbar, JNCCRelationships;

{$R *.DFM}

const
  CONFIDENTIAL = 'CONFIDENTIAL';
  VERIFIED     = 'VERIFIED';

  OCCURRENCE='OCCURRENCE'; // must be the last word in any occurrence table name
  DETERMINATION='DETERMINATION'; // must be the last word in any determination table name
  LIST_ITEM='_List_Item';
  LIST_ITEM_KEY='_List_Item_Key';

resourcestring
  ResStr_RecordMissing       = 'A record is missing from the dataset : ';
  ResStr_CreateRecord        = 'Error occurred during call to CreateRecord';
  ResStr_TwoWayRelationship  = 'A database relationship problem has been located between the tables ';
  ResStr_SQLWrong            = 'Internal error.  SQL wrong in XML output dataset.';
  ResStr_UnsupportedDataClass= 'Internal error.  The dataset class is not supported by the export : ';
  ResStr_InvalidSpecialType  = 'The following is not a valid type for special XML elements : ';
  ResStr_IncompatibleStructure = 'The structure of a table in the temporary import database does not match the main table : ';
  ResStr_RestrictionQueryBroken= 'A structure problem has been identified in the restriction query.';
  ResStr_DataRestricted = 'Data restricted in %s for key %s';
  ResStr_ExportNBNData =  'Exporting NBN Data - %s Items output';


//==============================================================================
{ Constructor- just store the keylist for future in appropriate format }
constructor TdmNBNXML.Create( iDocument : TXMLDoc );
var
  lOldCursor : TCursor;
begin
  lOldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    inherited Create(nil);
    FRestrictedData := TStringList.Create;
    FOutputItemCount := 0;
    PreloadSpecialElements;
    FDocument := iDocument;
    FKeyList := TEditableKeyList.Create;
    FInvalidKeys := TEditableKeyList.Create;
    FWantObservations := True; // default value
    FAlreadyOutputData := TStringList.Create;
    FAlreadyOutputData.Sorted := True;
    FAlreadyOutputData.Duplicates := dupIgnore;
    {$IFDEF DEBUG}
    FLogInfo := TStringList.Create;
    {$ENDIF}
    FTermsToOutput := TStringList.Create;
    PrepareOptimisations;
    FQueryPool := TList.Create;
    FQueryPool.Capacity := 10;
    {$IFDEF DEBUG}
    FSQLRun := TStringList.Create;
    {$ENDIF}
  finally
    Screen.Cursor := lOldCursor;
  end; // try..finally
end;


{ Destructor cleans up resricted data lists }
destructor TdmNBNXML.Destroy;
var
  i : integer;
begin
  if FLogInfo <> nil then
    FLogInfo.SaveToFile(ExtractFilePath(Application.Exename) + 'loginfo.txt');
  if FRestrictedData <> nil then
  begin
    for i := 0 to FRestrictedData.Count-1 do
    begin
      TKeyList(FRestrictedData.Objects[i]).Free;
      FRestrictedData.Objects[i] := nil;
    end;
    FRestrictedData.Free;
  end; // if
  FAlreadyOutputData.Free;
  FKeyList.Free;
  FInvalidKeys.Free;
  FLogInfo.Free;
  FTermsToOutput.Free;
  FTermLists.Free;
  FUnusedTables.Free;
  {$IFDEF DEBUG}
  FSQLRun.SaveToFile('c:\temp\sqlrun.txt');
  FSQLRun.Free;
  {$ENDIF}
  if FSpecialElements <> nil then // saftey check in case constructor fails
    For i := 0 to FSpecialElements.Count-1 do
      TSpecialElementItem(FSpecialElements.Objects[i]).Free;
  FSpecialElements.Free;
  { Clean up the query pool }
  for i := 0 to FQueryPool.Count-1 do
    TJNCCTable(FQueryPool[i]).Free;
  FQueryPool.Free;
  FQueryPool := nil;
  inherited Destroy;
end;




{ Read a list of termlist names from the database.  This allows us to optimise
    by spotting termlist entries before we've opened the table }
procedure TdmNBNXML.PrepareOptimisations;

    procedure CheckTableUsed( const iTableName, iElementName : String );
    begin
      with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'select count(*) from ' + iTableName;
      Open;
      try
        if Fields[0].AsInteger = 0 then
          FUnusedTables.Add(iElementName);
      finally
        Close;
      end; // try
    end;

    end;

begin
  FTermlists := TStringList.Create;
  FTermlists.Sorted := True;
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'select "TABLE" from TERM_LIST';
    ParseSQL := False;
    try
      Open;
    finally
      ParseSQL := true;
    end;
    try
      while not EOF do begin
        { measurement units & feature gradings have embedded terms so handle differently }
        if (FieldByName('TABLE').AsString <> 'MEASUREMENT_UNIT') and
           (FieldByName('TABLE').AsString <> 'LOCATION_FEATURE_GRADING') then
          FTermlists.Add(FieldByName('TABLE').AsString);
        Next;
      end;
    finally
      Close;
    end; // try
  end;
  { Prepare a list of tables we would regularly need to scan, but which may be unused }
  FUnusedTables := TStringList.Create;
  FUnusedTables.Sorted := True;
  CheckTableUsed('Taxon_Occurrence_Sources', 'taxon_occurrence=source_link');
  CheckTableUsed('Specimen', 'taxon_occurrence=taxon_specimen');
  CheckTableUsed('Taxon_Occurrence_Relation', 'taxon_occurrence=taxon_occurrence_relation');
end;



{ Return true if a tag name refers to a termlist }
function TdmNBNXML.IsTermlistTable( const iTagName : string ): boolean;
begin
  Result := (FTermLists.IndexOf(iTagName) <> -1);
end;



{ Return true if a Table is unused }
function TdmNBNXML.IsTableUnused( const iTagName : string ): boolean;
begin
  Result := (FUnusedTables.IndexOf(iTagName) <> -1);
end;


{ GetContainerElementItem operates on the internal keylist (the data to
     be output).  It identifies which top level element should be output in the
     XML file to contain the list for the selected table.  For example,
     a sample should be output within a survey (which contains the survey event
     which contains the sample).  It returns a keylist which identifies all the
     top level records to be output (ie several observations could mean several
     surveys) and also prepares a list of restrictions so that by including a
     survey, you don't automatically pick up all the events for that survey etc.
     etc. Converts the Result list to a MIXED list.
     ALL ITEMS FOR SELECTED TABLE, and iIndex is updated past all items
     processed }
function TdmNBNXML.GetContainerElementListForTable( var iIndex : Integer ) : TKeyList;
var
  lQuery : TJNCCQuery;
  lDataListIndex : integer;
  lLastTable : string;
begin
  lQuery := TJNCCQuery.Create(nil);
  try
    lLastTable := GetTableContainerForCurrentTable(FKeyList, iIndex, lQuery);
    lDataListIndex := FRestrictedData.IndexOf( ItemTableIdentifier(lLastTable) );
    if lDataListIndex <> -1 then
    begin
      Result := TKeyList(FRestrictedData.Objects[lDataListIndex])
    end
    else
      Result := nil; // signifies can use item as is
  finally
    lQuery.Free;
  end; // try..finally
end;


function TdmNBNXML.ItemTableIdentifier(const iTableName : string): string;
begin
  if   (comparetext(iTableName, 'TAXON_OCCURRENCE')=0) or
       (comparetext(iTableName, 'BIOTOPE_OCCURRENCE')=0) then
    Result := 'OCCURRENCE'
  else
    Result := lowercase(iTableName) + '_tablekey';
end;


function TdmNBNXML.GetTableContainerForCurrentTable( iKeyList : TEditableKeyList;
          var iIndex : integer; iQuery : TJNCCQuery ) : string;
var
  lLastTableFound, lCurrentTable : string;
  lContainerElement : TElement;
  ltfFinished : boolean;
begin
  lCurrentTable := FKeyList.ItemTable[iIndex];
  while iIndex < FKeyList.Header.ItemCount do
  begin
    Application.ProcessMessages;
    if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
      raise ECancellation.CreateNonCritical(ResStr_Cancelled);

    { Determine our starting table }
    if (lCurrentTable <> iKeyList.ItemTable[iIndex]) and not
       ((CompareText(lCurrentTable, 'BIOTOPE_OCCURRENCE')=0) and
        (CompareText(iKeyList.ItemTable[iIndex], 'TAXON_OCCURRENCE')=0))
        then
      break; // from loop - all items for current table handled
    lLastTableFound := iKeyList.ItemTable[iIndex];
    ltfFinished := False;
    iQuery.SQL.Clear;
    repeat
      { Locate any table elements which contain this element in their content }
      lContainerElement := SearchForTableContainer( lLastTableFound );
      if lContainerElement = nil then
        ltfFinished := True
      else
        If CheckRelationshipDirection( lContainerElement.Name,
                                     lLastTableFound ) = rdMasterDetail then
        begin
          AddJoinToQuery( lContainerElement.Name, lLastTableFound,
                                iQuery, iKeyList.Items[iIndex].KeyField1 );
          lLastTableFound := lContainerElement.Name
        end
        else
          ltfFinished := True;
    until ltfFinished;
    if iQuery.SQL.Count = 0 then
    begin
(*      MakeRestrictorTable( iKeyList.ItemTable[iIndex], iKeyList.ItemTable[iIndex] + '_tablekey' );
      RestrictItem( iKeyList.ItemTable[iIndex], iKeyList.ItemTable[iIndex] + '_tablekey',
                    iKeyList.Items[iIndex].KeyField1 );*)
      Inc(iIndex);
      break; // already top level data, so return as is
    end;
    AddItemsToRestrictedData(iQuery);
    Inc(iIndex);
  end;
  Result := lLastTableFound;
end;

{ Builds an SQL statement one join at a time.  If the sql statement is empty,
     then a query for the detail table is set up.  The master table is then
     joined into the query (whether or not it was empty to start with). }
procedure TdmNBNXML.AddJoinToQuery(const iMaster, iDetail: string;
       ioQuery: TJNCCQuery; iDetailKey : TKeyString);
var
  lRelationships : TRelationshipArray;
  lMasterFieldName, lDetailFieldName : string;
  lDetailTableKey : string;
  lWherePos, lFromPos : integer;
begin
  lRelationships := dmDatabase.Relationships.FindRelationsBetweenTables( iMaster, iDetail );
  { By this stage, we already know there is a relationship }
  lMasterFieldName := lRelationships[0].Fields[0].MasterName;
  lDetailFieldName := lRelationships[0].Fields[0].DetailName;
  if ioQuery.SQL.Count = 0 then // need to set up for detail table
  begin
    lDetailTableKey := dmDatabase.GetPrimaryKey(iDetail, False);
    ioQuery.SQL.Add('SELECT');
    ioQuery.SQL.Add(iDetail + '.' + lDetailTableKey + ' as ' + iDetail + '_tablekey');
    ioQuery.SQL.Add('FROM');
    ioQuery.SQL.Add(iDetail);
    ioQuery.SQL.Add('WHERE');
    ioQuery.SQL.Add(iDetail + '.' + lDetailTableKey + '=''' +
                           iDetailKey + '''');
  end;
  lFromPos := ioQuery.SQL.IndexOf('FROM');
  if (lFromPos = -1) then
    raise EXMLDataError.Create(ResStr_RestrictionQueryBroken + ioQuery.SQL.Text);
  { Splice clauses into correct SQL locations }
  ioQuery.SQL.Insert(lFromPos, ', ' + iMaster + '.' + lMasterFieldName +
                    ' as ' + iMaster + '_tablekey');
  lWherePos := ioQuery.SQL.IndexOf('WHERE');
  if (lFromPos = -1) then
    raise EXMLDataError.Create(ResStr_RestrictionQueryBroken + ioQuery.SQL.Text);
  ioQuery.SQL.Insert(lWherePos, ', ' + iMaster);  // add table to from clause
  { add where clause to end }
  ioQuery.SQL.Add(' and ' + iMaster + '.' + lMasterFieldName + '='
               + iDetail + '.' + lDetailFieldName);
end;



{ Keeps iterating up through containers of an element until it finds a table
     element.  Returns nil if it can't find one.  If the element already
     is a table element, then still finds the next table upwards through the
     elements. }
function TdmNBNXML.SearchForTableContainer(const iName: string): TElement;
var
  lContainerElement : TElement;
begin
  lContainerElement := FDocument.GetElementByName(iName);
  repeat
    lContainerElement := FDocument.SearchForContainedContentByName( lContainerElement.Name );
    if lContainerElement = nil then
      break; // from while loop - there is no container so return nil!
  until lContainerElement.IsTable;
  Result := lContainerElement;
end;



{ Find out if two tables are related, and if so, in which direction.  If several
     relations exist which specify different directions, then something is
     wrong with the data model and an exception is raised. }
function TdmNBNXML.CheckRelationshipDirection(const iTable1,
                         iTable2: string): TRelationshipDirection;
var
  lRelations : TRelationshipArray;
  ltfMasterDetailFound, ltfDetailMasterFound : boolean;
  i : integer;
begin
  { Find all the relationships, both directions }
  lRelations := dmDatabase.Relationships.FindRelationsBetweenTables( iTable1, iTable2 );
  { Need to identify which way they are }
  ltfMasterDetailFound := False;
  ltfDetailMasterFound := False;
  for i := 0 to High(lRelations) do
  begin
    if Uppercase(lRelations[i].MasterTable) = Uppercase(iTable1) then
      ltfMasterDetailFound := True
    else
      if Uppercase(lRelations[i].MasterTable) = Uppercase(iTable2) then
        ltfDetailMasterFound := True;
  end;
  if (not ltfMasterDetailFound) and (not ltfDetailMasterFound) then
    { No relationships present }
    Result := rdUnrelated
  else if ltfMasterDetailFound and (not ltfDetailMasterFound) then
    Result := rdMasterDetail
  else if (not ltfMasterDetailFound) and ltfDetailMasterFound then
    Result := rdDetailMaster
  else
    { Can't be both master and detail at both ends!}
    raise ERelationsProblem.Create( ResStr_TwoWayRelationship + iTable1 +
                                     ' and ' + iTable2)
end;



{ Maintains a list of keylists, which identifies all the tables whose output
     is restricted in this xml document.  For example, if you output a sample,
     you only want to output the one survey event for the sample }
procedure TdmNBNXML.AddItemsToRestrictedData(iQuery: TJNCCQuery);
var
  i : integer;
  lFieldTableName : string;
begin
  if iQuery.SQL.Count = 0 then
    { nothing to do }
    Exit;
//  iQuery.SessionName := JNCC_SESSION_NAME;
//  iQuery.DatabaseName := LOCAL_DATABASE_NAME;
  dmDatabase.SetDatabaseLocal([iQuery]);
  iQuery.Open;
  { Prepare any keylists we don't yet have }
  for i := 0 to iQuery.FieldCount-1 do
  begin
    { Strip the _tablekey bit off the field name to find the table the field came from }
    lFieldTableName := Copy( iQuery.Fields[i].FieldName, 1,
                       Length(iQuery.Fields[i].FieldName) - Length('_tablekey'));
    MakeRestrictorTable( lFieldTableName, iQuery.Fields[i].FieldName );
  end;
  with iQuery do
  begin
    First;
    while not EOF do
    begin
      Application.ProcessMessages;
      if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
        raise ECancellation.CreateNonCritical(ResStr_Cancelled);

      AddRecordToRestrictedDataLists( iQuery );
      Next;
    end; // while
    Close;
  end; // with lQuery
end;


{ Prepares a key list for holding restricted data items for a given table,
     if one is not already available }
procedure TdmNBNXML.MakeRestrictorTable( const iTableName, iTableRef : string );
var
  lKeyList : TEditableKeyList;
begin
  if not IsOccurrenceTable(iTableName) then
  begin
    if FRestrictedData.IndexOf(lowercase(iTableRef) ) = -1 then
    begin
      { Need a new keylist for this table }
      lKeyList := TEditableKeyList.Create;
      lKeyList.SetTable(iTableName);
      FRestrictedData.AddObject(Lowercase(iTableRef), lKeyList);
    end;
  end else
  begin
    if FRestrictedData.IndexOf(OCCURRENCE) = -1 then
    begin
      { Need a new keylist for this table }
      lKeyList := TEditableKeyList.Create;
      lKeyList.SetTable(MIXED_DATA);
      FRestrictedData.AddObject(OCCURRENCE, lKeyList);
    end;
  end;
end;

procedure TdmNBNXML.RestrictItem( const iTableName, iTableRef, iKey : string );
var
  lListIndex : integer;
begin
  { Find the relevant key list - OCCURRENCE keys all in same keylist }
  if not IsOccurrenceTable(iTableName) then
  begin
    lListIndex := FRestrictedData.IndexOf( Lowercase(iTableRef) );
    if lListIndex <> -1 then // just a safety check
    begin
      with TEditableKeyList(FRestrictedData.Objects[lListIndex]) do
        { If not already in list, add item to end of list }
        if IndexOf(iKey, '') = -1 then
          AddItem(iKey, '');
    end; // if llistindex
  end else
  begin // special case for occurrences
    lListIndex := FRestrictedData.IndexOf(OCCURRENCE);
    if lListIndex <> -1 then // just a safety check
    begin
      with TEditableKeyList(FRestrictedData.Objects[lListIndex]) do
        { If not already in list, add item to end of list }
        if IndexOf(iKey, iTableName) = -1 then
          AddItem(iKey, Uppercase(iTableName));
    end; // if llistindex
  end;
end;


{ Returns true is a table name matches the criteria to identify it as an
   occurrence table.  Not case sensitive }
function TdmNBNXML.IsOccurrenceTable( const iTableName : string ) : boolean;
begin
  { Simply look for OCCURRENCE at the end of the table name }
  Result := CompareText(Copy(iTableName, Length(iTableName)-Length(OCCURRENCE)+1,
                               Length(OCCURRENCE)),OCCURRENCE)=0;
end;


{ Returns true is a table name matches the criteria to identify it as a
   dictionary list item (e.g. TAXON_LIST_ITEM) table.  Not case sensitive }
function TdmNBNXML.IsDictListItemTable( const iTableName : string ) : boolean;
begin
  { Simply look for OCCURRENCE at the end of the table name }
  Result := CompareText(Copy(iTableName, Length(iTableName)-Length(LIST_ITEM)+1,
                               Length(LIST_ITEM)),LIST_ITEM)=0;
end;


{ Returns true is a table name matches the criteria to identify it as a
   determination table.  Not case sensitive }
function TdmNBNXML.IsDeterminationTable( const iTableName : string ) : boolean;
begin
  { Simply look for DETERMINATION at the end of the table name }
  Result := CompareText(Copy(iTableName, Length(iTableName)-Length(DETERMINATION)+1,
                               Length(DETERMINATION)),DETERMINATION)=0;
end;



{ For the current record in the query, adds the item keys into the restricted
     data lists.  This means we ARE allowed to export these keys, but not
     any other ones from the same tables.  Occurrence data handled differently
     as it all goes into one mixed restriction list.}
procedure TdmNBNXML.AddRecordToRestrictedDataLists(iQuery: TJNCCQuery);
var
  lFieldIndex : integer;
  lFieldTableName : string;
begin
  { Go through each field }
  for lFieldIndex := 0 to iQuery.FieldCount-1 do
  begin
    lFieldTableName := Copy( iQuery.Fields[lFieldIndex].FieldName, 1,
                       Length(iQuery.Fields[lFieldIndex].FieldName) - Length('_tablekey'));
    RestrictItem(lFieldTableName, iQuery.Fields[lFieldIndex].FieldName,
                                  iQuery.Fields[lFieldIndex].AsString);
  end;  // for
end; // procedure AddRecordToRestrictedDataLists



{ Returns true if a record should not be output because it is already output.
    This is much faster than a full ResdtrictedData check (see below) so this
    is done first to pre-filter, without opening a dataset unecessarily first. }
function TdmNBNXML.AlreadyOutput(const iTableName, iPrimaryKey : string) : boolean;
begin
  Result := FAlreadyOutputData.IndexOf( iPrimaryKey + lowercase(iTableName) ) <> -1;
end;

{
  Returns true if the record is invalid. This means it should not be exported.
}
function TdmNBNXML.InvalidRecord(const tableName, key: String): Boolean;
begin
  Result := FInvalidKeys.IndexOf(key, tableName) > -1;
end;

{ Returns true if a record should not be output because it is restricted }
function TdmNBNXML.RestrictedData(iDataset: TDataset; iFromExportFilter: Boolean): boolean;
var
  lKeyField, lTableName : string;
  lListIndex : integer;
  ltfFound : boolean;
  {$IFDEF DEBUG}lstReason : string; {$ENDIF}   // diagnostics
begin
  lTableName := GetDatasetTable(iDataset);
  Result := false; // default - unrestricted
  try
    lKeyField := dmDatabase.GetPrimaryKey( lTableName, False );
  except
    on EMultiFieldKey do begin
      if (AppSettings.CanExportSystemSupplied) and
         dmGeneralData.IsSystemSuppliedRecord(iDataset) then
        Result := True;
      Exit; // nothing - join tables never get restricted unless system supplied
    end;
  end;

  if IsConfidential(iDataset) and
     ((AppSettings.UserAccessLevel <> ualAdmin) or not AppSettings.ExportConfidentialOccurrences) then
    Result := True
  else
  if (not AppSettings.CanExportSystemSupplied) and
     dmGeneralData.IsSystemSuppliedRecord(iDataset) then
    Result := True
  else
  if IsLocalSpeciesAccount(iDataset) then
    Result := True
  else
  if IsRestrictedByHierarchy(iDataset, lTableName, lKeyField) then
    Result := True
  else
  if IsUnchecked(iDataset) and not iFromExportFilter then
    Result := True
  else
  if IsInvalid(iDataset) and not iFromExportFilter then
    Result := True
  else begin
    // is it in a restriction list?
    lListIndex :=FRestrictedData.IndexOf( Lowercase(lTableName) + '_tablekey' );
    ltfFound := False;
    if lListIndex <> -1 then
    begin
      { List found, so must check that item is allowed }
      with TKeyList(FRestrictedData.Objects[lListIndex]) do
        ltfFound := (IndexOf(iDataset.FieldByName(lKeyField).AsString, '') <> -1)
    end;// if list found

    { If item not found, but a list was available for the table, then restrict data }
    if (not ltfFound) and (lListIndex <> -1) then
      Result := True
    else begin
      // Still OK to output so we must check it hasn't already been done -
      // not restricted if the data not found in the already output data
      // And check it hasn't been flagged invalid during revalidation before output.
      Result := AlreadyOutput(lTableName, iDataset.FieldByName(lKeyField).AsString) or
                InvalidRecord(lTableName, iDataset.FieldByName(lKeyField).AsString);
    end;
  end; // if wantobservations
  {$IFDEF DEBUG}
  if Result then
    Log(Format(ResStr_DataRestricted, [lTableName, iDataset.FieldByName(lKeyField).AsString]));
  {$ENDIF}
end;


{ Return true if we are outputting a checklist as a secondary item, and the
     data we are outputting is a list item.  Otherwise you output 1 list item,
     which creates a secondary output item for the list, then you get the whole
     list! }
function TdmNBNXML.IsRestrictedDictListItem( const iTableName : string ): boolean;
begin
  Result := False; // default
  if IsDictListItemTable( iTableName ) then
    Result := RestrictListItems;
end;


{ This function checks that if one type of occurrence is restricted, then
    all types of occurrence get restricted as well.  Therefore, if you export
    a taxon occurrence specifically, you don't get all the biotopes as well. }
function TdmNBNXML.IsRestrictedByHierarchy(iDataset : TDataset;
        const iTableName, iKeyField : string) : boolean;
var
  lListIndex : integer;
  ltfFound : boolean;
begin
  Result := False; // default
  { if an occurrence record }
  if IsOccurrenceTable(iTableName) or IsDictListItemTable(iTableName) then
  begin
    if IsOccurrenceTable(iTableName) then
      lListIndex :=FRestrictedData.IndexOf( 'OCCURRENCE' )
    else
      lListIndex := FRestrictedData.IndexOf( Uppercase(iTableName) );
    ltfFound := False;
    if lListIndex <> -1 then
    begin
      { List found, so must check that item is allowed - OCCURRENCE list is MIXED }
      with TKeyList(FRestrictedData.Objects[lListIndex]) do
        ltfFound := (IndexOf(iDataset.FieldByName(iKeyField).AsString,
                    Uppercase(iTableName)) <> -1)
    end;// if list found
    { If item not found, but a list was available for the table, then restrict data }
    if (not ltfFound) and (lListIndex <> -1) then
      Result := True
    else
    begin
      { Still OK to output so we must check it hasn't already been done -
         not restricted if the data not found in the already output data }
      Result := AlreadyOutput(iTableName, iDataset.FieldByName(iKeyField).AsString);
    end;
  end;
end;


{ Returns true if the dataset has a field called confidential, which is boolean
    and is set to True }
function TdmNBNXML.IsConfidential(iDataset: TDataset): boolean;
begin
  { Default to false if no valid field found }
  Result := False;
  if iDataset.FieldList.IndexOf( CONFIDENTIAL )<>-1 then
    if iDataset.FieldByName(CONFIDENTIAL).DataType = ftBoolean then
      Result := iDataset.FieldByName(CONFIDENTIAL).AsBoolean;
end;

{ Returns true if the dataset has a field called verified, which is an integer
    and is not set to 1 }
function TdmNBNXML.IsInvalid(iDataset: TDataset): Boolean;
begin
  Result := False;
  if iDataset.FieldList.IndexOf(VERIFIED) <> -1 then
    if iDataset.FieldByName(VERIFIED).DataType in [ftSmallInt, ftInteger] then
      Result := iDataset.FieldByName(VERIFIED).AsInteger = 1;
end;

{ For a dataset (currently must be Titan but easy to change) returned by the
     CreateRecord function, locates the table name.  Easy for tables - but for
     queries it scans for the 'from' and returns the next word }
function TdmNBNXML.GetDatasetTable(iDataset: TDataset): string;
var
  lRow, lPos : integer;
begin
  // Currently only titan datasets
  if iDataset is TADOTable then
    Result := TADOTable(iDataset).TableName
  else if iDataset is TADOQuery then
  begin
    { Temporarily embed the SQL file into the document, so that we can use
         our DocParser against it. }
    FDocument.EmbedDocument( TADOQuery(iDataset).SQL );
    lRow := 0;
    lPos := 0;
    try
      try
        FDocument.DocParser.SearchFor( stFindwords, ['FROM', 'from'], lRow, lPos );
      except
        on E:EEOFError do { trap the parser failing to find the from }
          raise EDocumentError.Create(ResStr_SQLWrong, E);
      end; // try except
      Inc(lPos, Length('from') + 1);
      Result := FDocument.DocParser.GetNextWord( lRow, lPos );
    finally
      FDocument.FinishEmbeddedDoc;
    end; // try
  end
  else
    raise EXMLDataError.Create(ResStr_UnsupportedDataClass + iDataset.Classname);
end;


{ Where a table name is not represented by a directly matching element name,
    scans the SPECIAL_XML_ELEMENTS table for possible matches.  NOTE - THERE
    IS SOME AMBIGUITY HERE - NEED TO DEFINE RULES }
function TdmNBNXML.GetSpecialTableElement(iDataset: TDataset): string;
var
  ltfFound : boolean;
  lTableName : string; // from the actual dataset
begin
  Result := ''; // default -  not found
  ltfFound := False;
  lTableName := GetDatasetTable( iDataset );
  with qrySpecialTables do
  begin
    if not Active then
      Open;
    First;
    while (not EOF) and (not ltfFound) do
    begin
      if Self.NameContainsSpecifier( lTableName,
           Self.ParseSemiColonString(FieldByName('Data').AsString), False ) then
      begin
        ltfFound := True;
        Break; // from loop
      end;
      next;
    end; // while
    if ltfFound then
      Result := FieldByName('Name').AsString
  end; // with
end;


{ If an element name is one of the special elements, then follows relationships
     from the dataset to find the first matching associated table.  For example,
     for a determination element with the BIOTOPE_OCCURRENCE dataset, then the
     BIOTOPE_DETERMINATION table is located and the name returned. If not a
     special element, then the original element name is returned. }
function TdmNBNXML.GetTableFromSpecialElement(const iSourceTableName,
                                               iElementName: string): string;
var
  lSpecialElement : TSpecialElementItem;
begin
  lSpecialElement := FindSpecialElement( iElementName, TRANSLATE_TABLE );
  Result := iElementName; // default
  if lSpecialElement <> nil then
    { Perform scan through relationships and find related table which
              matches specifiers }
    ReplaceSpecialTable( Result, lSpecialElement.Data, iSourceTableName );
end;


{ If the field name in the dataset represents a foreign key value, then the
     master table is identified and the field's value added to the list
     of data we will need to output at the end }
procedure TdmNBNXML.StoreIfForeignKey(iDataset: TDataset;
  const iFieldName: string);
var
  lRelIndex : integer;
  lTableName : string;
begin
  lTableName := GetDatasetTable( iDataset );
  { Special case for one to one type joins which don't need to be stored }
  if not ((CompareText(lTableName,'NAME')=0) and
         (CompareText(iFieldName, 'name_key')=0)) then
  begin
    lRelIndex := dmDatabase.Relationships.FindRelationship(lTableName, iFieldName);
    if lRelIndex > -1 then
    begin
      { found a relationship where we are starting at the foreign key, so need to
         remember to output the master record }
      { If the key field is null, don't store it }
      if iDataset.FieldByName(iFieldName).AsString <> '' then
        FKeyList.AddItem(
                      iDataset.FieldByName(iFieldName).AsString,
                      dmDatabase.Relationships.Relationship[lRelIndex].MasterTable);
    end
    else if lRelIndex = NAME_REL then // non-physical metadata relationship
    begin
      { If the key field is null, don't store it }
      if iDataset.FieldByName(iFieldName).AsString <> '' then
        FKeyList.AddItem(
                      iDataset.FieldByName(iFieldName).AsString,
                      'NAME');
    end;
  end; // if
end;



{ Record that the data item passed in has been output.  This is then checked
     when a call to restricteddata is made, so that the item is not output
     twice. }
procedure TdmNBNXML.RememberOutput(const iTableName, iKeyValue : string);
begin
  FAlreadyOutputData.Add( iKeyValue + lowercase(iTableName) );
  { As every record output comes through this procedure, this is a good time to
      update the status bar }
  Inc(FOutputItemCount);
  frmMain.SetStatus(Format(ResStr_ExportNBNData, [IntToStr(FOutputItemCount)]));
end;



{ Overloaded version of RemeberOutput which uses the current record of a dataset
    to identify the table and key value }
procedure TdmNBNXML.RememberOutput(iDataset: TDataset);
var
  lTableName : string;
  lKeyField : string;
begin
  lTableName := GetDatasetTable(iDataset);
  try
    lKeyField := dmDatabase.GetPrimaryKey(lTableName, False);
    RememberOutput(lTableName, iDataset.FieldByName(lKeyField).AsString);
  except
    on EMultiFieldKey do
      ; { nothing - we don't need to remember that we have output join records
           as the master record we joined from will control this instead }
  end; // try..except
end;



{ Utility function to check if the name is in the current database's list
    of tables.  Used to segregate table elements from field elements.  Just
    looks through an already prepared list, and also checks for special tables
    as described by the SPECIAL_XML_ELEMENTS table.  }
function TdmNBNXML.CheckIsTable(const iName: string): boolean;
begin
  // Metadata table should not be checked, as this is part of the XML header
  if CompareText(iName, 'metadata')<>0 then begin
    Result := CheckIsRealTable( iName );
    { If not a real table, check special xml elements for a special table element }
    if not Result then
      Result := (FindSpecialElement(iName, TRANSLATE_TABLE) <> nil);
  end
  else
    Result := False;
end;



{ When an element is found which has a 'special' name, then this method looks
    for a detail table related to the current table with the appropriate
    specifier.  The table name (currently one of the special values) is then
    replaced with the located detail table.  Allows determinations,
    identifications etc to remain generic in the DTD.  The iSpecifier string
    can be a list of specifiers separated by semi-colons }
procedure TdmNBNXML.ReplaceSpecialTable(var iTableName : string;
            const iSpecifier: string; const iSourceTableName : string);
var
  lDetRelations, lMasterRelations : TRelationshipArray;
  lSpecifiers : TStringArray;
begin
  lSpecifiers := ParseSemiColonString( iSpecifier );
  { try looking at detail tables first }
  lDetRelations := dmDatabase.Relationships.FindDetails(iSourceTableName);
  lMasterRelations := dmDatabase.Relationships.FindDependencies(iSourceTableName);
  { Try and find a related table with correct specifier in the name.  If not
      found, no change is made to the table name }

  if not FindSpecifierInRelations( lDetRelations, False, lSpecifiers,
                                   iTableName, True ) then
    if not FindSpecifierInRelations( lMasterRelations, True, lSpecifiers,
                                   iTableName, True ) then
      if not FindSpecifierInRelations( lDetRelations, False, lSpecifiers,
                                   iTableName, False ) then
        FindSpecifierInRelations( lMasterRelations, True, lSpecifiers,
                                   iTableName, False );
end;


{ FindSpecifierInRelations looks through a list of relations for a table name
    which matches one of the specifiers.  Can search either end of the relations
    ( iSearchMaster parameter ), and can do exact matches, or contains...
    matches.  If found, ioTableName is changed accordingly. }
function TdmNBNXML.FindSpecifierInRelations( iRelations : TRelationshipArray;
         iSearchMaster : boolean; iSpecifiers : TStringArray;
         var ioTableName : string; const iExactMatch : boolean ): boolean;
var
  i : integer;
  lSearchTable : string;
begin
  Result := False; // default no change made
  for i := 0 to High(iRelations) do
  begin
    if iSearchMaster then
      lSearchTable := iRelations[i].MasterTable
    else
      lSearchTable := iRelations[i].DetailTable;
    { Must do a non-case sensitive test }
    if NameContainsSpecifier( lSearchTable, iSpecifiers, iExactMatch ) then
    begin
      { Found a match for the determination specifier, so use this table name }
      ioTableName := lSearchTable;
      Result := True;
      break; // from loop
    end;
  end; // for
end;



{ Check is real table is like checkIsTable, but only returns true if the table
    name passed in is a real physical table, ie not one of the special tables
    like source_link }
function TdmNBNXML.CheckIsRealTable(const iName: string): boolean;
begin
  Result := (dmGeneralData.Tablelist.IndexOf( iName ) <> -1) and
         (CompareText(iName, 'determination')<>0);
end;



{ Accessor method.  If wantObservations is true, then we are allowed export any
     observation data we encounter }
procedure TdmNBNXML.SetWantObservations(const Value: boolean);
begin
  FWantObservations := Value;
end;


{ Store a bit of text on the 'non-critical' log so we can keep a record of bits
     we had to skip etc }
procedure TdmNBNXML.Log(const iText: string);
begin
  if FLogInfo<>nil then
    FLogInfo.Add( iText );
end;

{ Overloaded version which also logs the contents of an exception }
procedure TdmNBNXML.Log(const iText: string; iException: Exception);
const
  TAB = #9;
begin
  if FLogInfo<>nil then begin
    FLogInfo.Add( iText );
    { Add the real exception message }
    FLogInfo.Add( TAB + iException.Message );
  end;
end;

{ NameContainsSpecifier function.  Returns true if the name string contains any
     of the specifier strings, but only if the specifier string is surrounded by
     underscores or is at the extremity of the name.  For example,
     THIS_FROM_THAT, THIS_FROM and FROM_THAT all contain the FROM specifier, but
     THIS_FROMTHAT and FROMTHAT and THISFROM_THAT do not.  Case sensitive. If
     iExactMatch parameter is true then the name must match the specifier
     exactly. }
function TdmNBNXML.NameContainsSpecifier(const iName : string;
     const iSpecifiers: array of string; const iExactMatch : boolean): boolean;
var
  i : integer;
  lFound : boolean;
begin
  lFound := False;
  i := 0;
  while (i <= High(iSpecifiers)) and (not lFound) do
  begin
    if iExactMatch then
      lFound := iSpecifiers[i]=iName
    else
      { Need to look for a substring }
      lFound := (Pos(iSpecifiers[i], iName + '_') = 1) or // is it at beginning
            (Pos(iSpecifiers[i], '_' + iName + '_') <> 0) or // in middle
            ((Pos(iSpecifiers[i], '_' + iName) = Length(iName) - Length(iSpecifiers[i])) and
                (Pos(iSpecifiers[i], '_' + iName)<>0)) or // end
            (iName = iSpecifiers[i]); // or exactly same
    Inc(i);
  end;
  Result := lFound;
end;



{ Parses a semi-colon separated list of string values into an array of string
     values }
function TdmNBNXML.ParseSemiColonString(iList: string): TStringArray;
var
  lSpecifiers : TStringArray;
  lParseString : string;
begin
  { Parse the iList string into an array (if it has semi-colon separators) }
  SetLength(lSpecifiers, 0);
  lParseString := iList;
  while Pos(';', lParseString) <> 0 do
  begin
    SetLength(lSpecifiers, High(lSpecifiers) + 2); // inc by 1 (not 2!) since High = count-1
    { Read the next item from the parse string }
    lSpecifiers[High(lSpecifiers)] := Copy(lParseString, 1, Pos(';', lParseString) - 1);
    { Take remaining string and continue parsing }
    lParseString := Copy(lParseString, Pos(';', lParseString) + 1, High(integer));
  end;
  SetLength(lSpecifiers, High(lSpecifiers) + 2); // inc by 1 (not 2!)
  { Read the final item from the parse string }
  lSpecifiers[High(lSpecifiers)] := lParseString;
  Result := lSpecifiers;
end;


{ Procedure to read the special elements from SPECIAL_XML_ELEMENTS and store
    them in a local cache.  This is because we repetitively query the info from
    this table during import/export, so preloading improves performance. }
procedure TdmNBNXML.PreloadSpecialElements;
var
  lNewElement : TSpecialElementItem;
begin
  FSpecialElements := TStringList.Create;
  with qrySpecialXMLElements do
  begin
    Open;
    while not EOF do
    begin
      { Create a new element }
      lNewElement := TSpecialElementItem.Create;
      lNewElement.Name := FieldByName('Name').AsString;
      { Check type field is one of 4 valid characters }
      if (FieldByName('Type').AsString = 'F') or
         (FieldByName('Type').AsString = 'T') or
         (FieldByName('Type').AsString = 'M') or
         (FieldByName('Type').AsString = 'I') then
        lNewElement.ElementType := FieldByName('Type').AsString[1]
      else
        raise EXMLDataError.Create(ResStr_InvalidSpecialType +
                     FieldByName('Type').AsString);
      lNewElement.Data := FieldByName('Data').AsString;
      { Store it on the list }
      FSpecialElements.AddObject( lNewElement.Name + FieldByName('Type').AsString,
                                                  lNewElement );
      Next;
    end; // while
    Close;
  end; // with query
end;


{ Look through cached special xml elements for an item with correct name and
     type.  If not found, returns nil. }
function TdmNBNXML.FindSpecialElement(iName: string;
                                       iType: char): TSpecialElementItem;
var
  lIndex : integer;
begin
  Result := nil; // default
  lIndex := FSpecialElements.IndexOf(iName + iType);
  if lIndex <> -1 then
    Result := TSpecialElementItem(FSpecialElements.Objects[lIndex]);
end;


{ Checks for field names which require conversion according to the SPECIAL_XML
     ELEMENTS table.  For example, determination_key means the primary key of
     any determination table.  If no conversion performed then the original
     string is not changed.  A * specifier (in the data column) indicates the
     primary key, otherwise the fieldname is replaced with the specifier }
procedure TdmNBNXML.HandleSpecialFields(var ioFieldName: string;
  iRecord: TDataset);
var
  lSpecialElement : TSpecialElementItem;
  lFieldArray : TStringArray;
  i : integer;
begin
  lSpecialElement := dmNBNXML.FindSpecialElement( ioFieldName, TRANSLATE_FIELD );
  SetLength(lFieldArray, 0); // remove compiler warning
  if lSpecialElement <> nil then
  begin
    lFieldArray := ParseSemiColonString(lSpecialElement.Data);
    { Find the first of all the optional field names which we can get a valid
        field for }
    for i := 0 to High(lFieldArray) do
    begin
      { Record found, so perform appropriate conversion }
      if lFieldArray[i] = '*' then
      begin
        ioFieldName := dmDatabase.GetPrimaryKey( dmNBNXML.GetDatasetTable( iRecord ),
                                               False );
        break; // from loop - we have a translation
      end else
        if iRecord.FieldList.IndexOf( lFieldArray[i] ) <> -1 then
        begin // found a match
          ioFieldName := lFieldArray[i];
          break; // finished loop
        end;
    end; // for
  end; // if item found
end;


{ Looks for an existing free_term in the main database.  If one exists, copies
     the record into the provided dataset and returns true.  Otherwise,
     returns false.  The dataset should obviously be ready with a blank record
     to populate.  If iLocate is true, then tries to locate the record in the
     iDataset, otherwise it tries to copy into the dataset. }
function TdmNBNXML.ExistingFreeTerm(const iTableName,
         iFreeTerm: string; iDataset : TDataset; iLocate : boolean): boolean;
begin
  Result := False; // default
  with qryFindExistingFreeTerm do
  begin
    SQL[1] := iTableName;
    Parameters.ParamByName('short_name').Value := iFreeTerm;
    Open;
    try
      if RecordCount > 0 then
      begin
        if iLocate then
        begin
          { need to remove existing 'new' record, and locate the matching term record }
          iDataset.Delete;
          iDataset.Locate( dmDatabase.GetPrimaryKey(GetDatasetTable(iDataset), False),
                   FieldByName(dmDatabase.GetPrimaryKey(
                   GetDatasetTable(iDataset), False)).AsString, [] );
        end
        else
          { need to copy term data into the existing record }
          CopyRecord( qryFindExistingFreeTerm, iDataset);
        Result := True;
      end;
    finally
      Close;
    end;
  end; //with qryFindExistingFreeTerm
end;


{ Copies a record from an existing record.  It doesn't matter if there are
   fields in the source but not in the dest, as long as all destination fields
   are available in the source.  Raises an exception if the structures are
   incompatible.  Does not post the information }
procedure TdmNBNXML.CopyRecord(iSource, iDest: TDataset);
var
  i : integer;
begin
  for i:=0 to iDest.FieldCount-1 do
  begin
    if iSource.FieldList.IndexOf(iDest.Fields[0].FieldName)=-1 then
      raise EXMLDataError.Create( ResStr_IncompatibleStructure +
                                  GetDatasetTable(iSource));
    if iDest.Fields[i].IsBlob then
    begin
      {Check we have a blob, and copy info across}
      if not iSource.FieldByName(iDest.Fields[i].FieldName).IsBlob then
        raise EXMLDataError.Create( ResStr_IncompatibleStructure +
                                  GetDatasetTable(iSource));
      iDest.Fields[i].Assign(iSource.FieldByName(iDest.Fields[i].FieldName));
    end
    else
    begin
      {Check we don't have a blob, and copy info across}
      if iSource.FieldByName(iDest.Fields[i].FieldName).IsBlob then
        raise EXMLDataError.Create( ResStr_IncompatibleStructure +
                                  GetDatasetTable(iSource));
      iDest.Fields[i].Value :=
                        iSource.FieldByName(iDest.Fields[i].FieldName).Value;
    end; // if isblob
  end; // for
end;


{ Record the output of a termlist key, so when we get to the termlist
     element we know what we have to output }
procedure TdmNBNXML.StoreTerm(iDataset: TDataset);
var
  lTermString : string; // name-value pair for tablename=keyvalue
  lTableName : string;
begin
  lTableName := GetDatasetTable(iDataset);
  lTermString := lTableName + '=' + iDataset.FieldByName
              (dmDatabase.GetPrimaryKey(lTableName, False)).AsString;
  { Remember each term only once }
  if FTermsToOutput.IndexOf(lTermString) = -1 then
    FTermsToOutput.Add(lTermString);
end;

{ As StoreTerm above, where the table name and key are known }
procedure TdmNBNXML.StoreTerm( const iTableName, iKey : string );
var
  lTermString : string; // name-value pair for tablename=keyvalue
begin
  lTermString := iTableName + '=' + iKey;
  { Remember each term only once }
  if FTermsToOutput.IndexOf(lTermString) = -1 then
    FTermsToOutput.Add(lTermString);
end;


{ Returns true if the structure of the supplied dataset is a standard termlist -
     it must contain SHORT_NAME and LONG_NAME fields }
function TdmNBNXML.IsTermTable(const iDataset: TDataset): boolean;
begin
  Result := not ((iDataset.FieldList.IndexOf(TERM_SHORT) = -1) or
                 (iDataset.FieldList.IndexOf(TERM_FULL) = -1));
end;


{ Returns the description of a termlist from the TERM_LIST table.  If none
     is available, then returns blank }
function TdmNBNXML.GetTermlistDescription(const iListName: string): string;
begin
  Result := ''; // default
  with qryTermListDesc do
  begin
    Parameters.ParamByName('table').Value := iListName;
    Open;
    try
      if RecordCount > 0 then
        Result := FieldByName('description').AsString;
    finally
      Close;
    end; // try..finally
  end; // with
end;



{ Returns true if a table name represents an identification table.  Could
    optimise by pre-preparing the identifiers array }
function TdmNBNXML.IsIdentification(const iTable : TJNCCTable): boolean;
var
  lSpecialElement : TSpecialElementItem;
  lIdentifiers : TStringArray;
begin
  Result := False;
  lSpecialElement := FindSpecialElement('IDENTIFICATION', TRANSLATE_TABLE);
  lIdentifiers := ParseSemiColonString(lSpecialElement.Data);
  if lSpecialElement <> nil then
    if NameContainsSpecifier(iTable.TableName, lIdentifiers, False) then
      Result := True;
end;


{ Returns true if an occurrence is unchecked.  Used to filter out these records
        from export files }
function TdmNBNXML.IsUnchecked(iDataset: TDataset): boolean;
begin
  Result := False; // default
  if iDataset.FieldList.IndexOf('CHECKED') <> -1 then
    Result := not iDataset.FieldByName('CHECKED').AsBoolean;
end;


{ Refresh the restricted data lists for each type of item output }
procedure TdmNBNXML.ClearRestrictedData;
var
  i : integer;
begin
  if FRestrictedData<>nil then
  begin
    for i := 0 to FRestrictedData.Count-1 do
      TKeyList(FRestrictedData.Objects[i]).Free;
    FRestrictedData.Clear;
  end;
end;


{ Returns true if the dataset is a determination for a species which belongs
    to a local account }
function TdmNBNXML.IsLocalSpeciesAccount(iDataset: TDataset): boolean;
var
  lTableName : string;
begin
  Result := False; // default
  lTableName := GetDatasetTable(iDataset);
  if IsDeterminationTable(lTableName) then begin
    Result := CheckListItem(iDataset);
  end;
end;


function TdmNBNXML.CheckListItem(iDataset: TDataset): boolean;
var
  lDetType, lTableName : string;
  lKey : TKeyString;
begin
  lTableName := GetDatasetTable(iDataset);
  lDetType := Copy(lTableName, 1, length(lTableName) - length(DETERMINATION) - 1);
  lKey := iDataset.FieldByName(lDetType + LIST_ITEM_KEY).AsString;
  with qryListItem do begin
    SQL.Clear;
    SQL.Add('Select System_Supplied_Data From ' + lDetType + LIST_ITEM);
    SQL.Add('Where ' + lDetType + LIST_ITEM_KEY + ' = ''' + lKey + '''');
    try
      Open;
      First;
      Result := not FieldByName('System_Supplied_Data').AsBoolean;
    finally
      Close;
    end;
  end;
end;

//==============================================================================

procedure TdmNBNXML.SetRestrictListItems(const Value: boolean);
begin
  FRestrictListItems := Value;
end;

//==============================================================================
{ Takes a record identifier.  Returns a query filtered to the identified
     records.
     Raises an exception if the record is not found }
function TdmNBNXML.GetQueryObject( const iRecord : TRecordIdentifier ): TDataset;
var
  lKeyField : string;
  lQuery : TJNCCQuery;
begin
  try
    { If there is no specified field to filter on, we use the primary key }
    if iRecord.KeyField = '' then
      lKeyField := dmDatabase.GetPrimaryKey( iRecord.TableName, False )
    else
      lKeyField := iRecord.KeyField;
    if FQueryPool.Count > 0 then begin  // Look for a query in the pool
      // Return query and delete it from the pool
      lQuery := TJNCCQuery(FQueryPool[FQueryPool.Count-1]);
      FQueryPool.Delete(FQueryPool.Count-1);
    end else begin
      // We need to create a query since there aren't enough to go around
      lQuery := TJnccQuery.Create(nil);
      dmDatabase.SetDatabaseLocal([lQuery]);
    end;
    if lQuery <> nil then
      { Set up the query }
      with lQuery do begin
        ParseSQL := false;
        SQL.Clear;
        SQL.Add('select * from ' + iRecord.TableName);
        SQL.Add(' where ' + lKeyField + '=''' + iRecord.Key1 + '''');
        Open;
        {$IFDEF DEBUG}
        FSQLRun.Add(SQL.Text + ' - ' + IntToStr(lQuery.RecordCount));
        {$ENDIF}
      end; // with Result
      Result := lQuery;
  except
    on E:Exception do // just add some extra info so we know where it went wrong
      raise EXMLDataError.Create(ResStr_CreateRecord, E);
  end; // try..except
end; // GetQueryObject

//==============================================================================
{ Adds query to pool if fewer than maximum number are stored, otherwise frees
  the query }
procedure TdmNBNXML.ReleaseQueryObject(iQuery: TDataset);
begin
  iQuery.Close;
  if FQueryPool.Count >= 10 then
    iQuery.Free // Keep a maximum of 10 queries in the pool
  else
    FQueryPool.Add(iQuery); // Store query in pool
end; // ReleaseQueryObject



end.
