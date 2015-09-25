//------------------------------------------------------------------------
//  Author:    Stuart Ball
//  Date:      October 2006
//  Version:   Recorder 6
//  Purpose:   Import data dumped out of Recorder 3
//
// The Bulk move facility in Recorder 3 exports data  for each Recorder
// 2002/6 table as a CSV file. This addin reads that data into a temporary
// Access database, zips it and passes it to the CheckDatabase
// interface of Recorder 6.
//
// It is implimented as an Import Filter. Data is imported via the normal
// Tools - Import command in Recorder 6 and the file that is targeted is
// the control file written by Bulk Move. This has the extension ".exp"
// and is in INI file format.
//
//------------------------------------------------------------------------
//  Modification history
//------------------------------------------------------------------------
// Date   Initials Change
//------------------------------------------------------------------------
//  May 07  SGB    Added fixes for taxon/biotope determination date earlier
//                 than the date of the record. The SAMPLE date is used in
//                 such case
//  Jun 07  SGB    Progress form prettied up and now uses an ImageList
//                 instead of a series of TImages. The status images
//                 are written to the form canvas.
//  Jun 07  SGB    Added fix for Channel Island grid refs in WA/WV OSGB
//                 format. Such grid refs are translated into UTM refs and
//                 the row updated (including Lat/Long coordinates).
//  09/01/08 SGB   Recompiled using DAO 3.6 to use an Access 2000 mdb as
//                 the temporary import file. This should increase capacity
//                 and dosen't require DAO 3.51 to be installed.
//------------------------------------------------------------------------
unit Rec3Importer_v6;

interface

uses
  ComObj, ActiveX, Recorder_3_Import_v6_TLB, Windows, Recorder2000_TLB,
  Registry, INIFiles, AddinClasses, Classes, SysUtils, Dialogs, ImportError,
  SpatialRefFuncs, GenFuncs, VersionInfo, Variants, StdVcl, DB, ADODB,
  DAO36_TLB, Math;

type
  TRec3Importer = class(TAutoObject, IRec3Importer, IRecorderAddin, IFilter,
                        IImportFilter)
  private
    FRecorder2000: IRecorder2000; // interface to main app

    FTempPath: string;    // Path to temp database
    FTempMDBName: string; // Name of temp database

    // DAO stuff on temp database (Access 97)
    FTempdb: Database;
    FTempQuery: _QueryDef;
    FTempRecSet: RecordSet;

    // ADO stuff on NBN database (SQL Server)
    FConnection: TADOConnection;
    FTable: TADOTable;
    FQuery: TAdoQuery;

    FDOSPath: string;            // Path from which csv files will be read
    FControl: TINIFile;          // Control file written by Recorder 3 - INI format
    FInsertedTables: TStrings;   // List of tables added to temp database
    FLastKey: TStrings;          // Will hold last key info from NBNdata
    FErrors: TErrorList;         // List or errors that have been found
    FSiteID: string;             // Rec2K SiteID - needed to form new keys
    FSaveMDIContainerPos: TRect; // Original position of MDI container read from Registry
    FMainDBCount: TStrings;
    procedure UpdateChannelIslandsGrid(const sTable, sPrefix, sKey,
      sGrid: string);
    function ChannelIslandsGrid(const sText: string): string;
    procedure FixChannelIslandGrids(IForm: IImportProgressX);      // Number of rows in tables in the main database

    procedure Fix_DetDate(IForm: IImportProgressX);
    procedure CopyTableStructure(const aTableName: string);
    procedure CreateNewDB(const dbName: string);
    procedure DoImport(const iSourceFile: TFileName);
    function DoInsert(const aTable, aLine, aColList: string): boolean;
    procedure GetLastKeys;
    function GetNextKey(const aTable: string): string;
    function GetPrimaryIndex(const aTableName: string): string;
    function GetRelatedFields(const aTableName: string;
      RelatedList: Tstrings): Longint;
    function GetTempDir: string;
    procedure ImportTable(const aTableName: string; IForm: IImportProgressX);
    procedure ImportTables(IForm: IImportProgressX);
    procedure IncrementChar(var Ch: string; var Carry: boolean);
    function IncrementString(const InStr: string): string;
    procedure MakeTempdb;
    procedure MakeTempKeysTable;
    function NeedToUpdateKeys(const aTable: string): boolean;
    procedure ReportError(const aType: TErrorType; const aTable, aKey,
      aMessage: string);
    function UpdateForeignKey(const aTable, aField: string): integer;
    procedure UpdateKeys(IForm: IImportProgressX);
    procedure WriteTempKeys(const nStart, nRows: Integer;
      const aTable: string; var n: longint; IForm: IImportProgressX);
    function GetTitle: string;
    function ReadMDIContainerPos(aPos: TRect): boolean;
    procedure WriteMDIContainerPos(aPos: TRect);
    function RemoveStr(var InStr: String; const Delim: string): String;
    procedure GetMainDBRowCount;
    function ImportDone: boolean;
    function ConvertHTMLtoRTF(aLine: string): string;
    procedure WriteTempKeysNonSequential(const aTable: string;
      var n: Integer; IForm: IImportProgressX);
    procedure AddRelations(const aTable: string; RelatedList: Tstrings);
    procedure FixGrids(IForm: IImportProgressX);
    function IsFieldPresent(const aTable, aCol: string): boolean;
    procedure DeleteDuplicateBiotopeOccurrences(IForm: IImportProgressX);
    procedure LoadNextBlock(iFileStream: TFileStream; iStrings: TStrings);
    function Get_SiteID: string;
    function GetQuery: TADOQuery;
    function GetRecorder2000: IRecorder2000;
    function GetTableDef(const aTableName: string): boolean;
    function GetConnection: TADOConnection;
    function GetTable: TADOTable;
    function TranslateType(const aType: TFieldType; const aName: string): DataTypeEnum;
  protected
    { IRecorderAddin }
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    { IFilter }
    function Get_DefaultFileExtension: WideString; safecall;
    { IImportFilter }
    function ImportFile(const iSourceFile: WideString): WordBool; safecall;
    function Get_LastImportError: WideString; safecall;
  public
    destructor Destroy; override;
    property Recorder2000: IRecorder2000 read GetRecorder2000;
    property ADOQuery: TADOQuery read GetQuery;
    property ADOConnection: TADOConnection read GetConnection;
    property ADOTable: TADOTable read GetTable;
  end;

implementation

uses ComServ;

const
     NBN_DIR = 'NBN Import\';   // Suffix for temp database path
     TEMP_FILE_NAME = '~temp';  // base name for temp datbase
     HTML = '<HTML>';           // Starting tag to trigger HTML to RTF conversion

     FeedBackFormHt = 338;
     FeedBackFormWd = 260;

     {Used for incrementing the running string for new keys}
     Charset = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

     {Registry key to find add-ins}
     BaseKey = 'Software\Dorset Software\Recorder 6\Installed Addins';


{ TRec3Importer }

//=========================================================================
//           I N T E R F A C E    I M P L E M E N T A T I O N S
//=========================================================================

//-------------------------------------------------------------------------
//  This is the extension that will be used by the Import dialog.
//  Use ".EXP" to pick up the control file for the export set
//-------------------------------------------------------------------------
function TRec3Importer.Get_DefaultFileExtension: WideString;
begin
     Result := 'exp';
end;

//-------------------------------------------------------------------------
// Description which shows in the Addin install window
//-------------------------------------------------------------------------
function TRec3Importer.Get_Description: WideString;
begin
     Result := 'Import set of CSV files exported from Recorder v3.45 to Recorder 6. ' +
               AddinVersion('Recorder_3_import_v6.ocx');
end;

//-------------------------------------------------------------------------
//  The icon for this addon in the Addin Install window
//-------------------------------------------------------------------------
function TRec3Importer.Get_ImageFileName: WideString;
begin
    Result := ''; // leave it to default for the moment
end;

//-------------------------------------------------------------------------
// If we had errors Recorder 2000 reads this property to get
// message to display to user
//-------------------------------------------------------------------------
function TRec3Importer.Get_LastImportError: WideString;
begin
     Result := '';
end;

//-------------------------------------------------------------------------
//  The name that will appear in the list in Addin Install window
//  and also in the Import Dialog drop down list
//-------------------------------------------------------------------------
function TRec3Importer.Get_Name: WideString;
begin
     Result := 'Recorder 3 import';
end;

//-------------------------------------------------------------------------
// This is the main routine which does the actual import
//-------------------------------------------------------------------------
function TRec3Importer.ImportFile(const iSourceFile: WideString): WordBool;
begin
     FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     DoImport(iSourceFile);
     Result := True;
end;

//-------------------------------------------------------------------------
// This is called when the addin is installed
//-------------------------------------------------------------------------
procedure TRec3Importer.Install(const iInstalledFilePath: WideString);
begin
    ShowMessage('Recorder 3 import filter for Recorder 6 installed.' +
                AddinVersion('Recorder_3_import_v6.ocx'));
end;

//--------------------------------------------------------------------------------------
// Property implimentation
//--------------------------------------------------------------------------------------
function TRec3Importer.GetRecorder2000: IRecorder2000;
begin
     {Get Recorder2000 interface}
     If FRecorder2000 = nil then
        FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     Result := FRecorder2000;
end;

//--------------------------------------------------------------------------------------
// Opens a connection to the SQL Server database.
//--------------------------------------------------------------------------------------
function TRec3Importer.GetConnection: TADOConnection;
begin
  if FConnection = nil then
  begin
    FConnection := TADOConnection.Create(nil);
    FConnection.LoginPrompt := False;
    FConnection.ConnectionString := Recorder2000.ConnectionString;
    FConnection.Open;
    Recorder2000.SetApplicationSecurity(FConnection.ConnectionObject);
  end;
  Result := FConnection;
end;

//--------------------------------------------------------------------------------------
// Creates a query linked to the SQL Database. This is used for 3 purposes:
// 1. Access SiteID from the SETTINGS table
// 2. Access the LAST_KEY table
// 3. Access the DATABASE_RELATIONSHIP table
//--------------------------------------------------------------------------------------
function TRec3Importer.GetQuery: TADOQuery;
begin
  If FQuery = nil then
  begin
    FQuery := TADOQuery.Create(nil);
    FQuery.Connection := ADOConnection;
  end;
  Result := FQuery;
end;

//--------------------------------------------------------------------------------------
// Create a table linked to the SQL Database. This is used to access table
// definitions to create tables in the temp database
//--------------------------------------------------------------------------------------
function TRec3Importer.GetTable: TADOTable;
begin
  if FTable = nil then
  begin
    FTable := TADOTable.Create(nil);
    FTable.Connection := ADOConnection;
  end;
  Result := FTable;
end;

//--------------------------------------------------------------------------------------
// Clean up when Addin closes. Make sure SQL Server access objects are properly
// freed.
//--------------------------------------------------------------------------------------
destructor TRec3Importer.Destroy;
begin
     if FTable <> nil then
     begin
       FTable.Close;
       FTable.Free;
     end;
     If FQuery <> nil then
     begin
        FQuery.Close;
        FQuery.Free;
     end;
     If FConnection <> nil then
     begin
        FConnection.Close;
        FConnection.Free;
     end;
  inherited;
end;

//=========================================================================
//                M A I N    I M P L E M E N T A T I O N
//=========================================================================

//------------------------------------------------------------------------
// Create a temporary database into which data is initially copied from csv
// files. It can then be checked before copying into main database.
// The location and naming of the temporary file is based on Dorset
// Software's code.
//------------------------------------------------------------------------
Procedure TRec3Importer.MakeTempdb;
var lName: string;
    l: integer;
begin
     FTempPath := GetTempDir + NBN_DIR;
     CreateDir(FTempPath);
     l := 0;
     Repeat
           lName := FTempPath + TEMP_FILE_NAME + IntToStr(l) + '.mdb';
           inc(l);
     until not FileExists(lName);
     CreateNewDB(lName);
     FTempMDBName := lName;
end;

//------------------------------------------------------------------------
// Create the temporary mdb file. Based on Diamond Access demos
//------------------------------------------------------------------------
Procedure TRec3Importer.CreateNewDB(const dbName: string);
var dbEng: _DBEngine;
begin
     dbEng := CoDBEngine.Create;
     FTempdb := dbEng.CreateDatabase(dbName, ';LANGID=0x0409;CP=1252;COUNTRY=0', dbVersion40);
     FTempQuery := FTempdb.CreateQueryDef('', emptyParam);
end;

//--------------------------------------------------------------------------------------
// Get SiteID from the SETTING table in SQL Server database
//--------------------------------------------------------------------------------------
Function TRec3Importer.Get_SiteID: string;
begin
  Result := '';
  ADOQuery.SQL.Clear;
  ADOQuery.SQL.Add('SELECT DATA FROM SETTING WHERE (NAME = ''SiteID'')');
  try
    ADOQuery.Open;
    If ADOQuery.RecordCount > 0 then
       Result := ADOQuery.FieldByName('DATA').AsString;
  finally
    ADOQuery.Close;
  end;
end;

//------------------------------------------------------------------------
// Create a table in the temporary database based on the table definition
// from the main database. The field name, type, size, default and Required
// settings are copied and the Primary index(es) set up.
// Note that it assumes that there are no more than two fields involved
// in the primary index (which is OK for NBNDATA).
//------------------------------------------------------------------------
Procedure TRec3Importer.CopyTableStructure(const aTableName: string);
var  lTableToCopy: TADOTable;
     lTableDef: TableDef;
     lIndexDef: Index;
     lFieldDef: Field;
     i: integer;
     lFieldType: TFieldType;
     lIndexNames: string;
     lIndexName: string;
     lIndexSize1, lIndexSize2: integer;
begin
     lIndexSize1 := 0; // satisfy the compiler
     lIndexSize2 := 0; // so we don't get a warning

     if GetTableDef(aTableName) then
     begin
       lTableToCopy := ADOTable;
       lIndexNames := GetPrimaryIndex(aTableName); // Name(s) of primary index field(s)
       lTableDef := FTempdb.CreateTableDef(aTableName, 0, '', ''); // Create table in temp database

       {Create the fields}
       for i:=0 to lTableToCopy.Fields.Count-1 do
          // we don't want to create CUSTODIAN or OUTPUT_GROUP tables
          // These were added to Recorder 6 and will be populated on import
          if not((CompareText('CUSTODIAN',lTableToCopy.Fields[i].FieldName) = 0)
              or (CompareText('OUTPUT_GROUP_KEY',lTableToCopy.Fields[i].FieldName) = 0)) then
          begin
              lFieldType := lTableToCopy.Fields[i].DataType;
              lFieldDef  := lTableDef.CreateField(lTableToCopy.Fields[i].FieldName,
                                                  TranslateType(lFieldType, lTableToCopy.Fields[i].FieldName),
                                                  lTableToCopy.Fields[i].size);
              //lFieldDef.DefaultValue := lTableToCopy.Fields[i].DefaultValue;
              lFieldDef.Required     := lTableToCopy.Fields[i].Required;
              lTableDef.Fields.Append(lFieldDef); // Add the field
              {Is this field involved in the Primary index?
               If so, get its size for use when the index is created}
              If Pos(lTableToCopy.Fields[i].name, lIndexNames) > 0 then
              begin
                 If Pos(lTableToCopy.Fields[i].name, lIndexNames) > 1 then
                    lIndexSize2 := lTableToCopy.Fields[i].size
                 else
                    lIndexSize1 := lTableToCopy.Fields[i].size
              end;
          end; // for loop

       {Set up Primary index}
       If lIndexNames <> '' then
       begin
            i := Pos(';', lIndexNames); // Do we have two field names? (semi-colon delinated)
            If i=0 then
               lIndexName := lIndexNames // first field name
            else
               lIndexName := Copy(lIndexNames, 1, i-1); // second

            {Create Primary Index}
            lIndexDef := lTableDef.CreateIndex('IDX_PRIMARY');
            lIndexDef.Primary := True;
            lIndexDef.Fields.Append(lIndexDef.CreateField(lIndexName, dbText, lIndexSize1));
            if i>0 then // add second field to Primary index
            begin
                 lIndexName := Copy(lIndexNames, i+1, Length(lIndexNames)-i);
                 lIndexDef.Fields.Append(lIndexDef.CreateField(lIndexName, dbText, lIndexSize2));
            end;
            lTableDef.Indexes.Append(lIndexDef); // add index to TableDef
       end; // set up primary index
       FTempdb.TableDefs.Append(lTableDef); // add TableDef to temp database
     end;
end;

//--------------------------------------------------------------------------------------
// Connect the ADOTable object to given table
// Returns TRUE if we got the table OK
//--------------------------------------------------------------------------------------
function TRec3Importer.GetTableDef(const aTableName: string): boolean;
begin
  ADOTable.Close;
  ADOTable.TableName := aTableName;
  ADOTable.Open;
  Result := ADOTable.Active;
end;

//-------------------------------------------------------------------------
// The temporary database will be created in a subdirectory off the Windows
// temp directory.  Code from Dorset Software.
//-------------------------------------------------------------------------
Function TRec3Importer.GetTempDir: string;
var
  bufLen:    Integer;
  tempPath:  AnsiString;
begin

  // get required buffer length
  bufLen := GetTempPath(0, PChar(tempPath));

  // read path
  if bufLen > 0 then begin
    SetLength(tempPath, bufLen);
    bufLen := GetTempPath(bufLen, PChar(tempPath));
  end;  // if bufLen > 0

  // exception in case of failure
  if bufLen = 0 then
     ShowMessage('Cannot get path for temp database');

  Result := Copy(tempPath, 1, bufLen);  // removes zero terminator

end;  // gfnGetTempPath

//------------------------------------------------------------------------
// Get the names of the fields involved in a table's Primary index from the
// main database. If more than one field is involved, they are returned
// as a semi-colon delinated list.
//------------------------------------------------------------------------
Function TRec3Importer.GetPrimaryIndex(const aTableName: string): string;
var i: integer;
begin
  Result := '';
  if GetTableDef(aTableName) then
  begin
    if ADOTable.IndexDefs.Count > 0 then
      for i:=0 to ADOTable.IndexDefs.Count-1 do
        If ixPrimary in ADOTable.IndexDefs[i].Options then
        begin
          // returns names of indexes in semicolon delim list
          Result := ADOTable.IndexDefs[i].Fields;
          Break;
        end;
  end
  else
    ShowMessage(aTableName + ' not found.');
end;

//------------------------------------------------------------------------
// Get the name of the controlling file (.exp) which is in ini file format.
// Get the table names (in section [Tables] into a string list
// Loop through each table (sections named after the table to which they
// apply) and import the data for that table into the temporary database
//------------------------------------------------------------------------
Procedure TRec3Importer.ImportTables(IForm: IImportProgressX);
var i,n : integer;
    TableList: TStrings;
    aName: string;
begin
     TableList := TStringList.Create;
     try
        FControl.ReadSectionValues('Tables', TableList);
        If TableList.IndexOfName('NoTables') >=0 then // get number of tables
        begin
             {the number of tables is storerd as NoTables=n}
             n := StrToInt(TableList.Values['NoTables']);
             iForm.ProgressTablesMax := n;
             {then the tables are listed as
                   Table1=name
                   Table2=name
                   etc.}
             if n>0 then
                for i:=1 to n do
                begin
                     aName := 'Table' + IntToStr(i);
                     If TableList.IndexOfName(aName) >= 0 then
                     begin
                          iForm.ProgressTables := i;
                          ImportTable(TableList.Values[aName], iForm); // Import this table
                     end;
                end; // for
             iForm.ProgressTables := 0;
        end; // IndexOf
     finally
         TableList.Free;
     end; // try .. finally
end;

//------------------------------------------------------------------------
// Copy the data from a csv file into a table in the temporary database
// JVB 09/04/2002 Now uses a file stream, so that larger csv files can be
// handled.  Procedure does have a 2GB limit though (limit of longint).
//------------------------------------------------------------------------
Procedure TRec3Importer.ImportTable(const aTableName: string; IForm: IImportProgressX);
var
  theDetails, theFile: TStrings;
  theLine: string;
  i, lRowsInserted: longint;
  lFileStream : TFileStream;    // read from file as a stream
  lProgress : integer;
begin
  lRowsInserted := 0;
  theDetails := TStringList.Create;
  theFile := TStringList.Create;
  try
    FControl.ReadSectionValues(aTableName, theDetails);
    // try to open the file
    try
      lFileStream := TFileStream.Create(
                 FDOSPath + theDetails.Values['DOSfileName'], fmOpenRead);
    except
      ShowMessage('Cannot open ' + theDetails.Values['DOSfileName']);
      Exit;
    end;

    try
      iForm.ProgressMax := lFileStream.Size;
      iForm.CurrentTableName := aTableName;

      {Create the table in the temporary database}
      CopyTableStructure(aTableName);
      while lFileStream.Position < lFileStream.Size do
      begin
          FTempDb.BeginTrans;
          lProgress := lFileStream.Position;
          LoadNextBlock( lFileStream, theFile );
          For i:=0 to theFile.Count-1 do
          begin
              theLine := theFile[i];
              If Pos(HTML, theLine) > 0 then
                 theLine := ConvertHTMLtoRTF(theLine);
              If DoInsert(aTableName, theLine, theDetails.Values['Columns']) then
              begin
                 lProgress := lProgress + Length(theLine) + 2; // +  2 for CRLF
                 iForm.Progress := lProgress;
                 inc(lRowsInserted)
              end;
          end; // for
          FTempDb.CommitTrans(dbForceOSFlush);
      end; // while
      iForm.Progress := 0;
    finally
      lFileStream.Free;
    end;

  finally
    theDetails.Free;
    theFile.Free;
  end;

  If lRowsInserted > 0 then
    FInsertedTables.Add(aTableName + '=' + IntToStr(lRowsInserted));

  iForm.CurrentTableName := '';
  iForm.NoErrors := FErrors.Count;
end;

//--------------------------------------------------------------------------------------
// Translate SQL Server field type definitions to suitable Access 97 types
// Note that VAGUE_DATE_START and VAGUE_DATE_END fields in SQL Server
// are held as numbers to get round SQL Server's date limitations. We want these
// to be DateTime fields in the Access temporary database, so we test on the
// field name.
//--------------------------------------------------------------------------------------
function TRec3Importer.TranslateType(const aType: TFieldType; const aName: string): DataTypeEnum;
var t: integer;
    sWork: string;
begin
  // we need to take special measures for VAGUE_DATE_START and _END fields
  sWork := UpperCase(aName);
  t := Pos('DATE_START', sWork);
  if t>0 then
  begin
    Result := dbDate;
  end
  else
  begin
    t := Pos('DATE_END', sWork);
    if t>0 then
    begin
      Result := dbDate;
    end
    else
    // not a VAGUE_DATE field
    begin
      case aType of
        ftString:     Result := dbText;
        ftSmallInt:   Result := dbInteger;
        ftWord:       Result := dbInteger;
        ftInteger:    Result := dbLong;
        ftBoolean:    Result := dbBoolean;
        ftDateTime:   Result := dbDate;
        ftMemo:       Result := dbMemo;
        ftFloat:      Result := dbDouble;
        ftBlob:       Result := dbLongBinary;
      else
        t := ord(aType);
        ShowMessage(Format('Type %d not recognised', [t]));
        Result := dbText;
      end;
    end;
  end;

end;

//------------------------------------------------------------------------
//  JVB 09/04/2002
//  Load a block of approx 1MB from the file into a string list.
//------------------------------------------------------------------------
procedure TRec3Importer.LoadNextBlock( iFileStream : TFileStream; iStrings : TStrings );
const
  TARGET_BLOCK_SIZE = 10000000; // load approx 1 meg of file at a time
  CARRIAGE_RETURN = #13;
var
  lBlockStart : longint;       // start of next block in filestream
  lActualBlockSize : longint;   // approx 1 meg - but aligned to next carriage return
  lBytesRead : integer;
  lCurrentChar : char;          // used to search for Carriage Return
  lBlockStream : TMemoryStream; // 1MB block read from file into memory
begin
  lBlockStart := iFileStream.Position;
  if iFileStream.Position + TARGET_BLOCK_SIZE >= iFileStream.Size then
    iStrings.LoadFromStream(iFileStream) // load entire remaining file
  else begin
    // load 1 block
    lBlockStream := TMemoryStream.Create;
    try
      iFileStream.Position := iFileStream.Position + TARGET_BLOCK_SIZE;
      // round block size up to next Carriage return
      repeat
        lBytesRead := iFileStream.Read( lCurrentChar, 1 );
      until (lCurrentChar = CARRIAGE_RETURN) or (lBytesRead = 0);
      iFileStream.Read( lCurrentChar, 1 ); // move past Line Feed
      lActualBlockSize := iFileStream.Position - lBlockStart;
      // wind back to start of current block
      iFileStream.Position := lBlockStart;
      // lBlockStream is just an intermediary location for the memory, before
      // loading the string list
      lBlockStream.CopyFrom(iFileStream, lActualBlockSize);
      lBlockStream.Position := 0;
      // move block into string list
      iStrings.LoadFromStream(lBlockStream);
    finally
      lBlockStream.Free;
    end;
  end;
end;

//------------------------------------------------------------------------
// Insert a single row into a table in the temporary database
//------------------------------------------------------------------------
Function TRec3Importer.DoInsert(const aTable, aLine, aColList: string): boolean;
var sSQL: string;
begin
     Result := False;
     sSQL:= 'INSERT INTO ' + widestring(aTable)
                       + ' (' + widestring(aColList) + ')'
                       + ' VALUES'
                       + ' (' + widestring(aLine) + ');';
     FTempQuery.SQL := sSQL;
     try
        FTempQuery.Execute(dbFailOnError);
        Result := (FTempQuery.RecordsAffected > 0);
     except
        on E:EOleException do
             ReportError(errInsert, aTable, sSQL, E.Message);
     end;
end;

//------------------------------------------------------------------------
// Get the tables and fields to which the specified table is related
// from the DATABASE_RELATIONSHIP table. Returnes the number
// of rows in the related table(s);
//------------------------------------------------------------------------
Function TRec3Importer.GetRelatedFields(const aTableName: string; RelatedList: Tstrings): Longint;
var fTable, fField: string;
begin
  Result := 0;
  ADOQuery.SQL.Clear;
  ADOQuery.SQL.Add('SELECT DETAIL_TABLE, DETAIL_FIELD FROM DATABASE_RELATIONSHIP ');
  ADOQuery.SQL.Add(Format('WHERE (MASTER_TABLE = ''%s'');', [aTableName]));
  try
    ADOQuery.Open;
    If ADOQuery.RecordCount > 0 then
    begin
      ADOQuery.First;
      while not ADOQuery.EoF do
      begin
        fTable := ADOQuery.FieldByName('DETAIL_TABLE').AsString;
        fField := ADOQuery.FieldByName('DETAIL_FIELD').AsString;
        {Only worry about foriegn tables that are present
        in the temp database}
        If FInsertedTables.IndexOfName(fTable) >= 0 then
        begin
          // Check the field is in the imported data
          If IsFieldPresent(fTable, fField) then
          begin
            RelatedList.Add(fTable + '=' + fField);
            Result := Result + StrToInt(FInsertedTables.Values[fTable]);
          end
        end;
        ADOQuery.Next;
      end;
    end;
  finally
    ADOQuery.Close;
  end;
end;

//------------------------------------------------------------------------------
// These are relations we cannot get from DATABASE_RELATIONSHIP table
//------------------------------------------------------------------------------
Procedure TRec3Importer.AddRelations(const aTable: string; RelatedList: Tstrings);
var i: integer;
begin
     // entered_by columns
     If CompareText(aTable, 'NAME') = 0 then
     begin
          If FInsertedTables.IndexOfName('INDIVIDUAL') >= 0 then
          begin
               RelatedList.Add('INDIVIDUAL=NAME_KEY');
               GetRelatedFields('INDIVIDUAL', RelatedList);
          end;
          If FInsertedTables.IndexOfName('ORGANISATION') >= 0 then
          begin
               RelatedList.Add('ORGANISATION=NAME_KEY');
               GetRelatedFields('ORGANISATION', RelatedList);
          end;
          For i:= 0 to FInsertedTables.Count-1 do
              If IsFieldPresent(FInsertedTables.Names[i], 'entered_by') then
                 RelatedList.Add(FInsertedTables.Names[i] + '=ENTERED_BY');
     end;

     // location parent column
     If CompareText(aTable, 'LOCATION') = 0 then
     begin
          Repeat
                i := RelatedList.IndexOf('LOCATION_RELATION=LOCATION_KEY');
                if i>=0 then
                   RelatedList.Delete(i);
          Until i < 0;
     end;


     // taxon_list_item
     If CompareText(aTable, 'TAXON_LIST_ITEM') = 0 then
     begin
          RelatedList.Add('TAXON_LIST_ITEM=PARENT');
          RelatedList.Add('TAXON_LIST_ITEM=PREFERRED_NAME');
     end;

     // taxon_version
     If CompareText(aTable, 'TAXON_VERSION') = 0 then
        If FInsertedTables.IndexOfName('TAXON_COMMON_NAME') >= 0 then
           RelatedList.Add('TAXON_COMMON_NAME=TAXON_VERSION_KEY');

     // taxon_occurrence_relation
     If CompareText(aTable, 'TAXON_OCCURRENCE') = 0 then
     begin
          Repeat
                i := RelatedList.IndexOf('TAXON_OCCURRENCE_RELATION=TAXON_OCCURRENCE_KEY');
                if i>=0 then
                   RelatedList.Delete(i);
          Until i < 0;
        If FInsertedTables.IndexOfName('TAXON_OCCURRENCE_RELATION') >= 0 then
        begin
             RelatedList.Add('TAXON_OCCURRENCE_RELATION=TAXON_OCCURRENCE_KEY_1');
             RelatedList.Add('TAXON_OCCURRENCE_RELATION=TAXON_OCCURRENCE_KEY_2');
        end;
     end;

     // Indirect relation from SOURCE
     If CompareText(aTable, 'SOURCE') = 0 then
     begin
          // Add indirect references
          GetRelatedFields('REFERENCE', RelatedList);
          If FInsertedTables.IndexOfName('TAXON_FACT') >= 0 then
             RelatedList.Add('TAXON_FACT=SOURCE_KEY');
     end;
end;

//------------------------------------------------------------------------------
// Check a particular Column in a Particular table is present in the data set
// to import.
//------------------------------------------------------------------------------
Function TRec3Importer.IsFieldPresent(const aTable, aCol: string): boolean;
var ldetails: TStrings;
    lCols: string;
begin
     lDetails := TStringList.Create;
     try
        FControl.ReadSectionValues(aTable, lDetails);
        If lDetails.Count > 0 then
        begin
             lCols := lDetails.Values['Columns'];
             Result := (Pos(LowerCase(aCol), lCols) > 0);
        end else Result := False;
     finally
        lDetails.Free;
     end;
end;

//------------------------------------------------------------------------
// If Local keys have been used for a table they need to be replaced with
// proper NBN keys.
// Loop through the Inseterd Tables. If they have a single Primary key field
// Check whether it is a 16 character string.
// If not, it needs replacing - update it to the next available key for that
// table from nbndata LAST_KEY.
// Locate any related fields and similarly update them
//------------------------------------------------------------------------
Procedure TRec3Importer.UpdateKeys(IForm: IImportProgressX);
var i, r: integer;
    iRows, iStart, n: Longint;
    sRows, sStart: string;
    theDetails, RelatedList: TStrings;
    aTable, aColumn: string;
begin
     iForm.ProgressTables := 0;
     iForm.ProgressTablesMax := FInsertedTables.Count;
     GetLastKeys;
     If FInsertedTables.Count > 0 then
     begin
        RelatedList := TStringList.Create;
        theDetails := TStringList.Create;
        try
           For i:=0 to FInsertedTables.Count-1 do
           begin
                aTable := FInsertedTables.Names[i];
                If NeedToUpdateKeys(aTable) then
                begin
                     iForm.CurrentTableName := aTable;
                     iForm.ProgressTables := i;
                     n := StrToInt(FInsertedTables.Values[aTable]) * 2; // because we will write the keys then update them
                     RelatedList.Clear;
                     n :=  n + GetRelatedFields(aTable, RelatedList);
                     iForm.ProgressMax := n;
                     n := 0;
                     MakeTempKeysTable;
                     // read number or rows and start number which may be comma delim list
                     theDetails.Clear;
                     FControl.ReadSectionValues(aTable, theDetails);
                     If theDetails.IndexOfName('StartLocalKey') >= 0 then // check we have start row number
                     begin
                          sRows  := theDetails.Values['NoRows'];
                          sStart := theDetails.Values['StartLocalKey'];
                          iRows := 0;
                          iStart := 0;
                          While Length(sRows) > 0 do
                          begin
                               try
                                  iRows  := StrToInt(RemoveStr(sRows, ','));
                                  iStart := StrToInt(RemoveStr(sStart, ','));
                               except
                                  on EConvertError do
                                     ShowMessage('UpdateKeys: unable to convert string');
                               end;
                               WriteTempKeys(iStart, iRows, aTable, n, iForm);
                          end;
                     end else
                         // if we don't have this info - we will have to read the table
                         WriteTempKeysNonSequential(aTable, n, iForm);
                     AddRelations(aTable, RelatedList);
                     {Update the primary table}
                     n := n + UpdateForeignKey(aTable, GetPrimaryIndex(aTable));
                     iForm.Progress := n;
                     {Update each of the related tables}
                     If RelatedList.Count > 0 then
                        For r:=0 to RelatedList.Count-1 do
                        begin
                             aTable := RelatedList.Names[r];
                             aColumn := Copy(RelatedList[r], Length(aTable)+2, 999);
                             n :=  n + UpdateForeignKey(aTable, aColumn);
                             iForm.Progress := n;
                        end; // For related fields
                     FTempdb.TableDefs.Delete('TempKeys'); // Delete the temp key table
                     iForm.Progress := 0;
                end; // need to update
                iForm.NoErrors := FErrors.Count;
           end; // for tables
        finally
           RelatedList.Free;
           theDetails.Free;
        end; // try .. finally
     end; // If Count
     {pbarTables.position := 0;}
end;

//------------------------------------------------------------------------
// InStr consists of a series of substrings separated by a comma (e.g. 23,45,67).
// Return the first substring (23) and remove it from InStr (e.g. InStr is
// returned as 45,67). If InStr does not contain a comma the whole of InStr
// will be returned as the result and InStr will be set to empty.
//------------------------------------------------------------------------
Function TRec3Importer.RemoveStr(var InStr: String; const Delim: string): String;
var ld: integer;
    p:  Longint;
    Work: String;
begin
    ld := Length(Delim);
    If ld > 0 then
    begin
         Work:=Trim(InStr);
         If CompareText(Copy(Work, Length(Work)-ld, ld),Delim) <> 0 then
            Work:=Concat(Work, Delim);
         p:=Pos(Delim,Work);
         If p > 1 then
            Result:=Trim(Copy(Work, 1, p-1))
         else Result:='';
         p:=Pos(Delim, InStr);
         If (p > 0) and (p < Length(InStr)) then
            InStr:=Copy(InStr, p + ld, Length(InStr)-p)
         else InStr:='';
    end else Result := '';
end;

//------------------------------------------------------------------------
// Create a table in the temp database in which to put records which relate
// the temporary running keys to the proper NBN keys. This will be used in
// UPDATE queries to re-assign temp keys.
//------------------------------------------------------------------------
Procedure TRec3Importer.MakeTempKeysTable;
var  lTableDef: TableDef;
     lIndexDef: Index;
     lFieldDef: Field;
begin
     lTableDef := FTempdb.CreateTableDef('TempKeys', 0, '', ''); // Create table in temp database

     {Create the fields}
     lFieldDef  := lTableDef.CreateField('oldKey',dbText, 16);
     lTableDef.Fields.Append(lFieldDef); // Add the field

     lFieldDef  := lTableDef.CreateField('newKey',dbText, 16);
     lTableDef.Fields.Append(lFieldDef); // Add the field

     {Create Primary Index}
     lIndexDef := lTableDef.CreateIndex('IDX_PRIMARY');
     lIndexDef.Primary := True;
     lIndexDef.Fields.Append(lIndexDef.CreateField('oldKey', dbText, 16));
     lTableDef.Indexes.Append(lIndexDef); // add index to TableDef
     FTempdb.TableDefs.Append(lTableDef); // add TableDef to temp database
end;

//------------------------------------------------------------------------
// We can use the fact that we know the temporary running keys were assigned
// consecutively when the data was written by Recorder 3. All we need to
// know is the start number and number of rows (which are available from the
// ".exp" file) and we can generate a table which contains these numbers in
// "oldKey" and the newly assigned NBN key in "newKey".
//------------------------------------------------------------------------
Procedure TRec3Importer.WriteTempKeys(const nStart, nRows: Longint; const aTable: string;
                                      var n: longint; IForm: IImportProgressX);
var i: Longint;
    oldKey, newKey: string;
begin
     If nRows > 0 then
     begin
        FTempDb.BeginTrans;
        For i := nStart to (nStart + nRows - 1) do
        begin
           oldKey := IntToStr(i);
           newKey := GetNextKey(aTable);
           FTempQuery.SQL := 'INSERT INTO TempKeys'
                             + ' (oldKey, newKey)'
                             + ' VALUES'
                             + ' ("' + oldKey + '","' + newKey + '");';
           n := n+1;
           iForm.Progress := n;
           try
              FTempQuery.Execute(dbFailOnError);
           except
              on E:EOleException do
              begin
                 FTempDb.Rollback;
                 ReportError(errKeyUpdate, 'TempKeys', oldKey, E.Message);
                 Exit;
              end;
           end; // try .. except
        end; // for
        FTempDb.CommitTrans(dbForceOSFlush);
     end;
end;

//--------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------
Procedure TRec3Importer.WriteTempKeysNonSequential(const aTable: string;
                                      var n: longint; IForm: IImportProgressX);
var oldKey, newKey: string;
    lRecSet: RecordSet;
begin
    FTempDb.BeginTrans;
    lRecSet := FTempdb.OpenRecordSet(aTable, dbOpenTable, emptyParam, emptyParam);
    try
       If lRecSet.RecordCount > 0 then
       begin
            lRecSet.MoveFirst;
            While Not lRecSet.EoF do
            begin
                 oldKey := lRecSet.Fields[0].Value;
                 newKey := GetNextKey(aTable);
                 FTempQuery.SQL := 'INSERT INTO TempKeys'
                                   + ' (oldKey, newKey)'
                                   + ' VALUES'
                                   + ' ("' + oldKey + '","' + newKey + '");';
                 n := n+1;
                 iForm.Progress := n;
                 try
                    FTempQuery.Execute(dbFailOnError);
                 except
                    on E:EOleException do
                    begin
                         FTempDb.Rollback;
                         ReportError(errKeyUpdate, 'TempKeys', oldKey, E.Message);
                         Exit;
                    end;
                 end; // try .. except
                 lRecSet.MoveNext;
            end;
       end;
    finally
       lRecSet.Close;
    end;
    FTempDb.CommitTrans(dbForceOSFlush);
end;

//------------------------------------------------------------------------
// Replace the foreign keys in a particular field by looking up oldKey from
// KeyList and replacing it with newKey. Returnes the number of keys updated.
//------------------------------------------------------------------------
Function TRec3Importer.UpdateForeignKey(const aTable, aField: string): integer;
begin
     Result := 0;
     FTempDb.BeginTrans;
     FTempQuery.SQL := 'UPDATE ' + aTable
        + ' INNER JOIN TempKeys ON ' + aTable + '.' + aField + '=TempKeys.oldKey'
        + ' SET ' + aTable + '.' + aField + '=TempKeys.NewKey;';
     try
        FTempQuery.Execute(dbFailOnError);
        Result := FTempQuery.RecordsAffected;
        FTempDb.CommitTrans(dbForceOSFlush);
     except
        on E:EOleException do
        begin
           FTempDb.Rollback;
           ReportError(errKeyUpdate, aTable, aField, E.Message);
        end;
     end;
end;

//------------------------------------------------------------------------
// Does a given table need its primary key updated?
// If primary index is a single field which is specified as 16 character
// string, but the value it actually contains is not a 16 character string,
// we need to update it.
//------------------------------------------------------------------------
Function TRec3Importer.NeedToUpdateKeys(const aTable: string): boolean;
var lName: string;
    i: integer;
begin
     Result := False;
     lName := GetPrimaryIndex(aTable);
     If lName <> '' then
     begin
          i := Pos(';', lName); // Is it multiple fields?
          If i=0 then  // single field
             With FTempdb.TableDefs[aTable].Fields[lName] do
                  If (Size = 16) and (Type_ = 10) then // 16 char string
                  begin
                     FTempQuery.SQL := 'SELECT ' + lName +
                                       ' FROM ' + aTable +
                                       ' WHERE (((' + lName + ') Not Like ' +
                                       ' "[0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z][0-9,A-Z]"));';
                     {Test whether data in key field has 16 characters}
                     try
                        FTempRecSet := FTempQuery.OpenRecordSet(emptyParam, emptyParam, emptyParam);
                        Result := (FTempRecSet.RecordCount > 0);
                     finally
                        FTempRecSet.Close;
                     end; // try .. finally
                end; // If Size..
     end; // if lName...
end;

//------------------------------------------------------------------------
// Get the last running code that was used in each table from NBNdata
// Store them in FLastKey in TableName=LastRunningKey pairs
//------------------------------------------------------------------------
procedure TRec3Importer.GetLastKeys;
var aTable: string;
    i: integer;
begin
    If FlastKey = nil then
       FLastKey := TStringList.Create;
    FLastKey.Clear;

  ADOQuery.SQL.Clear;
  ADOQuery.SQL.Add('SELECT TABLE_NAME, LAST_KEY_TEXT FROM LAST_KEY');
  try
    ADOQuery.Open;
    If ADOQuery.RecordCount > 0 then
    begin
      ADOQuery.First;
      while not ADOQuery.Eof do
      begin
        aTable :=  ADOQuery.FieldByName('TABLE_NAME').AsString;
        If FInsertedTables.IndexOfName(aTable) >=0 then // Only worry about tables in temp database
            FLastKey.Add(aTable + '=' + ADOQuery.FieldByName('LAST_KEY_TEXT').AsString);
        ADOQuery.Next;
      end;
    end;
  finally
    ADOQuery.Close;
  end;

    {Check we have entries for all the tables we are dealing with?
     If not initialise the counter for additional tables}
    For i:=0 to FInsertedTables.Count-1 do
        If FLastKey.IndexOfName(FInsertedTables.Names[i]) < 0 then
           If Pos(';', GetPrimaryIndex(FInsertedTables.Names[i])) = 0 then
              FLastKey.Add(FInsertedTables.Names[i] + '=00000000');  // If not - initialise it
end;

//------------------------------------------------------------------------
// For the next key for a given table from the site ID and the running key
// stored in FLastKey
//------------------------------------------------------------------------
Function TRec3Importer.GetNextKey(const aTable: string): string;
var temp : string;
begin
     Result := '';
     temp := IncrementString(FLastKey.Values[aTable]);
     If temp <> '' then
     begin
        FLastKey.Values[aTable] := temp;
        Result := FSiteID + temp;
     end;
end;

//---------------------------------------------------------------------
// Increment the string
//---------------------------------------------------------------------
Function TRec3Importer.IncrementString(const InStr: string): string;
var p: integer;
    Carry: boolean;
    Ch: string;
begin
     Result := '';
     Carry := True;
     p:=Length(Instr);
     {Increment the last character of the string.
      If this generates a carry, we need to increment the
      previous one, etc. Loop until we don't carry or hit
      the start of the string.}
     While (p>0) and (Carry) do
     begin
          ch := Copy(Instr, p, 1);
          IncrementChar(Ch, Carry);
          Result := ch + Result;
          dec(p);
     end;
     {Result is the unchanged part of the original string
      concatenated with the incremented bit already in Result}
     Result := Copy(Instr, 1, p) + Result;
end;

//---------------------------------------------------------------------
// Get the next character in tha character set. Carry indicates that we
// have wrapped back to the beginning of Charset
//---------------------------------------------------------------------
Procedure TRec3Importer.IncrementChar(var Ch: string; var Carry: boolean);
var p: integer;
begin
     Carry := False;
     p:= Pos(Ch, Charset);
     inc(p);
     If p>Length(CharSet) then
     begin
          p := 1;
          Carry := True;
     end;
     Ch := Copy(Charset, p, 1)
end;

//---------------------------------------------------------------------
// Take a line from the import file and check each (comma separated) field
// to see if it needs HTML to RTF conversion. Rebuild the line including
// converted comments.
//---------------------------------------------------------------------
Function TRec3Importer.ConvertHTMLtoRTF(aLine: string): string;
const      Quote = '"';
           Comma = ',';
var        i: integer;
           work: TStrings;
begin
    Result := '';
    Work := TStringList.Create;
    try
       {parse csv string}
       Work.CommaText := aLine;
       If Work.Count > 0 then
          For i:=0 to Work.Count-1 do
              If CompareText(Copy(Work[i], 1, Length(HTML)), HTML) = 0 then // look for lines starting <HTML>
                 Work[i] := FRecorder2000.RecorderFunctions.HTMLtoRTF(Work[i]); // Convert to RTF
       {rebuild the csv string}
       If Work.Count > 0 then
          For i:=0 to Work.Count-1 do
              If CompareText(Work[i], 'Null')=0 then
                 Result := Result + Work[i] + Comma  // don't put Null in quotes
              else
                 Result := Result + Quote + Work[i] + Quote + Comma;
    finally
        Work.Free;
    end;
    {trim the trailing comma}
    If Result[Length(Result)] = Comma then
       Result := Copy(Result, 1, Length(Result)-1);
end;


//------------------------------------------------------------------------
// Store the number of rows in the main database for each table in the temp
// database. We will check whether this has changed to find out whether the
// user cancelled the Import.
//------------------------------------------------------------------------
Procedure TRec3Importer.GetMainDBRowCount;
var i: integer;
    aTableName, test: string;
begin
     FMainDBCount.Clear;
     If FInsertedTables.Count > 0 then
        For i:=0 to FInsertedTables.Count-1 do
        begin
             aTableName := FInsertedTables.Names[i];
             if GetTableDef(aTableName) then
             begin
                test := IntToStr(ADOTable.RecordCount);
                FMainDBCount.Add(aTableName + '=' + test);
             end;
        end;
end;

//------------------------------------------------------------------------
// In the CheckDatabase call the user might have Cancelled (because errors
// were found). Test whether tables in the main database have been changed -
// if they have, then the user must have gone ahead with the Import.
//------------------------------------------------------------------------
Function TRec3Importer.ImportDone: boolean;
var i,x,y: integer;
    aTableName: string;
begin
     Result := False;
     If FMainDBCount.Count > 0 then
        For i:=0 to FMainDBCount.Count-1 do
        begin
             aTableName := FMainDBCount.Names[i];
             if GetTableDef(aTableName) then
             begin
                x :=  ADOTable.RecordCount;
                y :=  StrToInt(FMainDBCount.Values[aTableName]);
               If x > y then
               begin
                    Result := True; // table has had rows added - user must have gone ahead with Import
                    Exit;
               end;
             end;
        end;
end;

//------------------------------------------------------------------------
// add an error to FErrors object
//------------------------------------------------------------------------
Procedure TRec3Importer.ReportError(const aType: TErrorType; const aTable, aKey, aMessage: string);
begin
     FErrors.AddItem(aTable, aKey, aMessage, aType);
     {labelErrors.Caption := IntToStr(i) + ' errors.';
     labelErrors.Update;}
end;

//------------------------------------------------------------------------
// The main driving routine
//------------------------------------------------------------------------
procedure TRec3Importer.DoImport(const iSourceFile: TFileName);
var iForm: IImportProgressX;
    MDIPos: TRect;
    sZipName: OLEVariant;
begin
     try
          FInsertedTables := TStringList.Create;
          FMainDBCount := TStringList.Create;
          FErrors := TErrorList.Create;
          FDOSPath := ExtractFilePath(iSourceFile); // we need the DOS path elsewhere
          FControl := TINIFile.Create(iSourceFile); // get the file as an ini file

          // Set up the MDI container form where WE want it
          MDIPOs.Left := 0;
          MDIPOs.Top := 0;
          MDIPOs.Right := FeedBackFormWd;
          MDIPOs.Bottom := FeedBackFormHt;
          // save the current settings for MDIContainer form
          If not ReadMDIContainerPos(FSaveMDIContainerPos) then
             FSaveMDIContainerPos := MDIPos;
          WriteMDIContainerPos(MDIPos);

          // Show our progress form
          iForm := FRecorder2000.ShowActiveForm(CLASS_ImportProgressX) as IImportProgressX;
          iForm.ShowTitle(GetTitle);

          {Create temporary database and open NBNData}
          iForm.ShowTask(taskCreate, taskInProgress);
          MakeTempdb;
          // now the database is open, we can get the Site ID from the SETTING table
          FSiteID := Get_SiteID;
          iForm.ShowTask(taskCreate, taskDone);

          {Copy the csv files into temp database}
          iForm.ShowTask(taskImport, taskInProgress);
          ImportTables(iForm);
          iForm.ShowTask(taskImport, taskDone);

          {Replace temporary keys}
          iForm.ShowTask(taskAllocate, taskInProgress);
          UpdateKeys(iForm);
          iForm.ShowTask(taskAllocate, taskDone);

          {Fix potential problemsd in temp database}
          iForm.ShowTask(taskFix, taskInProgress);
          FixGrids(iForm); // fix problems of mismatch between Survey_event and Sample
          DeleteDuplicateBiotopeOccurrences(iForm); // remove duplicated biotope_occurrences
          Fix_DetDate(iForm);  // ensure det date is not before sample date
          FixChannelIslandGrids(iForm);  //OSGB WA and WV grid refs to UTM
          iForm.ShowTask(taskFix, taskDone);

          // close the Tempdb
          FTempdb.Close;
          FTempdb := nil;

          // Report errors
          FErrors.DumpToFile(FDOSPath);

          // Zip the mdb file
          iForm.ShowTask(taskZip, taskInProgress);
          sZipName := FTempMDBName;
          iForm.ZipFile(sZipName);
          iForm.ShowTask(taskZip, taskDone);

          {Copy the temp database into main database}
          iForm.ShowTask(taskCopy, taskInProgress);
          GetMainDBRowCount;


          FRecorder2000.CheckDatabase(widestring(sZipName), GetTitle);
          iForm.ShowTask(taskCopy, taskDone);
          If ImportDone then
             iForm.ReportCompletion(FErrors.Count)
          else
             iForm.ReportCompletion(-1);

          // Restore the MDI container position to where user left it
          WriteMDIContainerPos(FSaveMDIContainerPos);

     finally
          FControl.Free;
          FInsertedTables.Free;
          FErrors.Free;
          If FMainDBCount <> nil then
             FMainDBCount.Free;
          If FLastKey <> nil then
             FLastKey.Free;
          If FTempdb <> nil then
             FTempdb.Close;
          // clean up zip file
          if FileExists(string(sZipName)) then
            DeleteFile(string(sZipName));
     end;
end;


//------------------------------------------------------------------------
// Generate the title for the progress form
//------------------------------------------------------------------------
Function TRec3Importer.GetTitle: string;
var TitleInfo: TStrings;
    Temp: string;
begin
     TitleInfo := TStringList.Create;
     try
        FControl.ReadSectionValues('Export', TitleInfo);
        Temp := 'Exported from Recorder ' + TitleInfo.Values['RecorderVersion'];
        Temp := Temp + ' (copy no. ' + TitleInfo.Values['CopyNo'] + ': "';
        Temp := Temp + TitleInfo.Values['CopyName'] + '")';
        Temp := Temp + ' on ' + TitleInfo.Values['Date'];
        Temp := Temp + ' by user "' + TitleInfo.Values['User'] + '".';
        Result := temp;
     finally
        TitleInfo.Free;
     end;
end;

//------------------------------------------------------------------------
// Get the position of Recorder 2000's MDIContainer form from the Registry.
//------------------------------------------------------------------------
Function TRec3Importer.ReadMDIContainerPos(aPos: TRect): boolean;
var lReg: TRegistry;
begin
     Result := True;
     lReg := TRegistry.Create;
     try
        lReg.OpenKey('Software\JNCC\Recorder\Forms\MDIContainer',False);
        try
           lReg.ReadBinaryData('Position', aPos, SizeOf(aPos));
        except
           Result := False;
        end;
     finally
        lReg.Free;
     end;
end;

//------------------------------------------------------------------------
// Set the position and size we want Recorder 2000's MDI container to appear
// and write it to the Registry.
//------------------------------------------------------------------------
Procedure TRec3Importer.WriteMDIContainerPos(aPos: TRect);
var lReg: TRegistry;
begin
     lReg := TRegistry.Create;
     try
        lReg.OpenKey('Software\JNCC\Recorder\Forms\MDIContainer',True);
        lReg.WriteBinaryData('Position', aPos, SizeOf(aPos));
        lReg.WriteInteger('Opened', 0);
        lReg.WriteInteger('State', 2);
     finally
        lReg.Free;
     end;
end;

//----------------------------------------------------------------------
// Grid refs are used to identify Samples within a Survey event - so a
// single Survey event can end up with several Samples with different grid
// refs. This may fail validation. This routine finds such cases (i.e.
// Survey events with more than one different grid ref amongst its Samples)
// and finds a lower precision grid ref that is common to all of them.
// The Survey event is then updated with this common grid ref. This
// may mean no grid ref in the Survey event if Samples are in different
// 10km squares. NOTE: It only works for grid refs (OSGB, OSNI), for other
// spatial ref systems the spatial ref in the Survey event will be set
// to empty.
//----------------------------------------------------------------------
procedure TRec3Importer.FixGrids(IForm: IImportProgressX);
var lGrids: TStrings;
    lRecSet, qRecSet: RecordSet;
    lSE_Key, lSpatialRefSystem, lGridSys, lGrid, NewGrid: String;
    ChangeGrid: boolean;
    i, n: integer;

    //--------------------------------------------------
    // Reduce the precision of grids down a notch, e.g.
    // from 1km to 10km square. Returns a string list
    // containing the unique squares at the new precision
    //--------------------------------------------------
    procedure ReducePrecision(Grids: TStrings);
    var i, maxLen, l, nFigs, nLetters: integer;
        lGrid: string;
        Work: TStrings;
    begin
         // initialise
         Work := TStringList.Create;
         try
            maxLen := 0;
            nLetters := 0;
            // set nLetters depending on grid system
            If lSpatialRefSystem = 'OSGB' then
               nLetters := 2;
            If lSpatialRefSystem = 'OSNI' then
               nLetters := 1;
            // We can only handle grid refs, so if it is a different system
            // just quit. This will blank the ref in the Survey event
            If nLetters = 0 then
            begin
               Grids.Clear;
               Exit;
            end;
            // find longest grid ref
            For i:=0 to Grids.Count-1 do
            begin
                 l := Length(Grids[i]);
                 if l>maxLen then maxLen := l;
            end; // for
            // get numer of figs in longest ref
            nFigs := (maxLen - nLetters) DIV 2;
            // reduce the precision
            If nFigs > 1 then
            begin
                 For i:=0 to Grids.Count-1 do
                 begin
                      lGrid := Grids[i];
                      if Length(lGrid) = MaxLen then
                         lGrid := Copy(lGrid, 1, nLetters + nFigs - 1) +
                                  Copy(lGrid, nLetters + nFigs + 1, nFigs - 1);
                      // do we already have it?
                      l := Work.IndexOf(lGrid);
                      If l < 0 then // not found
                         Work.Add(lGrid); //so add it
                 end; // for
            end; // if nFigs ...
            Grids.Assign(Work);
         finally
            Work.Free;
         end;
    end; //Reduce precision

    //--------------------------------------------------
    // If there is more than one grid ref within the
    // survey_event, reduce the precision until only]
    // one is left
    //--------------------------------------------------
    function GetGrid: string;
    begin
         // reduce the precision of grid refs until we only have 1 grid ref
         // common to all of them
         While lGrids.Count > 1 do
              ReducePrecision(lGrids);
         // if we have one, return it
         If lGrids.Count = 1 then
            Result := lGrids[0]
         else
            // otherwise return an empty string - this will happen
            // if grid refs where in different 10km squares
            Result := '';
         ChangeGrid := True;
    end; // Get Grid

    //--------------------------------------------------
    // Update the spatial ref in the survey_event whose
    // key is in lSE_Key
    //--------------------------------------------------
    procedure UpdateGrid;
    var aLatLong: TLatLong;
        lSpatRefSystem: string[4];
        lQualifier: string;
    begin
          lSpatRefSystem := '';
          lQualifier := '';
          // if we have a grid ref, convert it to internal format
          If NewGrid <> '' then
          begin
               lSpatRefSystem := DetermineSpatialRefSystem(NewGrid);
               If lSpatRefSystem <> SYSTEM_UNKNOWN then
               try
                    aLatLong := ConvertToLatLong(NewGrid, lSpatRefSystem);
                    lQualifier := 'Estimated from Map';
               except
                    on Exception do NewGrid := ''; // conversion error - blank grid
               end;
          end;
          If NewGrid <> '' then
             FTempQuery.SQL := 'UPDATE SURVEY_EVENT SET ' +
                               'SURVEY_EVENT.SPATIAL_REF = "' + NewGrid + '", ' +
                               'SURVEY_EVENT.SPATIAL_REF_SYSTEM = "' + lSpatRefSystem + '", ' +
                               'SURVEY_EVENT.LAT = "' + aLatLong.Lat + '", ' +
                               'SURVEY_EVENT.[LONG] = "' + aLatLong.Long + '", ' +
                               'SURVEY_EVENT.SPATIAL_REF_QUALIFIER = "' + lQualifier + '" ' +
                               'WHERE (((SURVEY_EVENT.SURVEY_EVENT_KEY)="' + lSE_Key + '"));'
          else
             // if the grid was blank, this will remove the spatial ref from the Survey event
             FTempQuery.SQL := 'UPDATE SURVEY_EVENT SET ' +
                               'SURVEY_EVENT.SPATIAL_REF = Null, ' +
                               'SURVEY_EVENT.SPATIAL_REF_SYSTEM = Null, ' +
                               'SURVEY_EVENT.LAT = Null, ' +
                               'SURVEY_EVENT.[LONG] = Null, ' +
                               'SURVEY_EVENT.SPATIAL_REF_QUALIFIER = Null ' +
                               'WHERE (((SURVEY_EVENT.SURVEY_EVENT_KEY)="' + lSE_Key + '"));';
          FTempQuery.Execute(dbFailOnError);
    end; //UpdateGrid

begin // FixGrids
   iForm.ProgressTables := 0;
   iForm.ProgressTablesMax :=10;
   iForm.CurrentTableName := 'survey_event';
   iForm.ProgressTables := 1;
   // Find Survey events where the related Samples contain more than one unique grid refs
   FTempQuery.SQL := 'SELECT SURVEY_EVENT.SURVEY_EVENT_KEY, Count(SAMPLE.SPATIAL_REF) AS CountOfSPATIAL_REF ' +
                     'FROM SURVEY_EVENT INNER JOIN SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY ' +
                     'GROUP BY SURVEY_EVENT.SURVEY_EVENT_KEY ' +
                     'HAVING (((Count(SAMPLE.SPATIAL_REF))>1)) ' +
                     'ORDER BY SURVEY_EVENT.SURVEY_EVENT_KEY;';
   lRecSet := FTempQuery.OpenRecordSet(emptyParam, emptyParam, emptyParam);
   try
     if lRecSet.RecordCount > 0 then
     begin
          n := 0;
          iForm.ProgressMax := lRecSet.RecordCount;
          iForm.Progress := 0;
          // String list to conatin the grid refs
          lGrids := TStringList.Create;
          try
             lRecSet.MoveFirst;
             while not lRecSet.EoF do
             begin
                  // Get the key of the Survey event
                  lSE_Key := lRecSet.Fields['SURVEY_EVENT_KEY'].Value;
                  // Get the samples within this survey event
                  FTempQuery.SQL := 'SELECT SPATIAL_REF, SPATIAL_REF_SYSTEM ' +
                                    'FROM SAMPLE ' +
                                    'WHERE (((SURVEY_EVENT_KEY)="' + lSE_Key + '"));';
                  qRecSet := FTempQuery.OpenRecordSet(emptyParam, emptyParam, emptyParam);
                  try
                     If qRecSet.RecordCount > 0 then
                     begin
                          // initialise for this set of Samples
                          lGrids.Clear;
                          ChangeGrid := False;
                          NewGrid := '';
                          lSpatialRefSystem := '';
                          // Loop through the samples
                          while not qRecSet.EoF do
                          begin
                               // do we have a spatail ref?
                               If TVarData(qRecSet.Fields['SPATIAL_REF_SYSTEM'].value).VType
                                  = varOLEStr then
                               begin
                                    // read the grid system and spatial ref
                                    lGridSys := qRecSet.Fields['SPATIAL_REF_SYSTEM'].value;
                                    lGrid := TrimInsideStr(qRecSet.Fields['SPATIAL_REF'].value);
                                    // if the sptaial ref systems are different we exit
                                    // This will have the effect of blanking the spatial ref in the
                                    // Survey event (ChageGrid set to True and NewGrid blank)
                                    If lSpatialRefSystem = '' then
                                       lSpatialRefSystem := lGridSys
                                    else
                                        If NOT ((lSpatialRefSystem = lGridSys) AND
                                               ((lSpatialRefSystem = 'OSGB') OR
                                               (lSpatialRefSystem = 'OSNI'))) then
                                        begin
                                             ChangeGrid := True;
                                             NewGrid := '';
                                             Break;
                                        end;
                                    // do we have this one yet?
                                    i := lGrids.IndexOf(lGrid);
                                    If i< 0 then
                                       lGrids.Add(lGrid);
                               end; // have a spatial_ref_system
                               qRecSet.MoveNext; // next Sample
                          end; // while
                          // get a grid of lower precision common to all of them
                          If not ChangeGrid then
                             NewGrid := GetGrid;
                          If ChangeGrid then UpdateGrid; // update the Survey event
                     end; // RecordCount > 1
                  finally
                     qRecSet.Close;
                  end;
                  lRecSet.MoveNext; // next Survey event
                  inc(n);
                  iForm.Progress := n;
             end; // While EoF
          finally
             lGrids.Free;
          end;
     end; //RecordCount
   finally
     lRecSet.Close;
   end;
end;

//------------------------------------------------------------------------
// When a set of records was entered into Rec3.3 via a data entry list in
// which a habitat was given, each record was saved with the same habitat.
// When these are reconstructed into a sample in Rec2K, the effect is to produce
// a series of identical biotope_occurences, one for each taxon_occurrence
// in the sample. This procedure removes all but one of these identical
// biotope occurences within a sample - i.e. those that have the same biotope_list_item
// and SAMPLE_KEY.
//------------------------------------------------------------------------
Procedure TRec3Importer.DeleteDuplicateBiotopeOccurrences(IForm: IImportProgressX);
var lSQL: string;

    Function DoMakeTable(const theSQL: string): boolean;
    begin
        Result := false;
        FTempQuery.SQL := theSQL;
        FTempDb.BeginTrans;
        try
           FTempQuery.Execute(dbFailOnError);
        except
           on E:EOleException do
           begin
                FTempDb.Rollback;
                ShowMessage(e.Message);
                Exit;
           end;
        end; // try .. except
        FTempDb.CommitTrans(dbForceOSFlush);
        Result := True;
    end;

begin
  // only do it if we have biotope occurrences!
  If FInsertedTables.IndexOfName('biotope_occurrence') >= 0 then
  begin
     iForm.CurrentTableName := 'biotope_occurrence';
     iForm.ProgressTables := 2;
     iForm.ProgressMax := 10;
     iForm.Progress := 1;
     // Make a table of the Biotope_Occurrence_Keys for uniques habitat recs
     // i.e. unique combinations of SAMPLE_KEY and BIOTOPE_LIST_ITEM
     lSQL := 'SELECT First(biotope_occurrence.BIOTOPE_OCCURRENCE_KEY) AS BOKey INTO BO_Key ' +
             'FROM biotope_occurrence INNER JOIN biotope_determination ON ' +
             'biotope_occurrence.BIOTOPE_OCCURRENCE_KEY = biotope_determination.BIOTOPE_OCCURRENCE_KEY ' +
             'GROUP BY [biotope_determination]![BIOTOPE_LIST_ITEM_KEY] & [biotope_occurrence]![SAMPLE_KEY] ' +
             'ORDER BY [biotope_determination]![BIOTOPE_LIST_ITEM_KEY] & [biotope_occurrence]![SAMPLE_KEY];';
     If DoMakeTable(lSQL) then
     begin
          iForm.Progress := 5;
          // now copy the rows we do want into new tables
          // biotope determination
          lSQL := 'SELECT biotope_determination.* INTO BD2 ' +
                  'FROM biotope_determination INNER JOIN BO_Key ON ' +
                  'biotope_determination.BIOTOPE_OCCURRENCE_KEY = BO_Key.BOKey;';
          if DoMakeTable(lSQL) then
          begin
               iForm.Progress := 7;
               // biotope occurrence
               lSQL := 'SELECT biotope_occurrence.* INTO BO2 ' +
                       'FROM biotope_occurrence INNER JOIN BO_Key ON ' +
                       'biotope_occurrence.BIOTOPE_OCCURRENCE_KEY = BO_Key.BOKey;';
               If DoMakeTable(lSQL) then
               begin
                    iForm.Progress := 9;
                    // we can now delete the original biotope_occurrence and
                    //biotope_determination tables
                    FTempdb.TableDefs.Delete('biotope_determination');
                    FTempdb.TableDefs.Delete('biotope_occurrence');
                    FTempdb.TableDefs.Refresh;
                    // .... and rename BO2 and BD2
                    FTempdb.TableDefs['BO2'].Name := 'biotope_occurrence';
                    FTempdb.TableDefs['BD2'].Name := 'biotope_determination';
                    iForm.Progress := 10;
                    // No longer need BO_Key
                    FTempdb.TableDefs.Delete('BO_Key');
                    FTempdb.TableDefs.Refresh;
               end; // make BO2
          end; // make BD2
     end; // make BO_Key
     iForm.Progress := 0;
  end; // if biotope occurrences
end;

//------------------------------------------------------------------------
// Ensure that the date of determination is not earlier than the date of
// observation. In such cases, make the det date the same as the
// sample date. This is done by running a straight UPDATE query.
//------------------------------------------------------------------------
Procedure TRec3Importer.Fix_DetDate(IForm: IImportProgressX);
begin
  // fix taxon det dates
  If FInsertedTables.IndexOfName('taxon_occurrence') >= 0 then
  begin
    iForm.CurrentTableName := 'taxon_determination';
    iForm.ProgressTables := 3;
    FTempQuery.SQL := 'UPDATE (sample INNER JOIN taxon_occurrence ON ' +
            'sample.SAMPLE_KEY = taxon_occurrence.SAMPLE_KEY) ' +
            'INNER JOIN taxon_determination ON taxon_occurrence.TAXON_OCCURRENCE_KEY = ' +
            'taxon_determination.TAXON_OCCURRENCE_KEY ' +
            'SET taxon_determination.VAGUE_DATE_START = [sample]![VAGUE_DATE_START], ' +
            'taxon_determination.VAGUE_DATE_END = [sample]![VAGUE_DATE_END], ' +
            'taxon_determination.VAGUE_DATE_TYPE = [sample]![VAGUE_DATE_TYPE] ' +
            'WHERE (((taxon_determination.VAGUE_DATE_START)<[sample]![VAGUE_DATE_START]));';
     try
        FTempQuery.Execute(dbFailOnError);
     except
        on E:EOleException do
             ReportError(errUpdate, 'taxon_determination', 'SQL', E.Message);
     end;
  end;

  // fix biotope det dates
  If FInsertedTables.IndexOfName('biotope_occurrence') >= 0 then
  begin
    iForm.CurrentTableName := 'biotope_determination';
    iForm.ProgressTables := 4;
    FTempQuery.SQL := 'UPDATE (sample INNER JOIN biotope_occurrence ON ' +
            'sample.SAMPLE_KEY = biotope_occurrence.SAMPLE_KEY) ' +
            'INNER JOIN biotope_determination ON biotope_occurrence.BIOTOPE_OCCURRENCE_KEY = ' +
            'biotope_determination.BIOTOPE_OCCURRENCE_KEY ' +
            'SET biotope_determination.VAGUE_DATE_START = [sample]![VAGUE_DATE_START], ' +
            'biotope_determination.VAGUE_DATE_END = [sample]![VAGUE_DATE_END], ' +
            'biotope_determination.VAGUE_DATE_TYPE = [sample]![VAGUE_DATE_TYPE] ' +
            'WHERE (((biotope_determination.VAGUE_DATE_START)<[sample]![VAGUE_DATE_START]));';
     try
        FTempQuery.Execute(dbFailOnError);
     except
        on E:EOleException do
             ReportError(errUpdate, 'biotope_determination', 'SQL', E.Message);
     end;
  end;
end;

//------------------------------------------------------------------------
// Loook for SPATIAL_REFs that are Channel island grid refs given in
// OSGB format (WAnnnn or WVnnnn - where "nnnn" is an even number of
// digits). Convert such referemces into UTM format
//------------------------------------------------------------------------
Procedure TRec3Importer.FixChannelIslandGrids(IForm: IImportProgressX);
var sTable, sPrefix, sKey, sGrid, sql: string;
    i, j: integer;
    lRecSet: RecordSet;
begin
  for i := 1 to 6 do
  begin
    // These are the tables and columns that may be affected
    case i of
    1:   begin sTable := 'survey';       sPrefix := 'SW_'; end;
    2:   begin sTable := 'survey';       sPrefix := 'NE_'; end;
    3:   begin sTable := 'survey_event'; sPrefix := '';    end;
    4:   begin sTable := 'sample';       sPrefix := '';    end;
    5:   begin sTable := 'location';     sPrefix := '';    end;
    6:   begin sTable := 'grid_square';  sPrefix := '';    end;
    end;

    // progress indicator
    iForm.CurrentTableName := sTable + ' Channel Island grids';
    iForm.ProgressTables := i + 4;

    // search for Channel Island grids in OSGB format
    sql := 'SELECT ' + sTable + '_KEY, ' + sPrefix + 'SPATIAL_REF' +
                      ' FROM ' + sTable +
                      ' WHERE SPATIAL_REF_SYSTEM="OSGB"' +
                      ' AND (' + sPrefix + 'SPATIAL_REF Like "WA*" OR ' +
                      sPrefix + 'SPATIAL_REF Like "WV*");';
    FTempQuery.SQL := sql;
    lRecSet := FTempQuery.OpenRecordSet(emptyParam, emptyParam, emptyParam);
    try
      // did we find any ?
      if lRecSet.RecordCount > 0 then
      begin
        //lRecSet.MoveLast(0);
        iForm.ProgressMax := lRecSet.RecordCount;
        iForm.Progress := 0;
        j := 0;
        lRecSet.MoveFirst;
        // if we did, then update them
        while not lRecSet.EoF do
        begin
          inc(j);
          iForm.Progress := j;
          sKey := lRecSet.Fields[0].Value;
          sGrid := lRecSet.Fields[1].Value;
          UpdateChannelIslandsGrid(sTable, sPrefix, sKey, sGrid);
          lRecSet.MoveNext;
        end;
      end;
    finally
      lRecSet.Close;
      iForm.Progress := 0;
    end;
  end;
end;

//------------------------------------------------------------------------
// Update the given row in the given table
// The Grid is reformatted into UTM format and re-converted to LAt/LONG
// The row is updated with the new UTM grid, "UTM" as the Grid system
// and the new LAT, LONG coordinates
//------------------------------------------------------------------------
Procedure TRec3Importer.UpdateChannelIslandsGrid(const sTable, sPrefix, sKey, sGrid: string);
var sUTM, sql: string;
    ll: TLatLong;
begin
  // change WA/WV ref to a UTM formatted ref
  sUTM := ChannelIslandsGrid(sGrid);
  // get corresponding LAT/LONG coordinates
  ll := ConvertToLatLong(sUTM, 'UTM');
  // generate the SQL
  sql := 'UPDATE ' + sTable +
          ' SET ' + sPrefix + 'SPATIAL_REF = "' + sUTM + '", ' +
          'SPATIAL_REF_SYSTEM = "UTM", ' +
          sPrefix + 'LAT = ' + ll.Lat  + ', ['  +
          sPrefix + 'LONG] = ' + ll.Long  +
          ' WHERE (' + sTable + '_KEY="' + sKey + '");';
  // .. and try to execute the update
  FTempQuery.SQL := sql;
  try
    FTempQuery.Execute(dbFailOnError);
  except
    on E:EOleException do
         ReportError(errUpdate, sTable, sKey, E.Message);
  end;
end;

//-----------------------------------------------------------------------
// Function which examines a grid ref string. If it is a Channel Island
// grid ref in the format "WAnnnnnn" or "WVnnnnnn" (where nnnnn is an
// even number of digits of length 2 - 10) then it is reformatted as a
// UTM grid ref in the format "30 nnnnnn nnnnnnn".
//-----------------------------------------------------------------------
Function TRec3Importer.ChannelIslandsGrid(const sText: string): string;
var sInput, s100km: string;
    iFigs: integer;
    ix,iy: Longint;
begin
  Result := sText;
  sInput := UpperCase(Trim(sText));
  iFigs := Length(sInput);
  if (iFigs > 2) and (not odd(iFigs)) then
  begin
    s100km := Copy(sInput,1,2);
    iFigs := (iFigs - 2) div 2;
    if (iFigs <=5) AND
       ((CompareText(s100km, 'WA') = 0) OR
       (CompareText(s100km, 'WV') = 0)) then
    try
      ix := StrToInt(Copy(sInput, 3, iFigs));
      iy := StrToInt(Copy(sInput, 3 + iFigs, iFigs));
      iFigs := Trunc(Power(10.0, 5-iFigs));
      ix := ix * iFigs + 500000;
      if (CompareText(s100km, 'WA') = 0) then
        iy := iy * iFigs + 5500000
      else
        iy := iy * iFigs + 5400000;
      Result := '30 ' + IntToStr(ix) + ' ' + IntToStr(iy);
    except
      on EConvertError do
        Result := sText;
    end;
  end;
end;



//=========================================================================
initialization
  TAutoObjectFactory.Create(ComServer, TRec3Importer, Class_Rec3Importer,
    ciSingleInstance, tmApartment);
end.
