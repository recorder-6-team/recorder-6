{===============================================================================
  Unit:        ExternalFilterTables.pas

  Description: This unit contains the External Filter File classes.

  Model:       AdvancedReportFiles.mpb

  Created:     May 2008

  Last revision information:
    $Revision: 4 $
    $Date: 5/06/08 16:31 $
    $Author: Johndurman $
===============================================================================}

unit CRExternalFilterFiles;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, 
  Forms, Dialogs, XMLIntf, XMLDoc, DB, ExceptionForm, DBClient, Grids,
  CRCommonClasses, ADODB;

resourcestring
  ResStr_ExternalFilterFileNotFound = 'The expected external filter file could not be found at the following location: %s';
  ResStr_ExternalFilterFileOpen = 'The data in the external filter file ''%s'' could not be read.'#13#10'If the file is still open in another application (e.g. Excel) please close it before repeating this action.';
  ResStr_ExternalFilterInvalidLine = 'The external filter has an invalid line of data:'#13#10'%s';
  ResStr_ExternalFilterFile = 'External Filter - %s';
  ResStr_EmptyExternalFilterFile = 'This external filter file contains no data:'#13#10'%s';

type
  EExternalFilterFileException = class(TExceptionPath)
  end;

  TExternalFilter = class
  private
    FItemKey: string;
    FDescription: string;
    FTableName: string;
  public
    constructor Create(const AFilterLine: string);
    property TableName: string read FTableName;
    property ItemKey: string read FItemKey;
    property Description: string read FDescription;
  end;

  TExternalFilterTable = class(TObject)
  private
    FExternalFilterRows: TStringList;
    FConnection: TADOConnection;
    FFileName: String;
    FInputParam: TInputParam;
    FName: String;
    procedure CreateTable;
    function GetRowData(ExternalFilter: TExternalFilter): String;
    procedure InsertData;
    procedure LoadExternalFilter;
    procedure Validate(ANode: IXMLNode);
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    procedure BuildTable;
    procedure DropTable;
    procedure ReadXML(ANode: IXMLNode);
    property FileName: String read FFileName;
    property InputParam: TInputParam read FInputParam;
    property Name: String read FName;
  end;

  TExternalFilterTables = class(TList)
  private
    FConnection: TADOConnection;
    function GetInputParam(Index: Integer): TInputParam;
    function GetInputParamCount: Integer;
    function GetItems(AIndex: Integer): TExternalFilterTable;
    procedure SetItems(AIndex: Integer; AItem: TExternalFilterTable);
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    function Add(AItem: TExternalFilterTable): Integer;
    procedure Clear; override;
    function Extract(AItem: TExternalFilterTable): TExternalFilterTable;
    function First: TExternalFilterTable;
    function IndexOf(AItem: TExternalFilterTable): Integer;
    procedure Insert(AIndex: Integer; AItem: TExternalFilterTable);
    function Last: TExternalFilterTable;
    procedure ReadXML(ANode: IXMLNode);
    function Remove(AItem: TExternalFilterTable): Integer;
    property InputParam[Index: Integer]: TInputParam read GetInputParam;
    property InputParamCount: Integer read GetInputParamCount;
    property Items[AIndex: Integer]: TExternalFilterTable read GetItems write SetItems; default;
  end;

implementation

uses
  CRConstants, DatabaseAccessADO, ResourceStrings, ComObj, CRReportSQL,
  ApplicationSettings, StrUtils, GenFuncs;

const
  EXTERNAL_FILTER_DATA_TEMPLATE = '''%s'',''%s'',%s';
  MAX_DESCRIPTION_LENGTH = 8000;
  COLUMN_NAMES = 'TableName, ItemKey, Description';
  COLUMN_PARAMETERS = 'TableName sysname, ItemKey char(16) COLLATE SQL_Latin1_General_CP1_CI_AS, Description varchar(8000) COLLATE SQL_Latin1_General_CP1_CI_AS';

{===============================================================================
    TExternalFilterTable
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor.
}
constructor TExternalFilterTable.Create(AConnection: TADOConnection);
begin
  inherited Create;

  FConnection := AConnection;
  FExternalFilterRows := TStringList.Create;
end;  // TExternalFilterTable.Create

{-------------------------------------------------------------------------------
  Destructor
}
destructor TExternalFilterTable.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(FExternalFilterRows.Count) do
    FExternalFilterRows.Objects[i].Free;
  FExternalFilterRows.Free;
  FInputParam.Free;
  inherited;
end;  // TExternalFilterTable.Destroy

{-------------------------------------------------------------------------------
  Create the temp table and populate it from the source ExternalFilter file.
}
procedure TExternalFilterTable.BuildTable;
begin
  CreateTable;
  LoadExternalFilter;
  InsertData;
end;  // TExternalFilterTable.BuildTable

{-------------------------------------------------------------------------------
  Create the temporary table for the data read in from the ExternalFilter file to go into.
}
procedure TExternalFilterTable.CreateTable;
var
  lSQL: String;
begin
  // Ensure that if for some reason the table already exists, it is dropped.
  FConnection.Execute(Format(SQL_DROPTEMPTABLE, [FName, FName]),
      cmdText, [eoExecuteNoRecords]);
  // Create the table;
  lSQL := Format(SQL_CREATETEMPTABLE, [FName, COLUMN_PARAMETERS]);
  FConnection.Execute(lSQL, cmdText, [eoExecuteNoRecords]);
end;  // TExternalFilterTable.CreateTable

{-------------------------------------------------------------------------------
  Drop the temporary table.
}
procedure TExternalFilterTable.DropTable;
begin
  // If the user failed to enter a name for the temporary table, validation would
  // fail and the reporting process would be aborted. It would then come here
  // in the clean up, but FName would be ''. This would mean the SQL line
  // created below to drop the temporary tables would fail. Hence, only run the
  // line if FName <> ''.
  if FName <> '' then
    FConnection.Execute(Format(SQL_DROPTEMPTABLE, [FName, FName]), cmdText,
        [eoExecuteNoRecords]);
end;  // TExternalFilterTable.DropTable

{-------------------------------------------------------------------------------
  Build a string containing the data to be inserted for a row.
}
function TExternalFilterTable.GetRowData(ExternalFilter: TExternalFilter): String;
var
  lDescription: string;
begin
  if (Length(ExternalFilter.Description) > MAX_DESCRIPTION_LENGTH) then
    lDescription := '''' + Copy(ExternalFilter.Description, 1, MAX_DESCRIPTION_LENGTH) + ''''
  else if (Length(ExternalFilter.Description) > 0) then
    lDescription := '''' + ExternalFilter.Description + ''''
  else
    lDescription := 'NULL';

  Result := Format(EXTERNAL_FILTER_DATA_TEMPLATE,
      [ExternalFilter.TableName, ExternalFilter.ItemKey, lDescription]);
end;  // TExternalFilterTable.GetRowData

{-------------------------------------------------------------------------------
  Insert data into the temporary table one row at a time.
}
procedure TExternalFilterTable.InsertData;
var
  lSQL, lRowData: String;
  i: integer;
begin
  for i := 0 to Pred(FExternalFilterRows.Count) do begin
    if (FExternalFilterRows.Objects[i] is TExternalFilter) then begin
      lRowData := GetRowData(TExternalFilter(FExternalFilterRows.Objects[i]));
      lSQL := Format(SQL_INSERTRECORD, [FName, COLUMN_NAMES, lRowData]);
      try
        FConnection.Execute(lSQL, cmdText, [eoExecuteNoRecords]);
      except on E:EOLEException do
        raise EReportSQLException.CreateNonCritical(Format(ResStr_SQLErrorDetail,
            [E.Message, lSQL]));
      end;
    end;    // if (FExternalFilterRows.Objects[i] is TExternalFilter)
  end;    // for i := 0 to Pred(FExternalFilterRows.Count)
end;  // TExternalFilterTable.InsertData

{-------------------------------------------------------------------------------
  Load the ExternalFilter file.
}
procedure TExternalFilterTable.LoadExternalFilter;
var
  i: integer;
  lFilename: String;
begin
  if FFileName <> '' then lFilename := FFilename
                     else lFilename := Copy(InputParam.Values[0], 2, Length(InputParam.Values[0])-2);
  if ExtractFilePath(lFilename) = '' then
    lFilename := AppSettings.ReportPath + 'Output\' + lFilename;
  if not FileExists(lFileName) then
    raise EExternalFilterFileException.CreateNonCritical(
        Format(ResStr_ExternalFilterFileNotFound, [lFileName]));

  try
    FExternalFilterRows.LoadFromFile(lFilename);
    if FExternalFilterRows.Count = 0 then
      raise EExternalFilterFileException.CreateNonCritical(
          Format(ResStr_EmptyExternalFilterFile, [lFilename]));

    for i := 0 to Pred(FExternalFilterRows.Count) do
      if not AnsiSameText(AnsiLeftStr(FExternalFilterRows[i], 11), 'Description') then
        FExternalFilterRows.Objects[i] := TExternalFilter.Create(FExternalFilterRows[i]);
  except
    on E:EExternalFilterFileException do raise;
    on E:Exception do
      raise EExternalFilterFileException.CreateNonCritical(
          Format(ResStr_ExternalFilterFileOpen, [lFilename]), E);
  end;
end;  // TExternalFilterTable.LoadExternalFilter

{-------------------------------------------------------------------------------
  Reads the ExternalFilterTable XML node for the report file.
}
procedure TExternalFilterTable.ReadXML(ANode: IXMLNode);
var
  ParamName: string;
begin
  with ANode do begin
    Validate(ANode);
    if HasAttribute(AT_INPUTFILE) then FFileName := Attributes[AT_INPUTFILE];
    if HasAttribute(AT_TABLENAME) then FName := Attributes[AT_TABLENAME];
  end;
  if FFileName='' then begin
    if FName[1] = '#' then ParamName := Copy(FName, 2, MaxInt)
    else ParamName := FName;
    ParamName := Format(ResStr_ExternalFilterFile, [ReadableFormat(ParamName)]);
    FInputParam := TInputParam.Create(dtCSVFile, ParamName, nil);
    // Ensure input control is placed inside scroll box, like CSV file parameters,
    //  to avoid a search dialog appearing when running the report.
    FInputParam.EntryCount := 0;
  end;  // if FFileName=''
end;  // TExternalFilterTable.ReadXML

{-------------------------------------------------------------------------------
  Validate the contents of the node to check all the required attributes are present.
}
procedure TExternalFilterTable.Validate(ANode: IXMLNode);
begin
  with ANode do begin
    if (not HasAttribute(AT_TABLENAME)) then
      raise EExternalFilterFileException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                            ['tablename', 'externalfilterfile']));
    if Pos('#', Attributes[AT_TABLENAME]) <> 1 then
      raise EExternalFilterFileException.CreateNonCritical(Format(ResStr_TemporaryTablePrefixError,
                                                [Attributes[AT_NAME]]));
  end;
end;  // TExternalFilterTable.Validate

{-==============================================================================
    TExternalFilterTables
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  Requires connection that the report is being created on.
}
constructor TExternalFilterTables.Create(AConnection: TADOConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;  // TExternalFilterTables.Create

{-------------------------------------------------------------------------------
}
destructor TExternalFilterTables.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TExternalFilterTables.Destroy

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.Add(AItem: TExternalFilterTable): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TExternalFilterTables.Add

{-------------------------------------------------------------------------------
}
procedure TExternalFilterTables.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TExternalFilterTables.Clear

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.Extract(AItem: TExternalFilterTable): TExternalFilterTable;
begin
  Result := TExternalFilterTable(inherited Extract(AItem));
end;  // TExternalFilterTables.Extract

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.First: TExternalFilterTable;
begin
  Result := TExternalFilterTable(inherited First);
end;  // TExternalFilterTables.First

{-------------------------------------------------------------------------------
  Accessor method - allows access to ExternalFilter table and SQL input parameters.
}
function TExternalFilterTables.GetInputParam(Index: Integer): TInputParam;
var
  I: Integer;
  lPos: Integer;
begin
  i := 0;
  lPos := 0;
  Result := nil;

  while i < Count do begin
    if Assigned(Items[i].InputParam) then begin
      if Index=lPos then begin
        Result := Items[i].InputParam;
        Break; // from loop
      end;
      Inc(lPos);
    end;
    Inc(i);
  end;
end;  // TExternalFilterTables.GetInputParam

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.GetInputParamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do begin
    if Assigned(TExternalFilterTable(Items[i]).InputParam) then
      Result := Result + 1;
  end;
end;  // TExternalFilterTables.GetInputParamCount

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.GetItems(AIndex: Integer): TExternalFilterTable;
begin
  Result := TExternalFilterTable(inherited Items[AIndex]);
end;  // TExternalFilterTables.GetItems

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.IndexOf(AItem: TExternalFilterTable): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TExternalFilterTables.IndexOf

{-------------------------------------------------------------------------------
}
procedure TExternalFilterTables.Insert(AIndex: Integer; AItem: TExternalFilterTable);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TExternalFilterTables.Insert

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.Last: TExternalFilterTable;
begin
  Result := TExternalFilterTable(inherited Last);
end;  // TExternalFilterTables.Last

{-------------------------------------------------------------------------------
  Reads the ExternalFilterTables XML node for the report file.
}
procedure TExternalFilterTables.ReadXML(ANode: IXMLNode);
var
  i: Integer;
  lExternalFilterTable: TExternalFilterTable;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do begin
    if ANode.ChildNodes.Nodes[i].NodeName = EL_EXTERNAL_FILTER_FILE then begin
      lExternalFilterTable := TExternalFilterTable.Create(FConnection);
      Add(lExternalFilterTable);
      lExternalFilterTable.ReadXML(ANode.ChildNodes.Nodes[i]);
    end else
      raise EExternalFilterFileException.CreateNonCritical(Format(ResStr_ProblemParsingSection,
          ['externalfilterfiles']));
  end;
end;  // TExternalFilterTables.ReadXML

{-------------------------------------------------------------------------------
}
function TExternalFilterTables.Remove(AItem: TExternalFilterTable): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TExternalFilterTables.Remove

{-------------------------------------------------------------------------------
}
procedure TExternalFilterTables.SetItems(AIndex: Integer; AItem: TExternalFilterTable);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TExternalFilterTables.SetItems

//==============================================================================
{ TExternalFilter }
//==============================================================================

{-------------------------------------------------------------------------------
  Splits AFilterLine into its constituent parts and stores values in fields
}
constructor TExternalFilter.Create(const AFilterLine: string);
var
  S: string;
  TabPos: integer;
begin
  inherited Create;
  S := AFilterLine;
  TabPos := Pos(#9, S);
  if (TabPos = 0) then
    raise EExternalFilterFileException.CreateNonCritical(Format(
        ResStr_ExternalFilterInvalidLine, [AFilterLine]));
  FTableName := Copy(S, 1, TabPos - 1);
  S := Copy(S, TabPos + 1, MaxInt);
  TabPos := Pos(#9, S);
  if (TabPos = 0) then begin
    FItemKey := S;
    FDescription := '';
  end else begin
    FItemKey := Copy(S, 1, TabPos - 1);
    FDescription := Copy(S, TabPos + 1, MaxInt);
  end;    // if (TabPos = 0)
end;    // TExternalFilter.Create

end.
