{===============================================================================
  Unit:        CSVTables.pas

  Description: This unit contains the CSV temporary tables.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 9 $
    $Date: 18/02/08 14:16 $
    $Author: Johndurman $
===============================================================================}

unit CRCSVTables;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, SMIBase, SMI2TXT,
  Forms, Dialogs, XMLIntf, XMLDoc, DB, ExceptionForm, DBClient, Grids,
  CRCommonClasses, ADODB;

resourcestring
  ResStr_CSVFileNotFound = 'The expected CSV file could not be found at the following location: %s';
  ResStr_CSVFileOpen = 'The data in the CSV file ''%s'' could not be read.'#13#10'If the file is still open in another application (e.g. Excel) please close it before repeating this action.';

type
  ECSVTableException = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Reads the CSVColumn XML node for the report file.
  }
  TCSVColumn = class(TObject)
  private
    FDataSize: Integer;
    FDataType: String;
    FFieldName: String;
    procedure Validate(ANode: IXMLNode);
  public
    procedure ReadXML(ANode: IXMLNode);
    property DataSize: Integer read FDataSize;
    property DataType: String read FDataType;
    property FieldName: String read FFieldName;
  end;
  
  TCSVColumns = class(TList)
  private
    function GetItems(AIndex: Integer): TCSVColumn;
    procedure SetItems(AIndex: Integer; AItem: TCSVColumn);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TCSVColumn): Integer;
    procedure Clear; override;
    function Extract(AItem: TCSVColumn): TCSVColumn;
    function First: TCSVColumn;
    function IndexOf(AItem: TCSVColumn): Integer;
    procedure Insert(AIndex: Integer; AItem: TCSVColumn);
    function Last: TCSVColumn;
    function Remove(AItem: TCSVColumn): Integer;
    property Items[AIndex: Integer]: TCSVColumn read GetItems write SetItems; default;
  end;
  
  TCSVTable = class(TObject)
    function FieldSpecified(const AName: string): Boolean;
  private
    FClientDataSet: TClientDataSet;
    FConnection: TADOConnection;
    FCSVFields: TCSVColumns;
    FDescription: String;
    FFileName: String;
    FInputParam: TInputParam;
    FName: String;
    FSMImportFromText: TSMImportFromText;
    procedure AddColumns(ANode: IXMLNode);
    procedure CreateStructure(Sender: TObject; Columns: TSMIColumns);
    procedure CreateTable;
    function GetColumnNames: String;
    function GetColumnParameters: String;
    function GetRowData: String;
    procedure Initialise;
    procedure InsertData;
    procedure LoadCSV;
    function RequiresQuotes(AType: string): Boolean;
    procedure Validate(ANode: IXMLNode);
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    procedure BuildTable;
    procedure DropTable;
    procedure ReadXML(ANode: IXMLNode);
    property CSVFields: TCSVColumns read FCSVFields write FCSVFields;
    property Description: String read FDescription;
    property FileName: String read FFileName;
    property InputParam: TInputParam read FInputParam;
    property Name: String read FName;
  end;
  
  TCSVTables = class(TList)
  private
    FConnection: TADOConnection;
    function GetInputParam(Index: Integer): TInputParam;
    function GetInputParamCount: Integer;
    function GetItems(AIndex: Integer): TCSVTable;
    procedure SetItems(AIndex: Integer; AItem: TCSVTable);
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    function Add(AItem: TCSVTable): Integer;
    procedure Clear; override;
    function Extract(AItem: TCSVTable): TCSVTable;
    function First: TCSVTable;
    function IndexOf(AItem: TCSVTable): Integer;
    procedure Insert(AIndex: Integer; AItem: TCSVTable);
    function Last: TCSVTable;
    procedure ReadXML(ANode: IXMLNode);
    function Remove(AItem: TCSVTable): Integer;
    property InputParam[Index: Integer]: TInputParam read GetInputParam;
    property InputParamCount: Integer read GetInputParamCount;
    property Items[AIndex: Integer]: TCSVTable read GetItems write SetItems; default;
  end;
  

implementation

uses
  CRConstants, DatabaseAccessADO, ResourceStrings, ComObj, CRReportSQL;

{-==============================================================================
    TCSVColumns
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TCSVColumns.Create;
begin
  inherited Create;
end;  // TCSVColumns.Create 

{-------------------------------------------------------------------------------
}
destructor TCSVColumns.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TCSVColumns.Destroy 

{-------------------------------------------------------------------------------
}
function TCSVColumns.Add(AItem: TCSVColumn): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TCSVColumns.Add 

{-------------------------------------------------------------------------------
}
procedure TCSVColumns.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TCSVColumns.Clear 

{-------------------------------------------------------------------------------
}
function TCSVColumns.Extract(AItem: TCSVColumn): TCSVColumn;
begin
  Result := TCSVColumn(inherited Extract(AItem));
end;  // TCSVColumns.Extract 

{-------------------------------------------------------------------------------
}
function TCSVColumns.First: TCSVColumn;
begin
  Result := TCSVColumn(inherited First);
end;  // TCSVColumns.First 

{-------------------------------------------------------------------------------
}
function TCSVColumns.GetItems(AIndex: Integer): TCSVColumn;
begin
  Result := TCSVColumn(inherited Items[AIndex]);
end;  // TCSVColumns.GetItems 

{-------------------------------------------------------------------------------
}
function TCSVColumns.IndexOf(AItem: TCSVColumn): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TCSVColumns.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TCSVColumns.Insert(AIndex: Integer; AItem: TCSVColumn);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TCSVColumns.Insert 

{-------------------------------------------------------------------------------
}
function TCSVColumns.Last: TCSVColumn;
begin
  Result := TCSVColumn(inherited Last);
end;  // TCSVColumns.Last 

{-------------------------------------------------------------------------------
}
function TCSVColumns.Remove(AItem: TCSVColumn): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TCSVColumns.Remove 

{-------------------------------------------------------------------------------
}
procedure TCSVColumns.SetItems(AIndex: Integer; AItem: TCSVColumn);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TCSVColumns.SetItems 

{-==============================================================================
    TCSVColumn
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TCSVColumn.ReadXML(ANode: IXMLNode);
begin
  with ANode do begin
    if HasAttribute(AT_DATATYPE) then
      FDataType := Attributes[AT_DATATYPE]
    else
      raise ECSVTableException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                                ['DataType', 'CSVColumn']));
    // Now we know the datatype, we can validate the rest of the contents
    // of the node.
    Validate(ANode);
  
    if HasAttribute(AT_DATASIZE) then FDataSize := Attributes[AT_DATASIZE];
    if HasAttribute(AT_FIELDNAME) then FFieldName := Attributes[AT_FIELDNAME];
  end;
end;  // TCSVColumn.ReadXML 

{-------------------------------------------------------------------------------
  Ensure that the CSVColumn has a name. Also, if the datatype of the column is 'text', then a
      DataSize attribute must be found and read.
}
procedure TCSVColumn.Validate(ANode: IXMLNode);
begin
    with ANode do begin
      if (not HasAttribute(AT_FIELDNAME)) then
        raise ECSVTableException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                                  ['Name', 'CSVColumn']));
      if (not HasAttribute(AT_DATASIZE)) and (LowerCase(FDataType) = 'text') then
        raise ECSVTableException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                                  ['DataSize', 'CSVColumn']));
  end;
end;  // TCSVColumn.Validate 

{-==============================================================================
    TCSVTable
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor. 
}
constructor TCSVTable.Create(AConnection: TADOConnection);
begin
  inherited Create;
  
  FCSVFields := TCSVColumns.Create;
  FConnection := AConnection;
  
  FSMImportFromText := TSMImportFromText.Create(Application);
  FSMImportFromText.Options := FSMImportFromText.Options - [soShowMessage];
  FSMImportFromText.OnCreateStructure := CreateStructure;
  FClientDataSet := TClientDataSet.Create(Application);
  Initialise;
end;  // TCSVTable.Create 

{-------------------------------------------------------------------------------
  Destructor 
}
destructor TCSVTable.Destroy;
begin
  FClientDataSet.Free;
  FSMImportFromText.Free;
  FCSVFields.Free;
  FInputParam.Free;
  inherited;
end;  // TCSVTable.Destroy 

{-------------------------------------------------------------------------------
  Method that adds columns to the FCSVFields list.
}
procedure TCSVTable.AddColumns(ANode: IXMLNode);
var
  lIndex: Integer;
  lCSVColumn: TCSVColumn;
begin
  if ANode.NodeName = EL_CSVCOLUMN then begin
    lCSVColumn := TCSVColumn.Create;
    lIndex := FCSVFields.Add(lCSVColumn);
    FCSVFields.Items[lIndex].ReadXML(ANode);
  end else
    raise ECSVTableException.CreateNonCritical(Format(ResStr_ProblemParsingSection, ['CSVTables']));
end;  // TCSVTable.AddColumns

{-------------------------------------------------------------------------------
  Create the temp table and populate it from the source CSV file.
}
procedure TCSVTable.BuildTable;
begin
  CreateTable;
  LoadCSV;
  InsertData;
end;  // TCSVTable.BuildTable

{-------------------------------------------------------------------------------
  Create the structure of the FClientDataset for the data from SMImport to go into.
}
procedure TCSVTable.CreateStructure(Sender: TObject; Columns: TSMIColumns);
var
  i: Integer;
begin
  FClientDataset.FieldDefs.Clear;

  for i := 0 to Columns.Count-1 do begin
    with FClientDataset.FieldDefs.AddFieldDef do begin
      Name := Columns[i].FieldName;
      DataType := ftString;
      // Mantis 212
      Size := 255;
    end;
  end;

  if (FClientDataSet.State <> dsInactive) then
    FClientDataSet.Close;
  FClientDataset.CreateDataset;
end;  // TCSVTable.CreateStructure

{-------------------------------------------------------------------------------
  Create the temporary table for the data read in from the CSV file to go into.
}
procedure TCSVTable.CreateTable;
var
  lSQL: String;
begin
  // Ensure that if for some reason the table already exists, it is dropped.
  FConnection.Execute(Format(SQL_DROPTEMPTABLE, [FName, FName]),
      cmdText, [eoExecuteNoRecords]);
  // Create the table;
  lSQL := Format(SQL_CREATETEMPTABLE, [FName, GetColumnParameters]);
  FConnection.Execute(lSQL, cmdText, [eoExecuteNoRecords]);
end;  // TCSVTable.CreateTable 

{-------------------------------------------------------------------------------
  Drop the temporary table. 
}
procedure TCSVTable.DropTable;
begin
  // If the user failed to enter a name for the temporary table, validation would
  // fail and the reporting process would be aborted. It would then come here
  // in the clean up, but FName would be ''. This would mean the SQL line
  // created below to drop the temporary tables would fail. Hence, only run the
  // line if FName <> ''.
  if FName <> '' then
    FConnection.Execute(Format(SQL_DROPTEMPTABLE, [FName, FName]), cmdText,
        [eoExecuteNoRecords]);
end;  // TCSVTable.DropTable 

{-------------------------------------------------------------------------------
  Returns true if a field in a csv file has been specified in the definition of the csv file
      in the XML.  If not, then the field is ignored.
}
function TCSVTable.FieldSpecified(const AName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FCSVFields.Count-1 do
    if CompareText(FCSVFields[i].FieldName, AName)=0 then begin
      Result := True;
      Break;
    end;
end;  // TCSVTable.FieldSpecified 

{-------------------------------------------------------------------------------
  Build a string that lists all of the names of the columns of the temporary table. 
}
function TCSVTable.GetColumnNames: String;
var
  lIdx: Integer;
begin
  Result := '';
  with FCSVFields do
    for lIdx := 0 to Count -1 do begin
      if lIdx <> 0 then Result := Result + ', ';
      Result := Result + Items[lIdx].FieldName;
    end;
end;  // TCSVTable.GetColumnNames 

{-------------------------------------------------------------------------------
  Build a string that returns the names of all of the columns and their datatypes (and sizes
      where necessary).
}
function TCSVTable.GetColumnParameters: String;
var
  lIdx: Integer;
begin
  Result := '';
  
  with FCSVFields do
    for lIdx := 0 to Count -1 do begin
      if lIdx <> 0 then Result := Result + ', ';
      Result := Result + Items[lIdx].FieldName + ' ';
      Result := Result + Items[lIdx].DataType;
      if Items[lIdx].DataSize <> 0 then
        Result := Result + ' (' + IntToStr(Items[lIdx].DataSize) + ')';
      if (Pos('CHAR', UpperCase(Items[lIdx].DataType))>0) or
          (Pos('TEXT', UpperCase(Items[lIdx].DataType))>0) then
        Result := Result + ' COLLATE SQL_Latin1_General_CP1_CI_AS';
    end;
end;  // TCSVTable.GetColumnParameters

{-------------------------------------------------------------------------------
  Build a string containing the data to be inserted for a row.
}
function TCSVTable.GetRowData: String;
var
  lIdx: Integer;
  msg: string;
begin
  Result := '';
  with FClientDataSet do
    for lIdx := 0 to FCSVFields.Count-1 do begin
      if FieldList.IndexOf(FCSVFields.Items[lIdx].FieldName)=-1 then begin
        msg := Format(ResStr_CSVFieldNotFound, [FCSVFields.Items[lIdx].FieldName]);
        if FFileName <> '' then
          msg := msg + #13#10#13#10 + FFileName;
        raise ECSVTableException.CreateNonCritical(msg);
      end;
      if lIdx <> 0 then Result := Result + ', ';
      if RequiresQuotes(LowerCase(FCSVFields.Items[lIdx].DataType)) then
        Result := Result + '''';
      Result := Result + FieldByName(FCSVFields.Items[lIdx].FieldName).Text;
      if RequiresQuotes(LowerCase(FCSVFields.Items[lIdx].DataType)) then
        Result := Result + '''';
    end;
end;  // TCSVTable.GetRowData

{-------------------------------------------------------------------------------
  Initialise the FSMImportFromText object. 
}
procedure TCSVTable.Initialise;
begin
  with FSMImportFromText do begin
    AnimatedStatus := False;
    FieldDelimiter := fdComma;
    RowFirst := 2;
    DataSet := FClientDataSet;
  end;
end;  // TCSVTable.Initialise 

{-------------------------------------------------------------------------------
  Insert data into the temporary table one row at a time. 
}
procedure TCSVTable.InsertData;
var
  lSQL, lColumnHeadings, lRowData: String;
begin
  lColumnHeadings := GetColumnNames;
  
  with FClientDataSet do begin
    if not (BOF and EOF) then
      First;
    while not EOF do begin
      lRowData := GetRowData;
      lSQL := Format(SQL_INSERTRECORD, [FName, lColumnHeadings, lRowData]);
      try
        FConnection.Execute(lSQL, cmdText, [eoExecuteNoRecords]);
      except on E:EOLEException do
        raise EReportSQLException.CreateNonCritical(Format(ResStr_SQLErrorDetail, [E.Message, lSQL]));
      end;
      Next;
    end;
  end;
end;  // TCSVTable.InsertData 

{-------------------------------------------------------------------------------
  Load the CSV file. 
}
procedure TCSVTable.LoadCSV;
var
  lFilename: String;
begin
  if FFileName <> '' then lFilename := FFilename
                     else lFilename := Copy(InputParam.Values[0], 2, Length(InputParam.Values[0])-2);
  if not FileExists(lFileName) then
    raise ECSVTableException.CreateNonCritical(Format(ResStr_CSVFileNotFound, [lFileName]));

  try
    FSMImportFromText.SourceFileName := lFilename;
    FSMImportFromText.Execute;
  except on E:Exception do
    raise ECSVTableException.CreateNonCritical(Format(ResStr_CSVFileOpen, [lFilename]), E);
  end;
end;  // TCSVTable.LoadCSV 

{-------------------------------------------------------------------------------
  Reads the CSVTable XML node for the report file. 
}
procedure TCSVTable.ReadXML(ANode: IXMLNode);
var
  i: Integer;
begin
  with ANode do begin
    Validate(ANode);
    if HasAttribute(AT_FILENAME) then FFileName := Attributes[AT_FILENAME];
    if HasAttribute(AT_NAME) then FName := Attributes[AT_NAME];
  
    for i := 0 to ChildNodes.Count - 1 do begin
      if ChildNodes.Nodes[i].NodeName = EL_CSVCOLUMN then
        AddColumns(ChildNodes.Nodes[i])
      else if ChildNodes.Nodes[i].NodeName = EL_CSVFILE then
        if ChildNodes.Nodes[i].HasAttribute(AT_DESCRIPTION) then
          FDescription := ChildNodes.Nodes[i]. Attributes[AT_DESCRIPTION]
      else
        raise ECSVTableException.CreateNonCritical(Format(ResStr_ProblemParsingSection,
            ['CSVTable']));
    end;
  end;
  if FFileName='' then
    FInputParam := TInputParam.Create(dtCSVFile, FDescription, nil);
end;  // TCSVTable.ReadXML 

{-------------------------------------------------------------------------------
  See if the given datatype requires quotes around it. 
}
function TCSVTable.RequiresQuotes(AType: string): Boolean;
begin
  Result := (Pos('char', AType)>0) or
            (Pos('text', AType)>0) or
            (Pos('datetime', AType)>0);
end;  // TCSVTable.RequiresQuotes 

{-------------------------------------------------------------------------------
  Validate the contents of the node to check all the required attributes are present. 
}
procedure TCSVTable.Validate(ANode: IXMLNode);
var
  i: Integer;
  lDescription: String;
begin
  with ANode do begin
    if (not HasAttribute(AT_NAME)) then
      raise ECSVTableException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                            ['Name', 'CSVTable']));
    if Pos('#', Attributes[AT_NAME]) <> 1 then
      raise ECSVTableException.CreateNonCritical(Format(ResStr_TemporaryTablePrefixError,
                                                [Attributes[AT_NAME]]));
    // If Filename is not specified there must be a <CSVFile> tag to supply
    // the file location
    if (not HasAttribute(AT_FILENAME)) then begin
      lDescription := '';
      for i := 0 to ChildNodes.Count - 1 do
        if (ChildNodes.Nodes[i].NodeName = EL_CSVFILE) and
           (ChildNodes.Nodes[i].HasAttribute(AT_DESCRIPTION)) then
            lDescription := ChildNodes.Nodes[i].Attributes[AT_DESCRIPTION];
      if lDescription = '' then
        raise ECSVTableException.CreateNonCritical(Format(ResStr_ParameterMissing,
                                              ['Description', 'CSVFile']));
    end;
  end;
end;  // TCSVTable.Validate 

{-==============================================================================
    TCSVTables
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  Requires connection that the report is being created on. 
}
constructor TCSVTables.Create(AConnection: TADOConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;  // TCSVTables.Create 

{-------------------------------------------------------------------------------
}
destructor TCSVTables.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TCSVTables.Destroy 

{-------------------------------------------------------------------------------
}
function TCSVTables.Add(AItem: TCSVTable): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TCSVTables.Add 

{-------------------------------------------------------------------------------
}
procedure TCSVTables.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TCSVTables.Clear 

{-------------------------------------------------------------------------------
}
function TCSVTables.Extract(AItem: TCSVTable): TCSVTable;
begin
  Result := TCSVTable(inherited Extract(AItem));
end;  // TCSVTables.Extract 

{-------------------------------------------------------------------------------
}
function TCSVTables.First: TCSVTable;
begin
  Result := TCSVTable(inherited First);
end;  // TCSVTables.First 

{-------------------------------------------------------------------------------
  Accessor method - allows access to CSV table and SQL input parameters. 
}
function TCSVTables.GetInputParam(Index: Integer): TInputParam;
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
end;  // TCSVTables.GetInputParam 

{-------------------------------------------------------------------------------
}
function TCSVTables.GetInputParamCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do begin
    if Assigned(TCSVTable(Items[i]).InputParam) then
      Result := Result + 1;
  end;
end;  // TCSVTables.GetInputParamCount 

{-------------------------------------------------------------------------------
}
function TCSVTables.GetItems(AIndex: Integer): TCSVTable;
begin
  Result := TCSVTable(inherited Items[AIndex]);
end;  // TCSVTables.GetItems 

{-------------------------------------------------------------------------------
}
function TCSVTables.IndexOf(AItem: TCSVTable): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TCSVTables.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TCSVTables.Insert(AIndex: Integer; AItem: TCSVTable);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TCSVTables.Insert 

{-------------------------------------------------------------------------------
}
function TCSVTables.Last: TCSVTable;
begin
  Result := TCSVTable(inherited Last);
end;  // TCSVTables.Last 

{-------------------------------------------------------------------------------
  Reads the CSVTables XML node for the report file. 
}
procedure TCSVTables.ReadXML(ANode: IXMLNode);
var
  i: Integer;
  lCSVTable: TCSVTable;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do begin
    if ANode.ChildNodes.Nodes[i].NodeName = EL_CSVTABLE then begin
      lCSVTable := TCSVTable.Create(FConnection);
      Add(lCSVTable);
      lCSVTable.ReadXML(ANode.ChildNodes.Nodes[i]);
    end else
      raise ECSVTableException.CreateNonCritical(Format(ResStr_ProblemParsingSection,
          ['CSVTables']));
  end;
end;  // TCSVTables.ReadXML 

{-------------------------------------------------------------------------------
}
function TCSVTables.Remove(AItem: TCSVTable): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TCSVTables.Remove 

{-------------------------------------------------------------------------------
}
procedure TCSVTables.SetItems(AIndex: Integer; AItem: TCSVTable);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TCSVTables.SetItems 


end.
