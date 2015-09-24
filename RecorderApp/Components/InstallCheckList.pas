unit InstallCheckList;

{This unit contains a set of objects which take care of the install checklist
functionality.
Each object inherits from TImporter and unsually takes care of the import of one
database entity. The TImporter class has a abstract Import method which performs
the Import of that database entity.

We start by creating an instance of TTLImporter (Taxon List Importer). This has a
TTLImporter.TLKey_Source input property which holds the taxon list key in the source
database of the Taxon list we wish to import. We then call the TTLImporter.Import
method. Within this method firstly the Taxon List record is copied across to the
destination database and the new key value is determined. (See (1) Below). Secondly
a local Instance of TLVImporter is created because we wish to import the taxon
list version records associated with the taxon list. We set both the input properties
TLVImporter.TLKey_Source & TLVImporter.TLKey_Dest which represent the taxon list
source and destination keys. TLVImporter.Import copies each of the  taxon list version
records and creates an instance of TLIImporter which imports the taxon list items of
each taxon list version in turn. TLIImporter creates instances of TDImporter (taxon
designation importer) and TVImporter. The process continues to the ultimate end
tables which do not require any associated records in other tables.

Within TLIImporter.Import it is worth noting that before a taxon list item can be
added we must add a taxon version so TVImporter is created BEFORE adding the Taxon
list item. TVImporter.TVKey_Dest is a read only parameter which returns the new key
for the taxon version after the TVImporter.Import method has been called. We then
add the Taxon list item and obtain the new destination key FTLIKey_Dest. We then
must import the associated taxon designation records so an instance of TDImporter
is created AFTER the Taxon list item has been added.

{(1) Each object has the ability to copy records across into the destination database
either by using the same key in the source table as in the destination table (FNewKeys = False)
or by creating a new key in the destination table (FNewKeys = True). The latter is
redundant code but remains in place in case the ability is required at a later stage.
i.e. copying lists etc.}


interface

uses
  DataClasses, Db, Classes, JNCCDatasets, ExceptionForm, InstallCheckListData;

type
  EInstallCheckList = Class(TExceptionPath);

  //----------------------------------------------------------------------------
  { Importer Base Class }
  TImporter = class(TObject)
  private
  protected
    FqryDest,FqrySource : TJNCCQuery;
    FNewKeys : Boolean;
    FDatamodule : TdmInstallCheckList;

    function GetDestKey(const ATableName, AKeyFieldName:string; const iOldKey : TKeyString) : TKeyString;
    procedure SetEditChangeFields;
    procedure AssignField(const iFieldName : string);
    procedure SetDatamodule(iDatamodule : TdmInstallCheckList);
  public
    property Datamodule : TdmInstallCheckList write SetDatamodule;
    function PutIntoEditAppendState(const AKeyField, AKeyValue:string) : TDataSetState;
    procedure Import; virtual; abstract;
    procedure Rollback; virtual;
    constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { LookUp tables - Import whole table }
  TLUImporter = class(TImporter)
    private
    protected
    public
      procedure Import; override;
  end;

  //----------------------------------------------------------------------------
  { Taxon Designation Type Importer }
  TTDTImporter = class(TImporter)
    private
      FTDTKey_Source, FTDTKey_Dest : TKeyString; // internal variables for Taxon Designation Type Keys
    protected
      procedure AddTDTRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
    public
      procedure Import; override;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon List Type Importer }
  TTLTImporter = class(TImporter)
    private
      FTLTKey_Source, FTLTKey_Dest : TKeyString; // internal variables for Taxon Designation Type Keys
    protected
      procedure AddTLTRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
    public
      procedure Import; override;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Rank Importer }
  TTRImporter = class(TImporter)
    private
      FTRKey_Source, FTRKey_Dest : TKeyString; // internal variables for Taxon Designation Type Keys
    protected
      procedure AddTRRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
    public
      procedure Import; override;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Name Type Importer }
  TNTImporter = class(TImporter)
    private
      FTNTKey_Source, FTNTKey_Dest : TKeyString; // internal variables for Taxon Designation Type Keys
    protected
      procedure AddTNTRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
    public
      procedure Import; override;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Main Tables - Import Required Records }
  { Taxon List Importer }
  TTLImporter = class(TImporter)
    private
      FTLKey_Source,FTLKey_Dest : TKeyString;  // Taxon List Version Input & Output parameters
    protected
      procedure AddTLRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      { properties }
      property TLKey_Source : TKeyString write FTLKey_Source;
      property TLKey_Dest : TKeyString read FTLKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon List Version Importer }
  TTLVImporter = class(TImporter)
    private
      FTLKey_Source,FTLKey_Dest : TKeyString;  // Taxon List Input values
      FTLVKey_Source,FTLVKey_Dest : TKeyString;  // Taxon Version InternalVariables
    protected
      procedure AddTLVRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      { properties }
      property TLKey_Source : TKeyString write FTLKey_Source;
      property TLKey_Dest : TKeyString write FTLKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon List Item Importer }
  TTLIImporter = class(TImporter)
    private
      FslTLIKey_Source, FslTLIKey_Dest : TStringList;
      FTLVKey_Source,FTLVKey_Dest : TKeyString;  // Taxon List Version Input values
      FTLIKey_Source,FTLIKey_Dest : TKeyString;  // Taxon List Item Internal variables
      FTVKey_Source,FTVKey_Dest : TKeyString;  // Taxon Version Input and Output variables
    protected
      procedure AddTLIRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
      procedure AddToKeyLists;
      function FindNewKey(iOriginalKey : TKeyString) : TKeyString;
    public
      procedure Import; override;
      { properties }
      property TLVKey_Source : TKeyString write FTLVKey_Source;
      property TLVKey_Dest : TKeyString write FTLVKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Version Importer}
  TTVImporter = class(TImporter)
    private
      FTVKey_Source, FTVKey_Dest : TKeyString; // Taxon Version Input & Output values
      FTKey_Source, FTKey_Dest : TKeyString; // Taxon Internal variables
    protected
      procedure AddTVRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables    
      procedure SetParams;
    public
      procedure Import; override;
      { properties }
      property TVKey_Source : TKeyString write FTVKey_Source;
      property TVKey_Dest : TKeyString read FTVKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Importer }
  TTImporter = class(TImporter)
    private
      FTKey_Source, FTKey_Dest : TKeyString;
    protected
      procedure AddTRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      { properties }
      property TKey_Source : TKeyString write FTKey_Source;
      property TKey_Dest : TKeyString read FTKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Fact Importer}
  TTFImporter = class(TImporter)
    private
      FTVKey_Source, FTVKey_Dest : TKeyString; // Input Values
      FSKey_Source, FSKey_Dest : TKeyString; // Internal Vars
      FTFKey_Source,FTFKey_Dest : TKeyString;
    protected
      procedure AddTFRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      property TVKey_Source : TKeyString write FTVKey_Source;
      property TVKey_Dest : TKeyString write FTVKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Sources Importer}
  TTSImporter = class(TImporter)
    private
      FTKey_Source, FTKey_Dest : TKeyString; // Input Values
      FSKey_Source, FSKey_Dest : TKeyString; // Internal Vars
    protected
      procedure AddTSRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      property TKey_Source : TKeyString write FTKey_Source;
      property TKey_Dest : TKeyString write FTKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Taxon Designation Importer}
  TTDImporter = class(TImporter)
    private
      FTLIKey_Source, FTLIKey_Dest : TKeyString;
      FSKey_Source, FSKey_Dest : TKeyString; // Internal Vars
      FTDKey_Source,FTDKey_Dest : TKeyString;
    protected
      procedure AddTDRecord;
      procedure GetSourceVars;  // procedure which obtains the source variables
      procedure SetParams;
    public
      procedure Import; override;
      property TLIKey_Source : TKeyString write FTLIKey_Source;
      property TLIKey_Dest : TKeyString write FTLIKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Source Importer}
  TSImporter = class(TImporter)
    private
      FSKey_Source, FSKey_Dest : TKeyString;
      FSInternal : Boolean;
    protected
      procedure AddSRecord;
      procedure SetParams;
      procedure GetSourceVars;  // procedure which obtains the source variables
    public
      procedure Import; override;
      property SKey_Source : TKeyString write FSKey_Source;
      property SKey_Dest : TKeyString read FSKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { Reference Importer }
  TRSImporter = class(TImporter)
    private
      FRSKey_Source, FRSKey_Dest : TKeyString;
    protected
      procedure AddRSRecord;
      procedure SetParams;
    public
      procedure Import; override;
      property RSKey_Source : TKeyString write FRSKey_Source;
      property RSKey_Dest : TKeyString write FRSKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

  //----------------------------------------------------------------------------
  { File Source Importer }
  TFSImporter = class(TImporter)
    private
      FFSKey_Source, FFSKey_Dest : TKeyString;
    protected
      procedure AddFSRecord;
      procedure SetParams;
    public
      procedure Import; override;
      property FSKey_Source : TKeyString write FFSKey_Source;
      property FSKey_Dest : TKeyString write FFSKey_Dest;
      constructor Create(iDatamodule : TdmInstallCheckList);
  end;

//==============================================================================
implementation

uses
  GeneralData, Dialogs;

const
  ST_INVALID_KEY = '* NOT IN TABLE *';

//==============================================================================
constructor TImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create;
  FDatamodule:= iDatamodule;
  FNewKeys   := False;
end;

//------------------------------------------------------------------------------
procedure TImporter.Rollback;
begin
  // Do nothing - Neither called from anywhere nor implemented in derived classes. 
end;

//------------------------------------------------------------------------------
function TImporter.GetDestKey(const ATableName, AKeyFieldName:string; const iOldKey : TKeyString) : TKeyString;
begin
  if FNewKeys then
    Result := dmGeneralData.GetNextKey(ATableName,AKeyFieldName)
  else
    Result := iOldKey;
end;

//------------------------------------------------------------------------------
procedure TImporter.AssignField(const iFieldName : string);
var lSrcField, lDestField:TField;
begin
  // FindField and FieldByName are fairly slow, so find the fields once only
  lSrcField :=FqrySource.FindField(iFieldName);
  lDestField:=FqryDest.FindField(iFieldName);
  // if both source and destination fields exist then assign values
  if Assigned(lSrcField) and Assigned(lDestField) then
    lDestField.Assign(lSrcField);
    //FqryDest.FieldByName(iFieldName).Assign(FqrySource.FieldByName(iFieldName));
end;

//------------------------------------------------------------------------------
procedure TImporter.SetDatamodule(iDatamodule: TdmInstallCheckList);
begin
  FDatamodule := iDatamodule
end;

//------------------------------------------------------------------------------
function TImporter.PutIntoEditAppendState(const AKeyField, AKeyValue:string) : TDataSetState;
begin
  with FqryDest do
  begin
    if FNewKeys then
      Append
    else
      // check whether the record already exists and if it does then we edit it
      if Locate(AKeyField,AKeyValue,[loCaseInsensitive]) then
        Edit
      else
        Append;
    Result := State;
  end;
end;

//------------------------------------------------------------------------------
procedure TImporter.SetEditChangeFields;
begin
  AssignField('ENTERED_BY');
  AssignField('ENTRY_DATE');
  AssignField('CHANGED_BY');
  AssignField('CHANGED_DATE');
  AssignField('SYSTEM_SUPPLIED_DATA');
end;

//==============================================================================
{ Taxon Version Importer - TTVImporter }
constructor TTVImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTVSource; // synonym for source query
  FqryDest  := FDatamodule.qryTVDest;   // synonym for source query
end;

//------------------------------------------------------------------------------
procedure TTVImporter.AddTVRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_VERSION_KEY',FTVKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_VERSION_KEY').AsString := FTVKey_Dest;

      // foreign keys
      if Assigned(FindField('TAXON_KEY')) then FieldByName('TAXON_KEY').AsString := FTKey_Dest;
      // copy fields
      AssignField('ATTRIBUTE');
      AssignField('AUTHORITY');
      AssignField('DATE_FROM');
      AssignField('DATE_TO');
      AssignField('COMMENT');
      AssignField('VALIDATION_LEVEL');
      AssignField('UK_NATIVE');

      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTVImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_VERSION_KEY').Value := FTVKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_VERSION_KEY').Value := FTVKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_VERSION_KEY').Value := ST_INVALID_KEY;
end;

//------------------------------------------------------------------------------
procedure TTVImporter.GetSourceVars;
begin
  FTKey_Source := FqrySource.FieldByName('TAXON_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTVImporter.Import;
var
  lTImporter : TTImporter;
  lTFImporter : TTFImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      GetSourceVars;   // get some source values
      // Import new Taxon
      lTImporter := TTImporter.Create(FDatamodule);
      try
        lTImporter.TKey_Source := FTKey_Source;
        lTImporter.Import;
        FTKey_Dest := lTImporter.TKey_Dest;  // get new key value in import table
      finally
        lTImporter.Free;
      end;

      FTVKey_Dest := GetDestKey('TAXON_VERSION','Taxon_Version_Key',FTVKey_Source); // obtain new key for TLI
      AddTVRecord;
      // Import Taxon Facts
      lTFImporter := TTFImporter.Create(FDatamodule);
      try
        lTFImporter.TVKey_Source := FTVKey_Source;
        lTFImporter.TVKey_Dest := FTVKey_Dest;
        lTFImporter.Import;
      finally
        lTFImporter.Free;
      end;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//==============================================================================
{ Taxon Importer - TTImporter }
constructor TTImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTSource; // synonym for source query
  FqryDest  := FDatamodule.qryTDest; // synonym for source query
end;

//------------------------------------------------------------------------------
procedure TTImporter.AddTRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_KEY',FTKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_KEY').AsString := FTKey_Dest;
      // copy fields
      AssignField('ITEM_NAME');
      AssignField('AUTHORITY');
      AssignField('INTRODUCED_VAGUE_DATE_START');
      AssignField('INTRODUCED_VAGUE_DATE_END');
      AssignField('INTRODUCED_VAGUE_DATE_TYPE');
      AssignField('TAXON_NAME_TYPE_KEY');
      AssignField('LANGUAGE');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTImporter.GetSourceVars;
begin
  // no keys to get
end;

//------------------------------------------------------------------------------
procedure TTImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_KEY').Value := FTKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_KEY').Value := FTKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_KEY').Value := ST_INVALID_KEY;
end;

//------------------------------------------------------------------------------
procedure TTImporter.Import;
var
  lTSImporter : TTSImporter;
begin
 try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      GetSourceVars;   // get some source values
      FTKey_Dest := GetDestKey('TAXON','Taxon_Key',FTKey_Source); // obtain new key for TLI
      AddTRecord;
      // Import Taxon Sources
      lTSImporter := TTSImporter.Create(FDatamodule);
      try
        lTSImporter.FTKey_Source := FTKey_Source;
        lTSImporter.FTKey_Dest := FTKey_Dest;
        lTSImporter.Import;
      finally
        lTSImporter.Create(FDatamodule);
      end;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//==============================================================================
{ Taxon Fact Importer - TTFImporter }
constructor TTFImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTFSource;
  FqryDest  := FDatamodule.qryTFDest;
end;

//------------------------------------------------------------------------------
procedure TTFImporter.AddTFRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_FACT_KEY',FTFKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_FACT_KEY').AsString := FTFKey_Dest;

      // foreign Keys
      if Assigned(FindField('TAXON_VERSION_KEY')) then FieldByName('TAXON_VERSION_KEY').AsString := FTVKey_Dest;
      if Assigned(FindField('SOURCE_KEY')) then FieldByName('SOURCE_KEY').AsString := FSKey_Dest;
      // copy fields
      AssignField('TITLE');
      AssignField('DATA');
      AssignField('TYPE');
      AssignField('FACT_VAGUE_DATE_START');
      AssignField('FACT_VAGUE_DATE_END');
      AssignField('FACT_VAGUE_DATE_TYPE');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTFImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_VERSION_KEY').Value := FTVKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_VERSION_KEY').Value := FTVKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_VERSION_KEY').Value := ST_INVALID_KEY;
end;

//------------------------------------------------------------------------------
procedure TTFImporter.GetSourceVars;
begin
  with FqrySource do
  begin
    FSKey_Source := FieldByName('SOURCE_KEY').AsString;
    FTFKey_Source:= FieldByName('TAXON_FACT_KEY').AsString;
  end;
end;

//------------------------------------------------------------------------------
procedure TTFImporter.Import;
var
  lSImporter : TSImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      while not FqrySource.Eof do
      begin
        GetSourceVars; // get some source values
        // Import Source Details
        lSImporter := TSImporter.Create(FDatamodule);
        try
          lSImporter.SKey_Source := FSKey_Source;
          lSImporter.Import;
          FSKey_Dest := lSImporter.SKey_Dest;  // get new key value in import table
        finally
          lSImporter.Free;
        end;
        FTFKey_Dest := GetDestKey('TAXON_FACT','Taxon_Fact_Key',FTFKey_Source); // obtain new key for Source
        AddTFRecord;
        FqrySource.Next;
      end;   // while
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//==============================================================================
{ Taxon Designation Importer }
constructor TTDImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTDSource; // synonym for source query
  FqryDest  := FDatamodule.qryTDDest; // synonym for source query
end;

//------------------------------------------------------------------------------
procedure TTDImporter.AddTDRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_DESIGNATION_KEY',FTDKey_Dest) = dsInsert then
        // primary keys
      FieldByName('TAXON_DESIGNATION_KEY').AsString := FTDKey_Dest;
      // foreign keys
      if Assigned(FindField('TAXON_LIST_ITEM_KEY')) then FieldByName('TAXON_LIST_ITEM_KEY').AsString := FTLIKey_Dest;
      if Assigned(FindField('SOURCE_KEY')) then FieldByName('SOURCE_KEY').AsString := FSKey_Dest;
      // copy fields
      AssignField('DATE_FROM');
      AssignField('DATE_TO');
      AssignField('STATUS_GEOGRAPHIC_AREA');
      AssignField('STATUS_CONSTRAINT');
      AssignField('DETAIL');
      AssignField('TAXON_DESIGNATION_TYPE_KEY');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTDImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_LIST_ITEM_KEY').Value := FTLIKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_LIST_ITEM_KEY').Value := FTLIKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_LIST_ITEM_KEY').Value := ST_INVALID_KEY;
end;

//------------------------------------------------------------------------------
procedure TTDImporter.GetSourceVars;
begin
  with FqrySource do
  begin
    FSKey_Source := FieldByName('SOURCE_KEY').AsString;
    FTDKey_Source:= FieldByName('TAXON_DESIGNATION_KEY').AsString;
  end;
end;

//------------------------------------------------------------------------------
procedure TTDImporter.Import;
var
  lSImporter : TSImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      while not FqrySource.Eof do
      begin
        GetSourceVars; // get some source values
        // Import Source Details
        lSImporter := TSImporter.Create(FDatamodule);
        try
          lSImporter.SKey_Source := FSKey_Source;
          lSImporter.Import;
          FSKey_Dest := lSImporter.SKey_Dest;  // get new key value in import table
        finally
          lSImporter.Free;
        end;
        FTDKey_Dest := GetDestKey('TAXON_DESIGNATION','Taxon_Designation_Key',FTDKey_Source); // obtain new key for Source
        AddTDRecord;
        FqrySource.Next;
      end;   // while
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//==============================================================================
{ Taxon List Item Importer }
constructor TTLIImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTLISource; // synonym for source query
  FqryDest  := FDatamodule.qryTLIDest; // synonym for source query
end;

//------------------------------------------------------------------------------
procedure TTLIImporter.AddTLIRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_LIST_ITEM_KEY',FTLIKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_LIST_ITEM_KEY').AsString := FTLIKey_Dest;

      // foreign keys
      if Assigned(FindField('TAXON_LIST_VERSION_KEY')) then
        FieldByName('TAXON_LIST_VERSION_KEY').AsString := FTLVKey_Dest;
      if Assigned(FindField('TAXON_VERSION_KEY')) then
        FieldByName('TAXON_VERSION_KEY').AsString := FTVKey_Dest;
      // copy fields
      AssignField('TAXON_LIST_VERSION_TO');
      AssignField('SORT_CODE');
      AssignField('LST_ITM_CODE');
      AssignField('TAXON_RANK_KEY');
      SetEditChangeFields;
      // clear fields to be populated on second pass
      if Assigned(FindField('PREFERRED_NAME')) then FieldByName('PREFERRED_NAME').Clear;
      if Assigned(FindField('PARENT')) then FieldByName('PARENT').Clear;
      Post;
      AddToKeyLists; // keep a record of the new key
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
// get values from source dataset
procedure TTLIImporter.AddToKeyLists;
var
  lIndexOfNewKey : Integer;
begin
  // add new key to new key list
  lIndexOfNewKey := FslTLIKey_Dest.Add(FTLIKey_Dest);
  // add original key to original key list and point the list item to the index of the new key
  // Ptr simply casts the value lIndexOfNewKey to a pointer
  FslTLIKey_Source.AddObject(FTLIKey_Source,Ptr(lIndexOfNewKey));
end;

//------------------------------------------------------------------------------
function TTLIImporter.FindNewKey(iOriginalKey : TKeyString) : TKeyString;
var
  lIndexOfOldKey : Integer;
  lIndexOfNewKey : Integer;
begin
  // add new key to new key list
  if FslTLIKey_Source.Find(iOriginalKey,lIndexOfOldKey) then
  begin
    lIndexOfNewKey := Integer(FslTLIKey_Source.Objects[lIndexOfOldKey]);
    Result := FslTLIKey_Dest.Strings[lIndexOfNewKey];
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------
procedure TTLIImporter.GetSourceVars;
begin
  with FqrySource do
  begin
    FTLIKey_Source:= FieldByName('TAXON_LIST_ITEM_KEY').AsString;
    FTVKey_Source := FieldByName('TAXON_VERSION_KEY').AsString;
  end;
end;

//------------------------------------------------------------------------------
procedure TTLIImporter.Import;
var
  lTVImporter : TTVImporter;
  lTDImporter : TTDImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      try
        FslTLIKey_Source := TStringList.Create;   // list containing source keys points to its new key in FslTLIKey_Dest
        FslTLIKey_Source.Sorted := True;          // which has the same index
        FslTLIKey_Source.Duplicates := dupError;
        FslTLIKey_Dest := TStringList.Create;
        while not FqrySource.Eof do
        begin
          GetSourceVars;   // get some source values
          // Import new Taxon Version
          lTVImporter := TTVImporter.Create(FDatamodule);
          try
            lTVImporter.TVKey_Source := FTVKey_Source;
            lTVImporter.Import;
            FTVKey_Dest := lTVImporter.TVKey_Dest;  // get new key value in import table
          finally
            lTVImporter.Free;
          end;
          FTLIKey_Dest := GetDestKey('TAXON_LIST_ITEM','Taxon_List_Item_Key',FTLIKey_Source); // obtain new key for TLI
          AddTLIRecord;
          // Import Taxon Designations
          lTDImporter := TTDImporter.Create(FDatamodule);
          try
            lTDImporter.TLIKey_Source := FTLIKey_Source;
            lTDImporter.TLIKey_Dest := FTLIKey_Dest;
            lTDImporter.Import;
          finally
            lTDImporter.Free;
          end;
          FqrySource.Next;
        end;   // while
        // we have all TLI items imported we now must set the new Parent and
        // Preferred Name fields using the two StringLists we have created
        FqrySource.First;
        FqryDest.First;
        // step through both datasets together
        while not FqrySource.Eof do
        begin
          if FindNewKey(FqrySource.FieldByName('TAXON_LIST_ITEM_KEY').AsString) <> FqryDest.FieldByName('TAXON_LIST_ITEM_KEY').AsString then Showmessage('We have a key mismatch');
          FqryDest.Edit;
          // set new key values for Parent and Preferred Name fields
          FqryDest.FieldByName('PARENT').AsString := FindNewKey(FqrySource.FieldByName('PARENT').AsString);
          FqryDest.FieldByName('PREFERRED_NAME').AsString := FindNewKey(FqrySource.FieldByName('PREFERRED_NAME').AsString);
          FqryDest.Post; // save record
          FqrySource.Next; // move to next two records
          FqryDest.Next;
        end;   // while
      finally
        FslTLIKey_Source.Free;
        FslTLIKey_Dest.Free;
      end;  // try
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TTLIImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_LIST_VERSION_KEY').Value := FTLVKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_LIST_VERSION_KEY').Value := FTLVKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_LIST_VERSION_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
{ Source Importer }
constructor TSImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qrySSource;
  FqryDest  := FDatamodule.qrySDest;
end;

//------------------------------------------------------------------------------
procedure TSImporter.AddSRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('SOURCE_KEY',FSKey_Dest) = dsInsert then
        // primary keys
        FieldByName('SOURCE_KEY').AsString := FSKey_Dest;
      // fields
      AssignField('INTERNAL');
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TSImporter.Import;
var
  lRSImporter : TRSImporter;
  lFSImporter : TFSImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;

    if not FqrySource.Eof then
    begin
      FqrySource.First;
      GetSourceVars;
      FSKey_Dest := GetDestKey('SOURCE','Source_Key',FSKey_Source); // obtain new key for Source
      AddSRecord;
      if FSInternal then
      begin
        // Import reference
        lRSImporter := TRSImporter.Create(FDatamodule);
        try
          lRSImporter.RSKey_Source := FSKey_Source;
          lRSImporter.RSKey_Dest := FSKey_Dest;
          lRSImporter.Import;
        finally
          lRSImporter.Free;
        end;
      end else begin
        // import file source
        lFSImporter := TFSImporter.Create(FDatamodule);
        try
          lFSImporter.FSKey_Source := FSKey_Source;
          lFSImporter.FSKey_Dest := FSKey_Dest;
          lFSImporter.Import;
        finally
          lFSImporter.Free;
        end;
      end;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TSImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('SOURCE_KEY').Value := FSKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := FSKey_Source
  else
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := ST_INVALID_KEY;
end;

//------------------------------------------------------------------------------
procedure TSImporter.GetSourceVars;
begin
  FSInternal := FqrySource.FieldByName('INTERNAL').AsBoolean;
end;

//==============================================================================
{ Reference Source Importer }
constructor TRSImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryRSSource;
  FqryDest  := FDatamodule.qryRSDest;
end;

//------------------------------------------------------------------------------
procedure TRSImporter.AddRSRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('SOURCE_KEY',FRSKey_Dest) = dsInsert then
        // primary keys
        FieldByName('SOURCE_KEY').AsString := FRSKey_Dest;

      // copy fields
      AssignField('YEAR_VAGUE_DATE_START');
      AssignField('YEAR_VAGUE_DATE_END');
      AssignField('YEAR_VAGUE_DATE_TYPE');
      AssignField('FULL_REFERENCE');
      AssignField('TITLE');
      AssignField('VOLUME');
      AssignField('PART');
      AssignField('NUMBER');
      AssignField('PAGES');
      AssignField('SUPPLEMENT');
      AssignField('EDITION');
      AssignField('PUBLISHER');
      AssignField('SYMPOSIUM_TITLE');
      AssignField('PLACE_OF_PUBLICATION');
      AssignField('REFERENCE_TYPE');
      AssignField('JOURNAL_KEY');
      AssignField('INTERNAL');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TRSImporter.Import;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;
    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      if not FqrySource.Eof then AddRSRecord;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TRSImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('SOURCE_KEY').Value := FRSKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := FRSKey_Source
  else
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
{ File Source Importer }
constructor TFSImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryFSSource;
  FqryDest  := FDatamodule.qryFSDest;
end;

//------------------------------------------------------------------------------
procedure TFSImporter.AddFSRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('SOURCE_KEY',FFSKey_Dest) = dsInsert then
        // primary Keys
        FieldByName('SOURCE_KEY').AsString := FFSKey_Dest;
      // copy fields
      AssignField('FILENAME');
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TFSImporter.Import;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;
    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      if not FqrySource.Eof then AddFSRecord;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TFSImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('SOURCE_KEY').Value := FFSKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := FFSKey_Source
  else
    FqryDest.Parameters.ParamByName('SOURCE_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
{ Taxon List Importer }
constructor TTLImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTLSource;
  FqryDest  := FDatamodule.qryTLDest;
end;

//------------------------------------------------------------------------------
procedure TTLImporter.AddTLRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_LIST_KEY',FTLKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_LIST_KEY').AsString := FTLKey_Dest;

      // copy fields
      AssignField('ITEM_NAME');
      AssignField('DESCRIPTION');
      AssignField('AUTHORITY');
      AssignField('TAXON_LIST_TYPE_KEY');
      if Assigned(FqryDest.FindField('LOCAL_DISK')) then FieldByName('LOCAL_DISK').AsBoolean := True;
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTLImporter.GetSourceVars;
begin
  FTLKey_Source := FqrySource.FieldByName('TAXON_LIST_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTLImporter.Import;
var
  lTLVImporter : TTLVImporter;
  lLUImporter : TLUImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;
    // import look ups
    lLUImporter := TLUImporter.Create(FDatamodule);
    try
      lLUImporter.Import;
    finally
      lLUImporter.Free;
    end;

    if not FqrySource.Eof then
    begin
      FqrySource.First;
      GetSourceVars;
      FTLKey_Dest := GetDestKey('TAXON_LIST','Taxon_List_Key',FTLKey_Source); // obtain new key for Source
      AddTLRecord;
      // import Taxon List Version records
      lTLVImporter := TTLVImporter.Create(FDatamodule);
      try
        lTLVImporter.TLKey_Source := FTLKey_Source;
        lTLVImporter.TLKey_Dest := FTLKey_Dest;
        lTLVImporter.Import;
      finally
        lTLVImporter.Free;
      end;
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TTLImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_LIST_KEY').Value := FTLKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_LIST_KEY').Value := FTLKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_LIST_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
{ Taxon List Version Importer }
constructor TTLVImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTLVSource;
  FqryDest  := FDatamodule.qryTLVDest;
end;

//------------------------------------------------------------------------------
procedure TTLVImporter.AddTLVRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_LIST_VERSION_KEY',FTLVKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_LIST_VERSION_KEY').AsString := FTLVKey_Dest;

      // foreign keys
      if Assigned(FindField('TAXON_LIST_KEY')) then FieldByName('TAXON_LIST_KEY').AsString := FTLKey_Dest;
      // copy fields
      AssignField('VERSION');
      AssignField('AUTHORITY');
      AssignField('VAGUE_DATE_START');
      AssignField('VAGUE_DATE_END');
      AssignField('VAGUE_DATE_TYPE');
      AssignField('DESCRIPTION');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTLVImporter.GetSourceVars;
begin
  FTLVKey_Source := FqrySource.FieldByName('TAXON_LIST_VERSION_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTLVImporter.Import;
var
  lTLIImporter : TTLIImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;
    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      while not FqrySource.Eof do
      begin
        GetSourceVars;
        FTLVKey_Dest := GetDestKey('TAXON_LIST_VERSION','Taxon_List_Version_Key',FTLVKey_Source); // obtain new key for TLV
        AddTLVRecord;
        // Import Taxon List Items for this Taxon List Version
        lTLIImporter := TTLIImporter.Create(FDatamodule);
        try
          lTLIImporter.TLVKey_Source := FTLVKey_Source;
          lTLIImporter.TLVKey_Dest := FTLVKey_Source;
          lTLIImporter.Import;
        finally
          lTLIImporter.Free;
        end;
        FqrySource.Next;
      end;   // while
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TTLVImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_LIST_KEY').Value := FTLKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_LIST_KEY').Value := FTLKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_LIST_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
{ Lookup Tables Importer }
procedure TLUImporter.Import;
var
  lTDTImporter : TTDTImporter;
  lTLTImporter : TTLTImporter;
  lTRImporter : TTRImporter;
  lNTImporter : TNTImporter;
begin
  { Taxon Designation Type Importer }
  lTDTImporter := TTDTImporter.Create(FDatamodule);
  try
    lTDTImporter.Import;
  finally
    lTDTImporter.Free;
  end;
  { Taxon List Type Importer }
  lTLTImporter := TTLTImporter.Create(FDatamodule);
  try
    lTLTImporter.Import;
  finally
    lTLTImporter.Free;
  end;
  { Taxon Rank Importer }
  lTRImporter := TTRImporter.Create(FDatamodule);
  try
    lTRImporter.Import;
  finally
    lTRImporter.Free;
  end;
  { Taxon Name Type Importer }
  lNTImporter := TNTImporter.Create(FDatamodule);
  try
    lNTImporter.Import;
  finally
    lNTImporter.Free;
  end;
end;

//==============================================================================
{ TTDTImporter }
constructor TTDTImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTDTSource;
  FqryDest  := FDatamodule.qryTDTDest;
end;

//------------------------------------------------------------------------------
procedure TTDTImporter.AddTDTRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_DESIGNATION_TYPE_KEY',FTDTKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_DESIGNATION_TYPE_KEY').AsString := FTDTKey_Dest;

      // copy fields
      AssignField('SHORT_NAME');
      AssignField('LONG_NAME');
      AssignField('DESCRIPTION');
      AssignField('KIND');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTDTImporter.GetSourceVars;
begin
  FTDTKey_Source := FqrySource.FieldByName('TAXON_DESIGNATION_TYPE_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTDTImporter.Import;
begin
  FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

  FqrySource.Open;
  FqryDest.Open;
  if (not FqrySource.Eof) then
  begin
    FqrySource.First;
    while not FqrySource.Eof do
    begin
      GetSourceVars;   // get some source values
      FTDTKey_Dest := GetDestKey('TAXON_DESIGNATION_TYPE','Taxon_Designation_Type_Key',FTDTKey_Source); // obtain new key for TLI
      AddTDTRecord;
      FqrySource.Next;
    end;   // while
  end;   // if
end;

//==============================================================================
{ Taxon List Type Importer }
constructor TTLTImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTLTSource;
  FqryDest  := FDatamodule.qryTLTDest;
end;

//------------------------------------------------------------------------------
procedure TTLTImporter.AddTLTRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_LIST_TYPE_KEY',FTLTKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_LIST_TYPE_KEY').AsString := FTLTKey_Dest;

      // copy fields
      AssignField('SHORT_NAME');
      AssignField('LONG_NAME');
      AssignField('DESCRIPTION');
      AssignField('SCHEDULE');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTLTImporter.GetSourceVars;
begin
  FTLTKey_Source := FqrySource.FieldByName('TAXON_LIST_TYPE_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTLTImporter.Import;
begin
  FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

  FqrySource.Open;
  FqryDest.Open;
  if (not FqrySource.Eof) then
  begin
    FqrySource.First;
    while not FqrySource.Eof do
    begin
      GetSourceVars;   // get some source values
      FTLTKey_Dest := GetDestKey('TAXON_LIST_TYPE','Taxon_List_Type_Key',FTLTKey_Source); // obtain new key for TLI
      AddTLTRecord;
      FqrySource.Next;
    end;   // while
  end;   // if
end;

//==============================================================================
{ Taxon Rank Importer }
constructor TTRImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTRSource;
  FqryDest  := FDatamodule.qryTRDest;
end;

//------------------------------------------------------------------------------
procedure TTRImporter.AddTRRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_RANK_KEY',FTRKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_RANK_KEY').AsString := FTRKey_Dest;

      // copy fields
      AssignField('SEQUENCE');
      AssignField('SHORT_NAME');
      AssignField('DESCRIPTION');
      AssignField('LIST_FONT_ITALIC');
      AssignField('IMAGE');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTRImporter.GetSourceVars;
begin
  FTRKey_Source := FqrySource.FieldByName('TAXON_RANK_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTRImporter.Import;
begin
  FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

  FqrySource.Open;
  FqryDest.Open;
  if (not FqrySource.Eof) then
  begin
    FqrySource.First;
    while not FqrySource.Eof do
    begin
      GetSourceVars;   // get some source values
      FTRKey_Dest := GetDestKey('TAXON_RANK','Taxon_Rank__Key',FTRKey_Source); // obtain new key for TLI
      AddTRRecord;
      FqrySource.Next;
    end;   // while
  end;   // if
end;

//==============================================================================
{ Taxon Name Type Importer }
constructor TNTImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTNTSource;
  FqryDest  := FDatamodule.qryTNTDest;
end;

//------------------------------------------------------------------------------
procedure TNTImporter.AddTNTRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('TAXON_NAME_TYPE_KEY',FTNTKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_NAME_TYPE_KEY').AsString := FTNTKey_Dest;
      // copy fields
      AssignField('SHORT_NAME');
      AssignField('LONG_NAME');
      AssignField('DESCRIPTION');
      AssignField('AUTHORITY');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TNTImporter.GetSourceVars;
begin
  FTNTKey_Source := FqrySource.FieldByName('TAXON_NAME_TYPE_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TNTImporter.Import;
begin
  FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

  FqrySource.Open;
  FqryDest.Open;
  if (not FqrySource.Eof) then
  begin
    FqrySource.First;
    while not FqrySource.Eof do
    begin
      GetSourceVars;   // get some source values
      FTNTKey_Dest := GetDestKey('TAXON_NAME_TYPE','Taxon_Name_Type',FTNTKey_Source); // obtain new key for TLI
      AddTNTRecord;
      FqrySource.Next;
    end;   // while
  end;   // if
end;

//==============================================================================
{ Taxon Sources Importer}
constructor TTSImporter.Create(iDatamodule : TdmInstallCheckList);
begin
  inherited Create(iDatamodule);
  FqrySource:= FDatamodule.qryTSSource;
  FqryDest  := FDatamodule.qryTSDest;
end;

//------------------------------------------------------------------------------
procedure TTSImporter.AddTSRecord;
begin
  with FqryDest do
    try
      if PutIntoEditAppendState('SOURCE_KEY',FSKey_Dest) = dsInsert then
        // primary keys
        FieldByName('TAXON_KEY').AsString := FTKey_Dest;
      // foreign keys
      if Assigned(FindField('SOURCE_KEY')) then FieldByName('SOURCE_KEY').AsString := FSKey_Dest;
      // copy fields
      AssignField('INTERNAL');
      SetEditChangeFields;
      Post;
    except
      Cancel;
      raise;
    end;
end;

//------------------------------------------------------------------------------
procedure TTSImporter.GetSourceVars;
begin
  FSKey_Source := FqrySource.FieldByName('SOURCE_KEY').AsString;
end;

//------------------------------------------------------------------------------
procedure TTSImporter.Import;
var
  lSImporter : TSImporter;
begin
  try
    FqrySource.Close; FqryDest.Close;  // close datasets initially (should be closed anyway)

    SetParams; // set source query parameters
    FqrySource.Open;
    FqryDest.Open;
    if (not FqrySource.Eof) then
    begin
      FqrySource.First;
      while not FqrySource.Eof do
      begin
        GetSourceVars;   // get some source values
        lSImporter := TSImporter.Create(FDatamodule);
        try
          lSImporter.SKey_Source := FSKey_Source;
          lSImporter.Import;
          FSKey_Dest := lSImporter.SKey_Dest;  // get new key value in import table
        finally
          lSImporter.Free;
        end;
        AddTSRecord;
        FqrySource.Next;
      end;   // while
    end;   // if
  finally
    FqrySource.Close;
    FqryDest.Close;
  end;
end;

//------------------------------------------------------------------------------
procedure TTSImporter.SetParams;
begin
  FqrySource.Parameters.ParamByName('TAXON_KEY').Value := FTKey_Source;
  if not FNewKeys then
    FqryDest.Parameters.ParamByName('TAXON_KEY').Value := FTKey_Source
  else
    FqryDest.Parameters.ParamByName('TAXON_KEY').Value := ST_INVALID_KEY;
end;

//==============================================================================
end.
