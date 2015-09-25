{===============================================================================
  Unit:        IWSettings

  Defines:     TdmSettings

  Description: Settings used through the Import Wizard (IW).

  Model:       ImportWizard

  Last revision information:
    $Revision: 46 $
    $Date: 30/04/09 16:38 $
    $Author: Pauldavies $

===============================================================================}

unit IWSettings;

interface

uses
  SysUtils, Classes, Forms, ImgList, Controls, Grids, IWColumnMappingClasses,
  DataClasses, ADODB, XmlDoc, XmlIntf, ExceptionForm, Variants, StrUtils,
  IWUserSuppliedData, RapTree, TempData_ADO, JNCCXmlDoc, Windows, DB;

const
  FLD_ROWID='__ID__';

type
  TImportType = (itUnknown, itNBNData, itZippedAccess, itText, itCSV,
      itFixedWidth, itExcel, itDBase, itParadox, itLotus, itQuattro, itADO, itAddin);

  TImportTypeFilePrefix = array[TImportType] of string;

  TdmIWSettings = class(TDataModule)
    ilMain: TImageList;
  private
    FADOImportTable: String;
    FBackFromMatchPage: Boolean;
    FMatchPageChecklistKeys: TStringList;
    FColumnTypes: String;
    FDataFileLoaded: Boolean;
    FDateDelimiter: Char;
    FDateFormat: String;
    FDecimalSymbol: Char;
    FdsImportedData: TADOTable;
    FDuplicateCount: Integer;
    FDuplicateNodeTree: TRapidTree;
    FFieldNameOnFirstRow: Boolean;
    FFixedWidthBreaks: TList;
    FHaveVisitedFileSelect: Boolean;
    FImportFile: TImportFile;
    FImportOnlySpecifiedRows: Boolean;
    FImportRowsFrom: Integer;
    FImportRowsTo: Integer;
    FImportType: TImportType;
    FInvalidCount: Integer;
    FInvalidNodes: TFlyNodes;
    FLargeFields: TStringList;
    FMatchRuleIndex: Integer;
    FRecordCount: Integer;
    FRejections: TStringlist;
    FSelectedMatchValue: String;
    FSourceDataFile: String;
    FTempData: TTempData;
    FTemplateFileName: string;
    FTextFieldDelimiter: Char;
    FTextQualifier: Char;
    FTextRecordSeparator: String;
    FThousandSeparator: Char;
    FTotalCount: Integer;
    FUserSuppliedData: TUserSuppliedData;
    FUseVagueLocations: Boolean;
    FUseFirstCentroid: Boolean;
    FUseOldImportWizard: Boolean;
    FXMLDoc: TXMLDoc;
    FFirstRow: TStringList;
    FValidationFailureCount: Integer;
    procedure DropImportTable;
    function GetXMLDoc: TXMLDoc;
    procedure ReadADOXML(const ParentNode: IXMLNode);
    procedure ReadColumnTypesXML(const ParentNode: IXMLNode);
    procedure ReadDelimiterXML(const ParentNode: IXMLNode);
    procedure ReadFixedWidthXML(const ParentNode: IXMLNode);
    procedure ReadFormattingXML(const ParentNode: IXMLNode);
    procedure ReadImportTypeXML(const ParentNode: IXMLNode);
    procedure SetDuplicateCount(const Value: Integer);
    procedure SetImportFile(const Value: TImportFile);
    procedure SetInvalidCount(const Value: Integer);
    procedure SetTotalCount(const Value: Integer);
    procedure WriteADOXML(const ParentNode: IXMLNode);
    procedure WriteColumnTypesXML(const ParentNode: IXMLNode);
    procedure WriteDelimiterXML(const ParentNode: IXMLNode);
    procedure WriteFixedWidthXML(const ParentNode: IXMLNode);
    procedure WriteFormattingXML(const ParentNode: IXMLNode);
    procedure WriteImportTypeXML(const ParentNode: IXMLNode);
    procedure SetRecordCount(const Value: Integer);
    procedure SetDefaults;
    procedure SetImportType(const Value: TImportType);
    procedure MoveFirstRowToColumnTitles;
    procedure SetFieldNameOnFirstRow(const Value: Boolean);
    procedure ResetColumnTitles;
    function GetDuplicateNodes: TFlyNodes;
    procedure SetValidationFailureCount(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateImportFile;
    procedure FreeXMLDoc;
    function GetTemplatesOfMatchingType(const Directory: string): TStringList;
    procedure LoadFileSettingsFromFile(const AFileName: string);
    procedure LoadColumnsFromFile;
    procedure ResetImportedData;
    procedure SaveSettingsToFile(const AFileName: string); 
    procedure LargeFieldGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure LargeFieldSetText(Sender: TField; const Text: string);
    property ADOImportTable: String read FADOImportTable write FADOImportTable;
    property BackFromMatchPage: Boolean read FBackFromMatchPage write FBackFromMatchPage;
    property ColumnTypes: String read FColumnTypes write FColumnTypes;
    property DataFileLoaded: Boolean read FDataFileLoaded write FDataFileLoaded;
    property DateDelimiter: Char read FDateDelimiter write FDateDelimiter;
    property DateFormat: String read FDateFormat write FDateFormat;
    property DecimalSymbol: Char read FDecimalSymbol write FDecimalSymbol;
    property DuplicateCount: Integer read FDuplicateCount write SetDuplicateCount;
    property DuplicateNodes: TFlyNodes read GetDuplicateNodes;
    property DuplicateNodeTree: TRapidTree read FDuplicateNodeTree;
    property FieldNameOnFirstRow: Boolean read FFieldNameOnFirstRow write SetFieldNameOnFirstRow;
    property FirstRow: TStringList read FFirstRow;
    property FixedWidthBreaks: TList read FFixedWidthBreaks write FFixedWidthBreaks;
    property HaveVisitedFileSelect: Boolean read FHaveVisitedFileSelect write
        FHaveVisitedFileSelect;
    property ImportedData: TADOTable read FdsImportedData;
    property ImportFile: TImportFile read FImportFile write SetImportFile;
    property ImportOnlySpecifiedRows: Boolean read FImportOnlySpecifiedRows write
        FImportOnlySpecifiedRows;
    property ImportRowsFrom: Integer read FImportRowsFrom write FImportRowsFrom;
    property ImportRowsTo: Integer read FImportRowsTo write FImportRowsTo;
    property ImportType: TImportType read FImportType write SetImportType;
    property InvalidCount: Integer read FInvalidCount write SetInvalidCount;
    property InvalidNodes: TFlyNodes read FInvalidNodes;
    property LargeFields: TStringList read FLargeFields;
    property MatchPageChecklistKeys: TStringList read FMatchPageChecklistKeys;
    property MatchRuleIndex: Integer read FMatchRuleIndex write FMatchRuleIndex;
    property RecordCount: Integer read FRecordCount write SetRecordCount;
    property Rejections: TStringlist read FRejections;
    property SelectedMatchValue: String read FSelectedMatchValue write FSelectedMatchValue;
    property SourceDataFile: String read FSourceDataFile write FSourceDataFile;
    property TempData: TTempData read FTempData write FTempData;
    property TextFieldDelimiter: Char read FTextFieldDelimiter write FTextFieldDelimiter;
    property TextQualifier: Char read FTextQualifier write FTextQualifier;
    property TextRecordSeparator: String read FTextRecordSeparator write FTextRecordSeparator;
    property ThousandSeparator: Char read FThousandSeparator write FThousandSeparator;
    property TotalCount: Integer read FTotalCount write SetTotalCount;
    property UserSuppliedData: TUserSuppliedData read FUserSuppliedData;
    property UseVagueLocations: Boolean read FUseVagueLocations write FUseVagueLocations;
    property UseFirstCentroid: Boolean read FUseFirstCentroid write FUseFirstCentroid;
    property UseOldImportWizard: Boolean read FUseOldImportWizard;
    property ValidationFailureCount: Integer read FValidationFailureCount write
        SetValidationFailureCount;
    property XMLDoc: TXMLDoc read GetXMLDoc;
  end;

  EXMLException = class(TExceptionPath)
  end;

//==============================================================================
implementation

uses 
  Maintbar, Treecoll, Constants, DatabaseAccessADO, ApplicationSettings,
  IWLargeImportFile;

{$R *.dfm}

const
  //XML elements
  EL_COLUMNTYPES = 'column_types';
  EL_COLUMNTYPE = 'column_type';
  EL_IMPORTWIZARDTEMPLATE = 'import_wizard_template';
  EL_IMPORTTYPE = 'import_type';
  EL_DELIMITEROPTIONS = 'delimiter_options';
  EL_DELIMITER = 'delimiter';
  EL_RECORDSEPARATOR = 'record_separator';
  EL_TEXTQUALIFIER = 'text_qualifier';
  EL_FORMATTINGOPTIONS = 'formatting_options';
  EL_IMPORTALLROWS = 'import_all';
  EL_IMPORTROWSFROM = 'rows_from';
  EL_IMPORTROWSTO = 'rows_to';
  EL_DATEFORMAT = 'date_format';
  EL_DATEDELIMITER = 'date_delimiter';
  EL_DECIMALSYMBOL = 'decimal_symbol';
  EL_THOUSANDSEPARATOR = 'thousand_separator';
  EL_DATATABLE = 'data_table';
  EL_FIXEDWIDTH = 'fixed_width';
  EL_FIXEDWIDTHBREAK = 'break';

  AT_FIELD = 'field';
  AT_FIRSTROWISDATA = 'first_row_is_data';
  AT_KEY = 'key';
  AT_QUALIFIER = 'qualifier';

  SMI_TABLE_NAME = '#SMImport';

resourcestring
  ResStr_XMLFormatError = 'XML file is not of the correct format.';
  ResStr_ValueNotChar = 'Value is not a char.';

function HasChildNode(AXMLNode: IXMLNode; const AName: string): boolean;
var
  lIdx: integer;
begin
  Result := False;
  if Assigned(AXMLNode) then
    for lIdx := 0 to AXMLNode.ChildNodes.Count - 1 do
      if AXMLNode.ChildNodes[lIdx].NodeName = AName then begin
        Result := True;
        Break;
      end;
end;

procedure EncodeXMLNodeValue(var AString: string);
begin
  StringReplace(AString, #13, '&#13;', [rfReplaceAll]);
  StringReplace(AString, #10, '&#10;', [rfReplaceAll]);
  StringReplace(AString, #20, '&#20;', [rfReplaceAll]);
end;

procedure AddTextNode(const AParentNode: IXMLNode; const ATextNodeName,
  ATextNodeValue: string);
var
  lTextNode: IXMLNode;
  lValue: string;
begin
  lTextNode := AParentNode.AddChild(ATextNodeName);
  lValue := ATextNodeValue;
  EncodeXMLNodeValue(lValue);
  lTextNode.NodeValue := lValue;
end;

function CharFromVar(const AValue: OleVariant): char;
var
  lStringValue: string;
begin
  lStringValue := VarToStr(AValue);
  case length(lStringValue) of
    1: Result := lStringValue[1];
    0: Result := #0;
  else
    raise Exception.Create(ResStr_ValueNotChar);
  end;
end;

{-==============================================================================
    TdmIWSettings
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdmIWSettings.Create(AOwner: TComponent);

  function CreateList: TStringList;
  begin
    Result := TStringList.Create;
    Result.Sorted := True;
  end;

begin
  inherited;
  FMatchRuleIndex := -1;
  FUserSuppliedData := TUserSuppliedData.Create;
  FDuplicateNodeTree := TRapidTree.Create(nil);
  with FDuplicateNodeTree do
  begin
    Items.Clear;
    FitColumnToClientWidth := True;
    Constraints.MinWidth := 100;
    DefaultRowHeight := 18;
    FixedRows := 0;
    GridLineWidth := 0;
    Indent := 18;
    SmoothExpandCollapse := False;
  end;
  FInvalidNodes := TFlyNodes.Create(nil);
  FRejections := CreateList;
  FLargeFields := CreateList;
  // Hold first row of data from a text file
  FFirstRow := TStringList.Create;
  FieldNameOnFirstRow := False;
  // Remembers the checklist selected on a match page in the form "RuleName=Key".
  FMatchPageChecklistKeys := TStringList.Create;
  FdsImportedData := TADOTable.Create(nil);
  FdsImportedData.Connection := dmDatabase.Connection;
  FdsImportedData.TableName := SMI_TABLE_NAME;
  SetDefaults;
  // Don't want to have this changing halfway through an import.
  FUseOldImportWizard := AppSettings.UseOldImportWizard;
end;  // TdmIWSettings.Create

{-------------------------------------------------------------------------------
}
destructor TdmIWSettings.Destroy;
var
  I: Integer;
begin
  FUserSuppliedData.Free;
  FFixedWidthBreaks.Free;
  if Assigned(FInvalidNodes) then
    for I := 0 to InvalidNodes.Count - 1 do
      if TObject(InvalidNodes.Items[I].Data) is TMessage then
        TObject(InvalidNodes.Items[I].Data).Free;
  FInvalidNodes.Free;
  FXMLDoc.Free;
  FRejections.Free;
  FLargeFields.Free;
  FFirstRow.Free;
  FImportFile.Free;
  FMatchPageChecklistKeys.Free;
  FdsImportedData.Free;
  if Assigned(FTempData) then FreeAndNil(FTempData);
  DropImportTable;
  inherited;
end;  // TdmIWSettings.Destroy

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.DropImportTable;
begin
  dmDatabase.ExecuteSql(
      'IF object_id(''tempdb..' + SMI_TABLE_NAME + ''') IS NOT NULL'
      + ' DROP TABLE ' + SMI_TABLE_NAME); 
end;  // TdmIWSettings.DropImportTable

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetDefaults;
begin
  TextFieldDelimiter := #9;
  TextRecordSeparator := #13#10;
  TextQualifier := '"';
  FDateFormat := SysUtils.ShortDateFormat;
  FDateDelimiter := SysUtils.DateSeparator;
  FDecimalSymbol := SysUtils.DecimalSeparator;
  FThousandSeparator := SysUtils.ThousandSeparator;
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.FreeXMLDoc;
begin
  FreeAndNil(FXMLDoc);
end;  // TdmIWSettings.FreeXMLDoc 

{-------------------------------------------------------------------------------
}
function TdmIWSettings.GetTemplatesOfMatchingType(const Directory: string): TStringList;
var
  lTemplates: TStringList;
  lSearchRec: TSearchRec;
  lSearchStr: String;
begin
  lTemplates := TStringList.Create;
  try
    lSearchStr := Directory + '*.xml';
    try
      if FindFirst(lSearchStr, 0, lSearchRec) = 0 then
      begin
          (*TODO: check prefix*)
          lTemplates.Add(LeftStr(lSearchRec.Name,
            length(lSearchRec.Name) - 4));
          while FindNext(lSearchRec) = 0 do
            lTemplates.Add(LeftStr(lSearchRec.Name,
              length(lSearchRec.Name) - 4));
      end;
    finally
      Sysutils.FindClose(lSearchRec);
    end;
    Result := lTemplates;
  except on Exception do
    begin
      lTemplates.Free;
      raise;
    end;
  end;
end;  // TdmIWSettings.GetTemplatesOfMatchingType 

{-------------------------------------------------------------------------------
}
function TdmIWSettings.GetXMLDoc: TXMLDoc;
begin
  if not Assigned(FXMLDoc) then
    FXMLDoc := TXMLDoc.Create(SourceDataFile, nil, frmMain.ProgressBar);
  Result := FXMLDoc;
end;  // TdmIWSettings.GetXMLDoc

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.LoadFileSettingsFromFile(const AFileName: string);
var
  lXMLDoc: IXMLDocument;
  lRootNode: IXMLNode;
begin
  //set the property that says fileselect page has been visited - since
  //we have set the properties set by the page anyway
  HaveVisitedFileSelect := true;
  FTemplateFileName := AFileName;
  try
    lXMLDoc := NewXMLDocument;
    lXMLDoc.LoadFromFile(AFileName);
    lRootNode := lXMLDoc.ChildNodes[EL_IMPORTWIZARDTEMPLATE];
    ReadImportTypeXML(lRootNode);
    //text file delimiters/spearators
    if HasChildNode(lRootNode, EL_DELIMITEROPTIONS) then
      ReadDelimiterXML(lRootNode);
    //text/spreadsheet formatting/rows
    if HasChildNode(lRootNode, EL_FORMATTINGOPTIONS) then
      ReadFormattingXML(lRootNode);
    //data table
    if HasChildNode(lRootNode, EL_DATATABLE) then
      ReadADOXML(lRootNode);
    //fixed width breaks
    if Assigned(FFixedWidthBreaks) then
      FreeAndNil(FFixedWidthBreaks);
    if HasChildNode(lRootNode, EL_FIXEDWIDTH) then
      ReadFixedWidthXML(lRootNode);
  except on Exception do
    raise EXMLException.CreateNonCritical(ResStr_XMLFormatError);
  end;
end;  // TdmIWSettings.LoadFileSettingsFromFile

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.LoadColumnsFromFile;
var
  lXMLDoc: IXMLDocument;
  lRootNode: IXMLNode;
begin
  //set the property that says fileselect page has been visited - since
  //we have set the properties set by the page anyway
  HaveVisitedFileSelect := true;
  try
    if FTemplateFileName <> '' then begin
      lXMLDoc := NewXMLDocument;
      lXMLDoc.LoadFromFile(FTemplateFileName);
      lRootNode := lXMLDoc.ChildNodes[EL_IMPORTWIZARDTEMPLATE];
      ReadColumnTypesXML(lRootNode);
      // clear the template file name so we don't reload the columns if returning
      // to the column types page
      FTemplateFileName := '';
    end;
  except on Exception do
    raise EXMLException.CreateNonCritical(ResStr_XMLFormatError);
  end;
end;  // TdmIWSettings.LoadColumnsFromFile


{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadADOXML(const ParentNode: IXMLNode);
begin
  ADOImportTable := VarToStr(ParentNode.ChildNodes[EL_DATATABLE].NodeValue);
end;  // TdmIWSettings.ReadADOXML 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadColumnTypesXML(const ParentNode: IXMLNode);
var
  lNode: IXMLNode;
  i: integer;
  lFieldName: string;

    // convert display name of field to field name
    function GetFieldName(const ADisplayName: string): string;
    var
      j: integer;
    begin
      Result := '';
      for j := 0 to FdsImportedData.FieldCount-1 do
        if ImportFile.ColumnMapping.ColumnTitle[j] = ADisplayNAme then begin
          Result := FdsImportedData.Fields[j].FieldName;
          Exit;
        end;
    end;

begin
  if Assigned(ParentNode.ChildNodes[EL_COLUMNTYPES]) then begin
    CreateImportFile;
    lNode := ParentNode.ChildNodes[EL_COLUMNTYPES];
    FieldNameOnFirstRow := lNode.HasAttribute(AT_FIRSTROWISDATA);
    for i := 0 to lNode.ChildNodes.Count-1 do begin
      with lNode.ChildNodes[i] do begin
        // DisplayName of field stored in XML, convert to FieldName
        lFieldName := GetFieldName(Attributes[AT_FIELD]);
        if lFieldName<>'' then begin
          if HasAttribute(AT_QUALIFIER) then
            // measurement
            ImportFile.ColumnMapping.MapColumn(
              lFieldName,
              ImportFile.ColumnMapping.ColumnTypeByKey(
                  Attributes[AT_KEY], Attributes[AT_QUALIFIER]))
          else
            // standard column type
            ImportFile.ColumnMapping.MapColumn(
                lFieldName,
                ImportFile.ColumnMapping.ColumnTypeByKey(
                    Attributes[AT_KEY]));
        end;
      end;
    end;
  end;
end;  // TdmIWSettings.ReadColumnTypesXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.MoveFirstRowToColumnTitles;
var
  i: integer;
begin
  // remove first row
  if ImportedData.Locate(FLD_ROWID, 1, []) then begin
    ImportedData.Delete;
    // Copy over column titles into mapping object
    for i := 0 to ImportFile.ColumnMapping.ColumnCount-1 do begin
      if FirstRow.IndexOfName(ImportFile.ColumnMapping.ColumnTitle[i])>-1 then
        if FirstRow.Values[ImportFile.ColumnMapping.ColumnTitle[i]]='' then
          ImportFile.ColumnMapping.ColumnTitle[i] := 'Unknown'
        else
          ImportFile.ColumnMapping.ColumnTitle[i] := FirstRow.Values[
              ImportFile.ColumnMapping.ColumnTitle[i]];
    end;
  end;
end;  // TdmIWSettings.MoveFirstRowToColumnTitles

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.CreateImportFile;
begin
  if Assigned(ImportFile) then
    ImportFile.ColumnMapping.LoadColumns(ImportedData)
  else
  if UseOldImportWizard then
    ImportFile := TImportFile.Create(
        ImportedData,
        LargeFields,
        UserSuppliedData,
        True)
  else
    ImportFile := TLargeImportFile.Create(
        ImportedData,
        LargeFields,
        UserSuppliedData,
        False);

  DataFileLoaded := True;
end;  // TdmIWSettings.CreateImportFile

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadDelimiterXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
begin
  lSubNode := ParentNode.ChildNodes[EL_DELIMITEROPTIONS];
  if HasChildNode(lSubNode, EL_DELIMITER) then
    TextFieldDelimiter := CharFromVar(lSubNode.ChildNodes[EL_DELIMITER].NodeValue);
  if HasChildNode(lSubNode, EL_RECORDSEPARATOR) then
    TextRecordSeparator := VarToStr(lSubNode.ChildNodes[EL_RECORDSEPARATOR].NodeValue);
  if HasChildNode(lSubNode, EL_TEXTQUALIFIER) then
    TextQualifier := CharFromVar(lSubNode.ChildNodes[EL_TEXTQUALIFIER].NodeValue);
end;  // TdmIWSettings.ReadDelimiterXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadFixedWidthXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
  lNodeList: IXMLNodeList;
  i: Integer;
begin
  lSubNode := ParentNode.ChildNodes[EL_FIXEDWIDTH];
  if HasChildNode(lSubNode, EL_FIXEDWIDTHBREAK) then begin
    FFixedWidthBreaks := TList.Create;
    lNodeList := lSubNode.ChildNodes;
    for i := 0 to lNodeList.Count - 1 do
      if lNodeList[i].NodeName = EL_FIXEDWIDTHBREAK then
        FFixedWidthBreaks.Add(TObject(StrToInt(lNodeList[i].NodeValue)));
  end;
end;  // TdmIWSettings.ReadFixedWidthXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadFormattingXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
begin
  lSubNode := ParentNode.ChildNodes[EL_FORMATTINGOPTIONS];
  if HasChildNode(lSubNode, EL_IMPORTALLROWS) then
    ImportOnlySpecifiedRows := not StrToBool(lSubNode.ChildNodes[EL_IMPORTALLROWS].NodeValue);
  if HasChildNode(lSubNode, EL_IMPORTROWSFROM) then
    ImportRowsFrom := StrToInt(lSubNode.ChildNodes[EL_IMPORTROWSFROM].NodeValue);
  if HasChildNode(lSubNode, EL_IMPORTROWSTO) then
    ImportRowsTo := StrToInt(lSubNode.ChildNodes[EL_IMPORTROWSTO].NodeValue);
  if HasChildNode(lSubNode, EL_DATEFORMAT) then
    DateFormat := VarToStr(lSubNode.ChildNodes[EL_DATEFORMAT].NodeValue);
  if HasChildNode(lSubNode, EL_DATEDELIMITER) then
    DateDelimiter := CharFromVar(lSubNode.ChildNodes[EL_DATEDELIMITER].NodeValue);
  if HasChildNode(lSubNode, EL_DECIMALSYMBOL) then
    DecimalSymbol := CharFromVar(lSubNode.ChildNodes[EL_DECIMALSYMBOL].NodeValue);
  if HasChildNode(lSubNode, EL_THOUSANDSEPARATOR) then
    ThousandSeparator := CharFromVar(lSubNode.ChildNodes[EL_THOUSANDSEPARATOR].NodeValue);
end;  // TdmIWSettings.ReadFormattingXML 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ReadImportTypeXML(const ParentNode: IXMLNode);
var
  lType: String;
begin
  if HasChildNode(ParentNode, EL_IMPORTTYPE) then begin
    lType := VarToStr(ParentNode.ChildNodes[EL_IMPORTTYPE].NodeValue);
    if lType = 'Text' then
      ImportType := itText
    else if lType = 'CSV' then
      ImportType := itCSV
    else if lType = 'Fixed Width' then
      ImportType := itFixedWidth
    else if lType = 'Excel' then
      ImportType := itExcel
    else if lType = 'DBase' then
      ImportType := itDBase
    else if lType = 'Paradox' then
      ImportType := itParadox
    else if lType = 'Lotus' then
      ImportType := itLotus
    else if lType = 'Quattro' then
      ImportType := itQuattro
    else if lType = 'ADO' then
      ImportType := itADO
    else
      raise EXMLException.CreateNonCritical(ResStr_XMLFormatError);;;;;;;;
  end else
    raise EXMLException.CreateNonCritical(ResStr_XMLFormatError);
end;  // TdmIWSettings.ReadImportTypeXML 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ResetImportedData;
begin
  DropImportTable;
  FdsImportedData.Close;
  DataFileLoaded := False;
  FreeAndNil(FImportFile);
  // reset first row and other remembered data
  FFieldNameOnFirstRow := False;
  FLargeFields.Clear;
  FFirstRow.Clear;
end;  // TdmIWSettings.ResetImportedData

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SaveSettingsToFile(const AFileName: string);
var
  lXMLDoc: IXMLDocument;
  lRootNode: IXMLNode;
begin
  lXMLDoc := NewXMLDocument;
  lRootNode := lXMLDoc.AddChild(EL_IMPORTWIZARDTEMPLATE);
  
  WriteImportTypeXML(lRootNode);
  //text file delimiters/spearators
  if ImportType in [itText, itCSV, itFixedWidth] then
    WriteDelimiterXML(lRootNode);
  //text/spreadsheet formatting/rows
  if ImportType in [itText, itCSV, itFixedWidth, itParadox, itExcel, itLotus] then
    WriteFormattingXML(lRootNode);
  //data table
  if ImportType = itADO then
    WriteADOXML(lRootNode);
  //fixed width breaks
  if Assigned(FixedWidthBreaks) and (FixedWidthBreaks.Count > 0) then
    WriteFixedWidthXML(lRootNode);
  WriteColumnTypesXML(lRootNode);
  lXMLDoc.SaveToFile(AFileName);
end;  // TdmIWSettings.SaveSettingsToFile 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetDuplicateCount(const Value: Integer);
begin
  FDuplicateCount := Value;
end;  // TdmIWSettings.SetDuplicateCount 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetImportFile(const Value: TImportFile);
begin
  FImportFile := Value;
end;  // TdmIWSettings.SetImportFile 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetInvalidCount(const Value: Integer);
begin
  FInvalidCount := Value;
end;  // TdmIWSettings.SetInvalidCount 

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TdmIWSettings.SetRecordCount(const Value: Integer);
begin
  FRecordCount := Value;
  if Owner is TWinControl then
    PostMessage(TWinControl(Owner).Handle, WM_REFRESH_RECORD_COUNT, 0, 0);
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetTotalCount(const Value: Integer);
begin
  FTotalCount := Value;
end;  // TdmIWSettings.SetTotalCount 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteADOXML(const ParentNode: IXMLNode);
begin
  AddTextNode(ParentNode, EL_DATATABLE, ADOImportTable);
end;  // TdmIWSettings.WriteADOXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteColumnTypesXML(const ParentNode: IXMLNode);
var
  lNode: IXMLNode;
  lSubNode: IXMLNode;
  i: integer;
begin
  if Assigned(ImportedData) then begin
    lNode := ParentNode.AddChild(EL_COLUMNTYPES);
    if FieldNameOnFirstRow then
      lNode.Attributes[AT_FIRSTROWISDATA]:='"1"';
    for i := 0 to ImportedData.Fields.Count-1 do
      if ImportFile.ColumnMapping.IsMapped(ImportedData.Fields[i].FieldName) then
        with ImportFile.ColumnMapping.MappedType(ImportedData.Fields[i].FieldName) do
        begin
          lSubNode := lNode.addChild(EL_COLUMNTYPE);
          lSubNode.Attributes[AT_Field] := ImportFile.ColumnMapping.
              ColumnTitleByName[ImportedData.Fields[i].FieldName];
          lSubNode.Attributes[AT_KEY] := Key;
          if Qualification<>'' then
            lSubNode.Attributes[AT_QUALIFIER] := Qualification;
        end;
  end;
end;  // TdmIWSettings.WriteColumnTypesXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteDelimiterXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
begin
  lSubNode := ParentNode.AddChild(EL_DELIMITEROPTIONS);
  //if TextFieldDelimiter <> #0 then
  AddTextNode(lSubNode, EL_DELIMITER, TextFieldDelimiter);
  //if length(TextRecordSeparator) > 0 then
  AddTextNode(lSubNode, EL_RECORDSEPARATOR, TextRecordSeparator);
  //if TextQualifier <> #0 then
  AddTextNode(lSubNode, EL_TEXTQUALIFIER, TextQualifier);
end;  // TdmIWSettings.WriteDelimiterXML 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteFixedWidthXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
  i: Integer;
begin
  lSubNode := ParentNode.AddChild(EL_FIXEDWIDTH);
  for i := 0 to FixedWidthBreaks.Count - 1 do
    AddTextNode(lSubNode, EL_FIXEDWIDTHBREAK, IntToStr(integer (FixedWidthBreaks[i])));
end;  // TdmIWSettings.WriteFixedWidthXML 

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteFormattingXML(const ParentNode: IXMLNode);
var
  lSubNode: IXMLNode;
begin
  lSubNode := ParentNode.AddChild(EL_FORMATTINGOPTIONS);
  AddTextNode(lSubNode, EL_IMPORTALLROWS,
    BoolToStr(not ImportOnlySpecifiedRows));
  if ImportOnlySpecifiedRows then begin
    //if ImportRowsFrom > 0 then
      AddTextNode(lSubNode, EL_IMPORTROWSFROM, IntToStr(ImportRowsFrom));
    //if ImportRowsTo > 0 then
      AddTextNode(lSubNode, EL_IMPORTROWSTO, IntToStr(ImportRowsTo));
  end;
  //if length(DateFormat) > 0 then
    AddTextNode(lSubNode, EL_DATEFORMAT, DateFormat);
  //if DateDelimiter <> #0 then
    AddTextNode(lSubNode, EL_DATEDELIMITER, DateDelimiter);
  //if DecimalSymbol <> #0 then
    AddTextNode(lSubNode, EL_DECIMALSYMBOL, DecimalSymbol);
  //if ThousandSeparator <> #0 then
    AddTextNode(lSubNode, EL_THOUSANDSEPARATOR, ThousandSeparator);
end;  // TdmIWSettings.WriteFormattingXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.WriteImportTypeXML(const ParentNode: IXMLNode);
var
  lType: String;
begin
  case ImportType of
    itText: lType := 'Text';
    itCSV: lType := 'CSV';
    itFixedWidth: lType := 'Fixed Width';
    itExcel: lType := 'Excel';
    itDBase: lType := 'DBase';
    itParadox: lType := 'Paradox';
    itLotus: lType := 'Lotus';
    itQuattro: lType := 'Quattro';
    itADO: lType := 'ADO';
  else
    lType := 'Unknown';
  end;
  AddTextNode(ParentNode, EL_IMPORTTYPE, lType);
end;  // TdmIWSettings.WriteImportTypeXML

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetImportType(const Value: TImportType);
begin
  FImportType := Value;
  // CSV import defaults field separator to ,
  if FImportType = itCSV then
    TextFieldDelimiter := ',';
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.SetFieldNameOnFirstRow(const Value: Boolean);
begin
  if FFieldNameOnFirstRow <> Value then begin
    FFieldNameOnFirstRow := Value;
    // Ensure column titles are correct in the ColumnMapping object
    if Value then
      MoveFirstRowToColumnTitles
    else
      ResetColumnTitles;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.ResetColumnTitles;
var
  i: integer;
begin
  for i := 0 to ImportFile.ColumnMapping.ColumnCount-1 do
    ImportFile.ColumnMapping.ResetColumnTitle(i);
end;

{-------------------------------------------------------------------------------
}
function TdmIWSettings.GetDuplicateNodes: TFlyNodes;
begin
  Result := FDuplicateNodeTree.Items;
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.LargeFieldGetText(Sender: TField; var Text: string;
    DisplayText: Boolean);
begin
  // If the field has a large value, get that instead of the normal value
  if LargeFields.Values[Sender.FieldName + ','
      + ImportedData.Fields.DataSet.FieldByName(FLD_ROWID).AsString] <> '' then
    Text := LargeFields.Values[Sender.FieldName + ','
      + ImportedData.Fields.DataSet.FieldByName(FLD_ROWID).AsString]
  else
    Text := Sender.AsString;
end;

{-------------------------------------------------------------------------------
}
procedure TdmIWSettings.LargeFieldSetText(Sender: TField; const Text: string);
var
  lTextToSet: string;
  lLargeFieldIndex: string;
begin
  // Set the LargeFields value when the normal value changes.
  lLargeFieldIndex := Sender.FieldName + ','
      + ImportedData.Fields.Dataset.FieldByName(FLD_ROWID).AsString;
  if Length(Text) > Sender.Size then
    lTextToSet := Text
  else
    lTextToSet := '';
  if lTextToSet <> LargeFields.Values[lLargeFieldIndex] then begin
    LargeFields.Sorted := False;
    try
      LargeFields.Values[lLargeFieldIndex] := lTextToSet;
    finally
      LargeFields.Sorted := True;
    end;
  end;
  Sender.AsString := Copy(Text, 1, Sender.Size);
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TdmIWSettings.SetValidationFailureCount(const Value: Integer);
begin
  FValidationFailureCount := Value;
end;

end.

