unit CRReportIndex;

interface

uses
  SysUtils, Classes, CRCommonClasses, CRReportSQL, XMLDoc, XMLIntf, CRConstants;

resourcestring
  ResStr_DocumentElementInvalid = 'The XML file %s is invalid because the top level element '+
      'is expected to be called %s, but it is currently set to %s. Note that XML is case sensitive.';

type
  TBaseReportIndex = class(TObject)
  private
    FFiles: TStringList;
    FFileTypes: TStringList;
    FTitles: TStringList;
    FPaths: TStringList;
    FReportPath: String;
    FTemplates: TStringList;
    FDocumentNode: String;
    function LoadXML(const AFileName: string): IXMLNode;
    procedure ReadIndexFile;
    procedure ReIndexFile(const AFileName: string);
    procedure RemoveFileFromIndex(const AFileName: string);
    procedure UpdateIndex;
    procedure WriteIndexFile;
  protected
    function GetDocumentNode: String; virtual; abstract;
  public        
    constructor Create(const AReportPath: string);
    destructor Destroy; override;    
    procedure RebuildIndex(const APath: string='');
    property FileTypes: TStringList read FFileTypes;
    property Paths: TStringList read FPaths;
    property Templates: TStringList read FTemplates;
    property Titles: TStringList read FTitles;
    property DocumentNode: String read FDocumentNode;
  end;

  TCRReportIndex = class(TBaseReportIndex)
  protected
    function GetDocumentNode: String; override;
  end; 

  TBUReportIndex = class(TBaseReportIndex)
  protected
    function GetDocumentNode: String; override;
  end;
  
implementation

uses
  ApplicationSettings, ResourceStrings, XMLDom, GeneralFunctions;


{-==============================================================================
    TBaseReportIndex
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.  Opens the index, updates and saves it.
}
constructor TBaseReportIndex.Create(const AReportPath: string);
begin
  inherited Create;
  FFiles := TStringList.Create;
  FPaths := TStringList.Create;
  FFileTypes := TStringList.Create;
  FTitles := TStringList.Create;
  FTemplates := TStringList.Create;
  FFiles.NameValueSeparator := ':';
  FFileTypes.NameValueSeparator := ':';
  FTitles.NameValueSeparator := ':';
  FTemplates.NameValueSeparator := ':';
  FPaths.NameValueSeparator := ':';
  FReportPath := AReportPath;
  FDocumentNode := GetDocumentNode;
  RebuildIndex;
end;  // TBaseReportIndex.Create

{-------------------------------------------------------------------------------
  Object cleanup.
}
destructor TBaseReportIndex.Destroy;
begin
  FFiles.Free;
  FFileTypes.Free;
  FPaths.Free;
  FTitles.Free;
  FTemplates.Free;
  inherited;
end;  // TBaseReportIndex.Destroy

{-------------------------------------------------------------------------------
  Retrieve the document node for an xml file.
}
function TBaseReportIndex.LoadXML(const AFileName: string): IXMLNode;
var
  lXMLDoc: IXMLDocument;
begin
  lXMLDoc := NewXMLDocument;
  lXMLDoc.LoadFromFile(AFileName);
  if lXmlDoc.DocumentElement.NodeName = FDocumentNode then
    Result := lXMLDoc.DocumentElement
  else
    raise ECRClassException.CreateNonCritical(Format(ResStr_DocumentElementInvalid,
        [AFileName, FDocumentNode, lXmlDoc.DocumentElement.NodeName]));
end;  // TBaseReportIndex.LoadXML

{-------------------------------------------------------------------------------
  Load the index file from disk, into the string lists used internally.
}
procedure TBaseReportIndex.ReadIndexFile;
var
  lFile: TextFile;
  lLine: String;
begin
  FFiles.Clear;
  FFileTypes.Clear;
  FPaths.Clear;
  FTitles.Clear;
  FTemplates.Clear;
  if FileExists(FReportPath + 'CrIdx.txt') then begin
    AssignFile(lFile, FReportPath + 'CrIdx.txt');
    Reset(lFile);
    // Start on the first part of the index - the file and date stamp list
    while not EOF(lFile) do begin
      ReadLn(lFile, lLine);
      if lLine = '[Reports]' then
        FFiles.Add(lLine)
      else if lLine = '[Where Types]' then
        FFileTypes.Add(lLine)
      else if lLine = '[Paths]' then
        FPaths.Add(lLine)
      else if lLine = '[Titles]' then
        FTitles.Add(lLine)
      else if lLine = '[Templates]' then
        FTemplates.Add(lLine);
    end;
    CloseFile(lFile);
  end;
end;  // TBaseReportIndex.ReadIndexFile

{-------------------------------------------------------------------------------
  Rebuilds the index.  The folder can be specified, otherwise uses the standard report path. 
}
procedure TBaseReportIndex.RebuildIndex(const APath: string='');
var
  lStandardPath: String;
begin
  if APath <> '' then begin
    lStandardPath := FReportPath;
    FReportPath := APath;
  end;
  try
    ReadIndexFile;
    UpdateIndex;
    WriteIndexFile;
  finally
    if APath <> '' then
      // reset the normal path
      FReportPath := lStandardPath;
  end;
end;  // TBaseReportIndex.RebuildIndex

{-------------------------------------------------------------------------------
  Repopulates the type index for a given XML file.  This obtains the data types supported by
      the file, and stores an entry in FFileTypes:
  filename + ':' + Ord(data type)
}
procedure TBaseReportIndex.ReIndexFile(const AFileName: string);
var
  i: Integer;
  lReportFile: TReportFile;
begin
  // remove existing types for this file
  RemoveFileFromIndex(AFileName);
  // repopulate the type list - parse XML to get details
  lReportFile := TReportFile.Create(nil);
  try
    lReportFile.ReadXML(LoadXML(FReportPath + AFileName));
    try
      for i := 0 to lReportFile.ReportSQL.WhereClauses.Count-1 do begin
        if lReportFile.ReportSQL.WhereClauses[i].KeyType <> ktAddin then
          FFileTypes.Add(AFileName + ':' + IntToStr(Ord(
              lReportFile.ReportSQL.WhereClauses[i].KeyType)))
        else
          // If it's an addin defined keytype, write the name of the
          // keytype as well as the fact that it's addin defined
          FFileTypes.Add(AFileName + ':' + IntToStr(Ord(
              lReportFile.ReportSQL.WhereClauses[i].KeyType)) + ':' +
              lReportFile.ReportSQL.WhereClauses[i].AddinKeyType);
      end;
      FPaths.Values[AFileName] := lReportFile.MenuPath;
      FTitles.Values[AFileName] := lReportFile.Title;
      if lReportFile.Template <> '' then
        FTemplates.Add(AFileName + ':' + lReportFile.Template);
    finally
      lReportFile.Free;
    end; // try
  except
    on E:EDOMParseError do
      ShowInformation(Format(ResStr_ParseFailure, [AFileName, E.Message]));
  end;
end;  // TBaseReportIndex.ReIndexFile

{-------------------------------------------------------------------------------
  Clears the references to this file from the type index string list. 
}
procedure TBaseReportIndex.RemoveFileFromIndex(const AFileName: string);
var
  lIdx: Integer;

    procedure DeleteFromList(AList: TStringList);
    begin
      lIdx := AList.IndexOfName(AFileName);
      if lIdx>-1 then
        AList.Delete(lIdx);
    end;

begin
  lIdx := FFileTypes.IndexOfName(AFileName);
  while lIdx > -1 do begin
    FFileTypes.Delete(lIdx);
    lIdx := FFileTypes.IndexOfName(AFileName);
  end;
  DeleteFromList(FFiles);
  DeleteFromList(FPaths);
  DeleteFromList(FTitles);
  DeleteFromList(FTemplates);
end;  // TBaseReportIndex.RemoveFileFromIndex

{-------------------------------------------------------------------------------
  Rebuild the index.  Finds all the files, and checks date time stamps to see if they need
      updating.  Also clears details for missing files.
}
procedure TBaseReportIndex.UpdateIndex;
var
  lSr: TSearchRec;
  lCheckFile: Boolean;
  lFileList: TStringList;

    procedure ClearMissingFiles(AList: TStringlist);
    var
      i: Integer;
    begin
      for i := AList.Count-1 downto 0 do
        if lFileList.IndexOf(AList.Names[i])=-1 then
          AList.Delete(i);
    end;

begin
  lFileList := TStringList.Create;
  lFileList.Sorted := True;
  lFileList.Duplicates := dupIgnore;
  try
    if FindFirst(FReportPath + '*.xml', faAnyFile, lSr)=0 then
    begin
      repeat
        lCheckFile := True;
        if FFiles.Values[lSr.Name] <> '' then begin
          if StrToInt(FFiles.Values[lSr.Name]) = lSr.Time then
            lCheckFile := False;
        end;
        try
        if lCheckFile then
          ReIndexFile(lSr.Name);
        FFiles.Values[lSr.Name] := IntToStr(lSr.Time);
        // remember all the file names, so we can delete removed files from the index
        lFileList.Add(lSr.Name);
        except on E:Exception do
          ShowInformation(Format(ResStr_ParseFailure, [lSR.Name, E.Message]));
        end;
      until FindNext(lSr) <> 0;
      ClearMissingFiles(FFileTypes);
      ClearMissingFiles(FFiles);
      ClearMissingFiles(FPaths);
      ClearMissingFiles(FTitles);
      ClearMissingFiles(FTemplates);
    end;
  finally
    lFileList.Free;
  end; // try
end;  // TBaseReportIndex.UpdateIndex

{-------------------------------------------------------------------------------
  Write the index file to disk.
}
procedure TBaseReportIndex.WriteIndexFile;
var
  lFileData: TStringList;
begin
  lFileData := TStringList.Create;
  with lFileData do begin
    try
      Add('[Reports]');
      AddStrings(FFiles);
      Add('[Where Types]');
      AddStrings(FFileTypes);
      Add('[Paths]');
      AddStrings(FPaths);
      Add('[Titles]');
      AddStrings(FTitles);
      Add('[Templates]');
      AddStrings(FTemplates);
      try
        SaveToFile(FReportPath + 'CrIdx.txt');
      except
        on EFCreateError do ; // user has no rights to update the index file
      end;
    finally
      Free;
    end;
  end;
end;  // TBaseReportIndex.WriteIndexFile

{-==============================================================================
    TCRReportIndex
===============================================================================}
{-------------------------------------------------------------------------------
}
function TCRReportIndex.GetDocumentNode: String;
begin
  Result := EL_CUSTOMREPORT;
end;

{-==============================================================================
    TBUReportIndex
===============================================================================}
{-------------------------------------------------------------------------------
}
function TBUReportIndex.GetDocumentNode: String;
begin
  Result := EL_BATCHUPDATE;
end;

end.
