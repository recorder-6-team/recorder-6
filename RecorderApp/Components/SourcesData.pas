//==============================================================================
//  Unit:        SourcesData
//
//  Implements:  TdmSources
//               TExternalRefItem
//               TExternalRefList
//               TInternalRefItem
//               TInternalRefList
//
//  Description: Implements the data access functionality for the Sources
//               component.
//
//               TExternalRefItem and TExternalRefList
//               Helper classes for external references.
//
//               TInternalRefItem and TInternalRefList
//               Helper classes for internal references
//
//  Author:      John van Breda
//  Created:     22 April 1999
//
//  Changes:     Eric Salmon - 27/03/2002
//               Implementation changes concerning the handling of the database
//               object, makes it easier to deal with either TtaDatabase or
//               TADOConnection.
//
//  Last Revision Details:
//    $Revision: 42 $
//    $Date: 16/04/09 9:34 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 1999
//
//==============================================================================

unit SourcesData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DataClasses, Db, JNCCDatasets, DropStruct, Grids, ComCtrls, Constants, ADODB;

const
  COL_ICON     = 0;
  COL_DOCUMENT = 1;
  COL_ORIGINAL = 2;
  COL_TYPE     = 3;

resourcestring
  ResStr_InternalRef_Original     = 'Yes';
  ResStr_InternalRef_Not_Original = 'No';

type
  eSourceListError = class(EDatalistError);
  eSourcesDataError = class(Exception);

  TGetNextKey = function (const ATableName, AKeyFieldName:string):TKeyString of object;

  //============================================================================
  TdmSources = class(TDataModule)
    qryGetExtSources : TJNCCQuery;
    qryGetIntSources : TJNCCQuery;
    qryGetRef: TJNCCQuery;
  private
    FNextKey: TGetNextKey;
    FDatabase: TADOConnection;
    FJoinTable: String;
    procedure SetDatabase(const ADataset: TCustomADODataset);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ConvertKeysToFiles(iSourceData : TKeyList; ioFiles : TStringList);
    procedure UpdateDataSets(const ADatabase: TADOConnection);
    property NextKey:TGetNextKey read FNextKey write FNextKey;
    property JoinTable: string read FJoinTable write FJoinTable;
  end;  // TdmSources

  //============================================================================
  { Internal references item.  This item can be referenced by just a key so
       no need for additional properties - the ones present are just for
       display optimisation in the grid }
  TInternalRefItem = class(TGridDataItem)
  private
    FReference : string;
    FOriginal  : string;
    FType      : string;
    FSourceKey : TKeyString;
    FFileName: String;
    procedure SetOriginal(const Value: string);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell(const iX : integer) : string; override;
  public
    procedure SetupProperties(const AKey: TKeyString; const AOriginal, ARef, AType,
        AFileName: string);
    property Reference: string read FReference;
    property Original: string read FOriginal write SetOriginal;
    property RefType: string read FType;
    property FileName: String read FFileName;
    property SourceKey: TKeyString read FSourceKey;
  end;  // TInternalRefItem

  //----------------------------------------------------------------------------
  TInternalRefList = class(TGridDataList)
  private
    FJoinTable     : string;
    FJoinKeyField  : string;
    FSourcesForKey : TKeyString;
    FdmSources     : TdmSources;
  protected
    function ItemsDisplayName: String; override;
    procedure ProcessUpdate; override;
    procedure DoAdditions(iQuery : TJNCCQuery);
  public
    constructor Create(const iSourcesForKey : TKeyString; iStringGrid : TStringGrid;
      idmSources:TdmSources);
    procedure RefreshList(const iNewKey: TKeyString);
    property SourcesForKey:TKeyString read FSourcesForKey write FSourcesForKey;
  end;  // TInternalRefList

  //============================================================================
  TExternalRefItem = class(TStringDataItem)
  private
    FFileName:string;
    FTitle: string;
    FSourceKey :TKeyString;
  protected
    procedure SetFileName(const Value: string);
    procedure SetTitle(const Value: string);
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
  public
    property FileName : string read FFileName write SetFileName;
    property Title : string read FTitle write SetTitle;
    property SourceKey:TKeyString read FSourceKey;
  end;  // TExternalRefItem

  //----------------------------------------------------------------------------
  TExternalRefList = class(TStringDataList)
  private
    FJoinTable     : string;
    FSourcesForKey : TKeyString;
    Fdmsources     : TdmSources;
    FDefaultPath: string;
    procedure SetDefaultPath(const Value: string);
  protected
    function ItemsDisplayName: String; override;
    function GetExternalRef(const iIndex: integer): TExternalRefItem;
    function GetText(iDataItem : TDataItem): string; override;
    { implementation of abstract method }
    procedure ProcessUpdate; override;
    procedure DoAdditions(iQuery : TJNCCQuery);
    procedure DoModifications(iQuery: TJNCCQuery); reintroduce;
  public
    constructor Create(const iSourcesForKey : TKeyString; iStrings : TStrings;
      idmSources:TdmSources); reintroduce; overload;
    procedure RefreshList(const iNewKey: TKeyString);
    property DataItems[const iIndex : integer]: TExternalRefItem read GetExternalRef;
    property SourcesForKey:TKeyString read FSourcesForKey write FSourcesForKey;
    property DefaultPath: string read FDefaultPath write SetDefaultPath;
  end;  // TExternalRefList

//==============================================================================
implementation

uses
  GeneralFunctions;

resourcestring
  ResStr_ListUninitialised = 'String list for file conversions is not initialised in call to ConvertKeysToFiles';
  ResStr_WritingInternalRef = 'Writing internal ref';
  ResStr_InternalRef =  ' Internal Reference';
  ResStr_WritingExtRef =  'Writing external ref';
  ResStr_ExternalRef =  ' External Reference';

{$R *.DFM}

//==============================================================================
{ TdmSources }
constructor TdmSources.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
end;  // TdmSources.Create

//------------------------------------------------------------------------------
procedure TdmSources.ConvertKeysToFiles(iSourceData : TKeyList; ioFiles: TStringList);
var
  lQuery : TJNCCQuery;
  iCount : integer;
begin
  if ioFiles = nil then
    raise ESourcesDataError.Create(ResStr_ListUninitialised);
  ioFiles.Clear;
  lQuery := TJNCCQuery.Create(nil);
  try
    SetDatabase(lQuery);
    with lQuery do begin
      SQL.Add('SELECT File_Name FROM Source_File WHERE Source_Key IN (');
      for iCount := 0 to iSourceData.Header.ItemCount-1 do
        SQL.Add('''' + iSourceData.Items[iCount].KeyField1 + ''', ');
      SQL.Add(')');
      Open;
      while not Eof do begin
        ioFiles.Add(FieldByName('Source_Key').AsString);
        Next;
      end;
      Close;
    end; // with
  finally
    lQuery.Free;
  end;
end;  // TdmSources.ConvertKeysToFiles

//------------------------------------------------------------------------------
procedure TdmSources.UpdateDataSets(const ADatabase: TADOConnection);
begin
  FDatabase := ADatabase;
  SetDatabase(qryGetExtSources);
  SetDatabase(qryGetIntSources);
  SetDatabase(qryGetRef);
end;  // TdmSources.UpdateDataSets

//------------------------------------------------------------------------------
procedure TdmSources.SetDatabase(const ADataset: TCustomADODataset);
begin
  ADataset.Connection := FDatabase;
end;  // SetDatabase

//==============================================================================
{ TInternalRefItem }
procedure TInternalRefItem.InitFromRecord(iDataset: TDataset);
var lreTemp:TRichEdit;
begin
  lreTemp:=TRichEdit.Create(TInternalRefList(OwnerList).FdmSources);
  try
    lreTemp.Parent :=TWinControl(TInternalRefList(OwnerList).FdmSources.Owner.Owner);
    lreTemp.Visible:=false;
    with iDataSet do
      try
        FSourceKey := FieldByName('Source_Link_Key').AsString;
        FItemKey   := FieldByName('Source_Key').AsString;
        lreTemp.Lines.Assign(FieldByName('Full_Reference'));
        if Pos(#13, lreTemp.Text)>0 then
          FReference := Copy(lreTemp.Text, 1, Pos(#13, lreTemp.Text) - 1)
        else
          FReference := lreTemp.Text;
        if FieldByName('Original').AsBoolean then FOriginal := ResStr_InternalRef_Original
                                             else FOriginal := ResStr_InternalRef_Not_Original;
        { Type is from a termlist }
        FType     := FieldByName('Reference_Type').AsString;
        FFileName := FieldByName('Original_File').AsString;
      except on Exception do
        raise EDataListError.Create(ResStr_BadStructure + 'TITLE, ORIGINAL or SHORT_NAME');
      end;
  finally
    lreTemp.Free;
  end;
end;  // TInternalRefItem.InitFromRecord

//------------------------------------------------------------------------------
procedure TInternalRefItem.WriteToRecord(iDataset: TDataset);
begin
  try
    { Note only Original field can be modified here }
    iDataset.FieldByName('Original').AsBoolean := SameText(FOriginal, ResStr_InternalRef_Original);
  except
    on Exception do
      raise EDataListError.Create(ResStr_BadStructure + ResStr_WritingInternalRef);
  end;
end;  // TInternalRefItem.WriteToRecord

//------------------------------------------------------------------------------
function TInternalRefItem.GetCell(const iX: integer): string;
begin
  case iX of
    COL_ICON    : Result := FFileName;
    COL_DOCUMENT: Result := FReference;
    COL_ORIGINAL: Result := FOriginal;
    COL_TYPE    : Result := FType;
  else
    Result := '';
  end;
end;  // TInternalRefItem.GetCell

//------------------------------------------------------------------------------
procedure TInternalRefItem.SetOriginal(const Value: string);
begin
  FOriginal := Value;
  SetModified;
end;  // TInternalRefItem.SetOriginal

//------------------------------------------------------------------------------
procedure TInternalRefItem.SetupProperties(const AKey: TKeyString; const AOriginal,
  ARef, AType, AFileName: String);
begin
  FItemKey  := AKey;
  FOriginal := AOriginal;
  FReference:= ARef;
  FType     := AType;
  FFileName := AFileName;
end;  // TInternalRefItem.SetupProperties

//==============================================================================
{ TInternalRefList }
function TInternalRefList.ItemsDisplayName:String;
begin
  Result := ReadableFormat(Copy(FJoinTable, 1, Pos('_SOURCES', FJoinTable) - 1)) +
            ResStr_InternalRef;
end;

//------------------------------------------------------------------------------
constructor TInternalRefList.Create(const iSourcesForKey: TKeyString;
  iStringGrid: TStringGrid; idmSources:TdmSources);
begin
  { Store the join table name and ID for use when updating }
  FJoinTable    :=UpperCase(idmSources.JoinTable);
  FSourcesForKey:=iSourcesForkey;
  FdmSources    :=idmSources;
  { Extract the join field from the table name - eg TAXON_OCCURRENCE_SOURCES
       becomes TAXON_OCCURRENCE_KEY }
  FJoinKeyField := Copy(FJoinTable, 1, Pos('_SOURCES', FJoinTable) - 1) + '_KEY';
  FdmSources.qryGetIntSources.SQL[2] := FJoinTable;
  FdmSources.qryGetIntSources.SQL[5] := 'JN.' + FJoinKeyField;
  FdmSources.qryGetIntSources.Parameters.ParamByName('KeyParameter').Value := FSourcesForKey;
  inherited Create(FdmSources.qryGetIntSources, 'SOURCE_KEY', iStringGrid, TInternalRefItem);
end;  // TInternalRefList.Create

//------------------------------------------------------------------------------
procedure TInternalRefList.ProcessUpdate;
var
  lUpdateQuery : TJNCCQuery;
begin
  { Create a temporary query for doing updates }
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    FdmSources.SetDatabase(lUpdateQuery);
    DeleteFromTable(lUpdateQuery, FJoinTable, FJoinKeyField, 'Source_Key', FSourcesForKey);
    DoAdditions(lUpdateQuery);
    DoModifications;
  finally
    lUpdateQuery.Free;
  end;
end;  // TInternalRefList.ProcessUpdate

//------------------------------------------------------------------------------
procedure TInternalRefList.DoAdditions(iQuery: TJNCCQuery);
var iCount   :integer;
    lDataItem:TInternalRefItem;
    lNewKey  :TKeyString;
begin
  with iQuery do begin
    SQL.Clear;
    SQL.Add('INSERT INTO '+FJoinTable);
    SQL.Add('');
    for iCount := 0 to ItemsToAdd.Count-1 do
    begin
      lDataItem:=TInternalRefItem(ItemsToAdd[iCount]);
      lNewKey  :=FdmSources.NextKey(FJoinTable,'Source_Link_Key');
      SQL[1] := 'VALUES ('''
          + lNewKey + ''', '''
          + FSourcesForKey + ''', '''
          + lDataItem.ItemKey + ''', '
          + SQLBoolString[lDataItem.Original = ResStr_InternalRef_Original]
          + ', Null)';
      ExecuteSQL;
    end;
  end;
end;  // TInternalRefList.DoAdditions

//------------------------------------------------------------------------------
procedure TInternalRefList.RefreshList(const iNewKey: TKeyString);
begin
  FSourcesForKey:=iNewKey;
  FdmSources.qryGetIntSources.Parameters.ParamByName('KeyParameter').Value := FSourcesForKey;
  Refresh;
end;  // TInternalRefList.RefreshList

{===============================================================================
  TExternalRefItem
===============================================================================}
{ Reads the FFilename and FTitle values from a supplied dataset.  If the field is missing,
     an error is raised }
procedure TExternalRefItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do begin
    if FieldList.IndexOf('File_Name') <> -1 then begin
      FFileName  := FieldByName('File_Name').AsString;
      FTitle     := FieldByName('Title').AsString;
      FSourceKey := FieldByName('Source_Link_Key').AsString;
      FItemKey   := FieldByName('Source_Key').AsString;
    end else
      raise EDataListError.Create(ResStr_BadStructure + 'FILE_NAME');
  end;
end;  // TExternalRefItem.InitFromRecord

//------------------------------------------------------------------------------
procedure TExternalRefItem.WriteToRecord(iDataset: TDataset);
begin
  try
    iDataset.FieldByName('File_Name').AsString :=FFileName;
    iDataset.FieldByName('Title').AsString     :=FTitle;
    iDataset.FieldByName('Source_Key').AsString:=FItemKey;
  except
    on Exception do
      raise EDataListError.Create(ResStr_BadStructure + ResStr_WritingExtRef);
  end;
end;  // TExternalRefItem.WriteToRecord

//------------------------------------------------------------------------------
{ Accessor method for the FileName property }
procedure TExternalRefItem.SetFileName(const Value: string);
begin
  FFileName := Value;
  { Note it doesn't matter if we do this during AddNew as the Added overrides
    the modified }
  SetModified;
end;  // TExternalRefItem.SetFileName

//------------------------------------------------------------------------------
{ Accessor method for the Title property }
procedure TExternalRefItem.SetTitle(const Value: string);
begin
  FTitle := Value;
  { Note it doesn't matter if we do this during AddNew as the Added overrides
    the modified }
  SetModified;
end;  // TExternalRefItem.SetTitle

//==============================================================================
{ TExternalRefList }
function TExternalRefList.ItemsDisplayName: String;
begin
  Result := ReadableFormat(Copy(FJoinTable, 1, Pos('_SOURCES', FJoinTable) - 1)) +
            ResStr_ExternalRef;
end;

//------------------------------------------------------------------------------
constructor TExternalRefList.Create(const iSourcesForKey: TKeyString;
  iStrings: TStrings; idmSources:TdmSources);
var lJoinKeyField:string;
begin
  { Store the join table name and ID for use when updating }
  FJoinTable    :=UpperCase(idmSources.JoinTable);
  FSourcesForKey:=iSourcesForkey;
  FdmSources    :=idmSources;
  { Extract the join field from the table name - eg TAXON_OCCURRENCE_SOURCES
       becomes TAXON_OCCURRENCE_KEY }
  lJoinKeyField := Copy(FJoinTable, 1, Pos('_SOURCES', FJoinTable) - 1) + '_KEY';
  FdmSources.qryGetExtSources.SQL[2] := FJoinTable;
  FdmSources.qryGetExtSources.SQL[5] := 'JN.' + lJoinKeyField;
  FdmSources.qryGetExtSources.Parameters.ParamByName('KeyParameter').Value := FSourcesForKey;
  inherited Create(FdmSources.qryGetExtSources, 'SOURCE_KEY', iStrings, TExternalRefItem);
end;  // TExternalRefList.Create

//------------------------------------------------------------------------------
procedure TExternalRefList.DoAdditions(iQuery: TJNCCQuery);
var lNewKey, lSourceKey:TKeyString;
    iCount  :integer;
begin
  with iQuery do begin
    ParseSQL := false;
    SQL.Clear;
    SQL.Add('INSERT INTO');
    SQL.Add('');
    SQL.Add('VALUES ');
    SQL.Add(''); // blank line populated in loop
    for iCount:=0 to ItemsToAdd.Count-1 do begin
      lSourceKey:=FdmSources.NextKey('Source','Source_Key');
      { Do SOURCE table }
      SQL[1]:='SOURCE (SOURCE_KEY, INTERNAL)';
      SQL[3]:='(''' + lSourceKey + ''', 0)';  // SOURCE_KEY, INTERNAL
      ExecuteSQL;

      { Do SOURCE_FILE table }
      SQL[1]:='SOURCE_FILE(Source_Key, File_Name, Title)';
      { Note - drop null terminator from file name }  // SOURCE_KEY, FILE_NAME
      SQL[3]:='('''+lSourceKey+''', '''+DuplicateCharacters(
          TExternalRefItem(ItemsToAdd[iCount]).FileName, '''')+''', '''+DuplicateCharacters(
          TExternalRefItem(ItemsToAdd[iCount]).Title, '''')+''')';
      ExecuteSQL;

      { Do join table }
      lNewKey:=FdmSources.NextKey(FJoinTable,'Source_Link_Key');
      SQL[1]:=FJoinTable;
      SQL[3]:='(''' + lNewKey + ''', ''' + FSourcesForKey + ''', ''' + lSourceKey + ''', 0, Null)';
      ExecuteSQL;
    end;
  end;
end;  // TExternalRefList.DoAdditions

//------------------------------------------------------------------------------
procedure TExternalRefList.DoModifications(iQuery: TJNCCQuery);
var iCount : integer;
begin
  if ItemsToModify.Count > 0 then
  begin
    iQuery.SQL.Text:='UPDATE Source_File SET File_Name = :FileName, Title = :Title ' +
                     'WHERE Source_Key = :SourceKey';
    for iCount := 0 to ItemsToModify.Count-1 do
    begin
      iQuery.Parameters.ParamByName('FileName').Value  := TExternalRefItem(ItemsToModify[iCount]).FileName;
      iQuery.Parameters.ParamByName('Title').Value   := TExternalRefItem(ItemsToModify[iCount]).Title;
      iQuery.Parameters.ParamByName('SourceKey').Value := TExternalRefItem(ItemsToModify[iCount]).ItemKey;
      iQuery.ExecSQL;
    end; // for
  end; // if count
end;  // TExternalRefList.DoModifications

//------------------------------------------------------------------------------
procedure TExternalRefList.ProcessUpdate;
var
  lUpdateQuery : TJNCCQuery;
begin
  { Create a temporary query for doing updates }
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    FdmSources.SetDatabase(lUpdateQuery);
    DoAdditions(lUpdateQuery);
    DoModifications(lUpdateQuery);
    DeleteFromTable(lUpdateQuery, 'Source_File', 'Source_Key');
    DeleteFromTable(lUpdateQuery, FJoinTable, 'Source_Key');
    DeleteFromTable(lUpdateQuery, 'Source', 'Source_Key');
  finally
    lUpdateQuery.Free;
  end;
end;  // TExternalRefList.ProcessUpdate

//------------------------------------------------------------------------------
{ property accessor method for the DataItems array property }
function TExternalRefList.GetExternalRef(const iIndex: integer): TExternalRefItem;
begin
  { Type cast the GetDataItem function }
  Result := TExternalRefItem(GetDataItem(iIndex));
end;  // TExternalRefList.GetExternalRef

//------------------------------------------------------------------------------
function TExternalRefList.GetText(iDataItem: TDataItem): string;
begin
  if TExternalRefItem(iDataItem).Title<>'' then
    // This is a URL with a title specified, so we can display that
    Result := TExternalRefItem(iDataItem).Title
  else begin
    Result := TExternalRefItem(iDataItem).FileName;
    // If the file is in the default external file path, then there is no need to display the whole path
    if Copy(Result, 0, Length(FDefaultPath))=FDefaultPath then
      Result := Copy(Result, Length(FDefaultPath)+1, 1000);
  end;
end;  // TExternalRefList.GetText

//------------------------------------------------------------------------------
procedure TExternalRefList.RefreshList(const iNewKey:TKeyString);
begin
  FSourcesForKey:=iNewKey;
  FdmSources.qryGetExtSources.Parameters.ParamByName('KeyParameter').Value := FSourcesForKey;
  Refresh;
end;  // TExternalRefList.RefreshList

//------------------------------------------------------------------------------
procedure TExternalRefList.SetDefaultPath(const Value: string);
begin
  FDefaultPath := Value;
end;

//==============================================================================

end.

