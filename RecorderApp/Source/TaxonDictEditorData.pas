//==============================================================================
//  Unit:        TaxonDictEditorData
//
//  Implements:  TdmTaxonDictEditor
//               TBaseTaxonList
//               TTaxonFactsItem
//               TTaxonFactsList
//               TTaxonNamesItem
//               TTaxonNamesList
//               TTaxonStatusItem
//               TTaxonStatusList
//
//  Description: Contains taxon dictionary editor specific functions to
//               access, format, and save data.
//               Helper classes: Taxon Facts/Names/Status Item and List. Used
//               to manage taxon facts, user names and statuses before
//               committing any changes to the database.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 19 $
//    $Date: 27/12/07 16:10 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TaxonDictEditorData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseTaxonDictDataUnit, Grids, DataClasses, DictionaryDataGeneric, Constants,
  ExceptionForm, JNCCDatasets, ADODB, DatabaseAccessADO, HTTPApp, HTTPProd;

type
  ETaxonEditorError = class(TExceptionPath);
  ETaxonUserName = class(TExceptionPath);

  TdmTaxonDictEditor = class(TBaseTaxonDictData)
    dsRank: TDataSource;
    dsGeneral: TDataSource;
    dsStatusType: TDataSource;
    qryStatusType: TJNCCQuery;
    qryRank: TJNCCQuery;
    qrySortcode: TJNCCQuery;
    qryTaxonNames: TJNCCQuery;
    qryRankMinChild: TJNCCQuery;
  private
    procedure UpdateSortCode(const ASortCode: Integer; const AParentKey,
      AListVersion: TKeyString; const AddItem: Boolean);
  public
    procedure DelTaxonUserNames(const AKey: TKeyString);
    procedure DelStatuses(const AKey: TKeyString);
    procedure DelFacts(const AKey: TKeyString);
    procedure CleanGroupIndex(const AKey: TKeyString);
    function ReferencedInTable(const AKey: TKeyString): Boolean;
    procedure UpdateSortCodeDel(const ASortCode: LongInt; const AParentKey: TKeyString;
      const AListVersion: TKeyString);
    procedure UpdateSortCodeAdd(const ASortCode: LongInt; const AParentKey: TKeyString;
      const AListVersion: TKeyString);
  end;

  //============================================================================
  // base list for taxon edit screen
  TBaseTaxonList = class(TGridDataList)
  protected
    FTaxonVersionKey : TKeyString;
    FTaxonListItemKey: TKeyString;
    // Accessor methods
    procedure SetTaxonListItemKey(const Value: TKeyString);
    procedure SetTaxonVersionKey(const Value: TKeyString);
  public
    property TaxonVersionKey: TKeyString read FTaxonVersionKey write SetTaxonVersionKey;
    property TaxonListItemKey: TKeyString read FTaxonListItemKey write SetTaxonListItemKey;
  end;

  //============================================================================
  TTaxonStatusItem = class(TBaseStatusItem);

  //============================================================================
  TTaxonStatusList = class(TBaseTaxonList)
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(ADataset: TDataSet; AKeyFieldName: String; AStringGrid: TStringGrid);
  end;

  //============================================================================
  TTaxonFactsItem = class(TBaseFactsItem);

  //============================================================================
  TTaxonFactsList = class(TBaseTaxonList)
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(ADataset: TDataSet; AKeyFieldName: String; AStringGrid: TStringGrid);
  end;

  //============================================================================
  TTaxonNamesItem = class(TGridDataItem)
  private
    FItemName: String;
    FLanguage: String;
    FPreferred: Boolean;
    procedure SetItemName(const Value: String);
    procedure SetLanguage(const Value: String);
    procedure SetPreferred(const Value: Boolean);
  protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    property ItemName: String read FItemName write SetItemName;
    property Language: String read FLanguage write SetLanguage;
    property Preferred: Boolean read FPreferred write SetPreferred;
  end;

  //----------------------------------------------------------------------------
  TTaxonNamesList = class(TGridDataList)
  private
    FTaxonListItemKey: TKeyString;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(ADataset: TDataSet; AKeyFieldName: String; AStringGrid: TStringGrid);
    property TaxonListItemKey: TKeyString read FTaxonListItemKey write FTaxonListItemKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, SQLConstants;

//==============================================================================
//==============================================================================
{ Base Taxon List }
//------------------------------------------------------------------------------
procedure TBaseTaxonList.SetTaxonListItemKey(const Value: TKeyString);
begin
  FTaxonListItemKey := Value;
end;  // TBaseTaxonList.SetTaxonListItemKey

//------------------------------------------------------------------------------
procedure TBaseTaxonList.SetTaxonVersionKey(const Value: TKeyString);
begin
  FTaxonVersionKey := Value;
end;  // TBaseTaxonList.SetTaxonVersionKey

//==============================================================================
//==============================================================================
{ TTaxonStatusList }
//------------------------------------------------------------------------------
constructor TTaxonStatusList.Create(ADataset: TDataSet; AKeyFieldName: String;
  AStringGrid: TStringGrid);
begin
  inherited Create(ADataSet, AKeyFieldName, AStringGrid, TTaxonStatusItem);
end;  // TTaxonStatusList.Create

//------------------------------------------------------------------------------
function TTaxonStatusList.ItemsDisplayName: String;
begin
  Result := ResStr_TaxonStatus;
end;

//------------------------------------------------------------------------------
procedure TTaxonStatusList.DoAdditions;
var i           : integer;
    lDataItem   : TTaxonStatusItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Taxon_Designation WHERE Taxon_Designation_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := ItemsToAdd.Items[i];
          if lDataItem <> nil then begin
            Append;
            FieldByName('Taxon_Designation_Key').AsString  :=
                dmGeneralData.GetNextKey('Taxon_Designation', 'Taxon_Designation_Key');
            FieldByName('Taxon_List_Item_Key').AsString    := FTaxonListItemKey;
            FieldByName('Date_From').AsString              := lDataItem.DateFromAsString;
            FieldByName('Date_To').AsString                := lDataItem.DateToAsString;
            FieldByName('Status_Geographic_Area').AsString := lDataItem.GeographicArea;
            FieldByName('Status_Constraint').AsString      := lDataItem.Constraints;
            lDataItem.Details.Position := 0;
            TMemoField(FieldByName('Detail')).LoadFromStream(lDataItem.Details);
            FieldByName('Taxon_Designation_Type_Key').AsString := lDataItem.StatusTypeKey;
            if lDataItem.SourceKey <> '' then
              FieldByName('Source_Key').AsString := lDataItem.SourceKey;
            FieldByName('Entered_By').AsString   := AppSettings.UserID;
            Post;
          end;
        end;
        Close;
      except on E:Exception do
        raise ETaxonEditorError.Create(Format(ResStr_AddFail, ['TAXON_DESIGNATION']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TTaxonStatusList.DoAdditions

//------------------------------------------------------------------------------
procedure TTaxonStatusList.ProcessUpdate;
var lqryUpdate: TJnccQuery;
begin
  { Create a temporary query for doing updates }
  lqryUpdate := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryUpdate]);
    DoAdditions;
    DoModifications;
    DeleteFromTable(lqryUpdate, 'TAXON_DESIGNATION', 'TAXON_DESIGNATION_KEY');
  finally
    lqryUpdate.Free;
  end;
end;  // TTaxonStatusList.ProcessUpdate

//==============================================================================
//==============================================================================
{ TTaxonFactsList }
//------------------------------------------------------------------------------
constructor TTaxonFactsList.Create(ADataset: TDataSet; AKeyFieldName: String;
  AStringGrid: TStringGrid);
begin
  inherited Create(ADataset, AKeyFieldName, AStringGrid,TTaxonFactsItem);
end;  // TTaxonFactsList.Create

//------------------------------------------------------------------------------
function TTaxonFactsList.ItemsDisplayName: String;
begin
  Result := ResStr_TaxonFact;
end;

//------------------------------------------------------------------------------
procedure TTaxonFactsList.DoAdditions;
var i           : Integer;
    lDataItem   : TTaxonFactsItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Taxon_Fact WHERE Taxon_Fact_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := ItemsToAdd.Items[i];
          if lDataItem <> nil then begin
            Append;
            FieldByName('Taxon_Fact_Key').AsString    :=
                dmGeneralData.GetNextKey('Taxon_Fact','Taxon_Fact_Key');
            FieldByName('Fact_Vague_Date_Start').Text := lDataItem.Date;
            FieldByName('Taxon_Version_Key').AsString := FTaxonVersionKey;
            FieldByName('Type').AsString              := lDataItem.FactType;
            FieldByName('Data').AsString              := lDataItem.Facts;
            FieldByName('Title').AsString             := lDataItem.Title;
            if lDataItem.SourceKey <> '' then
              FieldByName('Source_Key').AsString := lDataItem.SourceKey;
            FieldByName('Entered_By').AsString := AppSettings.UserID;
            Post;
          end;
        end;
        Close;
      except on E:Exception do
        raise ETaxonEditorError.Create(Format(ResStr_AddFail, ['TAXON_FACT']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TTaxonFactsList.DoAdditions

//------------------------------------------------------------------------------
procedure TTaxonFactsList.ProcessUpdate;
var lqryUpdate: TJnccQuery;
begin
  { Create a temporary query for doing updates }
  lqryUpdate := TJnccQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lqryUpdate]);
  try
    DoAdditions;
    DoModifications;
    DeleteFromTable(lqryUpdate, 'TAXON_FACT', 'TAXON_FACT_KEY');
  finally
    lqryUpdate.Free;
  end;
end;  // TTaxonFactsList.ProcessUpdate

//==============================================================================
//==============================================================================
{ TdmTaxonDictEditor }
//------------------------------------------------------------------------------
function TdmTaxonDictEditor.ReferencedInTable(const AKey: TKeyString): Boolean;
var lQry  : TJnccQuery;
    lsKeys: String;
begin
  Result := false;
  lsKeys := dmGeneralData.GetKeysToString('SELECT Taxon_List_Item_Key FROM Taxon_Version TV, Taxon_List_Item TLI ' +
                                          'WHERE TV.Taxon_Key = ''' + AKey + ''' ' +
                                          'AND TLI.Taxon_Version_Key = TV.Taxon_Version_Key');
  if lsKeys <> '' then begin
    lQry := TJnccQuery.Create(nil);
    try
      dmDatabase.SetDatabaseLocal([lQry]);
      with lQry do begin
        SQL.Text := 'SELECT * FROM Taxon_Determination WHERE Taxon_List_Item_Key IN (' + lsKeys + ')';
        Open;
        Result := not Eof;
        Close;
      end;
    finally
      lQry.Free;
    end;
  end;
end;  // ReferencedInTable

//------------------------------------------------------------------------------
procedure TdmTaxonDictEditor.DelTaxonUserNames(const AKey: TKeyString);
begin
  dmGeneralData.ExecuteSQL(
      'DELETE FROM Index_Taxon_Name ' +
      'WHERE Index_Taxon_Name.Taxon_List_Item_Key = ''' + AKey + ''' ' +
      'AND Index_Taxon_Name.Actual_Name IN (' +
          'SELECT TUN.Item_Name ' +
          'FROM Taxon_User_Name TUN ' +
          'WHERE TUN.Taxon_List_Item_Key = ''' + AKey + ''')',
      ResStr_DelFail + ' - INDEX_TAXON_NAME table');

  dmGeneralData.ExecuteSQL(
      'DELETE FROM Taxon_User_Name ' +
      'WHERE Taxon_List_Item_Key = ''' + AKey + '''',
      ResStr_DelFail + ' - TAXON_USER_NAME table');
end;

//------------------------------------------------------------------------------
procedure TdmTaxonDictEditor.DelStatuses(const AKey: TKeyString);
begin
  dmGeneralData.ExecuteSQL(
      'DELETE FROM Taxon_Designation WHERE Taxon_List_Item_Key IN (' + 
          'SELECT Taxon_List_Item_Key FROM Taxon_Version TV, Taxon_List_Item TLI ' +
          'WHERE TV.Taxon_Key = ''' + AKey + ''' ' +
          'AND TLI.Taxon_Version_Key = TV.Taxon_Version_Key)',
      ResStr_DelFail + ' - TAXON_DESIGNATION table');
end;  // DelStatuses

//------------------------------------------------------------------------------
procedure TdmTaxonDictEditor.DelFacts(const AKey: TKeyString);
begin
  dmGeneralData.ExecuteSQL(
      'DELETE FROM Taxon_Fact WHERE Taxon_Version_Key IN (' +
          'SELECT Taxon_Version_Key FROM Taxon_Version ' +
          'WHERE Taxon_Key = ''' + AKey + ''')',
      ResStr_DelFail + ' - TAXON_FACT table');
end;  // DelFacts


//==============================================================================
{ Clear out related items in the group index when deleting a taxon }
procedure TdmTaxonDictEditor.CleanGroupIndex(const AKey: TKeyString);
begin
  dmGeneralData.ExecuteSQL(Format(SQL_CLEAN_GROUP_INDEX, [AKey, AKey]),
                   ResStr_DelFail + ' - INDEX_TAXON_GROUP table');
end;

//------------------------------------------------------------------------------
procedure TdmTaxonDictEditor.UpdateSortCode(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString; const AddItem: Boolean);
begin
  with qrySortCode do begin
    SQL.Clear;
    if AddItem then
      SQL.Add('UPDATE Taxon_List_Item SET Sort_Code = Sort_Code + 1')
    else
      SQL.Add('UPDATE Taxon_List_Item SET Sort_Code = Sort_Code - 1');
    SQL.Add('WHERE Sort_Code > ' + IntToStr(ASortCode) + ' AND Taxon_List_Version_Key = ''' + AListVersion + '''');

    // check to see if we are at a child level or at the top level modify SQL accordingly
    if AParentKey <> '' then
      // child level
      SQL.Add('AND Parent = ''' + AParentKey + '''')
    else
      // top level
      SQL.Add('AND Parent IS NULL');
    ExecuteSQL;
  end;
end;  // UpdateSortCode

//------------------------------------------------------------------------------
// decreases Sort_Code by 1 for list items within the group where SORT_CODE > iiSortCode
procedure TdmTaxonDictEditor.UpdateSortCodeDel(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString);
begin
  inherited;
  UpdateSortCode(ASortCode, AParentKey, AListVersion, false);
end;  // UpdateSortCodeDel

//------------------------------------------------------------------------------
// increases Sort_Code by 1 for list items within the group where SORT_CODE >= iiSortCode
procedure TdmTaxonDictEditor.UpdateSortCodeAdd(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString);
begin
  inherited;
  UpdateSortCode(ASortCode, AParentKey, AListVersion, true);
end;  // UpdateSortCodeAdd

//==============================================================================
{ TTaxonNamesItem }
procedure TTaxonNamesItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do begin
    try
      FItemKey   := FieldByName('Taxon_User_Name_Key').AsString;
      FItemName  := FieldByName('Item_Name').AsString;
      FLanguage  := FieldByName('Language').AsString;
      FPreferred := FieldByName('Preferred').AsBoolean;
    except on E:Exception do
      raise ETaxonUserName.Create(ResStr_InitRecFail + ' - TAXON_USER_NAME', E);
    end;
  end;
end;  // TTaxonNamesItem.InitFromRecord

//------------------------------------------------------------------------------
procedure TTaxonNamesItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('Item_Name').AsString  := FItemName;
      FieldByName('Language').AsString   := FLanguage;
      FieldByName('Preferred').AsBoolean := FPreferred;

      FieldByName('Changed_By').AsString     := AppSettings.UserID;
      FieldByName('Changed_Date').AsDateTime := Now;
    except on E:Exception do
      raise ETaxonUserName.Create(ResStr_WriteRecFail + ' - TAXON_USER_NAME', E);
    end;
end;  // TTaxonNamesItem.WriteToRecord

//------------------------------------------------------------------------------
function TTaxonNamesItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: if FPreferred then //Use + to signal check box
         Result:= '+'
       else
         Result:= '';
    1: Result:= FItemName;
    2: Result:= FLanguage;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // TTaxonNamesItem.GetCell

//------------------------------------------------------------------------------
procedure TTaxonNamesItem.SetItemName(const Value: String);
begin
  FItemName := Value;
  SetModified;
end;  // TTaxonNamesItem.SetItemName

//------------------------------------------------------------------------------
procedure TTaxonNamesItem.SetLanguage(const Value: String);
begin
  FLanguage := Value;
  SetModified;
end;  // TTaxonNamesItem.SetLanguage

//------------------------------------------------------------------------------
procedure TTaxonNamesItem.SetPreferred(const Value: Boolean);
begin
  FPreferred := Value;
  SetModified;
end;  // TTaxonNamesItem.SetPreferred

//==============================================================================
{ TTaxonNamesList }
constructor TTaxonNamesList.Create(ADataset: TDataSet; AKeyFieldName: String;
  AStringGrid: TStringGrid);
begin
  inherited Create(ADataSet, AKeyFieldName, AStringGrid, TTaxonNamesItem);
end;  // TTaxonNamesList.Create

//------------------------------------------------------------------------------
function TTaxonNamesList.ItemsDisplayName: String;
begin
  Result := ResStr_TaxonName;
end;

//------------------------------------------------------------------------------
procedure TTaxonNamesList.DoAdditions;
var i           : Integer;
    lDataItem   : TTaxonNamesItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Taxon_User_Name WHERE Taxon_User_Name_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := ItemsToAdd.Items[i];
          if lDataItem <> nil then begin
            Append;
            FieldByName('Taxon_User_Name_Key').AsString :=
                dmGeneralData.GetNextKey('Taxon_User_Name', 'Taxon_User_Name_Key');
            FieldByName('Taxon_List_Item_Key').AsString := FTaxonListItemKey;
            FieldByName('Item_Name').AsString           := lDataItem.ItemName;
            FieldByName('Language').AsString            := lDataItem.Language;
            FieldByName('Preferred').AsBoolean          := lDataItem.Preferred;
            FieldByName('Entered_By').AsString          := AppSettings.UserID;
            Post;
          end;
        end;
        Close;
      except on E:Exception do
        raise ETaxonEditorError.Create(Format(ResStr_AddFail, ['TAXON_USER_NAME']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TTaxonNamesList.DoAdditions

//------------------------------------------------------------------------------
procedure TTaxonNamesList.ProcessUpdate;
var lqryUpdate: TJnccQuery;
begin
  { Create a temporary query for doing updates }
  lqryUpdate := TJnccQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lqryUpdate]);
  try
    DoAdditions;
    DoModifications;
    DeleteFromTable(lqryUpdate, 'Taxon_User_Name', 'Taxon_User_Name_Key');
  finally
    lqryUpdate.Free;
  end;
end;  // TTaxonNamesList.ProcessUpdate

//==============================================================================
end.
