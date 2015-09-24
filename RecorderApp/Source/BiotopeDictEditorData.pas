//==============================================================================
//  Unit:        BiotopeDcitEditorData
//
//  Implements:  TdmBiotopeDictEditor
//               TBiotopeFactsItem
//               TBiotopeFactsList
//
//  Description: Contains biotope dictionary editor specific functions to
//               access, format, and save data.
//               Helper classes: Biotope facts Item and List. Used to manage
//               biotope facts before committing any changes to the database.
//
//  Author:      Eric Salmon
//  Created:     10 Dec 2001
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 27/12/07 14:28 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BiotopeDictEditorData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseBiotopeDictDataUnit, Grids, JNCCDatasets, DataClasses, DictionaryDataGeneric,
  Constants, ADODB, DatabaseAccessADO, ExceptionForm, HTTPApp, HTTPProd;

type
  EBiotopeEditorError = class(TExceptionPath);

  TdmBiotopeDictEditor = class(TBaseBiotopeDictData)
    dsGeneral: TDataSource;
    qrySortcode: TJNCCQuery;
  private
    procedure UpdateSortCode(const ASortCode: Integer; const AParentKey,
      AListVersion: TKeyString; const AddItem: Boolean);
  public
    procedure DelFacts(const AKey: TKeyString);
    function ReferencedInTable(const ABLIKey: TKeyString): Boolean;
    procedure UpdateSortCodeAdd(const ASortCode: Integer; const AParentKey,
      AListVersion: TKeyString);
    procedure UpdateSortCodeDel(const ASortCode: Integer; const AParentKey,
      AListVersion: TKeyString);
  end;

  //============================================================================
  TBiotopeFactsItem = class(TBaseFactsItem);

  //============================================================================
  TBiotopeFactsList = class(TGridDataList)
  private
    FBiotopeKey : TKeyString;
    FBiotopeListItemKey : TKeyString;
    // Accessor methods
    procedure SetBiotopeListItemKey(const Value: TKeyString);
    procedure SetBiotopeKey(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
  public
    constructor Create(iDataset: TDataSet; iKeyFieldName: String;
      iStringGrid: TStringGrid);
    procedure ProcessUpdate; override;
    property BiotopeKey : TKeyString read FBiotopeKey write SetBiotopeKey;
    property BiotopeListItemKey : TKeyString read FBiotopeListItemKey write SetBiotopeListItemKey;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData;

resourcestring
  ResStr_BiotopeFact = 'Biotope Fact';

//==============================================================================
{ TBiotopeFactsList }
//------------------------------------------------------------------------------
constructor TBiotopeFactsList.Create(iDataset: TDataSet;
  iKeyFieldName: String; iStringGrid: TStringGrid);
begin
  inherited Create(iDataSet, iKeyFieldName, iStringGrid, TBiotopeFactsItem);
end;  // TBiotopeFactsList.Create

//------------------------------------------------------------------------------
function TBiotopeFactsList.ItemsDisplayName: String;
begin
  Result := ResStr_BiotopeFact;
end;

//------------------------------------------------------------------------------
procedure TBiotopeFactsList.DoAdditions;
var i           : Integer;
    lDataItem   : TBiotopeFactsItem;
    lqryAddition: TJnccQuery;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Biotope_Fact WHERE  Biotope_Fact_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lDataItem := ItemsToAdd.Items[i];
          if (lDataItem <> nil) then
          begin
            Append;
            FieldByName('Biotope_Fact_Key').AsString  :=
                dmGeneralData.GetNextKey('Biotope_Fact','Biotope_Fact_Key');
            FieldByName('Fact_Vague_Date_Start').Text := lDataItem.Date;
            FieldByName('Biotope_Key').AsString       := FBiotopeKey;
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
        raise EBiotopeEditorError.Create(Format(ResStr_AddFail, ['BIOTOPE_FACT']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // TBiotopeFactsList.DoAdditions

//------------------------------------------------------------------------------
procedure TBiotopeFactsList.ProcessUpdate;
var
  lupdateQuery : TJNCCQuery;
begin
  { Create a temporary query for doing updates }
  lUpdateQuery := TJNCCQuery.Create(nil);
  dmDatabase.SetDatabaseLocal([lUpdateQuery]);
  try
    DoAdditions;
    DoModifications;
    DeleteFromTable( lUpdateQuery, 'BIOTOPE_FACT', 'BIOTOPE_FACT_KEY' );
  finally
    lUpdateQuery.Free;
  end;
end;  // TBiotopeFactsList.ProcessUpdate

//------------------------------------------------------------------------------
procedure TBiotopeFactsList.SetBiotopeKey(const Value: TKeyString);
begin
  FBiotopeKey := Value;
end;  // TBiotopeFactsList.SetBiotopeKey

//------------------------------------------------------------------------------
procedure TBiotopeFactsList.SetBiotopeListItemKey(const Value: TKeyString);
begin
  FBiotopeListItemKey := Value;
end;  // TBiotopeFactsList.SetBiotopeListItemKey

//==============================================================================
//==============================================================================
{ TdmBiotopeDictEditor }
//------------------------------------------------------------------------------
function TdmBiotopeDictEditor.ReferencedInTable(const ABLIKey: TKeyString): Boolean;
var lqryReferences: TJNCCQuery;
begin
  lqryReferences := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryReferences]);
    with lqryReferences do begin
      SQL.Text := 'SELECT * FROM Biotope_Determination WHERE Biotope_List_Item_Key=''' + ABLIKey + '''';
      Open;
      Result := not Eof;
      Close;
    end;
  finally
    lqryReferences.Free;
  end;
end;  // ReferencedInTable

//------------------------------------------------------------------------------
procedure TdmBiotopeDictEditor.DelFacts(const AKey: TKeyString);
begin
  dmGeneralData.ExecuteSQL('DELETE FROM Biotope_Fact WHERE Biotope_Key=''' + AKey + '''',
                           ResStr_DelFail + ' - BIOTOPE_FACT table');
end;  // DelFacts

//------------------------------------------------------------------------------
procedure TdmBiotopeDictEditor.UpdateSortCode(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString; const AddItem: Boolean);
begin
  with qrySortCode do begin
    SQL.Clear;
    if AddItem then
      SQL.Add('UPDATE BIOTOPE_LIST_ITEM SET SORT_CODE = SORT_CODE + 1')
    else
      SQL.Add('UPDATE BIOTOPE_LIST_ITEM SET SORT_CODE = SORT_CODE - 1');
    SQL.Add('WHERE SORT_CODE > ' + IntToStr(ASortCode) + ' AND BT_CL_VERSION_KEY = ''' + AListVersion + '''');

    // check to see if we are at a child level or at the top level modify SQL accordingly
    if (AParentKey <> '') then
      // child level
      SQL.Add('AND Parent = ''' + AParentKey + '''')
    else
      // top level
      SQL.Add('AND PARENT IS NULL');
    ExecuteSQL;
  end;
end;  // UpdateSortCode

//------------------------------------------------------------------------------
// decreases Sort_Code by 1 for list items within the group where SORT_CODE > ASortCode
procedure TdmBiotopeDictEditor.UpdateSortCodeDel(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString);
begin
  inherited;
  UpdateSortCode(ASortCode, AParentKey, AListVersion, false);
end;  // TdmBiotopeDictDetails.UpdateSortCodeDel

//------------------------------------------------------------------------------
// increases Sort_Code by 1 for list items within the group where SORT_CODE >= ASortCode
procedure TdmBiotopeDictEditor.UpdateSortCodeAdd(const ASortCode: LongInt;
  const AParentKey, AListVersion: TKeyString);
begin
  inherited;
  UpdateSortCode(ASortCode, AParentKey, AListVersion, true);
end;  // TdmBiotopeDictDetails.UpdateSortCodeAdd

//==============================================================================
end.
