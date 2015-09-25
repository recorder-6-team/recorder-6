//==============================================================================
//  Unit:        TermListsData
//
//  Implements:  TdmTermLists
//
//  Description: Implements data access for the term lists details screen.
//
//  Author:      Paul Thomas
//  Created:     23 June 1999
//
//  Last Revision Details:
//    $Revision: 17 $
//    $Date: 29/05/08 16:34 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TermListsData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, CheckLst, DataClasses, ADODB, ExtCtrls, Variants;

type
  TdmTermLists = class(TBaseDataModule)
    qryTermLists: TJNCCQuery;
    dsTermList: TDataSource;
    qryLinkedList: TJNCCQuery;
    qryContexts: TJNCCQuery;
    tblMeasurementContexts: TJNCCTable;
    qryLinkedTables: TJNCCQuery;
    qryTermList: TJNCCQuery;
  private
    procedure InitContextsQuery(const istKey: String);
  public
    procedure InitLinkedListQuery(const istSourceTable, istParentTableKey, istKey: string);
    procedure InitLinkedTablesQuery(const AMasterTable: String);
    procedure CheckContexts(const istKey: String; ACheckListBox: TCheckListBox);
    procedure UpdateContexts(const istKey: String; ACheckListBox: TCheckListBox);
    procedure DeleteContexts(const istKey: String);
    procedure DeleteUnitValues(const key: String);
    procedure InsertUnitValue(const key, data: String);
    procedure CheckDetType(const istKey: String; ARadioGroup: TRadioGroup);
    procedure UpdateDetType(const istKey: String; AVerified: Integer);
  end;

//==============================================================================
implementation

uses
  DatabaseAccessADO, BaseADODataModule, Constants, ApplicationSettings;

{$R *.DFM}

//==============================================================================
{ TdmTermLists }

procedure TdmTermLists.InitLinkedListQuery(const istSourceTable, istParentTableKey, istKey: string);
begin
  with qryLinkedList do
  begin
    SQL.CLear;
    SQL.Add('SELECT * FROM ' + istSourceTable + ' WHERE ' + istParentTableKey + ' = ''' + istKey + '''');
  end;
end;  // InitLinkedListQuery

//==============================================================================
procedure TdmTermLists.InitLinkedTablesQuery(const AMasterTable: String);
begin
  with qryLinkedTables do begin
    SQL.Clear;
    SQL.Text := 'SELECT * FROM TERM_LIST WHERE LINKED_TABLE = ''' + AMasterTable + ''' ORDER BY Description';
  end;
end;  // InitLinkedTablesQuery

//==============================================================================
procedure TdmTermLists.InitContextsQuery(const istKey: String);
begin
  with qryContexts do begin
    SQL.Clear;
    SQL.Text := 'SELECT * FROM Measurement_Type_Context ' +
                'WHERE Measurement_Type_Key = ''' + istKey + '''';
  end;
end;  // InitContextsQuery

//==============================================================================
procedure TdmTermLists.CheckContexts(const istKey: String; ACheckListBox: TCheckListBox);
var liIdx: Integer;
    lstKey: String;
begin
  with ACheckListBox do begin
    // Reset Checked flags
    for liIdx := 0 to Items.Count-1 do Checked[liIdx] := false;

    if istKey<>'' then
      with qryContexts do begin
        // Check found screens
        InitContextsQuery(istKey);
        Open;
        First;
        while not Eof do begin
          lstKey := FieldByName('MEASUREMENT_CONTEXT_KEY').AsString;
          // If screen selected for measurement type, tick it.
          for liIdx := 0 to Items.Count-1 do
            if TKey(Items.Objects[liIdx]).Key = lstKey then
              Checked[liIdx] := true;
          Next;
        end;
        Close;
      end;
  end;
end;  // CheckContexts

//==============================================================================
procedure TdmTermLists.DeleteContexts(const istKey: String);
begin
  with qryContexts do begin
    SQL.Text := 'DELETE ' +
                'FROM Measurement_Type_Context ' +
                'WHERE Measurement_Type_Key = ''' + istKey + '''';
    ExecSQL;
  end;
end;  // DeleteContexts

//==============================================================================
procedure TdmTermLists.DeleteUnitValues(const key: String);
begin
  dmDatabase.RunStoredProc('usp_MeasurementUnitValue_Delete_ForMeasurementUnit', ['@Key', key]);
end;  // DeleteUnitValues

//==============================================================================
procedure TdmTermLists.InsertUnitValue(const key, data: String);
begin
  dmDatabase.RunStoredProc('usp_MeasurementUnitValue_Insert', [
    '@Key', key, '@Data', data, '@EnteredBy', AppSettings.UserId]);
end;  // InsertUnitValue

//==============================================================================
procedure TdmTermLists.UpdateContexts(const istKey: String; ACheckListBox: TCheckListBox);
var liIdx: Integer;
begin
  DeleteContexts(istKey);
  // Now, add the selected ones
  with ACheckListBox, tblMeasurementContexts do begin
    TableName := TN_MEASUREMENT_TYPE_CONTEXT;
    Open;
    for liIdx := 0 to Items.Count-1 do
      if Checked[liIdx] then begin
        Append;
        FieldByName('MEASUREMENT_TYPE_KEY').AsString    := istKey;
        FieldByName('MEASUREMENT_CONTEXT_KEY').AsString := TKey(Items.Objects[liIdx]).Key;
        Post;
      end;
    Close;
  end;
end;  // UpdateContexts

//==============================================================================
procedure TdmTermLists.CheckDetType(const istKey: string; ARadioGroup: TRadioGroup);
var
  lIdx: Variant;
begin
  lIdx := dmDatabase.GetStoredProcOutputParam('usp_DeterminationType_Verified_Get',
      ['@Key', istKey], '@Output');
  if VarIsNull(lIdx) then
    lIdx := 0;
  ARadioGroup.Buttons[lIdx].Checked := True;
end;

//==============================================================================
procedure TdmTermLists.UpdateDetType(const istKey: String; AVerified: Integer);
begin
  dmDatabase.RunStoredProc('usp_DeterminationType_Update',
      ['@Key', istKey, '@Verified', AVerified]);
end;

//==============================================================================
end.

