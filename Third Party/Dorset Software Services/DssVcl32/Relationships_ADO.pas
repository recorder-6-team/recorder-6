//==================================================================================================
//  Unit:        Relationships_ADO
//
//  Implements:  TRelationship
//               TRelationshipList
//
//  Description: Unit containing classes for extracting relationships information
//               using ADO.
//
//               TRelationship
//               Class which defines a single relationship.
//
//               TRelationshipList
//               Class which provides a list of all relationships.
//
//  Author:      Eric Salmon
//  Created:     18 March 2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 9/01/03 18:17 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==================================================================================================

{$I DelphiVersions.Inc}

unit Relationships_ADO;

interface

uses
  SySutils, Classes, Controls, Forms, ADODB {$IFDEF DELPHI7UP}, Variants{$ENDIF};

const
  NO_RELATIONSHIP = -1;  // as standard return value of IndexOf

type
  PAccessField = ^TAccessField;
  TAccessField = record
    MasterName: String;
    DetailName: String;
  end;

  TRelationship = class
  private
    FFields     : TList;
    FName       : String;
    FAttributes : Integer;
    FMasterTable: String;
    FDetailTable: String;
  protected
    function GetAttributes: Integer;
    function GetForeignTable: String;
    function GetName: String;
    function GetTable: String;
    procedure SetAttributes(const Value: Integer);
    procedure SetForeignTable(const Value: String);
    procedure SetName(const Value: String);
    procedure SetTable(const Value: String);
    function GetFields(const iIndex: Integer): TAccessField;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField( const iFieldName, iForeignName: String);
    function GetMasterField( const iForeignName: String): String;
    property Name: String read GetName write SetName;
    property MasterTable: String read GetTable write SetTable;
    property DetailTable: String read GetForeignTable write SetForeignTable;
    property Fields[const iIndex: Integer]: TAccessField read GetFields;
    property Attributes: Integer read GetAttributes write SetAttributes;
  end;

  //------------------------------------------------------------------------------------------------
  TRelationshipArray = array of TRelationship;

  //------------------------------------------------------------------------------------------------
  TRelationshipList = class(TStringList)
  protected
    FDatabase : TADOConnection;
    FConnectionProvided: Boolean;
    procedure PopulateRelationships;
    function GetRelationship(const iIndex: Integer): TRelationship;
  public
    constructor Create(const iPath: String); overload;
    constructor Create(iDatabase: TADOConnection); overload;
    destructor Destroy; override;
    function GetForeignTableCount(const iTable: String): Integer; virtual;
    function FindDependencies(const iTable: String): TRelationshipArray; virtual;
    function FindRelationship(const iTable, iField: String): Integer; virtual;
    function FindRelationsBetweenTables(const iMaster, iDetail: String): TRelationshipArray;
    function FindDetails(const iTable: String): TRelationshipArray;
    property Relationship[const iIndex: Integer]: TRelationship read GetRelationship;
    property Database: TADOConnection read FDatabase;
  end;

//======================================================================================================================
implementation

//======================================================================================================================
{ TRelationship }
//==================================================================================================
//==================================================================================================
constructor TRelationship.Create;
begin
  inherited Create;
  FFields := TList.Create;
end;  // TRelationship.Create

//==================================================================================================
destructor TRelationship.Destroy;
var i: Integer;
begin
  for i := 0 to FFields.Count-1 do
    Dispose(FFields[i]);
  FFields.Free;
  inherited Destroy;
end;  // TRelationship.Destroy

//==================================================================================================
procedure TRelationship.AddField(const iFieldName, iForeignName: String);
var lField: PAccessField;
begin
  New(lField);
  lField^.MasterName := iFieldName;
  lField^.DetailName := iForeignName;
  FFields.Add(lField);
end;  // TRelationship.AddField

//==================================================================================================
function TRelationship.GetAttributes: integer;
begin
  Result := FAttributes;
end;  // TRelationship.GetAttributes

//==================================================================================================
function TRelationship.GetFields(const iIndex : integer): TAccessField;
begin
  Result := TAccessField(FFields.Items[iIndex]^);
end;  // TRelationship.GetFields

//==================================================================================================
function TRelationship.GetForeignTable: String;
begin
  Result := FDetailTable;
end;  // TRelationship.GetForeignTable

//==================================================================================================
function TRelationship.GetMasterField(const iForeignName: String): String;
var i     : Integer;
    lField: PAccessField;
begin
  Result := '';
  for i := 0 to FFields.Count-1 do
  begin
    lField := FFields.Items[i];
    if lField.DetailName = iForeignName then
      Result := lField.MasterName;
  end;
end;  // TRelationship.GetMasterField

//==================================================================================================
function TRelationship.GetName: String;
begin
  Result := FName;
end;  // TRelationship.GetName

//==================================================================================================
function TRelationship.GetTable: String;
begin
  Result := FMasterTable;
end;  // TRelationship.GetTable

//==================================================================================================
procedure TRelationship.SetAttributes(const Value: Integer);
begin
  FAttributes := Value;
end;  // TRelationship.SetAttributes

//==================================================================================================
procedure TRelationship.SetForeignTable(const Value: String);
begin
  FDetailTable := Value;
end;  // TRelationship.SetForeignTable

//==================================================================================================
procedure TRelationship.SetName(const Value: String);
begin
  FName := Value;
end;  // TRelationship.SetName

//==================================================================================================
procedure TRelationship.SetTable(const Value: String);
begin
  FMasterTable := Value;
end;  // TRelationship.SetTable

//==================================================================================================
//==================================================================================================
{ TRelationshipList }
//==================================================================================================
{ Constructor.  First sets up ADO, then populates the list of relationships }
constructor TRelationshipList.Create(const iPath: String);
var lCursor: TCursor; // hold original
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    inherited Create;
    Sorted := True;
    Duplicates := dupAccept;
    // Setup ADO connection
    FDatabase:= TADOConnection.Create(nil);
    FDatabase.ConnectionString := iPath;
    FDatabase.LoginPrompt := false;
    FDatabase.Open;
    FConnectionProvided := false;
    PopulateRelationships;
  finally
    Screen.Cursor := lCursor;
  end;
end;  // TRelationshipList.Create

//--------------------------------------------------------------------------------------------------
// Overloaded version of constructor where the Connection object is provided
constructor TRelationshipList.Create(iDatabase: TADOConnection);
var lCursor : TCursor;
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    inherited Create;
    Sorted := True;
    Duplicates := dupAccept;
    // Store ADO settings
    FDatabase := iDatabase;
    FConnectionProvided := true;
    PopulateRelationships;
  finally
    Screen.Cursor := lCursor;
  end;
end;  // TRelationshipList.Create

//==================================================================================================
{ Destructor - Cleanup }
destructor TRelationshipList.Destroy;
var i: Integer;
begin
  if not FConnectionProvided then begin
    FDatabase.Close;
    FDatabase.Free;
  end;
  for i := 0 to Count-1 do
  begin
    Objects[i].Free;
    // safety - some objects are duplicated on the list
    Objects[i] := nil;
  end;
  inherited Destroy;
end;  // TRelationshipList.Destroy

//==================================================================================================
{ Copy the ADO database relationships into a local list }
procedure TRelationshipList.PopulateRelationships;
var lRelation  : TRelationship;
    ladoDataset: TADODataset;
begin
  ladoDataset := TADODataset.Create(nil);
  try
    // Read Foreign Keys Schema, i.e. get all relationships
    FDatabase.OpenSchema(siForeignKeys, EmptyParam, EmptyParam, ladoDataset);
    with ladoDataset do
      while not Eof do begin
        lRelation             := TRelationship.Create;
        lRelation.Name        := FieldByName('FK_NAME').AsString;
        lRelation.MasterTable := FieldByName('PK_TABLE_NAME').AsString;
        lRelation.DetailTable := FieldByName('FK_TABLE_NAME').AsString;
        lRelation.AddField(FieldByName('PK_COLUMN_NAME').AsString,
                           FieldByName('FK_COLUMN_NAME').AsString);
        // Add to a string list providing a lookup string to help searching for
        // relations from the detail side
        AddObject(FieldByName('FK_TABLE_NAME').AsString + ' ' +
                  FieldByName('FK_COLUMN_NAME').AsString,
                  lRelation);
        Next;
      end;
  finally
    ladoDataset.Free;
  end;
end;  // TRelationshipList.PopulateRelationships

//==================================================================================================
{ Locate a relationship by foreign table name and keyfield }
function TRelationshipList.FindRelationship(const iTable, iField: String): Integer;
begin
  Result := IndexOf(iTable + ' ' + iField);
end;  // TRelationshipList.FindRelationship

//==================================================================================================
{ Accessor method for the relationship property }
function TRelationshipList.GetRelationship(const iIndex: Integer): TRelationship;
begin
  Result := TRelationship(Objects[iIndex]);
end;  // TRelationshipList.GetRelationship

//==================================================================================================
{ Get a count of the tables which need to be fixed for a merge }
function TRelationshipList.GetForeignTableCount(const iTable: String): Integer;
var lCount, i: Integer;
begin
  lCount := 0; // Starting point - we are going to count the tables that need fixup
  for i := 0 to Count - 1 do
    with Relationship[i] do
      if CompareText(MasterTable, iTable) = 0 then Inc(lCount);
  Result := lCount;
end;  // TRelationshipList.GetForeignTableCount

//==================================================================================================
{ Searches the relationship list for a relationship between the two tables.
     If none are found, then an exception is raised }
function TRelationshipList.FindRelationsBetweenTables(const iMaster, iDetail: String): TRelationshipArray;
var i: Integer;
    lstUCMaster, lstUCDetail: String;
begin
  { Prepare the result array }
  SetLength(Result, 0);
  lstUCMaster := UpperCase(iMaster);
  lstUCDetail := UpperCase(iDetail);
  { Loop through and record each one we want }
  for i := 0 to Count-1 do
  begin
    { Case insensitive test }
    if (Uppercase(TRelationship(Objects[i]).MasterTable) = lstUCMaster) and
       (Uppercase(TRelationship(Objects[i]).DetailTable) = lstUCDetail) then
    begin
      SetLength(Result, High(Result) + 2);  // + 2 because array is zero based
      Result[High(Result)] := TRelationship(Objects[i]);
    end;
  end; // for
end;  // TRelationshipList.FindRelationshipBetweenTables

//--------------------------------------------------------------------------------------------------
// Find all relationships in which the specified table is the 'detail'
//--------------------------------------------------------------------------------------------------
function TRelationshipList.FindDependencies(const iTable: String): TRelationshipArray;
var i, lNewIndex: Integer;
    lRelation   : TRelationship;
begin
  SetLength(Result, 0);

  for i := 0 to Count - 1 do begin
    lRelation := TRelationship(Objects[i]);
    if CompareText(iTable, lRelation.DetailTable) = 0 then begin
      lNewIndex := High(Result) + 1;
      SetLength(Result, lNewIndex + 1);  // + 1 because array is zero based
      Result[lNewIndex] := lRelation;
    end;  // if ComapreText(..) = 0
  end;  // for i := 0 to Count - 1
end;  // TRelationshipList.FindDependencies

//==================================================================================================
{ Finds all relationships where the table specified is the master }
function TRelationshipList.FindDetails(const iTable: String): TRelationshipArray;
var i, lNewIndex: Integer;
    lRelation   :  TRelationship;
begin
  SetLength(Result, 0);
  for i := 0 to Count - 1 do begin
    lRelation := TRelationship(Objects[i]);
    if CompareText(iTable, lRelation.MasterTable) = 0 then begin
      lNewIndex := High(Result) + 1;
      SetLength(Result, lNewIndex + 1);  // + 1 because array is zero based
      Result[lNewIndex] := lRelation;
    end;  // if ComapreText(..) = 0
  end;  // for i := 0 to Count - 1
end;  // TRelationshipList.FindDetails

//==================================================================================================
end.
