//==============================================================================
//  Unit:        TablePriorityList_ADO
//
//  Implements:  TTablePriorityList
//
//  Description: List of all the non-system tables in a database accessed via ADO,
//               ordered so that each table always comes after those on which
//               it is dependent.
//
//               Assumes that there are no cycles in the ERD -- these cause
//               infinite looping and stack overflow.
//
//  Author:      AJWK
//  Created:     20/05/1999
//
//  Changes:     Eric Salmon - 03/04/2002
//               Original code from TablePriorityList adapted to work with ADO.
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 9/02/04 12:08 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit TablePriorityList_ADO;

interface

uses
  Classes, ADODB, Relationships_ADO;

type
  TTablePriorityList = class
  private
    FTables: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): String;
    procedure GetTables(db: TADOConnection);
    procedure LoseTables;
    procedure BuildDependencyTrees(Relations: TRelationshipList);
    procedure DetermineLevels;
    procedure TraverseDependencyTree(treeUnk: IUnknown; level: Integer);
    procedure LoseDependencyTrees;
  public
    constructor Create(Relations: TRelationshipList);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: String read GetItem; default;
  end;  // class TTablePriorityList

//==================================================================================================
implementation

uses
  SysUtils;

type
  IDependencyTree = interface;

  TPrioritisedTable = record
    Name:            String;
    RequiredBy:      Boolean;
    DependencyTree:  IDependencyTree;
    Level:           Integer;
    TableList:       TList;
  end;  // record TPrioritisedTable

  PPrioritisedTable = ^TPrioritisedTable;

  IDependencyTree = interface
    ['{84F95F71-0EA8-11D3-B5BD-0060085C1609}']
    function GetChildCount: Integer;
    function GetChild(Index: Integer): IDependencyTree;
    function GetRootTable: PPrioritisedTable;
    property ChildCount: Integer read GetChildCount;
    property Child[Index: Integer]: IDependencyTree read GetChild;
    property RootTable: PPrioritisedTable read GetRootTable;
  end;  // interface IDependencyTree

  TDependencyTree = class(TInterfacedObject, IDependencyTree)
  private
    FRootTable: PPrioritisedTable;
    FChildren : IInterfaceList;
    function GetPrioritisedTable(tableList: TList; const tableName: String): PPrioritisedTable;
    procedure BuildTree(relList: TRelationshipList; const ADependencyStack: string);
  public
    // IDependencyTree
    function GetChildCount: Integer;
    function GetChild(Index: Integer): IDependencyTree;
    function GetRootTable: PPrioritisedTable;
    // other methods
    constructor Create(rootTable: PPrioritisedTable; relList: TRelationshipList;
        const ADependencyStack: string);
  end;  // class TDependencyTree

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function CompareTables(Item1, Item2: Pointer): Integer;
var
  lptTable1, lptTable2:  PPrioritisedTable;
begin
  lptTable1 := Item1;
  lptTable2 := Item2;
  // note that items at the top level (0) must come *last*
  // -- the sort order is the opposite of the Level order
  Result := -(lptTable1^.Level - lptTable2^.Level);
end;  // CompareTables

//==================================================================================================
{ TTablePriorityList }
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
constructor TTablePriorityList.Create(Relations: TRelationshipList);
begin
  inherited Create;
  FTables := TList.Create;
  GetTables(Relations.Database);
  BuildDependencyTrees(Relations);
  DetermineLevels;
  LoseDependencyTrees;
  FTables.Sort(CompareTables);
end;  // TTablePriorityList.Create

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
destructor TTablePriorityList.Destroy;
begin
  if Assigned(FTables) then LoseTables;
  FTables.Free;
  inherited Destroy;
end;  // TTablePriorityList.Create

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TTablePriorityList.GetCount: Integer;
begin
  Result := FTables.Count;
end;  // TTablePriorityList.GetCount

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TTablePriorityList.GetItem(Index: Integer): String;
var
  lptEntry: PPrioritisedTable;
begin
  lptEntry := FTables[Index];
  Result   := lptEntry.Name;
end;  // TTablePriorityList.GetItem

// -----------------------------------------------------------------------------
// TTablePriorityList.GetTables
//
// Loads table names from the database and initialises the corresponding
// TPrioritisedTable records
// -----------------------------------------------------------------------------
procedure TTablePriorityList.GetTables(db: TADOConnection);
var
  lptEntry : PPrioritisedTable;
  liIdx    : Integer;
  lslTables: TStringList;
begin
  lslTables := TStringList.Create;
  try
    db.GetTableNames(lslTables, false);  // False for no system tables

    for liIdx := 0 to lslTables.Count - 1 do begin
      New(lptEntry);
      try
        FillChar(lptEntry^, SizeOf(TPrioritisedTable), #0);
        lptEntry^.Name := lslTables[liIdx];
        lptEntry^.TableList := FTables;
        FTables.Add(lptentry);
      except
        Dispose(lptEntry);
        raise;
      end;  // try .. except
    end;
  finally
    lslTables.Free;
  end;
end;  // TTablePriorityList.GetTables

// -----------------------------------------------------------------------------
// TTablePriorityList.LoseTables
//
// Disposes of the TPrioritisedTable records.
// -----------------------------------------------------------------------------
procedure TTablePriorityList.LoseTables;
var
  lptEntry:  PPrioritisedTable;
begin
  while FTables.Count > 0 do begin
    lptEntry := FTables[0];
    lptEntry^.DependencyTree := nil;
    FTables.Delete(0);
    Dispose(lptEntry);
  end;
end;  // TTablePriorityList.LoseTables

// -----------------------------------------------------------------------------
// TTablePriorityList.BuildDependencyTrees
//
// Works out the dependency trees for each of the database tables.
// -----------------------------------------------------------------------------
procedure TTablePriorityList.BuildDependencyTrees(Relations: TRelationshipList);
var
  i:      Integer;
  lptEntry:  PPrioritisedTable;
begin
  for i := 0 to FTables.Count - 1 do begin
    lptEntry := FTables[i];
    if not Assigned(lptEntry^.DependencyTree) then
      lptEntry^.DependencyTree := TDependencyTree.Create(lptEntry, Relations, ';');
  end;  // for i := 0 to ..
end;  // TTablePriorityList.BuildDependencyTrees

// -----------------------------------------------------------------------------
// TTablePriorityList.DetermineLevels
//
// For each table that does not appear in ay other table's dependency list,
// traverse the dependency tree.  Note the maximum depth (in any tree) at
// which each table occurs.
// -----------------------------------------------------------------------------
procedure TTablePriorityList.DetermineLevels;
var
  lptEntry: PPrioritisedTable;
  liIdx   : Integer;
begin
  for liIdx := 0 to FTables.Count - 1 do begin
    lptEntry := FTables[liIdx];
    if not lptEntry^.RequiredBy then begin
      // this table is not depended on by anything else
      // -- i.e. it is at level 1, and will not appear in
      // the tree for any other table
      TraverseDependencyTree(lptEntry^.DependencyTree, 1);
    end;  // if not entry^.RequiredBy
  end;  // for tableIndex := 0 to ..
end;  // TTablePriorityList.DetermineLevels

// -----------------------------------------------------------------------------
// TTablePriorityList.TraverseDependencyTree
//
// Traverse a given dependency tree (assumed to start at a specified level),
// recording the maximum depth at which each table occurs (unless a greater
// depth has already been recorded for a different tree).
// -----------------------------------------------------------------------------
procedure TTablePriorityList.TraverseDependencyTree(treeUnk: IUnknown;
    level: Integer);
var
  i:      Integer;
  table:  PPrioritisedTable;
  tree:   IDependencyTree;

begin
  tree := treeUnk as IDependencyTree;

  table := tree.RootTable;
  if table.Level < level then table.Level := level;

  for i := 0 to tree.ChildCount - 1 do begin
    TraverseDependencyTree(tree.Child[i], level + 1);
  end;  // for i := 0 to ..

end;  // TTablePriorityList.TraverseDependencyTree


// -----------------------------------------------------------------------------
// TTablePriorityList.LoseDependencyTrees
//
// Dispose of the dependency trees (by clearing the interface pointers).
// -----------------------------------------------------------------------------
procedure TTablePriorityList.LoseDependencyTrees;
var
  i:      Integer;
  entry:  PPrioritisedTable;

begin
  for i := 0 to FTables.Count - 1 do begin
    entry := FTables[i];
    entry^.DependencyTree := nil;
  end;  // for i := 0 to ..
end;  // TTablePriorityList.LoseDependencyTrees




{ TDependencyTree -- tree of tables on which the root table is dependent }

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
constructor TDependencyTree.Create(
    rootTable: PPrioritisedTable;
    relList: TRelationshipList;
    const ADependencyStack: string
  );
begin
  inherited Create;
  FRootTable := rootTable;
  FChildren := TInterfaceList.Create;
  BuildTree(relList, ADependencyStack + rootTable.Name + ';');
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TDependencyTree.GetChild(Index: Integer): IDependencyTree;
begin
  Result := FChildren.Items[Index] as IDependencyTree;
end;  // TDependencyTree.GetChild


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TDependencyTree.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;  // TDependencyTree.GetChildCount


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TDependencyTree.GetRootTable: PPrioritisedTable;
begin
  Result := FRootTable;
end;  // TDependencyTree.GetRootTable


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
procedure TDependencyTree.BuildTree(relList: TRelationshipList;
    const ADependencyStack: string);
var
  i:              Integer;
  immediateDeps:  TRelationshipArray;
  requiredName:   String;
  requiredTable:  PPrioritisedTable;

begin
  immediateDeps := relList.FindDependencies(FRootTable^.Name);
  for i := 0 to High(immediateDeps) do begin

    requiredName := immediateDeps[i].MasterTable;
    if CompareText(FRootTable^.Name, requiredName) <> 0 then begin

      requiredTable := GetPrioritisedTable(
          FRootTable^.TableList, requiredName
        );
      requiredTable^.RequiredBy := True;
      // Ensure table not in
      if Pos(';' + requiredTable.Name + ';', ADependencyStack)<1 then begin
        if not Assigned(requiredTable.DependencyTree) then begin
          requiredTable.DependencyTree :=
              TDependencyTree.Create(requiredTable, relList, ADependencyStack);
        end;
        FChildren.Add(requiredTable.DependencyTree);
      end;
    end;  // if CompareText(..) <> 0

  end;
end;  // TDependencyTree.BuildTree


// -----------------------------------------------------------------------------
// TDependencyTree.GetPrioritisedTable
//
// Returns a pointer to the TPrioritisedTable structure in the given TList for
// the named table.
// -----------------------------------------------------------------------------
function TDependencyTree.GetPrioritisedTable(
    tableList: TList; const tableName: String
  ): PPrioritisedTable;
var
  i:       Integer;
  pTable:  PPrioritisedTable;

begin
  Result := nil;  // default return value

  i := 0;
  while (i < tableList.Count) and (not Assigned(Result)) do begin
    pTable := tableList[i];
    if CompareText(pTable^.Name, tableName) = 0 then Result := pTable;
    Inc(i);
  end;

end;  // TDependencyTree.GetPrioritisedTable


end.