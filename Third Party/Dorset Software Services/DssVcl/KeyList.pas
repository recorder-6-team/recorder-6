{ ------------------------------------------------------------------------------
// Unit:          KeyList
//
//                Copyright © Dorset Software Services Ltd, 1998
//
// Description:   List of key/value pairs with binary search on the key.
//
// Author:        AJWK
//
// Created:       3/12/1998
//
// Changes:
//
// To Do:         Write InsertIndirectedPair.
// ----------------------------------------------------------------------------}
unit KeyList;

interface

uses
  Classes;

type
  EKeyedListError = class(EListError);

  TCustomKeyedList = class(TObject)
  private
    { instance data }
    FCursor:  Integer;
    FList:    TList;
    FSorted:  Boolean;

    function GetIndirectedKey(i: Integer): Pointer;
    function GetIndirectedValue(i: Integer): Pointer;
    function GetIndirectedFoundKey: Pointer;
    function GetIndirectedFoundValue: Pointer;
    function GetSorted: Boolean;
    function GetCount: Integer;

  protected
    procedure AppendIndirectedPair(pKey, pValue: Pointer);
    { procedure InsertIndirectedPair(pKey, pValue: Pointer); }
    procedure Sort;
    procedure SetUnsorted;
    function FindIndirectedKey(pKey: Pointer): Boolean; virtual;
    procedure RemoveByIndirectedKey(pKey: Pointer);

    function CompareIndirectedKeys(pKey1, pKey2: Pointer): Integer;
        virtual; abstract;

    procedure FreeIndirectedPair(pKey, pValue: Pointer);
        virtual; abstract;

    property IndirectedKey[i: Integer]: Pointer read GetIndirectedKey;
    property IndirectedValue[i: Integer]: Pointer read GetIndirectedValue;

    property Cursor: Integer read FCursor;
    property IndirectedFoundKey: Pointer read GetIndirectedFoundKey;
    property IndirectedFoundValue: Pointer read GetIndirectedFoundValue;

    property Sorted: Boolean read GetSorted;

  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;

    procedure Clear;

  end;  { class TCustomKeyedList }




implementation

uses
  SysUtils;

type
  TKeyValuePair = record
    pKey:    Pointer;
    pValue:  Pointer;
  end;  { record TKeyValuePair }

const
  EST_NOTFOUND = 'Item not found.';

var
  { Horrific hack to allow a member function of a class derived from
    TCustomKeyedList to be used in TList.Sort. }
  compareInstance: TCustomKeyedList;


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function CompareIndirectedPair(pItem1, pItem2: Pointer): Integer;
var
  pPair1:  ^TKeyValuePair;
  pPair2:  ^TKeyValuePair;

begin
  pPair1 := pItem1;
  pPair2 := pItem2;
  Result := compareInstance.CompareIndirectedKeys(pPair1^.pKey, pPair2^.pKey);
end;  { CompareIndirectedPair }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TCustomKeyedList.Create;
begin
  inherited Create;

  FSorted := True;
  FCursor := -1;
  FList := TList.Create;

end;  { TCustomKeyedList.Create }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TCustomKeyedList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;  { TCustomKeyedList.Destroy }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetIndirectedKey(i: Integer): Pointer;
var
  pPair:  ^TKeyValuePair;

begin
  pPair := FList.Items[i];
  Result := pPair^.pKey;
end;  { TCustomKeyedList.GetIndirectedKey }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetIndirectedValue(i: Integer): Pointer;
var
  pPair:  ^TKeyValuePair;

begin
  pPair := FList.Items[i];
  Result := pPair^.pValue;
end;  { TCustomKeyedList.GetIndirectedValue }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetIndirectedFoundKey: Pointer;
begin
  Result := IndirectedKey[FCursor];
end;  { TCustomKeyedList.GetIndirectedFoundKey }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetIndirectedFoundValue: Pointer;
begin
  Result := IndirectedValue[FCursor];
end;  { TCustomKeyedList.GetIndirectedFoundValue }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetSorted: Boolean;
begin
  Result := FSorted;
end;  { TCustomKeyedList.GetSorted }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.GetCount: Integer;
begin
  Result := FList.Count;
end;  { TCustomKeyedList.GetCount }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TCustomKeyedList.AppendIndirectedPair(pKey, pValue: Pointer);
var
  pPair:  ^TKeyValuePair;

begin
  New(pPair);
  try
    pPair^.pKey := pKey;
    pPair^.pValue := pValue;
    FList.Add(pPair);
    FSorted := False;  { list can no longer be assumed to be ordered }

  except
    on Exception do begin
      Dispose(pPair);
      raise;
    end;  { on Exception }
  end;  { try .. except }
end;  { TCustomKeyedList.AppendIndirectedPair }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{ TCustomKeyedList.InsertIndirectedPair
  This is intended to insert a key value pair at the correct point in the
  list to maintain the sort order.  Not yet implemented.
}


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TCustomKeyedList.Clear;
var
  pPair:  ^TKeyValuePair;

begin
  while FList.Count > 0 do begin
    pPair := FList.Items[0];
    FreeIndirectedPair(pPair^.pKey, pPair^.pValue);
    Dispose(pPair);
    FList.Delete(0);
  end;  { while FList.Count > 0 }
  FCursor := -1;
end;  { TCustomKeyedList.Clear }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TCustomKeyedList.FindIndirectedKey(pKey: Pointer): Boolean;
var
  found:       Boolean;
  minPos:      Integer;
  maxPos:      Integer;
  pos:         Integer;
  pThisKey:    Pointer;
  pThisPair:   ^TKeyValuePair;
  testResult:  Integer;

begin
  { initialisation }
  if not Sorted then Sort;

  pos := 0;
  minPos := 0;
  maxPos := FList.Count - 1;
  found := False;

  { search }
  while (minPos <= maxPos) and (not found) do begin

    pos := minPos + ((maxPos - minPos) div 2);
    pThisPair := FList.Items[pos];
    pThisKey := pThisPair^.pKey;

    testResult := CompareIndirectedKeys(pKey, pThisKey);
    if testResult = 0 then
      found := True
    else if testResult < 0 then
      maxPos := pos - 1
    else
      minPos := pos + 1;

  end;  { while (minPos <= maxPos) and (not found) }

  if found then FCursor := pos;
  Result := found;

end;  { TCustomKeyedList.FindIndirectedKey }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TCustomKeyedList.RemoveByIndirectedKey(pKey: Pointer);
begin
  if not FindIndirectedKey(pKey) then
      raise EKeyedListError.Create(EST_NOTFOUND);

  FreeIndirectedPair(IndirectedFoundKey, IndirectedFoundValue);
  FList.Delete(FCursor);
  FCursor := -1;
end;  { TCustomKeyedList.RemoveByIndirectedKey }


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
procedure TCustomKeyedList.Sort;
begin
  if not Sorted then begin
    compareInstance := Self;
    FList.Sort(CompareIndirectedPair);
    FSorted := True;
  end;  { if not Sorted }
end;  { TCustomKeyedList.Sort }


{-------------------------------------------------------------------------------
// Mark the list as unsorted (i.e. requiring a 'Sort' as part of the next
// find operation).  This is typically useful where the keys used are
// mutable for some reason.
// --------------------------------------------------------------------------- }
procedure TCustomKeyedList.SetUnsorted;
begin
  FSorted := False;
end;


end.
