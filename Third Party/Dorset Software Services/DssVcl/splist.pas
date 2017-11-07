{ ------------------------------------------------------------------------------
// Unit:          splist
//
//                Copyright © Dorset Software Services Ltd, 1999
//
// Description:   List of strings with associated untyped data pointers,
//                and binary search on the strings.
//
// Author:        AJWK
//
// Created:       1999-09-15
//
// Changes:
//
// To Do:         
// ----------------------------------------------------------------------------}
unit splist;

interface

uses
  KeyList;

type
  TStringAndPointerList = class(TCustomKeyedList)
  private
    function GetKey(i: Integer): String;
    function GetData(i: Integer): Pointer;

    function GetFoundData: Pointer;

  protected
    function CompareIndirectedKeys(pKey1, pKey2: Pointer): Integer;
        override;

    procedure FreeIndirectedPair(pKey, pValue: Pointer);
        override;

  public
    procedure Add(const key: String; data: Pointer);
    procedure Remove(const key: String);

    property Key[i: Integer]: String read GetKey;
    property Data[i: Integer]: Pointer read GetData;

    function FindKey(const key: String): Boolean;
    property FoundData: Pointer read GetFoundData;

  end;  {class TStringAndPointerList}




implementation

uses
  SysUtils;


{ ------------------------------------------------------------------------------
// TStringAndPointerList.GetKey
// ----------------------------------------------------------------------------}
function TStringAndPointerList.GetKey(i: Integer): String;
var
  pKey:  PString;

begin
  pKey := IndirectedKey[i];
  Result := pKey^;
end;  {TStringAndPointerList.GetKey}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.GetData
// ----------------------------------------------------------------------------}
function TStringAndPointerList.GetData(i: Integer): Pointer;
begin
  Result := IndirectedValue[i];
end;  {TStringAndPointerList.GetData}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.GetFoundData
// ----------------------------------------------------------------------------}
function TStringAndPointerList.GetFoundData: Pointer;
begin
  Result := IndirectedFoundValue;
end;  {TStringAndPointerList.GetFoundData}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.CompareIndirectedKeys
// ----------------------------------------------------------------------------}
function TStringAndPointerList.CompareIndirectedKeys(pKey1, pKey2: Pointer): Integer;
var
  pStr1, pStr2:  PString;

begin
  pStr1 := pKey1;
  pStr2 := pKey2;
  Result := CompareStr(pStr1^, pStr2^);
end;  {TStringAndPointerList.CompareIndirectedKeys}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.FreeIndirectedPair
// ----------------------------------------------------------------------------}
procedure TStringAndPointerList.FreeIndirectedPair(pKey, pValue: Pointer);
var
  pStr:  PString;

begin
  pStr := pKey;
  Dispose(pStr);
end;  {TStringAndPointerList.FreeIndirectedPair}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.Add
// ----------------------------------------------------------------------------}
procedure TStringAndPointerList.Add(const key: String; data: Pointer);
var
  pStr:  PString;

begin
  New(pStr);
  try
    pStr^ := key;
    AppendIndirectedPair(pStr, data);
  except
    Dispose(pStr);
    raise;
  end;  {try .. except}
end;  {TStringAndPointerList.Add}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.Remove
// ----------------------------------------------------------------------------}
procedure TStringAndPointerList.Remove(const key: String);
begin
  RemoveByIndirectedKey(@key);
end;  {TStringAndPointerList.Remove}


{ ------------------------------------------------------------------------------
// TStringAndPointerList.FindKey
// ----------------------------------------------------------------------------}
function TStringAndPointerList.FindKey(const key: String): Boolean;
begin
 Result := FindIndirectedKey(@key);
end;


end.

