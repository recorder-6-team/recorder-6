//------------------------------------------------------------------------
// Author:    Stuart Ball
// Date:      October 2006
// Version:   Recorder 6 version
// Purpose:   Simple object to handle error reporting for Recorder 3
//            data import addin. Error details are stored in arecord which
//            is kept in a dynamic list. Mostly, error are added to the list
//            during processing and then they are dumped to a text file
//            near the end.
//------------------------------------------------------------------------
unit ImportError;

interface

uses
  Classes, SysUtils;

type
  EImportError = class(Exception);

  TErrorType = (errNone, errInsert, errLastKey, errKeyUpdate, errCopy,
                errValidate, errDuplicate, errDelete, errUpdate);

  // record in which error item will be stored
  TImportErr = record
    TableName:  string;
    Key: string;
    ErrMessage: string;
    ErrType: TErrorType;
    Reject: boolean;
    Fatal: boolean;
  end;

  TErrorList = class(TObject)
    private
      FCount: Longint;
      function GetItem(iIndex: integer): TImportErr;
    protected
      FItems: Array of TImportErr;
    public
      destructor Destroy; override;
      constructor Create;
      property Items[iIndex: integer]: TImportErr read GetItem;
      property Count: integer read FCount;
      procedure DeleteItem(iIndex: integer);
      procedure Clear;
      procedure DumpToFile(const aPath: string);
      function AddItem(const aTableName, aKey, aMessage: string; const aType: TErrorType): integer;
      function IndexOf(const aTableName, aKey: string): integer;
      function IsReject(const aTableName, aKey: string): boolean;
  end;

implementation

{ TErrorList }

//------------------------------------------------------------------------
// Initialise
//------------------------------------------------------------------------
constructor TErrorList.Create;
begin
     inherited Create;
     FCount := 0;
end;

//------------------------------------------------------------------------
// Don#'t actually need to do much
//------------------------------------------------------------------------
destructor TErrorList.Destroy;
begin
     inherited Destroy;
end;

//------------------------------------------------------------------------
// Add a new error to the list
//------------------------------------------------------------------------
function TErrorList.AddItem(const aTableName, aKey, aMessage: string; const aType: TErrorType): integer;
begin
  FCount := FCount + 1;
  SetLength(FItems, FCount);
  FItems[High(FItems)].TableName := aTableName;
  FItems[High(FItems)].Key := aKey;
  FItems[High(FItems)].ErrMessage := aMessage;
  FItems[High(FItems)].errType := aType;
  Result := FCount;
end;

//------------------------------------------------------------------------
//  Clear the error list
//------------------------------------------------------------------------
procedure TErrorList.Clear;
begin
  FCount := 0;
  SetLength(FItems, FCount);
end;

//------------------------------------------------------------------------
//  Delete the error from the list at given position
//------------------------------------------------------------------------
procedure TErrorList.DeleteItem(iIndex: integer);
var
  i: Integer;
begin
  //Move all subsequent items up one place
  for i:= (iIndex + 1) to FCount - 1 do
	  FItems[i - 1] := FItems[i];

  //Resize to lose last element
  FCount:= FCount - 1;
  SetLength(FItems, FCount);
end;

//------------------------------------------------------------------------
// Dump errors to a text file
//------------------------------------------------------------------------
procedure TErrorList.DumpToFile(const aPath: string);
var lStrings: TStrings;
    work: string;
    i: integer;
begin
     If FCount > 0 then
     begin
          lStrings := TStringList.Create;
          try
             // header
             work := 'Bulk import from Recorder 3 on ' + DateToStr(date) + '.';
             lStrings.Add(work);
             lStrings.Add(StringOfChar('-', Length(Work)));
             lStrings.Add('');
             // add a line for each error
             For i:=0 to FCount-1 do
             begin
                  case Items[i].ErrType of
                  errNone:       work := 'No error in ';
                  errInsert:     work := 'Insert error in ';
                  errLastKey:    work := 'Could not get key for ';
                  errKeyUpdate:  work := 'Could not update the key in ';
                  errCopy:       work := 'Could not copy ';
                  errValidate:   work := 'Validation failure ';
                  errDuplicate:  work := 'Duplicate key error in ';
                  errDelete:     work := 'Could not delete ';
                  errUpdate:     work := 'Error updating ';
                  end; //case
                  work := work + 'table ' + UpperCase(items[i].TableName) +
                          ' and row "' + Items[i].key + '" with message "' +
                          items[i].errMessage + '".';
                  lStrings.Add(work);
             end;
             // save the file
             lStrings.Add('');
             lStrings.SaveToFile(aPath + 'Rec3Import_Errs.txt');
          finally
             lStrings.Free;
          end;
     end;
end;

//------------------------------------------------------------------------
//  Get item at given position
//------------------------------------------------------------------------
function TErrorList.GetItem(iIndex: integer): TImportErr;
begin
  if iIndex <= High(FItems) then
    Result := FItems[iIndex]
  else
    Raise EImportError.Create('Trying to read non-existent item from error list');
end;

//------------------------------------------------------------------------
// Find item by table and key
//------------------------------------------------------------------------
function TErrorList.IndexOf(const aTableName, aKey: string): integer;
var  lItemIndex, lMatchIndex: Integer;
begin
  //Set pessimistic result
  lMatchIndex:= -1;

  //Find first item then break from loop
  for lItemIndex:= 0 to FCount - 1 do
    if (CompareText(FItems[lItemIndex].TableName, aTableName) = 0)
          and (CompareText(FItems[lItemIndex].Key, aKey) = 0) then
    begin
      lMatchIndex:= lItemIndex;
      break; // from loop - found item
    end;
  Result := lMatchIndex;
end;

//------------------------------------------------------------------------
//
//------------------------------------------------------------------------
function TErrorList.IsReject(const aTableName, aKey: string): boolean;
var i: Longint;
begin
   i := IndexOf(aTableName, aKey);
   Result := Items[i].Reject;
end;

end.
