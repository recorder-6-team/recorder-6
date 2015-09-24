//==============================================================================
//  Unit:        LastSearchedRucksack
//
//  Implements:  TLastSearchedRucksack
//
//  Description: Maintains a temporary table in SQL Server containing details
//                of the last rucksack used for a search.
//
//  Author:      John Durman
//  Created:     6 February 2008
//
//  Last Revision Details:
//    $Revision: 5 $
//    $Date: 11/04/08 15:32 $
//    $Author: Ericsalmon $
//
//  $History: LastSearchedRucksack.pas $
//  
//  *****************  Version 5  *****************
//  User: Ericsalmon   Date: 11/04/08   Time: 15:32
//  Updated in $/JNCC/Development/Build/Source
//  Bug fix for collation issue.
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 28/03/08   Time: 10:27
//  Updated in $/JNCC/Development/Build/Source
//  Bug fix for filename.
//  
//  *****************  Version 3  *****************
//  User: Johndurman   Date: 25/02/08   Time: 9:16
//  Updated in $/JNCC/Development/Build/Source
//  VI 16178 - No current rucksack message
//  
//  *****************  Version 2  *****************
//  User: Johndurman   Date: 11/02/08   Time: 9:21
//  Updated in $/JNCC/Development/Build/Source
//  VI 15634 - CCN151 - ZENTRUM - Add a search code to taxon list in
//  rucksack.
//  
//  *****************  Version 1  *****************
//  User: Johndurman   Date: 7/02/08    Time: 13:11
//  Created in $/JNCC/Development/Build/Source
//  VI 15634 - CCN151 - ZENTRUM - Add a search code to taxon list in
//  rucksack.
//
//==============================================================================

unit LastSearchedRucksack;

interface

uses
  Classes, ExceptionForm, DataClasses;

type
  TLastSearchedRucksack = class
  private
    FIsDirty: Boolean;
    FRucksackName: string;
    FTableCreated: Boolean;
    FRucksackFile: TextFile;
    FUseCurrentRucksack: Boolean;
    procedure SetRucksackName(const Value: string);
    procedure SetIsDirty(const Value: Boolean);
    procedure ReadCurrentTLIKeys(TLIKeys: TStringList);
    procedure ReadTLIKeys(TLIKeys: TStringList);
    procedure InsertSearchCodesFromCurrentRucksack(TLIKeys: TStringList);
    procedure InsertSearchCodesFromFile(TLIKeys: TStringList);
    procedure InsertSearchCodeToTemporaryTable(const AKey: TKeyString;
      const ASearchCode: string; TLIKeys: TStringList);
    procedure InsertTaxaWithoutSearchCodes(TLIKeys: TStringList);
  public
    constructor Create;
    property RucksackName: string read FRucksackName write SetRucksackName;
    property IsDirty: Boolean read FIsDirty write SetIsDirty;
    property UseCurrentRucksack: Boolean read FUseCurrentRucksack;
    procedure BuildTemporaryTable;
  end;

implementation

uses
  GeneralData, SysUtils, Constants, ApplicationSettings, Rucksack;

const
  CREATE_TABLE_SQL =
      'CREATE TABLE #LastSearchedRucksack ('
      + 'TAXON_LIST_ITEM_KEY CHAR(16) COLLATE Database_Default NOT NULL, '
      + 'SearchCode VARCHAR(30) COLLATE Database_Default NULL)';
  CLEAR_TABLE_SQL = 'DELETE #LastSearchedRucksack';
  INSERT_NO_SEARCH_CODE_SQL =
      'INSERT INTO #LastSearchedRucksack (TAXON_LIST_ITEM_KEY, SearchCode) VALUES (''%s'', NULL)';
  INSERT_SEARCH_CODE_SQL =
      'INSERT INTO #LastSearchedRucksack (TAXON_LIST_ITEM_KEY, SearchCode) VALUES (''%s'', ''%s'')';

resourcestring
  ResStr_CannotClearTemporaryRucksackTable = 'The system is unable to clear the temporary database table for the rucksack.';
  ResStr_CannotCreateTemporaryRucksackTable = 'The system is unable to create a temporary database table for the rucksack.';
  ResStr_CannotInsertSearchCodeIntoTemporaryRucksackTable = 'The system cannot insert the search code "%s" for taxon list item "%s" in rucksack "%s" into the temporary table.';
  ResStr_CannotInsertTaxonListItemIntoTemporaryRucksackTable = 'The system cannot insert the taxon list item "%s" in rucksack "%s" into the temporary table.';
  ResStr_RucksackFileNotFound = 'The rucksack file "%s" cannot be found.';
  ResStr_FileError = 'Unable to load the rucksack file "%s".';
  ResStr_NoCurrentRucksack = 'There is no current rucksack at present.';

{ TLastSearchedRucksack }

{-------------------------------------------------------------------------------
  If neccessary, generates a temporary table in SQL Server containing details
  of the rucksack being searched.
}
procedure TLastSearchedRucksack.BuildTemporaryTable;
var
  TLIKeys: TStringList;
begin
  if IsDirty and (UseCurrentRucksack or (Length(RucksackName) > 0)) then begin
    // Create or clear out the temporary table
    if (FTableCreated) then begin
      dmGeneralData.ExecuteSQL(CLEAR_TABLE_SQL, ResStr_CannotClearTemporaryRucksackTable, False);
    end else begin
      dmGeneralData.ExecuteSQL(CREATE_TABLE_SQL, ResStr_CannotCreateTemporaryRucksackTable, False);
      FTableCreated := True;
    end;    // if (FTableCreated)

    // Populate the temporary table with keys and search codes for this rucksack
    TLIKeys := TStringList.Create;
    try
      if UseCurrentRucksack then begin
        ReadCurrentTLIKeys(TLIKeys);
        InsertSearchCodesFromCurrentRucksack(TLIKeys);
      end else begin
        AssignFile(FRucksackFile, RucksackName);
        try
            ReadTLIKeys(TLIKeys);
            InsertSearchCodesFromFile(TLIKeys);
        finally
          CloseFile(FRucksackFile);
        end;
      end;    // if UseCurrentRucksack
      InsertTaxaWithoutSearchCodes(TLIKeys);
    finally
      TLIKeys.Free;
    end;
  end;    // if IsDirty and (Length(RucksackName) > 0)
end;    // TLastSearchedRucksack.BuildTemporaryTable

{-------------------------------------------------------------------------------
  Initialise the object's fields
}
constructor TLastSearchedRucksack.Create;
begin
  FIsDirty := True;
end;    // TLastSearchedRucksack.Create

{-------------------------------------------------------------------------------
  Read the search codes from the current rucksack and insert them into the temporary table
}
procedure TLastSearchedRucksack.InsertSearchCodesFromCurrentRucksack(
  TLIKeys: TStringList);
var
  i: integer;
  KeyList: TEditableKeyList;
begin
  KeyList := AppSettings.CurrentRucksack.TaxonSearchCodeList;
  for i := 0 to Pred(KeyList.Header.ItemCount) do
    if Length(KeyList.Items[i].KeyField2) > 0 then
      InsertSearchCodeToTemporaryTable(KeyList.Items[i].KeyField1,
        KeyList.Items[i].KeyField2, TLIKeys);
end;    // TLastSearchedRucksack.InsertSearchCodesFromCurrentRucksack

{-------------------------------------------------------------------------------
  Read the search codes from the rucksack file and insert them into the temporary table
}
procedure TLastSearchedRucksack.InsertSearchCodesFromFile(TLIKeys: TStringList);
var
  lCurrent, lKey, lSearchCode: string;
begin
  try
    lCurrent := '';
    while (lCurrent <> '<TAXONSEARCHCODE>') and (not Eof(FRucksackFile)) do
      Readln(FRucksackFile, lCurrent);
    Readln(FRucksackFile, lCurrent);
    while (lCurrent <> '</TAXONSEARCHCODE>') and (not Eof(FRucksackFile)) do begin
      TfrmRuckSack.SplitNameValuePair(lKey, lSearchCode, lCurrent);
      if (TLIKeys.IndexOf(lKey) >= 0) then begin
        InsertSearchCodeToTemporaryTable(lKey, lSearchCode, TLIKeys);
      end;    // if (TLIKeys.IndexOf(lKey) >= 0)
      Readln(FRucksackFile, lCurrent);
    end;    // while
  except on e: Exception do
    if e is ERUKError then
      raise
    else
      raise ERUKError.Create(Format(ResStr_FileError, [RucksackName]), E);
  end;
end;  // TLastSearchedRucksack.InsertSearchCodesFromFile

{-------------------------------------------------------------------------------
  Inserts the search codes into the temporary table
}
procedure TLastSearchedRucksack.InsertSearchCodeToTemporaryTable(
  const AKey: TKeyString; const ASearchCode: string; TLIKeys: TStringList);
var
  lSql, lErrorMsg: string;
begin
  lSql := Format(INSERT_SEARCH_CODE_SQL, [AKey, ASearchCode]);
  lErrorMsg := Format(ResStr_CannotInsertSearchCodeIntoTemporaryRucksackTable,
    [ASearchCode, AKey, RucksackName]);
  dmGeneralData.ExecuteSQL(lSql, lErrorMsg, False);
  TLIKeys.Delete(TLIKeys.IndexOf(AKey));
end;    // TLastSearchedRucksack.InsertSearchCodeToTemporaryTable

{-------------------------------------------------------------------------------
  Inserts the taxa keys that don't have search codes into the temporary table
}
procedure TLastSearchedRucksack.InsertTaxaWithoutSearchCodes(
  TLIKeys: TStringList);
var
  i: integer;
begin
  for i := 0 to Pred(TLIKeys.Count) do
    dmGeneralData.ExecuteSQL(Format(INSERT_NO_SEARCH_CODE_SQL, [TLIKeys[i]]),
      Format(ResStr_CannotInsertTaxonListItemIntoTemporaryRucksackTable,
      [TLIKeys[i], RucksackName]), False);
end;    // TLastSearchedRucksack.InsertTaxaWithoutSearchCodes

{-------------------------------------------------------------------------------
  Reads the Taxon Keys from the currently selected rucksack
}
procedure TLastSearchedRucksack.ReadCurrentTLIKeys(TLIKeys: TStringList);
var
  i: integer;
  KeyList: TEditableKeyList;
begin
  KeyList := AppSettings.CurrentRucksack.TaxonList;
  for i := 0 to Pred(KeyList.Header.ItemCount) do
    TLIKeys.Add(KeyList.Items[i].KeyField1);
end;    // TLastSearchedRucksack.ReadCurrentTLIKeys

{-------------------------------------------------------------------------------
  Reads the Taxon Keys from the relevent rucksack file
}
procedure TLastSearchedRucksack.ReadTLIKeys(TLIKeys: TStringList);
var
  lCurrent: string;
begin
  try
    Reset(FRucksackFile);
    lCurrent := '';
    while lCurrent <> '<TAXON>' do
      Readln(FRucksackFile, lCurrent);
    Readln(FRucksackFile, lCurrent);
    while lCurrent <> '</TAXON>' do begin
      TLIKeys.Add(lCurrent);
      Readln(FRucksackFile, lCurrent);
    end;    // while
  except on e: Exception do
    if e is ERUKError then
      raise
    else
      raise ERUKError.Create(Format(ResStr_FileError, [RucksackName]), E);
  end;
end;    // TLastSearchedRucksack.ReadTLIKeys

{-------------------------------------------------------------------------------
  Accessor method - sets whether the temporary table needs to be refreshed
}
procedure TLastSearchedRucksack.SetIsDirty(const Value: Boolean);
begin
  FIsDirty := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method - sets the filename of the rucksack being searched.
  If supplied with the STR_RUCKSACK constant it replaces it with the current
  rucksack's file name.
  If the file cannot be found it tries removing any ' Rucksack' suffix
  and adding the correct extension.  If all fails, it throws an exception.
}
procedure TLastSearchedRucksack.SetRucksackName(const Value: string);
var
  NewFileName, CompleteNewFileName: string;
begin
  if (FRucksackName <> Value) and (Value <> ExtractFileName(FRucksackName)) then begin
    // Check whether current rucksack has been specified
    if AnsiSameText(Value, STR_RUCKSACK) or AnsiSameText(Value + ResStr_Rucksack, STR_RUCKSACK) then begin
      FUseCurrentRucksack := True;
      NewFileName         := AppSettings.CurrentRucksack.FileName;
      CompleteNewFileName := NewFileName;
    end else begin
      FUseCurrentRucksack := False;
      NewFileName         := Value;
      CompleteNewFileName :=
          IncludeTrailingPathDelimiter(AppSettings.RucksackPath)
          + NewFileName + '.ruk';

      // If neccessary, remove any ' Rucksack' suffix from the file name
      if not FileExists(CompleteNewFileName) then
        if (Pos(ResStr_Rucksack, NewFileName) = Length(NewFileName) - Length(ResStr_Rucksack)) then begin
          NewFileName         := Copy(NewFileName, 1, Pos(ResStr_Rucksack, NewFileName));
          CompleteNewFileName :=
              IncludeTrailingPathDelimiter(AppSettings.RucksackPath)
              + ChangeFileExt(NewFileName, '.ruk');
        end;

      // Check the rucksack file exists
      if (not FileExists(CompleteNewFileName)) then
        raise ERUKError.CreateNonCritical(Format(
          ResStr_RucksackFileNotFound, [CompleteNewFileName]));

      if AnsiSameText(CompleteNewFileName, AppSettings.CurrentRucksack.FileName) then
        FUseCurrentRucksack := True;
    end;

    if (CompleteNewFileName <> FRucksackName) then begin
      FRucksackName := CompleteNewFileName;
      FIsDirty := True;
    end;
  end;
end;  // TLastSearchedRucksack.SetRucksackName

end.
