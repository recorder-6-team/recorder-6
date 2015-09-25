{===============================================================================
  Unit:           DBUpgrader

  Defines:

  Description:

  Created:

  Last revision information:
    $Revision: 28 $
    $Date: 6/07/09 11:03 $
    $Author: Ericsalmon $

===============================================================================}
unit DBUpgrader;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ComCtrls, ADODb, Settings,
  ExceptionForm, StdCtrls, PrepareMDB, VersionInfo2, VCLUnzip, Dialogs;

resourcestring
  ResStr_MissingScript =
      'The upgrade cannot complete because script number %s '
      + 'is missing.  Please locate this script, place it into the Scripts folder in '
      + 'the upgrade kit and restart the upgrade.';
  ResStr_CannotDeleteFile =
      'The upgrader could not delete the file %s and therefore cannot proceed.';

type
  EDBUpgraderError = class (TExceptionPath)
  end;

  TDBUpgrader = class (TObject)
  private
    FConnection: TADOConnection;
    FCurrentScriptSequence: string;
    FQuery: TADOQuery;
    FRunList: TStringList;
    FSettings: TSettings;
    FPrepareMDB: TPrepareMDB;
    FMergeProgressBar: TProgressBar;
    FMergeLabel: TLabel;
    FPreviousScriptSequence: string;
    FUpdateDescriptions: TStringList;
    FThisFileRecordCount: integer;
    FTaxonNameIndexRebuild: boolean;
    FTaxonSynonymIndexRebuild: boolean;
    FTaxonGroupIndexRebuild: boolean;
    FSettingsRecordChecked: boolean;
    procedure Connect;
    function GetPrepareMDB: TPrepareMDB;
    procedure RunLinkedAccessUpdate(const AUpdateFile: string);
    function GetCurrentScriptNumber(const AKey: string): string;
    procedure PrepareRunList;
    procedure RunQuery;
    procedure RunScript(const AScriptIndex: integer; AProgressBar: TProgressBar);
    property PrepareMDB: TPrepareMDB read GetPrepareMDB;
    procedure CheckScriptsInSequence(const lastSequence: string; runList: TStringList);
    procedure UpdateVersion;
    procedure MergeZipFile(const AFileName: string);
    procedure CreateDmDatabaseIfNeeded;
  public
    constructor Create(ASettings: TSettings);
    destructor Destroy; override;
    procedure FixMapDatasetFile;
    procedure PrepareMapSheetTable(AProgressBar: TProgressBar);
    procedure RunUpdates(AProgressBar, AMergeProgressBar: TProgressBar;
        AMergeLabel: TLabel);
    procedure UpdateDBSeq(const ALastScript: String);
    procedure SetProgress(const iProgress: Integer; iPrcMsgs: Boolean = true);
    procedure SetStatus(const iStatusString: String; iPrcMsgs: Boolean = true);
    procedure HandleMetadataIniFile(const AFileName: string);
    procedure UpdateTaxonIndices;
    property PreviousScriptSequence: string read FPreviousScriptSequence;
    property CurrentScriptSequence: string read FCurrentScriptSequence;
    property UpdateDescriptions: TStringList read FUpdateDescriptions;
  end;

//==============================================================================
implementation

uses
  GeneralFunctions, DBMerger, DatabaseAccessADO, DatabaseUtilities,
  DefaultPaths, MissingDBScripts;

resourcestring
  SDBUpgrader_ScriptError = 'Error occurred during processing of script ';

const
  SQL_ENSURE_LOCAL_DB_SEQ = 'IF NOT EXISTS(SELECT 1 FROM Setting WHERE [Name]=''%s'') '+
      'INSERT INTO Setting ([Name], Data) VALUES (''%s'', '''')';

{-==============================================================================
    TDBUpgrader
===============================================================================}
{-------------------------------------------------------------------------------
  Initialises member fields.
  Pass True in AConnectionString to use integrated security, or false if the sa username and
      password are provided.
}
constructor TDBUpgrader.Create(ASettings: TSettings);
begin
  inherited Create;

  FRunList := TStringList.Create;
  FSettings := ASettings;
  FConnection := TADOConnection.Create(nil);
  FQuery := TADOQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.ParamCheck := False;
  FQuery.CommandTimeout := 0;
  FUpdateDescriptions := TStringList.Create;
  // flag to check that thet tracking record in Settings exists
  FSettingsRecordChecked := false;

  // dmDatabase is created on demand for a merge
  dmDatabase := nil;
end;  // TDBUpgrader.Create

{-------------------------------------------------------------------------------
  Destroys any member fields.
}
destructor TDBUpgrader.Destroy;
begin
  FRunList.Free;
  FQuery.Free;
  FConnection.Free;
  dmDatabase.Free;
  FUpdateDescriptions.Free;

  inherited Destroy;
end;  // TDBUpgrader.Destroy

{-------------------------------------------------------------------------------
  Confirms that no scripts are missing from the sequence.  Aborts if any missing
  scripts detected.
}
procedure TDBUpgrader.CheckScriptsInSequence(const lastSequence: string; runList: TStringList);
var
  lExpectedNextScript: string;
  i: integer;

  function IncrementSequence(seq: string): string;
  var
    lPos: integer;
  begin
    if seq='' then
      Result := '00000000'
    else begin
      lPos := 8;
      Result := seq;
      while lPos>=1 do begin
        if ((Result[lPos]>='0') and (Result[lPos]<'9') or
            (Result[lPos]>='A') and (Result[lPos]<'Z')) then begin
          Result[lPos] := Chr(Ord(Result[lPos])+1);
          break;
        end
        else if (Result[lPos]='9') then begin
          Result[lPos] := 'A';
          break;
        end
        else begin
          Result[lPos] := '0';
          Dec(lPos);
        end;
      end;
    end;
  end;

begin
  lExpectedNextScript := IncrementSequence(lastSequence);
  for i := 0 to runList.Count-1 do begin
    if ExtractWithoutExt(runList[i]) > lExpectedNextScript then
    begin
      with TdlgMissingScripts.Create(nil) do
        try
          ShowModal;
        finally
          Free;
        end;
      raise EAbort.Create('');
    end;
    lExpectedNextScript := IncrementSequence(lExpectedNextScript);
  end;
end;  // TDBUpgrader.CheckScriptsInSequence

{-------------------------------------------------------------------------------
  Sets the connection login to the current settings and connects to the database.
  Raises an exception if the login fails.
}
procedure TDBUpgrader.Connect;
begin
  if not FConnection.Connected then begin
    FConnection.ConnectionString := FSettings.GetConnectionString;
    // connect or raise an exception
    FConnection.Connected := True;
  end;
end;  // TDBUpgrader.Connect

{-------------------------------------------------------------------------------
}
procedure TDBUpgrader.FixMapDatasetFile;
begin
  // Only do something if there is a reason to do it.
  if FileExists(FSettings.MapFilePath + 'R2KMapDataset.gds') then begin
    // Get the installed maps, ordered to get default one first, if any.
    // There should be only one there anyway.
    FQuery.SQL.Text := 'SELECT * FROM Computer_Map ORDER BY Default_Map DESC';
    FQuery.Open;
    if FQuery.Eof then
      // No maps in database, file must be messed up left over. Remove it.
      DeleteFile(PChar(FSettings.MapFilePath + 'R2KMapDataset.gds'))
    else
      // Map there, use Base_Map_Key to rename file.
      RenameFile(FSettings.MapFilePath + 'R2KMapDataset.gds',
                 FSettings.MapFilePath + FQuery.FieldByName('Base_Map_Key').AsString + '.gds');
    FQuery.Close;
  end
end;  // TDBUpgrader.FixMapDatasetFile 

{-------------------------------------------------------------------------------
  Returns the number of the last run script on the database.  This is an NBN 8 character key. 
}
function TDBUpgrader.GetCurrentScriptNumber(const AKey: string): string;
begin
  with FQuery do
    try
      SQL.Text := 'SELECT Data FROM Setting WHERE Name=''' + AKey + '''';
      Open;
      if RecordCount = 0 then
        Result := ''
      else
        Result := FieldByName('Data').AsString;
    finally
      Close;
    end;
end;  // TDBUpgrader.GetCurrentScriptNumber

{-------------------------------------------------------------------------------
}
procedure TDBUpgrader.PrepareMapSheetTable(AProgressBar: TProgressBar);
var
  i: Integer;

  function GetSpatialSystem(const AFileName: String): String;
  var
    lRootName: String;
    lStrings: TStringList;
  begin
    lRootName := ExtractWithoutExt(AFileName);
    // Read the .ini file which contains extra information about the map
    if FileExists(FSettings.BaseMapPath + lRootName + '.ini') then begin
      lStrings := TStringList.Create;
      try
        lStrings.LoadFromFile(FSettings.BaseMapPath + lRootName + '.ini');
        try
          Result := lStrings.Values['system'];
        except
          on EStringListError do;
        end;
      finally
        lStrings.Free;
      end;
    end else
      Result := '';
  end;
  
begin
  // This will make certain the Spatial_Ref_System of Base Map records is set
  // Can't be done just by scripting  :-(
  Connect;
  with TADODataset.Create(nil) do
    try
      Connection := FConnection;
      CommandTimeout := 0;
      CommandText := 'SELECT * FROM Map_Sheet ' +
                     'WHERE Sheet_Type = 0 AND Spatial_Ref_System IS NULL';
      Open;
      i := 0;
      while not Eof do begin
        FQuery.SQL.Text := Format('UPDATE Map_Sheet SET Spatial_Ref_System = ''%s'' ' +
                                  'WHERE Map_Sheet_Key = ''%s''',
                                  [GetSpatialSystem(FieldByName('File_Name').AsString),
                                   FieldByName('Map_Sheet_Key').AsString]);
        FQuery.ExecSQL;
        Inc(i);
        AProgressBar.Position := i * 100 div RecordCount;
        Next;
      end;
      Close;
    finally
      Free;
    end;
end;  // TDBUpgrader.PrepareMapSheetTable

{-------------------------------------------------------------------------------
  Prepares the list of scripts to run (FRunList).  This is all scripts whose keys are higher
      than the value currently recorded against the database Settings table.
}
procedure TDBUpgrader.PrepareRunList;
var
  lLastRunScript: string;

    procedure GetFilesForExt(const AExt, ALastRunScript: string);
    var
      lSearchRec: TSearchRec;
    begin
      if FindFirst(ExtractFilePath(Application.Exename) + 'Scripts\*.' + AExt,
                   faAnyFile, lSearchRec) = 0 then
        repeat
          if (CompareText(ExtractFileExt(lSearchRec.Name), '.' + AExt)=0) and
              (ExtractWithoutExt(lSearchRec.Name) > ALastRunScript) then
            FRunList.Add(lSearchRec.Name);
        until FindNext(lSearchRec) <> 0;
    end;

begin
  FRunList.Clear;
  lLastRunScript := GetCurrentScriptNumber(FSettings.SequenceSettingName);
  FPreviousScriptSequence := lLastRunScript;
  FCurrentScriptSequence := FPreviousScriptSequence;
  GetFilesForExt('sql', lLastRunScript);
  // Also retrieve any dictionary zipped Access files
  GetFilesForExt('zip', lLastRunScript);
  FRunList.Sort;
  CheckScriptsInSequence(FCurrentScriptSequence, FRunList);
end;  // TDBUpgrader.PrepareRunList

{-------------------------------------------------------------------------------
  Runs the query already loaded into FQuery.
  Clears the SQL ready for the next query.
  If the query starts with a comment --try, then any errors are ignored.
}
procedure TDBUpgrader.RunQuery;
begin
  try
    FQuery.ExecSQL;
    Application.ProcessMessages;
  except
    on E:Exception do begin
      // batches with --try on first line are allowed to fail in our
      if CompareText(FQuery.SQL[0], '--try') <> 0 then begin
        FQuery.SQL.SaveToFile(IncludeTrailingPathDelimiter(GetWindowsTempDir) +
                              'Failed Query.sql');
        raise;
      end;
    end; // on Exception
  end; // try
  FQuery.SQL.Clear;
end;  // TDBUpgrader.RunQuery

{-------------------------------------------------------------------------------
  Loads the SQL file identified by AScriptFile (without extension) from the \Scripts folder 
      and executes it.
}
procedure TDBUpgrader.RunScript(const AScriptIndex: integer; AProgressBar: 
    TProgressBar);
var
  lScript: TStringList;
  lLineIdx: Integer;
begin
  lScript := TStringList.Create;
  try
    lScript.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Scripts\' +
                                         FRunList[AScriptIndex]);
    FQuery.SQL.Clear;
    try
      FQuery.Connection.BeginTrans;
      for lLineIdx := 0 to lScript.Count - 1 do
        if CompareText(Trim(lScript[lLineIdx]), 'GO') = 0 then begin
          RunQuery;
          AProgressBar.Position := AScriptIndex * 100 div FRunList.Count
              + lLineIdx div (FRunList.Count * lScript.Count);
          AProgressBar.Refresh;
        end else
          FQuery.SQL.Add(lScript[lLineIdx]);
      // if there was a query at the end with no GO, also run it
      if FQuery.SQL.Count > 0 then
        RunQuery;
      FQuery.Connection.CommitTrans;
    except
      on E:Exception do begin
        FQuery.Connection.RollbackTrans;
        raise EDBUpgraderError.Create('Error occurred running script ' + FRunList[AScriptIndex] +
            '.  The error is described as:'#13#10 + E.Message);
      end;
    end;
    // If the script file requires the linked access MDB file to be updated,
    // then do it.  This can not be locale specific, so only compare against
    // main SQL files
    if CompareText(ExtractFileExt(FRunList[AScriptIndex]), '.sql')=0 then
      if FileExists(ExtractFilePath(Application.ExeName) + 'Scripts\' +
                    ExtractWithoutExt(FRunList[AScriptIndex]) + '.ldu') then
        RunLinkedAccessUpdate(ExtractFilePath(Application.ExeName) + 'Scripts\' +
                    ExtractWithoutExt(FRunList[AScriptIndex]) + '.ldu');
  finally
    lScript.Free;
  end;
end;  // TDBUpgrader.RunScript

{-------------------------------------------------------------------------------
  When database structure changes are applied, update the access linked database
}
procedure TDBUpgrader.RunLinkedAccessUpdate(const AUpdateFile: string);
var
  lScript: TStringList;
  i: integer;
  lTable, lAction: string;

    // parse the table and command from a line in the script
    procedure GetTableAndAction(const AIndex: integer; var ATable, AAction: string);
    begin
      AAction := Copy(lScript[AIndex], 1, Pos(' ', lScript[AIndex])-1);
      ATable := Copy(lScript[AIndex], Length(AAction)+2, 255);
    end;

begin
  try
    lScript := TStringList.Create;
    try
      lScript.LoadFromFile(AUpdateFile);
      for i := 0 to lScript.Count-1 do begin
        GetTableAndAction(i, lTable, lAction);
        if CompareText(lAction, 'delete')=0 then
          PrepareMDB.Delete(lTable)
        else if CompareText(lAction, 'update')=0 then
          PrepareMDB.Update(lTable)
        else if CompareText(lAction, 'add')=0 then
          PrepareMDB.Add(lTable);
      end;
    finally
      lScript.Free;
    end; // try
  except
    On E:Exception do
      ShowInformation('The linked Access database was not properly upgraded.  This does '+
          'not affect any core Recorder functionality but might affect any addins '+
          'that use this to access the database.');
   end;
end; // TDBUpgrader.RunLinkedAccessUpdate

{-------------------------------------------------------------------------------
  Run each of the scripts identified in FRunList in turn.
}
procedure TDBUpgrader.RunUpdates(AProgressBar, AMergeProgressBar: TProgressBar;
    AMergeLabel: TLabel);
var
  i: Integer;
begin
  Connect;
  FQuery.SQL.Text := 'SET ARITHABORT ON';
  FQuery.ExecSQL;
  PrepareRunList;
  FMergeProgressBar := AMergeProgressBar;
  FMergeLabel := AMergeLabel;
  AProgressBar.Position := 0;
  if FRunList.Count > 0 then begin
    for i := 0 to FRunList.Count - 1 do begin
      FThisFileRecordCount := 0;
      if FileExists(ExtractFilePath(Application.ExeName) + 'Scripts\' +
          ExtractWithoutExt(FRunList[i]) + '.ini') then
        HandleMetadataIniFile(ExtractFilePath(Application.ExeName) + 'Scripts\' +
          ExtractWithoutExt(FRunList[i]) + '.ini');
      if SameText(ExtractFileExt(FRunList[i]), '.sql') then begin
        // each script should default to ansi_nulls, as this allows indexed views
        FQuery.SQL.Text := 'SET ANSI_NULLS ON';
        FQuery.ExecSQL;
        RunScript(i, AProgressBar);
      end
      else if SameText(ExtractFileExt(FRunList[i]), '.zip') then
        MergeZipFile(FRunList[i]);
      AProgressBar.Position := (i + 1) * 100 div FRunList.Count;
      AProgressBar.Refresh;
      UpdateDBSeq(FRunList[i]);
    end;
  end;
  UpdateTaxonIndices;
  UpdateVersion;
end;  // TDBUpgrader.RunScripts

{-------------------------------------------------------------------------------
  Updates the DB/Dict Seq setting in the Settings table to reflect the last script run.
}
procedure TDBUpgrader.UpdateDBSeq(const ALastScript: String);
begin
  // check that the appropriate record is available in Settings, only once
  if not FSettingsRecordChecked then begin
    FQuery.SQL.Text := 'IF NOT EXISTS(SELECT 1 FROM Setting WHERE Name=' +
      '''' + FSettings.SequenceSettingName + ''') '+
      'INSERT INTO Setting (Name) VALUES (''' + FSettings.SequenceSettingName + ''')';
    FQuery.ExecSQL;
    FSettingsRecordChecked := true;
  end;
  FQuery.SQL.Text := 'UPDATE Setting SET Data=''' + ExtractWithoutExt(ALastScript) +
      ''' WHERE Name=''' + FSettings.SequenceSettingName + '''';
  FQuery.ExecSQL;
  FCurrentScriptSequence := ExtractWithoutExt(ALastScript);
end;  // TDBUpgrader.UpdateDBSeq

{-------------------------------------------------------------------------------
  Instantiate the PrepareMDB object on demand
}
function TDBUpgrader.GetPrepareMDB: TPrepareMDB;
begin
  if not assigned(FPrepareMDB) then
    FPrepareMDB := TPrepareMDB.Create(FSettings.ServerName, FSettings.DatabasePath,
      FSettings.TrustedSecurity, 'sa', FSettings.Password);
  Result := FPrepareMDB;
end;

{-------------------------------------------------------------------------------
  Update version info in Setting table.
}
procedure TDBUpgrader.UpdateVersion;
var
  lVersion: TVersionInfo;
begin
  lVersion := TVersionInfo.CreateFromFile(IncludeTrailingPathDelimiter(FSettings.InstallationPath) +
                                          'RecorderApp.exe');
  try
    FQuery.SQL.Text := 'UPDATE Setting SET Data=''' + lVersion.FileVersion + ''' WHERE Name=''Version''';
    FQuery.ExecSQL;
  finally
    lVersion.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Unzip and merge the supplied zip file into the main database.
}
procedure TDBUpgrader.MergeZipFile(const AFileName: string);
var
  lDBMerger: TDBMerger;
  lstTempFile : String;
  lVCLUnzip : TVclUnzip;
  mdbFile: string;
begin
  // create dmDatabase on demand for merging
  CreateDmDatabaseIfNeeded;
  if Assigned(FMergeProgressBar) then
    FMergeProgressBar.Visible := True;
  if Assigned(FMergeLabel) then
    FMergeLabel.Visible := True;
  lVCLUnzip := TVCLUnzip.Create(nil);
  try
    lVCLUnzip.ZipName := ExtractFilePath(Application.ExeName) + 'Scripts\' +
                                         AFileName;
    lVCLUnzip.DestDir := GetWindowsTempDir;
    lVCLUnzip.OverwriteMode := Always;
    lVCLUnzip.ReadZip;
    lstTempFile := lVCLUnzip.FileName[0];
    lVCLUnzip.DoAll := True;
    mdbFile := IncludeTrailingPathDelimiter(lVCLUnzip.DestDir) + lstTempFile;
    try
      lVCLUnzip.Unzip;
      lDBMerger := TDBMerger.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source='
          + mdbFile,
            SetProgress, SetStatus);
      try
        // Have the log where Recorder is installed.
        lDBMerger.DefaultOutputPath :=
            GetProgramDataFolder(FSettings.InstallationPath, PATH_USER_FILES);
        lDBMerger.DoMerge(nil, nil, nil, FThisFileRecordCount);
      finally
        lDBMerger.Free;
      end;
    finally
      if not DeleteFile(PChar(IncludeTrailingPathDelimiter(lVCLUnzip.DestDir) + lstTempFile)) then begin
        raise EDBUpgraderError.Create(Format(ResStr_CannotDeleteFile, [mdbFile]));
      end;
    end;
  finally
    lVCLUnzip.Free;
    if Assigned(FMergeProgressBar) then
      FMergeProgressBar.Visible := false;
    if Assigned(FMergeLabel) then
      FMergeLabel.Visible := false;
  end; // try
end;

{-------------------------------------------------------------------------------
  Callback used to set progress during merge of a zip file
}
procedure TDBUpgrader.SetProgress(const iProgress: Integer; iPrcMsgs: Boolean = true);
begin
  if Assigned(FMergeProgressBar) then begin
    FMergeProgressBar.Position := iProgress;
    FMergeProgressBar.Refresh;
  end;
end;

{-------------------------------------------------------------------------------
  Callback used to set progress label during merge of a zip file
}
procedure TDBUpgrader.SetStatus(const iStatusString: String; iPrcMsgs: Boolean
    = true);
begin
  if Assigned(FMergeLabel) then begin
    FMergeLabel.Caption := iStatusString;
    FMergeLabel.Refresh;
  end;
end;

{-------------------------------------------------------------------------------
  If not already done, instantiates dmDatabase
}
procedure TDBUpgrader.CreateDmDatabaseIfNeeded;
begin
  if not Assigned(dmDatabase) then
    dmDatabase := TdmDatabase.Create(nil, FSettings.ServerName, FSettings.DatabaseName,
        FSettings.TrustedSecurity, False, FSettings.Username, FSettings.Password);
end;

{-------------------------------------------------------------------------------
  If an ini file exists in the list of files being processed, check it
     for update descriptions and also record counts.
}
procedure TDBUpgrader.HandleMetadataIniFile(const AFileName: string);
var
  lIniFile: TStringList;
  lIndex: integer;
  i: integer;
begin
  lIniFile := TStringList.Create;
  try
    lIniFile.LoadFromFile(AFileName);
    lIndex := lIniFile.IndexOf('[RecCount]');
    if lIndex<>-1 then
      // Next line contains a record count
      FThisFileRecordCount := StrToInt(lIniFile[lIndex+1]);
    lIndex := lIniFile.IndexOf('[Update Index_Taxon_Name]');
    if lIndex<>-1 then
      FTaxonNameIndexRebuild := true;
    lIndex := lIniFile.IndexOf('[Update Index_Taxon_Group]');
    if lIndex<>-1 then
      FTaxonGroupIndexRebuild := true;
    lIndex := lIniFile.IndexOf('[Update Index_Taxon_Synonym]');
    if lIndex<>-1 then
      FTaxonSynonymIndexRebuild := true;
    lIndex := lIniFile.IndexOf('[Description]');
    if lIndex<>-1 then begin
      for i := lIndex+1 to lIniFile.Count-1 do begin
        if Copy(lIniFile[i],1,1)='[' then
          break; // next tag found so description finished
        FUpdateDescriptions.Add(lIniFile[i]);
      end;
      FUpdateDescriptions.Add('');
    end;
  finally
    lIniFile.Free;
  end;
end;

{-------------------------------------------------------------------------------
  If a flag is set, update the appropriate taxon dictionary index tables.
    This can be prompted by ini files in the sequenced scripts folder.
}
procedure TDBUpgrader.UpdateTaxonIndices;
var
  lQuery: TADOQuery;
begin
  if FTaxonNameIndexRebuild or FTaxonGroupIndexRebuild or FTaxonSynonymIndexRebuild then begin
    lQuery := TADOQuery.Create(nil);
    CreateDmDatabaseIfNeeded;
    FMergeProgressBar.Visible := true;
    FMergeLabel.Visible := true;
    try
      lQuery.Connection := dmDatabase.Connection;
      lQuery.CommandTimeout := 0;
      if FTaxonNameIndexRebuild then
        DatabaseUtilities.RebuildIndexTaxonName(lQuery, SetStatus, SetProgress);
      if FTaxonSynonymIndexRebuild then
        DatabaseUtilities.RebuildIndexTaxonSynonym(lQuery, SetStatus, SetProgress);
      if FTaxonGroupIndexRebuild then begin
        DatabaseUtilities.ClearSystemTaxonGroupIndex(lQuery);
        DatabaseUtilities.PopulateTaxonGroupIndex(lQuery, SetStatus, SetProgress);
      end;
    finally
      lQuery.Free;
      FMergeProgressBar.Visible := false;
      FMergeLabel.Visible := false;
    end;
  end;
end;

end.
