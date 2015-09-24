{===============================================================================
  Unit:        MigratePage

  Defines:     TfraMigrate

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 12 $
    $Date: 6/03/09 11:50 $
    $Author: Pauldavies $

===============================================================================}

unit MigratePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  BasePage, StdCtrls, ExtCtrls, ComCtrls, DBMigrator, InstallFiles, ADODB, MS5,
  DatabaseUtilities, DatabaseAccessADO, ExceptionForm;

type
  TMigrateException = class(TExceptionPath);

  TfraMigrate = class(TBasePage)
    Animation: TAnimate;
    lblCancel: TLabel;
    lblCurrentTable: TLabel;
    lblErrors: TLabel;
    lblFile: TLabel;
    lblInstallStep: TLabel;
    lblOverall: TLabel;
    lblRecordsOutOf: TLabel;
    lblSub: TLabel;
    lblTotalRecords: TLabel;
    pbOverall: TProgressBar;
    pbProgress: TProgressBar;
    pnlControls: TPanel;
  private
    FdmDatabase: TdmDatabase;
    FInstallFiles: TInstallFiles;
    FMigrator: TDBMigrator;
    FProcessedTableName: String;
    procedure CleanDatabaseProgress(AValue: Integer);
    procedure CopyFiles;
    procedure FixMapDatasetFile(AConnection: TADOConnection);
    function GetComputerID: String;
    function GetLongSheetName(const AFileName: string): string;
    function InstallCleanDatabase: Boolean;
    procedure LabelCallback(ALabelType: TLabelType; const ACaption: string);
    procedure OpenLocalDatabase;
    procedure UpdateMapTables(AConnection: TADOConnection);
  protected
    function GetNext: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
  public
    procedure Cancel; override;
    function Execute: Boolean; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  DatabaseServerSelectPage, MigrateComplete, Settings, Functions, SetupConstants,
  GeneralFunctions, AttachDB, MigrationSettings, ApiUtils, TextMessages;

{-==============================================================================
    TfraMigrate
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMigrate.Cancel;
begin
  inherited;
  lblCancel.Visible := True;
  Refresh;
  
  // Stop data migration.
  if Assigned(FMigrator) then
    FMigrator.Cancel;
  
  // Stop copying files now.
  if Assigned(FInstallFiles) then
    FInstallFiles.Cancel;
end;  // TfraMigrate.Cancel 

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.CleanDatabaseProgress(AValue: Integer);
begin
  pbProgress.Position := AValue;
  Application.ProcessMessages;
end;  // TfraMigrate.CleanDatabaseProgress 

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.CopyFiles;
var
  lInstallFolder: String;
begin
  Animation.Visible := True;
  Animation.Enabled := True;
  lblFile.Visible   := True;
  lblTotalRecords.Visible := False;
  lblCurrentTable.Visible := False;
  lblSub.Visible          := False;
  lblRecordsOutOf.Visible := False;

  pbProgress.Position := 0;
  pbProgress.Visible  := True;
  lblOverall.Visible  := True;
  pbOverall.Visible   := True;
  Refresh;

  // Don't want anything in Settings.InstallFolder during file transfer.
  lInstallFolder := Settings.InstallFolder;
  Settings.InstallFolder := '';

  FInstallFiles := TInstallFiles.Create(Settings, lblFile, pbProgress, pbOverall);
  try
    with TMigrationSettings(Settings) do begin
      // Need to access database to cleanup Map tables.
      OpenLocalDatabase;

      // Map_Sheet table already migrated. If files are not, clean it up.
      if not MigrateMapFiles or not MigrateObjectSheets then begin
        // Not transfering any map files, remove all from Map_Sheet that don't have a
        // Base_Map_Key, they are the new ones that should disappear.
        if not MigrateMapFiles and not MigrateObjectSheets then
          FdmDatabase.ExecuteSQL('DELETE Map_Sheet WHERE Base_Map_Key IS NULL')
        else begin
          // Not migrating map files, remove them.
          if not MigrateMapFiles then
            FdmDatabase.ExecuteSQL(
                'DELETE Map_Sheet WHERE Base_Map_Key IS NULL AND Sheet_Type = 2');
          // Not migrating object sheets, remove them.
          if not MigrateObjectSheets then
            FdmDatabase.ExecuteSQL(
                'DELETE Map_Sheet WHERE Base_Map_Key IS NULL AND Sheet_Type = 3');
        end;
      end;
  
      // Now proceed with what needs to be transfered.
      if MigrateMapFiles then
        FInstallFiles.GetFilesIn(MigrationMapFilesPath, RecorderMapFilesPath);
      if MigrateObjectSheets then
        FInstallFiles.GetFilesIn(MigrationObjectSheetsPath, RecorderObjectSheetsPath);
      UpdateMapTables(FdmDatabase.Connection);

      if MigratePolygonFilters then
        FInstallFiles.GetFilesIn(MigrationPolygonFiltersPath, RecorderPolygonFiltersPath);
      if MigrateRucksacks then
        FInstallFiles.GetFilesIn(MigrationRucksacksPath, RecorderRucksacksPath);
      if MigrateCards then
        FInstallFiles.GetFilesIn(MigrationCardsPath, RecorderCardsPath);
      if MigrateTemplates then
        FInstallFiles.GetFilesIn(MigrationTemplatesPath, RecorderTemplatesPath);
      if MigrateImages then
        FInstallFiles.GetFilesIn(MigrationImagesPath, RecorderImagesPath);
    end;
    FInstallFiles.CopyFiles;
    FixMapDatasetFile(FdmDatabase.Connection);
  finally
    FreeAndNil(FInstallFiles);
    Settings.InstallFolder := lInstallFolder;
  end;
  Application.ProcessMessages;
end;  // TfraMigrate.CopyFiles

{-------------------------------------------------------------------------------
}
function TfraMigrate.Execute: Boolean;
var
  lCursor: TCursor;
  lPath: String;
  lQuery: TADOQuery;
begin
  Result := False;
  lblFile.Visible    := False;
  lblOverall.Visible := False;
  pbOverall.Visible  := False;
  lblTotalRecords.Visible := False;
  lblCurrentTable.Visible := False;
  lblRecordsOutOf.Visible := False;

  if InstallCleanDatabase then begin
    lCursor := HourglassCursor;
    try
      // Transfering database content.
      CheckServices;

      Animation.Visible := True;
      Animation.Enabled := True;
      lblTotalRecords.Visible := True;
      lblCurrentTable.Visible := True;
      lblRecordsOutOf.Visible := True;

      lblInstallStep.Caption := ResStr_MigratingDataFromAccess;
      try
        if Settings.InstallType = itWorkstation then lPath := 'Workstation Setup\'
                                                else lPath := 'System\';
        with TMigrationSettings(Settings) do
          FMigrator := TDBMigrator.Create(LabelCallback, pbProgress, ServerName,
                                          MigrationAccessDBPath + '\' + ACCESS_MAIN_DB,
                                          MigrationAccessDBPassword,
                                          TrustedLogin, Username, Password,
                                          RootFolder + lPath);
        try
          TMigrationSettings(Settings).MigrationErrors := not FMigrator.Execute;

          // All theses are unnecessary for the next bit.
          pbProgress.Position := 0;
          lblTotalRecords.Visible := False;
          lblCurrentTable.Visible := False;
          lblRecordsOutOf.Visible := False;
          lblErrors.Visible       := False;
          lQuery := TADOQuery.Create(nil);
          try
            OpenLocalDatabase;
            lQuery.Connection := FdmDatabase.Connection;
            lQuery.CommandTimeout := 0;
          finally
            lQuery.Free;
          end;
        finally
          FreeAndNil(FMigrator);
        end; //try
      except
        // Will break if can't login using given username/password.
        on E:Exception do begin
          MessageDlg(E.Message, mtError, [mbOk], 0);
          DefaultCursor(lCursor);
          // For workstation, the page will go through to Workstation's version or TfraLogin.
          // But makes code a bit simpler than sprinkling tests everywhere.
          ForceNextPage(TfraDatabaseServerSelect);
        end;
      end;
      // Copying files.
      if lblCancel.Visible then Exit;
      CopyFiles;
      Result := True;
    finally
      DefaultCursor(lCursor);
    end;
  end;
end;  // TfraMigrate.Execute

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.FixMapDatasetFile(AConnection: TADOConnection);
var
  lBaseMapPath, lMapFilePath, lObjectSheetPath, lFileName: String;
  lMapDatasetFileName: String;
  lMapHandle: HWnd;
  i, lTotalSheets: Integer;
  lScale: Double;
  lOrigin: MSCoord;
  lPanel: TPanel;
begin
  with TMigrationSettings(Settings) do begin
    lBaseMapPath := IncludeTrailingPathDelimiter(RecorderBaseMapPath);
    lMapFilePath := IncludeTrailingPathDelimiter(RecorderMapFilesPath);
    lObjectSheetPath := IncludeTrailingPathDelimiter(RecorderObjectSheetsPath);
  end;

  // Only do something if there is a reason to do it.
  if not FileExists(lMapFilePath + 'R2KMapDataset.gds') then Exit;

  // Get the installed maps for this machine, ordered to get default one first, if any.
  // There should be only one there anyway.
  with AConnection.Execute(Format(
      'SELECT * FROM Computer_Map WHERE COMPUTER_ID=''%s'' ORDER BY Default_Map DESC',
      [GetComputerID])) do
  begin
    if Eof then begin
      // No maps in database, file must be messed up left over. Remove it.
      DeleteFile(PChar(lMapFilePath + 'R2KMapDataset.gds'));
      lMapDatasetFileName := '';
    end else begin
      lMapDatasetFileName := lMapFilePath + Fields['Base_Map_Key'].Value + '.gds';
      // Map there, use Base_Map_Key to rename file.
      if FileExists(lMapDatasetFileName) then DeleteFile(PChar(lMapDatasetFileName));
      RenameFile(lMapFilePath + 'R2KMapDataset.gds', lMapDatasetFileName);
    end;
    Close;
  end;

  // Nothing else to do. Leave now.
  if lMapDatasetFileName = '' then Exit;

  // Now do the same on Map_sheet for map files. Don't touch object sheets, base maps already done
  with AConnection.Execute(
      'SELECT Map_Sheet_Key, File_Name, Dataset_Sheet_FileName, Sheet_Type, Dataset_Sheet_Name '
      + 'FROM Map_Sheet WHERE Sheet_Type not in (3,0)') do
    try
      while not Eof do begin
        lFileName := QuotedStr(ExpandLongPathName(
            lMapFilePath + ExtractFileName(Fields['File_Name'].Value)));
        AConnection.Execute(Format(
            'UPDATE Map_Sheet SET File_Name = %s WHERE Map_Sheet_Key = ''%s''',
            [lFileName, Fields['Map_Sheet_Key'].Value]));
        MoveNext;
      end;
    finally
      Close;
    end;
  
  // The only thing left to do is update the content of the map dataset file itself.
  lPanel := TPanel.Create(nil);
  try
    lPanel.Parent := Application.MainForm;
    lPanel.Visible := False;
    lMapHandle := MapCreateNewWindow(lPanel.Handle, 0, 0, lPanel.Width, lPanel.Height);
    try
      MapSelectDataset(lMapHandle, PChar(lMapDatasetFileName));
      MapGetViewOrigin(lMapHandle, lOrigin.X, lOrigin.Y);
      MapGetScale(lMapHandle, lScale);
      MapGetNumSheets(lMapHandle, lTotalSheets);
      for i := lTotalSheets - 1 downto 0 do
        MapDetachSheet(lMapHandle, i);

      with AConnection.Execute(
          'SELECT Dataset_Sheet_FileName, Sheet_Type FROM Map_Sheet '
          + 'ORDER BY Dataset_Sheet_Order') do
        try
          while not Eof do begin
            lFileName := Fields['Dataset_Sheet_FileName'].Value;
            if Fields['Sheet_Type'].Value = 0 then
              lFileName := lBaseMapPath + lFileName
            else
            if Fields['Sheet_Type'].Value = 3 then
              lFileName := lObjectSheetPath + lFileName;
  
            MapAttachSheet(lMapHandle, PChar(lFileName));
  
            // Set query mode to appropriate.
            MapGetNumSheets(lMapHandle, lTotalSheets);
            MapSetQuerySheet(lMapHandle, lTotalSheets - 1, Fields['Sheet_Type'].Value =
                3);
            MoveNext;
          end;
        finally
          Close;
        end;
  
      MapSetScale(lMapHandle, lScale);
      MapSetViewOrigin(lMapHandle, lOrigin.X, lOrigin.Y);
    finally
      MapCloseData(lMapHandle);
      MapDestroyWindow(lMapHandle);
    end;
  finally
    lPanel.Free;
  end;
end;  // TfraMigrate.FixMapDatasetFile 

{ Use the API to obtain a unique identifier for the computer }
function TfraMigrate.GetComputerID: String;
var
  len: DWORD;
begin
  len := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength (Result, len);
  gpcApiCheck(Windows.GetComputerName (PChar (Result), len));
  SetLength (Result, len);
end;

{-------------------------------------------------------------------------------
  Convert a sheet file name to a long sheet file name, as Recorder 2002
    sheet filenames are 8.3
}
function TfraMigrate.GetLongSheetName(const AFileName: string): string;
begin
  Result := ExtractFileName(ExpandLongPathName(AFileName));
end;

{-------------------------------------------------------------------------------
}
function TfraMigrate.GetNext: TBasePageClass;
begin
  Result := TfraMigrateComplete;
end;  // TfraMigrate.GetNext 

{-------------------------------------------------------------------------------
}
function TfraMigrate.GetResourceImage: String;
begin
  Result := ResImg_Migrate;
end;  // TfraMigrate.GetResourceImage 

{-------------------------------------------------------------------------------
}
function TfraMigrate.InstallCleanDatabase: Boolean;
var
  lAttachDB: TAttachDB;
begin
  Result := False;
  lAttachDB := TAttachDB.Create(Settings.ServerName, Settings.RegistryInstanceName, CleanDatabaseProgress,
                                Settings.UserName, Settings.Password,
                                Settings.TrustedLogin);
  try
    try
      lblInstallStep.Caption := ResStr_StartingSQLExpress;
      if lAttachDB.ConnectAndCheckDb(False) then begin
        lblInstallStep.Caption := ResStr_CreatingDatabaseFile;
        if Settings.InstallType = itWorkstation then
          lAttachDB.CopyMDF(Settings.RootFolder + STR_NBNDATA_ZIP_WKS)
        else
          lAttachDB.CopyMDF(Settings.RootFolder + STR_NBNDATA_ZIP_STD);
        lblInstallStep.Caption := ResStr_ConfiguringDatabase;
        lAttachDB.InitDB(Settings.InstallType = itStandalone, Settings.SiteID);
        Result := True;
      end else begin
        MessageDlg(lAttachDB.ErrorMessage, mtWarning, [mbOk], 0);
        // For workstation, the page will go through to Workstation's version or TfraLogin.
        // But makes code a bit simpler than sprinkling tests everywhere.
        ForceNextPage(TfraDatabaseServerSelect);
      end;
    finally
      lAttachDB.Free;
    end;
  except
    on EAttachAborted do begin
      Application.Terminate;
      Abort;
    end;
  end; // try
end;  // TfraMigrate.InstallCleanDatabase 

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.LabelCallback(ALabelType: TLabelType; const ACaption: string);
var
  lPreviousTotal, lNewTotal, lDifference: Integer;
begin
  case ALabelType of
    ltCurrentTable:  begin
                       lblCurrentTable.Caption := ACaption;
                       FProcessedTableName     := Trim(Copy(ACaption, Pos(':', ACaption) + 1, 255));
                     end;
    ltRecordsCopied: begin
                       // Work out how many records processed since last time.
                       with lblTotalRecords do begin
                         lPreviousTotal := StrToInt(Trim(Copy(Caption, Pos(':', Caption) + 1, 255)));
                         Caption        := ACaption;
                         lNewTotal      := StrToInt(Trim(Copy(Caption, Pos(':', Caption) + 1, 255)));
                         lDifference    := lNewTotal - lPreviousTotal;
                       end;
                       // Refresh to latest value.
                       with TMigrationSettings(Settings) do begin
                         MigratedRecords := IntToStr(lNewTotal);
  
                         // Work out separate values for completion page.
                         if SameText(FProcessedTableName, 'Location') then
                           MigratedLocations := MigratedLocations + lDifference;
                         if SameText(FProcessedTableName, 'Name') then
                           MigratedNames := MigratedNames + lDifference;
                         if SameText(FProcessedTableName, 'Reference') then
                           MigratedReferences := MigratedReferences + lDifference;
                         if SameText(FProcessedTableName, 'Survey') then
                           MigratedSurveys := MigratedSurveys + lDifference;
                         if SameText(FProcessedTableName, 'Taxon Occurrence') then
                           MigratedTaxOccs := MigratedTaxOccs + lDifference;
                         if SameText(FProcessedTableName, 'Biotope Occurrence') then
                           MigratedBioOccs := MigratedBioOccs + lDifference;
                       end;
                     end;
    ltRecordsOutOf:  begin
                       lblRecordsOutOf.Caption := ACaption;
                       lblSub.Visible          := ACaption <> '';
                     end;
    ltErrors:        lblErrors.Caption := ACaption;
  end;
end;  // TfraMigrate.LabelCallback 

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.LoadContent;
begin
  inherited;
  if Settings.OSVersion >= wvVista then begin
    Settings.SetAnimationFromResource(Animation, ResAvi_VistaFileCopy);
    Animation.Active := True;
  end;
  with TMigrationSettings(Settings) do begin
    MigratedRecords    := '';
    MigratedLocations  := 0;
    MigratedNames      := 0;
    MigratedReferences := 0;
    MigratedSurveys    := 0;
    MigratedTaxOccs    := 0;
    MigratedBioOccs    := 0;
  end;
end;  // TfraMigrate.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.UpdateMapTables(AConnection: TADOConnection);
var
  lScript: TStringList;
  lSystem: string;

  function GetSpatialSystem(const AFileName: String): String;
  var
    lFileName: String;
  begin
    lFileName :=
        TMigrationSettings(Settings).RecorderBaseMapPath
        + ExtractWithoutExt(AFileName) + '.ini';
    // Read the .ini file which contains extra information about the map
    if FileExists(lFileName) then begin
      with TStringList.Create do
        try
          LoadFromFile(lFileName);
          try
            Result := Values['system'];
          except
            on EStringListError do;
          end;
        finally
          Free;
        end;
    end else
      Result := '';
  end;

begin
  // This will make certain the Spatial_Ref_System of Base Map records is set
  // Can't be done just by scripting  :-(
  with AConnection.Execute(
      'SELECT Base_Map_Key, File_Name, Dataset_Sheet_Filename, Map_Sheet_Key '
      + 'FROM Map_Sheet WHERE Sheet_Type = 0 AND Spatial_Ref_System IS NULL') do
    try
      // Set Spatial System, plus ensure file names are LONG not 8.3
      while not Eof do begin
        lSystem := GetSpatialSystem(Fields['File_Name'].Value);
        AConnection.Execute(Format(
            'UPDATE Map_Sheet SET Spatial_Ref_System = ''%s'', File_Name = ''%s'','
            + ' Dataset_Sheet_Filename=''%s'' WHERE Map_Sheet_Key = ''%s''',
            [lSystem,
             GetLongSheetName(Fields['File_Name'].Value),
             GetLongSheetName(Fields['Dataset_Sheet_Filename'].Value),
             Fields['Map_Sheet_Key'].Value]));
        MoveNext;
      end;
    finally
      Close;
    end;

  // Need to run script to update ALL map tables now.
  lScript := TStringList.Create;
  try
    if Settings.InstallType = itWorkstation then
      lScript.LoadFromFile(Settings.RootFolder + STR_MAP_SCRIPT_WKS)
    else
      lScript.LoadFromFile(Settings.RootFolder + STR_MAP_SCRIPT_STD);
    AConnection.Execute(lScript.Text);
    if AConnection.Errors.Count<>0 then
      raise TMigrateException.Create(AConnection.Errors[0].Description);
  finally
    lScript.Free;
  end;
end;  // TfraMigrate.UpdateMapTables

{-------------------------------------------------------------------------------
}
procedure TfraMigrate.OpenLocalDatabase;
begin
  with Settings do begin
    if not Assigned(FdmDatabase) then
      FdmDatabase := TdmDatabase.Create(Self, ServerName, 'NBNData', TrustedLogin, False);

    if not TrustedLogin then
      with FdmDatabase do begin
        Connection.Close;
        // Swap nbnuser login for Sa
        Connection.ConnectionString :=
              StringReplace(
                        StringReplace(GetNewConnectionString,
                                      'NBNUser', UserName, [rfReplaceAll]),
                        'NBNPassword', Password, [rfReplaceAll]);
        Connection.Open;
      end;
  end;
end;  // TfraMigrate.OpenLocalDatabase

end.

