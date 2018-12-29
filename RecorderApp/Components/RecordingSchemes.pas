//==============================================================================
//  Unit:        RecordingSchemes
//
//  Implements:  TSchemeManager
//
//  Description: Implements a class that manages the list of recording schemes
//               available.  Also undertakes the task of creating a file for
//               contribution to a scheme.
//
//  Author:      John van Breda
//  Created:     21 Feb 2002
//
//  Changes:     Eric Salmon 13/06/02
//               Database export.
//
//  Last Revision Details:
//    $Revision: 26 $
//    $Date: 27/05/08 10:31 $
//    $Author: Johnvanbreda $
//
//==============================================================================
unit RecordingSchemes;

interface

uses
  Sysutils, Classes, Menus, Windows, ExceptionForm, DataClasses, Dialogs, Controls,
  MapiDLLFunctions, Forms;

type
  ESchemeManager = class(TExceptionPath);

  TSchemeManager = class
  private
    FContainerMenu : TMenuItem;
    FTempDir       : string;
    FEmailFailed   : boolean;
    procedure EmailSchemeContributionTo( const iSchemeName, iEmail : string;
          iRecordsSince : TDateTime; const iZipPath : string );
    procedure CreateContributionXML(iKeyList, iKeyListFilter: TEditableKeyList);
    procedure ExportFileToScheme(const iSchemeKey, iSchemeName, iFilterKey,
      iEmail: string; const iLastContribution: TDateTime; const AExportFormat: Integer);
    procedure ZipXML;
    procedure UpdateContributionDate( const iSchemeKey : string; iDate : TDateTime );
    procedure CreateContributionZIP(AKeyList, AKeyListFilter: TEditableKeyList);
  protected
  public
    constructor Create(iContainerMenu : TMenuItem);
    procedure UpdateRecordingSchemes;
    procedure ContributeToScheme(Sender: TObject);
  end;

//==============================================================================
implementation

uses
  GeneralData, GeneralFunctions, ApplicationSettings, VCLUnZip, VCLZip,
  DataOutput, DatabaseOutput, MainTBar, ContributionProgress, Db, FileCtrl,
  ExportFilters, DatabaseAccessADO, FormActions, ExternalFilter;

const
  ST_ZIP_FILENAME = 'NBNRecords.zip';
  ST_XML_FILENAME = 'NBNRecords.xml';
  ST_MDB_FILENAME = 'NBNRecords.mdb';

resourcestring
  ResStr_ManualEmail =  'A file has been created for your contribution to the Recording Scheme. ' +
                        'Please email the file at %s to %s';

  ResStr_ContrCallIncorrect = 'Contribute to scheme called incorrectly';
  ResStr_IdentifyExportRec  = 'Identifying records to export...';
  ResStr_RecSchemeRemoved   = 'Recording scheme has been removed from the database by another user';

  ResStr_SchemeUsedFirstTime= 'This is the first time this Scheme is used.  This means the whole ' +
                              'filter will be exported '#13#10'and this may take a while, but ' +
                              'you can choose to only contribute today''s data instead.'#13#10#13#10 +
                              'If you wish to only export today''s data, click <Yes>, otherwise click <No>.';

  ResStr_RecordsAddedEdited = 'Records extracted for %s';
  ResStr_RecordsSent        = 'Records added or edited since %s have been sent to %s';
  ResStr_CreatingNBNExportFile = 'Creating NBN Export file...';
  ResStr_InvalidExpFormat =   'Invalid Export Format (%s).';
  ResStr_CompEmailExportFile =  'Compressing and emailing export file...';

  ResStr_FileDetails =  'File Created on %s at %s '#13#10 +
                        'by User ID %s (%s)'#13#10 +
                        'containing records entered since %s';

  ResStr_CannotEmailContFile =  'Failed to email the contribution file.'#13#10 +
                                'Please send the file in %s to %s.'#13#10 +
                                'The error is described as %s.';

  ResStr_RecordingSchemeCannotUpdate =  'The Recording Schemes list cannot be updated';

//==============================================================================
{ Constructor - performs initial task of setting up the submenu }
constructor TSchemeManager.Create(iContainerMenu: TMenuItem);
begin
  inherited Create;
  FContainerMenu := iContainerMenu;
  UpdateRecordingSchemes;
  FTempDir := GetWindowsTempDir;
end;

//==============================================================================
{ Event handler for dynamically built Contribute to ... menu items }
procedure TSchemeManager.ContributeToScheme(Sender: TObject);
var
  lstSchemeName      : String;
  lstFilter          : String;
  lstEmail           : String;
  ldtLastContribution: TDateTime;
  liExportFormat     : Integer;
  ltfContinue        : Boolean;
  liAnswer           : Integer;
  confirmation       : string;
begin
  if not (Sender is TComponent) then
    raise ESchemeManager.Create(ResStr_ContrCallIncorrect);
  frmContributionProgress := TfrmContributionProgress.Create(nil);
  frmContributionProgress.Stage := ResStr_IdentifyExportRec;
  frmContributionProgress.Show;
  Application.ProcessMessages;

  ltfContinue := true;
  ldtLastContribution := 0;
  liExportFormat := 0;
  try
    with dmGeneralData.qryAllPurpose do begin
      SQL.Text := 'SELECT Item_Name, Email, Export_Filter_Key, Last_Contribution_Date, Export_Format_ID ' +
                  'FROM Recording_Scheme ' +
                  'WHERE Recording_Scheme_Key=''' + TKey(Ptr(TComponent(Sender).Tag)).Key + '''';
      Open;
      try
        if RecordCount=0 then begin
          UpdateRecordingSchemes;
          raise ESchemeManager.CreateNonCritical(ResStr_RecSchemeRemoved );
        end;
        lstSchemeName       := FieldByName('Item_Name').AsString;
        frmContributionProgress.Scheme := lstSchemeName;
        lstFilter           := FieldByName('Export_Filter_Key').AsString;
        lstEmail            := FieldByName('Email').AsString;
        ldtLastContribution := FieldByName('Last_Contribution_Date').AsDateTime;
        liExportFormat      := FieldByName('Export_Format_Id').AsInteger;
      finally
        Close;
      end;
    end; // with qryAllPurpose
    // If first time, ask if everything should be processed or not.
    if (ldtLastContribution = 0) and (lstFilter <> '') then begin
      liAnswer := MessageDlg(ResStr_SchemeUsedFirstTime,mtConfirmation, [mbYes, MbNo, mbCancel], 0);
      if liAnswer = mrCancel then
        ltfContinue := false
      else if liAnswer = mrYes then
        ldtLastContribution := Date;
    end;

    if ltfContinue then
      try
        ExportFileToScheme(TKey(Ptr(TComponent(Sender).Tag)).Key, lstSchemeName,
                           lstFilter, lstEmail, ldtLastContribution, liExportFormat);
        frmMain.TaskFinished;
        if (not (frmContributionProgress.Cancelled or FEmailFailed)) and
           AppSettings.AutoSchemeEmail then
        begin
          if ldtLastContribution > 0 then
             confirmation := Format(ResStr_RecordsSent, [DateTimeToStr(ldtLastContribution), lstSchemeName])
          else
             confirmation := Format(ResStr_RecordsAddedEdited, [lstSchemeName]);
          MessageDlg(confirmation, mtInformation, [mbOk], 0);
        end;
      except
        ltfContinue := false;
        raise;
      end;
  finally
    // If Contribution was wanted, update the date in table.
    if ltfContinue and (not frmContributionProgress.Cancelled) then
      UpdateContributionDate(TKey(Ptr(TComponent(Sender).Tag)).Key, Now);

    frmMain.TaskFinished; // in case cancel clicked
    frmContributionProgress.Free;
    frmContributionProgress := nil;
  end;
end;

//==============================================================================
{ Creates an export file to send to a recording scheme.  The file can be filtered
    using an Export Filter, and picks up all changed observations since
    a given date time.  Zips the file, then attaches it to an email which is
    added to the Outbox }
procedure TSchemeManager.ExportFileToScheme( const iSchemeKey, iSchemeName, iFilterKey, iEmail : string;
  const iLastContribution : TDateTime; const AExportFormat: Integer);
var
  lFilter       : TExportFilter;
  lKeyList      : TEditableKeyList;
  lOldDateFormat: String;
  lstZipToSend  : String;

  lKeyListFilter: TEditableKeyList;
  lValidationMessages: TStringList;
begin
  if iFilterKey = '' then lFilter := TExportFilter.CreateNew('tempFilter')
                     else lFilter := TExportFilter.CreateFromDatabase(iFilterKey);
  try
    lKeyList := lFilter.KeyList(iLastContribution);
  finally
    lFilter.Free;
  end;

  lKeyListFilter := TEditableKeyList.Create();
  lKeyListFilter.SetTable(MIXED_DATA);
  lValidationMessages := TStringList.Create;
  try
    dmFormActions.RevalidateNBNData(lKeyList, lKeyListFilter, lValidationMessages);

    if (lKeyListFilter.Header.ItemCount = 0) or
       (ConfirmYesNo(ResStr_ConfirmExportWithoutInvalidRecords) = mrYes) then
    begin
      try
        frmContributionProgress.Stage := ResStr_CreatingNBNExportFile;
        case AExportFormat of
          1 : CreateContributionXML(lKeyList, lKeyListFilter);
          2 : CreateContributionZIP(lKeyList, lKeyListFilter);
          else
            raise ESchemeManager.Create(Format(ResStr_InvalidExpFormat,[IntToStr(AExportFormat)]));
        end;
      finally
        lKeyList.Free;
      end;

      // Store user's date format, since we are going to force ISO dates for the file name
      lOldDateFormat := ShortDateFormat;
      ShortDateFormat := 'yyyymmdd';
      lstZipToSend := FTempDir + 'NBNRecords_' + AppSettings.SiteID + '_' + DateToStr(Now) + '.zip';
      if FileExists(lstZipToSend) then Sysutils.DeleteFile(lstZipToSend);
      // Rename ZIP to include SiteID and Date
      RenameFile(FTempDir + ST_ZIP_FILENAME, lstZipToSend);
      ShortDateFormat := lOldDateFormat;
      try
        EmailSchemeContributionTo(iSchemeName, iEmail, iLastContribution, lstZipToSend);
      finally
        { Cleanup files }
        if AExportFormat = 1 then
          Sysutils.DeleteFile(FTempDir + ST_XML_FILENAME)
        else
          Sysutils.DeleteFile(FTempDir + ST_MDB_FILENAME);
      end;
    end;

    // Regardless of cancelling the export, show any invalid records.
    if lKeyListFilter.Header.ItemCount > 0 then
      with TListFilter.Create do
        try
          LoadFilter(lKeyListFilter, lValidationMessages);
          ApplyFilter;
        finally
          Free;
        end;
  finally
    lKeyListFilter.Free;
    lValidationMessages.Free;
  end;
end;  // ExportFileToScheme

//==============================================================================
procedure TSchemeManager.CreateContributionXML(iKeyList, iKeyListFilter: TEditableKeyList);
var
  lDataOutput : TDataOutput;
begin
  lDataOutput := TDataOutput.Create(FTempDir + ST_XML_FILENAME);
  try
    frmContributionProgress.DataOutput := lDataOutput;
    lDataOutput.Execute(iKeyList, iKeyListFilter, true, True);
    frmContributionProgress.DataOutput := nil;
  finally
    lDataOutput.Free;
  end; // finally
  // Zip the XML file
  ZipXML;
end;  // CreateContributionXML

//==============================================================================
{ Convert the XML file to a zip file.}
procedure TSchemeManager.ZipXML;
var lZipTool : TVCLZip;
begin
  if frmContributionProgress.Cancelled then Exit;

  frmContributionProgress.Stage := ResStr_CompEmailExportFile;
  lZipTool := TVCLZip.Create(nil);
  try
    with lZipTool do begin
      // Store user's date format, since we are going to force ISO dates for the file name
      ZipName := FTempDir + ST_ZIP_FILENAME;
      FilesList.Add(FTempDir + ST_XML_FILENAME);
      Recurse    := False;
      StorePaths := False;
      PackLevel  := 9;
      Zip;
    end;
  finally
    lZipTool.Free;
  end;
end;  // ZipXML

//==============================================================================
procedure TSchemeManager.CreateContributionZIP(AKeyList, AKeyListFilter: TEditableKeyList);
var lDBOutput: TDatabaseOutput;
begin
   // Create a zipped export database
   lDBOutput := TDatabaseOutput.Create(dmDatabase, FTempDir + ST_ZIP_FILENAME,
                                       frmMain.SetStatus, frmMain.ProgressBar);
   try
     lDBOutput.UserAccessLevel := Integer(AppSettings.UserAccessLevel);
     lDBOutput.ExportConfidentialOccurrences := AppSettings.ExportConfidentialOccurrences;
     lDBOutput.CanExportSystemSupplied := AppSettings.CanExportSystemSupplied;
     lDBOutput.UsingExportFilter := true; // so we get unchecked, invalid etc.
     frmContributionProgress.DatabaseOutput := lDBOutput;
     // Mantis 343
     lDBOutput.Execute(AKeyList, AKeyListFilter, True,False,False,'');
     frmContributionProgress.DatabaseOutput := nil;
   finally
     lDBOutput.Free;
   end;
end;  // CreateContributionZIP

//==============================================================================
{ Email the zipped file to a given address }
procedure TSchemeManager.EmailSchemeContributionTo(const iSchemeName, iEmail: string;
          iRecordsSince : TDateTime; const iZipPath : string);
begin
  if frmContributionProgress.Cancelled then
    Exit;
  if AppSettings.AutoSchemeEmail then begin
    frmContributionProgress.Hide;
    FEmailFailed := False;
    try
      MapiDLLSendMail(iSchemeName + ' contribution',
                    Format(ResStr_FileDetails, [DateToStr(now), TimeToStr(Now), AppSettings.UserID, dmGeneralData.GetName(AppSettings.UserID), DateTimeToStr(iRecordsSince)]),
                    [iEmail],  [iZipPath], [soLogon, soDialog],
                    Application.Handle);
      Sysutils.DeleteFile(iZipPath);  // if succesfully sent, then delete it                    
    except
      on E:Exception do begin
        ShowInformation(Format(ResStr_CannotEmailContFile, [iZipPath, iEmail, E.Message]));
        FEmailFailed := True;
      end;
    end;
  end
  else begin
    // ensure we have an output directory to move the contribution file into.
    ForceDirectories(ExtractFilePath(Application.Exename)+'Output');
    // copy the contribution file into the output folder so the user can manually email
    CopyFile(PChar(iZipPath), PChar(ExtractFilePath(Application.Exename)+'Output\'
                               + ExtractFileName(iZipPath)), False);
    MessageDlg(Format(ResStr_ManualEmail, [ExtractFilePath(Application.Exename)
               +'Output\' + ExtractFileName(iZipPath), iEmail]), mtInformation, [mbOk], 0);
    Sysutils.DeleteFile(iZipPath);  // if succesfully sent, then delete it
  end;
end;

//==============================================================================
{ List Recording schemes on the main menu according to those available }
procedure TSchemeManager.UpdateRecordingSchemes;
var
  i : integer;
  lItemKey : TKey;
begin
  { First clear the exsiting menu items, except Manage item  }
  for i := FContainerMenu.Count - 1 downto 0 do begin
    TKey(FContainerMenu.Items[i].Tag).Free;
    FContainerMenu.Delete(i);
  end;
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'SELECT RECORDING_SCHEME_KEY, ITEM_NAME FROM RECORDING_SCHEME ORDER BY ITEM_NAME DESC';
    try
      Open;
      try
        {if RecordCount > 0 then begin // add separator
          FContainerMenu.Insert(0, TMenuItem.Create(FContainerMenu));
          FContainerMenu.items[0].Caption := '-';
        end;} //No longer necessary as these now form their own sub menu
        FContainerMenu.Visible := RecordCount > 0;
        while not EOF do begin // add schemes
          lItemKey := TKey.Create;
          lItemKey.Key := FieldByName('RECORDING_SCHEME_KEY').AsString;
          FContainerMenu.Insert(0, TMenuItem.Create(FContainerMenu));
          with FContainerMenu.items[0] do begin
            Caption := FieldByName('ITEM_NAME').AsString;
            Tag := Integer(lItemKey);
            OnClick := ContributeToScheme;
          end;
          Next;
        end;
      finally
        Close; // qryAllPurpose
      end;
    except
      on EDatabaseError do
        // This allows Rec2K to run, with warnings, againts old db versions
        MessageDlg(ResStr_RecordingSchemeCannotUpdate, mtWarning, [mbOk], 0);
    end;
  end;
end;

//==============================================================================
{ Update the contribution date for the scheme, as we have made a new
         contribution }
procedure TSchemeManager.UpdateContributionDate(const iSchemeKey: string;
  iDate: TDateTime);
begin
  dmDatabase.ExecuteSQL('UPDATE RECORDING_SCHEME ' +
                'SET LAST_CONTRIBUTION_DATE = ''' + FormatDateTime('dd mmm yyyy', iDate) + '''' +
                'Where RECORDING_SCHEME_KEY = ''' + iSchemeKey + '''', False);
end;

//==============================================================================
end.
