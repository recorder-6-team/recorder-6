//==============================================================================
//  Unit:        BatchUpdates
//
//  Implements:  TdlgBatchUpdate, TBatchUpdate
//
//  Description:
//
//  Author:      David Kelly
//  Created:     21 Jan 2008
//
//  Last Revision Details:
//    $Revision: 13 $
//    $Date: 23/04/08 15:03 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit BatchUpdates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseReportSelectUnit, ExtCtrls, StdCtrls, ComCtrls, exgrid, XMLIntf,
  RapTree, FolderBrowser, ImageListButton, CRReportIndex, OnlineHelp, CRReportSQL,
  DB, ADODB, JNCCDatasets, XMLDoc, CRConstants, Constants, CRCommonClasses,
  ExternalFilter, ADOInt, KeyboardRapidTree,ExceptionForm, Map;

resourcestring
  ResStr_BackupDatabase = 'You are about to run an batch update which may affect multiple database records. '+
      'Would you like to backup the database before proceeding? Please note that a backup may take a few minutes.';
  ResStr_CountAndFilter = 'Would you like to review the %s records that will be updated?';
  ResStr_Count = 'The batch update is about to change %s records.' +
      ' Are you sure you want to proceed?';
  ResStr_Filter = 'Would you like to review the records that have been updated?';
  ResStr_EnsureCommitCancel = 'When you have finished reviewing the updated records, ' +
      'please ensure you select Commit Updates or Cancel Updates from the Tools menu.';
  ResStr_NothingToUpdate = 'The Batch Update did not update any records.';
  ResStr_UpdatesCommitted = 'The batch update has been applied.';
  ResStr_UpdatesCancelled = 'All updates have been aborted. No data was updated by the batch update.';

type
  EBatchUpdate = class(TExceptionPath);

  TdlgBatchUpdate = class(TBaseReportSelect)
    qryBatchUpdate: TJNCCQuery;
  protected
    function GetDirectory: string; override;
    function GetHelpContext: Integer; override;
    function GetIndex: TBaseReportIndex; override;
  end;

  TBatchUpdate = class(TObject)
  private
    FBatchUpdateFile: TBatchUpdateFile;
    FFinished: Boolean;
    procedure Cleanup;
    procedure ConfirmAndBackup;
    procedure CreateBatchUpdateConnection;
    procedure HandlePostUpdate;
    procedure NothingToUpdate;
  protected
    procedure FilterUpdate;
  public
    destructor Destroy; override;
    procedure CommitUpdate;
    procedure CancelUpdate;
    procedure RunBatchUpdate(AFileName: String; AKeyType: TKeyType = ktDefault;
        const AItemKey: String = '');
    property Finished: Boolean read FFinished;
  end;

implementation

uses
  ApplicationSettings, Maintbar, DatabaseAccessADO, FormActions,
  GeneralFunctions;

{$R *.dfm}

{-------------------------------------------------------------------------------
}
function TdlgBatchUpdate.GetDirectory: String;
begin
  Result := AppSettings.BatchUpdatePath;
end;
  
{-------------------------------------------------------------------------------
}
function TdlgBatchUpdate.GetIndex: TBaseReportIndex;
begin
  Result := AppSettings.BatchUpdateIndex;
end;
    
{-------------------------------------------------------------------------------
}
function TdlgBatchUpdate.GetHelpContext: Integer;
begin
  Result := IDH_BATCHUPDATES;
end;

{-------------------------------------------------------------------------------
 TBatchUpdate
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
}
destructor TBatchUpdate.Destroy;
begin
  if Assigned(FBatchUpdateFile) then
    FBatchUpdateFile.Free;
  inherited;
end;
  
{-------------------------------------------------------------------------------
  Run the batch update
}
procedure TBatchUpdate.RunBatchUpdate(AFileName: String; AKeyType: TKeyType = ktDefault;
    const AItemKey: String = '');
var
  lXMLDoc: IXMLDocument;
  lDocNode: IXMLNode;
begin
  try
    ConfirmAndBackup;
    lXMLDoc := NewXMLDocument;
    lXMLDoc.LoadFromFile(AFileName);
    lDocNode := lXMLDoc.ChildNodes[EL_BATCHUPDATE];
    CreateBatchUpdateConnection;
    FBatchUpdateFile := TBatchUpdateFile.Create(AppSettings.BatchUpdateConnection);

    // Read the file and run the update.
    with FBatchUpdateFile do begin
      ReadXML(lDocNode);
      KeyType := AKeyType;
      ItemKey := AItemKey;

      // Populate the sample keys in the selected polygon
      if (AKeyType = ktSamplesInPolygon) and
       (frmMain.ActiveMDIChild is TfrmMap) then
            TfrmMap (frmMain.ActiveMDIChild).ReadSamplesInPolygon(
                 AppSettings.BatchUpdateConnection,
                 FBatchUpdateFile.CurrentWhereClause.PartialOverlap);

      // Populate the location keys in the selected polygon
      if (AKeyType = ktLocationsInPolygon) and
        (frmMain.ActiveMDIChild is TfrmMap) then
            TfrmMap (frmMain.ActiveMDIChild).ReadLocationsInPolygon(
                 AppSettings.BatchUpdateConnection,
                 FBatchUpdateFile.CurrentWhereClause.PartialOverlap);

      BuildTable(HandlePostUpdate, CancelUpdate);
    end;
    FFinished := not (AppSettings.HasUncommittedUpdates or FBatchUpdateFile.HasParamsDialog);
  except
    on E:Exception do begin
      Cleanup;
      FFinished := true;
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Callback method if the update isn't cancelled.
  Shows messages depending on the returned recordsets.
}
procedure TBatchUpdate.HandlePostUpdate;
var
  lResult: Integer;
begin
  if Assigned(FBatchUpdateFile.CountRecordset) and
      (FBatchUpdateFile.CountRecordset.Fields['Count'].Value = 0) then
    NothingToUpdate
  else if Assigned(FBatchUpdateFile.CountRecordset) and
      Assigned(FBatchUpdateFile.FilterRecordset) then begin
    lResult := MessageDlg(Format(ResStr_CountAndFilter,
        [VarToStr(FBatchUpdateFile.CountRecordset.Fields['Count'].Value)]),
        mtConfirmation, mbYesNoCancel, 0);
    if lResult = mrYes then FilterUpdate
    else if lResult = mrNo then CommitUpdate
    else CancelUpdate;
  end else if Assigned(FBatchUpdateFile.CountRecordset) then begin
    lResult := MessageDlg(Format(ResStr_Count,
        [VarToStr(FBatchUpdateFile.CountRecordset.Fields['Count'].Value)]),
        mtWarning, [mbOk, mbCancel], 0);
    if lResult = mrOk then CommitUpdate
    else CancelUpdate;
  end else if Assigned(FBatchUpdateFile.FilterRecordset) then begin
    lResult := MessageDlg(ResStr_Filter, mtWarning, [mbYes, mbNo], 0);
    if lResult = mrYes then FilterUpdate
    else CommitUpdate;
  end
  else
    CommitUpdate;
end;

{-------------------------------------------------------------------------------
  Callback method if the update is cancelled.
}
procedure TBatchUpdate.CancelUpdate;
begin
  if AppSettings.BatchUpdateConnection.InTransaction then begin
    AppSettings.BatchUpdateConnection.RollbackTrans;
    ShowInformation(ResStr_UpdatesCancelled);
  end;
  AppSettings.HasUncommittedUpdates := False;
end;

{-------------------------------------------------------------------------------
  Discard the update
}
procedure TBatchUpdate.Cleanup;
begin
  if Assigned(AppSettings.BatchUpdateConnection) then
    if AppSettings.BatchUpdateConnection.InTransaction then
      AppSettings.BatchUpdateConnection.RollbackTrans;
  AppSettings.HasUncommittedUpdates := False;
end;

{-------------------------------------------------------------------------------
  Commits the update
}
procedure TBatchUpdate.CommitUpdate;
begin
  if AppSettings.BatchUpdateConnection.InTransaction then begin
    AppSettings.BatchUpdateConnection.CommitTrans;
    ShowInformation(ResStr_UpdatesCommitted);
  end;
  AppSettings.HasUncommittedUpdates := False;
end;

{-------------------------------------------------------------------------------
  Ask the user if they want to proceed and if they want a backup first.
}
procedure TBatchUpdate.ConfirmAndBackup;
var
  lResult : TModalResult;
begin
  if dmFormActions.actBackup.Visible = True then begin
    lResult := MessageDlg(ResStr_BackupDatabase, mtConfirmation, mbYesNoCancel, 0);
    if lResult = mrYes then begin
      frmMain.SetStatus(ResStr_BackingUpDB);
      Application.ProcessMessages; // to clear the message off the screen properly
      try
        dmDatabase.DoBackup;
      finally
        frmMain.TaskFinished;
      end; // try
    end
    else if lResult = mrCancel then
      raise EBatchUpdate.CreateNonCritical(ResStr_UpdatesCancelled);
  end;
end;

{-------------------------------------------------------------------------------
  Prepare a connection with edit access for the batch update
}
procedure TBatchUpdate.CreateBatchUpdateConnection;
var
  VarRecsAffected: OleVariant;
begin
  if not Assigned(AppSettings.BatchUpdateConnection) then begin
    // create a connection if we haven't already got one
    AppSettings.BatchUpdateConnection := TADOConnection.Create(nil);
    AppSettings.BatchUpdateConnection.LoginPrompt := False;
    AppSettings.BatchUpdateConnection.CommandTimeout := 0;
    dmDatabase.SetConnectionToMainDB(AppSettings.BatchUpdateConnection);
    AppSettings.BatchUpdateConnection.Open;
    // Set the connection to have Full Edit rights.
    AppSettings.BatchUpdateConnection.ConnectionObject.Execute('EXEC sp_setapprole ''R2k_FullEdit'', ''d93nmc741''',
        VarRecsAffected, adCmdText+adExecuteNoRecords);
  end;
end;

{-------------------------------------------------------------------------------
  Displays the updated records as an external filter.
}
procedure TBatchUpdate.FilterUpdate;
begin
  ShowInformation(ResStr_EnsureCommitCancel);
  with TRecordsetFilter.Create do
    try
      if LoadFilter(FBatchUpdateFile.FilterRecordset) then
        ApplyFilter(False);
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  If the count returns zero, rollback and display a message that nothing was
      updated.
}
procedure TBatchUpdate.NothingToUpdate;
begin
  Cleanup;
  ShowInformation(ResStr_NothingToUpdate);
end;

end.

