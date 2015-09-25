{===============================================================================
  Unit:        ImportComplete

  Defines:     TdlgImportComplete

  Description: The dialog shown after importing data with the import wizard.
               Offers options to save an external filter file describing the
               imported data, to view this filter without saving it, and to
               close the dialog.

  Last revision information:
    $Revision: 20 $
    $Date: 6/05/09 15:25 $
    $Author: Ericsalmon $

===============================================================================}

unit ImportComplete;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GeneralData, DatabaseAccessADO, ExternalFilter, DataClasses,
  ApplicationSettings, JNCCDatasets, adodb, FormActions, ImageListButton,
  ExtCtrls, IWSettings;

type
  TdlgImportComplete = class(TForm)
    dlgSaveFilter: TSaveDialog;
    lblImported: TLabel;
    lblRejected: TLabel;
    gbRejects: TGroupBox;
    lblRejectFile: TLabel;
    chkViewRejectDetails: TCheckBox;
    chkReviewImports: TCheckBox;
    chkSaveFilter: TCheckBox;
    bevel: TBevel;
    btnOk: TImageListButton;
    lblFailedValidation: TLabel;
    procedure btnOkClick(Sender: TObject);
  private
    FFileName: string;
    FSettings: TdmIWSettings;
    FImportRejectsFile: TFileName;
    function CreateSourceTable(AConnection: TADOConnection): TADOTable;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveFilter(const AFilename: String; ASourceConnection:
        TADOConnection; ATablesCreated: TStringList);
    procedure SetDetails(ASettings: TdmIWSettings; AImports, ARejects: integer;
        const AImportRejectsFile: TFileName);
    procedure ViewFilter(ASourceConnection: TADOConnection; ATablesCreated: TStringList);
    property FileName: string read FFileName;
  end;

{-==============================================================================
}
implementation

uses
  GeneralFunctions, Constants, EasyShell, Maintbar, OnlineHelp;

resourcestring
  ResStr_TwoKeyTable =
      'A %s record relating to this record was added or modified by the import.';

{$R *.dfm}

{-==============================================================================
    TdlgImportComplete
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdlgImportComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.HelpContext := IDH_IMPORTRESULTS;
end;

{-------------------------------------------------------------------------------
  Save data about the imported records as a tab-separated file.
}
procedure TdlgImportComplete.SaveFilter(const AFilename: String;
    ASourceConnection: TADOConnection; ATablesCreated: TStringList);
var
  i: Integer;
  lPrimaryKey: TPrimaryKey;
  lSingleRecord: TSingleRecord;
  lSourceTable: TADOTable;
  lOutput: TStringList;
begin
  lOutput            := TStringList.Create;
  lOutput.Duplicates := dupIgnore;
  lOutput.Sorted     := True;

  lSourceTable       := CreateSourceTable(ASourceConnection);
  try
    lOutput.Add('Description: File '
        + AFileName
        + ' imported by '
        + dmGeneralData.GetIndividualName(AppSettings.UserID)
        + ' at '
        + DateTimeToStr(Now));

    for i := 0 to ATablesCreated.Count - 1 do
    begin
      lSourceTable.TableName := ATablesCreated[i];
      lSourceTable.Open;
      try
        lPrimaryKey := dmDatabase.SplitPrimaryKey(lSourceTable.TableName);
        while not lSourceTable.Eof do
        begin
          lSingleRecord.TableName := lSourceTable.TableName;
          lSingleRecord.Key1      := lSourceTable.FieldByName(lPrimaryKey.Key1).AsString;
          if lPrimaryKey.Key2 = '' then
            lOutput.Add(lSourceTable.TableName + #9 + lSingleRecord.Key1)
          else
            // If we have a double primary key, just save the first one against the
            // table it's a foreign key link to.
            lOutput.Add(
                Copy(lPrimaryKey.Key1, 1, Length(lPrimaryKey.Key1) - 4)
                + #9 + lSingleRecord.Key1
                + #9 + Format(ResStr_TwoKeyTable, [ReadableFormat(lSourceTable.TableName)]));
          lSourceTable.Next;
        end; // while
      finally
        lSourceTable.Close;
      end;
    end;
    lOutput.SaveToFile(AFileName);
  finally
    lOutput.Free;
  end;
end;

{-------------------------------------------------------------------------------
  View the imported records as an external filter.
}
procedure TdlgImportComplete.ViewFilter(ASourceConnection: TADOConnection;
    ATablesCreated: TStringList);
var
  i: Integer;
  lPrimaryKey: TPrimaryKey;
  lSingleRecord: TSingleRecord;
  lKeyList: TEditableKeyList;
  lSourceTable: TADOTable;
begin
  // Create a key list to store the data we want to view
  lKeyList     := TEditableKeyList.Create;
  lSourceTable := CreateSourceTable(ASourceConnection);
  try
    // Loop through the imported data to populate the key list
    for i := 0 to ATablesCreated.Count - 1 do
    begin
      lSourceTable.TableName := ATablesCreated[i];
      if lKeyList.Header.TableName = '' then
        lKeyList.SetTable(lSourceTable.TableName)
      else
        lKeyList.SetTable(MIXED_DATA);
      lSourceTable.Open;
      try
        lPrimaryKey := dmDatabase.SplitPrimaryKey(lSourceTable.TableName);
        while not lSourceTable.Eof do
        begin
          // Add each record to the keylist.
          lSingleRecord.TableName := lSourceTable.TableName;
          lSingleRecord.Key1      := lSourceTable.FieldByName(lPrimaryKey.Key1).AsString;
          if lPrimaryKey.Key2 = '' then
            lKeyList.AddItem(lSingleRecord.Key1, lSourceTable.TableName)
          else
            // For double primary keys, add the first key against the table it's
            // a foreign key link to.
            lKeyList.AddItem(
                lSingleRecord.Key1,
                Copy(lPrimaryKey.Key1, 1, Length(lPrimaryKey.Key1) - 4));
          lSourceTable.Next;
        end;
      finally
        lSourceTable.Close;
      end;
    end;

    // Load an external filter from the key list
    with TListFilter.Create do begin
      try
        if LoadFilter(lKeyList) then
          ApplyFilter;
      finally
        Free;
      end;
    end;
  finally
    lSourceTable.Free;
    lKeyList.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Prepare an ADO table for accessing the source database
}
function TdlgImportComplete.CreateSourceTable(AConnection: TADOConnection):
    TADOTable;
begin
  Result := TADOTable.Create(nil);
  Result.Connection := AConnection;
end;

{-------------------------------------------------------------------------------
  Set the label details
}
procedure TdlgImportComplete.SetDetails(ASettings: TdmIWSettings; AImports,
    ARejects: integer; const AImportRejectsFile: TFileName);
var
  shift: integer;
begin
  FSettings                   := ASettings;
  FImportRejectsFile          := AImportRejectsFile;
  lblImported.Caption         := Format(lblImported.Caption, [AImports]);
  lblFailedValidation.Caption := Format(lblFailedValidation.Caption, [ASettings.InvalidNodes.Count]);

  if ARejects>0 then begin
    lblRejected.Caption   := Format(lblRejected.Caption, [ARejects]);
    lblRejectFile.Caption := Format(lblRejectFile.Caption, [AImportRejectsFile]);
  end else begin
    lblRejected.Visible := false;
    gbRejects.Visible   := false;
    // slide up remaining controls by difference between top of the controls
    // being hidden and the next visible control
    shift := chkReviewImports.Top - lblRejected.Top;
    chkReviewImports.Top := chkReviewImports.Top - shift;
    chkSaveFilter.Top    := chkSaveFilter.Top - shift;
    bevel.Height         := bevel.Height - shift;
    btnOk.Top            := btnOk.Top - shift;
    Height               := Height - shift;
  end;
  chkReviewImports.Enabled := AImports > 0;
  chkSaveFilter.Enabled    := AImports > 0;
end;

{-------------------------------------------------------------------------------
  Take various actions depending on checked boxes when user clicks Ok
}
procedure TdlgImportComplete.btnOkClick(Sender: TObject);
var
  lYear, lMonth, lDay: Word;
begin
  if chkSaveFilter.Checked then begin
    dlgSaveFilter.InitialDir := AppSettings.ReportPath + 'Output';
    ForceDirectories(dlgSaveFilter.InitialDir);
    DecodeDate(Date, lYear, lMonth, lDay);
    dlgSaveFilter.FileName :=
        'Import_'
        + FormatDateTime('yyyy-mm-dd', Now)
        + '.ref';
    if not dlgSaveFilter.Execute then begin
      ModalResult := mrNone;
      Exit;
    end;
    SaveFilter(
        dlgSaveFilter.FileName,
        FSettings.TempData.Database,
        FSettings.TempData.TablesCreated);
  end;

  if chkViewRejectDetails.Checked then
    ShellFile(FImportRejectsFile);

  if chkReviewImports.Checked then
    ViewFilter(FSettings.TempData.Database, FSettings.TempData.TablesCreated)
  else
    frmMain.BroadcastMessage(WM_REFRESH_OBSERVATIONS); 
end;

end.
