{===============================================================================
  Unit:        InstallFolderPage

  Defines:     TfraInstallFolder

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 5 $
    $Date: 24/03/09 15:58 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallFolderPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, ExtCtrls, ComCtrls;

type
  TfraInstallFolder = class (TBasePage)
    btnBrowse: TButton;
    dlgFolder: TFolderBrowser;
    eInstallFolder: TEdit;
    lblInformation: TLabel;
    lblDisk: TLabel;
    lblRequiredSpace: TLabel;
    lblRequired: TLabel;
    lblNotLocal: TLabel;
    lblRequiredDatabase: TLabel;
    lblRequiredDB: TLabel;
    lblNoSpace: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure eInstallFolderChange(Sender: TObject);
  private
    function CheckAvailableDiskSpace(const ADrive: String): Boolean;
    function CheckDriveIsLocal(const ADrive: String): Boolean;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure ValidateContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  WelcomePage, SiteInfoPage, Settings, Functions, SetupConstants;

resourcestring
  ResStr_NotEnoughRoomForDatabase = 'No local drive contains enough free space to '
        + 'install the Recorder 6 Database. It may still be possible to proceed by '
        + 'overwriting an existing database.';

{-==============================================================================
    TfraInstallFolder
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.btnBrowseClick(Sender: TObject);
begin
  inherited;
  if DirectoryExists(eInstallFolder.Text) then
    dlgFolder.Folder := eInstallFolder.Text;
  if dlgFolder.Execute then begin
    eInstallFolder.Text := dlgFolder.Folder;
    eInstallFolder.Modified := True;
    ChangedContent;
  end;
end;  // TfraInstallFolder.btnBrowseClick 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.CheckAvailableDiskSpace(const ADrive: String): Boolean;
var
  lDrive: String;
  lFree, lFreeForCaller, lTotal: TLargeInteger;
begin
  lDrive := IncludeTrailingPathDelimiter(UpperCase(ADrive));
  if not (lDrive[1] in ['A'..'Z']) then lDrive := 'C:\';
  Result := CheckDriveIsLocal(lDrive);

  if SysUtils.GetDiskFreeSpaceEx(PChar(lDrive), lFreeForCaller, lTotal, @lFree) then begin
    Result := Result and (lFree > Settings.RequiredApplicationDiskSpace);
  end else begin
    Result := False;
  end;

  if (not lblNotLocal.Visible) and (not Result) then
    lblNoSpace.Visible := True
  else
    lblNoSpace.Visible := False;
end;  // TfraInstallFolder.CheckAvailableDiskSpace

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.CheckDriveIsLocal(const ADrive: String): Boolean;
begin
  Result := GetDriveType(PChar(ADrive)) = DRIVE_FIXED;
  lblNotLocal.Visible := not Result;
end;  // TfraInstallFolder.CheckDriveIsLocal

{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.eInstallFolderChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraInstallFolder.eInstallFolderChange

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetConfirmCancel: Boolean;
begin
  Result := Settings.FoldersCreated.Count <> 0;
end;  // TfraInstallFolder.GetConfirmCancel

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetHasNext: Boolean;
var
  lDrive: String;
begin
  // We must have at least a drive, even if the whole path is a root.
  lDrive := ExtractFileDrive(eInstallFolder.Text);
  Result := CheckAvailableDiskSpace(lDrive) and
            (lDrive <> '') and (Copy(eInstallFolder.Text, 1, 3) = lDrive + PathDelim);
end;  // TfraInstallFolder.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraInstallFolder.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetNext: TBasePageClass;
begin
  Result := TfraSiteInfo;
end;  // TfraInstallFolder.GetNext 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetPrevious: TBasePageClass;
begin
  Result := TfraWelcome;
end;  // TfraInstallFolder.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetResourceImage: String;
begin
  Result := ResImg_InstallFolder;
end;  // TfraInstallFolder.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.LoadContent;
var
  i: Char;
  labelTop, labelLeft: Integer;
  freeSpace, freeForCaller, total, maxSpace: TLargeInteger;
  drive: String;
begin
  inherited;
  eInstallFolder.Text   := Settings.InstallFolder;
  lblRequired.Caption   := FormatBytes(Settings.RequiredApplicationDiskSpace);
  lblRequiredDB.Caption := FormatBytes(Settings.RequiredDatabaseDiskSpace);
  labelTop  := lblDisk.Top;
  labelLeft := lblDisk.Left + 50; // Shift slightly to the right
  maxSpace := 0;
  // Adds a label for each local drive, showing how much space is available.
  for i := 'A' to 'Z' do begin
    drive := i + ':\';
    if CheckDriveIsLocal(drive) then begin
      labelTop := labelTop + 20; //Move down by the distance between labels
      with TLabel.Create(Self) do begin
        Left := labelLeft;
        Top  := labelTop;
        if SysUtils.GetDiskFreeSpaceEx(PChar(drive), freeForCaller, total, @freeSpace) then
        begin
          Caption := i + ':'#9 + FormatBytes(freeSpace);
          if freeSpace > maxSpace then maxSpace := freeSpace;
        end;
        Parent := Self;
      end;
    end;
  end;
  // Displays a warning label when none of the local drives have room to store
  // the database.
  if maxSpace < Settings.RequiredDatabaseDiskSpace then
  begin
    labelTop := labelTop + 20;
    with TLabel.Create(Self) do
    begin
      Left       := lblNotLocal.Left;
      Top        := labelTop;
      Caption    := ResStr_NotEnoughRoomForDatabase;
      Parent     := Self;
      Font.Color := clRed;
      Width      := lblNotLocal.Width;
      WordWrap   := True;
    end;
  end;
  ChangedContent;
end;  // TfraInstallFolder.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.SaveContent;
begin
  inherited;
  Settings.InstallFolder := eInstallFolder.Text;
end;  // TfraInstallFolder.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.ValidateContent;
begin
  inherited;
  // Only allow moving on to next page if folder gets created, or already exists.
  if not Settings.FolderExists(eInstallFolder.Text) then begin
    eInstallFolder.SetFocus;
    Abort;
  end;
end;  // TfraInstallFolder.ValidateContent 

end.
