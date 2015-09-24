{===============================================================================
  Unit:        InstallFolderPage

  Defines:     TfraInstallFolder

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 4 $
    $Date: 14/07/09 15:43 $
    $Author: Ericsalmon $

===============================================================================}

unit InstallFolderPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, ExtCtrls;

type
  TfraInstallFolder = class(TBasePage)
    btnBrowse: TButton;
    dlgFolder: TFolderBrowser;
    eInstallFolder: TEdit;
    lblFolderInfo: TLabel;
    lblRequiredSpace: TLabel;
    lblAvailable: TLabel;
    lblDisk: TLabel;
    lblDriveNotValid: TLabel;
    lblRequired: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure eInstallFolderChange(Sender: TObject);
  private
    function CheckAvailableDiskSpace: Boolean;
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
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
  SpatialRefPage, Settings, Functions, SetupConstants, TextMessages;

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
function TfraInstallFolder.CheckAvailableDiskSpace: Boolean;
var
  lDrive: String;
  lDriveType: Integer;
  lFree, lFreeForCaller, lTotal: TLargeInteger;
begin
  lblDriveNotValid.Visible := False;
  lDrive := ExtractFileDrive(eInstallFolder.Text);
  lDriveType := GetDriveType(PChar(lDrive));
  
  // Destination must be local drive.
  if lDriveType <> DRIVE_FIXED then
  begin
    lblDisk.Caption := ResStr_UnknownAvailableDiskSpace;
    lblAvailable.Visible := False;
    Result := False;
    lblDriveNotValid.Visible := True;
  end else begin
    lDrive := IncludeTrailingPathDelimiter(UpperCase(lDrive));
    if SysUtils.GetDiskFreeSpaceEx(PChar(lDrive), lFreeForCaller, lTotal, @lFree) then
    begin
      lblDisk.Caption := ResStr_AvailableDiskSpace;
      lblAvailable.Visible := True;

      lblAvailable.Caption := FormatBytes(lFree);
      if lFree >= Settings.RequiredDiskSpace then
        lblAvailable.Font.Color := clWindowText
      else
        lblAvailable.Font.Color := clRed;
      Result := lFree >= Settings.RequiredDiskSpace;
    end else begin
      lblDisk.Caption := ResStr_UnknownAvailableDiskSpace;
      lblAvailable.Visible := False;
      // Assume it's ok though.
      Result := True;
    end;
  end;
end;  // TfraInstallFolder.CheckAvailableDiskSpace 

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
begin
  Result := CheckAvailableDiskSpace;
end;  // TfraInstallFolder.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetHasPrevious: Boolean;
begin
  Result := Settings.FoldersCreated.Count = 0;
end;  // TfraInstallFolder.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetNext: TBasePageClass;
begin
  Result := TfraSpatialRef;
end;  // TfraInstallFolder.GetNext 

{-------------------------------------------------------------------------------
}
function TfraInstallFolder.GetResourceImage: String;
begin
  Result := ResImg_InstallFolder;
end;  // TfraInstallFolder.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraInstallFolder.LoadContent;
begin
  inherited;
  eInstallFolder.Text := Settings.InstallFolder;
  lblRequired.Caption := FormatBytes(Settings.RequiredDiskSpace);
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
