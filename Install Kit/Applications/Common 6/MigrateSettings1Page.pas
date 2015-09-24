{===============================================================================
  Unit:        MigrateSettingsPage

  Defines:     TfraMigrateSettings

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 9 $
    $Date: 5/08/09 10:47 $
    $Author: Ericsalmon $

===============================================================================}

unit MigrateSettings1Page;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, FolderBrowser, Settings, ExtCtrls;

type
  TfraMigrateSettings1 = class(TBasePage)
    btnDBFolder: TButton;
    btnMapFilesFolder: TButton;
    btnObjectSheetsFolder: TButton;
    btnPolygonFiltersFolder: TButton;
    chkMapFiles: TCheckBox;
    chkObjectSheets: TCheckBox;
    chkPolygonFilters: TCheckBox;
    dlgFolder: TFolderBrowser;
    eDBPath: TEdit;
    eMapFilesPath: TEdit;
    eObjectSheetsPath: TEdit;
    ePolygonFiltersPath: TEdit;
    lblInformation: TLabel;
    lblAlsoTransfer: TLabel;
    lblSourceMDB: TLabel;
    lblInstance: TLabel;
    procedure btnDBFolderClick(Sender: TObject);
    procedure btnMapFilesFolderClick(Sender: TObject);
    procedure btnObjectSheetsFolderClick(Sender: TObject);
    procedure CheckChange(Sender: TObject);
  private
    procedure GetFolder(AEdit: TEdit);
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
  LoginPage, MigrateWelcomePage, MigrateSettings2Page, TextMessages, Functions,
  GeneralFunctions, SetupConstants, MigrationSettings;

{-==============================================================================
    TfraMigrateSettings1
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.btnDBFolderClick(Sender: TObject);
begin
  inherited;
  GetFolder(eDBPath);
end;  // TfraMigrateSettings1.btnDBFolderClick 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.btnMapFilesFolderClick(Sender: TObject);
begin
  inherited;
  GetFolder(ePolygonFiltersPath);
end;  // TfraMigrateSettings1.btnMapFilesFolderClick 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.btnObjectSheetsFolderClick(Sender: TObject);
begin
  inherited;
  GetFolder(eObjectSheetsPath);
end;  // TfraMigrateSettings1.btnObjectSheetsFolderClick 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.CheckChange(Sender: TObject);
  
  procedure SetControlsState(AState: Boolean; AEdit: TEdit; AButton: TButton);
  begin
    AEdit.Enabled := AState;
    AButton.Enabled := AState;
  end;
  
begin
  inherited;
  if Sender = chkMapFiles then
    SetControlsState(chkMapFiles.Checked, eMapFilesPath, btnMapFilesFolder)
  else
  if Sender = chkObjectSheets then
    SetControlsState(chkObjectSheets.Checked, eObjectSheetsPath, btnObjectSheetsFolder)
  else
  if Sender = chkPolygonFilters then
    SetControlsState(chkPolygonFilters.Checked, ePolygonFiltersPath, btnPolygonFiltersFolder);
  ChangedContent;
end;  // TfraMigrateSettings1.CheckChange 

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetConfirmCancel: Boolean;
begin
  Result := False;  // Confirmation not necessary
end;  // TfraMigrateSettings1.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.GetFolder(AEdit: TEdit);
begin
  with dlgFolder do begin
    Folder := ExpandLongPathName(AEdit.Text);
    if Execute then begin
      AEdit.Text := ExcludeTrailingPathDelimiter(Folder);
      ChangedContent;
    end;
  end;
end;  // TfraMigrateSettings1.GetFolder 

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetHasNext: Boolean;
begin
  Result := (eDBPath.Text <> '') and
            FileExists(IncludeTrailingPathDelimiter(eDBPath.Text) + ACCESS_MAIN_DB) and
            (not chkMapFiles.Checked or (eMapFilesPath.Text <> '')) and
            (not chkObjectSheets.Checked or (eObjectSheetsPath.Text <> '')) and
            (not chkPolygonFilters.Checked or (ePolygonFiltersPath.Text <> ''));
end;  // TfraMigrateSettings1.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraMigrateSettings1.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetNext: TBasePageClass;
begin
  Result := TfraMigrateSettings2;
end;  // TfraMigrateSettings1.GetNext

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetPrevious: TBasePageClass;
begin
  if Settings.InstallType in [itStandalone, itServer] then
    Result := TfraLogin
  else
    Result := TfraMigrateWelcome;
end;  // TfraMigrateSettings1.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings1.GetResourceImage: String;
begin
  Result := ResImg_MigrateSettings1;
end;  // TfraMigrateSettings1.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.LoadContent;
begin
  inherited;
  with TMigrationSettings(Settings) do begin
    lblInstance.Caption      := ServerName;
    eDBPath.Text             := MigrationAccessDBPath;
    eMapFilesPath.Text       := MigrationMapFilesPath;
    eObjectSheetsPath.Text   := MigrationObjectSheetsPath;
    ePolygonFiltersPath.Text := MigrationPolygonFiltersPath;
  
    chkMapFiles.Checked       := MigrateMapFiles;
    chkObjectSheets.Checked   := MigrateObjectSheets;
    chkPolygonFilters.Checked := MigratePolygonFilters;
  end;
  CheckChange(chkMapFiles);
  CheckChange(chkObjectSheets);
  CheckChange(chkPolygonFilters);
end;  // TfraMigrateSettings1.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.SaveContent;
begin
  inherited;
  with TMigrationSettings(Settings) do begin
    MigrationAccessDBPath       := ExcludeTrailingPathDelimiter(eDBPath.Text);
    MigrationMapFilesPath       := ExcludeTrailingPathDelimiter(eMapFilesPath.Text);
    MigrationObjectSheetsPath   := ExcludeTrailingPathDelimiter(eObjectSheetsPath.Text);
    MigrationPolygonFiltersPath := ExcludeTrailingPathDelimiter(ePolygonFiltersPath.Text);
  
    MigrateMapFiles       := chkMapFiles.Checked;
    MigrateObjectSheets   := chkObjectSheets.Checked;
    MigratePolygonFilters := chkPolygonFilters.Checked;
  end;
end;  // TfraMigrateSettings1.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings1.ValidateContent;
begin
  inherited;
  // Check the paths are valid.
  CheckPath(eDBPath);

  if chkMapFiles.Checked then CheckPath(eMapFilesPath);
  if chkObjectSheets.Checked then CheckPath(eObjectSheetsPath);
  if chkPolygonFilters.Checked then CheckPath(ePolygonFiltersPath);

  // Check the Access data is being loaded from a local drive
  if not (GetDriveType(PChar(Copy(eDBPath.Text, 1, 2))) in [DRIVE_FIXED, DRIVE_REMOVABLE]) then
  begin
    MessageDlg(ResStr_LocalPathError, mtWarning, [mbOk], 0);
    eDBPath.SetFocus;
    Abort;
  end;
end;  // TfraMigrateSettings1.ValidateContent 

end.




