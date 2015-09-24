{===============================================================================
  Unit:        RemoveWelcomePage

  Defines:     TfraRemoveWelcome

  Description:

  Model:

  Created:

  Last revision information:
    $Revision: 5 $
    $Date: 11/02/09 15:26 $
    $Author: Ericsalmon $

===============================================================================}

unit RemoveWelcomePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, FolderBrowser, StdCtrls, ExtCtrls;

type
  TfraRemoveWelcome = class(TBasePage)
    btnDBFolder: TButton;
    btnZipName: TButton;
    chkZip: TCheckBox;
    dlgFolder: TFolderBrowser;
    dlgOpen: TOpenDialog;
    eDBPath: TEdit;
    eZipName: TEdit;
    lblInformation: TLabel;
    lblDB: TLabel;
    lblZip: TLabel;
    procedure btnDBFolderClick(Sender: TObject);
    procedure btnZipNameClick(Sender: TObject);
    procedure CheckChange(Sender: TObject);
    procedure chkZipClick(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetNextCaption: String; override;
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
  RemovePage, Functions, RemovalSettings, SetupConstants, TextMessages;

{-==============================================================================
    TfraRemoveWelcome
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.btnDBFolderClick(Sender: TObject);
begin
  inherited;
  with dlgFolder do begin
    Folder := eDBPath.Text;
    if Execute then eDBPath.Text := Folder;
  end;
  ChangedContent;
end;  // TfraRemoveWelcome.btnDBFolderClick 

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.btnZipNameClick(Sender: TObject);
begin
  inherited;
  with dlgOpen do begin
    InitialDir := ExtractFilePath(eZipName.Text);
    if Execute then eZipName.Text := FileName;
  end;
  ChangedContent;
end;  // TfraRemoveWelcome.btnZipNameClick 

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.CheckChange(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraRemoveWelcome.CheckChange 

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.chkZipClick(Sender: TObject);
begin
  inherited;
  btnDBFolder.Enabled := chkZip.Checked;
  btnZipName.Enabled := chkZip.Checked;
  eDBPath.Enabled := chkZip.Checked;
  eZipName.Enabled := chkZip.Checked;
  lblDB.Enabled := chkZip.Checked;
  lblZip.Enabled := chkZip.Checked;
  ChangedContent;
end;  // TfraRemoveWelcome.chkZipClick 

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetConfirmCancel: Boolean;
begin
  Result := False;
end;  // TfraRemoveWelcome.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetHasNext: Boolean;
begin
  Result := not chkZip.Checked or
            ((eDBPath.Text <> '') and
             FileExists(IncludeTrailingPathDelimiter(eDBPath.Text) + ACCESS_MAIN_DB) and 
             (eZipName.Text <> ''));
end;  // TfraRemoveWelcome.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraRemoveWelcome.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetNext: TBasePageClass;
begin
  Result := TfraRemove;
end;  // TfraRemoveWelcome.GetNext 

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetNextCaption: String;
begin
  Result := ResStr_RemoveCaption;
end;  // TfraRemoveWelcome.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TfraRemoveWelcome.GetResourceImage: String;
begin
  Result := ResImg_Welcome;
end;  // TfraRemoveWelcome.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.LoadContent;
begin
  with TRemovalSettings(Settings) do begin
    chkZip.Checked := ArchiveDatabase;
    if MigrationAccessDBPath <> '' then
      eDBPath.Text := IncludeTrailingPathDelimiter(MigrationAccessDBPath)
    else
      eDBPath.Text := '';
    eZipName.Text  := ArchiveZipName;
  end;
  chkZipClick(nil);
end;  // TfraRemoveWelcome.LoadContent

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.SaveContent;
begin
  with TRemovalSettings(Settings) do begin
    ArchiveDatabase := chkZip.Checked;
    MigrationAccessDBPath := IncludeTrailingPathDelimiter(eDBPath.Text);
    ArchiveZipName := eZipName.Text;
  end;
end;  // TfraRemoveWelcome.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraRemoveWelcome.ValidateContent;
begin
  if chkZip.Checked then begin
    CheckPath(eDBPath);
    if not FileExists(IncludeTrailingPathDelimiter(eDBPath.Text) + 'NBNData.mdb') then
    begin
      MessageDlg(ResStr_AccessMDBNotFound, mtWarning, [mbOk], 0);
      eDBPath.SetFocus;
      Abort;
    end;
    CheckPath(eZipName, ExtractFilePath(eZipName.Text));
  end;
end;  // TfraRemoveWelcome.ValidateContent 

end.





