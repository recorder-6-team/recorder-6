{===============================================================================
  Unit:        DBFileLocationSelectFrame

  Defines:     TfraDBFileLocationSelect

  Description: A frame for picking a file path for the database files.

  Created:     March 2009

  Last revision information:
    $Revision: 3 $
    $Date: 13/04/09 13:52 $
    $Author: Pauldavies $

===============================================================================}

unit DBFileLocationSelectFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Settings, FolderBrowser;

type
  TfraDBFileLocationSelect = class(TFrame)
    rbDefault: TRadioButton;
    rbPickLocation: TRadioButton;
    eFilePath: TEdit;
    btnFolderBrowse: TButton;
    lblNoSpace: TLabel;
    lblNotLocal: TLabel;
    dlgFolder: TFolderBrowser;
    procedure eFilePathChange(Sender: TObject);
    procedure RadioClick(Sender: TObject);
    procedure btnFolderBrowseClick(Sender: TObject);
  private     
    FSettings: TSettings;
    FOnChange: TNotifyEvent;
    procedure SetFilePath(value: String);
    function CheckDriveIsValid: Boolean;
    function CheckAvailableDiskSpace: Boolean;
    function GetFilePath: String;
  protected
    procedure Change; dynamic;
  public
    function IsValid: Boolean;
    property FilePath: String read GetFilePath write SetFilePath;
    property Settings: TSettings read FSettings write FSettings;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Checks whether the result is valid or not.
}
function TfraDBFileLocationSelect.IsValid: Boolean;
begin
  if rbDefault.Checked then
    Result := True
  else begin
    Result := CheckDriveIsValid;
  end;
end;

{-------------------------------------------------------------------------------
  Returns the entered file path. A blank file path represents the default location.
}
function TfraDBFileLocationSelect.GetFilePath: String;
begin
  if rbDefault.Checked then
    Result := ''
  else
    Result := eFilePath.Text;
end;

{-------------------------------------------------------------------------------
  Sets the entered file path. A blank file path represents the default location.
}
procedure TfraDBFileLocationSelect.SetFilePath(value: String);
begin
  if value = '' then
    rbDefault.Checked := True
  else begin
    rbPickLocation.Checked := True;
    eFilePath.Text := value;
  end;
end;

{-------------------------------------------------------------------------------
  Checks whether the selected drive is a local drive or not. Also sets the visibility
  of the warning labels.
}
function TfraDBFileLocationSelect.CheckDriveIsValid: Boolean;
var
  drive: String;
begin
  drive := ExtractFileDrive(eFilePath.Text) + PathDelim;
  lblNotLocal.Visible := False;
  lblNoSpace.Visible := False;
  if (Length(eFilePath.Text) > 2) and (eFilePath.Text[3] <> PathDelim) then
    Result := False
  else if drive = PathDelim then
    Result := False
  else if GetDriveType(PChar(drive)) <> DRIVE_FIXED then begin
    lblNotLocal.Visible := True;
    Result := False;
  end else if not CheckAvailableDiskSpace then begin
    lblNoSpace.Visible := True;
    Result := False;
  end else
    Result := True;
end;  // TfraDBFileLocationSelect.CheckDriveIsValid

{-------------------------------------------------------------------------------
  Checks whether there is sufficient disk space on the selected drive.
}
function TfraDBFileLocationSelect.CheckAvailableDiskSpace: Boolean;
var
  lDrive: String;
  lFree, lFreeForCaller, lTotal: TLargeInteger;
begin
  lDrive := ExtractFileDrive(eFilePath.Text) + PathDelim;

  if lDrive = PathDelim then
    Result := False
  else if SysUtils.GetDiskFreeSpaceEx(PChar(lDrive), lFreeForCaller, lTotal, @lFree) then
    Result := lFree > Settings.RequiredDatabaseDiskSpace
  else
    Result := False;
end;  // TfraDBFileLocationSelect.CheckAvailableDiskSpace

{-------------------------------------------------------------------------------
  When the file path is changed, check whether the drive is valid so as to display/
  hide the warning labels.
}
procedure TfraDBFileLocationSelect.eFilePathChange(Sender: TObject);
begin
  CheckDriveIsValid;
  Change;
end;

{-------------------------------------------------------------------------------
  When one of the radio buttons are clicked, enable or disable the file path selection
  controls.
}
procedure TfraDBFileLocationSelect.RadioClick(Sender: TObject);
begin
  eFilePath.Enabled := Sender = rbPickLocation;
  btnFolderBrowse.Enabled := Sender = rbPickLocation;
  Change;
end;

{-------------------------------------------------------------------------------
  Opens a folder dialog to allow the user to select a file path.
}
procedure TfraDBFileLocationSelect.btnFolderBrowseClick(Sender: TObject);
begin
  inherited;
  if DirectoryExists(eFilePath.Text) then
    dlgFolder.Folder := eFilePath.Text;
  if dlgFolder.Execute then begin
    eFilePath.Text := dlgFolder.Folder;
    eFilePath.Modified := True;
    CheckDriveIsValid;
    Change;
  end;
end;

{-------------------------------------------------------------------------------
  Call this to throw the 'onchange' event.
}
procedure TfraDBFileLocationSelect.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

end.
