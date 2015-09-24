{-------------------------------------------------------------------------------
  Unit:        FrameInstallFolder.pas

  Defines:     TfraInstallFolder

  Description: Gets the folder where Recorder will be installed

  Created:     March 2003

  Last revision information:
    $Revision: 9 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameInstallFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, FolderBrowser, StdCtrls, Settings, TextMessages,
  ComCtrls;

type
  TfraInstallFolder = class(TPageFrame)
    Label1: TLabel;
    btnBrowse: TButton;
    FolderBrowser: TFolderBrowser;
    eInstallFolder: TEdit;
    Label4: TLabel;
    procedure btnBrowseClick(Sender: TObject);
    procedure eInstallFolderChange(Sender: TObject);
  private
    FCreated: Boolean;
    procedure SetNextButtonState;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetNextButton(const Value: TButton); override;
    procedure SetSettings(const Value: TSettings); override;
    procedure DoPostProcess; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameWelcome, FrameSiteInfo, FrameNewOrExisting;

//==============================================================================
{ TfraInstallFolder }
//------------------------------------------------------------------------------
constructor TfraInstallFolder.Create(AOwner: TComponent);
begin
  inherited;
  FCreated := true;
  eInstallFolder.Modified := False;
end;

//------------------------------------------------------------------------------
function TfraInstallFolder.GetNextFrame: TPageFrameClass;
var lNextFrameOk: Boolean;
begin
  // Needs to stop messages coming up until user presses Next.
  if FCreated then begin
    FCreated := false;
    lNextFrameOk := true;
  end else
    lNextFrameOk := Settings.FolderExists(eInstallFolder.Text);

  if lNextFrameOk then
    if Settings.InstallMode = imUpgrade then
      Result := TfraNewOrExisting
    else
      Result := TfraSiteInfo
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
function TfraInstallFolder.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraWelcome;
end;

//------------------------------------------------------------------------------
procedure TfraInstallFolder.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraInstallFolder.SetSettings(const Value: TSettings);
begin
  inherited;
  eInstallFolder.Text := Settings.InstallFolder;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraInstallFolder.btnBrowseClick(Sender: TObject);
begin
  inherited;
  if DirectoryExists(eInstallFolder.Text) then
    FolderBrowser.Folder := eInstallFolder.Text;
  if FolderBrowser.Execute then begin
    eInstallFolder.Text   := FolderBrowser.Folder;
    Settings.InstallFolder:= FolderBrowser.Folder;
    eInstallFolder.Modified := True;
  end;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraInstallFolder.eInstallFolderChange(Sender: TObject);
begin
  inherited;
  Settings.InstallFolder := eInstallFolder.Text;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraInstallFolder.SetNextButtonState;
begin
  if Assigned(NextButton) then begin
    NextButton.Enabled := Length(eInstallFolder.Text) > 2;
    // Extra checks, but only for upgrade and if path seems valid to start with.
    if NextButton.Enabled and (Settings.InstallMode = imUpgrade) then begin
      // Recorder2000.exe must be in specified folder to allow upgrade
      NextButton.Enabled := FileExists(IncludeTrailingPathDelimiter(eInstallFolder.Text) + 'Recorder2000.exe');
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Description : Perform processing required before Next button is clicked
  Created : 24/03/2003 }
procedure TfraInstallFolder.DoPostProcess;
begin
  if not eInstallFolder.Modified then
    Settings.ForceFolders(eInstallFolder.Text);
end;

end.
