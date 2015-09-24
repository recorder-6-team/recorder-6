unit DatabaseFileLocationPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, DBFileLocationSelectFrame, StdCtrls, ExtCtrls;

type
  TfraDatabaseFileLocation = class(TBasePage)
    fraSelectDBFileLocation: TfraDBFileLocationSelect;
    lblFileLocationInstruct: TLabel;
  private
    procedure fraSelectDBFileLocationChange(Sender: TObject);
  protected
    function GetHasNext: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
    function GetNextCaption: String; override;
  public
    procedure ValidateContent; override;
  end;

var
  fraDatabaseFileLocation: TfraDatabaseFileLocation;

implementation

{$R *.dfm}

uses
  TextMessages, InstallationPage, SelectExistingServerPage;

{ TfraDatabaseFileLocation }

{-------------------------------------------------------------------------------
  When the DB File Location frame is changed, call the changed content procedure.
}
procedure TfraDatabaseFileLocation.fraSelectDBFileLocationChange(Sender: TObject);
begin
  ChangedContent;
end;

{-------------------------------------------------------------------------------
  Used by the base page to get the text to display on the 'next' button.
}
function TfraDatabaseFileLocation.GetNextCaption: String;
begin
  Result := ResStr_InstallCaption;
end;


{-------------------------------------------------------------------------------
  Gets whether or not the next button should be enabled.
}
function TfraDatabaseFileLocation.GetHasNext: Boolean;
begin
  Result := fraSelectDBFileLocation.IsValid;
end;

function TfraDatabaseFileLocation.GetNext: TBasePageClass;
begin
  result := TfraInstallation;
end;

function TfraDatabaseFileLocation.GetPrevious: TBasePageClass;
begin
  result := TfraSelectExistingServer;
end;

(**
 * Load the frame content from the settings object.
 *)
procedure TfraDatabaseFileLocation.LoadContent;
begin
  fraSelectDBFileLocation.Settings := Settings;
  fraSelectDBFileLocation.OnChange := fraSelectDBFileLocationChange;
end;

procedure TfraDatabaseFileLocation.SaveContent;
begin
  inherited;
  with Settings do begin
    if fraSelectDBFileLocation.FilePath = '' then
      SpecifyOwnDatabasePath := False
    else begin
      SpecifyOwnDatabasePath := True;
      DatabasePath := fraSelectDBFileLocation.FilePath;
    end;
  end;
end;

procedure TfraDatabaseFileLocation.ValidateContent;
begin
  if (fraSelectDBFileLocation.FilePath <> '') and
      not Settings.FolderExists(fraSelectDBFileLocation.FilePath) then
    Abort;
end;

end.
