unit RucksackSave;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, FolderBrowser, OnlineHelp, FormActions,
  ImageListButton;

type
  TdlgRucksackSave = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    eRucksackName: TEdit;
    btnBrowse: TButton;
    eDestFolder: TEdit;
    Label2: TLabel;
    dlgFolder: TFolderBrowser;
    bbOk: TImageListButton;
    bbCancel: TImageListButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure eRucksackNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    hlpRucksack: TOnlineHelp;
  public
  end;

//==============================================================================
implementation

{$R *.DFM}

uses ApplicationSettings;

//==============================================================================
procedure TdlgRucksackSave.FormCreate(Sender: TObject);
begin
  if AppSettings.RucksackPath='' then
    eDestFolder.Text:=ExtractFilePath(Application.ExeName)
  else
    eDestFolder.Text:=AppSettings.RucksackPath;
  eRucksackName.Text:='';
  bbOk.Enabled      :=false;

  //Help Setup
  hlpRucksack := TOnlineHelp.Create(Self.Handle);
  OnHelp      := hlpRucksack.OnHelpReplacement;
  HelpContext := IDH_RUCKSACK;
end;  // FormCreate

//==============================================================================
procedure TdlgRucksackSave.btnBrowseClick(Sender: TObject);
var stFolder:string;
begin
  stFolder:=eDestFolder.Text;
  if stFolder[Length(stFolder)]='\' then stFolder:=Copy(stFolder,1,Length(stFolder)-1);
  dlgFolder.Folder:=stFolder;
  if dlgFolder.Execute then
    eDestFolder.Text:=dlgFolder.Folder+'\';
end;  // btnBrowseClick

//==============================================================================
procedure TdlgRucksackSave.eRucksackNameChange(Sender: TObject);
begin
  bbOk.Enabled:=(eRucksackName.Text<>'');
end;  // eRucksackNameChange

//==============================================================================
procedure TdlgRucksackSave.FormDestroy(Sender: TObject);
begin
  hlpRucksack.Free;
end;

//==============================================================================
end.
