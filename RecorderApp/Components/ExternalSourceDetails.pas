unit ExternalSourceDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImageListButton;

type
  TdlgExternalSourceDetails = class(TForm)
    bbCancel: TImageListButton;
    Bevel1: TBevel;
    bbOK: TImageListButton;
    Label1: TLabel;
    ePath: TEdit;
    Label2: TLabel;
    eTitle: TEdit;
    btnOpen: TButton;
    procedure btnOpenClick(Sender: TObject);
  private
    FDefaultPath: string;
    procedure SetDefaultPath(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    property DefaultPath: string read FDefaultPath write SetDefaultPath;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
 Display a file open dialog to retrieve a file path from the disk.
}
procedure TdlgExternalSourceDetails.btnOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do try
    Filter:='All Files (*.*)|*.*';
    InitialDir:=FDefaultPath;
    if Execute then
      ePath.Text := FileName;
  finally;
    Free;
  end;
end;

procedure TdlgExternalSourceDetails.SetDefaultPath(const Value: string);
begin
  FDefaultPath := Value;
end;

end.
