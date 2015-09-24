unit Password;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FormActions, ImageListButton;

type
  TdlgPassword = class(TForm)
    ePassword: TEdit;
    lblPasswordInstruct: TLabel;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    procedure btnOKClick(Sender: TObject);
  private
    FstPassword: String;
  public
    property Password: String read FstPassword;
  end;

//==============================================================================
implementation

{$R *.dfm}

procedure TdlgPassword.btnOKClick(Sender: TObject);
begin
  FstPassword := ePassword.Text;
end;

end.
