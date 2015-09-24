{===============================================================================
  Unit:           MissingDBScripts

  Defines:        TdlgMissingScripts

  Description:    Shows error message if wrong version of upgrade is run. Displays
                  a "Hot Label" to link to JNCC site.

  Created:        July 2009

  Last revision information:
    $Revision: 3 $
    $Date: 16/07/09 14:39 $
    $Author: Ericsalmon $

===============================================================================}
unit MissingDBScripts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HotLabel, StdCtrls, ShellAPI;

type
  TdlgMissingScripts = class(TForm)
    lblErrorInfo: TLabel;
    btnOk: TButton;
    lblLink: THotLabel;
    lblWebAddress: TLabel;
    procedure lblLinkClick(Sender: TObject);
  private
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Use default web browser and go to address specified on the label.
}
procedure TdlgMissingScripts.lblLinkClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(lblLink.Caption), nil, nil, SW_SHOWNORMAL);
end;

end.
