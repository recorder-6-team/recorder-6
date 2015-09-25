{===============================================================================
  Unit:        NagScreen

  Defines:     TdlgNagScreen

  Description: Displays a warning message if Recorder 2k2 is still installed.

  Model:

  Last revision information:
    $Revision: 2 $
    $Date: 6/04/05 12:11 $
    $Author: Johnvanbreda $

===============================================================================}

unit NagScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TdlgNagScreen = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    btnOk: TBitBtn;
  private
  public
    class function MustShow: Boolean;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  Registry;

{-------------------------------------------------------------------------------
}
class function TdlgNagScreen.MustShow: Boolean;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Result := OpenKeyReadOnly('\Software\JNCC\Recorder');
    finally
      Free;
    end;
end;  // TdlgNagScreen.MustShow

end.
