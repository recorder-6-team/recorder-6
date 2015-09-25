{===============================================================================
  Unit:        UpgradeFrame

  Defines:     TfraUpgrade

  Description: Progress splash screen for Recorder database upgrades.

  Created:

  Last revision information:
    $Revision: 2 $
    $Date: 12/02/09 11:03 $
    $Author: Ericsalmon $

===============================================================================}
unit UpdateDescriptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmUpdateDescriptions = class(TForm)
    pnlButton: TPanel;
    mmDescriptions: TMemo;
    btnClose: TButton;
    pnlTop: TPanel;
    lblUpgradeApplied: TLabel;
  end;

implementation

{$R *.dfm}

end.
