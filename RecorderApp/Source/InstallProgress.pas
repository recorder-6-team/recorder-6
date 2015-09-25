//==============================================================================
//  Unit:        InstallProgress
//
//  Implements:  TfrmInstallProgress
//
//  Description:
//
//  Author:      John van Breda
//  Created:     29 May 2002
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 19/06/02 12:31 $
//    $Author: Ericsalmon $
//
//  $History: InstallProgress.pas $
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 19/06/02   Time: 12:31
//  Updated in $/JNCC/Source
//  Changed label caption to read "Updating Recorder 2002 settings..."
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit InstallProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmInstallProgress = class(TForm)
    pnlMain: TPanel;
    Image1: TImage;
    lblInstalling: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInstallProgress: TfrmInstallProgress;

//==============================================================================
implementation

{$R *.DFM}

//==============================================================================
end.
