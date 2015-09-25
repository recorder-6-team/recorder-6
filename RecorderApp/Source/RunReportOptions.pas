//==============================================================================
//  Unit:        RunReportOptions
//
//  Implements:
//               TdlgRunReportOptions
//  Description:
//
//  Author:      Polly Shaw
//  Created:     23 Dec 2002
//
//  Last Revision Details:
//    $Revision: 1 $
//    $Date: 24/12/02 10:12 $
//    $Author: Ericsalmon $
//
//  $History: RunReportOptions.pas $
//  
//  *****************  Version 1  *****************
//  User: Ericsalmon   Date: 24/12/02   Time: 10:12
//  Created in $/JNCC/Source
//
//==============================================================================

unit RunReportOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ImageListButton;

const
  //Constants for where the options are in rgOptions
  OPT_TEMPLATE_INDEX = 0;
  OPT_SNAPSHOT_INDEX = 1;


type
  TdlgRunReportOptions = class(TForm)
    lblRun: TLabel;
    rgOptions: TRadioGroup;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure rgOptionsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//==============================================================================
implementation

uses
  FormActions;
  
{$R *.DFM}

//==============================================================================
procedure TdlgRunReportOptions.rgOptionsClick(Sender: TObject);
begin
  bbOK.Enabled := true;
end;

//==============================================================================
end.
