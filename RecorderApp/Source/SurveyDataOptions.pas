//==============================================================================
//  Unit:        SurveyDataOptions
//
//  Implements:
//               TdlgSurveyDataOptions
//  Description:
//
//  Author:
//  Created:     20 Dec 2001
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 20/06/02 14:57 $
//    $Author: Ericsalmon $
//
//  $History: SurveyDataOptions.pas $
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 14:57
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit SurveyDataOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ImageListButton;

type
  TdlgSurveyDataOptions = class(TForm)
    lblDisplay: TLabel;
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
procedure TdlgSurveyDataOptions.rgOptionsClick(Sender: TObject);
begin
  bbOK.Enabled := true;
end;

//==============================================================================
end.
