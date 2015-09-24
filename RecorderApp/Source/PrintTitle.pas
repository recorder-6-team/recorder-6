//==============================================================================
//  Unit:        PrintTitle
//
//  Implements:  TdlgPrintTitle
//
//  Description:
//
//  Author:      Paul Thomas
//  Created:     17 Aug 1999
//
//  Last Revision Details:
//    $Revision: 9 $
//    $Date: 18/12/07 13:10 $
//    $Author: Rickyshrestha $
//
//  $History: PrintTitle.pas $
//  
//  *****************  Version 9  *****************
//  User: Rickyshrestha Date: 18/12/07   Time: 13:10
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded string to resourcestring
//  ResStr_TitleLengthError
//    ResStr_TitleForPrintOut
//  
//  *****************  Version 8  *****************
//  User: Ericsalmon   Date: 2/12/02    Time: 14:47
//  Updated in $/JNCC/Source
//  Adapted for Delphi 7
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 11:41
//  Updated in $/JNCC/Source
//  Repalced BitBtn with ImageListButton
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DelphiVersions.Inc'}

unit PrintTitle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ExceptionForm, ImageListButton, Variants;

type
  EPrintTitleError = class(TExceptionPath);

  TdlgPrintTitle = class(TForm)
    ePrintTitle: TEdit;
    lblPrintTiltle: TLabel;
    bvlPrintTitle: TBevel;
    bbOK: TImageListButton;
    procedure bbOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgPrintTitle: TdlgPrintTitle;

implementation

uses
  FormActions;

resourcestring
  ResStr_TitleLengthError = 'The maximum length of a title is 36 characters';
  ResStr_TitleForPrintOut = 'A title must be entered for the print-out';

{$R *.DFM}

//==============================================================================
procedure TdlgPrintTitle.bbOkClick(Sender: TObject);
begin
  If length( ePrintTitle.text ) > 36 then
  begin
    Modalresult := mrNone;
    raise EPrintTitleError.CreateValidation(ResStr_TitleLengthError, ePrintTitle );
  end
  else
  If (ePrintTitle.text = '') or (ePrintTitle.Text = null ) then
  begin
    ModalResult := mrNone;
    raise EPrintTitleError.CreateValidation(ResStr_TitleForPrintOut, ePrintTitle );
  end
  else
    ModalResult := mrOK;
end;

//==============================================================================
end.
 