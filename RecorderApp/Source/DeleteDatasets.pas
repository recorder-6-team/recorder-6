//==============================================================================
//  Unit:        DeleteDatasets
//
//  Implements:  TdlgDeleteDatasets
//
//  Description:
//
//  Author:      John van Breda
//  Created:     9 Mar 2001
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 16/01/04 12:07 $
//    $Author: Ericsalmon $
//
//  $History: DeleteDatasets.pas $
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 16/01/04   Time: 12:07
//  Updated in $/JNCC/Development/Build/Source
//  Multiple maps development.
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 10:30
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit DeleteDatasets;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, (*Map, *)ImageListButton;

type
  TdlgDeleteDatasets = class(TForm)
    lbDatasets: TListBox;
    lblDatasets: TLabel;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure lbDatasetsClick(Sender: TObject);
  end;

//==============================================================================
implementation

uses
  FormActions;

{$R *.DFM}

//==============================================================================
procedure TdlgDeleteDatasets.lbDatasetsClick(Sender: TObject);
begin
  bbOK.Enabled := not (lbDatasets.ItemIndex = -1);
end;

//==============================================================================
end.
