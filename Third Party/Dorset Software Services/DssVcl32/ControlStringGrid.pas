//==============================================================================
//  Unit:        ControlStringGrid
//
//  Implements:  TControlStringGrid,
//
//  Description: String grid descendant that forwards messages to child controls
//               allowing things like buttons to be embedded and still receive
//               clicks
//
//               INTERNATIONALISED
//
//  Author:      John van Breda
//  Created:     Oct 2006
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 1 $
//    $Date: 2/10/06 9:29 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2006
//
//==============================================================================
unit ControlStringGrid;

interface

uses SysUtils, Classes, Grids, Windows, Messages, Controls;

type
  TControlStringGrid = class(TStringGrid)
  private
    procedure WMCommand( var msg: TWMCommand ); message WM_COMMAND;
  end;

implementation

{-------------------------------------------------------------------------------
  Forward messages to child controls
}
procedure TControlStringgrid.WMCommand(var msg: TWMCommand);
begin
  If EditorMode and ( msg.Ctl = InplaceEditor.Handle ) Then
    inherited
  Else
    If msg.Ctl <> 0 Then
      msg.result :=
        SendMessage( msg.ctl, CN_COMMAND,
                     TMessage(msg).wparam,
                     TMessage(msg).lparam );
end;

end.
