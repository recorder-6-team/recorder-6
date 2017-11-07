//==============================================================================
//  Unit: HotLabel
//
//  Implements:   THotLabel
//
//  Description: Descendant of TLabel that can display a 'hot' colour when
//               the mouse is over it
//
//  Author:      John van Breda
//  Created:     11/06/2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: $
//    $Date: $
//    $Author: $
//
//  $History: $
//
//  Copyright (c) Dorset Software Services Ltd. 2002
//
//==============================================================================
unit HotLabel;

interface

uses
  Classes, stdctrls, messages, Graphics, Controls;

type

  THotLabel = class(TLabel)
  private
    FHotColor: TColor;
    FOriginalColor : TColor;
    procedure SetHotColor(const Value: TColor);
  protected
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property HotColor : TColor read FHotColor write SetHotColor;
  end;


implementation


{ THotLabel }

{ Set the label 'hot' when the mouse moves in }
procedure THotLabel.CMMouseEnter(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then begin
    // remember the old colour and set the label hot
    FOriginalColor := Font.Color;
    Font.Color := FHotColor;
  end;
end;

{ And set the label cold when the mouse leaves it }
procedure THotLabel.CMMouseLeave(var Message: TMessage);
begin
  Font.Color := FOriginalColor;
end;

{ Constructor - just set default hot colour }
constructor THotLabel.Create(AOwner: TComponent);
begin
  inherited;
  FHotColor := clHighlight;
end;

{ Accessor method }
procedure THotLabel.SetHotColor(const Value: TColor);
begin
  FHotColor := Value;
end;

end.
