{===============================================================================
  Unit:        KeyboardRapidTree

  Defines:     TKeyboardRapidTree
               TImageClickedEvent

  Description: Contains a control descended from TRapidTree with a number of
               improvements. An event OnImageClicked has been added, which
               called when an image is clicked or the space bar is pressed on
               an item. Finally, when an item's caption cannot be displayed
               because of restricted space, a tool tip is displayed. (Properly.)

  Model:       <none>

  Created:     January 2003

  Last revision information:
    $Revision: 12 $
    $Date: 21/01/08 12:21 $
    $Author: Johnvanbreda $

===============================================================================}

unit KeyboardRapidTree;

interface

uses
  SysUtils, Classes, Controls, exgrid, RapTree, Forms, Messages, Windows;

type
  TImageClickedEvent = procedure (Sender: TObject; Node: TFlyNode) of object;

  TKeyboardRapidTree = class(TRapidTree)
  private
    FOnImageClicked: TImageClickedEvent;
    FOnStateImageClicked: TImageClickedEvent;
    FHintWindow: THintWindow;
    FHasHTMLFormatting: boolean; //Hint that pops up when text too long
    procedure ReleaseHintWindow;
    function StripFormatting(const AText: string): string;
    procedure SetHasHTMLFormatting(const Value: boolean);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure CMMouseLeave (var Msg: TMessage); message CM_MOUSELEAVE;
  public
  published
    property OnImageClicked: TImageClickedEvent read FOnImageClicked write FOnImageClicked;
    property OnStateImageClicked: TImageClickedEvent read FOnStateImageClicked write FOnStateImageClicked;
    property HasHTMLFormatting: boolean read FHasHTMLFormatting write SetHasHTMLFormatting;
  end;

//==============================================================================
implementation

uses
  Types, ComCtrls, TreeColl, Graphics;

//==============================================================================
//When the control is clicked, check to see whether this is on the icon.
// if so call the OnImageClicked event.
procedure TKeyboardRapidTree.Click;
var ptPos: TPoint;
    HT   : THitTests;
begin
  inherited;
  if (Items.Count > 0) and Assigned(Selected) then begin
    ptPos := ScreenToClient(Mouse.CursorPos);
    HT    := GetHitTestInfoAt(ptPos.X, ptPos.Y);
    
    // Distinguish between state image and standard image.
    if (htOnIcon in HT) and Assigned(FOnImageClicked) then
      FOnImageClicked(Self, Selected)
    else
    if (htOnStateIcon in HT) and Assigned(FOnStateImageClicked) then
      FOnStateImageClicked(Self, Selected);
  end;
end;

//This is called when the mouse leaves the control. The hint should be hidden.
procedure TKeyboardRapidTree.CMMouseLeave(var Msg: TMessage);
begin
  ReleaseHintWindow;
end;

procedure TKeyboardRapidTree.ReleaseHintWindow;
begin
  if Assigned(FHintWindow) then
  begin
    FHintWindow.ReleaseHandle;
    FHintWindow := nil;
  end;
end;

//Calls FOnImageClicked if the key pressed is space.
procedure TKeyboardRapidTree.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_SPACE then
    if not Self.EditorMode and Assigned(Selected) then
      if Assigned(FonImageClicked) then
        FOnImageClicked(Self, Selected);
end;

//Shows the hintwindow with the caption if the caption is too long.
procedure TKeyboardRapidTree.MouseMove(Shift: TShiftState; X, Y: Integer);
var 
  lRect : TRect;
  lPoint : TPoint;
  lFlyNode : TFlyNode;
begin
  inherited;
  lFlyNode := GetNodeAt(X,Y);
  if Assigned(lFlyNode) then
    with lFlyNode do begin
      if (DisplayRect(True).Left <= X) and (DisplayRect(True).Right>=X) then
      begin
        //Find out if the text goes over the edge
        DrawText(Canvas.Handle, PChar(Caption), length(Caption),
          lRect, dt_calcrect);
        //If so, show hint. Assume text has 2 pixel margins.
        if lRect.Right - lRect.Left + 4 > DisplayRect(True).Right - DisplayRect(True).Left then
        begin
          lPoint := DisplayRect(True).TopLeft;
          lPoint := ClientToScreen(lPoint);
          //Show the window if it isn't already shown, or has an incorrect
          //position or caption.
          if not Assigned(FHintWindow) then
          begin // otherwise create a new one.
            FHintWindow := THintWindow.Create(Self);
            FHintWindow.Color := clInfoBk;
            FHintWindow.Font :=Canvas.Font;
            FHintWindow.Font.Color := clInfoText;
            FHintWindow.Caption := StripFormatting(Text);
            lRect.TopLeft := lPoint;
            lRect.Right := lRect.Left + FHintWindow.Width;
            lRect.Bottom := lRect.Top + Canvas.TextHeight('A') +6;
            FHintWindow.ActivateHint(lRect, Caption);
            //Make it the same size as it would be if the caption had been changed.
            FHintWindow.Height := Canvas.TextHeight('A') + 6;
            FHintWIndow.Invalidate;
          end else
          if (FHintWindow.Caption <> StripFormatting(Text)) or (FHintWindow.Top<>lPoint.Y) then
          begin
            FHintWindow.Caption := StripFormatting(Text);
            FHintWindow.Top := lPoint.Y;
            FHintWindow.Left := lPoint.X;
            FHintWindow.Height := Canvas.TextHeight('A') + 6;
            FHintWindow.Invalidate;
          end;
        end else
          //Otherwise, hide the hint.
          ReleaseHintWindow;
      end else
        ReleaseHintWindow;
    end
  else
    ReleaseHintWindow;
end;

{-------------------------------------------------------------------------------
  Remove HTML <i> and </i> tags from the hint string
}
function TKeyboardRapidTree.StripFormatting(const AText: string): string;
begin
  if HasHTMLFormatting then begin
    Result := StringReplace(AText, '<i>', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '</i>', '', [rfReplaceAll, rfIgnoreCase]);
  end
  else
    Result := AText;
end;

procedure TKeyboardRapidTree.SetHasHTMLFormatting(const Value: boolean);
begin
  FHasHTMLFormatting := Value;
end;

{-------------------------------------------------------------------------------
  Handle double clicks on the tree item captionss to expand (as opposed to
    clicking on the +/- buttons
}
procedure TKeyboardRapidTree.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lNode: TFlynode;
begin
  inherited;
  if ssDouble in Shift then begin
    lNode := GetNodeAt(X, Y);
    if Assigned(lNode) then
      lNode.Expand(False);
  end;
end;

end.
