{===============================================================================

  Copyright © Dorset Software Services Ltd, 2002

  Component:
    TImageListButton - Eric Salmon 15/04/2002

  Updates:
    Eric Salmon - 22/05/2002
    NumGlyphs and the size of the images in the ImageList are used to determine
    how to display the images on the button. Valid values for NumGlyphs range
    from 1 to 3. The images must be in the same order as for a TBitBtn:
      * Image 1: Up
      * Image 2: Disables
      * Image 3: Down/Clicked
    There is no need for a fourth state, as this is not applicable.
      * NumGlyphs = 1
          The "Disabled" state of the image will be done automatically by Delphi.
          The "Down/Clicked" state is the same as the "Up" state.
      * NumGlyphs = 2
          The "Disabled" state is assumed to be at (ImageIndex * NumGlyphs) + 1 in
          the image list.
          The "Down/Clicked" state is the same as the "Up" state.
      * NumGlyphs = 3
          The "Disabled" state is assumed to be at (ImageIndex * NumGlyphs) + 1 in
          the image list.
          The "Down/Clicked" state is assumed to be at (ImageIndex * NumGlyphs) + 2
          in the image list.

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.
    Inhouse7, Delphi 7 package for in house components.

  Description:
    TImageListButton
      Similar to BitBtn and SpeedBtn, but uses an ImageList as the source for
      the glyph.  The glyph itself is not saved/streamed into the DFM file,
      making it more resource efficient.

===============================================================================}

unit ImageListButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  StdCtrls, Buttons, ImgList, ActnList, Themes;

type
  TILNumGlyphs = 1..3;

  TImageListButton = class(TButton)
  private
    FCanvas: TCanvas;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FIsFocused: Boolean;
    FImageIndex: TImageIndex;
    FImageList: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FNumGlyphs: TILNumGlyphs;
    FDown: Boolean;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    function GetNumGlyphs: TILNumGlyphs;
    procedure SetNumGlyphs(Value: TILNumGlyphs);
    procedure ImageListChange(Sender: TObject);

    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CalcButtonLayout(AImageList: TCustomImageList; const AImageIndex: TImageIndex;
      Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; var GlyphPos: TPoint;
      var TextBounds: TRect; BiDiFlags: Integer);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: Integer);
    function DrawGlyph(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
      BiDiFlags: Integer): TRect;
    procedure SetDown(const Value: boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure Change; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Caption;
    property Constraints;
    property Default;
    property Down: Boolean read FDown write SetDown default False;
    property Enabled;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult;
    property NumGlyphs: TILNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
  end;

//==================================================================================================
implementation

//==================================================================================================
//==================================================================================================
{ TImageListButton }
//==================================================================================================

constructor TImageListButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin  := -1;
  ControlStyle := ControlStyle + [csReflector];
  FNumGlyphs  := 1;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;  // Create

//==================================================================================================
destructor TImageListButton.Destroy;
begin
  FImageChangeLink.Free;
  FCanvas.Free;
  inherited Destroy;
end;  // Destroy

//==================================================================================================
procedure TImageListButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;  // CreateParams

//==================================================================================================
procedure TImageListButton.SetDown(const Value: boolean);
begin
  FDown := Value;
  Invalidate;
end; // SetDown

//==================================================================================================
procedure TImageListButton.SetImageIndex(Value: TImageIndex);
begin
  FImageIndex := Value;
  Invalidate;
end;  // SetImageIndex

//==================================================================================================
procedure TImageListButton.SetImageList(const Value: TCustomImageList);
begin
  if ImageList <> nil then ImageList.UnRegisterChanges(FImageChangeLink);
  FImageList := Value;
  Invalidate;
end;  // SetImageList

//==================================================================================================
function TImageListButton.GetNumGlyphs: TILNumGlyphs;
begin
  Result := FNumGlyphs;
end;  // GetNumGlyphs

//==================================================================================================
procedure TImageListButton.SetNumGlyphs(Value: TILNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 3 then Value := 3;
  if Value <> FNumGlyphs then
    FNumGlyphs := Value;
  Invalidate;
end;  // SetNumGlyphs

//==================================================================================================
procedure TImageListButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  Invalidate;
end;  // ActionChange

//==================================================================================================
procedure TImageListButton.ImageListChange(Sender: TObject);
begin
  if Sender = ImageList then Change;
end;  // ImageListChange

//==================================================================================================
procedure TImageListButton.Change;
begin
  if csDesigning in ComponentState then
  begin
    if (Owner is TForm) and (TForm(Owner).Designer <> nil) then
      TForm(Owner).Designer.Modified;
  end;
end;  // Change

//==================================================================================================
procedure TImageListButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = ImageList then
      ImageList := nil;
end;  // Notification

//==================================================================================================
procedure TImageListButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;  // CMEnabledChanged

//==================================================================================================
procedure TImageListButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;  // CMFontChanged

//==================================================================================================
procedure TImageListButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    ItemWidth := Width;
    ItemHeight := Height;
  end;
end;  // CNMeasureItem

//==================================================================================================
procedure TImageListButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> FIsFocused then
  begin
    FIsFocused := ADefault;
    Refresh;
  end;
end;  // SetButtonStyle

//==================================================================================================
procedure TImageListButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;  // SetLayout

//==================================================================================================
procedure TImageListButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;  // SetMargin

//==================================================================================================
procedure TImageListButton.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;  // SetSpacing

//==================================================================================================
procedure TImageListButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;  // WMLButtonDblClk

//==================================================================================================
procedure TImageListButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;  // CNDrawItem

//==================================================================================================
procedure TImageListButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  lIsDown, lIsDefault: Boolean;
  lState: TButtonState;
  R: TRect;
  lFlags: Longint;
  lDetails: TThemedElementDetails;
  lButton: TThemedButton;
  lOffset: TPoint;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Font := Self.Font;
    lIsDown := ItemState and ODS_SELECTED <> 0;
    lIsDefault := ItemState and ODS_FOCUS <> 0;

    if not Enabled then
      lState := bsDisabled
    else if Down or lIsDown then
      lState := bsDown
    else
      lState := bsUp;
  end;

  if ThemeServices.ThemesEnabled then
  begin
    if not Enabled then
      lButton := tbPushButtonDisabled
    else
      if lState = bsDown then
        lButton := tbPushButtonPressed
//      else if FMouseInControl then       // If implementing hottracking...
//        lButton := tbPushButtonHot
      else if FIsFocused or lIsDefault then
        lButton := tbPushButtonDefaulted
      else
        lButton := tbPushButtonNormal;

    lDetails := ThemeServices.GetElementDetails(lButton);
    // Parent background.
    ThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @lDetails, True);
    // Button shape.
    ThemeServices.DrawElement(DrawItemStruct.hDC, lDetails, DrawItemStruct.rcItem);
    R := ThemeServices.ContentRect(FCanvas.Handle, lDetails, DrawItemStruct.rcItem);

    if lButton = tbPushButtonPressed then
      lOffset := Point(1, 0)
    else
      lOffset := Point(0, 0);

    DrawGlyph(FCanvas, R, lOffset, Caption, FLayout, FMargin,
              FSpacing, lState, False, DrawTextBiDiModeFlags(0));

    if FIsFocused and lIsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end else begin
    lFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if lState = bsDown then lFlags := lFlags or DFCS_PUSHED;
    if DrawItemStruct.ItemState and ODS_DISABLED <> 0 then
      lFlags := lFlags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if FIsFocused or lIsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;
    { DrawFrameControl does not draw a pressed button correctly }
    if lState = bsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, lFlags);

    if FIsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if lState = bsDown then
      OffsetRect(R, 1, 1);

    DrawGlyph(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
              FSpacing, lState, False, DrawTextBiDiModeFlags(0));

    if FIsFocused and lIsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;
  FCanvas.Handle := 0;
end;  // DrawItem

//==================================================================================================
function TImageListButton.DrawGlyph(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
  lImageList: TCustomImageList;
  lImageIndex: TImageIndex;
begin
  // Work out which ImageList to use, if any
  lImageIndex := -1;
  if not Assigned(Action) and not Assigned(ImageList) then
    lImageList := nil
  else
    // Which imagelist to use, Action's or the other
    if Assigned(Action) then begin
      with TCustomAction(Action) do begin
        // If no image to display, exit
        if ImageIndex = -1 then
          lImageList := nil
        else if not Assigned(ActionList.Images) then
          lImageList := nil
        else begin
          lImageList := ActionList.Images;
          lImageIndex := ImageIndex;
        end;
      end;
    end else begin
      // If no image to display, exit
      if ImageIndex = -1 then
        lImageList := nil
      else begin
        lImageList := FImageList;
        lImageIndex := FImageIndex;
      end;
    end;
  // Do the rest
  CalcButtonLayout(lImageList, lImageIndex, Canvas, Client, Offset, Caption,
                   Layout, Margin, Spacing, GlyphPos, Result, BiDiFlags);

  if Assigned(lImageList) then
    case State of
      bsUp:
          lImageList.Draw(Canvas, GlyphPos.X, GlyphPos.Y, lImageIndex * FNumGlyphs);
      bsDisabled:
          if FNumGlyphs = 1 then
            lImageList.Draw(Canvas, GlyphPos.X, GlyphPos.Y, lImageIndex, False)
          else
            lImageList.Draw(Canvas, GlyphPos.X, GlyphPos.Y, lImageIndex * FNumGlyphs + 1, False);
      bsDown:
          if FNumglyphs = 3 then
            lImageList.Draw(Canvas, GlyphPos.X, GlyphPos.Y, lImageIndex * FNumGlyphs + 2)
          else
            lImageList.Draw(Canvas, GlyphPos.X, GlyphPos.Y, lImageIndex * FNumGlyphs);
    end;
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;  // DrawGlyph

//==================================================================================================
// Taken from Buttons.pas and adapted
procedure TImageListButton.CalcButtonLayout(AImageList: TCustomImageList; const AImageIndex: TImageIndex;
  Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
  Layout: TButtonLayout; Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight else
    if Layout = blGlyphRight then Layout := blGlyphLeft;

  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  // Glyph size
  if Assigned(AImageList) and (AImageIndex in [0..AImageList.Count - 1]) then
    if AImageList.Width <> AImageList.Height then
      GlyphSize := Point(AImageList.Width div FNumGlyphs, AImageList.Height)
    else
      GlyphSize := Point(AImageList.Width, AImageList.Height)
  else
    GlyphSize := Point(0, 0);

  // Caption size
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end else begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end else begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end else begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end else begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y - (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  // Fixup according to layout  
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.X);
end;  // CalcButtonLayout

//==================================================================================================
// Taken from Buttons.pas
procedure TImageListButton.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
               DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
               DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
               DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;  // DrawButtonText


end.
