{===============================================================================
  Unit:        VagueDateEdit

  Defines:     TVagueDateEdit

  Description: A specialised edit box for the diaplsy of vague dates.

  Created:

  Last revision information:
    $Revision: 12 $
    $Date: 11/02/08 16:31 $
    $Author: Johnvanbreda $

===============================================================================}

unit VagueDateEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, VagueDate;

type
  TVagueDateEdit = class(TCustomEdit)
  private
    FStartDate : TDateTime;
    FEndDate : TDateTime;
    FDateTypeString: String;
    function GetVagueDate: TVagueDate;
  protected
    procedure Change; override;
    function DateString(iDate: TDateTime): String;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property StartDate: TDateTime read FStartDate;
    property EndDate: TDateTime read FEndDate;
    property DateTypeString: String read FDateTypeString;
    property VagueDate: TVagueDate read GetVagueDate;
  published
    // As we inherit from TCustomEdit, we need to publish all relevant properties.
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

//==============================================================================
implementation

resourcestring
  ResStr_UnknownDate  =  'Date is unknown';
  ResStr_ShowDate     =  'Date is %s';
  ResStr_DateRangeUpto    =  'Dates range up to %s';
  ResStr_DateRangeFrom    =  'Dates range from %s';
  ResStr_DateRangeFromTo  =  'Dates range from %s to %s';

//==============================================================================
{ Set the controls hint property according to the vague date specified by the
    string }
procedure TVagueDateEdit.Change;
var
  lVagueDate: TVagueDate;
begin
  inherited Change;
  if Text = '' then begin
    Hint := '';
    // And reset the values, or we might get some nasty surprises.
    FStartDate      := 0;
    FEndDate        := 0;
    FDateTypeString := '';
  end else begin
    try
      lVagueDate := StringToVagueDate(Text);
    except
      on E:Exception do
      begin
        Hint := E.Message;
        Font.Color      := clBlack;
        FStartDate      := 0;
        FEndDate        := 0;
        FDateTypeString := '';
        Exit; // from procedure
      end;
    end;
    FStartDate      := lVagueDate.StartDate;
    FEndDate        := lVagueDate.EndDate;
    FDateTypeString := lVagueDate.DateTypeString;
    Font.Color := clGreen;

    if SameText(DateTypeString, 'U') then
      Hint := ResStr_UnknownDate
    else
    // Start/End same, no range
    if (FStartDate = FEndDate) then
      Hint := Format(ResStr_ShowDate, [DateString(FStartDate)])
    else
    // No start date, range up to EndDate
    if FStartDate = 0 then
      Hint := Format(ResStr_DateRangeUpto, [DateString(FEndDate)])
    else
    // No EndDate, range from StartDate
    if FEndDate = 0 then
      Hint := Format(ResStr_DateRangeFrom,[DateString(FStartDate)])
    else
    // Start/End dates different, range from StartDate to EndDate.
      Hint := Format(ResStr_DateRangeFromTo, [DateString(FStartDate), DateString(FEndDate)]);
  end;
end;  // Change

//==============================================================================
function TVagueDateEdit.DateString(iDate: TDateTime): String;
var
  lYear, lMonth, lDay: Word;
  lDateFormat : String;
begin
  Result      := '';
  lDateFormat := ShortDateFormat;
  DecodeDate(iDate, lYear, lMonth, lDay);
  if lYear <> 9999 then
    Result := DateToStr(iDate)
  else begin
    { Cut the year out of the date format }
    ShortDateFormat := Copy(ShortDateFormat, 1, Pos('y', ShortDateFormat) - 1);
    Result          := DateToStr(iDate) + '----';
    ShortDateFormat := lDateFormat;
  end;
end;  // DateString

//==============================================================================
function TVagueDateEdit.GetVagueDate: TVagueDate;
begin
  Result := StringToVagueDate(Text);
end;  // GetVagueDate

{-------------------------------------------------------------------------------
  Trap F11 keypresses to put current date in.
}
procedure TVagueDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key=VK_F11) and Enabled and (not ReadOnly) then
    Text := DateToStr(Now);
end;

end.
