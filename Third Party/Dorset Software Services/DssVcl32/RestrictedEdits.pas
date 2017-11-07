{===============================================================================

  Copyright © Dorset Software Services Ltd, 2000

  Component:
    TRestrictedEditBase - John VB 04/02/2000
    TRestrictedEdit - John VB 04/02/2000
    TNumberEdit - John VB 04/02/2000
    TPhoneEdit - Eric Salmon 20/03/2000

  Updates:

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.

  Description:
    TRestrictedEditBase
      Abstract base class for providing an edit control that restricts its
      contents. A bit like a masked edit, but you can be more clever.
      Implement CheckText to return false if you don't want the current text -
      that's all you have to do!

    TRestrictedEdit
      Provides similar functionality, but uses an event to do the job rather
      than an abstract method

    TNumberEdit
      Provides a replacement edit control that allows you to control the data
      entered in a numerical format.  Like a mask edit, but copes with decimal
      points much better, and also allows a maximum value to be set.

    TPhoneEdit
      Provides functionality for phone numbers. Accepts only '+', '(', ')',
      '0'..'9' and ' '. A couple of rules apply:
        (1) '+' sign only allowed in first position.
        (2) Only one '(' allowed.
        (3) ')' allowed only if there already is a '(' entered.

  Additional information:
    TNumberEdit
      Current limitation - dose not cope with negative numbers.

===============================================================================}

unit RestrictedEdits;

interface

uses
  SysUtils, Windows, Classes, StdCtrls;

type
  TRestrictedEditBase = class(TEdit)
  private
    FPrevText : string;
    FPrevSelStart : integer;
    FPrevSelLength : integer;
  protected
    procedure KeyDown(var Key : word; Shift : TShiftState); override;
    procedure Change; override;
    function CheckText: boolean; virtual; abstract;
  end;  // TRestrictedEditBase

  //----------------------------------------------------------------------------
  TCheckTextEvent = procedure( Sender : TObject; const iText : string;
                               var ioAccept : boolean ) of object;

  TRestrictedEdit = class(TRestrictedEditBase)
  private
    FOnCheckText : TCheckTextEvent;
    procedure SetOnCheckText(const Value: TCheckTextEvent);
  protected
    function CheckText: boolean; override;
  published
    property OnCheckText : TCheckTextEvent read FOnCheckText write SetOnCheckText;
  end;  // TRestrictedEdit

  //----------------------------------------------------------------------------
  TNumberEdit = class(TRestrictedEditBase)
  private
    FDecimalPlaces: integer;         // max number of digits allowed after decimal place
    FMaximum: integer;
    procedure SetDecimalPlaces(const Value: integer);
    procedure SetMaximum(const Value: integer);
  protected
    function CheckText: boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Maximum : integer read FMaximum write SetMaximum default 100;
    property DecimalPlaces : integer read FDecimalPlaces write SetDecimalPlaces default 2;
  end;  // TNumberEdit

  //----------------------------------------------------------------------------
  TPhoneNumber = class(TRestrictedEdit)
  protected
    function CheckText:boolean; override;
  end;  // TPhoneNumber

//==============================================================================
implementation

//==============================================================================
{ TRestrictedEditBase }

{ Override KeyDown tp record the position in the edit control as you move
    around }
procedure TRestrictedEditBase.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FPrevSelStart := SelStart;
  FPrevSelLength := SelLength;
end;  // TRestrictedEditBase.KeyDown

//------------------------------------------------------------------------------
{ Prevent changes occurring if the number generated is invalid }
procedure TRestrictedEditBase.Change;
var
  lAbortChange : boolean;
  lPrevStart : integer;
  lPrevLength : integer;
begin
  inherited Change;
  if not (csDesigning in ComponentState) then
  begin
    lAbortChange := not CheckText;
    if (not lAbortChange) then
    begin
      FPrevText     := Text;
      FPrevSelStart := SelStart;
      FPrevSelLength:= SelLength;
    end else
    begin
      lPrevStart    := FPrevSelStart;
      lPrevLength   := FPrevSelLength;
      Text          := FPrevText;
      SelStart      := lPrevStart;
      SelLength     := lPrevLength;
      FPrevSelStart := lPrevStart;
      FPrevSelLength:= lPrevLength;
    end;
  end;
end;  // TRestrictedEditBase.Change

//==============================================================================
//==============================================================================
{ TRestrictedEdit }

function TRestrictedEdit.CheckText: boolean;
var lAccept : Boolean;
begin
  lAccept := True; //default
  if Assigned(FOnCheckText) then
    FOnCheckText( Self, Text, lAccept );
  Result := lAccept;
end;  // TRestrictedEdit.CheckText

//------------------------------------------------------------------------------
procedure TRestrictedEdit.SetOnCheckText(const Value: TCheckTextEvent);
begin
  FOnCheckText := Value;
end;  // TRestrictedEdit.SetOnCheckText

//==============================================================================
//==============================================================================
{ TNumberEdit }

// Constructor - sets up defaults for extra properties
constructor TNumberEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaximum := 100;
  FDecimalPlaces := 2;
end;  // TNumberEdit.Create

//------------------------------------------------------------------------------
procedure TNumberEdit.SetDecimalPlaces(const Value: integer);
begin
  FDecimalPlaces := Value;
end;  // TNumberEdit.SetDecimalPlaces

//------------------------------------------------------------------------------
procedure TNumberEdit.SetMaximum(const Value: integer);
begin
  FMaximum := Value;
end;  // TNumberEdit.SetMaximum

//------------------------------------------------------------------------------
{ Override abstract method of TRestrictedEditBase to prevent changes
     occurring if the number generated is invalid }
function TNumberEdit.CheckText: boolean;
var
  lValue : Double;
  lPointPosition : integer;
begin
  Result := True; //default
  if Text<>'' then
  begin
    try
      lValue := StrToFloat(Text);
    except
      on EConvertError do begin
        Result := false;
        Exit;
      end; // EConvertError
    end;
    if lValue>Maximum then
      Result := false;
    lPointPosition := Pos(DecimalSeparator, Text);
    if lPointPosition > 0 then
      if Length(Text) > lPointPosition + FDecimalPlaces then
         Result := false;
  end;
end;  // TNumberEdit.CheckText

//==============================================================================
//==============================================================================
{ TPhoneNumber }

function TPhoneNumber.CheckText:boolean;
var lOpenPos, lClosePos, lTextLength:integer;
begin
  Result:=true;
  if Text<>'' then begin
    lTextLength:=Length(Text);
    for lOpenPos:=1 to lTextLength do
      if not (Text[lOpenPos] in [' ','+','(',')','0'..'9']) then begin
        Result:=false;
        Exit;
      end;

    // Check '+'.
    if Pos('+',Text)>0 then begin
      // Found one. Allowed at Text[1] only, and no more than once in Text
      if (Text[1]<>'+') or (Pos('+',Copy(Text,2,lTextLength))>0) then begin
        Result:=false;
        Exit;
      end;
    end;
    // Check '(' and ')'. Find first occurrences to start with
    lOpenPos :=Pos('(',Text);
    lClosePos:=Pos(')',Text);
    // Must have '(' before ')'
    if (lOpenPos=0) and (lClosePos>0) then begin
      Result:=false;
      Exit;
    end;
    // Found '(', check for other '(' or ')'
    if lOpenPos>0 then begin
      // Want only one, and BEFORE any ')', and want only one ')' too
      if (Pos('(',Copy(Text,lOpenPos+1,lTextLength))>0) or
         ((lClosePos>0) and
          ((lClosePos<lOpenPos) or (Pos(')',Copy(Text,lClosePos+1,lTextLength))>0))) then begin
        Result:=false;
        Exit;
      end;
    end;
  end;
end;  // TPhoneNumber.CheckText

//==============================================================================


end.
