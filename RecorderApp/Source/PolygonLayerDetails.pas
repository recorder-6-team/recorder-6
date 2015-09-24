//==============================================================================
//  Unit:        PolygonLayerDetails
//
//  Implements:  TdlgPolygonLayersDetails
//
//  Description: Allow users to add polygon layers to the map.
//
//  Author:      John van Breda
//  Created:     20 Dec 2001
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 4/01/08 13:42 $
//    $Author: Rickyshrestha $
//
//  $History: PolygonLayerDetails.pas $
//  
//  *****************  Version 7  *****************
//  User: Rickyshrestha Date: 4/01/08    Time: 13:42
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 6  *****************
//  User: Rickyshrestha Date: 18/12/07   Time: 13:08
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  ResStr_SpecifyPolygonName
//  
//  *****************  Version 5  *****************
//  User: Ericsalmon   Date: 16/01/04   Time: 12:07
//  Updated in $/JNCC/Development/Build/Source
//  Multiple maps development.
//  
//  *****************  Version 4  *****************
//  User: Pollyshaw    Date: 16/01/03   Time: 15:07
//  Updated in $/JNCC/Source
//  IR 77: Read only users can now reset the basemap and add background
//  layers.
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 14/06/02   Time: 11:40
//  Updated in $/JNCC/Source
//  Internal service request #3: Ok button modal result property set to
//  mrNone if polygon layer name is missing, so the dialog doesn't close
//  unexpectedly.
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit PolygonLayerDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColorBtn, ExtCtrls, Buttons, ExceptionForm, ImageListButton;

type
  EPolygonLayerDetail = class(TExceptionPath);

  TdlgPolygonLayerDetails = class(TForm)
    Bevel1: TBevel;
    lblPattern: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    cmbPattern: TComboBox;
    cbSelected: TColorButton;
    cbUnselected: TColorButton;
    Label1: TLabel;
    eName: TEdit;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    procedure btnOKClick(Sender: TObject);
    procedure cmbPatternDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbUnselectedChange(Sender: TObject);
  private
    FReadonly: boolean;
    procedure SetReadOnly(const Value: boolean);
  public
    constructor Create(AOwner : TComponent); override;
    property ReadOnly : boolean read FReadonly write SetReadOnly;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, Constants;

resourcestring
  ResStr_SpecifyPolygonName = 'Please specify a name for the polygon layer';
  ResStr_BtnOk = 'Ok';
  ResStr_BtnCancel = 'Cancel';


{$R *.DFM}

//==============================================================================
constructor TdlgPolygonLayerDetails.Create(AOwner: TComponent);
begin
  inherited;
  eName.SelectAll;
  cmbPattern.ItemIndex := 0;
  if AppSettings.UserAccessLevel <= ualAddOnly then ReadOnly := True;
end;  // Create

//==============================================================================
procedure TdlgPolygonLayerDetails.btnOKClick(Sender: TObject);
begin
  if eName.Text = '' then begin
    // We don't want to close the dialog until a name has been entered.
    ModalResult := mrNone;
    raise EPolygonLayerDetail.CreateValidation(ResStr_SpecifyPolygonName, eName);
  end;
end;  // btnOkClick

//==============================================================================
{Draw the pattern in the combo box}
procedure TdlgPolygonLayerDetails.cmbPatternDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var lPColor, lBColor: TColor;
    lBStyle: TBrushStyle;
begin
  inherited;
  with cmbPattern.Canvas do begin
    lPColor := Pen.Color;
    lBColor := Brush.Color;
    lBStyle := Brush.Style;

    Brush.Color := clWhite;
    FillRect(Rect);
    Pen.Color   := clWhite;
    Brush.Color := cbUnselected.ActiveColor;
    Brush.Style := TBrushStyle(Index);

    Rectangle(Rect.Left, Rect.Top, Rect.Right - 1, Rect.Bottom - 1);

    Pen.Color   := lPColor;
    Brush.Color := lBColor;
    Brush.Style := lBStyle;
  end;
end;  // cmbPatternDrawItem

//==============================================================================
procedure TdlgPolygonLayerDetails.SetReadOnly(const Value: boolean);
begin
  eName.Readonly       := Value;
  cmbPattern.Enabled   := not Value;
  cbSelected.Enabled   := not Value;
  cbUnSelected.Enabled := not Value;
  btnOK.Visible        := not Value;
  if Value then begin
    btnCancel.Caption := ResStr_BtnOk;
    btnCancel.ImageIndex := 0;
    btnCancel.Default := True;
  end else begin
    btnCancel.Caption := ResStr_BtnCancel;
    btnCancel.ImageIndex := 1;
    btnOK.Default := True;
  end;
  FReadonly := Value;
end;

procedure TdlgPolygonLayerDetails.cbUnselectedChange(Sender: TObject);
begin
  cmbPattern.Invalidate;
end;

end.
