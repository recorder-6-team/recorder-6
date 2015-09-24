//==============================================================================
//  Unit:        ExportField
//
//  Implements:  TdlgExportField
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 13 $
//    $Date: 27/12/07 17:26 $
//    $Author: Rickyshrestha $
//
//  $History: ExportField.pas $
//  
//  *****************  Version 13  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 17:26
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 12  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 16:10
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//   ResStr_EnterRange
//    ResStr_EnterMinValue
//    ResStr_EnterCutOffValue
//    ResStr_EnterMaxValue
//    ResStr_MaxValueGreaterThanMin
//    ResStr_EnterValidNumber
//    ResStr_EnterBetween2And9
//  
//  *****************  Version 11  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 10:41
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ExportField;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, Buttons, ExtCtrls, DataClasses, ImageListButton;

type
  TdlgExportField = class(TForm)
    cblbColumns: TCheckListBox;
    lblTitle: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label2: TLabel;
    lblMax: TLabel;
    lblMin: TLabel;
    eRanges: TEdit;
    eMax: TEdit;
    eMin: TEdit;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure cblbColumnsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure eRangesExit(Sender: TObject);
    procedure eMinExit(Sender: TObject);
    procedure eMaxExit(Sender: TObject);
    procedure cblbColumnsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cblbColumnsClickCheck(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cblbColumnsDblClick(Sender: TObject);
  private
    { Private declarations }
    FMeasurementUnitKey : TKeyString;
    FNumberOfRanges : integer;
    FMinValue : double;
    FMaxValue : double;
    FLeftMouseDown: Boolean;
    FCheckedIndex: integer;
    FContinue : Boolean;

    procedure PopulateList;
    procedure ClearList;
    procedure ValidateValue(iEdit: TEdit);
  protected
    procedure SetMeasurementUnitKey(iMeasurementUnitKey : TKeyString);
    procedure SetMaxValue(const Value: double);
    procedure SetMinValue(const Value: double);
    procedure SetNumberOfRanges(const Value: integer);
  public
    { Public declarations }
    property MeasurementUnitKey : TKeyString read FMeasurementUnitKey write SetMeasurementUnitKey;
    property NumberOfRanges : integer read FNumberOfRanges write SetNumberOfRanges;
    property MinValue : double read FMinValue write SetMinValue;
    property MaxValue : double read FMaxValue write SetMaxValue;
  end;

//==============================================================================
implementation

uses
  GeneralData, FormActions;

resourcestring
  ResStr_EnterRange = 'Please enter the number of ranges this data column is to be divided into.';
  ResStr_EnterMinValue = 'Please enter the minimum value for symbols for the selected data column.';
  ResStr_EnterCutOffValue = 'Please enter the cut-off value for symbols for the selected data column.';
  ResStr_EnterMaxValue =  'Please enter the maximum value for symbols for the selected data column.';
  ResStr_MaxValueGreaterThanMin = 'The maximum value for the range must be greater than the minimum value for the range.';
  ResStr_EnterValidNumber = 'Please enter a valid numeric value.';
  ResStr_EnterBetween2And9 = 'Please enter a value between 2 and 9.';
  //Captions
  ResStr_LBCutOffValue = 'Cut-off Value:';
  ResStr_LBMinValue = 'Min. Value:';

{$R *.DFM}

//==============================================================================
procedure TdlgExportField.cblbColumnsClick(Sender: TObject);
var
  iCount,iIndex:integer;
begin
  if FLeftMouseDown then
  begin
    with cblbColumns do begin
      iIndex:=ItemIndex;
      for iCount := 0 to Items.Count-1 do
        Checked[iCount]:=false;
      Checked[iIndex]:=true;
    end;
    FMeasurementUnitKey := TKey(cblbColumns.Items.Objects[iIndex]).Key;
  end;
end;

//==============================================================================
procedure TdlgExportField.PopulateList;
begin
  dmGeneralData.BuildMeasurementUnitList(cblbColumns.Items);
end;

//==============================================================================
procedure TdlgExportField.FormCreate(Sender: TObject);
begin
  FContinue := False;
  ClearList;
  PopulateList;
  if cblbColumns.Items.Count <> 0 then begin
    cblbColumns.ItemIndex := 0;
    cblbColumns.Checked[0] := True;
    FMeasurementUnitKey := TKey(cblbColumns.Items.Objects[0]).Key;
  end else begin // can't select anything - should never happen as database always contains abundance count
    bbOK.Enabled := False;
    eRanges.Enabled := False;
    eMin.Enabled := False;
    eMax.Enabled := False;
  end;
  FNumberOfRanges := 0;
  FMinValue := 0;
  FMaxValue := 0;
end;

//==============================================================================
procedure TdlgExportField.SetMeasurementUnitKey(
  iMeasurementUnitKey: TKeyString);
begin
  FMeasurementUnitKey := iMeasurementUnitKey;
end;

//==============================================================================
procedure TdlgExportField.FormDestroy(Sender: TObject);
begin
  ClearList;
end;

//==============================================================================
procedure TdlgExportField.ClearList;
var
  i : integer;
begin
  with cblbColumns do begin
    for i := Items.Count - 1 downto 0 do begin
      TKey(Items.Objects[i]).Free;
      Items.Delete(i);
    end;
  end;
end;

//==============================================================================
procedure TdlgExportField.bbOkClick(Sender: TObject);
var
  lMinValue, lMaxValue : double;
begin
  if eRanges.Text = '' then begin
    MessageDlg(ResStr_EnterRange, mtInformation, [mbOK], 0);
    eRanges.SetFocus;
    Exit;
  end;

  if eMin.Text = '' then begin
    if eMax.Visible then
      MessageDlg(ResStr_EnterMinValue, mtInformation, [mbOK], 0)
    else
      MessageDlg(ResStr_EnterCutOffValue, mtInformation, [mbOK], 0);
    eMin.SetFocus;
    Exit;
  end;

  lMinValue := StrToFloat(eMin.Text);
  if eMax.Visible then begin
    if eMax.Text = '' then begin
      MessageDlg(ResStr_EnterMaxValue, mtInformation, [mbOK], 0);
      eMax.SetFocus;
      Exit;
    end;

    lMaxValue := StrToFloat(eMax.Text);
    if (lMaxValue <= lMinvalue) then begin
      MessageDlg(ResStr_MaxValueGreaterThanMin, mtInformation, [mbOK], 0);
      eMax.SetFocus;
      Exit;
    end;
  end else
    lMaxValue := 0;

  FNumberOfRanges := StrToInt(eRanges.Text);
  FMinValue := lMinValue;
  FMaxValue := lMaxValue;

  FContinue := True;
  Close;
end;

//==============================================================================
procedure TdlgExportField.ValidateValue(iEdit: TEdit);
var
  lValue : integer;
//  lExtValue : extended;
begin
  lValue   :=0;
//  lExtValue:=0.0;
  if iEdit.Text <> '' then begin
    try
      if iEdit.Name = 'eRanges' then
        lValue := StrToInt(iEdit.Text);
//      else
//        lExtValue := StrToFloat(iEdit.Text);
    except
      on EConvertError do begin
        MessageDlg(ResStr_EnterValidNumber, mtInformation, [mbOK], 0);
        iEdit.SetFocus;
        Exit;
      end;
    end;
    if iEdit.Name = 'eRanges' then begin
      if (lValue >= 2) and (lValue <= 9) then begin
        if lValue = 2 then begin
          eMax.Visible := False;
          lblMax.Visible := False;
          lblMin.Caption := ResStr_LBCutOffValue ;
        end else begin
          eMax.Visible := True;
          lblMax.Visible := True;
          lblMin.Caption := ResStr_LBMinValue;
        end;
      end else begin
        MessageDlg(ResStr_EnterBetween2And9, mtInformation, [mbOK], 0);
        iEdit.SetFocus;
        Exit;
      end;
    end;
  end;
end;

//==============================================================================
procedure TdlgExportField.eRangesExit(Sender: TObject);
begin
  ValidateValue(eRanges);
end;

//==============================================================================
procedure TdlgExportField.eMinExit(Sender: TObject);
begin
  ValidateValue(eMin);
end;

//==============================================================================
procedure TdlgExportField.eMaxExit(Sender: TObject);
begin
  ValidateValue(eMax);
end;

//==============================================================================

procedure TdlgExportField.SetMaxValue(const Value: double);
begin
  FMaxValue := Value;
end;

//==============================================================================
procedure TdlgExportField.SetMinValue(const Value: double);
begin
  FMinValue := Value;
end;

//==============================================================================
procedure TdlgExportField.SetNumberOfRanges(const Value: integer);
begin
  FNumberOfRanges := Value;
end;

//==============================================================================
procedure TdlgExportField.cblbColumnsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  liIndex: integer;
begin
  FLeftMouseDown := False;
  if (Button = mbLeft) then
    FLeftMouseDown := True
  else
  begin
    {Store index of currently checked box}
    liIndex := 0;
    while not cblbColumns.Checked[liIndex] and (liIndex < cblbColumns.Items.Count-1) do
      Inc(liIndex);
    FCheckedIndex := liIndex;
  end;
end;

//==============================================================================
procedure TdlgExportField.cblbColumnsClickCheck(Sender: TObject);
var
  liCount: integer;
begin
  {Clicked-on Check box is already checked. Test for which button used to click
  and if right mouse button reset display before ONClick event}
  if not FLeftMouseDown then
  begin
    for liCount:= 0 to cblbColumns.Items.Count-1 do
      cblbColumns.Checked[liCount]:=False;
    cblbColumns.Checked[FCheckedIndex]  :=true;
  end //if
end;

//==============================================================================
procedure TdlgExportField.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FContinue then
    ModalResult := mrOK;
end;

//==============================================================================
{ This really is bizarre.  There is some code that happens on a check
  list box after the double click is fired, which toggles the checked status
  of the current option.  Therefore, to force it to stay on, we have to turn
  it off!  Weird! }
procedure TdlgExportField.cblbColumnsDblClick(Sender: TObject);
begin
  cblbColumns.Checked[cblbColumns.ItemIndex] := false;
end;

//==============================================================================
end.
