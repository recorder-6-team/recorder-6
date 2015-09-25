//==============================================================================
//  Unit:        ExtractGridSquares
//
//  Implements:  TdlgExtractGridSquares
//
//  Description: user interface for the ability to extract grid squares from
//               a location boundary into the GRID_SQUARES table
//
//  Created:     July 2006
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 12/02/09 17:33 $
//    $Author: Ericsalmon $
//==============================================================================
unit ExtractGridSquares;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, LayerLegend, GridSquareExtractor,
  LocationDetailsData, LocationDetails,
  ExceptionForm;

type
  TExtractGridSquaresException = class(TExceptionPath);

  TdlgExtractGridSquares = class(TForm)
    lblInformation: TLabel;
    gbPolygonOrLayer: TGroupBox;
    gbSize: TGroupBox;
    rbCurrentPolygon: TRadioButton;
    lblCurrentLocation: TLabel;
    rbLayer: TRadioButton;
    lblLayer: TLabel;
    cmbLayer: TComboBox;
    chk10km: TCheckBox;
    chk2km: TCheckBox;
    chk1km: TCheckBox;
    chk100m: TCheckBox;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    gbGridSquares: TGroupBox;
    rbIncludeSquares: TRadioButton;
    rbExcludeSquares: TRadioButton;
    procedure rbPolygonOrLayerClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkSizeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cmbLayerChange(Sender: TObject);
  private
    FBaseMapKey: string;
    FExtractor: TGridSquareExtractor;
    FCancelled: Boolean;
    FLayerLegend: TLayerLegend;
    FLocationKey: string;
    FLocationObjectID: Integer;
    FLocationSheetID: Integer;
    FBoundaryList: TBoundaryList;
    FBoundaryCounter: integer;
    FGridSquareList: TGridSquareList;
    procedure SetBaseMapKey(const Value: string);
    procedure SetLayerLegend(const Value: TLayerLegend);
    procedure SetLocationKey(const Value: string);
    procedure SetLocationName(const Value: string);
    procedure SetLocationObjectID(const Value: Integer);
    procedure SetLocationSheetID(const Value: Integer);
    procedure SetBoundaryList(const value: TBoundaryList);
    procedure SetOkButtonState;
    procedure SetRadioButtonSubControlState;
    procedure ThreadTerminate(Sender: TObject);
    procedure HideControls;
    procedure SingleExtractor(Sender: TObject);
    procedure MultiExtractor(Sender: TObject);
    procedure SetGridSquareList(const Value: TGridSquareList);
  public
    property GridSquareList: TGridSquareList read FGridSquareList write SetGridSquareList;
    property BoundaryList: TBoundaryList read FBoundaryList write SetBoundaryList;
    property BaseMapKey: string read FBaseMapKey write SetBaseMapKey;
    property LayerLegend: TLayerLegend read FLayerLegend write SetLayerLegend;
    property LocationKey: string read FLocationKey write SetLocationKey;
    property LocationName: string write SetLocationName;
    property Extractor:  TGridSquareExtractor read FExtractor;
    property LocationObjectID: Integer read FLocationObjectID write SetLocationObjectID;
    property LocationSheetID: Integer read FLocationSheetID write SetLocationSheetID;
  end;

implementation

uses
  BaseLegend, FormActions, Maintbar, GeneralFunctions;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Accessor.  Updates the combo box content to show the layers
}
procedure TdlgExtractGridSquares.SetLayerLegend(const Value: TLayerLegend);
var
  i: integer;
  lSheetID: integer;
begin
  FLayerLegend := Value;
  // keep track of the visible sheets
  lSheetID := 0;
  for i := 0 to FLayerLegend.Count-1 do begin
    if (FLayerLegend.LegendItems[i] is TPolygonLayerItem) and FLayerLegend.LegendItems[i].Visible then
      cmbLayer.Items.AddObject(FLayerLegend.LegendItems[i].Title, Ptr(lSheetID));
    if FLayerLegend.LegendItems[i].Visible then
      Inc(lSheetID);
  end;
  rbLayer.Enabled := cmbLayer.Items.Count <> 0;
  // if combo box selection is obvious then select the item
  if cmbLayer.Items.Count = 1 then
    cmbLayer.ItemIndex := 0;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TdlgExtractGridSquares.SetLocationKey(const Value: string);
begin
  FLocationKey := Value;
end;

{-------------------------------------------------------------------------------
  Set the location name label
}
procedure TdlgExtractGridSquares.SetLocationName(const Value: string);
begin
  lblCurrentLocation.Caption := '(' + Value + ')';
  rbCurrentPolygon.Enabled   := Value<>'';
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TdlgExtractGridSquares.SetLocationObjectID(const Value: Integer);
begin
  FLocationObjectID := Value;
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TdlgExtractGridSquares.SetLocationSheetID(const Value: Integer);
begin
  FLocationSheetID := Value;
end;

{-------------------------------------------------------------------------------
  Checking a radio button enables the appropriate sub controls
}
procedure TdlgExtractGridSquares.rbPolygonOrLayerClick(Sender: TObject);
begin
  SetRadioButtonSubControlState;
  SetOkButtonState;
end;

{-------------------------------------------------------------------------------
  Enables the Ok button only if a valid selection present in the dialog
}
procedure TdlgExtractGridSquares.SetOkButtonState;
begin
  if gbPolygonOrLayer.Visible then
    btnOk.Enabled :=
        (rbCurrentPolygon.Checked or (rbLayer.Checked and (cmbLayer.ItemIndex <> -1)))
        and (chk10km.Checked or chk2km.Checked or chk1km.Checked or chk100m.Checked)
 else
   btnOk.Enabled := (chk10km.Checked or chk2km.Checked or chk1km.Checked or chk100m.Checked);
end;

{-------------------------------------------------------------------------------
  Ok button invokes the grid square extraction
}
procedure TdlgExtractGridSquares.btnOkClick(Sender: TObject);
var
  lConfirmResult: TModalResult;
begin
  FBoundaryCounter:= -1;
  if chk100m.Checked then begin
    // 100m - slow so warn user
    lConfirmResult := ConfirmYesNoCancel(ResStr_100mAreYouSure);
    if lConfirmResult = mrNo then
      ModalResult := mrNone
    else
    if lConfirmResult = mrCancel then
      ModalResult := mrCancel;

    if lConfirmResult in [mrCancel, mrNo] then
      Exit; // procedure
  end;
  btnOk.Enabled            := False;
  gbPolygonOrLayer.Enabled := False;
  gbSize.Enabled           := False;
  FCancelled               := False;
  frmMain.SetStatus(ResStr_ExtractingGridSquares);
  Screen.Cursor := crHourglass;
  if gbPolygonOrLayer.Visible then
    SingleExtractor(nil)
  else
    MultiExtractor(nil);
end;

{----------------------------------------------------------------------
   deal with main application menubar
}
procedure TdlgExtractGridSquares.SingleExtractor(Sender: TObject);
begin
  FExtractor := TGridSquareExtractor.Create(true);
  with FExtractor do begin
    FreeOnTerminate       := True;
    OnTerminate           := ThreadTerminate;
    SetProgressMethod     := frmMain.SetProgress;
    IncludePartialSquares := rbIncludeSquares.Checked;
    BaseMapKey            := FBaseMapKey;
    if rbCurrentPolygon.Checked then begin
      SheetID  := FLocationSheetID;
      StaticID := FLocationObjectID;
    end else
      SheetID := Integer(cmbLayer.Items.Objects[cmbLayer.ItemIndex]);

    if chk10km.Checked then AddPrecision(10000);
    if chk2km.Checked  then AddPrecision(2000);
    if chk1km.Checked  then AddPrecision(1000);
    if chk100m.Checked then AddPrecision(100);
    Resume;
  end;
end;

{--------------------------------------------------------------------
    deal with the button in location details form CCN 284
}
procedure TdlgExtractGridSquares.MultiExtractor(Sender: TObject);
begin
  Inc(FBoundaryCounter);
  if (FBoundaryCounter< FBoundaryList.Count) then
  begin
    FExtractor := TGridSquareExtractor.Create(true);
    FExtractor.FreeOnTerminate       := True;
    FExtractor.GridListToAddTo       := FGridSquareList;
    FExtractor.SetProgressMethod     := frmMain.SetProgress;
    FExtractor.OnTerminate           := MultiExtractor;
    FExtractor.IncludePartialSquares := rbIncludeSquares.Checked;

    if chk10km.Checked then FExtractor.AddPrecision(10000);
    if chk2km.Checked  then FExtractor.AddPrecision(2000);
    if chk1km.Checked  then FExtractor.AddPrecision(1000);
    if chk100m.Checked then FExtractor.AddPrecision(100);

    with TBoundaryItem(FBoundaryList[FBoundaryCounter]) do
    begin
      if (MapSheetKey<>'') and (ObjectID>=0) then begin
        FExtractor.MapSheetKey := MapSheetKey;
        FExtractor.StaticID    := ObjectID;
        FExtractor.Resume;
      end;
    end; // with
  end else begin
    Screen.Cursor := crDefault;
    ModalResult   := mrOk;
    frmMain.TaskFinished;
  end;
end;

{-------------------------------------------------------------------------------
  Accessor - indicates the base map to bind to
}
procedure TdlgExtractGridSquares.SetBaseMapKey(const Value: string);
begin
  FBaseMapKey := Value;
end;

{-------------------------------------------------------------------------------
  Update a checkbox must update the Ok button state
}
procedure TdlgExtractGridSquares.chkSizeClick(Sender: TObject);
begin
  SetOkButtonState;
end;

{-------------------------------------------------------------------------------
  Show the form - initialise the GUI to a friendly state
}
procedure TdlgExtractGridSquares.FormShow(Sender: TObject);
begin
  // if only 1 radio option available then check it
  rbCurrentPolygon.Checked := rbCurrentPolygon.Enabled and (not rbLayer.Enabled);
  rbLayer.Checked          := rbLayer.Enabled and (not rbCurrentPolygon.Enabled);

  if ((not rbLayer.Enabled) and (not rbCurrentPolygon.Enabled)) then
    HideControls
  else
    SetRadioButtonSubControlState;
end;

{------------------------------------------------------------------------------
Hide a few controls for button on locationdetails form
CCN 284
}
procedure TdlgExtractGridSquares.HideControls;
begin
  lblInformation.Visible   := False;
  gbPolygonOrLayer.Visible := False;
  gbSize.Top               := 8;
  gbGridSquares.Top        := 96;
  btnOk.Top                := 180;
  btnCancel.Top            := 180;
  Self.Height              := 240;
  SetOkButtonState;
end;

{-------------------------------------------------------------------------------
  Enable or disable to controls attached to each radio button
}
procedure TdlgExtractGridSquares.SetRadioButtonSubControlState;
begin
  lblCurrentLocation.Enabled := rbCurrentPolygon.Checked;
  lblLayer.Enabled := rbLayer.Checked;
  cmbLayer.Enabled := rbLayer.Checked;
end;

{-------------------------------------------------------------------------------
  Allow the user to abort the process of scanning grid squares
}
procedure TdlgExtractGridSquares.btnCancelClick(Sender: TObject);
begin
  if Assigned(FExtractor) then
    if ConfirmYesNo(ResStr_GridSquareExtractCancelAreYouSure)=mrYes then begin
      FCancelled := true;
      FExtractor.Terminate; // this auto-frees it
      ShowInformation(ResStr_GridSquareExtractionCancelled);
    end else
      ModalResult := mrNone; // dialog shouldn't close
end;

{-------------------------------------------------------------------------------
  Completion of the thread causes the dialog to close
}
procedure TdlgExtractGridSquares.ThreadTerminate(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  ModalResult   := mrOk;
  frmMain.TaskFinished;
  if not FCancelled then begin
    if FExtractor.FoundPolygonsInLayer then
      ShowInformation(Format(ResStr_GridSquareExtractionComplete, [FExtractor.RecordsInserted]))
    else
      ShowInformation(ResStr_NoLocationsInLayer);
  end;
  Close;
end;

{-------------------------------------------------------------------------------
  Change the combo box might need to enable the Ok button
}
procedure TdlgExtractGridSquares.cmbLayerChange(Sender: TObject);
begin
  SetOkButtonState;
end;

{-------------------------------------------------------------------------
  Accessor
}
procedure TdlgExtractGridSquares.SetBoundaryList(const value: TBoundaryList);
begin
  FBoundaryList := value;
end;

{-------------------------------------------------------------------------
  Accessor
}
procedure TdlgExtractGridSquares.SetGridSquareList(const Value: TGridSquareList);
begin
  FGridSquareList := Value;
end;

end.
