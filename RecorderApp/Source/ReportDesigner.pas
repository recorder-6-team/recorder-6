{$I QRDESIGN.INC}
unit ReportDesigner;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, DBTables, Menus, Buttons, IniFiles,
  DBCtrls, Grids, DBGrids, QRDOpt, QRDProcs, OnlineHelp,
  LabProp, DBTxProp, ShapProp, MemoProp, SysDProp, DBImProp, ImgProp, BandProp,
  ChldProp, SubProp, GrpProp, ExprProp, FramePro, QRDConst,
  {$IFDEF WIN32}
  {$IFNDEF WPTools}
  RichProp, DbRtfPro,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WPTools}
  RichPrWP, DbRtfPWP,
  {$ENDIF}
  QrdTools, QrdExprt, Printers, csProp, Quickrpt, Qrctrls, QRPrntr, Mask,
  QRDCtrls, ComCtrls, ToolWin, BaseChildUnit, JnccQRDesign, QRDesign,
  BaseFormUnit, Constants, XPMenu, JnccQRDesignReg7;

type
  TfrmReportDesigner = class(TBaseChild)
    dlgSave: TSaveDialog;
    StatusPanel: TPanel;
    NewElementMI: TMenuItem;
    NewBandMI: TMenuItem;
    NewLabelMI: TMenuItem;
    NewDatafieldMI: TMenuItem;
    NewCalculatedfieldMI: TMenuItem;
    NewSystemdataMI: TMenuItem;
    NewShapeMI: TMenuItem;
    NewImageMI: TMenuItem;
    NewImagefromTableMI: TMenuItem;
    NewMemoMI: TMenuItem;
    N3: TMenuItem;
    CutMI: TMenuItem;
    CopyMI: TMenuItem;
    PasteMI: TMenuItem;
    N5: TMenuItem;
    AlignMI: TMenuItem;
    AlignLeftMI: TMenuItem;
    AlignRightMI: TMenuItem;
    AlignTopMI: TMenuItem;
    AlignBottomMI: TMenuItem;
    CenterMI: TMenuItem;
    AlignCenterHorizontalMI: TMenuItem;
    AlignCenterVerticalMI: TMenuItem;
    ParentCenterMI: TMenuItem;
    AlignCenterBandHorizontalMI: TMenuItem;
    AlignCenterBandVerticalMI: TMenuItem;
    EqualspaceMI: TMenuItem;
    AlignEqualHorizontalMI: TMenuItem;
    AlignEqualVerticalMI: TMenuItem;
    N6: TMenuItem;
    SendtobackMI: TMenuItem;
    Bringtofront1: TMenuItem;
    mnuReportOptions: TMenuItem;
    DeleteMI: TMenuItem;
    pmPropertyPanel: TPopupMenu;
    pmPropertyTop: TMenuItem;
    pmPropertyBottom: TMenuItem;
    ElementPanel: TPanel;
    NormalSpeedButton: TSpeedButton;
    ToolbarPanel: TPanel;
    SendToBackButton: TSpeedButton;
    BandSpeedButton: TSpeedButton;
    LabelSpeedButton: TSpeedButton;
    DBTextSpeedButton: TSpeedButton;
    MemoSpeedButton: TSpeedButton;
    DBImageSpeedButton: TSpeedButton;
    ImageSpeedButton: TSpeedButton;
    ShapeSpeedButton: TSpeedButton;
    SysdataSpeedButton: TSpeedButton;
    CutSpeedButton: TSpeedButton;
    CopySpeedButton: TSpeedButton;
    PasteSpeedButton: TSpeedButton;
    BringToFrontButton: TSpeedButton;
    pmPropertyVisible: TMenuItem;
    N10: TMenuItem;
    LeftButton: TSpeedButton;
    CenterHorizontalButton: TSpeedButton;
    RightButton: TSpeedButton;
    HorizontalEqualButton: TSpeedButton;
    ParentCenterHorizontalButton: TSpeedButton;
    TopButton: TSpeedButton;
    CenterVerticalButton: TSpeedButton;
    BottomButton: TSpeedButton;
    VerticalEqualButton: TSpeedButton;
    ParentCenterVerticalButton: TSpeedButton;
    mnuViewToolbar: TMenuItem;
    mnuViewFontToolbar: TMenuItem;
    mnuViewElementToolbar: TMenuItem;
    mnuViewStatusline: TMenuItem;
    N7: TMenuItem;
    mnuViewOptions: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    XPosLabel: TLabel;
    YPosLabel: TLabel;
    Label1: TLabel;
    pmElementPanel: TPopupMenu;
    pmElementPanelRight: TMenuItem;
    pmElementPanelLeft: TMenuItem;
    N11: TMenuItem;
    pmElementPanelVisible: TMenuItem;
    SubdetailSpeedButton: TSpeedButton;
    ChildSpeedbutton: TSpeedButton;
    GroupSpeedButton: TSpeedButton;
    RichtextSpeedButton: TSpeedButton;
    DBRichtextSpeedButton: TSpeedButton;
    N8: TMenuItem;
    mnuViewZoomFactor: TMenuItem;
    mnuZoomDefault: TMenuItem;
    mnuZoomIn: TMenuItem;
    mnuZoomOut: TMenuItem;
    Childband1: TMenuItem;
    Subdetailband1: TMenuItem;
    Groupband1: TMenuItem;
    QRDPanel: TPanel;
    Scrollbox1: TEventScrollbox;
    QuickReport: TDesignQuickReport;
    ScrollBox2: TScrollBox;
    RulerPanel1: TRulerPanel;
    ElementsComboBox: TComboBox;
    UndoMI: TMenuItem;
    N4: TMenuItem;
    mnuViewRotatebands: TMenuItem;
    mnuViewHidebands: TMenuItem;
    N9: TMenuItem;
    mnuViewResetbands: TMenuItem;
    mnuViewDatafieldsListbox: TMenuItem;
    PropertyPanel: TPanel;
    Bevel2: TBevel;
    TextLeftAlignButton: TSpeedButton;
    TextCenterAlignButton: TSpeedButton;
    TextRightAlignButton: TSpeedButton;
    TypeLabel: TLabel;
    FrameSpeedButton: TSpeedButton;
    FontColorSpeedbutton: TSpeedButton;
    AllEdit: TEdit;
    AllComboBox: TComboBox;
    FontComboBox: TComboBox;
    FontSizeComboBox: TComboBox;
    Bevel1: TBevel;
    QRepDesigner: TJnccQRepDesigner;
    mnuEdit: TMenuItem;
    mnuView: TMenuItem;
    mnuReport: TMenuItem;
    dlgOpen: TOpenDialog;
    ExpressionSpeedButton: TSpeedButton;
    procedure BandSpeedButtonClick(Sender: TObject);
    procedure LabelSpeedButtonClick(Sender: TObject);
    procedure DBTextSpeedButtonClick(Sender: TObject);
    procedure SysDataSpeedButtonClick(Sender: TObject);
    procedure ShapeSpeedButtonClick(Sender: TObject);
    procedure OptionsSpeedButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageSpeedButtonClick(Sender: TObject);
    procedure DBImageSpeedButtonClick(Sender: TObject);
    procedure MemoSpeedButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure QRepDesignerxEditProperties(QRD: TQRepDesigner;
      Component: TComponent; var EditResult: Boolean);
    procedure QRepDesignerxSelectionChanged(QRD: TQRepDesigner;
      NumberOfSelections: Integer);
    procedure Scrollbox1ScrollHorz(Sender: TObject);
    procedure pmPropertyTopClick(Sender: TObject);
    procedure pmPropertyBottomClick(Sender: TObject);
    procedure AllEditChange(Sender: TObject);
    procedure AllComboBoxChange(Sender: TObject);
    procedure mnuViewToolbarClick(Sender: TObject);
    procedure mnuViewFontToolbarClick(Sender: TObject);
    procedure mnuViewElementToolbarClick(Sender: TObject);
    procedure QRepDesignerxShowInfo(Component: TComponent;
      var Info: QRDShowInfoString);
    procedure pmElementPanelRightClick(Sender: TObject);
    procedure pmElementPanelLeftClick(Sender: TObject);
    procedure LabelSpeedButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SubdetailSpeedButtonClick(Sender: TObject);
    procedure ChildSpeedbuttonClick(Sender: TObject);
    procedure GroupSpeedButtonClick(Sender: TObject);
    procedure RichtextSpeedButtonClick(Sender: TObject);
    procedure DBRichtextSpeedButtonClick(Sender: TObject);
    procedure mnuZoomDefaultClick(Sender: TObject);
    procedure mnuZoomInClick(Sender: TObject);
    procedure mnuZoomOutClick(Sender: TObject);
    procedure FrameSpeedButtonClick(Sender: TObject);
    procedure Panel2DblClick(Sender: TObject);
    procedure ElementsComboBoxDropDown(Sender: TObject);
    procedure ElementsComboBoxChange(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure mnuViewOptionsClick(Sender: TObject);
    procedure mnuViewStatuslineClick(Sender: TObject);
    procedure mnuViewRotatebandsClick(Sender: TObject);
    procedure mnuViewResetbandsClick(Sender: TObject);
    procedure mnuViewHidebandsClick(Sender: TObject);
    procedure mnuViewDatafieldsListboxClick(Sender: TObject);
    procedure FontComboBoxChange(Sender: TObject);
    procedure FontColorSpeedbuttonClick(Sender: TObject);
    procedure mnuViewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure QRepDesignerAfterReportComponentCreated(
      Control: TCustomControl);
    procedure NewCalculatedfieldMIClick(Sender: TObject);
    procedure ExpressionSpeedButtonClick(Sender: TObject);
  private
    FPreviewCancelled: Boolean;
    FstReportFileName: String;
    procedure SetReportFileName(Value: String);
  protected
  public
    { Public declarations }
    property ReportFileName: String read FstReportFileName write SetReportFileName;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure PreviewScreen; override;    
    procedure RefreshFontCombobox;
    procedure SaveDesign;
    procedure SaveDesignAs;
    procedure PrintScreen; override;
    procedure ShowDataFieldList;
    procedure OpenReport;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, GeneralData, FormActions, Maintbar, FilterResult,
  GeneralFunctions, StrUtils;

resourcestring
  ResStr_ReportAffect = 'Please note that any report using this same template will be affected.';
  ResStr_ReportDesigner = 'Report Designer - %s';

{$R *.DFM}

//==============================================================================
procedure TfrmReportDesigner.FormCreate(Sender: TObject);
begin
  inherited;
  TypeLabel.Caption:='';
  VertScrollBar.Visible:=false;
  HorzScrollBar.Visible:=false;
  VertScrollBar.Range:=0;
  HorzScrollBar.Range:=0;
  XPosLabel.Caption:='';
  YPosLabel.Caption:='';
  ElementsComboBox.Items.Clear;
  RulerPanel1.Zoom:=Quickreport.Zoom;
  RulerPanel1.Units:=Quickreport.Units;
  UndoMI.Visible:=QRepDesigner.UndoEnabled;
  N3.Visible:=QRepDesigner.UndoEnabled;

  {--- hide element speed buttons for unavailable components ---}
  {$IFNDEF WIN32}
  {$IFNDEF WPTools}
  RichTextSpeedButton.Visible:=false;
  DBRichTextSpeedButton.Visible:=false;
  {$ENDIF}
  {$ENDIF}

  //Help Setup
  HelpContext := IDH_REPORTDESIGN;
  mnuEdit.HelpContext := IDH_EDITMENU;
  mnuView.HelpContext := IDH_VIEWMENU;
  mnuReport.HelpContext := IDH_REPORTSMENU;

end;  // FormCreate

//==============================================================================
procedure TfrmReportDesigner.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(true);
  { Need to unlock window as clear context toolbar locks it }
  LockWindowUpdate(0);
  dmFormActions.actPrint.Enabled  :=true;
  dmFormActions.actPreview.Enabled:=true;
  frmMain.mnuFileOpen.Visible := true;
  frmMain.mnuFileSave.Enabled     :=true;
  frmMain.mnuFileSaveAs.Enabled   :=true
end;  // FormActivate

//==============================================================================
procedure TfrmReportDesigner.FormDeactivate(Sender: TObject);
begin
  inherited;
  dmFormActions.actPrint.Enabled  :=false;
  dmFormActions.actPreview.Enabled:=false;
  frmMain.mnuFileOpen.Visible := false;
  frmMain.mnuFileSave.Enabled     :=false;
  frmMain.mnuFileSaveAs.Enabled   :=false;
end;  // FormDeactivate

//==============================================================================
procedure TfrmReportDesigner.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var lAnswer:integer;
    lMessage: string;
begin
  if QRepDesigner.HasChanged then begin
    lMessage := Format(QRepDesigner.RuntimeMessages.Values['Save report'], [ReportFileName]);
    if ReportFileName <> '' then
      lMessage := lMessage + #13#13 +ResStr_ReportAffect;
    lAnswer := MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if lAnswer=mrCancel then begin
      CanClose:=false;
      Exit;
    end;
    if lAnswer=mrYes then begin
      if ReportFileName='' then begin
        if AppSettings.ReportTemplatePath='' then
          dlgSave.InitialDir:=ExtractFilePath(Application.ExeName)
        else
          dlgSave.InitialDir:=AppSettings.ReportTemplatePath;
        if dlgSave.Execute then begin
          ReportFileName:=dlgSave.FileName;
          QRepDesigner.SaveReport(ReportFileName);
          CanClose:=true;
        end else
          CanClose:=false;
      end else begin
        QRepDesigner.SaveReport(ReportFileName);
        CanClose:=true;
      end;
    end else
      CanClose:=true;
  end;
  // Answered once, set to not changed now.
  QRepDesigner.HasChanged:=false;
end;  // FormCloseQuery

//==============================================================================
procedure TfrmReportDesigner.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  action := caFree;
end;  // FormClose;

//==============================================================================
procedure TfrmReportDesigner.PreviewScreen;
begin
  QuickReport.Preview;
end;  // PreviewScreen

//==============================================================================
procedure TfrmReportDesigner.PrintScreen;
begin
  QuickReport.Print;
end;  // PrintScreen

//==============================================================================
procedure TfrmReportDesigner.BandSpeedButtonClick(Sender: TObject);
begin
  BandSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddBand;
end;  // BandSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.ChildSpeedbuttonClick(Sender: TObject);
begin
  ChildSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddChildband;
end;  // ChildSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.SubdetailSpeedButtonClick(Sender: TObject);
begin
  SubDetailSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddSubDetail;
end;  // SubDetailSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.GroupSpeedButtonClick(Sender: TObject);
begin
  GroupSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddGroup;
end;  // GroupSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.LabelSpeedButtonClick(Sender: TObject);
begin
  LabelSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddLabel;
end;  // LabelSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.MemoSpeedButtonClick(Sender: TObject);
begin
  MemoSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddMemo;
end;  // MemoSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.RichtextSpeedButtonClick(Sender: TObject);
begin
  RichtextSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddRichtext;
end;  // RichTextSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.ImageSpeedButtonClick(Sender: TObject);
begin
  ImageSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddImage;
end;  // ImageSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.ShapeSpeedButtonClick(Sender: TObject);
begin
  ShapeSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddShape;
end;  // ShapeSpeedButtonClick


//==============================================================================
{ Expression speed button added by JVB to support existing unlinked
             functionality - 06/02/02 }
procedure TfrmReportDesigner.ExpressionSpeedButtonClick(Sender: TObject);
begin
  ExpressionSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddExpression;
end;

//==============================================================================
procedure TfrmReportDesigner.SysDataSpeedButtonClick(Sender: TObject);
begin
  SysDataSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddSysData;
end;  // SysDataSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.DBTextSpeedButtonClick(Sender: TObject);
begin
  DBTextSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddDBText;
end;  // DBTextSpeedButtonClick


//==============================================================================
procedure TfrmReportDesigner.NewCalculatedfieldMIClick(Sender: TObject);
begin
  ExpressionSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddExpression;
end;


//==============================================================================
procedure TfrmReportDesigner.DBImageSpeedButtonClick(Sender: TObject);
begin
  DBImageSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddDBImage;
end;  // DBImageSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.DBRichtextSpeedButtonClick(Sender: TObject);
begin
  DBRichtextSpeedButton.Down:=true;
  QRepDesigner.QRDesignState:=qrds_AddDBRichtext;
end;  // DBRichTextSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.FontColorSpeedbuttonClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do begin
    if Execute then QRepDesigner.SetSelectedElementsFontColor(Color);
    Free;
  end;
end;  // FontColorSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.FrameSpeedButtonClick(Sender: TObject);
begin
  EditFrameProperty(QRepDesigner);
end;  // FrameSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.OptionsSpeedButtonClick(Sender: TObject);
begin
  EditReportOptions(QRepDesigner, QuickReport);
end;  // OptionsSpeedButtonClick

//==============================================================================
procedure TfrmReportDesigner.SaveDesign;
begin
  if ReportFileName='' then SaveDesignAs
                       else QRepDesigner.SaveReport(ReportFileName);
end;  // SaveDesign

//==============================================================================
procedure TfrmReportDesigner.SaveDesignAs;
begin
  with dlgSave do begin
    if FileExists(ReportFileName) then
      FileName := ReportFileName
    else
    begin
      InitialDir := AppSettings.ReportTemplatePath;
      FileName := 'Template.tpl';
    end;
    if Execute then begin
      ReportFileName:=FileName;
      QRepDesigner.SaveReport(ReportFileName);
    end;
  end;
end;  // SaveDesignAs

//==============================================================================
procedure TfrmReportDesigner.RefreshFontCombobox;
var lCount: Integer;
begin
  with FontCombobox do begin
    Clear;
    Items:=Screen.Fonts;
    for lCount:=1 to Printer.Fonts.Count do
      if Items.IndexOf(Printer.Fonts[lCount-1])=-1 then Items.Add(Printer.Fonts[lCount-1]);
  end;
end;  // RefreshFontComboBox

//==============================================================================
procedure TfrmReportDesigner.QRepDesignerxEditProperties(QRD: TQRepDesigner;
  Component: TComponent; var EditResult: Boolean);
begin
  if (Component is TDesignQuickReport)  then EditResult:=EditReportOptions  (QRD, TQuickRep(Component)) else
  if (Component is TQRDesignDBText)     then EditResult:=EditDBTextProps    (QRD, Self, TQRDesignDBText(Component)) else
  if (Component is TQRDesignShape)      then EditResult:=EditShapeProps     (QRD, Self, TQRDesignShape(Component)) else
  if (Component is TQRDesignExpr)       then EditResult:=EditExpressionProps(QRD, Self, TQRDesignExpr(Component)) else
  if (Component is TQRDesignChildband)  then EditResult:=EditChildProps     (QRD, Self, TQRDesignChildBand(Component)) else
  if (Component is TQRDesignGroup)      then EditResult:=EditGroupProps     (QRD, Self, TQRDesignGroup(Component)) else
  if (Component is TQRDesignSubdetail)  then EditResult:=EditSubdetailProps (QRD, Self, TQRDesignSubdetail(Component)) else
  {$IFDEF WPTools}
  if (Component is TQRDesignRichtext)   then EditResult:=EditRichtextProps  (QRD, Self, TQRDesignRichtext(Component)) else
  if (Component is TQRDesignDBRichtext) then EditResult:=EditDBRichtextProps(QRD, Self, TQRDesignDBRichtext(Component)) else
  {$ELSE}
  {$IFDEF WIN32}
  if (Component is TQRDesignRichtext)   then EditResult:=EditRichtextProps  (QRD, Self, TQRDesignRichtext(Component)) else
  if (Component is TQRDesignDBRichtext) then EditResult:=EditDBRichtextProps(QRD, Self, TQRDesignDBRichtext(Component)) else
  {$ENDIF}
  {$ENDIF}
  if (Component is TQRDesignSysData)    then EditResult:=EditSysDataProps   (QRD, Self, TQRDesignSysData(Component)) else
  if (Component is TQRDesignImage)      then EditResult:=EditImageProps     (QRD, Self, TQRDesignImage(Component)) else
  if (Component is TQRDesignDBImage)    then EditResult:=EditDBImageProps   (QRD, Self, TQRDesignDBImage(Component)) else
  if (Component is TQRDesignLabel)      then EditResult:=EditLabelProps     (QRD, Self, TQRDesignLabel(Component)) else
  if (Component is TQRDesignMemo)       then EditResult:=EditMemoProps      (QRD, Self, TQRDesignMemo(Component)) else
  if (Component is TQRDesignBand)       then EditResult:=EditBandProps      (QRD, Self, TQRDesignBand(Component)) else
  {$IFDEF QR2PP}
  if (Component is TQRDesignGrid)       then EditResult:=EditGridProps      (QRD, Self, TQRDesignGrid(Component)) else
  if (Component is TQRDesignCheckbox)   then EditResult:=EditCheckboxProps  (QRD, Self, TQRDesignCheckbox(Component)) else
  {$ENDIF}
end;  // QRepDesignerEditProperties

//==============================================================================
procedure TfrmReportDesigner.QRepDesignerxSelectionChanged(QRD: TQRepDesigner;
  NumberOfSelections: Integer);
  //----------------------------------------------------------------------------
  procedure DataFieldsToCombobox(iC: TComponent; Dataset: TDataset);
  var lIdx:integer;
      stDisplayName:string;
  begin
    if DataSet<>nil then
      QRDFillDatafieldCombobox(AllCombobox, Dataset, GetStringProperty(iC,'Datafield'));
    with AllComboBox do begin
      // Change raw Field names read from the dataset to proper display names
      for lIdx:=0 to Items.Count-1 do begin
        stDisplayName:=Items[lIdx];
        Items[lIdx]:=ConvertFieldName(stDisplayName);
      end;
      Sorted:=True;
      if (ItemIndex<0) and (Items.Count>0) then
        ItemIndex:=0;
      // Find the field name in the list and change the ItemIndex accordingly
      lIdx:=Items.IndexOf(ConvertFieldName(GetStringProperty(iC,'DataField')));
      if lIdx>0 then ItemIndex:=lIdx;
      Enabled:=not(GetBooleanProperty(iC,'BlockChange') OR
                   GetBooleanProperty(iC,'BlockEdit'));
    end;
  end;
  //----------------------------------------------------------------------------
var
  C: TControl;
  A: TAlignment;
  lIdx:integer;
begin
  AlignMI.Enabled       :=AlignLeftMI.Enabled;
  CenterMI.Enabled      :=AlignCenterHorizontalMI.Enabled;
  ParentCenterMI.Enabled:=AlignCenterBandHorizontalMI.Enabled;
  EqualSpaceMI.Enabled  :=AlignEqualHorizontalMI.Enabled;

  TextCenterAlignButton.Down:=false;
  TextLeftAlignButton.Down  :=false;
  TextRightAlignButton.Down :=false;

  if NumberOfSelections=1 then begin
    C:=TControl(QRD.GetSelectedElement);
    if PropertyExists(C,'Alignment') then begin
      A:=TAlignment(GetIntegerProperty(C,'Alignment'));
      if A=taCenter then
        TextCenterAlignButton.Down:=true
      else if A=taRightJustify then
        TextRightAlignButton.Down :=true
      else
        TextLeftAlignButton.Down  :=true;
    end;

    if C is TQRDesignLabel then begin
      TypeLabel.Caption  :=QRepDesigner.RuntimeMessages.Values['Label'];
      AllEdit.Visible    :=true;
      AllComboBox.Visible:=false;
      AllEdit.Text       :=TQRLabel(C).Caption;
      AllEdit.Enabled    :=not(GetBooleanProperty(C,'BlockChange') OR
                               GetBooleanProperty(C,'BlockEdit'));
    end else
    if C is TQRExpr then begin
      TypeLabel.Caption  :=QRepDesigner.RuntimeMessages.Values['Expression'];
      AllEdit.Visible    :=true;
      AllComboBox.Visible:=false;
      AllEdit.Text       :=TQRExpr(C).Expression;
      AllEdit.Enabled    :=not(GetBooleanProperty(C,'BlockChange') OR
                               GetBooleanProperty(C,'BlockEdit'));
    end else begin
      AllComboBox.Items.Clear;
      AllComboBox.Visible:=true;
      AllCombobox.Sorted:=false;
      AllEdit.Visible:=false;
      with QRepDesigner do begin
        {---- Band ----}
        if C is TQRChildBand then begin
          TypeLabel.Caption  :=QRepDesigner.RuntimeMessages.Values['Childband'];
          AllCombobox.Enabled:=false;
        end else
        if C is TQRSubDetail then begin
          TypeLabel.Caption  :=QRepDesigner.RuntimeMessages.Values['Subdetailband'];
          AllCombobox.Enabled:=false;
        end else
        if C is TQRGroup then begin
          TypeLabel.Caption  :=QRepDesigner.RuntimeMessages.Values['Groupband'];
          AllCombobox.Enabled:=false;
        end else
        if C is TQRDesignBand then begin
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Band'];;
          with AllComboBox.Items do begin
            Add(RuntimeMessages.Values[BandTypes[8]]);
            Add(RuntimeMessages.Values[BandTypes[2]]);
            Add(RuntimeMessages.Values[BandTypes[6]]);
            Add(RuntimeMessages.Values[BandTypes[5]]);
            Add(RuntimeMessages.Values[BandTypes[1]]);
            Add(RuntimeMessages.Values[BandTypes[3]]);
            Add(RuntimeMessages.Values[BandTypes[4]]);
            Add(RuntimeMessages.Values[BandTypes[0]]);
          end;

          case TQRDesignBand(C).BandType of
            rbColumnHeader : lIdx:=0;
            rbDetail       : lIdx:=1;
            rbGroupFooter  : lIdx:=2;
            rbGroupHeader  : lIdx:=3;
            rbPageHeader   : lIdx:=4;
            rbPageFooter   : lIdx:=5;
            rbSummary      : lIdx:=6;
            rbTitle        : lIdx:=7;
          else
            lIdx:=7;
          end;
          AllComboBox.ItemIndex:=lIdx;
          AllComboBox.Enabled:=not(GetBooleanProperty(C,'BlockChange') OR
                                   GetBooleanProperty(C,'BlockEdit'));
        end else
        {---- DBText ----}
        if C is TQRDesignDBText then begin
          DataFieldsToCombobox(C,TQRDesignDBText(C).DataSet);
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Datafield'];
        end else
        {---- Memo ----}
        if C is TQRDesignMemo then begin
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Memo'];
          AllComboBox.Visible:=false;
        end else
        {---- DBImage ----}
        if C is TQRDesignDBImage then begin
          DatafieldsToCombobox(C,TQRDesignDBImage(C).DataSet);
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Image datafield'];
        end else
        {---- Image ----}
        if C is TQRDesignImage then begin
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Image'];
          AllComboBox.Visible:=false;
        end else
        {---- Shape ----}
        if C is TQRDesignShape then begin
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Shape'];
          with AllComboBox.Items do begin
            Add(RuntimeMessages.Values['ShapeCircle']);
            Add(RuntimeMessages.Values['ShapeHorizontalLine']);
            Add(RuntimeMessages.Values['ShapeRectangle']);
            Add(RuntimeMessages.Values['ShapeLeftRightLines']);
            Add(RuntimeMessages.Values['ShapeTopBottomLines']);
            Add(RuntimeMessages.Values['ShapeVerticalLine']);
          end;
          case TQRDesignShape(C).Shape of
            qrsCircle       : lIdx:=0;
            qrsHorLine      : lIdx:=1;
            qrsRectangle    : lIdx:=2;
            qrsRightAndLeft : lIdx:=3;
            qrsTopAndBottom : lIdx:=4;
            qrsVertLine     : lIdx:=5;
          else
            lIdx:=2;
          end;
          AllComboBox.ItemIndex:=lIdx;
          AllComboBox.Enabled:=not(GetBooleanProperty(C,'BlockChange') OR
                                   GetBooleanProperty(C,'BlockEdit'));
        end else
        {---- Sysdata ----}
        if C is TQRDesignSysdata then begin
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Systemfield'];
          with AllComboBox.Items do begin
            Add(RuntimeMessages.Values[SysDataTypes[1]]);
            Add(RuntimeMessages.Values[SysDataTypes[0]]);
            Add(RuntimeMessages.Values[SysDataTypes[2]]);
            Add(RuntimeMessages.Values[SysDataTypes[5]]);
            Add(RuntimeMessages.Values[SysDataTypes[6]]);
            Add(RuntimeMessages.Values[SysDataTypes[3]]);
            Add(RuntimeMessages.Values[SysDataTypes[4]]);
          end;
          case TQRDesignSysdata(C).Data of
            qrsDate        : lIdx:=0;
            qrsTime        : lIdx:=1;
            qrsDateTime    : lIdx:=2;
            qrsDetailCount : lIdx:=3;
            qrsDetailNo    : lIdx:=4;
            qrsPageNumber  : lIdx:=5;
            qrsReportTitle : lIdx:=6;
          else
            lIdx:=2;
          end;
          AllComboBox.ItemIndex:=lIdx;
          AllComboBox.Enabled:=not(GetBooleanProperty(C,'BlockChange') OR
                                   GetBooleanProperty(C,'BlockEdit'));
        end
        {$IFDEF WIN32}
        {$IFNDEF WPTools}
        else
        if C is TQRDesignRichtext then begin
          AllEdit.Visible:=false;
          AllComboBox.Visible:=false;
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Richtext'];
        end else
        if C is TQRDesignDBRichtext then begin
          DataFieldsToCombobox(C,TQRDesignDBRichtext(C).DataSet);
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Richtext datafield'];
        end
        {$ENDIF}
        {$ENDIF}
        {$IFDEF WPTools}
        else
        if C is TQRDesignRichtext then begin
          AllEdit.Visible:=false;
          AllComboBox.Visible:=false;
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Richtext'];
        end else
        if C is TQRDesignDBRichtext then begin
          DataFieldsToCombobox(C,TQRDesignDBRichtext(C).Datasource.DataSet);
          TypeLabel.Caption:=QRepDesigner.RuntimeMessages.Values['Richtext datafield'];
        end
        {$ENDIF}
        else begin
          AllEdit.Visible:=false;
          AllComboBox.Visible:=false;
          TypeLabel.Caption:='';
        end;
      end { with QRDesigner }
    end { not C is TQRDesignLabel }
  end { NumberOfSelections = 1 }
  else begin
    AllEdit.Visible:=false;
    AllComboBox.Visible:=false;
    if NumberOfSelections=0 then TypeLabel.Caption:='Report' else TypeLabel.Caption:='';
  end;
  Scrollbox2.HorzScrollbar.Position:=ScrollBox1.HorzScrollbar.Position;
end;  // QRepDesignerSelectionChanged

//==============================================================================
procedure TfrmReportDesigner.Scrollbox1ScrollHorz(Sender: TObject);
begin
  Scrollbox2.HorzScrollbar.Position:=ScrollBox1.HorzScrollbar.Position;
end;  // Scrollbox1ScrollHorz

//==============================================================================
procedure TfrmReportDesigner.pmPropertyTopClick(Sender: TObject);
begin
  pmPropertyTop.Checked   :=true;
  pmPropertyBottom.Checked:=false;
  PropertyPanel.Align     :=alTop;
  if ToolbarPanel.Visible then
    ToolbarPanel.Top:=-1;
end;  // PropMenuTopClick

//==============================================================================
procedure TfrmReportDesigner.pmPropertyBottomClick(Sender: TObject);
begin
  pmPropertyTop.Checked   :=false;
  pmPropertyBottom.Checked:=true;
  PropertyPanel.Align     :=alBottom;
  if StatusPanel.Visible then
    PropertyPanel.Top:=StatusPanel.Top+1;
end;  // PropMenuBottomClick

//==============================================================================
procedure TfrmReportDesigner.pmElementPanelRightClick(Sender: TObject);
begin
  ElementPanel.Align:=alRight;
  pmElementPanelLeft.Checked :=false;
  pmElementPanelRight.Checked:=true;
end;  // pmElementPanelRightClick

//==============================================================================
procedure TfrmReportDesigner.pmElementPanelLeftClick(Sender: TObject);
begin
  ElementPanel.Align:=alLeft;
  pmElementPanelLeft.Checked :=true;
  pmElementPanelRight.Checked:=false;
end;  // pmElementPanelLeftClick

//==============================================================================
procedure TfrmReportDesigner.AllEditChange(Sender: TObject);
var TC: TControl;
begin
  TC:=TControl(QRepDesigner.GetSelectedElement);
  if TC=nil then Exit;
    if GetStringProperty(TC,'Caption')<>AllEdit.Text then begin
      SetStringProperty(TC,'Caption',AllEdit.Text);
      QRepDesigner.RedrawSelection;
    end;
end;  // AllEditChange

//==============================================================================
procedure TfrmReportDesigner.AllComboBoxChange(Sender: TObject);
var
  C: TControl;
  X: Integer;
  stText:string;
begin
  C:=TControl(QRepDesigner.GetSelectedElement);
  if C=nil then Exit;
  if C is TQRDesignDBText then begin
    stText:=AllComboBox.Items[AllComboBox.ItemIndex];
    with TQRDesignDBText(C).DataSet do
      for X:=1 to FieldCount do
        if ConvertFieldName(Fields[X-1].DisplayLabel)=stText then begin
          TQRDesignDBText(C).DataField:=Fields[X-1].FieldName;
          TQRDesignDBText(C).Caption  :=ConvertFieldName(Fields[X-1].DisplayLabel);
          Break;
        end;
  end else
  if C is TQRDesignBand then begin
    with TQRDesignband(C) do
      case AllComboBox.ItemIndex of
        0: BandType:=rbColumnHeader;
        1: BandType:=rbDetail;
        2: BandType:=rbGroupFooter;
        3: BandType:=rbGroupHeader;
        4: BandType:=rbPageHeader;
        5: BandType:=rbPageFooter;
        6: BandType:=rbSummary;
        7: BandType:=rbTitle;
      end;
  end else
  if C is TQRDesignDBImage then begin
    stText:=AllComboBox.Items[AllComboBox.ItemIndex];
    for X:=1 to TQRDesignDBImage(C).DataSet.FieldCount do
      if TQRDesignDBImage(C).DataSet.Fields[X-1].DisplayLabel=stText then
        TQRDesignDBImage(C).DataField:=TQRDesignDBImage(C).DataSet.Fields[X-1].FieldName;
  end else
  {$IFDEF WIN32}
  {$IFNDEF WPTools}
  if C is TQRDesignDBRichtext then begin
    stText:=AllComboBox.Items[AllComboBox.ItemIndex];
    for X:=1 to TQRDesignDBRichtext(C).DataSet.FieldCount do
      if TQRDesignDBRichtext(C).DataSet.Fields[X-1].DisplayLabel=stText then
        TQRDesignDBRichtext(C).DataField:=TQRDesignDBRichtext(C).DataSet.Fields[X-1].FieldName;
  end else
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WPTools}
  if C is TQRDesignDBRichtext then begin
    stText:=AllComboBox.Items[AllComboBox.ItemIndex];
    for X:=1 to TQRDesignDBRichtext(C).Datasource.DataSet.FieldCount do
      if TQRDesignDBRichtext(C).Datasource.DataSet.Fields[X-1].DisplayLabel=stText then
        TQRDesignDBRichtext(C).DataField:=
          TQRDesignDBRichtext(C).Datasource.DataSet.Fields[X-1].FieldName;
  end else
  {$ENDIF}
  if C is TQRDesignShape then begin
    with TQRDesignShape(C) do
      case AllComboBox.ItemIndex of
        0: Shape:=qrsCircle;
        1: Shape:=qrsHorLine;
        2: Shape:=qrsRectangle;
        3: Shape:=qrsRightAndLeft;
        4: Shape:=qrsTopAndBottom;
        5: Shape:=qrsVertLine;
      end;
  end else
  if C is TQRDesignSysdata then begin
    with TQRDesignSysdata(C) do
      case AllComboBox.ItemIndex of
        0: Data:=qrsDate;
        1: Data:=qrsTime;
        2: Data:=qrsDateTime;
        3: Data:=qrsDetailCount;
        4: Data:=qrsDetailNo;
        5: Data:=qrsPageNumber;
        6: Data:=qrsReportTitle;
      end;
  end;
  QRepDesigner.SelectElement(True, C);
end;  // AllComboBoxChange

//==============================================================================
procedure TfrmReportDesigner.mnuViewToolbarClick(Sender: TObject);
begin
  mnuViewToolbar.Checked:=not mnuViewToolbar.Checked;
  ToolbarPanel.Visible:=mnuViewToolbar.Checked;
  ToolbarPanel.Top:=-1;
end;  // mnuViewToolbarClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewFontToolbarClick(Sender: TObject);
begin
  mnuViewFontToolbar.Checked:=not mnuViewFontToolbar.Checked;
  pmPropertyVisible.Checked :=mnuViewFontToolbar.Checked;
  PropertyPanel.Visible     :=mnuViewFontToolbar.Checked;
  PropertyPanel.Top:=-1;
  if ToolbarPanel.Visible then ToolbarPanel.Top:=-1;
end;  // mnuViewFontToolbarClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewElementToolbarClick(Sender: TObject);
begin
  mnuViewElementToolbar.Checked:=not mnuViewElementToolbar.Checked;
  pmElementPanelVisible.Checked:=mnuViewElementToolbar.Checked;
  ElementPanel.Visible:=mnuViewElementToolbar.Checked;
  if ToolbarPanel.Visible then ToolbarPanel.Top:=-1;
end;  // mnuViewElementToolbarClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewOptionsClick(Sender: TObject);
begin
  EditViewOptions(Quickreport, QRepDesigner);
  RulerPanel1.Units:=Quickreport.Units;
end;  // mnuViewOptionsClick

//==============================================================================
procedure TfrmReportDesigner.QRepDesignerxShowInfo(Component: TComponent;
  var Info: QRDShowInfoString);
var
  U: String[15];
begin
  Info:='';
  if Component=nil then
  begin
    XPosLabel.Caption:='';
    YPosLabel.Caption:='';
    ElementsComboBox.Items.Clear;
  end else begin
    ElementsComboBox.Items.Clear;
    if Component.Name<>'' then
    begin
      ElementsComboBox.Items.Add(Component.Name);
      ElementsComboBox.ItemIndex:=0;
    end;
    case Quickreport.Units of
      MM        : U:=' mm';
      Inches    : U:=' Inches';
      Characters: U:=' Chars';
    else
      U:='';
    end;
    if Component is TQRPrintable then
    begin
      Label1.Visible:=True;
      XPosLabel.Caption:=IntToStr(TControl(Component).Left)
        +' ('+FloatToStrF(TQRPrintable(Component).Size.Left,ffFixed,18,2)+U+')';
      YPosLabel.Caption:=IntToStr(TControl(Component).Top)
        +' ('+FloatToStrF(TQRPrintable(Component).Size.Top,ffFixed,18,2)+U+')';
    end else begin
      Label1.Visible:=False;
      XPosLabel.Caption:='';
      YPosLabel.Caption:='';
    end;
  end;
end;  // QRepDesignerShowInfo

//==============================================================================
procedure TfrmReportDesigner.mnuViewStatuslineClick(Sender: TObject);
begin
  mnuViewStatusLine.Checked:=not mnuViewStatusLine.Checked;
  StatusPanel.Visible:=mnuViewStatusLine.Checked;
  StatusPanel.Top    :=Height+1;
end;  // mnuViewStatuslineClick

//==============================================================================
procedure TfrmReportDesigner.LabelSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  QRepDesigner.AddMultipleElements:=ssShift in Shift;
end;  // LabelSpeedButtonMouseDown

//==============================================================================
procedure TfrmReportDesigner.mnuZoomDefaultClick(Sender: TObject);
begin
  QuickReport.Zoom:=100;
  QRepDesigner.RedrawSelection;
  RulerPanel1.Zoom:=Quickreport.Zoom;
end;  // mnuZoomDefaultClick

//==============================================================================
procedure TfrmReportDesigner.mnuZoomInClick(Sender: TObject);
begin
  if QuickReport.Zoom*2>300 then
    QuickReport.Zoom:=300
  else
    QuickReport.Zoom:=QuickReport.Zoom*2;
  QRepDesigner.RedrawSelection;
  RulerPanel1.Zoom:=Quickreport.Zoom;
end;  // mnuZoomInClick

//==============================================================================
procedure TfrmReportDesigner.mnuZoomOutClick(Sender: TObject);
begin
  if QuickReport.Zoom/2>10 then QuickReport.Zoom:=Round(QuickReport.Zoom/2);
  QRepDesigner.RedrawSelection;
  RulerPanel1.Zoom:=Quickreport.Zoom;
end;  // mnuZoomOutClick

//==============================================================================
procedure TfrmReportDesigner.Panel2DblClick(Sender: TObject);
begin
  QRepDesigner.EditProperties(QRepDesigner.GetSelectedElement);
end;  // Panel2DblClick

//==============================================================================
procedure TfrmReportDesigner.ElementsComboBoxDropDown(Sender: TObject);
var
  I: Integer;
begin
  ElementsCombobox.Items.Clear;
  for I:=0 to ComponentCount-1 do
    begin
      if (PropertyExists(Components[I],'BlockDelete')) and (Components[I].Name<>'')
       then ElementsCombobox.Items.Add(Components[I].Name);
    end;
end;  // ElementsComboBoxDropDown

//==============================================================================
procedure TfrmReportDesigner.ElementsComboBoxChange(Sender: TObject);
var
  C: TControl;
begin
  C:=TControl(FindComponent(ElementsCombobox.Text));
  if (C<>nil) then QRepDesigner.SelectElement(True, C);
end;  // ElementsComboBoxChange

//==============================================================================
procedure TfrmReportDesigner.UndoMIClick(Sender: TObject);
begin
  QRepDesigner.DoUndo;
end;  // UndoMIClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewRotatebandsClick(Sender: TObject);
begin
  Quickreport.RotateBands:=Quickreport.RotateBands+1;
end;  // mnuViewRotateBandsClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewResetbandsClick(Sender: TObject);
begin
  Quickreport.RotateBands:=0;
  Quickreport.HideBands:=False;
  mnuViewHideBands.Checked:=False;
end;  // mnuViewResetBandsClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewHidebandsClick(Sender: TObject);
begin
  mnuViewHideBands.Checked:=not mnuViewHideBands.Checked;
  Quickreport.HideBands:=mnuViewHideBands.Checked;
end;  // mnuViewHideBandsClick

//==============================================================================
procedure TfrmReportDesigner.FontComboBoxChange(Sender: TObject);
begin
  Panel1.Caption:=FontCombobox.Text;
end;  // FontComboBoxChange

//==============================================================================
procedure TfrmReportDesigner.mnuViewDatafieldsListboxClick(Sender: TObject);
begin
  mnuViewDatafieldsListbox.Checked:=not mnuViewDatafieldsListbox.Checked;
  QRepDesigner.ShowDatafieldListbox:=mnuViewDatafieldsListbox.Checked;
end;  // mnuViewDatafieldsListBoxClick

//==============================================================================
procedure TfrmReportDesigner.mnuViewClick(Sender: TObject);
begin
  mnuViewDatafieldsListbox.Checked:=(QRepDesigner.DatafieldListbox<>nil) and
                                    (TForm(QRepDesigner.DatafieldListbox.Owner).Visible);
  QRepDesigner.ShowDatafieldListbox:=mnuViewDatafieldsListbox.Checked;
end;  // ViewMenuClick

//==============================================================================
procedure TfrmReportDesigner.ShowDataFieldList;
begin
  {--- Datafield listbox ---}
  CreateDatafieldListbox(QRepDesigner);
  mnuViewDatafieldsListbox.Checked:=true;
  QRepDesigner.ShowDatafieldListbox:=True;
  SendMessage(Handle,WM_UPDATE_MENU_ICONS,0,0);
end;  // ShowDataFieldList

//==============================================================================
procedure TfrmReportDesigner.QRepDesignerAfterReportComponentCreated(
  Control: TCustomControl);
begin
  inherited;
  // Select the newly added component
  QRepDesigner.SelectElement(True, Control);
  // Trigger a bogus change event so set the properies correctly if dragged from
  // floating list of fields. There is a little problem for VagueDates and SpatialRefs
  AllComboBoxChange(nil);
end;  // QRepDesignerAfterReportComponentCreated


//==============================================================================
{ Load a saved tpl file into the report designer }
procedure TfrmReportDesigner.OpenReport;
begin
  if AppSettings.ReportTemplatePath='' then
    dlgOpen.InitialDir:=ExtractFilePath(Application.ExeName)
  else
    dlgOpen.InitialDir:=AppSettings.ReportTemplatePath;
  if dlgOpen.Execute then begin
    QRepDesigner.LoadReport(dlgOpen.FileName);
    ReportFileName:=dlgOpen.FileName;
  end;
end;





constructor TfrmReportDesigner.Create(AOwner: TComponent);
begin
  inherited;
  // Initialise XP Appearance for menus
  with TXPMenu.Create(Self) do begin
    XPControls := [xcMainMenu, xcPopupMenu];
    Active := True;
  end;
  FPreviewCancelled := False;
end;

procedure TfrmReportDesigner.SetReportFileName(Value: String);
var
  lstShortPathName : String;
  lstShortFileName : String;
begin
  FstReportFileName := Value;

  if FstReportFileName <> '' then
  begin
    lstShortPathName := ExtractShortPathname(IncludeTrailingPathDelimiter(Appsettings.ReportTemplatePath));
    if (lstShortPathName <> '') and (ExtractShortPathName(IncludeTrailingPathDelimiter(ExtractFileDir(FstReportFileName))) = lstShortPathname) then
      lstShortFileName := ExtractFileName(FstReportFileName)
    else
      lstShortFileName := FstReportFileName
  end
  else
    lstShortFileName := 'Untitled';
  Caption := Format(ResStr_ReportDesigner, [lstShortFileName]);
  if Owner<>nil then begin
    TfrmFilterResult(Owner).TemplateFile :=FstReportFileName;
  end;
end;

destructor TfrmReportDesigner.Destroy;
begin
  if Assigned(QuickReport) then
  if not (QuickReport.Available or FPreviewCancelled) then begin
    QuickReport.Cancel;
    FPreviewCancelled := True;
  end;
  // Disassociate the report designer from the filter result screen.
  dmFormActions.actPrint.Enabled  :=false;
  dmFormActions.actPreview.Enabled:=false;
  frmMain.mnuFileSave.Enabled     :=false;
  frmMain.mnuFileSaveAs.Enabled   :=false;
  // If the FilterResult screen is still up, tell it that we are closing
  if Owner<>nil then begin
    TfrmFilterResult(Owner).ReportDesigner:=nil;
  end;
  inherited;
end;

end.
