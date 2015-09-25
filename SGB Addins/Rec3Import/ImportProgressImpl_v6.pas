//------------------------------------------------------------------------
//  Author:  Stuart Ball
//  Date:    October 2006
//  Version: Recorder 6 version
//  Purpose: Displays progress information to the user
//------------------------------------------------------------------------
unit ImportProgressImpl_v6;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, Recorder_3_Import_v6_TLB, StdCtrls, ExtCtrls, ComCtrls,
  Recorder2000_TLB, StdVcl, AbBase, AbBrowse, AbZBrows, AbZipper, ImgList;

type
  TImportProgressX = class(TActiveForm, IImportProgressX, IFormCaption)
    Bevel1: TBevel;
    labelTableName: TLabel;
    labelTables: TLabel;
    labelNoTables: TLabel;
    labelTask2: TLabel;
    labelTask3: TLabel;
    labelTask4: TLabel;
    labelTask1: TLabel;
    labelTime: TLabel;
    labelErrors: TLabel;
    pbarTables: TProgressBar;
    pbarCurrent: TProgressBar;
    panelTop: TPanel;
    StaticText: TStaticText;
    stReport: TStaticText;
    labelTask6: TLabel;
    AbZipper: TAbZipper;
    labelTask7: TLabel;
    ImageList: TImageList;
  private
    { Private declarations }
    FEvents: IImportProgressXEvents;
    { Our internal variables }
    FTime: TDateTime;
    FStatusImage: array [0..5]of byte;
    procedure UpdateStatus;

    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    procedure SetUpForm;
    procedure ShowString(const work: string);
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_BiDiMode: TxBiDiMode; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_BiDiMode(Value: TxBiDiMode); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    { IFormCaption }
    function Get_FormCaption: WideString; safecall;
    { Our additions }
    procedure ShowTitle(const aControl: WideString); safecall;
    procedure Set_Progress(Value: Integer); safecall;
    procedure Set_ProgressMax(Value: Integer); safecall;
    function ElapsedTime: TDateTime; safecall;
    procedure Set_CurrentTableName(const Value: WideString); safecall;
    procedure Set_NoErrors(Value: Integer); safecall;
    procedure Set_ProgressTables(Value: Integer); safecall;
    procedure Set_ProgressTablesMax(Value: Integer); safecall;
    procedure ShowTask(Task, TaskStatus: TxTask); safecall;
    procedure ReportCompletion(NoErrors: Integer); safecall;
    procedure ZipFile(var aFile: OleVariant); safecall;
  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TImportProgressX }

procedure TImportProgressX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ImportProgressXPage); }
end;

procedure TImportProgressX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IImportProgressXEvents;
end;

procedure TImportProgressX.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
  SetUpForm;
end;

function TImportProgressX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TImportProgressX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TImportProgressX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TImportProgressX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TImportProgressX.Get_BiDiMode: TxBiDiMode;
begin
  Result := Ord(BiDiMode);
end;

function TImportProgressX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TImportProgressX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TImportProgressX.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TImportProgressX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TImportProgressX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TImportProgressX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TImportProgressX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TImportProgressX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TImportProgressX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TImportProgressX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TImportProgressX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TImportProgressX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TImportProgressX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

procedure TImportProgressX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TImportProgressX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TImportProgressX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TImportProgressX.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TImportProgressX.Set_BiDiMode(Value: TxBiDiMode);
begin
  BiDiMode := TBiDiMode(Value);
end;

procedure TImportProgressX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TImportProgressX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TImportProgressX.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TImportProgressX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TImportProgressX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TImportProgressX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TImportProgressX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TImportProgressX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TImportProgressX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TImportProgressX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TImportProgressX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TImportProgressX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TImportProgressX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TImportProgressX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TImportProgressX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TImportProgressX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TImportProgressX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TImportProgressX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TImportProgressX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TImportProgressX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TImportProgressX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

//------------------------------------------------------------------------
//  INTERFACE IMPLIMENTATION
//------------------------------------------------------------------------
//  Provides a caption for the form
//------------------------------------------------------------------------
function TImportProgressX.Get_FormCaption: WideString;
begin
     Result := 'Import progress';
end;

//------------------------------------------------------------------------
//  LOCAL ROUTINES
//------------------------------------------------------------------------
// Initialisation
//------------------------------------------------------------------------
procedure TImportProgressX.SetUpForm;
var i: integer;
begin
  labelTableName.Caption := '';
  labelNoTables.Caption := '';
  StaticText.Caption := 'Import data exported from Recorder 3 into Recorder 6.';
  labelTime.Caption := '';
  labelErrors.Caption := '';
  FTime := now;
  for i := Low(FStatusImage) to High(FStatusImage) do
    FStatusImage[i] := 0;
  UpdateStatus;
end;

//------------------------------------------------------------------------
//  Shows the status images according to FStatusImage
//------------------------------------------------------------------------
procedure TImportProgressX.UpdateStatus;
var i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TLabel then
      if TLabel(Components[i]).Tag > 0 then
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(Rect(10, TLabel(Components[i]).Top-4, 26, TLabel(Components[i]).Top+12));
        ImageList.Draw(Canvas,10, TLabel(Components[i]).Top-4,
                  FStatusImage[TLabel(Components[i]).Tag-1], dsNormal, itImage, True);
      end;
end;

//------------------------------------------------------------------------
//  Shows the title for the import in static text block
//------------------------------------------------------------------------
procedure TImportProgressX.ShowTitle(const aControl: WideString);
begin
      Application.ProcessMessages;
      StaticText.Caption := aControl;
end;

//------------------------------------------------------------------------
// Set the progress bar's progress indicator
//------------------------------------------------------------------------
procedure TImportProgressX.Set_Progress(Value: Integer);
begin
    If pbarCurrent.Position <> Value then
       pBarCurrent.Position := Value;
    ElapsedTime;
end;

//------------------------------------------------------------------------
//  Set progress bar max value
//------------------------------------------------------------------------
procedure TImportProgressX.Set_ProgressMax(Value: Integer);
begin
    If pbarCurrent.Max <> Value then
       pBarCurrent.Max := Value;
end;

//------------------------------------------------------------------------
//  Zips the file whose name is passed. Returns the name of the ZIP file
// This is located here so we can directly use the AbZipper non-visual
//  component. I have not had much luck instantiating the component
// programmatically. Suspect quite a lot of initialisation and setting
// up is done behind the scenes!
//------------------------------------------------------------------------
procedure TImportProgressX.ZipFile(var aFile: OleVariant);
var sWork: string;
begin
  sWork := ChangeFileExt(aFile, '.zip');
  AbZipper.FileName := sWork;
  AbZipper.AddFiles(aFile, 0);
  if AbZipper.Count > 0 then
  begin
    aFile := sWork;
  end;
  AbZipper.CloseArchive;
end;

//------------------------------------------------------------------------
// Report elapsed time in a label
//------------------------------------------------------------------------
function TImportProgressX.ElapsedTime: TDateTime;
begin
     Result := now - FTime;
     try
        labelTime.Caption := TimeToStr(Result);
     except
        labelTime.Caption := '';
     end;
     labelTime.Update;
     UpdateStatus;
     Application.ProcessMessages;
end;

//------------------------------------------------------------------------
// Report the name of the table we are currently working on
//------------------------------------------------------------------------
procedure TImportProgressX.Set_CurrentTableName(const Value: WideString);
begin
     labelTableName.Caption := Value;
     labelTableName.Update;
     ElapsedTime;
end;

//------------------------------------------------------------------------
// Report number of errors in a label
//------------------------------------------------------------------------
procedure TImportProgressX.Set_NoErrors(Value: Integer);
begin
     If Value > 0 then
        try
           labelErrors.Caption := IntToStr(Value) + ' errors'
        except
           labelErrors.Caption := ''
        end
     else
         labelErrors.Caption := '';
     labelErrors.Update;
     ElapsedTime;
end;

//------------------------------------------------------------------------
//  Sets the number of tables to be processed in label
//------------------------------------------------------------------------
procedure TImportProgressX.Set_ProgressTables(Value: Integer);
begin
    If pbarTables.Position <> Value then
    begin
       pBarTables.Position := Value;
       if Value > 0 then
          try
             labelNoTables.Caption := IntToStr(Value) + ' of '
                                      + IntToStr(pbarTables.Max);
          except
             labelNoTables.Caption := '';
          end
       else
          labelNoTables.Caption := '';
    end;
    labelNoTables.Update;
    ElapsedTime;
end;

//------------------------------------------------------------------------
// Set progress bar max value
//------------------------------------------------------------------------
procedure TImportProgressX.Set_ProgressTablesMax(Value: Integer);
begin
    If pbarTables.Max <> Value then
       pBarTables.Max := Value;
end;

//------------------------------------------------------------------------
// This updates the task list.
// Items start out grey and unticked.
// When at item is in progress, the text is shown as blue.
// When it is finished with it is shown in normal text and a red tick
// is shown next to it.
//------------------------------------------------------------------------
procedure TImportProgressX.ShowTask(Task, TaskStatus: TxTask);
var work: string;

          {Local procedure to handle labels and images}
          Procedure SwitchAppearance(aLabel: TLabel; const Index: integer);
          begin
               If TaskStatus = taskInProgress then
               begin
                    aLabel.Font.Color := clBlue;
                    aLabel.Font.Style := [];
                    FStatusImage[Index] := 1;
                    UpdateStatus;
                    Application.ProcessMessages;
               end
               else
               begin
                    aLabel.Font.Color := clBlack;
                    aLabel.Font.Style := [];
                    FStatusImage[Index] := 2;
                    UpdateStatus;
                    Application.ProcessMessages;
               end;
          end; {SwitchAppearance}

          procedure HideDetails;
          begin
               labelTables.Visible := False;
               labelNoTables.Visible := False;
               pbarTables.Visible := False;
               labelTableName.Visible := False;
               pbarCurrent.Visible := False;
               UpdateStatus;
               Application.ProcessMessages;
          end;

begin {ShowTask}
     Case Task of
      taskCreate:       SwitchAppearance(labelTask1, 0);
      taskImport:       SwitchAppearance(labelTask2, 1);
      taskAllocate:     SwitchAppearance(labelTask3, 2);
      taskFix:          SwitchAppearance(labelTask6, 3);
      taskZip:          SwitchAppearance(labelTask7, 4);
      taskCopy:         SwitchAppearance(labelTask4, 5);
     end; {Case}
     If Ord(Task) > 5 then HideDetails;
     If Task = taskCopy then
     begin
        work := 'This process has taken ' + TimeToStr(ElapsedTime) + ' (h:m:s) so far. ' +
                'Handing over to Recorder 6 to validate your data ' +
                'and copy it to the main database. ' + #13 + #13 +
                'Please wait. This may take some time.';
        ShowString(work);
        labelTime.Caption := '';
     end
     else
        ElapsedTime;
end; {ShowTask}

//------------------------------------------------------------------------
//  Message showing imput is complete
//------------------------------------------------------------------------
procedure TImportProgressX.ReportCompletion(NoErrors: Integer);
var work: string;
begin
     With stReport.Font do
     begin
          Color := clBlue;
          Name := 'Arial';
          Size := 9;
     end;

     If NoErrors < 0 then
        work := 'Import canceled by user.'
     else
        work := 'Import completed.';

     work := work + #13 + 'Time (h:m:s) = ' + TimeToStr(ElapsedTime);
     if NoErrors > 0 then
        work := work + '. ' + IntToStr(NoErrors) + ' error';
     if NoErrors > 1 then
        work := work + 's.'
     else
         work := work + '.';
     ShowString(work);
end;

//------------------------------------------------------------------------
//  Display given string
//------------------------------------------------------------------------
procedure TImportProgressX.ShowString(const work: string);
begin
     with stReport do
     begin
          left :=  Bevel1.left + 8;
          width := Bevel1.width - 16;
          top := Bevel1.top + 8;
          height := Bevel1.Height - 16;
          Caption := work;
          Visible := True;
     end;
     Application.ProcessMessages;
end;

//=========================================================================
initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TImportProgressX,
    Class_ImportProgressX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
