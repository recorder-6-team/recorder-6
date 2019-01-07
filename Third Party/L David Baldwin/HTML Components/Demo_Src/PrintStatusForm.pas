unit PrintStatusForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, HTMLView, MetaFilePrinter;

const
  wm_StartPreview = wm_User+22;
  wm_StartPrint = wm_User+23;
type
  TPrnStatusForm = class(TForm)
    StatusLabel: TLabel;
    CancelButton: TBitBtn;
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    Viewer: ThtmlViewer;
    Canceled: boolean;
    MFPrinter: TMetaFilePrinter;
    FromPage, ToPage: integer;
    procedure wmStartPreview(var Message: TMessage); message wm_StartPreview;
    procedure wmStartPrint(var Message: TMessage); message wm_StartPrint;
    procedure PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
  public
    { Public declarations }
  procedure DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
  procedure DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
  end;

var
  PrnStatusForm: TPrnStatusForm;

implementation

{$R *.DFM}


procedure TPrnStatusForm.DoPreview(AViewer: ThtmlViewer; AMFPrinter: TMetaFilePrinter;
              var Abort: boolean);
begin
Viewer := AViewer;
MFPrinter := AMFPrinter;
Viewer.OnPageEvent := PageEvent;
try                                   
  PostMessage(Handle, Wm_StartPreview, 0, 0);
  Abort := ShowModal = mrCancel;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;

procedure TPrnStatusForm.DoPrint(AViewer: ThtmlViewer; FromPg, ToPg: integer;
              var Abort: boolean);
begin
Viewer := AViewer;
FromPage := FromPg;
ToPage := ToPg;
Viewer.OnPageEvent := PageEvent;
try                                   
  PostMessage(Handle, Wm_StartPrint, 0, 0);
  Abort := ShowModal = mrCancel;
finally
  Viewer.OnPageEvent := Nil;      
  end;
end;

procedure TPrnStatusForm.PageEvent(Sender: TObject; PageNum: integer; var Stop: boolean);
begin
if Canceled then
  Stop := True
else
  if PageNum = 0 then
    StatusLabel.Caption := 'Formating'
  else
    StatusLabel.Caption := 'Page Number '+ IntToStr(PageNum);
Update;
end;

procedure TPrnStatusForm.wmStartPreview(var Message: TMessage);
begin
Viewer.PrintPreview(MFPrinter);
if Canceled then
  ModalResult := mrCancel
else ModalResult := mrOK;
end;

procedure TPrnStatusForm.wmStartPrint(var Message: TMessage);
begin
Viewer.Print(FromPage, ToPage);
if Canceled then
  ModalResult := mrCancel
else ModalResult := mrOK;
end;

procedure TPrnStatusForm.CancelButtonClick(Sender: TObject);
begin
Canceled := True;
end;

end.
