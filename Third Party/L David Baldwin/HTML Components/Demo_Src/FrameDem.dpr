program FrameDem;
{A program to demonstrate the TFrameViewer component}

{$ifdef Windows}  {Delphi 1}
  {$M 24000, 8192}   {Default stack size may be too small}
{$else}
  {$M 24000, 100000}
{$endif}

uses
  Forms,
  FDemUnit in 'FDEMUNIT.PAS' {Form1},
  Fontdlg in 'FONTDLG.PAS' {FontForm},
  Submit in 'SUBMIT.PAS' {SubmitForm},
  HTMLAbt in 'HTMLABT.PAS' {AboutBox},
  PrintStatusForm in 'PRINTSTATUSFORM.PAS' {PrnStatusForm},
  Gopage in 'GOPAGE.PAS' {GoPageForm},
  PreviewForm in 'PREVIEWFORM.PAS' {PreviewForm},
  ImgForm in 'IMGFORM.PAS' {ImageForm};

{$R *.RES}

begin
{$ifndef Windows}
  Application.Initialize;
{$endif}
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSubmitForm, SubmitForm);
  Application.Run;
end.
