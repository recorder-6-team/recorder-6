library PhenologyNu;

uses
  ComServ,
  PhenologyNu_TLB in 'PhenologyNu_TLB.pas',
  PhenologyFormImpl1 in 'PhenologyFormImpl1.pas' {PhenologyFormX: TActiveForm} {PhenologyFormX: CoClass},
  DropTarget in 'DropTarget.pas',
  DragDrop in 'dragdrop.pas',
  DragDropComponent in 'DragDropComponent.pas',
  DropSource in 'Dropsource.pas',
  dropstruct in 'Dropstruct.pas',
  OptionDialog in 'OptionDialog.pas' {FormpOption},
  ADODB_TLB in '..\..\Third Party\Imports\ADODB_TLB.pas',
  Recorder2000_TLB in '..\..\Third Party\Imports\Recorder2000_TLB.pas';

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
