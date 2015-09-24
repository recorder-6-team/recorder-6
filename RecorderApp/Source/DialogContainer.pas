unit DialogContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OLETools, BaseFormUnit, StdCtrls, Buttons, ExtCtrls, FormActions, ImageListButton;

type
  TdlgContainer = class(TBaseForm)
    pnlButtons: TPanel;
    bbOk: TImageListButton;
    bbCancel: TImageListButton;
    procedure bbOkClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
  protected
    FOLEProxy : TOLEProxy;
    procedure InitContainer;
  public
    constructor Create(AOwner : TComponent; iClassID : TGuid); reintroduce; overload;
    constructor Create(AOwner : TComponent; iProxy : TOleProxy); reintroduce; overload;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

uses
  Recorder2000_TLB, COMObj, ApplicationSettings;

const
  MIN_WIDTH = 183; // allows space for buttons
{$R *.DFM}

{ TDLGContainer }

{ Constructor - create an OLEProxy for the activeX specified by the class ID.
     Set the form dimensions accordingly, and the caption }
constructor TdlgContainer.Create(AOwner: TComponent; iClassID: TGuid);
begin
  inherited Create(AOwner);
  FOLEProxy := TOLEProxy.Create(Self, iClassID);
  Initcontainer;
end;


{ Overloaded version of the constructor where the com object already exists }
constructor TdlgContainer.Create(AOwner: TComponent; iProxy: TOleProxy);
begin
  inherited Create(AOwner);
  FOleProxy := iProxy;
  InitContainer;
end;



{ Destructor just cleansup }
destructor TdlgContainer.Destroy;
begin
  FOleProxy.Free;
  inherited Destroy;
end;


{ Construction common to both overloaded constructors }
procedure TdlgContainer.InitContainer;
begin
  FOLEProxy.Parent := Self;
  FOleProxy.Align := alClient;

  { A Caption specification interface  is optional, else use the name of the
        addin }
  try
    Caption := (FOleProxy.ControlInterface as IFormCaption).FormCaption;
  except
    on EIntfCastError do
      Caption := (FOleProxy.ControlInterface as IRecorderAddin).Name; // not optional
  end;  // try..except
  { A dialog must specify the dimensions - allow additional space for buttons}
  if (FOleProxy.ControlInterface as IDialog).Width > MIN_WIDTH then
    Width := (FOleProxy.ControlInterface as IDialog).Width
  else
    Width := MIN_WIDTH;
  { Move the buttons accordingly }
  bbOk.Left := Width - 175;
  bbCancel.Left := Width - 91;
  Height := (FOleProxy.ControlInterface as IDialog).Height + pnlButtons.Height;
  { Center it }
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;


procedure TdlgContainer.bbOkClick(Sender: TObject);
begin
  inherited;
  { Test that the addin validates OK }
  if (FOleProxy.ControlInterface as IDialog).DoOk then
  begin
    ModalResult:=mrOk
  end
  else
    ModalResult:=mrNone;
end;

procedure TdlgContainer.bbCancelClick(Sender: TObject);
begin
  inherited;
  (FOleProxy.ControlInterface as IDialog).DoCancel;
end;


end.
