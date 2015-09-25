unit AddfilterContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DialogContainer, StdCtrls, Buttons, ExtCtrls, Recorder2000_TLB,
  ImageListButton;

type
  TdlgAddFilterContainer = class(TdlgContainer)
    procedure bbOkClick(Sender: TObject);
  private
    { Private declarations }
    FFilterList : TInterfaceList;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; iClassID : TGuid); reintroduce; overload;
    destructor Destroy; override;
    property FilterList : TinterfaceList read FFilterList;
  end;

var
  dlgAddFilterContainer: TdlgAddFilterContainer;

//==============================================================================
implementation

uses
  Filter, Wizard;

resourcestring
  ResStr_TooManyFilters = 'A probable failure in the addin filter dialog has occurred.  ' +
                       'Filtered data may not be correct.';

{$R *.DFM}

//==============================================================================
{ TdlgAddFilterContainer }

{ Constructor - sets up the IAdditionalFilterDialog with its field list }
constructor TdlgAddFilterContainer.Create(AOwner: TComponent;
  iClassID: TGuid);
var
   lFieldList :TList;
   lDialogIntf : IAdditionalFilterDialog;
   i : integer;
begin
  inherited Create(AOwner, iClassID);
  FFilterList := TInterfaceList.Create;
  try
    lDialogIntf := FOleProxy.ControlInterface as IAdditionalfilterDialog;
  except
    on EIntfCastError do
      Exit; // can't do anything - wrong sort of ActiveX
  end; // try

  lFieldList := TdlgWizard(AOwner).FilterFields;
  for i := 0 to lFieldList.Count - 1 do
    if TReportField(lFieldList.Items[i]).Filter then
      lDialogIntf.AddFilterableField(TReportField(lFieldList.Items[i]).ItemKey); 
end;

//==============================================================================
procedure TdlgAddFilterContainer.bbOkClick(Sender: TObject);
var
  lAddFilterIntf : IAdditionalFilter;
  lCount : integer;
begin
  inherited;
  lCount := 0;
  repeat
    Inc(lCount);
    lAddFilterIntf := (FOleProxy.ControlInterface as IAdditionalFilterDialog).
                        GetNextFilter;
    { Don't add it unless its valid }
    if lAddFilterIntf <> nil then
      FFilterList.Add(lAddFilterIntf);
  until (lAddFilterIntf = nil) or (lcount >= 255);
  if lCount >= 255 then
    MessageDlg(ResStr_TooManyFilters, mtWarning, [mbOk], 0);
end;

//==============================================================================
{ Destructor }
destructor TdlgAddFilterContainer.Destroy;
begin
  FFilterList.Free;
  inherited Destroy;
end;

//==============================================================================
end.

