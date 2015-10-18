unit OptionDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormpOption = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    Shape: TShape;
    cbApply: TCheckBox;
    bOk: TButton;
    Label2: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure Set_Apply(const Value: boolean);
    function Get_Apply: boolean;
    function Get_DateFrom: string;
    function Get_dateTo: string;
    function Get_UseOption: integer;
    function isDate(str: string):Boolean;
    function Get_UseDate : Boolean;
    function Get_HasOptions :Boolean;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent);
    destructor Destroy; override;
    property Apply: boolean read Get_Apply write Set_Apply;
    property DateFrom: string read Get_DateFrom;
    property DateTo: string read Get_DateTo;
    property UseOption : integer read Get_UseOption;
    property Usedate :Boolean read Get_UseDate;
    property HasOptions :Boolean read Get_HasOptions;
  end;

var
  FormpOption: TFormpOption;

implementation

{$R *.dfm}

{ TFormpOption }

constructor TFormpOption.Create(aOwner: TComponent);
begin
     inherited Create(aOwner);
end;

destructor TFormpOption.Destroy;
begin
    inherited;
end;

function TFormpOption.Get_Apply: boolean;
begin
     Result := cbApply.checked;
end;

function TFormpOption.Get_DateFrom: string;
begin
   if isdate('01/01/' + Edit1.text) then
     Result:=  Edit1.Text
   else
     Result := '';
end;

function TFormpOption.Get_dateTo: string;
begin
   if isdate('31/12/' + Edit2.text) then
     Result:=  Edit2.Text
   else
     Result := '';
end;

function TFormpOption.Get_UseDate: Boolean;
begin
   if isdate('01/01/'+ Edit1.Text) and isdate('31/12/' + Edit2.Text) then
     Result := true
   else
     Result := false;
end;

function TFormpOption.Get_UseOption: integer;
begin
   Result:= RadioGroup1.itemindex
end;

function TFormpOption.isDate(str: string): Boolean;
var dt: TdateTime;
begin
 Result := true;
 Try
   dt := StrToDate(str);
   except
     Result := false;
 end;
end;

procedure TFormpOption.Set_Apply(const Value: boolean);
begin
    cbApply.Checked := Value;
end;

procedure TFormpOption.Edit1KeyPress(Sender: TObject; var Key: Char);
begin

 if ((ord(key) <48) or (ord(key) > 57)) and (ord(key) <> 8) then
 key := '0';



end;

procedure TFormpOption.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
    if ((ord(key) <48) or (ord(key) > 57)) and (ord(key) <> 8) then
    key := '0';
end;

function TFormpOption.Get_HasOptions: Boolean;
begin
  if ((edit1.Text <> '') and (edit2.text <> '')) or (RadioGroup1.ItemIndex = 1) then
    Result := true
  else
    Result := false;
end;
end.
