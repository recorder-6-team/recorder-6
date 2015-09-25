unit DBListCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, DBCtrls;

resourcestring
  ResStr_None = '<None>';
  ResStr_UnableToAddDS = 'Unable to add a datasource which is not attached to ' +
                         'a valid dataset';
  ResStr_DataSourceNotSet = 'Datasource not set';
  ResStr_DataSourceInvalidDataSet = 'Datasource does not point to a valid dataset';
  ResStr_KeyFieldNotSet = 'KeyField not set';
  ResStr_KeyFieldNotFound = 'KeyField not found in dataset';
  ResStr_ListFieldNotSet =  'ListField not set';
  ResStr_ListFieldNotFound = 'ListField not found in dataset';

type
  EDBListCombo = class(Exception);
  EInvalidKeyField = class(EDBListCombo);
  EInvalidListField = class(EDBListCombo);
  EInvalidDatasource = class(EDBListCombo);
  EInvalidDataSet = class(EDBListCombo);

  TDBListCombo = class(TComboBox)
  private
    { Private declarations }
    FstlKey: TStringList;
    FListField: string;
    FKeyField: string;
    FDataSource: TDataSource;
    FKeyValue: string;
    FActive: boolean;
    FEmptyItem: boolean;
    FReadOnly :boolean;
    FLastIndex:integer;
    FtfKeyDown : boolean;// Indicates whether a key is currently pressed.
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource;
    procedure SetKeyValue(const Value: string);
    function GetKeyValue: string;
    procedure PopulateList;
    procedure SetActive(const Value: boolean);
    function ValidateDatasource: boolean;
    function ValidateKeyField: boolean;
    function ValidateListField: boolean;
    procedure ClearItems;
    procedure SetEmptyItem(const Value: boolean);
  protected
    { Protected declarations }
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property KeyValue: string read GetKeyValue write SetKeyValue;
    property ListField: string read FListField write FListField;
    property KeyField: string read FKeyField write FKeyField;
    property Datasource: TDataSource read GetDataSource write SetDataSource;
    property Active: boolean read FActive write SetActive;
    property EmptyItem: boolean read FEmptyItem write SetEmptyItem;
    property ReadOnly:boolean read FReadOnly write FReadOnly;
  end;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('JNCC', [TDBListCombo]);
end;

//==============================================================================

{ TDBListCombo }

constructor TDBListCombo.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FstlKey := TStringList.Create;
  FEmptyItem := False;
  FLastIndex :=-1;
  FtfKeyDown := false;
end;

//==============================================================================
destructor TDBListCombo.Destroy;
begin
  FstlKey.Free;
  Inherited Destroy;
end;

//==============================================================================
function TDBListCombo.GetDataSource: TDataSource;
begin
  if Assigned(FDatasource) then
    Result := FDataSource
  else
    Result := nil;
end;

//==============================================================================
function TDBListCombo.GetKeyValue: string;
begin
  if ItemIndex <> -1 then
    FKeyValue := FstlKey[ItemIndex];
  Result := FKeyValue;
end;

//==============================================================================
procedure TDBListCombo.ClearItems;
begin
  FstlKey.Clear;
  Items.Clear;
end;  // ClearItems

//==============================================================================
procedure TDBListCombo.PopulateList;
var
  lstKey: string;
  lstList: string;
  liIndex: integer;
begin
  ClearItems;
  // Ensure dataset gets refreshed properly
  if FDatasource.Dataset.Active then
    FDatasource.Dataset.Close;
  FDatasource.Dataset.Open;
  try
    if ValidateKeyField and ValidateListField then
    begin
      //If a <None> item is required in the list, add it and an empty key.
      if FEmptyItem then
      begin
        liIndex := Items.Add(ResStr_None);
        FstlKey.Insert(liIndex, '');
      end;

      with FDatasource.Dataset do
      begin
        First;
        while not Eof do
        begin
          lstKey := FieldByName(FKeyField).AsString;
          lstList := Fieldbyname(FListField).AsString;
          liIndex := Items.Add(lstList);
          FstlKey.Insert(liIndex, lstKey);
          Next;
        end;
      end;
      ItemIndex := 0;
      SetKeyValue(FKeyValue);
    end;
  finally
    FDatasource.Dataset.Close;
  end;
end;  // PopulateList

//==============================================================================
procedure TDBListCombo.SetActive(const Value: boolean);
begin
  if Value <> FActive then
  begin
    if Value then
      PopulateList
    else
      ClearItems;
    FActive := Value;
  end;
end;  // SetActive

//==============================================================================
procedure TDBListCombo.SetDataSource(Value: TDataSource);
begin
  if Value <> nil then
  begin
    if Value.Dataset = nil then
      Raise EInvalidDataSet.Create(ResStr_UnableToAddDS);
    FDataSource := Value;
  end else
    FDataSource := nil;
end;  // SetDataSource

//==============================================================================
procedure TDBListCombo.SetKeyValue(const Value: string);
begin
  FKeyValue := Value;
  ItemIndex := -1;
  if FKeyValue <> '' then
  	ItemIndex := FstlKey.IndexOf(FKeyValue);
  FLastIndex:=ItemIndex;
end;  // SetKeyValue

//==============================================================================
function TDBListCombo.ValidateDatasource: boolean;
begin
  if FDatasource = nil then
    Raise EInvalidDatasource.Create(ResStr_DataSourceNotSet)
  else if FDatasource.DataSet = nil then
    Raise EInvalidDatasource.Create(ResStr_DataSourceInvalidDataSet)
  else
    Result := true;
end;  // ValidateDatasource

//==============================================================================
function TDBListCombo.ValidateKeyField: boolean;
var i: integer;
    lKeyField:string;
begin
  Result := false;
  if ValidateDatasource then
  begin
    if FKeyField = '' then
      Raise EInvalidKeyField.Create(ResStr_KeyFieldNotSet)
    else begin
      lKeyField:=UpperCase(FKeyField);
      for i := 0 to FDatasource.Dataset.FieldCount - 1 do
        if UpperCase(FDatasource.DataSet.Fields[i].FieldName) = lKeyField then
          Result := true;
    end;
    if not Result then
      Raise EInvalidKeyField.Create(ResStr_KeyFieldNotFound);
  end;
end;  // ValidateKeyField

//==============================================================================
function TDBListCombo.ValidateListField: boolean;
var i: integer;
    lListField:string;
begin
  Result := false;
  if ValidateDatasource then
  begin
    if FListField = '' then
      Raise EInvalidListField.Create(ResStr_ListFieldNotSet)
    else begin
      lListField:=UpperCase(FListField);
      for i := 0 to FDatasource.Dataset.FieldCount - 1 do
        if UpperCase(FDatasource.DataSet.Fields[i].FieldName) = lListField then
          Result := true;
    end;
    if not Result then
      Raise EInvalidListField.Create(ResStr_ListFieldNotFound);
  end;
end;  // ValidateListField

//==============================================================================
procedure TDBListCombo.SetEmptyItem(const Value: boolean);
begin
  if FEmptyItem <> Value then
  begin
    FEmptyItem := Value;
    if FActive then
      PopulateList;
  end;
end;  // SetEmptyItem

//==============================================================================
procedure TDBListCombo.Change;
begin
  if not ReadOnly then begin
    inherited Change;
    FLastIndex:=ItemIndex;
  end else begin
    ItemIndex:=FLastIndex;
    if not FtfKeyDown then
      //Pretend that a key has been pressed so that the BaseChildUnit will give
      //a message.
      //Not necessary if this has been done by a key press.
      PostMessage(Handle, WM_CHAR, VK_Return, 0);
  end;
end;  // Change



//==============================================================================


procedure TDBListCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //Save the fact that a key press message has already been posted
  inherited;
  FtfKeyDown := True;
end;

procedure TDBListCombo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  // Save the fact that the key is no longer pressed
  inherited;
  FtfKeydown := False;
end;

end.
