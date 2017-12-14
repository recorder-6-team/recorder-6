unit IDComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TIDComboBox = class(TComboBox)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FIDArray : Array of LongInt; //TLongIntDynArray;
    FCapacity : integer;  // If > 0, then defines the size of the dynarray
    function GetCurrentID: Integer;
    function GetCurrentText: String;
    function GetID(iIndex: Integer): LongInt;
    procedure SetCapacity(liNewValue: Integer);
    function GetCapacity: Integer;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    function AddWithID(iText: String; iID: Integer): Integer;
    procedure ClearAll;
    procedure DeleteItem(iItemIndex: Integer);
    function IndexForID(const iiID: integer): integer;
    property CurrentId : integer read GetCurrentID;
    property CurrentText: String read GetCurrentText;
    property ID[iIndex: Integer]: LongInt read GetID;
    property Capacity : Integer read GetCapacity write SetCapacity;
  published
    { Published declarations }
  end;

//procedure Register;

implementation
{
procedure Register;
begin
  RegisterComponents('In House', [TIDComboBox]);
end;
}

resourcestring
  ResStr_MaxCapacityExceeded =  'Maximum capacity of item list exceeded';

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
constructor TIDComboBox.Create( AOwner : TComponent );
begin
  inherited Create(AOwner);
//  FIDArray := TLongIntDynArray.Create(0);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
destructor TIDComboBox.Destroy;
begin
//  FIDArray.Free;
  inherited Destroy;
end;


{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TIDComboBox.AddWithID( iText : String; iID : Integer): Integer;
var
  liIndex:    Integer;
  liNewIndex: Integer;
begin
  if (FCapacity > 0) and (Items.Count >= FCapacity) then
    raise ERangeError.Create(ResStr_MaxCapacityExceeded);

  liNewIndex := Items.Add( iText );
  if FCapacity=0 then
    SetLength(FIDArray, Items.Count); //FIDArray.ReSize( Items.Count );

  for liIndex := Items.Count - 2 downto liNewIndex do begin
//    FIDArray.Items[liIndex + 1] := FIDArray.Items[liIndex];
    FIDArray[liIndex + 1] := FIDArray[liIndex];
  end;

//  FIDArray.Items[liNewIndex] := iId;
  FIDArray[liNewIndex] := iId;

  Result := liNewIndex;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TIDComboBox.GetCurrentID : Integer;
begin
//  Result := FIDArray.Items[ItemIndex];
  Result := FIDArray[ItemIndex];
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TIDComboBox.GetCurrentText: String;
begin
  if ItemIndex = -1 then
    Result := ''
  else
    Result := Items.Strings[ItemIndex];
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
function TIDComboBox.GetID(iIndex: Integer): LongInt;
begin
//  Result := FIDArray.Items[iIndex];
  Result := FIDArray[iIndex];
end;

{-----------------------------------------------------------------------------}
{ Delete an item both from the items and the idarray
{-----------------------------------------------------------------------------}
procedure TIDComboBox.DeleteItem( iItemIndex : Integer );
var
  j: integer;
begin
  Items.Delete(iItemIndex);
  // Do up to Items.Count and not Count-1, as one has just been deleted
  for j := iItemIndex to Items.Count do  // FIdArray.Size-2 do    // Iterate
//    FIdArray.Items[j] := FIdArray.Items[j+1];
    FIdArray[j] := FIdArray[j+1];

  if FCapacity = 0 then
//    FIdArray.Resize(Items.Count);
    SetLength(FIdArray, Items.Count);
end;

{ ------------------------------------------------------------------------------ }
{ ------------------------------------------------------------------------------ }
procedure TIDComboBox.ClearAll;
begin
  Clear;
//  FIDArray.Resize(0);
  SetLength(FIDArray, 0);
  if FCapacity>0 then
//    FIDArray.Resize(FCapacity);
    SetLength(FIDArray, FCapacity);
end;


{ Accessor method - assigns value to Capacity property }
procedure TIDComboBox.SetCapacity(liNewValue: Integer);
begin
  FCapacity := liNewValue;
  { Don't resize if capacity is zero, you might throw away wanted data }
  if FCapacity > 0 then
//    FIDArray.Resize(FCapacity);
    SetLength(FIDArray, FCapacity);
end;


{ Accessor method - reads value from FCapacity }
function TIDComboBox.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TIDComboBox.IndexForID(const iiID : integer): integer;
var liIdx: Integer;
begin
//  Result:=FIDArray.IndexForID(iiID);
  Result := -1;
  for liIdx := 0 to Items.Count - 1 do
    if FIdArray[liIdx] = iiID then begin
      Result := liIdx;
      Break;
    end;
end;      {IndexForID}

//==============================================================================
end.
