unit SpatialComAddins;

interface

uses
  Sysutils, Classes, ComObj, ExceptionForm, ActiveX;

type
  ESpatialAddinError = class(TExceptionPath);

  TSpatialComAddins = class
  private
    FSpatialAddins : TStringList;
    FSpatialAddinIntfs : TInterfaceList;
  protected
    procedure ReadSpatialAddins( iValidatorClassID : TGUID );
    procedure CheckIfSpatialAddin(iClsID : TGUID);
  public
    constructor Create( iValidatorClassID : TGUID );
    destructor Destroy; override;
    property SpatialAddins : TStringList read FSpatialAddins;
    property SpatialAddinIntfs : TInterfaceList read FSpatialAddinIntfs;
  end;


implementation

uses
  Recorder2000_TLB, Registry, Constants, Windows;

{ TSpatialComAddins }

//==============================================================================
procedure TSpatialComAddins.CheckIfSpatialAddin( iClsID : TGUID );
var
  lSpatialSystem : ISpatialReference;
  lSpatialSystemList: ISpatialReferenceList;
  lComObject : IUnknown;
  i: Integer;
begin
  lComObject := CreateComObject(iClsID);
  if Supports(lComObject, IID_ISpatialReference, lSpatialSystem) then
  begin
    { Store the name on a list }
    FSpatialAddins.Add(lSpatialSystem.SpatialRefSystem);
    { And on a matching interface list, store the interface pointer }
    FSpatialAddinIntfs.Add(lComObject);
  end
  else if Supports(lComObject, IID_ISpatialReferenceList, lSpatialSystemList) then
  begin
    for i:=0 to lSpatialSystemList.SystemCount-1 do
    begin
      FSpatialAddins.Add(lSpatialSystemList.SystemInterface[i].SpatialRefSystem);
      FSpatialAddinIntfs.Add(lSpatialSystemList.SystemInterface[i])
    end;
  end;
end;


//==============================================================================
{ iValidatorClassID allows us to avoid recreating the validation library when
    we scan for spatial reference addins.  Otherwise we get a recurring call to
    this code as it is called from initialize of the object }
constructor TSpatialComAddins.Create( iValidatorClassID : TGUID );
begin
  inherited Create;
  FSpatialAddins := TStringList.Create;
  FSpatialAddinIntfs := TInterfaceList.Create;
  ReadSpatialAddins( iValidatorClassID );
end;

//==============================================================================
destructor TSpatialComAddins.Destroy;
begin
  FSpatialAddins.Free;
  FSpatialAddinIntfs.Free;
  inherited;
end;

//==============================================================================
procedure TSpatialComAddins.ReadSpatialAddins( iValidatorClassID : TGUID );
var
  lRegistry : TRegistry;
  lKeys : TStringList;
  i : integer;
  lInstalled : boolean;
  lClsId : TGUID;
begin
  { Read COM objects from the registry }
  lRegistry := TRegistry.Create;
  lKeys := TStringList.Create;
  try
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    lRegistry.OpenKeyReadOnly(REG_KEY_ADDIN);
    lRegistry.GetKeyNames(lKeys);  { Create }
    { Loop through the objects in regsitry }
    for i := 0 to lKeys.Count - 1 do
    begin
      lRegistry.CloseKey;
      if not lRegistry.OpenKeyReadOnly(REG_KEY_ADDIN + '\' + lKeys[i]) then
        raise ESpatialAddinError.Create(ResStr_RegistryProblem);
      lInstalled := (lRegistry.ReadString(REG_INSTALLED)='1');
      { Check its installed.  If not, ignore it }
      if lInstalled then
      begin
        lClsID := StringToGuid(lRegistry.ReadString(REG_CLASS_ID));
        if not IsEqualGUID(lClsID, iValidatorClassID) then
          CheckIfSpatialAddin(lClsID);
      end;
    end;
  finally
    lRegistry.Free;
    lKeys.Free;
  end;// try..finally
end;

end.
