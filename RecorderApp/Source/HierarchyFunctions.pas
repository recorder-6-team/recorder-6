unit HierarchyFunctions;

interface

Uses Windows, Messages, ApplicationSettings, SysUtils, ADODB, DatabaseAccessADO, ADOX_TLB,Variants;

function GetPreferredCustodians(AName : string) :string;
function GetLocationStatus(AKey : string) :integer;

implementation

function GetPreferredCustodians(AName : string) :string;
var keys: _Recordset;

begin

  keys := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name= ''' + AName  + '''', true);
  if (keys.recordcount>0) and (Not AppSettings.UseOriginalIcons) then result := VarToStr(keys.fields[0].value)
  else result := '';
  keys.close;
end;

function GetLocationStatus(AKey : string) :integer;
var rs: _Recordset;

begin
  Result := 0;
  IF NOT AppSettings.UseOriginalIcons then
  begin
    rs := dmDatabase.ExecuteSQL('SELECT [dbo].[ufn_Location_Expired] (''' + AKey  + ''')', true);
    if not rs.eof then
      if rs.fields[0].Value = '1' then Result := 1;
    rs.Close;
  end;
end;

end.

