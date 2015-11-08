unit HierarchyFunctions;

interface

Uses Windows, Messages, ApplicationSettings, SysUtils, ADODB, DatabaseAccessADO, ADOX_TLB,Variants;

function GetPreferredCustodians(AName : string) :string;

implementation

function GetPreferredCustodians(AName : string) :string;
var keys: _Recordset;

begin

  keys := dmDatabase.ExecuteSQL('SELECT Data FROM [Setting] WHERE Name= ''' + AName  + '''', true);
  if (keys.recordcount>0) and (Not AppSettings.UseOriginalIcons) then result := VarToStr(keys.fields[0].value)
  else result := ''
end;

end.

