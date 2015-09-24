unit UpdateDatabaseProperties;

{======================================================================================

UNIT: UpdateDatabaseProperties
DESCRIPTION: Used to insert a new property in the File|Database Properties... custom tab
             of an Access 97 database. The AddProperty procedure can be used to add version
             information to the database when a new database update pack has been installed.
USES: The imported type library from:
      "c:\Program Files\Common Files\Microsoft Shared\DAO\dao350.dll"

Andrew Cottam, JNCC 20/9/00

=======================================================================================}
interface

 uses DAO_tlb;

 procedure AddProperty (DatabaseFilename:String;PropertyName:string;PropertyValue:String);
{DatabaseFilename - the drive, path and filename of the Access 97 database (String)
 PropertyName - the Name of the property to add. This will appear in the Name field (String)
 PropertyValue - the Value of the property to add. This will appear in the Value field (String)}
 
implementation

procedure AddProperty (DatabaseFilename:String;PropertyName:string;PropertyValue:String);
var
 dbeng:_DBEngine;
 Db:Database;
 UserDefDoc:Document;
 Count:Integer;
 NewProp:Property_;
begin
 dbeng:=CoDBEngine.Create;
 db:=dbeng.OpenDatabase(DatabaseFilename,EmptyParam,EmptyParam,EmptyParam);
 UserDefDoc :=db.Containers.Item['Databases'].Documents.Item['UserDefined'];
 For Count:=0 to UserDefDoc.Properties.Count -1 do
 If UserDefDoc.Properties.Item[Count].Name= PropertyName Then UserDefDoc.Properties.Delete (PropertyName);
 NewProp:= db.CreateProperty(PropertyName, 10, PropertyValue,EmptyParam);
 UserDefDoc.Properties.Append(NewProp);
 UserDefDoc.Properties.Refresh;
 db.Close;
 db := nil; // to make sure
end;

end.
