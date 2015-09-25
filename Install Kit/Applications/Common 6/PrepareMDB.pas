{-------------------------------------------------------------------------------
  Unit:         PrepareMDB.pas

  Defines:      TPrepareMDB

  Description:  Class that loops through the MSDE tables and creates a linked
                table in the corresponding Access database.

  Requires:     Blank MDB file (created in desired version of Access)

  Author:       Ben Collier
  Created:      March 2003

  Last revision information:
    $Revision: 11 $
    $Date: 12/02/09 10:33 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit PrepareMDB;

interface

uses
  SysUtils, Classes, Forms, ADODB, ADOX_TLB, StdCtrls, ComCtrls, GeneralFunctions, ComObj,
  SetupConstants, Functions;

type
  EFailedConnect = class(EOleException);
  ENBNDataMissing = class(Exception);

  TPrepareMDB = class
  private
    FstNewAccessDBPath: string;
    FServerName: string;
    FSaUserName: string;
    FSaPassword: string;
    FTrustedConnection : boolean;
    FconnAccessDB: TADOConnection;
    FconnSQLServerDB: TADOConnection;
    FAccessCatalog: _Catalog;
    FSQLServerCatalog: _Catalog;
    FProgressBar : TProgressBar;
    FLabel       : TLabel;
    FJetProviderString: string;
    procedure CheckForNBNData;
    procedure CreateLinkedTables;
    procedure CreateLinkedTable(const AJetProviderString, AAccessTable, ASQLTableName: string);
    function GetSecurityString(JetConnection: boolean): string;
  public
    constructor Create(const AServerName, ostNewAccessDBPath: string;
      ATrustedConnection: boolean; SaUsername, SaPassword: string); reintroduce;
    destructor Destroy; override;
    procedure Execute(AProgressBar: TProgressBar; ALabel: TLabel);
    procedure Add(const ATable: string);
    procedure Delete(const ATable: string);
    procedure Update(const ATable: string);

  end;

//==============================================================================
implementation

//==============================================================================
// Description: Create connections to both the Access and MSDE databases.
//
// Author: Ben Collier
// Created: 05/02/2003
//------------------------------------------------------------------------------
constructor TPrepareMDB.Create(const AServerName, ostNewAccessDBPath: string;
  ATrustedConnection: boolean; SaUsername, SaPassword: string);
var
  lstSecurity: string;
begin
  inherited Create;

  FstNewAccessDBPath := ostNewAccessDBPath;
  FServerName := AServerName; // SQL Server name
  FSaUserName := SaUsername;
  FSaPassword := SaPassword;
  FTrustedConnection := ATrustedConnection;

  //Connect to the Access Database
  FconnAccessDB := TADOConnection.Create(nil);
  FconnAccessDB.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' +
                                    FstNewAccessDBPath + ';Persist Security Info=False;';
  FconnAccessDB.LoginPrompt := False;
  FconnAccessDB.ConnectionTimeout := 25;
  FconnAccessDB.Connected := true;
  FconnAccessDB.KeepConnection := True;

  // Will raise exceptions if anything goes wrong.
  CheckForNBNData;

  //Connect to the MSDE database
  FconnSQLServerDB := TADOConnection.Create(nil);
  FconnSQLServerDB.ConnectionString := 'Provider=SQLOLEDB.1;' + GetSecurityString(False) +
                                       'Persist Security Info=False;Initial Catalog=NBNData;'+
                                       'Data Source=' + FServerName;
  FconnSQLServerDB.LoginPrompt := False;
  FconnSQLServerDB.ConnectionTimeout := 25;
  FconnSQLServerDB.Connected := true;
  FconnSQLServerDB.KeepConnection := True;

  //Read the Catalog for the Access database and set its ActiveConnection
  FAccessCatalog := CoCatalog.Create;
  FAccessCatalog.Set_ActiveConnection(FconnAccessDB.ConnectionObject);

  //Read the Catalog for the MSDE database and set its ActiveConnection
  FSQLServerCatalog := CoCatalog.Create;
  FSQLServerCatalog.Set_ActiveConnection(FconnSQLServerDB.ConnectionObject);

  // Get a provider string for accessing the mdb file
  lstSecurity := GetSecurityString(True);
  FJetProviderString := 'ODBC;DRIVER={SQL Server};Server=' + FServerName +
                          ';Database=NBNData;' + lstSecurity + 'Mode= ReadWrite';
end;

//==============================================================================
// Description: Drop database connections and free all used memory.
//
// Author: Ben Collier
// Created: 05/02/2003
//------------------------------------------------------------------------------
destructor TPrepareMDB.Destroy;
begin
  if assigned(FConnAccessDB) then
    if FconnAccessDB.Connected then FconnAccessDB.Connected := false;
  if assigned(FconnSQLServerDB) then
  if FconnSQLServerDB.Connected then FconnSQLServerDB.Connected := false;
  FconnAccessDB.Free;
  FconnSQLServerDB.Free;
  inherited;
end;

//==============================================================================
// Description : Create the Linked Tables
// Created : 05/02/2003
//------------------------------------------------------------------------------
procedure TPrepareMDB.Execute(AProgressBar: TProgressBar; ALabel: TLabel);
begin
  FProgressBar := AProgressBar;
  FProgressBar.Position := 0;
  FLabel := ALabel;
  Application.ProcessMessages;
  CreateLinkedTables;
end;

{===============================================================================
 Description : Returns a connection string snippet that defines security.
             If a Jet Connection then the string returned is different
 Created : 28/1/2003}
function TPrepareMDB.GetSecurityString(JetConnection: boolean): string;
var
  lstPassword : string;
begin
  if FTrustedConnection and JetConnection then
    Result := 'Trusted_Connection=Yes;'
  else if FTrustedConnection and (not JetConnection) then
    Result := 'Integrated Security=SSPI;'
  else if JetConnection then
    Result := 'UID=' + FSaUsername + ';PWD=' + FSaPassword + ';'
  else begin
    if FSaPassword = '' then
      lstPassword := ''
    else
      lstPassword := 'password=' + FSaPassword + ';';
    Result := 'User ID=' + FSaUsername + ';' + lstPassword;
  end;
end;

//==============================================================================
// Description: For all MSDE tables that are of type 'TABLE', create a linked Access table.
//              Ignore TableMappings.ini as it only contains 'DATABASE_RELATIONSHIP'
//
// Author: Ben Collier
// Created: 05/02/2003
//------------------------------------------------------------------------------
procedure TPrepareMDB.CreateLinkedTables;
var
  liCounter: integer;
  lstAccessTableName: string;
  lstSQLServerTableName: string;
begin
  //Loop through all SQL Server tables
  for liCounter := 0 to (FSQLServerCatalog.Tables.Count -1) do
  begin
    {NB Not interested in tables of type:
        * ACCESS TABLE: An Access system table
        * PASS-THROUGH: A linked table through an ODBC data source
        * SYSTEM TABLE: A Jet system table
        * VIEW
    }

    //If the MSDE table is of type 'TABLE' then create a linked Access table
    if (FSQLServerCatalog.Tables[liCounter].Get_Type_ = 'TABLE') then
    begin
      lstAccessTableName := FSQLServerCatalog.Tables[liCounter].Name;
      lstSQLServerTableName := FSQLServerCatalog.Tables[liCounter].Name;

      if Assigned(FLabel) then
        FLabel.Caption := ReadableFormat(lstSQLServerTableName);
      //Create linked Access table
      CreateLinkedTable(FJetProviderString, lstAccessTableName, lstSQLServerTableName);
    end;
    if Assigned(FProgressBar) then begin
      FProgressBar.Position := liCounter * 100 div FSQLServerCatalog.Tables.Count;
      Application.ProcessMessages;
    end;
  end;
end;

//==============================================================================
// Description: Create the linked table from the Access database to the SQL server
//
// Author: Ben Collier
// Created: 05/02/2003
//------------------------------------------------------------------------------
procedure TPrepareMDB.CreateLinkedTable(const AJetProviderString, AAccessTable, 
    ASQLTableName: string);
var
  lLinkTable: _Table;
begin
  // skip the concept lineage table as Access can't cope with its structure
  if not SameText(ASQLTableName, 'Concept_Lineage') then begin
    lLinkTable := coTable.Create;
    lLinkTable.ParentCatalog := FAccessCatalog;

    with lLinkTable do begin
      Name := AAccessTable;
      //Properties['Jet OLEDB:Link Datasource'].value := istDirName ;
      Properties['Jet OLEDB:Link Provider String'].value := AJetProviderString;
      Properties['Jet OLEDB:Remote Table Name'].value := ASQLTableName;
      Properties['Jet OLEDB:Create Link'].value := True;
    end;// With
    FAccessCatalog.Tables.Append(lLinkTable);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TPrepareMDB.CheckForNBNData;
var
  lRs: _Recordset;
  lConn: TADOConnection;
begin
  lConn := TADOConnection.Create(nil);
  try
    lConn.ConnectionString := 'Provider=SQLOLEDB.1;' + GetSecurityString(False) +
                              'Persist Security Info=False;Initial Catalog=master;' +
                              'Data Source=' + FServerName;
    try
      lConn.Open;
    except
      on E:EOleException do
        raise EFailedConnect.Create('Failed to connect to Server ''' + FServerName + '''.' +
                                    #13#10 + E.Message,
                                    E.ErrorCode, E.Source, E.HelpFile, E.HelpContext);
    end;
    lRs := lConn.Execute('SELECT * FROM SysDatabases WHERE Name=''NBNData''');
    if lRs.Eof then
      raise ENBNDataMissing.Create('NBNData not found');
  finally
    lRs := nil;
    lConn.Free;
  end;
end;  // TPrepareMDB.CheckForNBNData

{-------------------------------------------------------------------------------
  Add a new linked table
}
procedure TPrepareMDB.Add(const ATable: string);
begin
  CreateLinkedTable(FJetProviderString, ATable, ATable);
end;

{-------------------------------------------------------------------------------
  Delete a linked table
}
procedure TPrepareMDB.Delete(const ATable: string);
begin
  try
    FAccessCatalog.Tables.Delete(ATable)
  except
    On Exception do ;
  end;
end;

{-------------------------------------------------------------------------------
  Update a linked table by dropping and recreating it
}
procedure TPrepareMDB.Update(const ATable: string);
begin
  Delete(ATable);
  Add(ATable);
end;



end.
