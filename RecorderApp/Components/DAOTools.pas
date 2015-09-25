{-------------------------------------------------------------------------------
  Unit:         DAOTools.pas

  Defines:      <nothing>

  Description:  Unit that exposes some functions useful when using DAO in
                Recorder 2003.  This is Delphi 4 and Delphi 7 compatible.

  Author:
  Created:      28 January 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/05/04 13:18 $
    $Author: Ericsalmon $

  Copyright © Dorset Software Services Ltd, 2003

-------------------------------------------------------------------------------}

unit DAOTools;

interface

uses  Sysutils, Classes, DAO_TLB, ApiUtils, ActiveX, ShlObj, Dialogs;

type
  TDAOLink = record
    Database : Database;
    Engine : DBEngine;
  end;

procedure CheckTableLinks(const ADatabasePath: String; const ADatabasePassword: String = '';
  ADAODatabase: Database = nil);
function InitDAODatabase(const DatabasePath, DatabasePassword: string): TDAOLink;

//==============================================================================
implementation

const
  // developer license key for DAO
  DAO_KEY                  = 'mbmabptebkjcdlgtjmskjwtsdhjbmkmwtrak';
  // ClsID for DAO - must be OLE2 type TGUID for some obscure reason
  DAO_ENGINE_ID : TGUID =
           (D1:$00000021;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$6D,$2E,$A4));
  // and the ClassFactory2 required
  IID_IClassFactory2 : TGUID =
           (D1:$B196B28F;D2:$BAB4;D3:$101A;D4:($B6,$9C,$00,$AA,$00,$34,$1D,$07));

{===============================================================================
 Description : Returns a DAO database and engine linked to the path supplied
 Created : 28/1/2003 }
function InitDAODatabase(const DatabasePath, DatabasePassword : string) : TDAOLink;
var
  lClsID : TCLSID;
  lClassFactory2 : IClassFactory2;
begin
  {get clsid for dao dbengine - wrapper handles API exceptions}
  try
    gpcAPIResultCheck(CLSIDFromProgID('DAO.DBENGINE.35', lClsId));
  except
    on E:EAPIError do
      gpcAPIResultCheck(CLSIDFromProgID('DAO.DBENGINE.36', lClsId));
  end;
  
  {get class factory 2 interface - required to create a licensed dbengine}
  gpcAPIResultCheck(CoGetClassObject(lClsid, CLSCTX_INPROC_SERVER, Nil,
                                 IID_IClassFactory2, lClassFactory2 ));

  gpcAPIResultCheck(lClassFactory2.CreateInstanceLic( Nil, Nil, DAO_ENGINE_ID,
          DAO_KEY, Result.Engine ));
  {parameters specify not opened in Exclusive mode, Read-only and
   pass the password from the registry}
  Result.Database := Result.Engine.OpenDatabase(DatabasePath, True, False, ';uid=admin;pwd=' + DatabasePassword);
end;

{===============================================================================
 Description : Ensure that the access files we are importing from are linked
               together correctly.
 Created : 28/1/2003 }
procedure CheckTableLinks(const ADatabasePath: String; const ADatabasePassword: String = '';
  ADAODatabase: Database = nil);
var
  i            : integer;
  lslTableNames: TStringList;
  lstTablePath : string;
  lDAOLink     : TDAOLink;
  lstFileName  : string;
begin
  lslTableNames := TStringList.Create;
  if not Assigned(ADAODatabase) then
    lDAOLink := InitDAODatabase(ADatabasePath, ADatabasePassword)
  else
    lDAOLink.Database := ADAODatabase;
  try
    for i := 1 to lDAOLink.Database.TableDefs.Count-1 do
      with lDAOLink.Database.TableDefs[i] do begin
        // Only examine link if there is one
        if Connect <> '' then begin
          if CompareText(Copy(Connect, 1, 10), ';DATABASE=')=0 then begin
            lstFileName := ExtractFileName(Copy(Connect, 12, 255));
            lstTablePath := ExtractFilePath(ADatabasePath) + lstFileName;
            if (dbAttachedTable and Attributes)>0 then
              if (CompareText(Connect, ';DATABASE=' + lstTablePath)<>0)
                 and FileExists(lstTablePath) then begin
                Connect := ';DATABASE=' + lstTablePath;
                RefreshLink;
              end;
          end; // if
        end; // if Connect <> ''
      end; // with
  finally
    lslTableNames.Free;
  end; // try
end;

//==============================================================================
end.
 