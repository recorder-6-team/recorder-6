//==============================================================================
//  Unit:        BaseData
//
//  Implements:  TBaseDataModule
//
//  Description:
//
//  Author:      Ian Addis
//  Created:     22 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 17/01/07 15:46 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ApplicationSettings, Db, ADODB, DatabaseAccessADO;

type
  EBaseDataError = class(Exception);

  TBaseDataModule = class(TDataModule)
  private
    { Private declarations }
  protected
    procedure SetDBPath( const iPath : string ); virtual;
		procedure SetDatabase(dbDatabase: TADOConnection);
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
  end;

//==============================================================================
implementation

{$R *.DFM}

//==============================================================================
{ TdmBaseData }

{ Constructor - read the database path from the main form and initiates the
     setup of all datasets }
constructor TBaseDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDatabase(dmDatabase.dbLocal);
end;

//==============================================================================
{ SetDBPath - sets the databasepath of all dataset components }
procedure TBaseDataModule.SetDBPath(const iPath: string);
var
  i : integer;
begin
  for i := 0 to ComponentCount-1 do
    { Check for relevant datasets }
    if Components[i] is TCustomADODataset then
      TCustomADODataset(Components[i]).ConnectionString := iPath;
end;  // SetDBPath
//==============================================================================
{ SetDatabase - sets the database of all dataset components }
procedure TBaseDataModule.SetDatabase(dbDatabase: TADOConnection);
var i: integer;
begin
  //Examine each component
  for i:= 0 to Self.ComponentCount - 1 do
    if Components[i] is TCustomADODataset then
      TCustomADODataset(Components[i]).Connection := dbDatabase;
end;  // SetDatabase

//==============================================================================
end.
