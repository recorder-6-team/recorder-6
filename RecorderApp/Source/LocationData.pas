//==============================================================================
//  Unit:        LocationData
//
//  Implements:  TdmLocation
//
//  Description: Implements data access functionality for the Locations screen.
//
//  Author:      Paul Thomas
//  Created:     23 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 19 $
//    $Date: 17/01/08 19:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit LocationData;

interface           

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, ADODB, AdoInt;

const
  SQL_LOCATION_LEVEL = 'SELECT L.Location_Key, LN.Item_Name, L.Spatial_Ref, L.File_Code, System_Supplied_Data,L.Custodian '+
                  'FROM Location AS L INNER JOIN Location_Name AS LN ON '+
                  'L.Location_Key = LN.Location_Key '+
                  'WHERE L.Parent_Key %s '+
                  'AND   LN.Preferred = 1 '+
                  '%s '+
                  'ORDER BY %s';

  SQL_FEATURE_LEVEL='SELECT F.Location_Feature_Key, '+
       '                    F.Item_Name, '+
       '                    FT.Short_Name '+
       'FROM (Location_Feature AS F '+
       'LEFT JOIN Location_Feature_Grading AS FG '+
       'ON        F.Feature_Grading_Key = FG.Feature_Grading_Key) '+
       'LEFT JOIN Location_Feature_Type AS FT '+
       'ON        FG.Location_Feature_Type_Key = FT.Location_Feature_Type_Key '+
       'WHERE F.Location_Key = ''%s'' '+
       '%s '+
       'ORDER BY %s';

type
  TLocationNodeType = (ntTopSite, ntChildSite, ntFeature);

  TdmLocation = class(TBaseDataModule)
    qryFindParentLoc: TJNCCQuery;
    qryAddBoundaryLink: TJNCCQuery;
    cmdFetch: TADOCommand;
  private
    FLocationOrder : string;
    FFeatureOrder  : string;
    FLocationFilter : string;
    FFeatureFilter: string;
    function HasFilteredChildNodes(const parentKey: String): Boolean;
    procedure SetLocationFilter(const Value: string);
    procedure SetFeatureOrder(const Value: string);
    procedure SetFeatureFilter(const Value: string);
  public
    constructor Create(AOwner: TComponent; const ASortField, AFeatureField: string); reintroduce;
    function GetRecordset(const ANodeType : TLocationNodeType;
             AParentKey : string='') : _Recordset;
    property LocationFilter : string read FLocationFilter write SetLocationFilter;
    property FeatureFilter : string read FFeatureFilter write SetFeatureFilter;
    property FeatureOrder : string read FFeatureOrder write SetFeatureOrder;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  DatabaseAccessADO, ApplicationSettings, Constants;

//==============================================================================
{ TdmLocation }

constructor TdmLocation.Create(AOwner: TComponent; const ASortField, AFeatureField: string);
begin
  inherited Create(AOwner);
  cmdFetch.Connection := qryFindParentLoc.Connection;
  // use remembered sort orders.
  if ASortField = 'Location_Name' then
    FLocationOrder := 'LN.Item_Name'
  else
    FLocationOrder := ASortField;

  FFeatureOrder := AFeatureField;
  FLocationFilter := '';
  FFeatureFilter := '';
end;


{-------------------------------------------------------------------------------
  Description : Return a recordset for populating the treeview
              Pass a parent key for child sites and features
  Created : 21/03/2003 }
function TdmLocation.GetRecordset(const ANodeType: TLocationNodeType;
  AParentKey: string = ''): _Recordset;
var
  lFilter: String;
begin
  if ANodeType in [ntTopSite, ntChildSite] then begin
    if (FLocationFilter <> '') and
       ((ANodeType = ntTopSite) or HasFilteredChildNodes(AParentKey)) then
      lFilter := 'AND L.LOCATION_KEY IN (' + FLocationFilter + ')'
    else
      lFilter := '';
  end else
  // Features always under a parent location.
  if (FFeatureFilter <> '') and HasFilteredChildNodes(AParentKey) then
    lFilter := 'AND F.LOCATION_FEATURE_KEY IN (' + FFeatureFilter + ')'
  else
    lFilter := '';

  case ANodeType of
    ntTopSite :
      cmdFetch.CommandText := Format(SQL_LOCATION_LEVEL, [
              'IS NULL', lFilter, FLocationOrder]);
    ntChildSite :
      cmdFetch.CommandText := Format(SQL_LOCATION_LEVEL, [
                '='''+AParentKey+'''', lFilter, FLocationOrder]);
    ntFeature :
      cmdFetch.CommandText := Format(SQL_FEATURE_LEVEL, [
                AParentKey, lFilter, FFeatureOrder]);
  end;
  Result := cmdFetch.Execute;
end;

{-------------------------------------------------------------------------------
  Checks a location has either filtered (external filters) locations or features.
}
function TdmLocation.HasFilteredChildNodes(const parentKey: String): Boolean;
var
  rs: _Recordset;
begin
  Result := False;
  rs := dmDatabase.ExecuteSQL(
      'SELECT Location_Key FROM Location WHERE Parent_Key = ''' + parentKey + '''', True);
  try
    while not rs.Eof do begin
      if Pos(rs.Fields['Location_Key'].Value, FLocationFilter) > 0 then begin
        Result := True;
        Exit;
      end;
      rs.MoveNext;
    end;
  finally
    rs.Close;
  end;

  rs := dmDatabase.ExecuteSQL(
      'SELECT Location_Feature_Key FROM Location_Feature WHERE Location_Key = ''' + parentKey + '''', True);
  try
    while not rs.Eof do begin
      if Pos(rs.Fields['Location_Feature_Key'].Value, FFeatureFilter) > 0 then begin
        Result := True;
        Exit;
      end;
      rs.MoveNext;
    end;
  finally
    rs.Close;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TdmLocation.SetFeatureFilter(const Value: string);
begin
  FFeatureFilter := Value;
end;

{-------------------------------------------------------------------------------
  Description : Accessor method
  Created : 27/03/2003 }
procedure TdmLocation.SetFeatureOrder(const Value: string);
begin
  FFeatureOrder := Value;
end;

{-------------------------------------------------------------------------------
  Description : Accessor method
  Created : 27/03/2003 }
procedure TdmLocation.SetLocationFilter(const Value: string);
begin
  FLocationFilter := Value;
  // disable the feature filter
  FFeatureFilter := '';
end;

end.

