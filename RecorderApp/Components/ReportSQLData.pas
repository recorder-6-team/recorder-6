//==============================================================================
//  Unit: SQLData
//
//  Implements:
//
//  Description: Stores the SQL code for each of the attributes that can be
//               returned by the report wizard. The SQL is broken down in to the
//               following blocks corresponding to an UPDATE Statement:
//               1. AttributeSQL holds SQL for the [multiple] SET statement[s]
//               2. JoinSQL holds the SQL for the FROM statement
//               3. WhereSQL holds the SQL for the WHERE clause
//
//  Author: Ben Collier
//  Created: 19/11/2002
//
//  Last Revision Details:
//    $Revision: 1 $
//    $Date: 4/12/02 10:17 $
//    $Author: Bencollier $
//
//  $History: ReportSQLData.pas $
//  
//  *****************  Version 1  *****************
//  User: Bencollier   Date: 4/12/02    Time: 10:17
//  Created in $/JNCC/Components
//  Initial Build
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================

unit ReportSQLData;

interface
uses classes, contnrs;

type
  TJoinSQLData = class
  private
    FJoinKey: string;
    FJoinSQL: string;
    FAttributes: TObjectList;
  public
    constructor Create(istJoinKey: string; istJoinSQL: string);
    destructor Destroy; override;
    property JoinKey: string read FJoinKey write FJoinKey;
    property JoinSQL: string read FJoinSQL write FJoinSQL;
    property Attributes: TObjectList read FAttributes;
  end;

type
  TAttributeSQLData = class
  private
    FItemName: string;
    FAttributeKey: string;
    FWhereKey: string;
    FAttributeSQL: string;
    FWhereSQL: string;
  public
    constructor Create(istAttributeKey: string; istAttributeSQL: string; istItemName: string;
        istWhereSQL: string);
    destructor Destroy; override;
    property AttributeKey: string read FAttributeKey write FAttributeKey;
    property WhereKey: string read FWhereKey write FWhereKey;
    property ItemName: string read FItemName write FItemName;
    property AttributeSQL: string read FAttributeSQL write FAttributeSQL;
    property WhereSQL: string read FWhereSQL write FWhereSQL;
  end;

implementation



{ JoinSQLData }
//==============================================================================
// Description: Class holding the SQL data for a required join and its related attributes
//              JOIN_KEY and related SQL is set in the constructor
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
constructor TJoinSQLData.Create(istJoinKey, istJoinSQL: string);
begin
  FJoinKey := istJoinKey;
  FJoinSQL := istJoinSQL;
  FAttributes := TObjectList.Create;
end;

destructor TJoinSQLData.Destroy;
begin
  FAttributes.Free;
  inherited;
end;



{ AttributeSQLData }
//==============================================================================
// Description: Class holding the SQL data for an attribute. ATTRIBUTE_KEY, related SQL,
//              attribute name and SQL for WHERE clause are set in the constructor
//
// Author: Ben Collier
// Created: 25/11/2002
//------------------------------------------------------------------------------
constructor TAttributeSQLData.Create(istAttributeKey: string; istAttributeSQL: string;
  istItemName: string; istWhereSQL: string);
begin
 FAttributeKey := istAttributeKey;
 FAttributeSQL := istAttributeSQL;
 FItemName := istItemName;
 FWhereSQL := istWhereSQL;
end;

destructor TAttributeSQLData.Destroy;
begin
  inherited;
end;

end.
