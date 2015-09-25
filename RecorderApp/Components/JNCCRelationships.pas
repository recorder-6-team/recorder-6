//==================================================================================================
//  Unit:        JNCCRelationships
//
//  Implements:  TJNCCRelationshipList
//
//  Description: Relationships unit with JNCC specific handling of names.
//
//  Author:      John van Breda
//  Created:     13 Feb 2002
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 19/03/02 17:38 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==================================================================================================

unit JNCCRelationships;

interface

uses
  Sysutils, Classes,
{$IFDEF USE_TITAN}
  Relationships;
{$ELSE}
  Relationships_ADO;
{$ENDIF}

const
  NAME_REL = -2; //special value to indicate a non-physical name relationship

type
  TJNCCRelationshipList = class(TRelationshipList)
  public
    function FindRelationship(const iTable, iField: String ): Integer; override;
    function GetForeignTableCount(const iTable: String): Integer; override;
  end;

//==================================================================================================
implementation

const
  NAME_ADDITIONAL_FIELDS = 100; // THIS VALUE IS MADE UP - NEED TO CORRECT IT!

//==================================================================================================
{ TJNCCRelationshipList }

{ Overriden FindRelationship with a check for non-physical NAME relationships
     specific to the NBN datamodel }
function TJNCCRelationshipList.FindRelationship(const iTable, iField: string): integer;
begin
  { Hard coded fields which are not physical joins }
  if (CompareText(iField,'ENTERED_BY')=0) or (CompareText(iField,'CHANGED_BY')=0)
     or (CompareText(iField,'CHECKED_BY')=0) then
    Result := NAME_REL
  else
    Result := inherited FindRelationship(iTable, iField);
end;

//==================================================================================================
{ Overriden method to find all the foreign tables linked to a master table.
     Includes a compensation for the name table which does not use
     physical joins due to access restrictions }
function TJNCCRelationshipList.GetForeignTableCount(const iTable: String): Integer;
begin
  Result := inherited GetForeignTableCount( iTable );
  if iTable = 'NAME' then
    Inc(Result, NAME_ADDITIONAL_FIELDS);
end;

//==================================================================================================
end.
 