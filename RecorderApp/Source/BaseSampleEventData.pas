//==============================================================================
//  Unit:        BaseSampleEventData
//
//  Implements:  TBaseDataModule
//
//  Description: Defines the base class for the sampleDetails and eventDetails forms
//               used in the application, and implements the static functions available for
//               each of these forms (fieldToDisplayName properties and GetChangedCascadeFields)
//
//  Author:      Qing Sun
//  Created:     13 May 2008
//
//  Last Revision Details:
//    $Revision: 4 $
//    $Date: 14/01/09 17:35 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2008
//
//==============================================================================
unit BaseSampleEventData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseData;

resourcestring
  ResStr_SpatialRef   = 'Spatial Reference';
  ResStr_LocationName = 'Location Name';
  ResStr_Location     = 'Location';
  ResStr_Date         = 'Date';
  ResStr_And          = 'and';

type
  TBaseSampleEventDataModule = class(TBaseDataModule)
  private
    class function FieldToDisplayName(const AFieldName: String): String;
  public
    class function GetChangedCascadeFields(AChanges: TStringList): String;
  end;

{------------------------------------------------------------------------------
}
implementation

{$R *.dfm}

{=============================================================================}
{ TBaseSampleEventDataModule }
{------------------------------------------------------------------------------
}
class function TBaseSampleEventDataModule.FieldToDisplayName(const AFieldName: String): String;
begin
  Result := '';
  if SameText(Copy(AFieldName, 1, 11), 'Spatial_Ref') then
    Result := ResStr_SpatialRef
  else
  if SameText(AFieldName, 'Location_Name') then
    Result := ResStr_LocationName
  else
  if SameText(Copy(AFieldName, 1, 10), 'Vague_Date') then
    Result := ResStr_Date
  else
  if SameText(AFieldName, 'Location_Key') then
    Result := ResStr_Location;
end;

{------------------------------------------------------------------------------
  Formats list of field names to be displayed on dialog.
}
class function TBaseSampleEventDataModule.GetChangedCascadeFields(AChanges: TStringList): String;
var
  lDistinctChangedFields: TStringList;
  lChangedFields: String;
  i: Integer;
begin
  lDistinctChangedFields := TStringList.Create;
  try
    lDistinctChangedFields.Sorted     := True;
    lDistinctChangedFields.Duplicates := dupIgnore;
    // Stringlist can contain name-value pairs instead of just strings.
    for i := 0 to AChanges.Count - 1 do
      if AChanges.Names[i] = '' then
        lDistinctChangedFields.Add(FieldToDisplayName(AChanges.Strings[i]))
      else
        lDistinctChangedFields.Add(FieldToDisplayName(AChanges.Names[i]));

    lChangedFields := lDistinctChangedFields[0];
    for i := 1 to lDistinctChangedFields.Count - 1 do begin
      if (i <> lDistinctChangedFields.Count - 1) then
        lChangedFields := lChangedFields + ', ' + lDistinctChangedFields[i]
      else
        lChangedFields := Format('%s %s %s', [lChangedFields, ResStr_And, lDistinctChangedFields[i]]);
     end;
     Result := lChangedFields;
  finally
    lDistinctChangedFields.Free;
  end;
end;

end.
