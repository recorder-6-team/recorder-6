//==============================================================================
//  Unit:        BoundaryFieldSelect
//
//  Implements:  TdlgBoundaryFieldSelect
//
//  Description: Allows the user to select the attribute field to use as a primary key.
//
//  Author:      Paul Davies
//  Created:     25 February 2009
//
//  Last Revision Details:
//    $Revision: 2 $
//    $Date: 9/03/09 17:50 $
//    $Author: Ericsalmon $
//
//==============================================================================

unit BoundaryFieldSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton;

resourcestring
  ResStr_NoneAvailable = '<none available>';

type
  TdlgBoundaryFieldSelect = class(TForm)
    lblBoundaryAttribute: TLabel;
    cmbBoundaryAttribute: TComboBox;
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    procedure FormShow(Sender: TObject);
  private
    FFileName: string;
  public
    property FileName: string write FFileName;
  end;

implementation

uses
  MapServerLink, ms5, ApplicationSettings, MapClasses;

{$R *.dfm}

{-------------------------------------------------------------------------------
  When the form is shown, populate the combo box with relevant data.
}
procedure TdlgBoundaryFieldSelect.FormShow(Sender: TObject);
var
  i, attributeCount, sheetID: integer;
  fieldName: Array[0..10] of char;
  mapServerLink: TMapServerLink;
  mapHandle: HWND;
begin
  // Populates the combo box with attribute fields.
  mapServerLink := TMapServerLink.Create(nil);
  try
    mapHandle := mapServerLink.MapHandle;
    mapServerLink.ActiveDataset := AppSettings.AvailableMaps.DefaultMap.BaseMapKey + '.gds';

    if CompareText(ExtractFileExt(FFileName), '.gsf') = 0 then
      MapCheck(MapAttachSheet(mapHandle, PChar(FFileName)), FFileName)
    else
      MapCheck(MapImport(mapHandle, PChar(FFileName), PChar(AppSettings.MapFilePath), 0, True), FFileName);

    sheetID := mapServerLink.SheetTotal - 1;

    try
      MapCheck(MapDataGetNumAttributeFields(mapHandle, sheetID, attributeCount));
      for i := 0 to attributeCount - 1 do begin
        MapCheck(MapDataGetAttributeFieldName(mapHandle, sheetID, i, fieldName, 10));
        cmbBoundaryAttribute.Items.Add(StrPas(fieldName));
      end;
    finally
      if sheetID <> -1 then begin
        MapCheck(MapSelectSheet(mapHandle, sheetID, False));
        MapCheck(MapDetachSheet(mapHandle, sheetID));
      end;
    end;
  finally
    mapServerLink.Free;
  end;

  // If no attributes are found, display 'none available' in the combo box.
  if cmbBoundaryAttribute.Items.Count = 0 then
    cmbBoundaryAttribute.Items.Add(ResStr_NoneAvailable);

  cmbBoundaryAttribute.ItemIndex := 0;
end;

end.
