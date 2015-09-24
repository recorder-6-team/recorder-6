//==============================================================================
//  Unit:        BoundaryImport
//
//  Implements:  TdlgBoundaryImportDialog
//
//  Description: Allows the user to select a method of matching imported polygons
//               to locations in the database.
//
//  Author:      Paul Davies
//  Created:     20 February 2009
//
//  Last Revision Details:
//    $Revision: 10 $
//    $Date: 29/10/09 16:38 $
//    $Author: Bonnerearle $
//
//==============================================================================

unit BoundaryImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, MapServerLink, Constants;

resourcestring
  ResStr_LocationFieldOutOfRange = 'Location Field Index is out of range.';

type
  TdlgBoundaryImportDialog = class(TForm)
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    pnlOptions: TPanel;
    lblOption: TLabel;
    rbNo: TRadioButton;
    rbYes: TRadioButton;
    lblBoundaryAttribute: TLabel;
    lblLocationField: TLabel;
    cmbBoundaryAttribute: TComboBox;
    cmbLocationField: TComboBox;
    procedure cmbBoundaryAttributeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioClick(Sender: TObject);
  private
    FMapHandle: HWnd;
    FSourceSheetID: integer;
    FAttributes: TStrings;
    FMatchField: string;
    FPrimaryAttribute: string;
    procedure SetControlStates;
    procedure PopulateAttributeList;
    function GetMatchExisting(): Boolean;
  public
    constructor Create(owner: TComponent; handle: HWnd; sourceSheet: Integer); reintroduce; overload;
    destructor Destroy; override;
    property Attributes: TStrings read FAttributes;
    property MatchField: string read FMatchField;
    property PrimaryAttribute: string read FPrimaryAttribute;
    property MatchExisting: Boolean read GetMatchExisting;
  end;

const
  OPTION_YES = 1;
  MANUAL_MATCH = 0;
  LOCATION_KEY = 0;
  LOCATION_NAME = 1;
  FILED_CODE = 2;

implementation

uses
  ms5, BoundaryLocationMatch;


{$R *.dfm}

{-------------------------------------------------------------------------------
  Sets up the initial state of the form.
}
constructor TdlgBoundaryImportDialog.Create(owner: TComponent; handle: HWnd;
    sourceSheet: Integer);
begin
  inherited Create(owner);
  FMapHandle         := handle;
  FSourceSheetID     := sourceSheet;

  PopulateAttributeList;
  SetControlStates;

end;

{-------------------------------------------------------------------------------
}
destructor TdlgBoundaryImportDialog.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Populates cmbBoundaryAttribute with the available attributes in the file that
  is being imported.
}
procedure TdlgBoundaryImportDialog.PopulateAttributeList;
var
  fieldName: Array[0..10] of char;
  i, attributeCount: integer;
begin
  MapCheck(MapDataGetNumAttributeFields(FMapHandle, FSourceSheetID, attributeCount));
  for i := 0 to attributeCount - 1 do begin
    MapCheck(MapDataGetAttributeFieldName(FMapHandle, FSourceSheetID, i, fieldName, 10));
    cmbBoundaryAttribute.Items.Add(StrPas(fieldName));
  end;
end;

{-------------------------------------------------------------------------------
  Sets up the enabled states of the controls based on the values of the other controls.
}
procedure TdlgBoundaryImportDialog.SetControlStates;
begin
  cmbBoundaryAttribute.Enabled :=  rbYes.Checked;
  cmbLocationField.Enabled := rbYes.Checked and (cmbBoundaryAttribute.ItemIndex <> MANUAL_MATCH);
  if cmbLocationField.Enabled and (cmbLocationField.ItemIndex = -1) then
    cmbLocationField.ItemIndex := 0;
end;

{-------------------------------------------------------------------------------
  Set the enabled states of the other controls when the option is changed.
}
procedure TdlgBoundaryImportDialog.RadioClick(Sender: TObject);
begin
  SetControlStates;
end;
   
{-------------------------------------------------------------------------------
  Set the enabled states of the other controls when the boundary attribute is changed.
}
procedure TdlgBoundaryImportDialog.cmbBoundaryAttributeChange(Sender: TObject);
begin
  SetControlStates;
end;

{-------------------------------------------------------------------------------
  When the form is closed
}
procedure TdlgBoundaryImportDialog.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i : Integer;
begin
  if (ModalResult = mrOK) and rbYes.Checked then begin
    FAttributes := TStringList.Create;
    // Adds all the items from the combo box except the first one (<Manual Match>)
    for i := 1 to self.cmbBoundaryAttribute.Items.Count - 1 do
      FAttributes.Add(self.cmbBoundaryAttribute.Items[i]);

    FMatchField := '';
    if cmbBoundaryAttribute.ItemIndex <> MANUAL_MATCH then begin
      FPrimaryAttribute := self.cmbBoundaryAttribute.Items[self.cmbBoundaryAttribute.ItemIndex];
      case cmbLocationField.ItemIndex of
        0: FMatchField := 'L.Location_Key';//Location Key
        1: FMatchField := 'LN.Item_Name';  //Location Name
        2: FMatchField := 'L.File_Code';   //File Code
        else raise Exception.Create(ResStr_LocationFieldOutOfRange);
      end;
    end else
      FPrimaryAttribute := '';
  end;
end;

function TdlgBoundaryImportDialog.GetMatchExisting: Boolean;
begin
  Result := rbYes.Checked;
end;

end.
