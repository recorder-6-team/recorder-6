//==============================================================================
//  Unit:        MapLocationOptions
//
//  Implements:  TdlgMapLocationOptions
//
//  Description:
//
//  Author:      Paul Thomas
//  Created:     18 August 1999
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 14/01/03 16:34 $
//    $Author: Ericsalmon $
//
//  $History: MapLocationOptions.pas $
//  
//  *****************  Version 12  *****************
//  User: Ericsalmon   Date: 14/01/03   Time: 16:34
//  Updated in $/JNCC/Source
//  Closed query after use.
//
//  *****************  Version 11  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 11:27
//  Updated in $/JNCC/Source
//  Replaced BitBtns wirh ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MapLocationOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Constants, ImageListButton;

type
  TdlgMapLocationOptions = class(TForm)
    lblNoAssociation: TLabel;
    GroupBox1: TGroupBox;
    rbNewLocation: TRadioButton;
    rbExistingLocation: TRadioButton;
    rbAdminArea: TRadioButton;
    cmbAdminAreaType: TComboBox;
    lblAdminArea: TLabel;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure rbAdminAreaClick(Sender: TObject);
  private
    FAdminTypeKeys : TStringList;
    function GetRadioIndex: integer;
    function GetAdminTypeKey: string;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property RadioIndex : integer read GetRadioIndex;
    property AdminTypeKey : string read GetAdminTypeKey;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, FormActions, GeneralData;

{$R *.DFM}

//==============================================================================
constructor TdlgMapLocationOptions.Create(AOwner: TComponent);
begin
  inherited;
  { Populate the admin type combo box }
  FAdminTypeKeys := TStringList.Create; // a list of keys for each combo item index
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'SELECT ADMIN_TYPE_KEY, LONG_NAME FROM ADMIN_TYPE ORDER BY LONG_NAME';
    Open;
    while not EOF do begin
      FAdminTypeKeys.Add(FieldByName('ADMIN_TYPE_KEY').AsString);
      cmbAdminAreaType.Items.Add(FieldByName('LONG_NAME').AsString);
      Next;
    end;
    Close;
  end;
end;

//==============================================================================
destructor TdlgMapLocationOptions.Destroy;
begin
  FAdminTypeKeys.Free;
  inherited;
end;

//==============================================================================
{ Reads the index of the radio buttons as if they were a group }
function TdlgMapLocationOptions.GetRadioIndex: integer;
begin
  if rbNewLocation.Checked then
    Result := 0
  else if rbExistingLocation.Checked then
    Result := 1
  else if rbAdminArea.Checked then
    Result := 2
  else
    Result := -1;
end;

//==============================================================================
{ Enable stuff when radio buttons are selected }
procedure TdlgMapLocationOptions.rbAdminAreaClick(Sender: TObject);
begin
  lblAdminArea.Enabled := rbAdminArea.Checked;
  cmbAdminAreaType.enabled := rbAdminArea.Checked;
  if cmbAdminAreaType.ItemIndex = -1 then
    cmbAdminAreaType.ItemIndex := 0; // ensure 1 selected
  bbOk.Enabled := True;
end;

//==============================================================================
{ Return the key for the admin type selected }
function TdlgMapLocationOptions.GetAdminTypeKey: string;
begin
  if cmbAdminAreaType.ItemIndex <>-1 then
    Result := FAdminTypeKeys[cmbAdminAreaType.ItemIndex]
  else
    Result := '';
end;

//==============================================================================
end.
