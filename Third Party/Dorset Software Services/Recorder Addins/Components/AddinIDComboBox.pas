{===============================================================================
  Unit:        TAddinIDComboBox

  Defines:     TAddinIDComboBox

  Description: Base class for IDComboBoxes. Supports a "No selection" item,
               which is added automatically (if specified)
               just before the items are populated.

  Model:       Components

  Created:     May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/04 15:21 $
    $Author: Andrewkemp $

===============================================================================}

unit AddinIDComboBox;

interface

uses
  Windows, Messages, Classes, StdCtrls, ComboListID, Sysutils;

const
  WM_CHECKITEMINDEX = WM_APP + 1;

type
  TAddinIDComboBox = class (TIDComboBox)
  private
    FHasNoSelectionItem: Boolean;
    FNoSelectionItemText: String;
    procedure CheckHint;
    procedure SetNoSelectionItemText(const Value: String);
    procedure WMCheckItemIndex(var Message: TMessage); message WM_CHECKITEMINDEX;
  protected
    procedure CloseUp; override;
    procedure DoPopulate; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Select; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SetReadOnly(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HasNoSelectionItem: Boolean read FHasNoSelectionItem write 
        FHasNoSelectionItem default False;
    property NoSelectionItemText: String read FNoSelectionItemText write 
        SetNoSelectionItemText;
    property Style default csDropDownList;
  end;
  
//==============================================================================
implementation

{-==============================================================================
    TAddinIDComboBox
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TAddinIDComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropdownList;
  FNoSelectionItemText := '';
  FHasNoSelectionItem := False;
end;  // TAddinIDComboBox.Create 

{-------------------------------------------------------------------------------
  Sets a hint for the combo box if the text is too wide. 
}
procedure TAddinIDComboBox.CheckHint;
begin
  // Set the combo box hint if too wide
  if Canvas.TextWidth(Text)>ClientWidth then begin
    Hint := Text;
    ShowHint := True;
  end
  else
    Hint := '';
end;  // TAddinIDComboBox.CheckHint 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.CloseUp;
begin
  inherited CloseUp;
  if Populated and (Style = csDropDownList) and
     FHasNoSelectionItem and (ItemIndex = 0) then
    ItemIndex := -1;
end;  // TAddinIDComboBox.CloseUp 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.DoPopulate;
begin
  // Populate the list, that will set the IDFormat
  inherited DoPopulate;
  
  // Then insert the "no selection" item at the top, if there is one,
  // using the correct key type
  if FHasNoSelectionItem then
    if IDFormat = idfLongint then
      Insert(0, FNoSelectionItemText, -1)
    else
      Insert(0, FNoSelectionItemText, '');
end;  // TAddinIDComboBox.DoPopulate 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  
  if not ReadOnly and (Style = csDropDownList) and (Key in [VK_BACK, VK_DELETE]) then
    ItemIndex := -1;
end;  // TAddinIDComboBox.KeyDown 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  
  // Get rid of the beep, for Return/Ctrl+Return
  if Key in [#13, #10] then Key := #0;
end;  // TAddinIDComboBox.KeyPress 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.Select;
begin
  inherited Select;
  
  CheckHint;
  // Add a delay to allow user to use mouse wheel to go through list.
  PostMessage(Handle, WM_CHECKITEMINDEX, ItemIndex, 0);
end;  // TAddinIDComboBox.Select 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  CheckHint;
end;  // TAddinIDComboBox.SetItemIndex 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.SetNoSelectionItemText(const Value: String);
begin
  if FNoSelectionItemText <> Value then
    if Value = '' then begin
      FNoSelectionItemText := '';
      FHasNoSelectionItem := False;
    end else begin
      FNoSelectionItemText := Value;
      FHasNoSelectionItem := True;
    end;
end;  // TAddinIDComboBox.SetNoSelectionItemText 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly(Value);
  
  if Populated and (Style = csDropDownList) and
     FHasNoSelectionItem and (ItemIndex = 0) then
    ItemIndex := -1;
end;  // TAddinIDComboBox.SetReadOnly 

{-------------------------------------------------------------------------------
}
procedure TAddinIDComboBox.WMCheckItemIndex(var Message: TMessage);
begin
  if not ReadOnly and Populated and (Style = csDropDownList) and
     not DroppedDown and (Message.WParam = 0) and FHasNoSelectionItem then
    ItemIndex := -1;
end;  // TAddinIDComboBox.WMCheckItemIndex

end.
