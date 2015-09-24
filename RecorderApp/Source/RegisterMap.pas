//==============================================================================
//  Unit:        RegisterMap
//
//  Implements:  TdlgRegisterMap
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 40 $
//    $Date: 4/01/08 14:28 $
//    $Author: Rickyshrestha $
//
//  $History: RegisterMap.pas $
//  
//  *****************  Version 40  *****************
//  User: Rickyshrestha Date: 4/01/08    Time: 14:28
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 39  *****************
//  User: Rickyshrestha Date: 18/12/07   Time: 13:50
//  Updated in $/JNCC/Development/Build/Source
//  Changed soem hardcoded strings to resroucestring
//   ResStr_ValidScale
//    ResStr_ValidSpatialRef
//    ResStr_SouthWestSR
//    ResStr_SameEstNorth
//    ResStr_EnterSheetName
//    ResStr_EnterFileName
//    ResStr_CannotLocateFileName
//    ResStr_EnterSWSR
//    ResStr_EnterNESR
//    ResStr_SameSRSystem
//    ResStr_EnterCutInScale
//    ResStr_EnterCutOutScale
//    ResStr_CutInGreaterThanOut
//  
//  *****************  Version 38  *****************
//  User: Ericsalmon   Date: 22/03/07   Time: 17:20
//  Updated in $/JNCC/Development/Build/Source
//  VI 13272. Internationalisation bug fix with ThousandSeparator.
//  
//  *****************  Version 37  *****************
//  User: Ericsalmon   Date: 22/03/07   Time: 16:51
//  Updated in $/JNCC/Development/Build/Source
//  VI 13271. Internationalisation bug fix. Thousand separator character
//  used in cut in/out dropdown boxes.
//  
//  *****************  Version 36  *****************
//  User: Ericsalmon   Date: 17/02/04   Time: 16:26
//  Updated in $/JNCC/Development/Build/Source
//  Replaced unnecessary TBitBtn with TButton.
//  
//  *****************  Version 35  *****************
//  User: Ericsalmon   Date: 16/01/04   Time: 12:07
//  Updated in $/JNCC/Development/Build/Source
//  Multiple maps development.
//
//  *****************  Version 34  *****************
//  User: Andrewkemp   Date: 20/01/03   Time: 15:52
//  Updated in $/JNCC/Source
//  IR321: PickupDetails calls SetEditMode to ensure that the South-West
//  and  North-East fields are filled in appropriately.
//
//  *****************  Version 33  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 11:51
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit RegisterMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SpatialRefFuncs, ExtCtrls, Buttons, ExceptionForm, MapData,
  OnlineHelp, ImageListButton;

type
  ERegisterMapError = class (TExceptionPath)
  end;
  
  TdlgRegisterMap = class (TForm)
    bbCancel: TImageListButton;
    bbOK: TImageListButton;
    bvlFram: TBevel;
    cmbCutIn: TComboBox;
    cmbCutOut: TComboBox;
    dlgOpenFile: TOpenDialog;
    eFileName: TEdit;
    eNorthEast: TEdit;
    eSheetName: TEdit;
    eSouthWest: TEdit;
    lblCutIn: TLabel;
    lblCutOut: TLabel;
    lblFileName: TLabel;
    lblInstructions: TLabel;
    lblLayerDisp: TLabel;
    lblLayerPrompt: TLabel;
    lblName: TLabel;
    lblNorthEast: TLabel;
    lblSouthWest: TLabel;
    bbFileName: TButton;
    procedure bbFileNameClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure dlgOpenFileClose(Sender: TObject);
    procedure eFileNameChange(Sender: TObject);
    procedure eFileNameExit(Sender: TObject);
    procedure eNorthEastExit(Sender: TObject);
    procedure eSouthWestExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FValidEnteredNE: String;
    FValidEnteredSW: String;
    hlpRegisterMap: TOnlineHelp;
    function GetVectorSheet: Boolean;
    procedure SetEditMode;
    procedure ValidateValue(const AValue: Boolean; const AMsg: String; const AControl: 
        TWinControl);
  public
    procedure PickupDetails;
    property ValidEnteredNE: String read FValidEnteredNE;
    property ValidEnteredSW: String read FValidEnteredSW;
    property VectorSheet: Boolean read GetVectorSheet;
  end;

resourcestring
  ResStr_NoneRequired = 'None Required';

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions;

resourcestring
  ResStr_ValidScale = 'Cut-in and Cut-out scales must be in the format of 1:100, 1:50%s000 etc...';
  ResStr_ValidSpatialRef = 'The Spatial Reference is not recognised by the current Spatial Reference System';
  ResStr_SouthWestSR =  'The South-West spatial reference must lie to the south-west of the North-East spatial reference';
  ResStr_SameEstNorth = 'The two spatial references lie on the same easting or northing.  Please change one of them.';
  ResStr_EnterSheetName = 'Please enter a Sheet Name';
  ResStr_EnterFileName =  'Please enter a File Name';

  ResStr_CannotLocateFileName = 'File name "%s" cannot be located.'#13#10 +
                                'Please enter a recognised filename';

  ResStr_EnterSWSR =  'Please enter a South-West spatial reference';
  ResStr_EnterNESR =  'Please enter a North-East spatial reference';
  ResStr_SameSRSystem = 'Both spatial references must be entered in the same spatial reference system';
  ResStr_EnterCutInScale =  'Please enter a Cut In scale';
  ResStr_EnterCutOutScale = 'Please enter a Cut Out scale';
  ResStr_CutInGreaterThanOut =  'Cut in scale must be greater than the cut out scale';


{-==============================================================================
    TdlgRegisterMap
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TdlgRegisterMap.bbFileNameClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then begin
    eFileName.Text := dlgOpenFile.FileName;
    SetEditMode;
  end;
end;  // TdlgRegisterMap.bbFileNameClick 

{-------------------------------------------------------------------------------
  Validation Code 
}
procedure TdlgRegisterMap.bbOKClick(Sender: TObject);
var
  lConvertCutIn: Double;
  lConvertCutOut: Double;
  lValidBB: TValidBoundingBox;
begin
  { Check that all the fields are filled if necessary... }
  try
    ValidateValue(eSheetName.Text <> '',  ResStr_EnterSheetName, eSheetName);
    ValidateValue(eFileName.Text <> '', ResStr_EnterFileName, eFileName);
    ValidateValue(FileExists(eFileName.text),
                  Format(ResStr_CannotLocateFileName, [eFileName.text]), eFileName);
  
    if not VectorSheet then begin
      ValidateValue((FValidEnteredSW <> '') and not VectorSheet,ResStr_EnterSWSR, eSouthWest);

      ValidateValue((FValidEnteredNE <> '') and not VectorSheet,
                    ResStr_EnterNESR, eNorthEast);
  
      ValidateValue(CompareText(DetermineSpatialRefSystem(FValidEnteredNE),
                                DetermineSpatialRefSystem(FValidEnteredSW)) = 0,
                    ResStr_SameSRSystem,
                    eSouthWest);
  
      lValidBB := CheckBoundingBox(FValidEnteredSW, FValidEnteredNE,
                                   DetermineSpatialRefSystem(FValidEnteredSW));
  
      ValidateValue(DifferentEastingAndNorthing(FValidEnteredSW, FValidEnteredNE),
                    ResStr_SameEstNorth, eSouthWest);
  
      ValidateValue(lValidBB.Valid, lValidBB.Error, eSouthWest);
    end;
  
    ValidateValue(cmbCutIn.Text <> '', ResStr_EnterCutInScale, cmbCutIn);
    ValidateValue(cmbCutOut.Text <> '', ResStr_EnterCutOutScale, cmbCutOut);
    lConvertCutIn  := RemoveCommas(cmbCutIn.Text);
    lConvertCutOut := RemoveCommas(cmbCutOut.Text);
    ValidateValue((lConvertCutIn - lConvertCutOut) > 0,
                  ResStr_CutInGreaterThanOut,
                  cmbCutIn);
  except
    on ERegisterMapError do begin
      ModalResult := mrNone;
      raise
    end;
    on EConvertError do begin
      ModalResult := mrNone;
      raise ERegisterMapError.CreateValidation(
          Format(ResStr_ValidScale, [ThousandSeparator]),
          cmbCutIn);
    end;
  end;
end;  // TdlgRegisterMap.bbOKClick 

{-------------------------------------------------------------------------------
  Depending on the value of VectorSheet, call to blank out, or enable the spatial reference 
      edit boxes 
}
procedure TdlgRegisterMap.dlgOpenFileClose(Sender: TObject);
begin
  SetEditMode;
end;  // TdlgRegisterMap.dlgOpenFileClose 

{-------------------------------------------------------------------------------
  Depending on the value of VectorSheet, call to blank out, or enable the spatial reference 
      edit boxes.
  Required in the OnChange for when we are edditting an existing Map Sheet 
}
procedure TdlgRegisterMap.eFileNameChange(Sender: TObject);
begin
  SetEditMode;
end;  // TdlgRegisterMap.eFileNameChange 

{-------------------------------------------------------------------------------
  Depending on the value of VectorSheet, call to blank out, or enable the spatial reference 
      edit boxes 
}
procedure TdlgRegisterMap.eFileNameExit(Sender: TObject);
begin
  SetEditMode;
end;  // TdlgRegisterMap.eFileNameExit 

{-------------------------------------------------------------------------------
  Validate on exit, saving the entered spatial reference 
}
procedure TdlgRegisterMap.eNorthEastExit(Sender: TObject);
begin
  ValidateSpatialRefEntry(eNorthEast, AppSettings.SpatialRefSystem, FValidEnteredNE);
end;  // TdlgRegisterMap.eNorthEastExit 

{-------------------------------------------------------------------------------
  Validate on exit, saving the entered spatial reference 
}
procedure TdlgRegisterMap.eSouthWestExit(Sender: TObject);
begin
  ValidateSpatialRefEntry(eSouthWest, AppSettings.SpatialRefSystem, FValidEnteredSW);
end;  // TdlgRegisterMap.eSouthWestExit 

{-------------------------------------------------------------------------------
}
procedure TdlgRegisterMap.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  dlgOpenFile.InitialDir := AppSettings.MapFilePath;
  SetEditMode;

  // Need to ensure the correct ThousansSeparator is used according to current locale.
  for i := 0 to cmbCutIn.Items.Count - 1 do
    cmbCutIn.Items[i] := StringReplace(cmbCutIn.Items[i], ',', ThousandSeparator, [rfReplaceAll]);
  for i := 0 to cmbCutOut.Items.Count - 1 do
    cmbCutOut.Items[i] := StringReplace(cmbCutOut.Items[i], ',', ThousandSeparator, [rfReplaceAll]);

  //Help Setup
  hlpRegisterMap := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpRegisterMap.OnHelpReplacement;
  HelpContext := IDH_REGISTERMAP;
end;  // TdlgRegisterMap.FormCreate

{-------------------------------------------------------------------------------
}
procedure TdlgRegisterMap.FormDestroy(Sender: TObject);
begin
  hlpRegisterMap.Free;
end;  // TdlgRegisterMap.FormDestroy 

{-------------------------------------------------------------------------------
  Reads eFileName to determine whether we are looking at a vector sheet 
}
function TdlgRegisterMap.GetVectorSheet: Boolean;
var
  lExtension: String;
begin
  lExtension := UpperCase(ExtractFileExt(eFileName.Text));
  Result := (lExtension = '.GSF') or (lExtension = '.DXF') or (lExtension = '.BNA') or
            (lExtension = '.VPA') or (lExtension = '.NTF') or (lExtension = '.MIF') or
            (lExtension = '.SHP') or (lExtension = '.OPT') or (lExtension = '.STD');
end;  // TdlgRegisterMap.GetVectorSheet 

{-------------------------------------------------------------------------------
  When details are added to the dialog externally, pickup the details from the northeast 
      southwest corner edit boxes.  Otherwise when you click OK it thinks nothing has been 
      entered 
}
procedure TdlgRegisterMap.PickupDetails;
begin
  eSouthWestExit(nil);
  eNorthEastExit(nil);
  SetEditMode;
end;  // TdlgRegisterMap.PickupDetails 

{-------------------------------------------------------------------------------
  Disable the spatial reference edit boxes if the filename of the sheet beeing added is a 
      vector sheet 
}
procedure TdlgRegisterMap.SetEditMode;
begin
  if VectorSheet then begin
    eSouthWest.Enabled  := False;
    eSouthWest.Color    := clBtnFace;
    eSouthWest.Text     := ResStr_NoneRequired;
    eNorthEast.Enabled  := False;
    eNorthEast.Color    := clBtnFace;
    eNorthEast.Text     := ResStr_NoneRequired;
  end else begin
    eSouthWest.Enabled := True;
    eSouthWest.Color   := clWindow;
    if CompareText(eSouthWest.Text, ResStr_NoneRequired) = 0 then eSouthWest.Text := '';
    eNorthEast.Enabled := True;
    eNorthEast.Color   := clWindow;
    if CompareText(eNorthEast.Text, ResStr_NoneRequired) = 0 then eNorthEast.Text := '';
  end;
end;  // TdlgRegisterMap.SetEditMode 

{-------------------------------------------------------------------------------
}
procedure TdlgRegisterMap.ValidateValue(const AValue: Boolean; const AMsg: String; const 
    AControl: TWinControl);
begin
  if not AValue then
  begin
    AControl.SetFocus;
    raise ERegisterMapError.CreateValidation(AMsg, AControl);
  end;
end;  // TdlgRegisterMap.ValidateValue 

end.

