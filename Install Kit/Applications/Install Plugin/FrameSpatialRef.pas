{-------------------------------------------------------------------------------
  Unit:        FrameSpatialRef.pas

  Defines:     TfraSpatialRef

  Description: Allows user to choose a Spatial Reference System for Recorder.

  Created:     February 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameSpatialRef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, StdCtrls, CheckLst, Settings, Constants, TextMessages;

type
  EInstallSettingsError = class(Exception);

  TfraSpatialRef = class(TPageFrame)
    Label3: TLabel;
    lblSpatialRef: TLabel;
    cblbSpatialRefs: TCheckListBox;
    Label4: TLabel;
    procedure cblbSpatialRefsClickCheck(Sender: TObject);
  private
    FSpatialSystems: TStringList;
    procedure SetNextButtonState;
    procedure ShowSpatialRefList;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
  public
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameWelcome, FrameSiteInfo, FrameCutOffYear;

//==============================================================================
{ TfraSpatialRef }
//------------------------------------------------------------------------------
destructor TfraSpatialRef.Destroy;
begin
  if Assigned(FSpatialSystems) then
    FSpatialSystems.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Load the Spatial References available into the list box, by looking at the
  contents of Spatial Systems.txt (must be in application directory)
}
procedure TfraSpatialRef.ShowSpatialRefList;
var i: integer;
begin
  if not FileExists(ExtractFilePath(Application.Exename) + STR_SPATIAL_SYSTEMS) then
    raise EInstallSettingsError.Create(EST_SPATIAL_REF_SYSTEMS);

  FSpatialSystems := TStringList.Create;
  FSpatialSystems.LoadFromFile(ExtractFilePath(Application.Exename) + STR_SPATIAL_SYSTEMS);
  // Remove all the items not of the form "Name=Value" before adding them to the
  // listbox
  i := 0;
  while i <> FSpatialSystems.Count do
    if Pos('=', FSpatialSystems[0]) = 0 then
      FSpatialSystems.Delete(i)
    else
      Inc(i);

  cblbSpatialRefs.Items.Clear;
  for i := 0 to FSpatialSystems.Count-1 do
    cblbSpatialRefs.Items.Add(FSpatialSystems.Names[i]);
end;

//------------------------------------------------------------------------------
function TfraSpatialRef.GetNextFrame: TPageFrameClass;
begin
  Result := TfraCutOffYear;
end;

//------------------------------------------------------------------------------
function TfraSpatialRef.GetPrevFrame: TPageFrameClass;
begin
  if Settings.InstallMode = imWorkstation then
    Result := TfraWelcome
  else
    Result := TfraSiteInfo;
end;

//------------------------------------------------------------------------------
procedure TfraSpatialRef.SetNextButton(const Value: TButton);
begin
  inherited;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSpatialRef.SetSettings(const Value: TSettings);
var i     : Integer;
    lFound: Boolean;
begin
  inherited;
  ShowSpatialRefList;
  lFound := false;
  for i := 0 to FSpatialSystems.Count - 1 do
    if FSpatialSystems.ValueFromIndex[i] = Settings.SpatialRefSystem then begin
      cblbSpatialRefs.Checked[i] := true;
      lFound := true;
    end;
  // Nothing previously selected, default to first in list, and update Settings too!! 
  if not lFound then begin
    cblbSpatialRefs.Checked[0] := true;
    Settings.SpatialRefSystem := FSpatialSystems.Values[FSpatialSystems.Names[0]];
  end;
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
procedure TfraSpatialRef.SetNextButtonState;
var i: Integer;
begin
  if Assigned(NextButton) then begin
    with cblbSpatialRefs do
      for i := 0 to Items.Count-1 do
        if Checked[i] then begin
          NextButton.Enabled := true;
          Exit;
        end;
    // If no Spatial Ref checked, the following line is reached.
    NextButton.Enabled := false;
  end;
end;

//------------------------------------------------------------------------------
procedure TfraSpatialRef.cblbSpatialRefsClickCheck(Sender: TObject);
var i, lItemIndex: integer;
begin
  inherited;
  {First remove the old selections apart from the current one }
  with cblbSpatialRefs do begin
    lItemIndex := ItemIndex;
    for i := 0 to Items.Count - 1 do Checked[i]:=False;
    Checked[ItemIndex] := true;
  end;
  Settings.SpatialRefSystem := FSpatialSystems.Values[FSpatialSystems.Names[lItemIndex]];
  SetNextButtonState;
end;

//------------------------------------------------------------------------------
end.
