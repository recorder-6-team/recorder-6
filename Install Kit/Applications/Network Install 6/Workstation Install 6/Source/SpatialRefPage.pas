{===============================================================================
  Unit:        SpatialRefPage

  Defines:     TfraSpatialRef

  Description:

  Model:       Workstation Install 6.mpb

  Created:     November 2004

  Last revision information:
    $Revision: 5 $
    $Date: 14/07/09 15:43 $
    $Author: Ericsalmon $

===============================================================================}

unit SpatialRefPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, CheckLst, ExtCtrls;

type
  TfraSpatialRef = class(TBasePage)
    cblbSpatialRefs: TCheckListBox;
    lblInfo: TLabel;
    lblSpatialRef: TLabel;
    procedure cblbSpatialRefsClickCheck(Sender: TObject);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetNextCaption: String; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  InstallFolderPage, InstallationPage, SetupConstants, TextMessages;

{-==============================================================================
    TfraSpatialRef
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraSpatialRef.cblbSpatialRefsClickCheck(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  // Remove the all selections apart from the current one;
  with cblbSpatialRefs do begin
    for i := 0 to Items.Count - 1 do Checked[i] := False;
    Checked[ItemIndex] := true;
  end;
  ChangedContent;
end;  // TfraSpatialRef.cblbSpatialRefsClickCheck 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetConfirmCancel: Boolean;
begin
  Result := Settings.FoldersCreated.Count <> 0;
end;  // TfraSpatialRef.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetHasNext: Boolean;
var
  i: Integer;
begin
  with cblbSpatialRefs do
    for i := 0 to Items.Count-1 do
      if Checked[i] then begin
        Result := True;
        Exit;
      end;
  // If no Spatial Ref checked, the following line is reached.
  Result := False;
end;  // TfraSpatialRef.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraSpatialRef.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetNext: TBasePageClass;
begin
  Result := TfraInstallation;
end;  // TfraSpatialRef.GetNext 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetNextCaption: String;
begin
  Result := ResStr_InstallCaption;
end;  // TfraSpatialRef.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetPrevious: TBasePageClass;
begin
  Result := TfraInstallFolder;
end;  // TfraSpatialRef.GetPrevious 

{-------------------------------------------------------------------------------
}
function TfraSpatialRef.GetResourceImage: String;
begin
  Result := ResImg_SpatialRef;
end;  // TfraSpatialRef.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraSpatialRef.LoadContent;
var
  i: Integer;
begin
  inherited;
  with Settings.AvailableSpatialRefSystems do begin
    cblbSpatialRefs.Items.Clear;
    for i := 0 to Count - 1 do begin
      cblbSpatialRefs.Items.Add(Names[i]);
      if ValueFromIndex[i] = Settings.SpatialRefSystem then
        cblbSpatialRefs.Checked[i] := True;
    end;
    // Default to first one if no other already chosen.
    if (Settings.SpatialRefSystem = '') and (Count > 0) then
      cblbSpatialRefs.Checked[0] := True;
  end;
  ChangedContent;
end;  // TfraSpatialRef.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraSpatialRef.SaveContent;
var
  i: Integer;
begin
  inherited;
  for i := 0 to cblbSpatialRefs.Items.Count - 1 do
    if cblbSpatialRefs.Checked[i] then
      Settings.SpatialRefSystem := Settings.AvailableSpatialRefSystems.ValueFromIndex[i];
end;  // TfraSpatialRef.SaveContent 

end.
