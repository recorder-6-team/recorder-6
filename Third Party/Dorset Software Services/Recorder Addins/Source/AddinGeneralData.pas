{===============================================================================
  Unit:        AddinGeneralData

  Defines:     TdmAddinGeneral

  Description:

  Model:       <none>

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/04 15:22 $
    $Author: Andrewkemp $

===============================================================================}

unit AddinGeneralData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseADODataModule, DB, ADODB, Menus, Recorder2000_TLB;

type
  TdmAddinGeneral = class(TdmBaseADO)
  private
    procedure MapWindowMenuClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Recorder: IRecorder2000;
    procedure UpdateMapMenu(Sender: TObject; AMenuItem: TMenuItem; ASetDefault: Boolean =
        False; AUserClickEvent: TNotifyEvent = nil);
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ComObj, ProjectSpecificAccess;

{-==============================================================================
    TdmAddinGeneral
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TdmAddinGeneral.Create(AOwner: TComponent);
begin
  inherited;
  OpenConnection(Recorder.ConnectionString);
  Recorder.SetApplicationSecurity(Connection.ConnectionObject);
end;  // TdmAddinGeneral.Create 

{-------------------------------------------------------------------------------
}
procedure TdmAddinGeneral.MapWindowMenuClick(Sender: TObject);
var
  lMap: IAvailableMap;
begin
  lMap := Recorder.CurrentSettings.AvailableMap[TMenuItem(Sender).Tag];
  if Assigned(lMap) then lMap.Display;
end;  // TdmAddinGeneral.MapWindowMenuClick 

{-------------------------------------------------------------------------------
}
function TdmAddinGeneral.Recorder: IRecorder2000;
begin
  Result := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000
end;  // TdmAddinGeneral.Recorder 

{-------------------------------------------------------------------------------
}
procedure TdmAddinGeneral.UpdateMapMenu(Sender: TObject; AMenuItem: TMenuItem; ASetDefault:
    Boolean = False; AUserClickEvent: TNotifyEvent = nil);
var
  i: Integer;
  lMenu: TMenuItem;
  lClickEvent: TNotifyEvent;
begin
  if Assigned(AUserClickEvent) then lClickEvent := AUserClickEvent
                               else lClickEvent := MapWindowMenuClick;
  AMenuItem.Clear;
  // Add as many items as there are initialised maps.
  for i := 0 to Recorder.CurrentSettings.AvailableMapCount - 1 do begin
    lMenu := TMenuItem.Create(AMenuItem);
    // Skip the "Default" marker
    lMenu.Caption    := Recorder.CurrentSettings.AvailableMap[i].Title;
    lMenu.ImageIndex := 9;
    lMenu.Tag        := i;
    lMenu.Default    := ASetDefault and Recorder.CurrentSettings.AvailableMap[i].IsDefault;
    lMenu.OnClick    := lClickEvent;
    AMenuItem.Add(lMenu);
  end;
  AMenuItem.Enabled := AMenuItem.Count > 0;
end;  // TdmAddinGeneral.UpdateMapMenu 

end.
