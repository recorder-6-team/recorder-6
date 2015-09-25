{ Class from which exportable mdi children are derived.  Ensures the export menu
     option is enabled/disabled at the correct time. }
unit BaseExportableUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseChildUnit, DataClasses, Menus, Constants;

type
  TBaseExportable = class(TBaseChild)
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ActivateActions(iState: boolean = True);
  end;

var
  BaseExportable: TBaseExportable;

//==============================================================================
implementation

{$R *.DFM}

uses
  MainTBar, FormActions;

//==============================================================================
{ The following three event handlers just ensure that the export action
    visibility is set at the correct time }
procedure TBaseExportable.FormActivate(Sender: TObject);
begin
  inherited;
  ActivateActions;
end;

//==============================================================================
procedure TBaseExportable.FormDeactivate(Sender: TObject);
begin
  inherited;
  ActivateActions( False );
end;

//==============================================================================
procedure TBaseExportable.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  ActivateActions( False );
end;

//==============================================================================
{ Set activation and visibility of actions that base exportable derivatives
     all share.  Default parameter means activation will occur }
procedure TBaseExportable.ActivateActions(iState: boolean = True);
begin
  dmFormActions.SetActionVisibility(dmFormActions.actExport, iState);
  dmFormActions.actPlacesForOccurrencesReport.Enabled:= iState;
  dmFormActions.actOccurrencesForPlacesReport.Enabled:= iState;

  dmFormActions.SetValidateSelectedAction(iState);
end;

end.
