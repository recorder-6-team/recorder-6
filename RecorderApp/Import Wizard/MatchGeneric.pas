{===============================================================================
  Unit:        MatchGeneric

  Defines:     TfraMatchGeneric

  Description: Generic match screen to handle all straightforward data matching.
               Screen behaviour defined by the MatchRule associated with it.

  Model:       ImportWizard

  Last revision information:
    $Revision: 9 $
    $Date: 06/03/13 09:10 $
    $Author: Michaelcaptain $

===============================================================================}

unit MatchGeneric;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWBasePage, ExtCtrls, Grids, StdCtrls, IWSettings, ImageListButton,
  BaseMatchPage, AddinCompositeComponent, AddinLinkedControls, HTMLView, Buttons,
  Menus, DB, ADODB, DBGrids, ImportWizardDBGrid, ComboListID;

type
  {-----------------------------------------------------------------------------
    Frame displayed when there are individuals in the import data that require matching
    agains the Recorder list of individuals.  This provides facilities for automatic and
    manual matching of names, as well as creating new entries in the Recorder list.
    This page is omitted from the wizard sequence if no Observer or Determiner column is
    specified.
  }
  TfraMatchGeneric = class(TBaseMatch)
  protected
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  MissingData, MatchLocations, Import, IWConstants, IWColumnMappingClasses,
  OnlineHelp, FastColumnTypes;


{-==============================================================================
    TfraMatchGeneric
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor
}
constructor TfraMatchGeneric.Create(AOwner: TComponent; ASettings:
    TdmIWSettings);
begin
  inherited;
  HelpContext := IDH_IWMATCHGENERIC;
end;

{-------------------------------------------------------------------------------
}
function TfraMatchGeneric.GetNext: TBasePageClass;
begin
  // MatchRuleIndex has already been incremented.
  with Settings do
    if MatchRuleIndex = ImportFile.MatchRuleCount then
      Result := TfraImport
    else
    if ImportFile.MatchRules[MatchRuleIndex].Key = MR_KEY_LOCATION then
      Result := TfraMatchLocations
    else
      Result := TfraMatchGeneric;
end;  // TfraMatchGeneric.GetNext

{-------------------------------------------------------------------------------
}
function TfraMatchGeneric.GetPrevious: TBasePageClass;
var
  lGotLocation : Boolean;
  lGotDate : Boolean;
  lGotObserver : Boolean;
  lGotDeterminer : Boolean;
  lTermFieldsRequired: Boolean;
begin
  // MatchRuleIndex has already been decremented.
  with Settings do
    if MatchRuleIndex = -1 then begin
      // Deciding whether or not to skip the MissingData screen
      with Settings.ImportFile.ColumnMapping do
      begin
        lGotLocation   := (IsMapped(ColumnTypeByKey(CT_KEY_GRID_REFERENCE))
          or IsMapped(ColumnTypeByKey(CT_KEY_LOCATION_NAME)));
        lGotDate       := IsMapped(ColumnTypeByKey(CT_KEY_DATE));
        lGotObserver   := IsMapped(ColumnTypeByKey(CT_KEY_OBSERVER));
        lGotDeterminer := not lGotObserver
          or (IsMapped(ColumnTypeByKey(CT_KEY_DETERMINER)));
        lTermFieldsRequired := TermFieldsRequiredCount > 0;
      end;
      if (lGotLocation and lGotDate and lGotObserver and lGotDeterminer and
          (not lTermFieldsRequired) and
          not Settings.UseOldImportWizard) then
        Result := TfraFastColumnTypes
      else
        Result := TfraMissingData;
      end
    else
    if ImportFile.MatchRules[MatchRuleIndex].Key = MR_KEY_LOCATION then
      Result := TfraMatchLocations
    else
      Result := TfraMatchGeneric;
end;  // TfraMatchGeneric.GetPrevious 

end.




