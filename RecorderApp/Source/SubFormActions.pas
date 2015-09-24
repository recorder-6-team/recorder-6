unit SubFormActions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, DataClasses;

type
  TdmSubFormActions = class(TDataModule)
    alSubForms: TActionList;
    actExport: TAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmSubFormActions: TdmSubFormActions;

//==============================================================================
implementation

{$R *.DFM}

uses Maintbar, FormActions, Observations, TaxonOccur, BiotopeOccur, DataExport;

end.
