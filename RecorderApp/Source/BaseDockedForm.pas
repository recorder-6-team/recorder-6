{===============================================================================
  Unit:        BaseDockedForm

  Defines:     TfrmBaseDockedForm

  Description:

  Model:       <none>

  Created:

  Last revision information:
    $Revision: 11 $
    $Date: 20/12/05 12:02 $
    $Author: Johnvanbreda $

===============================================================================}
unit BaseDockedForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, Constants;

type
  TfrmBaseDockedForm = class(TBaseForm)
  protected
    FEditMode: TEditMode;
    FCustodian: string;
    function GetHaveCustody: Boolean; virtual;
    property HaveCustody: boolean read GetHaveCustody;
  public
    procedure FindOnMap; virtual; abstract;
    procedure ShowMetadata; virtual; abstract;
    procedure UpdateRTFMenu; virtual; abstract;
    property Custodian: string read FCustodian;
    property EditMode: TEditMode read FEditMode;
  end;

//==============================================================================
implementation

{$R *.DFM}

{ TfrmBaseDockedForm }

uses
  ApplicationSettings;

{-------------------------------------------------------------------------------
  Default accessor
}
function TfrmBaseDockedForm.GetHaveCustody: Boolean;
begin
  Result := (FCustodian = AppSettings.SiteID);
end;

end.
