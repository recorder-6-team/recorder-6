{===============================================================================
  Unit:        AddinReg7

  Description: Registration unit for components used in Recorder addins.

  Last revision information:
    $Revision: 2 $
    $Date: 7/02/08 11:54 $
    $Author: Johnvanbreda $

===============================================================================}

unit AddinReg7;

interface

uses
  Classes, AddinIDComboBox, AddinLinkedControls, FixedWidthColumnSelector;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('JNCC', [TAddinIDComboBox,
                              TAddinLinkedEdit,
                              TFixedWidthColumnSelector,
                              TNameLinkedEdit]);
end;  { Register }

//==============================================================================
end.

