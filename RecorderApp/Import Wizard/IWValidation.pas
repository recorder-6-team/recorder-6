{===============================================================================
  Unit:        IWValidation

  Defines:     <nothing>

  Description: Functions to check the validity of data.

  Model:       <none>

  Last revision information:
    $Revision: 3 $
    $Date: 14/08/06 11:26 $
    $Author: Johnvanbreda $

===============================================================================}

unit IWValidation;

interface

uses
  Dialogs, AddinLinkedControls, AddinSearchManager, IWSearchManager, Variants;

function DoCheck(AControl: TAddinLinkedEdit; ASearchType: TSearchType; const
    ASearchKey: String = ''; const AAdditionalCaption: String = ''; AMandatory:
    boolean=false): Boolean; overload;
function DoCheck(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String = ''): Boolean; overload;
function DoCheckUnique(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String = ''): Boolean;

//==============================================================================
implementation

function DoCheck(AControl: TAddinLinkedEdit; ASearchType: TSearchType; const
    ASearchKey: String = ''; const AAdditionalCaption: String = ''; AMandatory:
    boolean=false): Boolean;
var
  lText, lKey: String;
begin
  // Ok if Key already there. We can assume value is correct.
  if AControl.Key <> '' then
    Result := True
  else
  // If no key, and no text, control is clear, so assume it's correct too.
  if (AControl.Text = '') and (not AMandatory) then
    Result := True
  else begin
    lText := AControl.Text;
    Result := DoCheck(lKey, lText, ASearchType, ASearchKey, AAdditionalCaption);
    AControl.Key := lKey;
    if Result then AControl.Text := lText;
  end;
end;

function DoCheck(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String = ''): Boolean;
begin
  with TIWSearchManager.Create do
    try
      SearchType := ASearchType;
      SearchKey  := ASearchKey;
      AdditionalCaption := AAdditionalCaption;
      // Get the key, if there is one coming back...
      AKey := RunSearch(AText);
      // Success?
      Result := AKey <> '';
      // If so, update Text too.
      if Result then AText := ResultText;
    finally
      Free;
    end;
end;

function DoCheckUnique(var AKey, AText: String; ASearchType: TSearchType;
  const ASearchKey: String = ''; const AAdditionalCaption: String = ''): Boolean;
begin
  with TIWSearchManager.Create do
    try
      SearchType := ASearchType;
      SearchKey  := ASearchKey;
      AdditionalCaption := AAdditionalCaption;
      // Get the key, if there is one coming back...
      AKey := FindUnique(AText);
      // Success?
      Result := AKey <> '';
      // If so, update Text too.
      if Result then AText := ResultText;
    finally
      Free;
    end;
end;

end.
