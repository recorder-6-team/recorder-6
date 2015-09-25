{===============================================================================
  Unit:        IWSearchManager

  Defines:     TIWSearchManager

  Description: SearchManager for Import Wizard

  Model:       <none>

  Last revision information:
    $Revision: 5 $
    $Date: 27/12/07 15:51 $
    $Author: Rickyshrestha $

===============================================================================}

unit IWSearchManager;

interface

uses
  SysUtils, AddinSearchManager;

const
  ST_REFERENCE = 100;

type
  TIWSearchManager = class(TAddinSearchManager)
  protected
    function GetStoredProc: String; override;
    function GetFindDialogTitle: String; override;
  end;

resourcestring
  ResStr_References = 'Documents';
  ResStr_NoTitleForSearch = 'No title for search type: %s.';
  ResStr_NoSPForSearch =  'No stored procedure for search type: %s.';

//==============================================================================
implementation

{-------------------------------------------------------------------------------
}
function TIWSearchManager.GetFindDialogTitle: String;
begin
  Result := Format(ResStr_NoTitleForSearch, [IntToStr(SearchType)]);
  case SearchType of
    ST_REFERENCE: Result := ResStr_References;
  else
    Result := inherited GetFindDialogTitle
  end;
end;  // TIWSearchManager.GetFindDialogTitle

{-------------------------------------------------------------------------------
}
function TIWSearchManager.GetStoredProc: String;
begin
  Result := Format(ResStr_NoSPForSearch, [IntToStr(SearchType)]);
  case SearchType of
    ST_REFERENCE: Result := 'usp_References_Select_ForSearch';
  else
    Result := inherited GetStoredProc
  end;
end;  // TIWSearchManager.GetStoredProc

end.
