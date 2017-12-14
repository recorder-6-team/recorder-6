{===============================================================================
  Unit:        DSSLocalizerUtils

  Defines:     TDSSLocUtils

  Description: Library class for additional functions to do with Korzh
               localizer

  Created:     Feb 2007

  Model:       <none>

  Last revision information:
    $Revision: 2 $
    $Date: 12.02.07 11:05 $
    $Author: Johnvanbreda $

===============================================================================}
unit DSSLocalizerUtils;

interface

uses
  Classes, LocOnFly, Sysutils, Windows;

type
  TDSSLocUtils = class(TObject)
  private
    class function GetModulePath: string;
  public
    class procedure Init;
    class procedure InitForComObject;
  end;

implementation

uses
  GeneralFunctions;

{-------------------------------------------------------------------------------
  Retrieve the path to a COM object.
}
class function TDSSLocUtils.GetModulePath: string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName,
      Windows.GetModuleFileName(HInstance, ModName, SizeOf(ModName)));
  Result := ExpandLongPathName(Result);
end;

{-------------------------------------------------------------------------------
  Initialise localisation, and select the 'best match' language files according
      to the regional settings of windows.
}
class procedure TDSSLocUtils.Init;
begin
  LocalizerOnFly.Init;
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID)>-1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  // else try to find a match on generic language e.g. French
  else if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID)>-1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
end;

{-------------------------------------------------------------------------------
  Initialise the localization for a COM object. Needs to specify the path to
     the object so that the localization files don't get confused with the host
     app.
}
class procedure TDSSLocUtils.InitForComObject;
begin
  LocalizerOnFly.AppFileName := GetModulePath;
  Init;
end;

end.
