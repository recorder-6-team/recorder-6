{===============================================================================
  Unit:        BaseMSDESettingsPage

  Defines:     TfraBaseMSDESettings

  Description: A base frame for pages that request MSDE settings

  Model:

  Created:     March 2006

  Last revision information:
    $Revision: 8 $
    $Date: 12/02/09 10:33 $
    $Author: Ericsalmon $

===============================================================================}
unit BaseMSDESettingsPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, Registry, StdCtrls, ComObj, ExtCtrls;

resourcestring
  ResStr_InvalidInstanceName =
   'Unable to reach the specified SQL Server/MSDE instance.'#13#10
   + 'If you entered the instance name manually, please check it is spelt correctly.'#13#10#13#10
   + 'The error was as follows:'#13#10 + '''%s''';

type
  TfraBaseMSDESettings = class(TBasePage)
  protected
    procedure CheckTrustedConnectionMode(const AUserInstanceName: string; AWarningLabel: TLabel);
    function ValidInstanceName(const AInstanceName: String): Boolean;
  end;

{==============================================================================}
implementation

{$R *.dfm}

uses
  SetupConstants, TextMessages, GeneralFunctions, Functions;

{-------------------------------------------------------------------------------
  Warns the user if a selected instance only supports a trusted connection
}
procedure TfraBaseMSDESettings.CheckTrustedConnectionMode(const AUserInstanceName: string;
    AWarningLabel: TLabel);
var
  lKeyOpened: boolean;
begin
  // Check registry for Trusted Connection-Only login mode
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      Access  := KEY_READ;
      if CompareText(AUserInstanceName, Settings.ComputerName) = 0 then
        lKeyOpened := OpenKeyReadOnly(REG_KEY_MSDE_DEFAULT)
      else
        lKeyOpened := OpenKeyReadOnly(Format(REG_KEY_MSDE_NAMED,
                          [GetRegistryInstanceName(AUserInstanceName)]));

      AWarningLabel.Visible :=
          lKeyOpened and
          (ValueExists(REG_MSDE_LOGINMODE) and (ReadInteger(REG_MSDE_LOGINMODE) = 1)) and
          not AWarningLabel.Visible;
    finally
      Free;
    end
end;

{-------------------------------------------------------------------------------
  Check the specified server/instance exists and can be reached.
}
function TfraBaseMSDESettings.ValidInstanceName(const AInstanceName: String): Boolean;
var
  srv : OleVariant;
  cursor: TCursor;
begin
  cursor := HourglassCursor;
  try
    srv := CreateOleObject('SQLDMO.SQLServer');
    srv.LoginTimeout := 60;
    // Start Server or just connect if already started.
    try
      srv.Start(False, AInstanceName);
    except
      // Only getting the error message back.
      // Quick and dirty fix for German install: include the german message check.
      on E:EOleException do begin
        if not SQLDMORunning(E.ErrorCode, E.Message) then
        begin
          MessageDlg(
              Format(
                  ResStr_InvalidInstanceName,
                  [IntToStr(E.ErrorCode) + ': ' + E.Message]),
              mtError, [mbOk], 0);
          Result := False;
          Exit;
        end;
      end;
    end;
    Result := True;
  finally
    DefaultCursor(cursor);
  end;
end;

end.
