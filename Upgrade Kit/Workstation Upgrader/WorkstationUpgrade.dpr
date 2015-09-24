program WorkstationUpgrade;

{$R 'workstation.upgrade.manifest.res' 'workstation.upgrade.manifest.rc'}

uses
  Forms,
  LocOnFly,
  Windows,
  Dialogs,
  SysUtils,
  Messages,
  BaseADODataModule in 'M:\DssVcl32\BaseADODataModule.pas' {dmBaseADO: TDataModule},
  Recorder2000_TLB  in '..\..\..\..\Build\Recorder2000_TLB.pas',

  SetupConstants    in '..\..\..\Install Kit\Applications\Common 6\SetupConstants.pas',

  Constants         in '..\..\..\Build\Source\Constants.pas',

  BaseFrameUnit     in '..\Common\BaseFrameUnit.pas' {BaseFrame: TFrame},
  LoginFrame        in '..\Common\LoginFrame.pas' {fraLogin: TFrame},
  Settings          in '..\Common\Settings.pas',
  RecorderLock      in '..\Common\RecorderLock.pas',

  Main              in 'Source\Main.pas' {frmMain},
  UpgradeFrame      in 'Source\UpgradeFrame.pas' {fraUpgrade: TFrame};

{$R WorkstationUpgrade.KLR}

{$R *.res}

begin
  LocalizerOnFly.Init;
  // try to find exact match on language & sub language e.g. French (Luxembourg)
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.DefaultLCID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.DefaultLCID)
  else
  // else try to find a match on generic language e.g. French
  if LocalizerOnFly.LangList.IndexByLocale(Sysutils.SysLocale.PriLangID) > -1 then
    LocalizerOnFly.SwitchTo(Sysutils.SysLocale.PriLangID)
  else
    LocalizerOnFly.SwitchTo(LocalizerOnFly.NativeLocale);
  // Check for previous instance of Recorder 2000
  if not AcquireRecorderLock then
  begin
    MessageDlg(ResStr_AlreadyRunningOnThisMachine, mtWarning, [mbOk], 0);
    if ParamCount >= 1 then
      PostMessage(StrToInt64(Copy(ParamStr(1), 2, 255)), WM_CLOSE, 0, 0);
    Exit;
  end;

  Application.Initialize;
  // This line is used by Delphi itself to set the title in the Project's Options.
  // Can't use resource string for it in here. It's done in SetApplicationTitle further down.
  Application.Title := 'Recorder Workstation Application Upgrade';
  Application.CreateForm(TfrmMain, frmMain);
  frmMain.Settings.SequenceSettingName := 'DB Seq';
  frmMain.Settings.ScriptsOnly         := False;
  frmMain.Settings.DatabaseDescription := 'database structure';
  frmMain.SetApplicationTitle(ResStr_WorkstationUpgrade);
  Application.Run;
end.

