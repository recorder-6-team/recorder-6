{-------------------------------------------------------------------------------
  Unit:        Constants.pas

  Defines:     <nothing>

  Description: Constant values used across the application.

  Created:     March 2003

  Last revision information:
    $Revision: 5 $
    $Date: 24/04/03 12:21 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit Constants;

interface

const
  PARAM_STANDALONE  = '/standalone';
  PARAM_SERVER      = '/server';
  PARAM_WORKSTATION = '/workstation';
  PARAM_UPGRADE     = '/upgrade';

  PARAM_SKIP        = '/skip';
  
  DATABASE_NAME     = 'NBNData';

  DEFAULT_INSTALL_FOLDER = 'JNCC\Recorder\';
  DEFAULT_MSDE_FOLDER    = 'Microsoft SQL Server\';

  // Values of registry entries, used with uninstall process
  STR_MSDE_DISPLAY_NAME    = 'Microsoft SQL Server Desktop Engine';
  STR_SYSCOMP_DISPLAY_NAME = 'Recorder System Components';

  // File names
  STR_SPATIAL_SYSTEMS = 'Spatial Systems.txt';
  STR_DEFAULT_ADDINS  = 'DefaultAddins.txt';
  STR_INSTALLSETTINGS = 'InstallSettings.ini';
  STR_INSTALLLOG      = 'InstallLog.txt';

  STR_WORKSTATIONSETUP = 'WorkstationSetup.exe'; // Launch app for workstation install
  STR_INSTALLPLUGIN    = 'InstallPlugin.exe';    // InstallPlugin's proper name
  STR_UNINSTALLER      = 'Uninstaller.exe';

  STR_ZIPPED_FILES_1  = 'System\RecStdSvr.zip';
  STR_ZIPPED_FILES_2  = 'System\RecStd.zip';

  // Registry keys
  REG_KEY_JNCC      = '\Software\JNCC';
  REG_KEY_RECORDER  = REG_KEY_JNCC + '\Recorder';
  REG_KEY_SETTINGS  = REG_KEY_RECORDER + '\Settings';
  REG_KEY_ADDIN     = REG_KEY_RECORDER + '\Installed Addins';

  REG_KEY_MICROSOFT       = '\Software\Microsoft';
  REG_KEY_SQLSERVER       = REG_KEY_MICROSOFT + '\Microsoft SQL Server';
  REG_KEY_CURRENT_VERSION = REG_KEY_MICROSOFT + '\Windows\CurrentVersion';
  REG_KEY_RUNONCE         = REG_KEY_CURRENT_VERSION + '\RunOnce';
  REG_KEY_UNINSTALL       = REG_KEY_CURRENT_VERSION + '\Uninstall';
  REG_KEY_UNINSTALLER     = REG_KEY_UNINSTALL + '\Recorder Uninstaller';

  // Registry value names
  REG_DBPLUGIN        = 'DBPlugin';
  REG_INSTANCES       = 'InstalledInstances';
  REG_ADDIN_PATH      = 'Addin Path';

  // Log file section titles
  STR_INSTALL_MODE      = '[InstallMode]';
  STR_ADDINS_SECTION    = '[Addins]';
  STR_SHORTCUTS_SECTION = '[Shortcuts]';
  STR_FOLDERS_SECTION   = '[Folders]';
  STR_FILES_SECTION     = '[Files]';
  STR_REGISTRY_SECTION  = '[Registry]';
  STR_MSDE_SECTION      = '[MSDE Instance]';
  STR_SYSCOMP_SECTION   = '[System Components]';

implementation

end.
