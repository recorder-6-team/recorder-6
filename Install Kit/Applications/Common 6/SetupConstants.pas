{===============================================================================
  Unit:        SetupConstants

  Description:

  Created:     March 2004

  Last revision information:
    $Revision: 31 $
    $Date: 6/07/09 9:06 $
    $Author: Ericsalmon $

===============================================================================}

unit SetupConstants;

interface

const
  STR_ARROW = #0240;  // Arrow using Wingdings font.
  STR_TICK  = #0252;  // Tick using Wingdings font.

  PARAM_SKIP       = '/skip';

  DATABASE_NAME    = 'NBNData';
  ACCESS_MAIN_DB   = 'NBNData.mdb';
  ACCESS_DICT_DB   = 'NBNDict.mdb';
  MDF_FILENAME     = 'nbndata_data';
  LDF_FILENAME     = 'nbndata_log.ldf';

  STR_SYSCOMP_DISPLAY_NAME = 'Recorder System Components';

  CONNECTION_STRING = 'Provider=SQLOLEDB.1;Persist Security Info=False;%s' +
                      'Initial Catalog=master;Data Source=%s';

  // The default install folder are appended to the default "Program Files" Windows folder.
  DEFAULT_SERVER_FOLDER      = 'Recorder 6 Server\';
  DEFAULT_WORKSTATION_FOLDER = 'Recorder 6 Workstation\';
  DEFAULT_INSTALL_FOLDER     = 'Recorder 6\';

  // Other defaults.
  DEFAULT_INSTANCE_NAME     = 'RECORDER';
  DEFAULT_SQLEXPRESS_FOLDER = 'Microsoft SQL Server\';
  DEFAULT_LOCAL_INSTANCE    = 'MSSQLSERVER';
  DEFAULT_CHECKLIST         = 'NBNSYS0000000074';

  // Shortcuts values.
  R6_START_MENU   = 'Recorder 6';
  R6_PROGRAM_LINK = 'Recorder.lnk';
  R6_GUIDE_LINK   = 'Recorder Getting Started Guide.lnk';

  // Used for Recorder 2002 removal
  R2K2_START_MENU   = 'JNCC';  // Don't need to also specify the extra "\Recorder"
  R2K2_PROGRAM_LINK = 'Recorder 2002.lnk';
  R2K2_GUIDE_LINK   = 'Getting Started 2002.lnk';

  // Path and File names
  STR_BORLAND_PACKAGES    = 'BorlandPackages.zip';
  STR_MAPUPGRADE_SQL      = 'MapUpgrade.sql';
  STR_NBNDATA_MDB_FILE    = 'NBNData.mdb';  
  STR_NBNDATA_DATA_ZIP    = 'NBNData_Data.zip';
  STR_RECORDER_SPLASH_EXE = 'Recorder.exe';
  STR_RECORDER_MAIN_EXE   = 'RecorderApp.exe';
  STR_RECORDER_MAIN_OLD   = 'Recorder.ex_';
  STR_TABLE_MAPPINGS_INI  = 'TableMappings.ini';
  STR_UNINSTALLER         = 'Uninstall.exe';
  STR_UNINSTALLER_LANG    = 'Uninstall.ntv.*';
  STR_UNINSTALLER_EX      = 'Uninstall.ex_';
  STR_WORKSTATION_SETUP   = 'WorkstationSetup.*';
  STR_ALLOWED_ERRORS      = 'AllowedErrors.txt';
  STR_ZIPPED_FILES        = 'System\RecStdSvr.zip';
  STR_ZIPPED_USER_FILES   = 'System\UserFiles.zip';
  STR_ADDIN_INSTALLER_DLL = 'RecorderAddInInstaller.dll';

  STR_DEFAULT_ADDINS      = 'DefaultAddins.txt';
  STR_GETTING_STARTED     = 'Getting Started\Recorder 6 Getting Started Guide.chm';
  STR_INSTALLSETTINGS     = 'InstallSettings.ini';
  STR_INSTALLLOG          = 'InstallLog.txt';
  STR_SPATIAL_SYSTEMS     = 'Spatial Systems.txt';
  STR_DATABASE_PATH       = 'System\Database\';
  STR_WORKSTATION_PATH    = 'Workstation Setup\';
  STR_WORKSTATION_DB_PATH = 'Workstation Setup\Database\';
  STR_MAP_SCRIPT_STD      = STR_DATABASE_PATH + STR_MAPUPGRADE_SQL;
  STR_MAP_SCRIPT_WKS      = STR_WORKSTATION_DB_PATH + STR_MAPUPGRADE_SQL;
  STR_NBNDATA_MDB         = STR_DATABASE_PATH + ACCESS_MAIN_DB;
  STR_NBNDATA_ZIP_STD     = STR_DATABASE_PATH + STR_NBNDATA_DATA_ZIP;
  STR_NBNDATA_ZIP_WKS     = STR_WORKSTATION_DB_PATH + STR_NBNDATA_DATA_ZIP;
  STR_SPATIAL_SYSTEMS_STD = 'System\' + STR_SPATIAL_SYSTEMS;
  STR_SPATIAL_SYSTEMS_WKS = STR_WORKSTATION_PATH + STR_SPATIAL_SYSTEMS;
  STR_INSTALL_LOG_PATH    = '%s\Microsoft SQL Server\90\Setup Bootstrap\LOG\Summary.txt';

  // Registry keys
  REG_KEY_JNCC          = '\Software\JNCC';
  REG_KEY_R2K2          = REG_KEY_JNCC + '\Recorder';
  REG_KEY_R2K2_SETTINGS = REG_KEY_R2K2 + '\Settings';
  REG_KEY_DSS           = '\Software\Dorset Software';
  REG_KEY_R6            = REG_KEY_DSS + '\Recorder 6';
  REG_KEY_R6_SERVER     = REG_KEY_R6 + ' Server';
  REG_KEY_R6_SETTINGS   = REG_KEY_R6 + '\Settings';
  REG_KEY_R6_ADDINS     = REG_KEY_R6 + '\Installed Addins';

  REG_KEY_MICROSOFT                = '\Software\Microsoft';
  REG_KEY_SQLSERVER                = REG_KEY_MICROSOFT + '\Microsoft SQL Server';
  REG_KEY_CURRENT_VERSION          = REG_KEY_MICROSOFT + '\Windows\CurrentVersion';
  REG_KEY_RUNONCE                  = REG_KEY_CURRENT_VERSION + '\RunOnce';
  REG_KEY_UNINSTALL                = REG_KEY_CURRENT_VERSION + '\Uninstall';
  REG_KEY_UNINSTALLER              = REG_KEY_UNINSTALL + '\Recorder Uninstaller';
  REG_KEY_R2K2UNINSTALLER          = REG_KEY_UNINSTALL + '\Recorder 2002';
  REG_KEY_MSDE_DEFAULT             = REG_KEY_MICROSOFT + '\MSSqlServer\MSSQLServer';
  REG_KEY_SQLEXPRESS_DEFAULT_SETUP = REG_KEY_MICROSOFT + '\SQL Express\Setup';
  REG_KEY_SQL2005_INSTANCES        = REG_KEY_SQLSERVER + '\Instance Names\SQL';
  REG_KEY_SQLSERVER_NAMED          = REG_KEY_SQLSERVER + '\%s\MSSQLServer';
  REG_KEY_SQLSERVER_NAMED_SETUP    = REG_KEY_SQLSERVER + '\%s\Setup';

  // Registry value names
  REG_DBPLUGIN      = 'DatabaseSetup';
  REG_INSTANCES     = 'InstalledInstances';
  REG_INSTALLED     = 'Installed';
  REG_ADDIN_PATH    = 'Addin Path';
  REG_LOGINMODE     = 'LoginMode';
  REG_INSTANCE_PATH = 'SQLPath';
  REG_INSTANCE_DATA = 'SQLDataRoot';
  REG_BACKUP_DIRECTORY = 'BackupDirectory';

  // allow visibility of the 64 bit registry view
  KEY_WOW64_64KEY = $0100;

  // Log file section titles
  STR_INSTALL_MODE       = '[InstallMode]';
  STR_ADDINS_SECTION     = '[Addins]';
  STR_SHORTCUTS_SECTION  = '[Shortcuts]';
  STR_FOLDERS_SECTION    = '[Folders]';
  STR_FILES_SECTION      = '[Files]';
  STR_REGISTRY_SECTION   = '[Registry]';
  STR_SQLEXPRESS_SECTION = '[SQLExpress Instance]';

  // Values of registry entries, used with uninstall process
  STR_SQLEXPRESS_DISPLAY_NAME = 'Microsoft SQL Server 2005 Express Edition';
  STR_WIN_REGISTRY            = '\SOFTWARE\Microsoft\Windows\CurrentVersion';
  STR_PROGRAM_FILES           = 'ProgramFilesDir';
  STR_DOTNET_REGISTRY_PATH    = 'SOFTWARE\Microsoft\.NETFramework\policy\v2.0';
  STR_DOTNET_REGISTRY_FIELD   = '50727'; // The name of the field to check for the .Net 2.0 framework

  // Names of images in resource.
  ResImg_Welcome          = 'Welcome';
  ResImg_InstallFolder    = 'InstallFolder';
  ResImg_SiteInfo         = 'SiteInfo';
  ResImg_SpatialRef       = 'SpatialRef';
  ResImg_SQLEXPRESS       = 'MSDE';
  ResImg_Installation     = 'Installation';
  ResImg_Completion       = 'Completion';
  ResImg_MigrateSettings1 = 'MigrateSettings1';
  ResImg_MigrateSettings2 = 'MigrateSettings2';
  ResImg_Migrate          = 'Migrate';
  ResImg_MigrateLogin     = 'MigrateLogin';
  ResImg_DatabaseLogin    = 'DatabaseLogin';
  ResImg_Remove           = 'Remove';

  // Names of animations (AVI) in resource.
  ResAvi_VistaFileCopy    = 'VistaFileCopy';
  ResAvi_VistaFileDelete  = 'VistaFileDelete';

  // The size (in bytes) of the SQL Express installation (280 MB)
  I_SQLEXPRESSSIZE        = 280 * 1024 * 1024;

implementation

end.
