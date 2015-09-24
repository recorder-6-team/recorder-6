
unit TextMessages;

interface

resourcestring
  ResStr_GettingStartedGuide = 'Recorder 6 Getting Started Guide';

  ResStr_OutOfMemory           = 'Out of memory.';
  ResStr_InvalidArguments      = 'One or more of the arguments is invalid.';
  ResStr_CannotWriteToFile     = 'The function could not write to the file.';
  ResStr_RegistryAccessProblem = 'The system registration database could not be opened.';
  ResStr_CannotOpenTypeLibrary = 'The type library could not be opened.';

  ResStr_CloseQueryInstall =
      'Closing the wizard before it is complete can leave your Recorder'
      + ' installation in an inconsistent state and can lead to unpredictable'
      + ' results.  Are you sure you want to close the wizard now?';

  ResStr_CloseQueryMigrate =
      'Aborting the transfer of data at this point is very likely to'
      + ' leave the data and the programme unusuable.  You will need to either rerun'
      + ' the transfer from scratch or revert to a blank copy of the database in order'
      + ' to use the programme effectively.  Are you sure you want to close the wizard now?';

  ResStr_SQLExpressInstallFailed =
      'The installation of SQL Express failed.  The error is detailed below:'#13#10#13#10;
  ResStr_DotNet2InstallFailed =
      'The installation of the .Net Framework v2.0 failed.';
  ResStr_ResetSQLExpressSettings = 'Please respecify the settings for your server installation.';
  ResStr_InvalidInstance   = 'The instance name specified is invalid.';
  ResStr_InvalidInstanceReason =
      'The instance name specified is invalid.'#13#10
      + 'This is likely to be because the instance name you have specified is the'
      + ' same as an MSDE, SQL Express or SQL Server instance already installed on this machine,'
      + ' or you are using the default instance name but there is already a default'
      + ' instance on this machine.  Specify a different instance name to fix this problem.';

  ResStr_ContinueCaption = '&Continue';
  ResStr_FinishCaption   = '&Finish';
  ResStr_InstallCaption  = '&Install';
  ResStr_MainMenuCaption = '&Main Menu';
  ResStr_NextCaption     = '&Next  >';
  ResStr_ProceedCaption  = '&Proceed';
  ResStr_RemoveCaption   = '&Remove';
  ResStr_TransferCaption = '&Transfer';

  ResStr_ServerLogin     = 'Server Login for %s';

  ResStr_CollationSequence = 'SQL_Latin1_General_CP1_CI_AS';

  ResStr_NoMoreInfo     = 'No further error information is available.';

  ResStr_ServerFolderAccess =
      'Please ensure that all users have read/write access to the following directories:';

  ResStr_CreateMissingFolder = 'The selected folder does not exist. Create now?';
  ResStr_FolderCreateFailed  =
      'Unable to create required folder.  Please make sure you have sufficient'#13#10
      + 'access rights to perform this operation and try again.';
  ResStr_FolderMustExist =
      'The folder must exist in order to complete the installation process.';

  ResStr_AvailableDiskSpace = 'Available disk space on selected drive:';
  ResStr_UnknownAvailableDiskSpace = 'Unable to determine available disk space on selected drive.';
  ResStr_DiskFull = 'The application cannot proceed until there is enough disk space available.';

  ResStr_InstallFiles      = 'Installing specific files...';
  ResStr_CreateShortcuts   = 'Creating shortcuts...';
  ResStr_FixShortcuts      = 'Fixing shortcuts...';
  ResStr_CreateRegistry    = 'Creating registry entries...';
  ResStr_InstallAddins     = 'Installing Add-ins...';
  ResStr_InstallSQLExpress = 'Installing SQL Express...';
  ResStr_InstallDotNet2    = 'Installing .Net Framework v2.0...';
  ResStr_AttachingDB       = 'Attaching NBN Database...';
  ResStr_CreatingAccessMDB = 'Creating Access linked database...';
  ResStr_PleaseWait        = 'Please wait, this may take several minutes.';
  ResStr_WaitingForSQLExpress='Cancelling operation, waiting for SQL Express...';
  ResStr_WaitingForDotNet2 = 'Cancelling operation, waiting for .Net Framework v2.0...';

  ResStr_AccessMDBNotFound =
      'The NBNData.mdb file cannot be found in the specified folder.'#13#10
      + 'Please make sure you are pointing to the right folder.';
  ResStr_MigratingDataFromAccess = 'Migrating data from Access database...';
  ResStr_StartingSQLExpress      = 'Connecting to the database...';
  ResStr_CreatingDatabaseFile    = 'Creating database file...';
  ResStr_ConfiguringDatabase     = 'Configuring database...';

  ResStr_WindowsAuthenticationWarning =
      'Warning:  This SQL Server instance has been set to support Windows Authentication only. '
      + 'This means that only your Windows login will be accepted when connecting to it.';

  // Remote install instructions
  ResStr_InstructTitle = 'Remote Server Database Setup Instructions';
  ResStr_InstructAttach1 =
      '1) Unzip the System\Database\NBNData_Data.zip file from the CD into the SQL'
      + ' Server data folder you wish to use.';
  ResStr_InstructAttach2 =
      '2) Attach this file to the SQL Server instance.  Use Enterprise Manager and right'
      + ' click on the Databases node for the server you are installing onto.  Select All'
      + ' Tasks\Attach Database.  In the dialog that opens, click the ... button to locate'
      + ' the NBNData_Data.mdf file you copied in step 1.  Select ''sa'' for the ''Specify'
      + ' Database Owner'' option and click Ok.';
  ResStr_InstructAttach3 =
      '3) Execute the following script using Query Analyser or a similar tool.  Note that you'
      + ' need to replace the <backup path> with an appropriate path to store the backup file in.';
  ResStr_InstructAttach4 =
      '4) Create a folder called ''%s'' on the SQL Server machine you are upgrading onto.'
      + '  Copy the *.mdb files from your installed Database folder to this folder.'
      + '  The database will be upgraded onto the SQL Server after completing this install'
      + ' kit and rebooting the machine.  When the upgrade process is complete, you can delete'
      + ' the folder you created on the SQL Server.';
  ResStr_InstructAttach5 =
      'Ensure that users you want to be able to access Recorder belong to a Network Group'
      + ' that has been associated with a login on the SQL Server, and that the login is granted'
      + ' access to the NBNData database on the server.';
  ResStr_InstructSQLNBNUser =
      'USE master '#13#10
      + '--Replace <backuppath> in the following statement with a suitable path for the backup file'#13#10
      + 'IF NOT EXISTS(select * from sysdevices where name=''NBNData_Backup'')'#13#10
      + '          EXEC sp_addumpdevice ''disk'', ''NBNData_Backup'',''<backup path>\NBNBackup.bak'''#13#10
      + '--Create a server login'#13#10
      + 'IF NOT EXISTS(select * from syslogins where name=''NBNUser'')'#13#10
      + '    EXEC sp_addlogin ''NBNUser'', ''NBNPassword'', ''NBNData'''#13#10
      + 'EXEC sp_addsrvrolemember ''NBNUser'', ''dbCreator'''#13#10
      + 'EXEC sp_addsrvrolemember ''NBNUser'', ''diskAdmin'''#13#10
      + #13#10
      + '--Link the server login to the new database'#13#10
      + 'USE NBNData'#13#10
      + 'EXEC sp_change_users_login ''Update_One'', ''NBNUser'', ''NBNUser''';
  ResStr_InstructSQLTrusted =
      'USE master '#13#10
      + '--Replace <backuppath> in the following statement with a suitable path for the backup file'#13#10
      + 'IF NOT EXISTS(select * from sysdevices where name=''NBNData_Backup'')'#13#10
      + '          EXEC sp_addumpdevice ''disk'', ''NBNData_Backup'',<backup path>\NBNBackup.bak';

  ResStr_CannotRegisterAddin = 'The addin file %s cannot be registered.  Error described as:'#13#10;
  ResStr_AddinsNotInstalled   =
      'The addins are not installed, and you do not have sufficient permissions on this machine'
      + ' to install them.  Please ask your system administrator to run the workstation install'
      + ' kit or Recorder Application whilst logged in with Local Machine Administrator permissions.';
  ResStr_AddinsAlreadyInstalled = 'Addins are already installed on this machine.';

  ResStr_DatabaseInstallMustRunLocal =
      'The NBN Database installation must be run from a local drive to the machine'#13#10
      + 'the server instance is installed on.'#13#10#13#10
      + 'Please see the help file for more information.';

  ResStr_SQLExpressInstallMustRunLocal =
      'The SQL Express installation program must be run from a local drive to the machine'#13
      + 'the server instance is to be installed on.'#13#13
      + 'Please see the help file for more information.';

  // Error messages
  ResStr_InvalidFolderName =
      'Unable to create folder.  Please make sure you entered a valid path.';

  ResStr_AddinInternalRegistrationError =
      'An error occurred during the internal registration of file %s.  Error:'#13#10;
  ResStr_AddinInstallError = 'An error occurred installing addin %s, described as:'#13#10;
  ResStr_AddinTypeLibRegistrationError =
      'Addins could not be registered because the Recorder Type Library failed to register.  '
      + 'Error: %s';

  ResStr_SpatialRefSystemsError =
      'Spatial Systems cannot be loaded due to a problem with the install kit.';

  ResStr_RegistryError = 'An error occurred while creating registry entries.';

  ResStr_InvalidPathError =
      'The path ''%s'' is invalid.'#13#13'Please make sure the path you entered exists.';
  ResStr_LocalPathError   =
      'The Recorder 2002 Access files must be copied into a local folder and migrated'
      + ' from there.  The path you have selected is on the network.  Before continuing,'
      + ' please copy all the *.mdb files in this folder into a folder on the local disk,'
      + ' then select the local folder to migrate files from.  You can delete the local'
      + ' copy of the files once the migration is complete.';

  ResStr_PasswordsDoNotMatch =
      'The confirmation password does not match the original password- please re-type '
      + 'the passwords to ensure there have been no errors.';

  ResStr_PasswordLength =
      'The password should be at least 8 characters in length.';

  ResStr_ThreeCharacterTypes =
      'The password should include 3 or more different types of character '
      + '(out of upper case letters, lower case letters, numbers, or non-alphanumeric characters).';

  ResStr_DotNet2NotFound =
      'SQL Express requires you to have .Net Framework Version 2.0 '
      + 'installed on your computer. The .Net Framework can be '
      + 'downloaded for free from:'#13#10#13#10
      + 'http://www.microsoft.com/downloads';
  ResStr_InvalidInstanceName =
      'The SQL Server instance name you have provided does not appear to be valid. Please recheck it.';
  ResStr_InvalidLogin =
      'The SQL Server connection settings you have provided are not working. This could be because the SQL Server instance '+
          'name has not been supplied correctly, the SQL Server instance name is correct but the server instance is not running, '+
          'or the login details you have provided are not correct. Please recheck these points and try again.';
  ResStr_SqlServerInstall = 'Before installing Recorder 6, you will need to install your '+
      'own copy of SQLExpress or SQL Server on this machine. If you do not already have it '+
      'installed then we recommend you install SQLExpress 2008 SP2, which is available from '+
      'the <a href="http://www.microsoft.com/download/en/details.aspx?id=20610">Microsoft '+
      'Download Center</a>.';

implementation

end.
