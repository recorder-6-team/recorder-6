{-------------------------------------------------------------------------------
  Unit:        TextMessages.pas

  Defines:     <nothing>

  Description: Various text messages displayed either on controls or in message
               boxes.

  Created:     February 2003

  Last revision information:
    $Revision: 7 $
    $Date: 24/04/03 12:21 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit TextMessages;

interface

resourcestring
  ST_CLOSE_QUERY = 'Closing the wizard before it is complete can leave your Recorder '+
           'installation in an inconsistent state and can lead to unpredictable '+
           'results.  Are you sure you want to close the wizard now?';

  ST_MSDE_INSTALL_FAIL = 'The installation of MSDE failed.  The error is detailed below:'#13#10#13#10;
  ST_RESET_MSDE_SETTINGS = 'Please respecify the settings for your server installation.';
  ST_INSTANCE_INVALID_LOG = 'The instance name specified is invalid.';
  ST_INSTANCE_INVALID_EXPLANATION = 'The instance name specified is invalid.'#13#10+
           'This is likely to be because the instance name you have specified '+
           'is the same as an MSDE or SQL Server instance already installed on this '+
           'machine, or you are using the default instance name but there is already a '+
           'default instance on this machine.  Specify a different instance name to '+
           'fix this problem.';

  ST_NEXT_CAPTION    = '&Next  >';
  ST_INSTALL_CAPTION = '&Install';
  ST_PROCEED_CAPTION = '&Proceed';
  ST_FINISH_CAPTION  = '&Finish';
  ST_COLLATION_SEQUENCE = 'SQL_Latin1_General_CP1_CI_AS';

  ST_NO_MORE_INFO = 'No further error information is available.';

  ST_SERVER_FOLDER_ACCESS = 'Please ensure that all users have read/write access to the ' +
           'following directories:';

  ST_CREATE_MISSING_FOLDER = 'The selected folder does not exist. Create now?';
  ST_FOLDER_CREATE_FAILED  = 'Unable to create required folder.  Please make sure you have sufficient'#13#10 +
                             'access rights to perform this operation and try again.';
  ST_FOLDER_MUST_EXIST     = 'The folder must exist in order to complete the installation process.';

  ST_INSTALL_FILES   = 'Installing specific files...';
  ST_CREATE_SHORTCUTS= 'Creating shortcuts...';
  ST_FIX_SHORTCUTS   = 'Fixing shortcuts...';
  ST_CREATE_REGISTRY = 'Creating registry entries...';
  ST_INSTALL_ADDINS  = 'Installing Add-ins...';
  ST_INSTALL_MSDE    = 'Installing MSDE...';
  ST_ATTACHING_DB    = 'Attaching NBN Database...';
  ST_PLEASE_WAIT     = 'Please wait, this may take several minutes.';
  ST_WAITING_FOR_MSDE= 'Cancelling operation, waiting for MSDE...';

  // Remote install instructions
  ST_INSTRUCT_TITLE = 'Remote Server Database Setup Instructions';
  ST_INSTRUCT_ATTACH1 =
    '1) Unzip the System\Database\NBNData_Data.zip file from the CD into the SQL Server data folder you wish to use.';
  ST_INSTRUCT_ATTACH2 =
    '2) Attach this file to the SQL Server instance.  Use Enterprise Manager and right click on the Databases node for '+
    'the server you are installing onto.  Select All Tasks\Attach Database.  In the dialog that opens, click the ... button to locate the NBNData_Data.mdf file you copied in step 1.'+
    '  Select ''sa'' for the ''Specify Database Owner'' option and click Ok.';
  ST_INSTRUCT_ATTACH3 =
    '3) Execute the following script using Query Analyser or a similar tool.  Note that you need to replace the <backup path> with an appropriate path to store the backup file in.';
  ST_INSTRUCT_ATTACH4 =
    '4) Create a folder called ''%s'' on the SQL Server machine you are upgrading onto.  '+
    'Copy the *.mdb files from your installed Database folder to this folder.  '+
    'The database will be upgraded onto the '+
    'SQL Server after completing this install kit and rebooting the machine.  '+
    'When the upgrade process is complete, you can delete the folder you created on the SQL Server.';
  ST_INSTRUCT_ATTACH5 =
    'Ensure that users you want to be able to access Recorder belong to a Network Group '+
    'that has been associated with a login on the SQL Server, and that the login is granted '+
    'access to the NBNData database on the server.';
  ST_INSTRUCT_SQL_NBNUSER=
    'USE master '#13#10+
    '--Replace <backuppath> in the following statement with a suitable path for the backup file'#13#10+
    'IF NOT EXISTS(select * from sysdevices where name=''NBNData_Backup'')'#13#10+
    '          EXEC sp_addumpdevice ''disk'', ''NBNData_Backup'',''<backup path>\NBNBackup.bak'''#13#10+
    '--Create a server login'#13#10+
    'IF NOT EXISTS(select * from syslogins where name=''NBNUser'')'#13#10+
    '    EXEC sp_addlogin ''NBNUser'', ''NBNPassword'', ''NBNData'''#13#10+
    'EXEC sp_addsrvrolemember ''NBNUser'', ''dbCreator'''#13#10+
    'EXEC sp_addsrvrolemember ''NBNUser'', ''diskAdmin'''#13#10+
    ''#13#10+
    '--Link the server login to the new database'#13#10+
    'USE NBNData'#13#10+
    'EXEC sp_change_users_login ''Update_One'', ''NBNUser'', ''NBNUser''';
  ST_INSTRUCT_SQL_TRUSTED=
    'USE master '#13#10+
    '--Replace <backuppath> in the following statement with a suitable path for the backup file'#13#10+
    'IF NOT EXISTS(select * from sysdevices where name=''NBNData_Backup'')'#13#10+
    '          EXEC sp_addumpdevice ''disk'', ''NBNData_Backup'',<backup path>\NBNBackup.bak';

  MSG_CANT_REGISTER = 'The addin file %s cannot be registered.  Error described as :'#13#10;
  MSG_NOT_INSTALLED = 'The addins are not installed, and you do not have sufficient ' +
                      'permissions on this machine to install them.  Please ask your system ' +
                      'administrator to run the workstation install kit or Recorder ' +
                      'Application whilst logged in with Local Machine Administrator permissions.';
  MSG_ALREADY_INSTALLED = 'Addins are already installed on this machine.';

  MSG_ASK_REBOOT = 'You must restart your system for the installation'#13#10 +
                   'of Recorder to complete. Click Yes to restart now'#13#10 +
                   'or No if you plan to restart later.';

  // Error messages
  EST_FAILED_CREATE_DIR = 'Unable to create directory.  Please make sure you entered a valid path.';

  EST_NO_SERVICE = 'The service %s is not available to start.';

  EST_INTERNAL_REG  = 'An error occurred during the internal registration of file ' +
                      '%s.  Error :'#13#10;
  EST_INSTALL_ADDIN = 'An error occurred installing addin %s, described as :'#13#10;
  EST_REG_TYPELIB   = 'Addins could not be registered because the Recorder Type Library ' +
                      'failed to register.  Error - %s';

  EST_SPATIAL_REF_SYSTEMS = 'Spatial Systems cannot be loaded due to a problem with the install kit.';

  EST_REGISTRY = 'An error occurred while creating registry entries.';

implementation

end.
