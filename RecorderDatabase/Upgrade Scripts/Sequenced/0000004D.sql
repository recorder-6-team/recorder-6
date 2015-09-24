-- Now that dbo rights have been denied, give read and write rights so that 
-- import code will still work as Access can't set an application role.
EXEC sp_Addrolemember 'db_datareader', 'NBNUser'
EXEC sp_Addrolemember 'db_datawriter', 'NBNUser'

-- Full edit can do export so need to update metadata
GRANT UPDATE ON Special_XML_Element TO R2K_FullEdit