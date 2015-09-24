/*
 This script add the application role R2k_Administrator to the 
 fixed database role db_ddladmin. This is required to allow the
 use of the TRUNCATE TABLE functionality.

 Eric Salmon, 14 Feb 2003

 $History: 610 - ddl_admin role.sql $
 * 
 * *****************  Version 1  *****************
 * User: Ericsalmon   Date: 14/02/03   Time: 15:07
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts
 * Created.
 
*/

IF EXISTS(SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	EXEC sp_addrolemember N'db_ddladmin', N'R2k_Administrator'
GO
