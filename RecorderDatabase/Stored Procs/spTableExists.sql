If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spTableExists]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spTableExists'
        DROP PROCEDURE [dbo].[spTableExists]
    END
GO

    PRINT 'Creating procedure spTableExists'
GO

    /*
    $History: spTableExists.sql $
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 5/12/02    Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE [dbo].[spTableExists]

--
--	DESCRIPTION
--	This procedure checks for the existance of a specified table.
--	Used to check if source table exists on the server when linking from
--	the Access database.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--	@TableName		Name of table to check for.
--	@TableExists	Return parameter indicating when table exists.
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 05/12/2002  
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@TableName VARCHAR(50),
@TableExists BIT OUTPUT
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

AS
SET NOCOUNT ON

    DECLARE	@Procedure_Name    SYSNAME	--    Holds the name of the currently executing procedure
    DECLARE	@ErrorFlag    INT         		--    Flags if there has been an error during the procedure
    DECLARE	@ErrorNo    INT         		--    Holds the @@ERROR value
    DECLARE	@ErrorMsg    VARCHAR(255)     	--    User defined error message

    --    Set the procedure name in the variable
    SELECT @Procedure_Name = OBJECT_NAME (@@PROCID)

    --    Set the @ErrorFlag variable to -1 as the default return value. 
    SELECT @ErrorFlag = -1

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[' + @TableName + ']') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  SET @TableExists = 1
ELSE
  SET @TableExists = 0
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    SELECT @ErrorNo = @@ERROR

    IF @ErrorNo <> 0 
    BEGIN
        SELECT @ErrorMsg = 'Error number ' + CONVERT ( VARCHAR(6), @ErrorNo ) + ' in procedure ' + @Procedure_Name
        RAISERROR (@ErrorMsg, 16, -1) --WITH LOG
        GOTO PROC_RETURN
    END

    SELECT @ErrorFlag = 0

PROC_RETURN:

    RETURN @ErrorFlag
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spTableExists') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spTableExists'
        	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        		GRANT EXECUTE ON dbo.spTableExists TO [Dev- JNCC SQL]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        		GRANT EXECUTE ON dbo.spTableExists TO [R2k_Administrator]
    END
GO
