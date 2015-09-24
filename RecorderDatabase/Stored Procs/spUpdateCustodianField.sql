If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spUpdateCustodianField]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spUpdateCustodianField'
        DROP PROCEDURE [dbo].[spUpdateCustodianField]
    END
GO

    PRINT 'Creating procedure spUpdateCustodianField'
GO

/*
    $History: spUpdateCustodianField.sql $
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 20/01/03   Time: 15:36
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE dbo.spUpdateCustodianField
--
--	DESCRIPTION
--	This procedure updates the custodian field for the specified table.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 20/01/2003
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@TableName VARCHAR(50),
@PrimaryKey VARCHAR(50)
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

EXEC('UPDATE [dbo].[' + @TableName + '] SET [CUSTODIAN] = LEFT(' + @PrimaryKey + ', 8)')


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spUpdateCustodianField') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spUpdateCustodianField'
        IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        	GRANT EXECUTE ON dbo.spUpdateCustodianField TO [Dev- JNCC SQL]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
       		GRANT EXECUTE ON dbo.spUpdateCustodianField TO [R2k_Administrator]
    END
GO