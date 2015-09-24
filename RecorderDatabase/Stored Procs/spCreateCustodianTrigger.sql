If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spCreateCustodianTrigger]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spCreateCustodianTrigger'
        DROP PROCEDURE [dbo].[spCreateCustodianTrigger]
    END
GO

    PRINT 'Creating procedure spCreateCustodianTrigger'
GO

/*
    $History: spCreateCustodianTrigger.sql $
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 14/01/03   Time: 16:48
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE dbo.spCreateCustodianTrigger
--
--	DESCRIPTION
--	This procedure creates a trigger for the input table to maintain the custodian field.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--	@TableName		Name of table to create the trigger for.
--	@PrimaryKey 	Primary Key which holds the data to be used in the Custodian field.
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 07/01/2003
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@TableName	VARCHAR(50),
@PrimaryKey	VARCHAR(50)
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

IF EXISTS (SELECT * FROM SysObjects 
WHERE Id = OBJECT_ID(@TableName + 'CustodianInsert') AND OBJECTPROPERTY(Id, N'IsTrigger') = 1)
BEGIN
	PRINT 'Dropping trigger ' + @TableName + 'CustodianInsert'
	EXEC ('DROP TRIGGER ' + @TableName + 'CustodianInsert')
END

EXEC 
(
	' CREATE TRIGGER ' + @TableName + 'CustodianInsert ON dbo.' + @TableName +
  	' AFTER INSERT' +
    ' AS' +
    ' UPDATE ' + @TableName + 
    ' SET ' + @TableName + '.CUSTODIAN = SUBSTRING(' + @TableName + '.' + @PrimaryKey + ', 1, 8)' +
	' FROM ' + @TableName + ' INNER JOIN INSERTED ON ' + @TableName + '.' + @PrimaryKey + ' = INSERTED.' + @PrimaryKey +
    ' WHERE ' + @TableName + '.CUSTODIAN IS NULL'
)

IF @@Error = 0
	PRINT 'Created trigger ' + @TableName + 'CustodianInsert'
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spCreateCustodianTrigger') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spCreateCustodianTrigger'
        IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        	GRANT EXECUTE ON dbo.spCreateCustodianTrigger TO [Dev- JNCC SQL]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
       		GRANT EXECUTE ON dbo.spCreateCustodianTrigger TO [R2k_Administrator]
    END
GO