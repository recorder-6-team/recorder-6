/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CheckTableName]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
GO

/*===========================================================================*\
  Description:  Is the named table valid in an import/export database?

  Parameters:   @table_name             Table name
                @is_expected            [on exit] Is the table expected?

  Created:      July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 13/10/04 11:53 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
    @table_name     VARCHAR(30),
    @is_expected    BIT             OUTPUT
AS
    IF EXISTS ( SELECT      1
                FROM        DATABASE_RELATIONSHIP
                WHERE       MASTER_TABLE            =   @table_name
                OR          DETAIL_TABLE            =   @table_name)
								OR @table_name in ('metadata', 'metadata_type', 'source_join')
        SET         @is_expected    =   1
    ELSE
        SET         @is_expected    =   0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CheckTableName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CheckTableName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [Dev - JNCC SQL]
END