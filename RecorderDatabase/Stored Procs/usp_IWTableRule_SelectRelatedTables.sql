/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectRelatedTables]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectRelatedTables]
GO

/*===========================================================================*\
  Description:  List of related tables for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/04 14:22 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectRelatedTables]
    @table_rule_key     CHAR(16)
AS
    SELECT      Table_Name,
                Relationship
    FROM        IW_Table_Rule_Related_Table
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectRelatedTables') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectRelatedTables'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [Dev - JNCC SQL]
END
