/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectGeneratingFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingFields]
GO

/*===========================================================================*\
  Description:  List of generating column types for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 1/07/04 9:07 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      IW_Column_Type_Key
    FROM        IW_Table_Rule_Related_Field
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
    AND         Relationship                    =   1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectGeneratingFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectGeneratingFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [Dev - JNCC SQL]
END
