/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectRequiredFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectRequiredFields]
GO

/*===========================================================================*\
  Description:  List of Required column types for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 2/07/04 13:52 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectRequiredFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      IW_Column_Type_Key
    FROM        IW_Table_Rule_Related_Field
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
    AND         Relationship                    =   2
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectRequiredFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectRequiredFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [Dev - JNCC SQL]
END
