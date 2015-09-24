/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypeMatchRules_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypeMatchRules_Select]
GO

/*===========================================================================*\
  Description:  List of match rules for fields of a column type.

  Parameters:   @column_type_key        Identifies the column type.

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/07/04 15:37 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypeMatchRules_Select]
    @column_type_key     CHAR(16)
AS
    SELECT      Field_Index,
                IW_Match_Rule_Key
    FROM        IW_Column_Type_Match_Rule
    WHERE       IW_Column_Type_Key          =   @column_type_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypeMatchRules_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWColumnTypeMatchRules_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [Dev - JNCC SQL]
END
