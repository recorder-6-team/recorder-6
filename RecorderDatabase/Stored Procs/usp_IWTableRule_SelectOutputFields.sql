/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectOutputFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectOutputFields]
GO

/*===========================================================================*\
  Description:  List of output fields for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 5/07/04 11:30 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectOutputFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      f.IW_Output_Field_Key,
                f.Name,
                f.Data_Type,
                f.IW_Column_Type_Key,
                f.Source_Field_Name,
                f.Generating_Class_Name,
                f.Generator_Field_Index,
                f.Literal_Value
    FROM        IW_Table_Rule_Output_Field  AS  rf
    INNER JOIN  IW_Output_Field             AS  f
    ON          f.IW_Output_Field_Key       =   rf.IW_Output_Field_Key
    WHERE       rf.IW_Table_Rule_Key        =   @table_rule_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectOutputFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectOutputFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [Dev - JNCC SQL]
END
