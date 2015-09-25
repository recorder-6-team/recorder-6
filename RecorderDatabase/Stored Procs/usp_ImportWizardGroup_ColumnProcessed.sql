/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizardGroup_ColumnProcessed]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizardGroup_ColumnProcessed]
GO

/*===========================================================================*\
  Description:  Check whether the specified column type has been processed
                in #IW_Group.

  Parameters:   @column_type_key        Identifies the column type
                @column_processed       [On exit] Indicates whether the column
                                        type has been processed

  Created:      June 2004

  Last revision:
    $Revision: 1 $
    $Date: 17.06.04 12:22 $
    $Author: Andrewkemp $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizardGroup_ColumnProcessed]
    @column_type_key    CHAR(16),
    @column_processed   BIT         OUTPUT
AS
    IF EXISTS ( SELECT      1
                FROM        #IW_Group
                WHERE       Column_Type_Key =   @column_type_key)
    BEGIN
        SET         @column_processed   =   1
    END
    ELSE
    BEGIN
        SET         @column_processed   =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizardGroup_ColumnProcessed') AND SysStat & 0xf = 4)
BEGIN
        PRINT 'Setting up security on procedure usp_ImportWizardGroup_ColumnProcessed'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
            GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [Dev - JNCC SQL]
END
GO