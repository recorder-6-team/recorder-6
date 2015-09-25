/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizardGroup_Insert]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizardGroup_Insert]
GO

/*===========================================================================*\
  Description:  Adds a record to #IW_Group.

  Parameters:   @column_type_key        Identifies the column type
                @qualification          Column type qualification
                @record_no              Identifies the imported record
                @group_id               Identifies the group of values
                @group_hash             Hash of grouped values

  Created:      June 2004

  Last revision:
    $Revision: 2 $
    $Date: 9/07/04 12:10 $
    $Author: Andrewkemp $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizardGroup_Insert]
    @column_type_key    CHAR(16),
    @qualification      VARCHAR(50),
    @record_no          INT,
    @group_id           INT,
    @group_hash         INT
AS
    /* work around BaseADODataModule crapness that makes it impossible
       to pass an empty string as an argument */
    IF @qualification IS NULL
        SET         @qualification  =   ''

    INSERT INTO #IW_Group (
                Column_Type_Key,
                Qualification,
                Record_No,
                Group_Hash,
                Group_ID)
    VALUES      (@column_type_key,
                @qualification,
                @record_no,
                @group_hash,
                @group_id)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizardGroup_Insert') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizardGroup_Insert'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [Dev - JNCC SQL]
END
GO