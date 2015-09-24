/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_RecordID]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the site
                and record identifiers specified in the import data.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @record_no              Identifies the import row.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/07/04 16:41 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
    @table_name             VARCHAR(50),
    @field_name             VARCHAR(256),
    @record_no              INT,
    @key                    CHAR(16)    OUTPUT
AS
    IF OBJECT_ID('tempdb..#master') IS NULL
    BEGIN
        RAISERROR('Missing import data (#master table does not exist)', 16, 1)
        RETURN
    END

    DECLARE     @site_id    CHAR(8),
                @record_id  VARCHAR(6)

    SELECT      @site_id    =   SYSTEM010000000K_Site_ID,
                @record_id  =   SYSTEM010000000H_Record_ID
    FROM        #master
    WHERE       Record_No   =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    EXECUTE     usp_ImportWizard_NextKey    @table_name,
                                            @field_name,
                                            @site_id,
                                            @record_id,
                                            @key        OUTPUT
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_RecordID') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_RecordID'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [Dev - JNCC SQL]
END
