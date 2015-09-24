/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the
                supplied site and record identifiers.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @site_id                Site identifier.
                @record_id              Record identifier.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 27/07/04 16:41 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey]
    @table_name             VARCHAR(50),
    @field_name             VARCHAR(256),
    @site_id                CHAR(8),
    @record_id              VARCHAR(6),
    @key                    CHAR(16)    OUTPUT
AS
    SET         @record_id  =   REPLICATE('0', 6 - LEN(@record_id))
                                + @record_id

    DECLARE     @previous_query     NVARCHAR(500),
                @unique_id          CHAR(2)

    SET         @previous_query =   N'SELECT @result = '
                                    + ' MAX(SUBSTRING('
                                            + @field_name + ', 9, 2))'
                                    + ' FROM #' + @table_name
                                    + ' WHERE ' + @field_name + ' LIKE '''
                                    + @site_id + '__' + @record_id + ''''

    EXECUTE     sp_executesql   @previous_query,
                                N'@result CHAR(2) OUTPUT',
                                @result = @unique_id OUTPUT
    IF @@ERROR <> 0 RETURN

    IF @unique_id IS NULL
        SET         @unique_id  =   '00'
    ELSE IF SUBSTRING(@unique_id, 2, 1) = 'Z'
        SET         @unique_id  =   dbo.IncrementChar(LEFT(@unique_id, 1)) + '0'
    ELSE
        SET         @unique_id  =   LEFT(@unique_id, 1)
                                    + dbo.IncrementChar(
                                        SUBSTRING(@unique_id, 2, 1))

    SET         @key    =   @site_id + @unique_id + @record_id
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [Dev - JNCC SQL]
END
