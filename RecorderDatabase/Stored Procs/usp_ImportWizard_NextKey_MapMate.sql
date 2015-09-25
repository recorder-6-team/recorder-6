/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_MapMate]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_MapMate]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the
                MapMate key specified in the import data.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @record_no              Identifies the import row.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 6 $
    $Date: 21/03/08 16:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_MapMate]
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

    DECLARE     @site_id        CHAR(8),
                @record_id      VARCHAR(8)

    SELECT      @site_id    =   SYSTEM010000000E_Site_ID,
                @record_id  =   SYSTEM010000000E_Record_ID
    FROM        #master
    WHERE       Record_No       =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    DECLARE     @previous_query     NVARCHAR(500),
                @previous_id        VARCHAR(4),
				@previous_key       VARCHAR(16),
				@next_id			VARCHAR(4)

    SET         @previous_query =   N'SELECT @key=MAX(' + @field_name + '), @result = '
                                    + ' CAST(MAX(CAST(SUBSTRING('
                                            + @field_name + ', 9, PATINDEX(''%E%'',RIGHT('
                                            + @field_name + ' ,8))-1 ) AS INT)) AS VARCHAR(4))'
                                    + ' FROM #' + @table_name
                                    + ' WHERE ' + @field_name + ' LIKE '''
                                    + @site_id + '____' + RIGHT(@record_id,4) + ''''

    EXECUTE     sp_executesql   @previous_query,
                                N'@key CHAR(16) OUTPUT, @result CHAR(2) OUTPUT',
								@key	= @previous_key OUTPUT,
                                @result = @previous_id OUTPUT
    IF @@ERROR <> 0 RETURN

	IF @previous_id IS NULL -- no previous record matching this key
        SET         @next_id  =   ''
    ELSE IF @previous_key IS NULL -- a previous record, but no unique key process applied yet
        SET         @next_id  =   '0'
    ELSE
		-- increment the id
        SET         @next_id  =   CAST(CAST(@previous_id AS INT)+1 AS VARCHAR(4))

    SET         @key    =   @site_id + @next_id + RIGHT(@record_id, 8-LEN(@next_id))

    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_MapMate') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_MapMate'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [Dev - JNCC SQL]
END
