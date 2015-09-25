SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_BiotopeDetermination_PreferredType_Get
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@Key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_BiotopeDetermination_PreferredType_Get
	@Key char(16),
	@Output char(16) output
AS
	SELECT @Output = Determination_Type_Key
	FROM Biotope_Determination
	WHERE Biotope_Occurrence_Key = @Key AND Preferred = 1
	SELECT @Output
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeDetermination_PreferredType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_DeterminationType_Update
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@Key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_DeterminationType_Update
	@Key char(16),
	@Verified int
AS
	UPDATE Determination_Type SET Verified = @Verified
    WHERE Determination_Type_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationType_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Verified_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationType_Verified_Get]
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@DetTypeKey

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationType_Verified_Get]
	@Key char(16),
	@Output int output
AS
	SELECT @Output = Verified
	FROM Determination_Type
	WHERE Determination_Type_Key = @Key
	SELECT @Output
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Verified_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationType_Verified_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [Dev - JNCC SQL]
END
GO

/*==	=========================================================================*\
Drop stored proc before re-creating.
\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_GetMainNodeTableForSubDetailTable]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_GetMainNodeTableForSubDetailTable]
GO
    
/*===========================================================================*\ 
Description: 
    When displaying a filter table, if the table is not one of the main hierarchy 
	nodes, find out the best match hierarchy node for it. For example, a 
	taxon determination actually links to the taxon occurrence node type.

Parameters: 
    @Key Collection unit key 
    @Name OUTPUT 

Created:    January 2008
Author:     David Kelly

Last revision information: 
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $
\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_GetMainNodeTableForSubDetailTable]
  @DetailTable  VARCHAR(100)
AS

	SELECT TOP 1 Master_Table, Master_Field, Detail_Table, Detail_Field
	FROM Database_Relationship 
	WHERE 
	Master_Table IN ('name', 'individual', 'organisation','survey','survey_event','sample','taxon_occurrence',
	'biotope_occurrence','location','location_feature','source','reference','taxon_list_item','biotope_list_item','admin_area')
	AND Detail_Table = @DetailTable
	ORDER BY 
		-- make joins to tables that have a partial match on the name higher priority
		CASE WHEN LEFT(Detail_Table, LEN(Master_Table))=Master_Table THEN 0 ELSE 1 END, 		
		-- make joins to the NAME table lower priority
		CASE Master_Table WHEN 'name' THEN 1 ELSE 0 END,
		-- if the Export tool follows down this relationship, then also prioritise
		Follow_Down DESC


GO

/*===========================================================================*\
Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.[usp_GetMainNodeTableForSubDetailTable]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_GetMainNodeTableForSubDetailTable]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $

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
                                    + ' MAX(SUBSTRING('
                                            + @field_name + ', 9, PATINDEX(''%E%'',RIGHT('
                                            + @field_name + ' ,8))-1 ))'
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

IF EXISTS (SELECT Name FROM   SysObjects 
	   WHERE  Id = Object_ID('[dbo].[usp_SampleDetails_Get]') AND Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleDetails_Get]
GO

/*
  DESCRIPTION
  This procedure returns the whole row from SAMPLE

  PARAMETERS
  @SampleKey  Parameter holding the Sample Key  

*/

CREATE PROCEDURE [dbo].[usp_SampleDetails_Get] 	
	@SampleKey char(16)
AS
	SELECT	S.SAMPLE_KEY, L.ITEM_NAME AS LOCATION, S.LOCATION_NAME, S.VAGUE_DATE_START, S.VAGUE_DATE_END,
			S.VAGUE_DATE_TYPE, S.SPATIAL_REF, ST.SHORT_NAME AS SAMPLE_TYPE			
	FROM	SAMPLE S
	LEFT JOIN LOCATION_NAME L
	ON 	S.LOCATION_KEY = L.LOCATION_NAME_KEY	
	INNER JOIN SAMPLE_TYPE ST
	ON	S.SAMPLE_TYPE_KEY = ST.SAMPLE_TYPE_KEY	
	WHERE S.SAMPLE_KEY = @SampleKey
GO

-- Grant access permissions
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_SampleDetails_Get]') AND type = 'P')
BEGIN
	PRINT 'Setting up security on procedure [dbo].[usp_SampleDetails_Get]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON [dbo].[usp_SampleDetails_Get] TO [R2k_AddOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON [dbo].[usp_SampleDetails_Get] TO [R2k_Administrator]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON [dbo].[usp_SampleDetails_Get] TO [R2k_FullEdit]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON [dbo].[usp_SampleDetails_Get] TO [R2k_ReadOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON [dbo].[usp_SampleDetails_Get] TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_TaxonDetermination_PreferredType_Get
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@DetTypeKey

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 28/01/08 15:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_TaxonDetermination_PreferredType_Get
	@Key char(16),
	@Output char(16) output
AS
	SELECT @Output = Determination_Type_Key
	FROM Taxon_Determination
	WHERE Taxon_Occurrence_Key = @Key AND Preferred = 1
	SELECT @Output
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_PreferredType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT Name FROM   SysObjects 
	   WHERE  Id = Object_ID('[dbo].[usp_EventDetails_Get]') AND Type = 'P')
    DROP PROCEDURE [dbo].[usp_EventDetails_Get]
GO

/*
  DESCRIPTION
  This procedure returns the whole row from SAMPLE

  PARAMETERS
  @SampleKey  Parameter holding the Sample Key  

*/

CREATE PROCEDURE [dbo].[usp_EventDetails_Get] 	
	@EventKey char(16)
AS
	SELECT	E.SURVEY_EVENT_KEY, L.ITEM_NAME AS LOCATION, E.LOCATION_NAME, E.VAGUE_DATE_START, E.VAGUE_DATE_END,
			E.VAGUE_DATE_TYPE, E.SPATIAL_REF
	FROM	SURVEY_EVENT E
	LEFT JOIN LOCATION_NAME L
	ON 	E.LOCATION_KEY = L.LOCATION_NAME_KEY		
	WHERE E.SURVEY_EVENT_KEY = @EventKey
GO

-- Grant access permissions
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_EventDetails_Get]') AND type = 'P')
BEGIN
	PRINT 'Setting up security on procedure [dbo].[usp_EventDetails_Get]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON [dbo].[usp_EventDetails_Get] TO [R2k_AddOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON [dbo].[usp_EventDetails_Get] TO [R2k_Administrator]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON [dbo].[usp_EventDetails_Get] TO [R2k_FullEdit]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON [dbo].[usp_EventDetails_Get] TO [R2k_ReadOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON [dbo].[usp_EventDetails_Get] TO [R2k_RecordCardsOnly]
END
GO


