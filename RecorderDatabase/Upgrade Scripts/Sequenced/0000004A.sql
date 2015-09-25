SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Event_Cascade_Location_Key_FromSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Event_Cascade_Location_Key_FromSample]
GO

/*===========================================================================*\
  Description:	Updates the event under which this sample is

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Location_Key_FromSample] (
	@EventKey char(16),
	@Value char(16),
	@PreviousValue char(16)
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		LOCATION_KEY = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
		AND
		(@PreviousValue IS NULL OR @PreviousValue = LOCATION_KEY)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Event_Cascade_Location_Key_FromSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Event_Cascade_Location_Key_FromSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Key_FromSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Key_FromSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Key_FromSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Event_Cascade_Location_Name_FromSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Event_Cascade_Location_Name_FromSample]
GO

/*===========================================================================*\
  Description:	Updates the event under which the sample is

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Location_Name_FromSample] (
	@EventKey char(16),
	@Value varchar(100),
	@PreviousValue varchar(100)
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		LOCATION_NAME = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
		AND
		(@PreviousValue IS NULL OR @PreviousValue = LOCATION_NAME)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Event_Cascade_Location_Name_FromSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Event_Cascade_Location_Name_FromSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Name_FromSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Name_FromSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Event_Cascade_Location_Name_FromSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Event_Cascade_Spatial_Ref_FromSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Event_Cascade_Spatial_Ref_FromSample]
GO

/*===========================================================================*\
  Description:	Updates the event under which this sample is

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Spatial_Ref_FromSample] (
	@EventKey char(16),
	@Value varchar(40),
	@PreviousValue varchar(40)
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		SPATIAL_REF = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
		AND
		(@PreviousValue IS NULL OR @PreviousValue = SPATIAL_REF)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Event_Cascade_Spatial_Ref_FromSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Event_Cascade_Spatial_Ref_FromSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_FromSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_FromSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_FromSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample]
GO

/*===========================================================================*\
  Description:	Updates the event under which this sample is

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample] (
	@EventKey char(16),
	@Value varchar(20),
	@PreviousValue varchar(20)
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		SPATIAL_REF_QUALIFIER = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
		AND
		(@PreviousValue IS NULL OR @PreviousValue = SPATIAL_REF_QUALIFIER)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Event_Cascade_Spatial_Ref_Qualifier_FromSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample]
GO

/*===========================================================================*\
  Description: Updates the event under which this sample is

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample] (
	@EventKey char(16),
	@Value int,
	@PreviousValue int
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		VAGUE_DATE_START = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
		AND
		(@PreviousValue IS NULL OR @PreviousValue = VAGUE_DATE_START)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Event_Cascade_Vague_Date_Start_FromSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Event_Cascade_Vague_Date_Start_FromSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Vague_Date_Start_FromSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Event_Cascade_Vague_Date_Start_FromSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Event_Cascade_Vague_Date_Start_FromSample TO [Dev - JNCC SQL]
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
    $Revision: 1 $
    $Date: 29/01/08 9:17 $
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Location_Key_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Location_Key_Update]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Location_Key_Update] (
	@EventKey char(16),
	@Value char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_KEY = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Key_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ImportInitialise'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Location_Key_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Location_Key_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Location_Key_FromEvent] (
	@EventKey char(16),
	@Value char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_KEY = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Cascade_Location_Key_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Cascade_Location_Key_FromEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Key_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Key_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Key_FromEvent TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Location_Name_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Location_Name_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Location_Name_FromEvent] (
	@EventKey char(16),
	@Value varchar(100)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_NAME = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Cascade_Location_Name_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Cascade_Location_Name_FromEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Name_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Name_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Location_Name_FromEvent TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Spatial_Ref_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_FromEvent] (
	@EventKey char(16),
	@Value varchar(40)
)
AS
	UPDATE dbo.SAMPLE
	SET
		SPATIAL_REF = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Cascade_Spatial_Ref_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Cascade_Spatial_Ref_FromEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_FromEvent TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent] (
	@EventKey char(16),
	@Value varchar(20)
)
AS
	UPDATE dbo.SAMPLE
	SET
		SPATIAL_REF_QUALIFIER = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent] (
	@EventKey char(16),
	@Value int
)
AS
	UPDATE dbo.SAMPLE
	SET
		VAGUE_DATE_START = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Cascade_Vague_Date_Start_FromEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [Dev - JNCC SQL]
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


