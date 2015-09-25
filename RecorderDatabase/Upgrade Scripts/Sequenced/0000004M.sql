SET QUOTED_IDENTIFIER ON
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample] (
	@EventKey char(16),
	@StartValue int,
	@PreviousStartValue int,
	@EndValue int,
	@PreviousEndValue int,
	@TypeValue varchar(2),
	@PreviousTypeValue varchar(2)
)
AS
	UPDATE dbo.SURVEY_EVENT
	SET
		VAGUE_DATE_START = @StartValue,
		VAGUE_DATE_END = @EndValue,
		VAGUE_DATE_TYPE = @TypeValue
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND	(@PreviousStartValue IS NULL OR @PreviousStartValue = VAGUE_DATE_START)
	AND	(@PreviousEndValue IS NULL OR @PreviousEndValue = VAGUE_DATE_END)
	AND	(@PreviousTypeValue IS NULL OR @PreviousTypeValue = VAGUE_DATE_TYPE)
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationLocationName') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_ImportWizard_GetLocationLocationName
GO

/*===========================================================================*\
  Description:  Obtain the location name from an import record.

  Parameters:   @record_no       Identifies the import record.
                @location_name   [on exit] Location name.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 25/02/08 11:29 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationLocationName]
    @record_no      INT,
    @location_name  VARCHAR(100) OUTPUT
AS
    SELECT      @location_name      =   SYSTEM010000000T_data
    FROM        #master
    WHERE       Record_No           =   @record_no    

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationLocationName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationLocationName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationName') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
GO

/*===========================================================================*\
  Description:  Obtain the location name from an import record.

  Parameters:   @record_no          Identifies the import record.
                @lock_key			Location key if the record has a location
                @location_name      [on exit] Location name.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 25/02/08 11:29 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
    @record_no      INT,
    @loc_key		CHAR(16) = null,
    @location_name  VARCHAR(100) OUTPUT
AS
	SELECT      @location_name      =   SYSTEM0100000000_data
	FROM        #master
	WHERE       Record_No           =   @record_no
	IF @@ROWCOUNT = 0 GOTO NoSuchRecord

	IF @loc_key IS NOT NULL
	BEGIN
		DECLARE @properlocname VARCHAR(100)
		EXEC usp_LocationName_Get @loc_key, @properlocname

		--If location name already identified through main location record, no need to 
		--use the vague location name field
		IF @properlocname = @location_name 
			SET @location_name = NULL
	END
        	
	RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [Dev - JNCC SQL]
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 4  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

*/

CREATE PROCEDURE [dbo].[usp_SampleDetails_Get] 	
	@SampleKey char(16)
AS
	SELECT	S.SAMPLE_KEY, L.ITEM_NAME AS LOCATION, S.LOCATION_NAME, S.VAGUE_DATE_START, S.VAGUE_DATE_END,
			S.VAGUE_DATE_TYPE, S.SPATIAL_REF, ST.SHORT_NAME AS SAMPLE_TYPE			
	FROM	SAMPLE S
	LEFT JOIN LOCATION_NAME L
	ON 	S.LOCATION_KEY = L.LOCATION_KEY	
	INNER JOIN SAMPLE_TYPE ST
	ON	S.SAMPLE_TYPE_KEY = ST.SAMPLE_TYPE_KEY	
	WHERE S.SAMPLE_KEY = @SampleKey
	ORDER BY Sample_Reference
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

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SampleSameDateAsSurvey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure usp_SampleSameDateAsSurvey'
        DROP PROCEDURE [dbo].[usp_SampleSameDateAsSurvey]
    END
GO

    PRINT 'Creating procedure usp_SampleSameDateAsSurvey'
GO

/*
    $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 1  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:26
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

*/

CREATE PROCEDURE dbo.usp_SampleSameDateAsSurvey
--
--	DESCRIPTION
--	This procedure checks whether the specified sample has the same vague date as its survey event
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	John Durman, Dorset Software.
--	CREATED: 15/02/2008
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@SampleKey CHAR(16),
@SameDates BIT OUTPUT
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

	IF EXISTS (
		SELECT		S.Sample_Key 
		FROM		Sample				S
		INNER JOIN	Survey_Event		E
			ON		S.Survey_Event_Key	=	E.Survey_Event_Key
			AND		S.Vague_Date_Start	=	E.Vague_Date_Start
			AND		S.Vague_Date_End	=	E.Vague_Date_End
			AND		S.Vague_Date_Type	=	E.Vague_Date_Type
		WHERE		S.Sample_Key		=	@SampleKey
	)
		SET		@SameDates	=	1
	ELSE
		SET		@SameDates	=	0


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleSameDateAsSurvey') AND SysStat & 0xf = 4)
    BEGIN
		PRINT 'Setting up security on procedure [dbo].[usp_SampleSameDateAsSurvey]'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_AddOnly]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_Administrator]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_FullEdit]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_ReadOnly]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_RecordCardsOnly]
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Location_Key_FromEvent] (
	@EventKey char(16),
	@Value char(16),
	@PreviousValue char(16),
	@CurrentSample char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_KEY = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND
		(LOCATION_KEY = @PreviousValue OR @PreviousValue IS NULL)
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Location_Name_FromEvent] (
	@EventKey char(16),
	@Value varchar(100),
	@PreviousValue varchar(100),
	@CurrentSample char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_NAME = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND 
		(@CurrentSample IS NULL OR @CurrentSample <> SAMPLE_KEY)
	AND
		(LOCATION_NAME = @PreviousValue OR @PreviousValue IS NULL)
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_FromEvent] (
	@EventKey char(16),
	@Value varchar(40),
	@PreviousValue varchar(40),
	@CurrentSample char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		SPATIAL_REF = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND 
		(@CurrentSample IS NULL OR @CurrentSample <> SAMPLE_KEY)
	AND
		(SPATIAL_REF = @PreviousValue OR @PreviousValue IS NULL)
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent] (
	@EventKey char(16),
	@Value varchar(20),
	@PreviousValue varchar(20),
	@CurrentSample char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		SPATIAL_REF_QUALIFIER = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND 
		(@CurrentSample IS NULL OR @CurrentSample <> SAMPLE_KEY)
	AND
		(SPATIAL_REF_QUALIFIER = @PreviousValue OR @PreviousValue IS NULL)
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent] (
	@EventKey char(16),
	@StartValue int,
	@PreviousStartValue int,
	@EndValue int,
	@PreviousEndValue int,
	@TypeValue varchar(2),
	@PreviousTypeValue varchar(2),
	@CurrentSample char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		VAGUE_DATE_START = @StartValue,
		VAGUE_DATE_END = @EndValue,
		VAGUE_DATE_TYPE = @TypeValue
	WHERE
		SURVEY_EVENT_KEY = @EventKey
	AND (@CurrentSample IS NULL OR @CurrentSample <> SAMPLE_KEY)
	AND	(@PreviousStartValue IS NULL OR @PreviousStartValue = VAGUE_DATE_START)
	AND	(@PreviousEndValue IS NULL OR @PreviousEndValue = VAGUE_DATE_END)
	AND	(@PreviousTypeValue IS NULL OR @PreviousTypeValue = VAGUE_DATE_TYPE)
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

  $History: 0000004M.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 25/02/08   Time: 11:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

*/

CREATE PROCEDURE [dbo].[usp_EventDetails_Get] 	
	@EventKey char(16)
AS
	SELECT	E.SURVEY_EVENT_KEY, L.ITEM_NAME AS LOCATION, E.LOCATION_NAME, E.VAGUE_DATE_START, E.VAGUE_DATE_END,
			E.VAGUE_DATE_TYPE, E.SPATIAL_REF
	FROM	SURVEY_EVENT E
	LEFT JOIN LOCATION_NAME L
	ON 	E.LOCATION_KEY = L.LOCATION_KEY		
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


