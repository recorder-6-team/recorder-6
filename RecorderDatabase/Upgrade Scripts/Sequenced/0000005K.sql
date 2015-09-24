SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the Determination under the parameter Event

  $History: 0000005K.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 19/06/08   Time: 12:03
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/06/08   Time: 15:44
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204 - CCN264 - amendment to cascading behaviour to include
 * determinations - points 7 and 8 fixed.
 * 
 * *****************  Version 1  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:15
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * New CCN264
 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent] (
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
	UPDATE BD
	SET
		BD.VAGUE_DATE_START = @StartValue,
		BD.VAGUE_DATE_END = @EndValue,
		BD.VAGUE_DATE_TYPE = @TypeValue
    FROM dbo.BIOTOPE_DETERMINATION BD
    INNER JOIN BIOTOPE_OCCURRENCE B ON B.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key
    INNER JOIN SAMPLE AS S on B.Sample_Key=S.Sample_Key
	WHERE
		S.SURVEY_EVENT_KEY = @EventKey
	AND (@CurrentSample IS NULL OR @CurrentSample <> S.SAMPLE_KEY)
	AND	(@PreviousStartValue IS NULL OR @PreviousStartValue = BD.VAGUE_DATE_START)
	AND	(@PreviousEndValue IS NULL OR @PreviousEndValue = BD.VAGUE_DATE_END)
	AND	(@PreviousTypeValue IS NULL OR @PreviousTypeValue = BD.VAGUE_DATE_TYPE)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent TO [Dev - JNCC SQL]
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

  $History: 0000005K.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 19/06/08   Time: 12:03
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 5  *****************
 * User: Johndurman   Date: 18/06/08   Time: 15:44
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204 - CCN264 - amendment to cascading behaviour to include
 * determinations - points 7 and 8 fixed.
 * 
 * *****************  Version 4  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:22
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * grant permission
 * 
 * *****************  Version 3  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:20
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204 CCN 264
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
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

  $History: 0000005K.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 19/06/08   Time: 12:03
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 5  *****************
 * User: Johndurman   Date: 18/06/08   Time: 15:44
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204 - CCN264 - amendment to cascading behaviour to include
 * determinations - points 7 and 8 fixed.
 * 
 * *****************  Version 4  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:24
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * grant permission
 * 
 * *****************  Version 3  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:19
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204
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

	-- VI 17204, bug 8 - Only update determinations if the Sample key is not specified
	IF @CurrentSample IS NULL 
	BEGIN
		EXEC [usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent] @EventKey,
		@StartValue,
		@PreviousStartValue,
		@EndValue,
		@PreviousEndValue,
		@TypeValue,
		@PreviousTypeValue,
		@CurrentSample
	    
		EXEC [usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent] @EventKey,
		@StartValue,
		@PreviousStartValue,
		@EndValue,
		@PreviousEndValue,
		@TypeValue,
		@PreviousTypeValue,
		@CurrentSample
	END
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent]
GO

/*===========================================================================*\
  Description:	Updates all the Determination under the parameter Event

  $History: 0000005K.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 19/06/08   Time: 12:03
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/06/08   Time: 15:44
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 17204 - CCN264 - amendment to cascading behaviour to include
 * determinations - points 7 and 8 fixed.
 * 
 * *****************  Version 1  *****************
 * User: Qingsun      Date: 28/05/08   Time: 11:16
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * CCN264
 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent] (
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
	UPDATE TD
	SET
		TD.VAGUE_DATE_START = @StartValue,
		TD.VAGUE_DATE_END = @EndValue,
		TD.VAGUE_DATE_TYPE = @TypeValue
    FROM dbo.TAXON_DETERMINATION TD
    INNER JOIN Taxon_Occurrence T ON T.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key
    INNER JOIN SAMPLE AS S on T.Sample_Key=S.Sample_Key
	WHERE
		((S.SURVEY_EVENT_KEY = @EventKey
	AND (@CurrentSample IS NULL OR @CurrentSample <> S.SAMPLE_KEY))
		OR (@EventKey IS NULL AND @CurrentSample = S.SAMPLE_KEY))
	AND	(@PreviousStartValue IS NULL OR @PreviousStartValue = TD.VAGUE_DATE_START)
	AND	(@PreviousEndValue IS NULL OR @PreviousEndValue = TD.VAGUE_DATE_END)
	AND	(@PreviousTypeValue IS NULL OR @PreviousTypeValue = TD.VAGUE_DATE_TYPE)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent TO [Dev - JNCC SQL]
END
GO

