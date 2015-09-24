If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptMeasurementByContextName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[spRptMeasurementByContextName]
GO

/*===========================================================================*\
  Description:

  Parameters:
	@ContextName

  Created:	
	November 2002

  Last revision information:
    $Revision: 2 $
    $Date: 28/05/08 4:16p $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[spRptMeasurementByContextName]
	@ContextName varchar(20)
AS
	SET NOCOUNT ON

	SELECT 		MQ.Measurement_Qualifier_Key, 
				@ContextName + ' ' + MT.Short_Name + ' (' + MQ.Short_Name + ')' AS Description
	FROM 		Measurement_Type 			MT
	INNER JOIN 	Measurement_Qualifier 		MQ	ON MQ.Measurement_Type_Key 		= MT.Measurement_Type_Key
	INNER JOIN 	Measurement_Type_Context	MTC	ON MTC.Measurement_Type_Key		= MT.Measurement_Type_Key
	INNER JOIN	Measurement_Context 		MC	ON MC.Measurement_Context_Key 	= MTC.Measurement_Context_Key
	WHERE 		MC.Context_Name = @ContextName
	ORDER BY 	Description DESC
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spRptMeasurementByContextName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure spRptMeasurementByContextName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'Dev- JNCC SQL')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [Dev- JNCC SQL]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_RecordCardsOnly]
END
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

  $History: 0000005G.sql $
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 4:16p
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Removed USE statements
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 1:38p
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
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


/****** Object:  StoredProcedure [dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample]    Script Date: 05/28/2008 10:49:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description: Updates the event under which this sample is

  $History: 0000005G.sql $
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 4:16p
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Removed USE statements
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 1:38p
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
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
ALTER PROCEDURE [dbo].[usp_Event_Cascade_Vague_Date_Start_FromSample] (
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
  
    EXEC [usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent] @EventKey,
	@StartValue,
	@PreviousStartValue,
	@EndValue,
	@PreviousEndValue,
	@TypeValue,
	@PreviousTypeValue,NULL

    
    EXEC [usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent] @EventKey,
	@StartValue,
	@PreviousStartValue,
	@EndValue,
	@PreviousEndValue,
	@TypeValue,
	@PreviousTypeValue,NULL

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

/****** Object:  StoredProcedure [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent]    Script Date: 05/28/2008 11:17:31 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

  $History: 0000005G.sql $
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 4:16p
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Removed USE statements
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 1:38p
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
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
ALTER PROCEDURE [dbo].[usp_Sample_Cascade_Vague_Date_Start_FromEvent] (
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

  $History: 0000005G.sql $
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 4:16p
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Removed USE statements
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 28/05/08   Time: 1:38p
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
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
		S.SURVEY_EVENT_KEY = @EventKey
	AND (@CurrentSample IS NULL OR @CurrentSample <> S.SAMPLE_KEY)
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

