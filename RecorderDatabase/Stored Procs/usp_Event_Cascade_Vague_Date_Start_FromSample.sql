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

  $History: usp_Event_Cascade_Vague_Date_Start_FromSample.sql $
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
