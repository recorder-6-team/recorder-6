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

  $History: usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent.sql $
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