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

  $History: usp_Sample_Cascade_Spatial_Ref_FromEvent.sql $
 * 
 * *****************  Version 3  *****************
 * User: Johndurman   Date: 14/03/08   Time: 11:40
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations
 * 
 * *****************  Version 2  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:27
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Cascade_Spatial_Ref_FromEvent] (
	@EventKey char(16),
	@Value varchar(40),		-- @Value is no longer used but retained in parameter list to avoid breaking Delphi code
	@PreviousValue varchar(40),
	@CurrentSample char(16)
)
AS
	UPDATE S
	SET S.Spatial_Ref = E.Spatial_Ref,
		S.Spatial_Ref_System = E.Spatial_Ref_System,
		S.Lat = E.Lat,
		S.Long = E.Long,
		S.Spatial_Ref_Qualifier = E.Spatial_Ref_Qualifier
	FROM dbo.Sample S
	INNER JOIN dbo.Survey_Event E ON E.Survey_Event_Key = S.Survey_Event_Key
	WHERE E.SURVEY_EVENT_KEY = @EventKey
	AND 
		(@CurrentSample IS NULL OR @CurrentSample <> S.SAMPLE_KEY)
	AND
		(S.SPATIAL_REF = @PreviousValue OR @PreviousValue IS NULL)

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