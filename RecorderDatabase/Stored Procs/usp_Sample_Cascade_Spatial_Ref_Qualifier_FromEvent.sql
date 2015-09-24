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

  $History: usp_Sample_Cascade_Spatial_Ref_Qualifier_FromEvent.sql $
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