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