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
	@SampleKey char(16),
	@PreviousValue varchar(40)
)
AS

	UPDATE E
	SET E.Spatial_Ref = S.Spatial_Ref,
		E.Spatial_Ref_System = S.Spatial_Ref_System,
		E.Lat = S.Lat,
		E.Long = S.Long,
		E.Spatial_Ref_Qualifier = S.Spatial_Ref_Qualifier
	FROM dbo.Survey_Event E
	INNER JOIN dbo.Sample S ON E.Survey_Event_Key = S.Survey_Event_Key
	WHERE S.SAMPLE_KEY = @SampleKey
	AND
		(E.SPATIAL_REF = @PreviousValue OR @PreviousValue IS NULL)

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