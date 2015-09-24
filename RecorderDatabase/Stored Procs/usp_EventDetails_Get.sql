IF EXISTS (SELECT Name FROM   SysObjects 
	   WHERE  Id = Object_ID('[dbo].[usp_EventDetails_Get]') AND Type = 'P')
    DROP PROCEDURE [dbo].[usp_EventDetails_Get]
GO

/*
  DESCRIPTION
  This procedure returns the whole row from SAMPLE

  PARAMETERS
  @SampleKey  Parameter holding the Sample Key  

  $History: us_EventDetails_Get.sql $
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

