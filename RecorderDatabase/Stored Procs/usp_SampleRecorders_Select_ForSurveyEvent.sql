/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorders_Select_ForSurveyEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorders_Select_ForSurveyEvent]
GO

/*===========================================================================*\
  Description:	Returns a list of Sample Recorders for a given 
		Survey_Event_Key. (Saved in JNCC folder)

  Parameters:	@Key	Survey_Event_Key

  Created:	May 2004

  Last revision information:
    $Revision: 2 $
    $Date: 4/06/04 16:06 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorders_Select_ForSurveyEvent]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		SE_Recorder_Key AS [Key],
			Name_Key,
			dbo.ufn_GetFormattedName(Name_Key) AS [Name],
			Custodian,
			0x00000000005C275F AS [Timestamp] -- Bodge timestamp required by TAddinCachedDataItem.CreateFromRecord
	FROM		Survey_Event_Recorder
	WHERE		Survey_Event_Key = @Key
	ORDER BY	[Name]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorders_Select_ForSurveyEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorders_Select_ForSurveyEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [Dev - JNCC SQL]
END

GO