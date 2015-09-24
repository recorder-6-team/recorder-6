/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
GO

/*===========================================================================*\
  Description:	Returns all names for an event's ownership

  Parameters:	@Key	Survey_Event_key

  Created:	Feb 2004

  Last revision information:
    $Revision: 2 $
    $Date: 19/01/06 15:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT DISTINCT
  SO.Survey_Event_Owner_Key,
	SO.Name_Key,
	OT.Short_Name,
	SO.Survey_Event_Owner_Type_Key,
	SO.Custodian,
	dbo.ufn_GetFormattedName(Name_Key) as DisplayName,
	SO.Entered_By
FROM Survey_Event_Owner SO
INNER JOIN Survey_Event_Owner_Type OT ON OT.Survey_Event_Owner_Type_Key = SO.Survey_Event_Owner_Type_Key
WHERE Survey_Event_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwners_Select_ForSurveyEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwners_Select_ForSurveyEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [Dev - JNCC SQL]
END
GO