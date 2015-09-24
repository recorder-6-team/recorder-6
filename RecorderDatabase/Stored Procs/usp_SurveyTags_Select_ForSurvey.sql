/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForSurvey') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select_ForSurvey]
GO

/*===========================================================================*\
  Description:	Retrieve the tags for a survey

  Parameters:
	@Key		Survey Key

  Created:	January 2008

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/08 11:08 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select_ForSurvey]
	@Key CHAR(16)
AS
	SELECT 	ST.Survey_Tag_Key,
			CT.Concept_Key, 
			CP.Preferred_Concept_Key,
			CASE 
				WHEN CP.Preferred_Concept_Key <> CT.Concept_Key THEN CT.PlainText + ' (' + CP.PlainText + ')' 
				ELSE CP.PlainText 
			END AS PlainText,
			S.User_Name_Key AS Entered_By, 
			ST.Custodian
	FROM 	Survey_Tag				ST
	JOIN 	VW_ConceptTerm 			CT 	ON 	CT.Concept_Key	=	ST.Concept_Key
	JOIN 	VW_ConceptTermPreferred CP 	ON 	CP.Concept_Key	=	ST.Concept_Key
	JOIN 	Session 				S 	ON 	S.Session_ID	=	ST.Entered_Session_ID
	WHERE 	ST.Survey_Key 			= 	@Key
	ORDER BY CT.Sort_Code, CT.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForSurvey') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select_ForSurvey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [Dev - JNCC SQL]
END
GO
