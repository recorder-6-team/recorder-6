
/****** Object:  StoredProcedure [dbo].[usp_Surveys_Select_ForSurveyTag]    Script Date: 08/19/2012 21:03:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

IF EXISTS(SELECT 1 FROM sysobjects WHERE NAME='usp_Surveys_Select_ForSurveyTag')
  DROP PROCEDURE [usp_Surveys_Select_ForSurveyTag];
GO

/*===========================================================================*\
  Description:	Retrieve the surveys for a tag

  Parameters:
	@Key		Survey Tag Key

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*=========================================================================== */
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
	@Key 	CHAR(16),
	@Order	BIT = 0,
	@UserNameKey	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Results TABLE (
		Survey_Tag_Key	CHAR(16) COLLATE Database_Default,
		Survey_Tag_Term	VARCHAR(150) COLLATE Database_Default,
		Concept_Key		CHAR(16) COLLATE Database_Default,
		Survey_Key		CHAR(16) COLLATE Database_Default,
		Item_Name		VARCHAR(100) COLLATE Database_Default,
		FullName		VARCHAR(60) COLLATE Database_Default,
		Entered_By		CHAR(16) COLLATE Database_Default,
		Custodian		CHAR(8) COLLATE Database_Default
	)

	CREATE TABLE #Synonyms (
		Item_Key 		CHAR(16) COLLATE Database_Default, 
		Item_Name 		VARCHAR(100) COLLATE Database_Default, 
		Language_Key 	VARCHAR(4) COLLATE Database_Default, 
		Language 		VARCHAR(50) COLLATE Database_Default, 
		Custodian 		CHAR(8) COLLATE Database_Default, 
		Entered_By 		CHAR(16) COLLATE Database_Default
	)
	INSERT INTO #Synonyms EXEC usp_ListSynonyms_Select_ForConcept @Key
	INSERT INTO #Synonyms (Item_Key) VALUES (@Key)

	INSERT INTO @Results
	SELECT		Survey_Tag_Key,
				CT.PlainText,
				CP.Preferred_Concept_Key,
				ST.Survey_Key, 
				SU.Item_Name,
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END,
				S.User_Name_Key, 
				ST.Custodian
	FROM		Survey_Tag				ST
	JOIN		VW_ConceptTerm			CT	ON	CT.Concept_Key	=	ST.Concept_Key
	JOIN		VW_ConceptTermPreferred CP 	ON 	CP.Concept_Key 	= 	ST.Concept_Key
	JOIN		Survey					SU	ON	SU.Survey_Key	=	ST.Survey_Key
	JOIN		Session					S	ON	S.Session_ID	= 	ST.Entered_Session_ID
	JOIN		Name					N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual				I	ON	I.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	0
	LEFT JOIN	Organisation 			O 	ON	O.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	1
	LEFT JOIN	User_Survey_Restriction USR ON USR.Survey_Key	=	SU.SURVEY_KEY 
											AND USR.Name_Key	=	@UserNameKey
	WHERE		ST.Concept_Key				IN	(SELECT Item_Key FROM #Synonyms)
				AND USR.User_Survey_Restriction_Key IS NULL
	DROP TABLE #Synonyms

	/* Return list sorted as requested */
	SELECT	R.Survey_Tag_Key,
			R.Concept_Key,
			R.Survey_Key,
			R.Item_Name AS SurveyName,
			R.FullName,
			CASE 
				WHEN ST.Concept_Key <> R.Concept_Key THEN '<' + R.Survey_Tag_Term + '> ' 
				ELSE '' 
			END + R.Item_Name + ' - ' + R.FullName AS ItemName,
			R.Entered_By,
			R.Custodian
	FROM	@Results	R
	JOIN	Survey_Tag	ST	ON	ST.Survey_Tag_Key = R.Survey_Tag_Key
	ORDER BY 
		CASE WHEN @Order = 0 THEN Item_Name ELSE FullName END

GO


GRANT EXECUTE ON usp_Surveys_Select_ForSurveyTag TO R2k_ReadOnly
GRANT EXECUTE ON usp_Surveys_Select_ForSurveyTag TO R2k_RecordCardsOnly
GRANT EXECUTE ON usp_Surveys_Select_ForSurveyTag TO R2k_AddOnly
GRANT EXECUTE ON usp_Surveys_Select_ForSurveyTag TO R2k_FullEdit
GRANT EXECUTE ON usp_Surveys_Select_ForSurveyTag TO R2k_Administrator