/*===========================================================================*\
  Drop view before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptTermPreferred]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptTermPreferred]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts with the preferred term details

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermPreferred]
AS
	SELECT 	C.Concept_Key, 
			T.Item_Name, 
			T.Plaintext, 
			C.Author_Copy, 
			C.Concept_Group_Key, 
			C.Concept_Rank_Key, 
			C.Sort_Code,
			CP.Concept_Key AS Preferred_Concept_Key
	FROM 	Concept C
	JOIN 	Concept CP 	ON 	CP.Meaning_Key			=	C.Meaning_Key
						AND CP.List_Preferred		=	1
						AND CP.Concept_Group_Key	=	C.Concept_Group_Key
	JOIN 	Term 	T 	ON	T.Term_Key				=	CP.Term_Key
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_ConceptTermPreferred] TO [Public]
GO


SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT Concept_Key as Item_Key,
	  CASE WHEN Author_Copy IS NULL THEN
		Item_Name
	  ELSE
		Item_Name + ' ' + Author_Copy
	  END AS DisplayTerm,
	  CASE WHEN Author_Copy IS NULL THEN
		Plaintext
	  ELSE
		Plaintext + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  Author_copy,
	  Concept_Rank_Key
	FROM VW_ConceptTerm
	WHERE Concept_Group_Key = @SearchKey
	AND (Plaintext like @SearchText + '%'
	OR Author_Copy like @SearchText + '%')
	AND Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
	@SearchKey char(16),
	@SearchText varchar(100)

AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
		CASE Actual_Name_Italic
			WHEN 1 THEN '<i>' + Actual_Name + '</i>'
			ELSE Actual_Name
		END + ' ' + ISNULL(ITN.Authority, '') AS DisplayTerm, 
		Actual_Name + ' ' + ISNULL(ITN.Authority, '') AS SearchTerm
	FROM	Index_Taxon_Name ITN
	JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(Actual_Name LIKE @SearchText
	OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText
	OR 	ITN.Authority LIKE @SearchText)
	AND	TLI.Taxon_List_Version_To IS NULL
	AND TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchKey
		@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText
		OR 	ITN.Authority LIKE @SearchText)
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText
		OR 	ITN.Authority LIKE @SearchText)
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTag') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
GO

/*===========================================================================*\
  Description:	Retrieve the surveys for a tag

  Parameters:
	@Key		Survey Tag Key

  Created:	February 2008

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
	@Key 	CHAR(16),
	@Order	BIT = 0
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
	WHERE		ST.Concept_Key				IN	(SELECT Item_Key FROM #Synonyms)

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

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTag') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForSurveyTag'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select]
GO

/*===========================================================================*\
  Description:	Retrieve the survey tags for display in observation tree

  Parameters:

  Created:	February 2008

  Last revision information:
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select]
AS
	-- All concepts in the TAGS group
	SELECT		DISTINCT
				CP.Preferred_Concept_Key AS Concept_Key,
				CP.Sort_Code,
				CP.PlainText
	FROM		VW_ConceptTermPreferred	CP
	WHERE		CP.Concept_Group_Key 	= 'SYSTEM0100000001'

	-- UNION by default discards duplicates, which is exactly what we want here.
	UNION

	-- All concepts used as tags that are not necessarily in the TAGS group
	SELECT		DISTINCT
				CP.Preferred_Concept_Key AS Concept_Key,
				CP.Sort_Code,
				CP.PlainText
	FROM		VW_ConceptTermPreferred	CP
	JOIN		Survey_Tag				ST	ON	ST.Concept_Key = CP.Concept_Key

	ORDER BY 	CP.Sort_Code, CP.PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 3 $
    $Date: 10/04/08 9:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select_ForSurvey]
	@Key CHAR(16)
AS
	SELECT 	ST.Survey_Tag_Key,
			CT.Concept_Key, 
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

