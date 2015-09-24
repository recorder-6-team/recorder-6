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
    $Revision: 2 $
    $Date: 28/02/08 9:57 $
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
