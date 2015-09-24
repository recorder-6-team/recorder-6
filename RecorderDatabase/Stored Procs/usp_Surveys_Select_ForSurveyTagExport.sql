/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTagExport]
GO

/*===========================================================================*\
  Description:	Returns all Surveys for a survey tag concept and its children.

  Parameters:
	@Key	Concept Key of tag

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Surveys_Select_ForSurveyTagExport
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	-- Doing this once is enough.
	DECLARE	@HierarchyRelationTypeKey CHAR(16)
	SELECT	@HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	C.Concept_Key	=	@Key

	-- Need somewhere to put all those keys.
	DECLARE	@Concepts TABLE (ConceptKey CHAR(16) COLLATE Database_Default)

	INSERT INTO @Concepts VALUES (@Key)

	-- Gather all nested concepts.
	WHILE @@RowCount > 0
		INSERT INTO @Concepts
		SELECT		To_Concept_Key
		FROM		Concept_Relation
		JOIN		@Concepts		ON	ConceptKey = From_Concept_Key
		WHERE		To_Concept_Key	NOT IN (SELECT ConceptKey FROM @Concepts)
		AND			Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey

	-- Return all relevant survey keys.
	SELECT 	DISTINCT Survey_Key
	FROM	Survey_Tag
	JOIN	@Concepts	ON	ConceptKey = Concept_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForSurveyTagExport'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [Dev - JNCC SQL]
END
GO
