/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Select_ForSurveyTag') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Select_ForSurveyTag]
GO

/*===========================================================================*\
  Description:	Returns the first fact for a given concept used as a survey tag.

  Parameters:	@Concept	Concept Key

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 7/02/08 15:50 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Select_ForSurveyTag]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON

	SELECT 	TOP 1
			Thesaurus_Fact_Key,
			Item_Name,
			Data,
			Language_Key,
			Fact_Vague_Date_Start,
			Fact_Vague_Date_End,
			Fact_Vague_Date_Type,
			[Timestamp]
	FROM	Thesaurus_Fact
	WHERE 	Concept_Key 			= @ConceptKey
	AND		Fact_Type_Concept_Key 	= 'SYSTEM00000002NQ'

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Select_ForSurveyTag') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ThesaurusFact_Select_ForSurveyTag'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [Dev - JNCC SQL]
END
GO
