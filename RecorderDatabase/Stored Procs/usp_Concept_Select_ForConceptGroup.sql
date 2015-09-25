/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Retrieves the concepts for the given concept group.

  Parameters:	@ConceptGroupKey	Key of the concept group

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 13:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroup]
	@ConceptGroupKey char(16)
AS
	-- For performance when using vw_ConceptTerm
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	Concept_Key, PlainText AS Item_Name
	FROM	vw_ConceptTerm
	WHERE 	Concept_Group_Key = @ConceptGroupKey
	AND 	List_Preferred = 1 
	AND 	Is_Current = 1 
	ORDER BY Sort_Code, PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroup TO [Dev - JNCC SQL]
END

GO
