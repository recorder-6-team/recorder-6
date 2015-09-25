/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForConcept') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Delete_ForConcept]
GO

/*===========================================================================*\
  Description:	Remove a given tag from all its associated surveys.

  Parameters:
	@ConceptKey

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 7/02/08 14:12 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Delete_ForConcept]
	@ConceptKey CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Survey_Tag
	WHERE  Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForConcept') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Delete_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [Dev - JNCC SQL]
END
GO
