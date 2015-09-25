/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Update') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Update]
GO

/*===========================================================================*\
  Description:	Update a tag for a survey

  Parameters:
	@Key		Survey Tag
	@ConceptKey
	@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 7/02/08 14:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Update]
	@Key 		CHAR(16),
	@SurveyKey	CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID 	CHAR(16)
AS
	SET NOCOUNT OFF

	UPDATE 	Survey_Tag
	SET 	Survey_Key			=	@SurveyKey,
			Concept_Key			=	@ConceptKey, 
			Changed_Session_ID	=	@SessionID
	WHERE 	Survey_Tag_Key		=	@Key

	IF @@Error <> 0
		RAISERROR ('usp_SurveyTag_Update failed', 16, 1)
	
	RETURN 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Update') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [Dev - JNCC SQL]
END
GO
