/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SurveyTag_Insert]
GO

/*===========================================================================*\
  Description:	Insert a tag for a survey

  Parameters:
	@Key		Survey Tag Key OUTPUT
	@SurveyKey
	@ConceptKey
	@SessionID

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 1/02/08 11:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Insert]
	@Key 		CHAR(16) OUTPUT,
	@SurveyKey 	CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID 	CHAR(16)
AS
	SET NOCOUNT OFF
	
	EXEC spNextKey 'Survey_Tag', @Key OUTPUT

	INSERT INTO Survey_Tag (Survey_Tag_Key, Survey_Key, Concept_Key, Entered_Session_ID)
	VALUES (@Key, @SurveyKey, @ConceptKey, @SessionID)

	IF @@Error <> 0
		RAISERROR ('usp_SurveyTag_Insert failed', 16, 1)
	
	RETURN 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [Dev - JNCC SQL]
END
GO
