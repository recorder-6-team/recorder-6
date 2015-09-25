/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Insert]
GO

/*===========================================================================*\
  Description:	Insert a keyword for a reference

  Parameters:	@Key		Reference Keyword Key OUTPUT
			@SourceKey
			@ConceptKey
			@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 14/12/05 15:43 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Insert]
	@Key CHAR(16) OUTPUT,
	@SourceKey CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID CHAR(16)
AS

	SET NOCOUNT OFF
	
	EXEC spNextKey 'Reference_Keyword', @Key OUTPUT

	INSERT INTO Reference_Keyword (Reference_Keyword_Key, Source_Key, Concept_Key, Entered_Session_ID)
	VALUES (@Key, @SourceKey, @ConceptKey, @SessionID)

	IF @@Error <> 0
		RAISERROR ('usp_ReferenceKeyword_Insert failed', 16, 1)
	
	RETURN 0

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [Dev - JNCC SQL]
END
GO
