/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Update]
GO

/*===========================================================================*\
  Description:	Update a keyword for a reference

  Parameters:	@Key		Reference Keyword Key
			@ConceptKey
			@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 14/12/05 15:43 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Update]
	@Key CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID CHAR(16)
AS

	SET NOCOUNT OFF

	UPDATE Reference_Keyword 
	SET Concept_Key=@ConceptKey, Changed_Session_ID=@SessionID
	WHERE Reference_Keyword_Key=@Key
	IF @@Error <> 0
		RAISERROR ('usp_ReferenceKeyword_Update failed', 16, 1)
	
	RETURN 0

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [Dev - JNCC SQL]
END
GO
