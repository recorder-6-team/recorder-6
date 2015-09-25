/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Delete]
GO

/*===========================================================================*\
  Description:	Delete a keyword for a reference

  Parameters:	@Key		Reference Keyword Key

  Created:	December 2005

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 13:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Delete]
	@Key CHAR(16)
AS

	SET NOCOUNT OFF

	DELETE FROM Reference_Keyword WHERE Reference_Keyword_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [Dev - JNCC SQL]
END
GO
