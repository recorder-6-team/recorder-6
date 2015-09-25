/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeywords_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeywords_Get]
GO

/*===========================================================================*\
  Description:	Retrieve the keywords for a reference

  Parameters:	@Key		Reference Source_Key

  Created:	December 2005

  Last revision information:
    $Revision: 5 $
    $Date: 24/01/06 16:04 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeywords_Get]
	@Key CHAR(16)
AS

	SELECT RK.Reference_Keyword_Key, CT.Concept_Key, CT.Plaintext,
		S.User_Name_Key AS Entered_By, RK.Custodian
	FROM Reference_Keyword RK
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=RK.Concept_Key
	INNER JOIN Session S ON S.Session_ID=RK.Entered_Session_ID
	WHERE RK.Source_Key = @Key
	ORDER BY CT.Sort_Code, CT.Plaintext


GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeywords_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeywords_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [Dev - JNCC SQL]
END
GO
