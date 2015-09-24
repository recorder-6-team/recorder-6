/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupLanguage_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupLanguage_Get]
GO

/*===========================================================================*\
  Description:	Update a simple non preferred synonym

  Parameters:	@Key		Concept Group key.
		@LanguageKey	OUTPUT

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 14/12/05 15:43 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupLanguage_Get]
	@Key CHAR(16),
	@LanguageKey VARCHAR(4) OUTPUT
AS
 	SELECT @LanguageKey = LD.Language_Key
	FROM Local_Domain LD 
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
		AND CG.Concept_Group_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupLanguage_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupLanguage_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [Dev - JNCC SQL]
END
GO
