/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptItemName_Get]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_ConceptItemName_Get
GO

/*===========================================================================*\
  Description:	Returns the actual name of a concept.  The common name is 
				appended if required

  Parameters:	@Key 		Concept_Key
				@ItemName	Output - 303 characters = 150 + 150 for common + 3 for 
								space and 2 brackets
				@IncludeCommonName (optional)
				@IncludeAuthor (optional)
				@Formatted (optional)

  Created:	09 Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/01/04 16:17 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_ConceptItemName_Get]
(
@Key CHAR(16),
@IncludeCommonName BIT = 0,
@IncludeAuthor BIT = 0,
@Formatted BIT = 0
)
RETURNS varchar(303)

AS
BEGIN
DECLARE @ReturnValue VARCHAR(303)

SELECT @ReturnValue=
	CASE @Formatted WHEN 1 THEN Item_Name ELSE Plaintext END +
	CASE WHEN @IncludeAuthor=1 AND Author_Copy IS NOT NULL THEN ' ' + Author_Copy ELSE '' END
FROM VW_ConceptTerm 
WHERE Concept_Key=@Key

IF @IncludeCommonName=1
BEGIN
  DECLARE @CommonName VARCHAR(150)
	SELECT Top 1 @CommonName=T.Item_Name
	FROM Concept C1
	INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
	    AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	INNER JOIN Term T on T.Term_Key=C2.Term_Key
	INNER JOIN Language L on L.Language_Key=T.Language_Key
	    AND L.Priority=1
	WHERE C1.Concept_Key=@Key
  IF @CommonName IS NOT NULL 
	  SET @ReturnValue = @CommonName + ' (' + @ReturnValue + ')'
END

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptItemName_Get]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_ConceptItemName_Get'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [Dev - JNCC SQL]
	END
GO
