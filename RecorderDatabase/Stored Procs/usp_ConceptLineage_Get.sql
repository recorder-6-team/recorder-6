/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptLineage_Get]
GO

/*===========================================================================*\
  Description: Returns a list of concept lineages for the supplied concept

  Parameters:	@ConceptKey
							@IncludeSynonyms - if 1 then lineages for all synonyms are returned
							@UserDomainMask

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 20/08/04 14:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_Get]
	@ConceptKey VARCHAR(100),
  @IncludeSynonyms BIT,
  @UserDomainMask INT
AS

IF @IncludeSynonyms=1
	SELECT DISTINCT CSyn.Concept_Key, CL.Lineage_ID, CL.Lineage, CSyn.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept CSyn on CSyn.Meaning_Key=C.Meaning_Key
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=CSyn.Concept_Key
  INNER JOIN Concept_Group CG on CG.Concept_Group_Key=CSyn.Concept_Group_Key
  INNER JOIN Local_Domain LD on LD.Local_Domain_Key=CG.Local_Domain_Key
  INNER JOIN Domain D on D.Domain_Key=LD.Domain_Key
      AND ((D.Domain_Mask & @UserDomainMask > 0) OR D.Has_Occurrences=0)
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

ELSE
	SELECT C.Concept_Key, CL.Lineage_ID, Lineage, C.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [Dev - JNCC SQL]
END

GO