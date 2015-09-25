/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ChooseListPreferred_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
GO

/*===========================================================================*\
  Description:	Takes a concept key from a concept that is about to stop
		being a synonym, selects all the other synonyms and makes the
		first one list preferred.

  Parameters:	@ConceptGroupKey - key of the concept group
		@ConceptKey - concept key

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 10/03/04 17:13 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
	@Key varchar(16)
AS
	DECLARE	@ConceptKeyMakePreferred char(16)

	SELECT TOP 1	@ConceptKeyMakePreferred = C1.Concept_Key
	FROM		Concept AS C1
	INNER JOIN 	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C2.Concept_Key = @Key
	AND		C2.Concept_Key <> C1.Concept_Key
	ORDER BY	C1.Sort_Code

	IF @ConceptKeyMakePreferred IS NOT NULL 
	BEGIN
		UPDATE	Concept
		SET	List_Preferred = 1
		WHERE	Concept_Key = @ConceptKeyMakePreferred
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ChooseListPreferred_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ChooseListPreferred_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [Dev - JNCC SQL]
END

GO

