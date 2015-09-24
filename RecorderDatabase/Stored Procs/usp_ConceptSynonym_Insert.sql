/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSynonym_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSynonym_Insert]
GO

/*===========================================================================*\
  Description:	Create a new synonym of an existing concept

  Parameters:	@PreferredConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@NameTypeConceptKey CHAR(16),
	@SessionID CHAR(16),
	@SiteID char(8) = NULL,
	@SystemSuppliedData BIT=0,
	@ConceptKey CHAR(16) OUTPUT - created concept key

  Created:	December 2005

  Last revision information:
    $Revision: 3 $
    $Date: 15/08/06 9:31 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSynonym_Insert]
	@PreferredConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@NameTypeConceptKey CHAR(16),
	@SessionID CHAR(16),
	@SiteID char(8) = NULL,
	@SystemSuppliedData BIT=0,
	@ConceptKey CHAR(16) OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET QUOTED_IDENTIFIER ON

	BEGIN TRANSACTION

		DECLARE 
			@TermKey CHAR(16),
			@MeaningKey CHAR(16),
			@ConceptGroupKey CHAR(16),
			@SynonymConceptKey CHAR(16)

		-- Find matching term record, if any
		SELECT @TermKey=Term_Key
		FROM Term
		WHERE Plaintext=@Term AND Language_Key=@LanguageKey
		
		-- No matching term record, so create one
		IF @TermKey IS NULL
		BEGIN
			EXEC usp_Term_Insert @TermKey OUTPUT, @LanguageKey, @Term, @Term, @SessionID, @SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		-- Identify the preferred term's meaning and concept group so we can link to it
		SELECT @MeaningKey = Meaning_Key, @ConceptGroupKey = Concept_Group_Key
		FROM Concept WHERE Concept_Key=@PreferredConceptKey
		
		EXEC spNextKey 'CONCEPT', @ConceptKey OUTPUT, @SiteID

		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID)
		VALUES (
			@ConceptKey, @TermKey, @ConceptGroupKey, 0, 1, 0, 
			@NameTypeConceptKey, @MeaningKey, @SessionID)
		IF @@Error <> 0 GOTO RollbackAndExit		

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSynonym_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSynonym_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSynonym_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [Dev - JNCC SQL]
END
GO
