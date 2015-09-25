/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSynonym_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSynonym_Update]
GO

/*===========================================================================*\
  Description:	Update a simple non preferred synonym

  Parameters:	@Key		Concept key.
		@Term		Term string
		@LanguageKey

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 15/08/06 13:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSynonym_Update]
	@ConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET QUOTED_IDENTIFIER ON

	DECLARE 
		@TermKey CHAR(16),
		@OldTermKey CHAR(16)
		 

	BEGIN TRANSACTION

		-- Check if the term record is still OK
		SELECT @TermKey = T.Term_Key
		FROM Concept C
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		WHERE T.Plaintext=@Term
		AND T.Language_Key=@LanguageKey
		AND C.Concept_Key=@ConceptKey

		IF @TermKey IS NULL 
		BEGIN
			-- Term does not match, so need to find a new one
			
			SELECT @OldTermKey = Term_Key
			FROM Concept
			WHERE Concept_Key=@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- Is there an existing match?
			SELECT @TermKey=Term_Key
			FROM Term
			WHERE Plaintext=@Term AND Language_Key=@LanguageKey
			
			-- No matching term record, so create one
			IF @TermKey IS NULL
			BEGIN
				EXEC usp_Term_Insert @TermKey OUTPUT, @LanguageKey, @Term, @Term, @SessionID, @SystemSuppliedData
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			
			-- Now update the synonym concept
			UPDATE Concept 
			SET Term_Key=@TermKey, Changed_Session_ID=@SessionID
			WHERE Concept_Key=@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit	

			-- Remove the old term if no longer required
			DELETE T
			FROM Term T
			LEFT JOIN Concept C ON  C.Term_Key=T.Term_Key
				AND C.Concept_Key<>@ConceptKey
			LEFT JOIN Term_Version TV ON TV.Term_Key=T.Term_Key
			WHERE C.Concept_Key IS NULL
			AND T.System_Supplied_Data = 0
			AND T.Term_Key=@OldTermKey
			AND TV.Term_Version_Key IS NULL
			IF @@Error <> 0 GOTO RollbackAndExit			

		END	

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSynonym_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSynonym_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSynonym_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [Dev - JNCC SQL]
END
GO



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
    $Revision: 2 $
    $Date: 15/08/06 13:57 $
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


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypes_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_RecordTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Record Types

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 15/08/06 13:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RecordTypes_Select]
AS
	SELECT		Record_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Record_Type
	ORDER BY 	Short_Name 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RecordTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [Dev - JNCC SQL]
END
GO