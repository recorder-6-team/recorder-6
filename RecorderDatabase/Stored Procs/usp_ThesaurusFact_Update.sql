/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 10:52 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16), 
	@SystemSuppliedData bit = NULL,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FactTypeConceptKey char(16)

		-- The combo box stores the meaning key and the item name. This meaning key
		-- needs to be converted to a concept key. Because many concepts can
		-- share the same meaning key, we have to use the meaning key and the item name.
		SELECT 		@FactTypeConceptKey = Concept_Key
		FROM 		Concept AS C
		INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
		WHERE 		C.Meaning_Key = @FactTypeMeaningKey
		AND 		T.Item_Name = @FactTypeMeaningName

		UPDATE	Thesaurus_Fact
		SET	Item_Name = @ItemName,
			Data = @Data,
			Meaning_Key = @MeaningKey,
			Concept_Key = @ConceptKey,
			Term_Version_Key = @TermVersionKey,
			Related_Term_Versions = @RelatedTermVersions,
			Inherited = @Inherited,
			Language_Key = @LanguageKey,
			Changed_Session_ID = @SessionID,
			Fact_Vague_Date_Start = @FactVagueDateStart,
			Fact_Vague_Date_End = @FactVagueDateEnd,
			Fact_Vague_Date_Type = IsNull(@FactVagueDateType, 'U'),
			Fact_Type_Concept_Key = @FactTypeConceptKey,
			System_Supplied_Data = IsNull(@SystemSuppliedData, 0)
		WHERE	Thesaurus_Fact_Key = @Key
		AND		@Timestamp = Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Fact WHERE Thesaurus_Fact_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION

	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Update') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ThesaurusFact_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [Dev - JNCC SQL]
END
GO