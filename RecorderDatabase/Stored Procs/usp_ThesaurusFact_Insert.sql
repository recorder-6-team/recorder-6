/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Thesaurus_Fact table, for 
                Collections Module or Recorder.

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 13/02/08 9:09 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
	@Key char(16) OUTPUT,
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
	@SystemSuppliedData bit = NULL
	
AS


	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Thesaurus_Fact', @Key OUTPUT

	DECLARE @FactTypeConceptKey char(16)

	/*-------------------------------------------------------------*\
	  There is no transaction in this proc, as the Delphi code
		wraps the whole lot in a transaction.
	\*-------------------------------------------------------------*/

	-- The combo box stores the meaning key and the item name. This meaning key
	-- needs to be converted to a concept key. Because many concepts can
	-- share the same meaning key, we have to use the meaning key and the item name.
	SELECT 		@FactTypeConceptKey = Concept_Key
	FROM 		Concept AS C
	INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
	WHERE 		C.Meaning_Key = @FactTypeMeaningKey
	AND 		T.Item_Name = @FactTypeMeaningName

	/*-------------------------------------------------------------*\
	  Insert in Thesaurus_Fact.
	\*-------------------------------------------------------------*/
	INSERT INTO Thesaurus_Fact (
		Thesaurus_Fact_Key,
		Item_Name,
		Data,
		Meaning_Key,
		Concept_Key,
		Term_Version_Key,
		Related_Term_Versions,
		Inherited,
		Language_Key,
		Fact_Vague_Date_Start,
		Fact_Vague_Date_End,
		Fact_Vague_Date_Type,
		Fact_Type_Concept_Key,
		Entered_Session_ID,
		System_Supplied_Data
	) VALUES (
		@Key,
		@ItemName,
		@Data,
		@MeaningKey,
		@ConceptKey,
		@TermVersionKey,
		@RelatedTermVersions,
		@Inherited,
		@LanguageKey,
		@FactVagueDateStart,
		@FactVagueDateEnd,
		IsNull(@FactVagueDateType, 'U'),
		@FactTypeConceptKey,
		@SessionID,
		IsNull(@SystemSuppliedData, 0)
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [Dev - JNCC SQL]
END

GO