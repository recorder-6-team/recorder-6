SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Delete_ForExportFilter') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ExportFilterTag_Delete_ForExportFilter]
GO

/*===========================================================================*\
  Description:	Remove a given tag from all its associated surveys.

  Parameters:
	@ConceptKey

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ExportFilterTag_Delete_ForExportFilter]
	@Key CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Export_Filter_Tag
	WHERE  Export_Filter_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Delete_ForExportFilter') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ExportFilterTag_Delete_ForExportFilter'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ExportFilterTag_Insert]
GO

/*===========================================================================*\
  Description:	Create a link between a concept and export filter.

  Parameters:
	@ExportFilterKey
	@ConceptKey

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ExportFilterTag_Insert]
	@ExportFilterKey 	CHAR(16),
	@ConceptKey 		CHAR(16)
AS
	SET NOCOUNT OFF

	INSERT INTO Export_Filter_Tag (Export_Filter_Key, Concept_Key)
	VALUES (@ExportFilterKey, @ConceptKey)

	IF @@Error <> 0
		RAISERROR ('usp_ExportFilterTag_Insert failed', 16, 1)
	
	RETURN 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ExportFilterTag_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForExportFilter') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForExportFilter]
GO

/*===========================================================================*\
  Description:	Returns all Surveys for the given export filter key.

  Parameters:

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForExportFilter]
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT		SU.Survey_Key,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name
	FROM		Survey					SU	
	JOIN		Export_Filter_Survey	EFS	ON	EFS.Survey_Key	=	SU.Survey_Key
	JOIN		Name					N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual				I	ON	I.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	0
	LEFT JOIN	Organisation 			O 	ON	O.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	1
	WHERE		EFS.Export_Filter_Key 	=	@Key
	ORDER BY	SU.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForExportFilter') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForExportFilter'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForExportFilter TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTagExport]
GO

/*===========================================================================*\
  Description:	Returns all Surveys for a survey tag concept and its children.

  Parameters:
	@Key	Concept Key of tag

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Surveys_Select_ForSurveyTagExport
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	-- Doing this once is enough.
	DECLARE	@HierarchyRelationTypeKey CHAR(16)
	SELECT	@HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	C.Concept_Key	=	@Key

	-- Need somewhere to put all those keys.
	DECLARE	@Concepts TABLE (ConceptKey CHAR(16) COLLATE Database_Default)

	INSERT INTO @Concepts VALUES (@Key)

	-- Gather all nested concepts.
	WHILE @@RowCount > 0
		INSERT INTO @Concepts
		SELECT		To_Concept_Key
		FROM		Concept_Relation
		JOIN		@Concepts		ON	ConceptKey = From_Concept_Key
		WHERE		To_Concept_Key	NOT IN (SELECT ConceptKey FROM @Concepts)
		AND			Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey

	-- Return all relevant survey keys.
	SELECT 	DISTINCT Survey_Key
	FROM	Survey_Tag
	JOIN	@Concepts	ON	ConceptKey = Concept_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForSurveyTagExport'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForExportFilter') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select_ForExportFilter]
GO

/*===========================================================================*\
  Description:	Retrieve the tags for an export filter

  Parameters:
	@Key		Export Filter Key

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select_ForExportFilter]
	@Key CHAR(16)
AS
	SELECT 	DISTINCT 
			CT.Concept_Key, 
			CT.Plaintext
	FROM 	Survey_Tag			ST
	JOIN 	VW_ConceptTerm 		CT 	ON 	CT.Concept_Key	=	ST.Concept_Key
	JOIN	Export_Filter_Tag	EFT	ON	EFT.Concept_Key	=	ST.Concept_Key
	WHERE	EFT.Export_Filter_Key	=	@Key
	AND		CT.List_Preferred 	= 	1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForExportFilter') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select_ForExportFilter'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END	
		ELSE BEGIN
			DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			WHERE	Thesaurus_Fact_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		(@Timestamp = Timestamp OR @Timestamp IS NULL)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Delete') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ThesaurusFact_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

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
    $Revision: 2 $
    $Date: 9/04/08 16:48 $
    $Author: Ericsalmon $

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

		IF @@Error <> 0 GOTO RollbackAndExit

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

