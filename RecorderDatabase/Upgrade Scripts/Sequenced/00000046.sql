SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the ConceptRelation table.

  Parameters:	@Key	Concept_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information: 
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Remove Concept_Lineage records.
		\*-------------------------------------------------------------*/
		DECLARE		@from_concept_key	CHAR(16),
					@to_concept_key		CHAR(16),
					@relation_type_key	CHAR(16),
					@error				INT,
					@RecordsAffected	INT

		SELECT		@from_concept_key		=	From_Concept_Key,
					@to_concept_key			=	To_Concept_Key,
					@relation_type_key		=	Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		Concept_Relation_Key	=	@Key

		IF @@ROWCOUNT > 0
		BEGIN
			EXECUTE		usp_ConceptLineage_RelationDeleted	@from_concept_key,
															@to_concept_key,
															@relation_type_key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update in Concept_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Concept_Relation
		WHERE	Concept_Relation_Key = @Key	
		AND	@Timestamp = Timestamp
		
		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Relation_Key FROM Concept_Relation WHERE Concept_Relation_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Delete failed', 16, 1)
GO	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Relation table

  Parameters:	@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Insert]
	@Key char(16) OUTPUT,
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16) = NULL,
	@Multiplicity float = NULL,
	@Inherited bit = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @ParentRelationTypeKey CHAR(16)

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT
	IF @@ERROR <> 0 GOTO RollBackAndExit

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  If the app. doesn't pass in a @ThesaurusRelationTypeKey then
		  the Hierarchy_Relation_Type_Key for the Concept Group of the
		  parent is used.
		\*-------------------------------------------------------------*/		
		SELECT 		@ParentRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM		Concept_Group AS CG
		INNER JOIN	Concept AS C ON C.Concept_Group_Key = CG.Concept_Group_Key
		WHERE		C.Concept_Key = @FromConceptKey

		IF @ThesaurusRelationTypeKey IS NULL 
			SET @ThesaurusRelationTypeKey = @ParentRelationTypeKey

		/*-------------------------------------------------------------*\
			Validate to ensure we are not trying to create a cycle in the 
			concept group hierarchy
		\*-------------------------------------------------------------*/				
		IF @ThesaurusRelationTypeKey = @ParentRelationTypeKey
		BEGIN
			DECLARE @Check	BIT
			EXECUTE	usp_Concept_RecursionCheck_Get	@ToConceptKey, @FromConceptKey, @Check OUTPUT

			IF @Check = 1
				RAISERROR ('Cyclical relationship', 16, 1)

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Insert in Concept_Relation.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Inherited,
			Multiplicity,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@FromConceptKey,
			@ToConceptKey,
			@ThesaurusRelationTypeKey,
			IsNull(@Inherited, 0),
			@Multiplicity,
			@Comment,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Make corresponding changes to lineage table if this is a 
			parent relationship
		\*-------------------------------------------------------------*/
		IF @ThesaurusRelationTypeKey=@ParentRelationTypeKey
		BEGIN
			EXECUTE		usp_ConceptLineage_NewRelation	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit:
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [Dev - JNCC SQL]
END

GO
			

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key
		AND	@Timestamp = Timestamp

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.  If @DeleteUnlinkedSynonyms
		is 1, then removes any non-list preferred concepts from the 
		same concept group.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@DeleteUnlinkedSynonyms bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp,
			@error				INT,
			@RecordsAffected	INT

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			

		/*====================================*\
		  Delete the synonyms that are no longer
		  required.
		\*====================================*/
		IF @DeleteUnlinkedSynonyms=1
		BEGIN
			DECLARE @SynConceptKey CHAR(16)
			
			DECLARE csr CURSOR FOR
				SELECT CSyn.Concept_Key
				FROM Concept C
				INNER JOIN Concept CSyn 
					ON CSyn.Meaning_Key=C.Meaning_Key
					AND CSyn.Concept_Group_Key=C.Concept_Group_Key
					AND CSyn.List_Preferred=0
					AND C.Concept_Key=@Key
			
			OPEN csr
			WHILE (1=1)
			BEGIN
				FETCH NEXT FROM csr INTO @SynConceptKey

				IF @@FETCH_STATUS <> 0
					BREAK

				-- Recurse to remove synonym concepts
				EXEC usp_Concept_Delete @SynConceptKey
			END
			CLOSE csr
			DEALLOCATE csr
		END
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(@Timestamp = @OriginalTimestamp OR @Timestamp IS NULL)

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_Paste]
GO

/*===========================================================================*\
  Description:	Pastes a concept from one position to another

  Parameters:	@DestConceptKey CHAR(16) - output param = key of newly pasted concept

  Created:	Aug 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey 			CHAR(16),
	@DestConceptGroupKey 	CHAR(16),
	@DestParentConceptKey 	CHAR(16),
	@IsCut 					BIT,
	@SessionID 				CHAR(16),
	@SystemSuppliedData 	BIT			=	0,
	@DestConceptKey 		CHAR(16) 	OUTPUT
AS

BEGIN TRANSACTION
	/*-------------------------------------------------------------*\
		Prepare things for the operation
	\*-------------------------------------------------------------*/

	--Enforce a value in @SystemSuppliedData as the default value 
	--doesn't seem to work every time
	IF @SystemSuppliedData IS NULL
		SET @SystemSuppliedData=0

	DECLARE @SrcConceptGroupKey CHAR(16),
			@Lineage 			VARCHAR(900),
			@OldRelationTypeKey CHAR(16),
			@NewRelationTypeKey CHAR(16),
			@Key 				CHAR(16),
			@DummyKey			CHAR(16),
			@RelationshipAdded	BIT

	SET		@RelationshipAdded	=	0

	SELECT 	@SrcConceptGroupKey = Concept_Group_Key
	FROM 	Concept
	WHERE 	Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the source concept group's hierarchy relationship
	SELECT 	@OldRelationTypeKey = CG.Hierarchy_Relation_Type_Key
	FROM 	Concept C
	JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	WHERE 	C.Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the dest concept group's hierarchy relationship
	IF @DestParentConceptKey IS NULL 
		SET @NewRelationTypeKey=@OldRelationTypeKey
	ELSE
	BEGIN
		SELECT	@NewRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM 	Concept C
		JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		WHERE 	C.Concept_Key = @DestParentConceptKey
	END

	/*-------------------------------------------------------------*\
		Perform the cut operation
	\*-------------------------------------------------------------*/
	IF @IsCut = 1 
	BEGIN
		DECLARE @OldKey CHAR(16)

		SET @DestConceptKey = @ConceptKey
	
		--Prepare to delete subtree of lineage
		SELECT 	@Lineage = Lineage
		FROM 	Concept_Lineage
		WHERE 	Concept_Key = @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	
		IF @DestParentConceptKey IS NULL
		BEGIN
			--Delete source's parent relationship(s)
			DECLARE @KeyToDel 	CHAR(16)
			DECLARE @Timestamp 	TIMESTAMP
	
			DECLARE csr CURSOR STATIC LOCAL FOR
				SELECT 	Concept_Relation_Key, Timestamp
				FROM 	Concept_Relation 
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
	
			OPEN csr
			
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp
			WHILE @@FETCH_STATUS = 0
			BEGIN
				EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
				IF @@Error <> 0 GOTO RollbackAndExit
				FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp
			END
		END
		ELSE
		BEGIN
			--Update source's parent relationship to point to new parent key
			IF EXISTS(	SELECT 	1 
						FROM 	Concept_Relation 
						WHERE 	To_Concept_Key				=	@ConceptKey
						AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey)
			BEGIN
				SELECT 	@OldKey	=	From_Concept_Key, 
						@Key	=	Concept_Relation_Key
				FROM 	Concept_Relation
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
	
				UPDATE 	Concept_Relation 
				SET 	From_Concept_Key			=	@DestParentConceptKey,
						Changed_Session_ID			=	@SessionID,
						Thesaurus_Relation_Type_Key	=	@NewRelationTypeKey
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
	
				EXECUTE	usp_ConceptLineage_UpdateRelation	
						@Key,
						@OldKey,
						@ConceptKey,
						@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			ELSE
			BEGIN
				EXECUTE	usp_ConceptRelation_Insert
						@DummyKey,
						@DestParentConceptKey,
						@ConceptKey,
						@OldRelationTypeKey,
						NULL,
						NULL,
						NULL,
						@SessionID, 
						@SystemSuppliedData
				IF @@Error <> 0 GOTO RollbackAndExit

				SET	@RelationshipAdded = 1
			END
		END
	
		IF @SrcConceptGroupKey <> @DestConceptGroupKey 
		BEGIN
			-- Update concept group for source concepts to new group, but not the synonyms, hence using ChildrenOnly.
			UPDATE 	CChild
			SET 	Concept_Group_Key = @DestConceptGroupKey
			FROM 	VW_ConceptChildrenOnly 	CC 
			JOIN 	Concept 				CChild 	ON 	CChild.Concept_Key			=	CC.Child_Concept_Key
													AND CChild.Concept_Group_Key	=	@SrcConceptGroupKey
			WHERE 	CC.Parent_Concept_Key	=	@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- And now do the Concept.
			UPDATE	Concept
			SET		Concept_Group_Key	=	@DestConceptGroupKey
			WHERE	Concept_Key			= 	@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- Cut/Paste to top level causes usp_ConceptRelation_Delete to create a lineage for orphaned concept.
			-- And it causes extra records in ConceptLineage that are no good. Lineage will be properly recreated further down anyway.
			EXECUTE usp_ConceptLineage_DeleteConcept @ConceptKey
		END
	
		-- Actually delete the old lineage information
		IF @OldKey <> @DestParentConceptKey
		BEGIN
			EXECUTE usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END
	ELSE
	/*-------------------------------------------------------------*\
		or copy operation
	\*-------------------------------------------------------------*/
	BEGIN
		--Whole branch being copied into a the concept group, so find all concepts and clone them
		DECLARE @ChildConceptKey CHAR(16)
	
		--Create a local table to hold key mappings
		DECLARE @ConceptMapping TABLE (
			Src_Concept_Key 	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
			Dest_Concept_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		--Clone the source concepts, updating concept group key
		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT 	CChild.Concept_Key
			FROM 	VW_ConceptChildrenOnly 	CC 
			JOIN 	Concept 				CChild	ON 	CChild.Concept_Key 			= CC.Child_Concept_Key
													AND CChild.Concept_Group_Key 	= @SrcConceptGroupKey
			WHERE 	CC.Parent_Concept_Key 	= @ConceptKey
			-- Add the copied concept, instead of duplicating code to clone it separatly
			UNION
			SELECT	@ConceptKey
	
		OPEN csr
		FETCH NEXT FROM csr INTO @ChildConceptKey
		WHILE @@FETCH_STATUS = 0
		BEGIN
			EXECUTE spNextKey 'Concept', @Key OUTPUT
			IF @@Error <> 0 GOTO RollBackAndExit
			
			-- When cloning the actual selected concept, remember the new concept key
			IF @ChildConceptKey = @ConceptKey 
				SET @DestConceptKey = @Key
	
			-- Rememer mappings so we can update relationships later
			INSERT INTO @ConceptMapping VALUES (@ChildConceptKey, @Key)
			IF @@Error <> 0 GOTO RollBackAndExit
	
			-- Clone the concept
			INSERT INTO Concept (
				Concept_Key, Term_Key, Concept_Group_Key, Term_Version_Key, List_Preferred, 
				Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
				Author_Copy, Sort_Code, List_Code, Entered_Session_ID, System_Supplied_Data, Custodian
			)
			SELECT 	@Key, Term_Key, @DestConceptGroupKey, Term_Version_Key, List_Preferred, 
				Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
				Author_Copy, Sort_Code, List_Code, @SessionID, @SystemSuppliedData, LEFT(@Key, 8)
			FROM 	Concept 
			WHERE 	Concept_Key = @ChildConceptKey
			IF @@Error <> 0 GOTO RollBackAndExit
	
			FETCH NEXT FROM csr INTO @ChildConceptKey
		END
	
		CLOSE csr
		DEALLOCATE csr
	
		/*-------------------------------------------------------------*\
			Clone the hierarchical relationships within the copied branch
				of concepts
		\*-------------------------------------------------------------*/
		DECLARE @SrcKey		CHAR(16), 
				@DestKey	CHAR(16)
	
		--Declare a temp table with same structure as concept relation that 
		--we can populate with dummy primary keys, then update later
		SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
		IF @@Error <> 0 GOTO RollbackAndExit
	
		DECLARE cmap CURSOR STATIC LOCAL FOR
			--Note we are cloning parent relationships within the branch, so 
			--exclude the top node
			SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key <> @DestConceptKey
		
		OPEN cmap
		
		FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
		WHILE @@FETCH_STATUS=0
		BEGIN
			INSERT INTO #TempRel (
				Concept_Relation_Key,
				From_Concept_Key,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian
			)
			SELECT 		CR.Concept_Relation_Key, -- Will be replaced later
						ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
						@DestKey,
						Thesaurus_Relation_Type_Key,
						Multiplicity,
						Inherited,
						Comment,
						@SessionID,
						@SystemSuppliedData,
						LEFT(@DestKey, 8)
			FROM 		Concept_Relation 	CR
			LEFT JOIN 	@ConceptMapping 	CM 	ON 	CM.Src_Concept_Key	=	CR.From_Concept_Key
			WHERE 		CR.To_Concept_Key				=	@SrcKey
			AND 		CR.Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey

			IF @@Error <> 0 GOTO RollbackAndExit
	
			FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
		END
		
		CLOSE cmap
		DEALLOCATE cmap 
	
		--Now we have a table of concept relationships to insert, but we must update the keys first
		DECLARE crel CURSOR LOCAL FOR
			SELECT Concept_Relation_Key FROM #TempRel
		
		OPEN crel
		
		FETCH NEXT FROM crel INTO @SrcKey
		
		WHILE @@FETCH_STATUS=0
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @DestKey OUTPUT
			IF @@Error <> 0 GOTO RollbackAndExit
			
			UPDATE 	#TempRel
			SET 	Concept_Relation_Key = @DestKey
			WHERE CURRENT OF crel

			IF @@Error <> 0 GOTO RollbackAndExit
	
			FETCH NEXT FROM crel INTO @SrcKey		
		END
	
		CLOSE crel
		DEALLOCATE crel
	
		--Copy the relationships into the concept relation table
		INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		)
		SELECT 	Concept_Relation_Key,
				From_Concept_Key,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian 
		FROM 	#TempRel
		IF @@Error <> 0 GOTO RollbackAndExit
	
		DROP TABLE #TempRel
	END

	/*-------------------------------------------------------------*\
	 Join the copied branch of concepts to the destination concept.
	 This also fixes up the lineage.
	\*-------------------------------------------------------------*/
	IF 	(@DestParentConceptKey IS NOT NULL) 
	AND ((@SrcConceptGroupKey <> @DestConceptGroupKey) OR (@IsCut = 0)) 
	AND (@RelationshipAdded = 0)	-- If already added (see cut handling), don't do it again.
	BEGIN
		EXECUTE usp_ConceptRelation_Insert
				@DummyKey,
				@DestParentConceptKey,
				@DestConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit
	END
	ELSE 
	BEGIN
		IF @DestParentConceptKey IS NULL
			EXECUTE usp_ConceptLineage_CreateSubtree @DestConceptKey, ''
	END

	COMMIT TRANSACTION

	RETURN

RollBackAndExit: 
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_Paste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
	@PotentialChildKey 	CHAR(16),
	@PotentialParentKey CHAR(16),
	@RecursionExists 	BIT OUTPUT
AS
    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET	@RecursionExists    =   1
    ELSE
    BEGIN
        SELECT 		@RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM 		Concept_Lineage LC
		JOIN		Concept			CC 	ON	CC.Concept_Key			=	LC.Concept_Key
        CROSS JOIN 	Concept_Lineage LP
		JOIN		Concept			CP	ON	CP.Concept_Key			= 	LP.Concept_Key
										AND	CP.Concept_Group_Key	= 	CC.Concept_Group_Key
        WHERE 		LC.Concept_Key	=	@PotentialChildKey
        AND 		LP.Concept_Key 	= 	@PotentialParentKey

        IF @RecursionExists IS NULL
            SET	@RecursionExists    =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_RecursionCheck_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_RecursionCheck_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Species]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Species]
	@ChecklistKey char(16)
AS
	IF @ChecklistKey IS NOT NULL BEGIN
		-- Update existing items, if they were "rematched"
		UPDATE	IW_Matched_Species
		SET	Matched_Key = Match_Key
		FROM	#Species
		WHERE	Matched_Value = Import_Value
		AND	Match_Checklist_Key = Checklist_Key
		AND	Match_Key IS NOT NULL
		AND	Manual_Match = 1

		-- Add the new ones now.
		INSERT INTO 	IW_Matched_Species
		SELECT		Import_Value, Match_Key, Checklist_Key
		FROM		#Species
		LEFT JOIN	IW_Matched_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key = Checklist_Key
		WHERE		Matched_Value IS NULL
		AND		Match_Key IS NOT NULL
		AND		Manual_Match = 1
	END ELSE BEGIN
		-- Update existing items, if they were "rematched"
		UPDATE	IW_Matched_Species
		SET	Matched_Key = Match_Key
		FROM	#Species
		WHERE	Matched_Value = Import_Value
		AND	Match_Checklist_Key IS NULL
		AND	Match_Key IS NOT NULL
		AND	Manual_Match = 1

		-- Add the new ones now.
		INSERT INTO 	IW_Matched_Species
		SELECT		Import_Value, Match_Key, @ChecklistKey
		FROM		#Species
		LEFT JOIN	IW_Matched_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key IS NULL
		WHERE		Matched_Value IS NULL
		AND		Match_Key IS NOT NULL
		AND		Manual_Match = 1
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
	@ChecklistKey char(16)
AS
	IF @ChecklistKey IS NOT NULL BEGIN
		-- Grab checklist name frist. Avoid joins on update query.
		DECLARE	@Checklist varchar(100)

		SELECT	@Checklist = Item_Name
		FROM	Taxon_List
		WHERE	Taxon_List_Key = @ChecklistKey

		-- Update temp table with relevant data.
		UPDATE 	#Species
		SET 	Match_Count = 1, 
			Match_Key = Matched_Key, 
			Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
			Remembered = 1,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		FROM 	IW_Matched_Species
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = Matched_Key
		WHERE 	Import_Value = Matched_Value 
		AND 	Match_Key IS NULL
		AND		Match_Checklist_Key = @ChecklistKey
	END ELSE BEGIN
		UPDATE 	#Species
		SET 	Match_Count = 1, 
			Match_Key = Matched_Key, 
			Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
			Remembered = 1,
			Checklist = TL.Item_Name,
			Checklist_Key = @ChecklistKey
		FROM 	IW_Matched_Species
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = Matched_Key
		JOIN	Taxon_List_Version TLV ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TLV.Taxon_List_Key = TL.Taxon_List_Key
		WHERE 	Import_Value = Matched_Value 
		AND 	Match_Key IS NULL
		AND		Match_Checklist_Key IS NULL
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#Species
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	-- Set Match_Count first. Broken down in two separate updates for speed.
	UPDATE	UpdatedSpecies
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name 
				JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
				WHERE	(TLV.Taxon_List_Key = @ChecklistKey
				OR ((TLI.Preferred_Name = TLI.Taxon_List_Item_Key OR ITN.Preferred_List = 1)
				AND @ChecklistKey IS NULL))
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				AND TLI.Taxon_List_Version_To IS NULL
				AND TLV.Version >= (SELECT MAX(Version)
								FROM Taxon_List_Version 
          						WHERE Taxon_List_Key = TLV.Taxon_List_Key
								AND Version_Is_Amendment = 0))
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL

	UPDATE	UpdatedSpecies
	SET	Match_Count =  Match_Count + (
				SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name + ' ' + ITN.Authority
				JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
				WHERE	(TLV.Taxon_List_Key = @ChecklistKey
				OR ((TLI.Preferred_Name = TLI.Taxon_List_Item_Key OR ITN.Preferred_List = 1)
				AND @ChecklistKey IS NULL))
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				AND TLI.Taxon_List_Version_To IS NULL
				AND TLV.Version >= (SELECT MAX(Version)
								FROM Taxon_List_Version 
          						WHERE Taxon_List_Key = TLV.Taxon_List_Key
								AND Version_Is_Amendment = 0))
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL


	-- Now get values and keys for unique matches only. Broken down in tow separate updates for speed.
	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	(TL.Taxon_List_Key = @ChecklistKey
	OR (@ChecklistKey IS NULL AND (TLI.Preferred_Name = TLI.Taxon_List_Item_Key OR ITN.Preferred_List = 1)))
	AND	Species_Name = Actual_Name
	AND TLI.Taxon_List_Version_To IS NULL

	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key	
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	(TL.Taxon_List_Key = @ChecklistKey
	OR (@ChecklistKey IS NULL AND (TLI.Preferred_Name = TLI.Taxon_List_Item_Key OR ITN.Preferred_List = 1)))
	AND	Species_Name = Actual_Name + ' ' + ITN.Authority
	AND TLI.Taxon_List_Version_To IS NULL

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	S
	SET	[Order] = ITN.Actual_Name
	FROM	#Species	S
	JOIN	Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
	JOIN	Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	JOIN	Taxon_List_Item TLI 
		ON	ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
	JOIN	Index_Taxon_Name ITN
		ON	ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
	WHERE Match_Count = 1
	AND	[Order] IS NULL

	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	UPDATE	#Species
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchKey
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
	@SearchKey char(16),
	@SearchText varchar(100)

AS
	SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
		CASE Actual_Name_Italic
			WHEN 1 THEN '<i>' + Actual_Name + '</i>'
			ELSE Actual_Name
		END + ' ' + ISNULL(ITN.Authority, '') AS DisplayTerm, 
		Actual_Name + ' ' + ISNULL(ITN.Authority, '') AS SearchTerm
	FROM	Index_Taxon_Name ITN
	JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(Actual_Name LIKE @SearchText + '%'
	OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
	OR 	ITN.Authority LIKE @SearchText + '%')
	AND	TLI.Taxon_List_Version_To IS NULL
	AND TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchKey
		@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 3 $
    $Date: 23/01/08 12:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	DECLARE @SpacePos INT
	SET @SearchText = LTRIM(RTRIM(@SearchText))
	SET @SpacePos = CHARINDEX(' ', @SearchText)
	IF @SpacePos > 0
	BEGIN
		IF @SpacePos = CHARINDEX(' .', @SearchText)
		BEGIN
			SET @SearchText = STUFF(@SearchText, @SpacePos, 2, '% %')
		END
		ELSE
		BEGIN
			SET @SearchText = STUFF(@SearchText, @SpacePos, 1, '% ') + '%'
		END
	END
	ELSE
	BEGIN
		SET @SearchText = @SearchText + '%'
	END

	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText
		OR 	ITN.Authority LIKE @SearchText)
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText
		OR 	ITN.Authority LIKE @SearchText)
		AND	TLI.Taxon_List_Version_To IS NULL
		AND TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [Dev - JNCC SQL]
END
GO

