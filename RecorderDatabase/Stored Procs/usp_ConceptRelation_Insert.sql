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
    $Revision: 8 $
    $Date: 26/11/07 12:23 $
    $Author: Ericsalmon $

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
			