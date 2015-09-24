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
    $Revision: 8 $
    $Date: 2/02/09 17:59 $
    $Author: Pauldavies $

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
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Relation WHERE Concept_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

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