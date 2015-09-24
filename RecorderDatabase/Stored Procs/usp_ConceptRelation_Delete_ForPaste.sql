/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete_ForPaste]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete_ForPaste]
GO

/*===========================================================================*\
  Description:	A From key and a To key are passed in, and the corresponding
		Concept_Relation record is deleted.

  Parameters:	@FromKey
				@ToKey
				@RecordsAffected

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 12/01/04 10:00 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete_ForPaste]
	@FromKey char(16),
	@ToKey char(16),
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Make required changes to Concept_Lineage
		\*-------------------------------------------------------------*/
		DECLARE		@relation_type_key		CHAR(16)
		
		DECLARE		types						CURSOR LOCAL FAST_FORWARD FOR
		SELECT		Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		From_Concept_Key			=	@FromKey
		AND			To_Concept_Key				=	@ToKey

		OPEN		types

		WHILE 1 = 1
		BEGIN
			FETCH		types
			INTO		@relation_type_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_RelationDeleted	@FromKey,
															@ToKey,
															@relation_type_key
			IF @@ERROR <> 0 GOTO CloseRollbackAndExit
		END

		CLOSE		types

		/*-------------------------------------------------------------*\
		  Delete record in Concept_Relation.
		\*-------------------------------------------------------------*/
		DECLARE		@error		INT

		DELETE		Concept_Relation
		WHERE		From_Concept_Key	=	@FromKey
		AND			To_Concept_Key		=	@ToKey

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION
	RETURN 0

CloseRollBackAndExit:
	CLOSE		types

RollBackAndExit:
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Delete_ForPaste failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete_ForPaste') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete_ForPaste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [Dev - JNCC SQL]
END

GO
