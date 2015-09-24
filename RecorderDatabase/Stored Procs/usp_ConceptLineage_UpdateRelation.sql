/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_UpdateRelation]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_UpdateRelation]
GO

/*===========================================================================*\
  Description:	Make changes to lineage corresponding to a change in a
				concept relationship.

  Parameters:	@concept_relation_key	Concept relation key
				@old_from_concept_key	Original source concept key
				@old_to_concept_key		Original destination concept key
				@old_type_key			Original relation type key

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_UpdateRelation]
	@concept_relation_key		CHAR(16),
	@old_from_concept_key		CHAR(16),
	@old_to_concept_key			CHAR(16),
	@old_type_key				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@new_type_key			CHAR(16),
				@new_from_concept_key	CHAR(16),
				@new_to_concept_key		CHAR(16)

	SELECT		@new_type_key			=	Thesaurus_Relation_Type_Key,
				@new_from_concept_key	=	From_Concept_Key,
				@new_to_concept_key		=	To_Concept_Key
	FROM		Concept_Relation
	WHERE		Concept_Relation_Key	=	@concept_relation_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept relationship does not exist', 16, 1)
		RETURN
	END

	IF @old_type_key <> @new_type_key
		OR @old_from_concept_key <> @new_from_concept_key
		OR @old_to_concept_key <> @new_to_concept_key
	BEGIN
		IF dbo.ufn_ConceptRelationAffectsLineage(
					@old_from_concept_key,
					@old_to_concept_key,
					@old_type_key) = 1
		BEGIN
			/* remove old lineage */
			EXECUTE		usp_ConceptLineage_RelationDeleted	@old_from_concept_key,
															@old_to_concept_key,
															@old_type_key
			IF @@ERROR <> 0 GOTO fail
		END

		IF dbo.ufn_ConceptRelationAffectsLineage(
					@new_from_concept_key,
					@new_to_concept_key,
					@new_type_key) = 1
		BEGIN
			/* create new lineage */
			EXECUTE		usp_ConceptLineage_NewRelation	@concept_relation_key
			IF @@ERROR <> 0 GOTO fail
		END
    END
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_UpdateRelation failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_UpdateRelation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_UpdateRelation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [Dev - JNCC SQL]
END
GO