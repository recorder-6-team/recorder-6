/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_NewRelation]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_NewRelation]
GO

/*===========================================================================*\
  Description:	Create concept lineage records as required for a new concept
				relationship.  Called from usp_ConceptLineage_Insert.

  Parameters:	@concept_relation_key	Concept relation key

  Created:		Jan 2004

  Last revision information:
	$Revision: 3 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_NewRelation]
	@concept_relation_key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@parent_key				CHAR(16),
				@child_key				CHAR(16),
				@relation_type_key		CHAR(16),
				@concept_group_key		CHAR(16),
				@child_lineage			VARCHAR(900),
				@parent_lineage			VARCHAR(900)

	SELECT		@parent_key				=	From_Concept_Key,
				@child_key				=	To_Concept_Key,
				@relation_type_key		=	Thesaurus_Relation_Type_Key
	FROM		Concept_Relation
	WHERE		Concept_Relation_Key	=	@concept_relation_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept relationship does not exist', 16, 1)
		RETURN
	END

	/* do nothing unless relationship should appear in the lineage */
	IF dbo.ufn_ConceptRelationAffectsLineage(
				@parent_key,
				@child_key,
				@relation_type_key) = 1
	BEGIN
		BEGIN TRANSACTION

		/* if the child has no other parents, remove its existing lineage */
		SELECT		@concept_group_key	=	c.Concept_Group_Key,
					@child_lineage		=	l.Lineage
		FROM		Concept				AS	c
		INNER JOIN	Concept_Lineage		AS	l
		ON			l.Concept_Key		=	c.Concept_Key
		WHERE		c.Concept_Key		=	@child_key

		IF @@ROWCOUNT = 1 AND CHARINDEX('\', @child_lineage) = 0
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteSubTree	@concept_group_key,
															@child_lineage
			IF @@ERROR <> 0 GOTO fail
		END

		/* for each lineage record of the parent, create lineage records for
		 * the child and its descendants
		 */ 
		DECLARE		lineage				CURSOR LOCAL FAST_FORWARD FOR
		SELECT		Lineage
		FROM		Concept_Lineage
		WHERE		Concept_Key			=	@parent_key

		OPEN		lineage

		WHILE 1 = 1
		BEGIN
			FETCH		lineage
			INTO		@parent_lineage

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_CreateSubtree	@child_key,
															@parent_lineage
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE 		lineage

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		lineage

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_NewRelation failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_NewRelation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_NewRelation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [Dev - JNCC SQL]
END
GO