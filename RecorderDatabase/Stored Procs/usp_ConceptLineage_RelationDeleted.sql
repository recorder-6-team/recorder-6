/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_RelationDeleted]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_RelationDeleted]
GO

/*===========================================================================*\
  Description:	Remove concept lineage associated with a concept relationship.

  Parameters:	@concept_relation_key	Concept relation key

  Created:		Jan 2004

  Last revision information:
	$Revision: 3 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_RelationDeleted]
	@from_concept_key		CHAR(16),
	@to_concept_key			CHAR(16),
	@relation_type_key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@child_lineage			VARCHAR(900)

	/* do nothing unless relationship appears in the lineage */
	IF dbo.ufn_ConceptRelationAffectsLineage(
				@from_concept_key,
				@to_concept_key,
				@relation_type_key) = 1
	BEGIN
		BEGIN TRANSACTION

		SELECT		@concept_group_key	=	Concept_Group_Key
		FROM		Concept
		WHERE		Concept_Key			=	@from_concept_key

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Source concept does not exist', 16, 1)
			RETURN
		END

		DECLARE		lineage				CURSOR LOCAL FAST_FORWARD FOR
		SELECT		cl.Lineage
		FROM		Concept_Lineage		AS	pl
		INNER JOIN	Concept_Lineage		AS	cl
		ON			cl.Lineage			LIKE pl.Lineage + '\%'
		WHERE		pl.Concept_Key	   	=	@from_concept_key
		AND         cl.Concept_Key		=	@to_concept_key

		OPEN		lineage

		WHILE 1 = 1
		BEGIN
			FETCH		lineage
			INTO		@child_lineage

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_DeleteSubTree	@concept_group_key,
															@child_lineage
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE		lineage

		/* if child concept is now an orphan, create top-level lineage */
		IF NOT EXISTS (	SELECT		1
						FROM		Concept_Lineage
						WHERE		Concept_Key			=	@to_concept_key )
		BEGIN
			EXECUTE		usp_ConceptLineage_CreateSubTree	@to_concept_key,
															NULL
			IF @@ERROR <> 0 GOTO fail
		END

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		lineage

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_RelationDeleted failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_RelationDeleted') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_RelationDeleted'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [Dev - JNCC SQL]
END
GO