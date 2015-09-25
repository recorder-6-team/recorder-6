/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_CreateForOrphans]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_CreateForOrphans]
GO

/*===========================================================================*\
  Description:	Generate concept lineage for orphaned concepts in a specified
				concept group.

  Parameters:	@concept_group_key		Concept group key

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_CreateForOrphans]
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@relation_type_key	CHAR(16),
				@concept_key		CHAR(16)

	/* determine whether the group has a hierarchical relation */
	SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key	=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
		GOTO fail
	END

	IF @relation_type_key IS NOT NULL
	BEGIN
		BEGIN TRANSACTION

		DECLARE		orphans					CURSOR LOCAL FAST_FORWARD FOR
		SELECT		c.Concept_Key
		FROM		Concept					AS	c
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.List_Preferred		=	1
		AND			NOT EXISTS (
						SELECT		1
						FROM		Concept_Lineage
						WHERE		Concept_Key			=	c.Concept_Key)
		AND			NOT EXISTS (
						SELECT		1
						FROM		Concept_Relation				AS	r
						INNER JOIN	Concept							AS	p
						ON			p.Concept_Key					=	r.From_Concept_Key
						WHERE		r.To_Concept_Key				=	c.Concept_Key
						AND			r.Thesaurus_Relation_Type_Key	=	@relation_type_key
						AND			p.Concept_Group_Key				=	@concept_group_key
						AND			p.List_Preferred				=	1)

		OPEN		orphans

		WHILE 1 = 1
		BEGIN
			FETCH		orphans
			INTO		@concept_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
			IF @@ERROR <> 0 GOTO fail_from_cursor	
		END

		CLOSE		orphans

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		orphans

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_CreateForOrphans failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_CreateForOrphans') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_CreateForOrphans'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [Dev - JNCC SQL]
END
GO

