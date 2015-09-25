/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_GenerateForGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
GO

/*===========================================================================*\
  Description:	Generate (or re-generate) concept lineage for a specified
				concept group.

				Either @job_id or @concept_group_key must be supplied.

  Parameters:	@job_id					Job identifier
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 9 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
	@job_id				INT			=	NULL,
	@concept_group_key	CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @hierarchy_relation_type_key	CHAR(16)

	IF @job_id IS NULL
	BEGIN
		IF @concept_group_key IS NULL
		BEGIN
			RAISERROR ('Concept group or Job must be specified', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		/* determine parameters of job */
		SELECT		@concept_group_key		=	Concept_Group_Key
		FROM		Import_Export_Job
		WHERE		Import_Export_Job_ID	=	@job_id

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Job does not exist or has not been configured', 16, 1)
			RETURN
		END

		EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
														'Generating lineage'
		IF @@ERROR <> 0 RETURN
	END

	/* determine whether the group has a hierarchical relation */
	SELECT		@hierarchy_relation_type_key	=	Hierarchy_Relation_Type_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key				=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
		RETURN
	END

	/* remove existing concept lineage records */
	EXECUTE		usp_ConceptLineage_DeleteSubtree	@concept_group_key,
													NULL
	IF @@ERROR <> 0 RETURN

	IF @hierarchy_relation_type_key IS NOT NULL
	BEGIN
		DECLARE		@concept_key	CHAR(16)

		/* create/update concept lineage */
		DECLARE		root_concepts			CURSOR LOCAL STATIC FOR
		SELECT		c.Concept_Key
		FROM		Concept					AS	c
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.List_Preferred		=	1
		AND			NOT EXISTS (
							SELECT		1
							FROM		Concept_Relation				AS	r
							INNER JOIN	Concept							AS	p
							ON			p.Concept_Key					=	r.From_Concept_Key
							WHERE		r.To_Concept_Key				=	c.Concept_Key
							AND			r.Thesaurus_Relation_Type_Key	=	@hierarchy_relation_type_key
							AND			p.Concept_Group_Key				=	@concept_group_key
							AND			p.List_Preferred				=	1)

		OPEN		root_concepts

		WHILE 1 = 1
		BEGIN
			FETCH		root_concepts
			INTO		@concept_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_CreateSubtree	@concept_key,
															NULL,
															@job_id
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE		root_concepts
	END
	RETURN

fail_from_cursor:
	CLOSE		root_concepts

fail:
	RAISERROR ('usp_ConceptLineage_GenerateForGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_GenerateForGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_GenerateForGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [Dev - JNCC SQL]
END
GO