SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_ConceptUpdated]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_ConceptUpdated]
GO

/*===========================================================================*\
  Description:	Make changes to lineage corresponding to a change in a
				concept.

  Parameters:	@concept_key			Concept key
				@old_concept_group_key	Original concept group key
				@old_list_preferred		Original "list preferred" flag

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_ConceptUpdated]
	@concept_key				CHAR(16),
	@old_concept_group_key      CHAR(16),
	@old_list_preferred			BIT
AS
	SET NOCOUNT ON

	DECLARE		@new_concept_group_key		CHAR(16),
				@new_list_preferred			BIT

	SELECT		@new_concept_group_key	=	Concept_Group_Key,
				@new_list_preferred		=	List_Preferred
	FROM		Concept
	WHERE		Concept_Key			   	=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	BEGIN TRANSACTION

	IF @old_concept_group_key <> @new_concept_group_key
		OR (@old_list_preferred = 1 AND @new_list_preferred = 0)
	BEGIN
		/* remove lineage (if any) from original concept group */
		DELETE		dl
		FROM		Concept_Lineage		AS	cl
		INNER JOIN	Concept_Lineage		AS	dl
		ON			dl.Lineage			LIKE cl.Lineage + '\%'
		INNER JOIN	Concept				AS	d
		ON			d.Concept_Key		=	dl.Concept_Key
		WHERE		cl.Concept_Key		=	@concept_key
		AND			d.Concept_Group_Key	=	@old_concept_group_key

		IF @@ERROR <> 0 GOTO fail

		EXECUTE		usp_ConceptLineage_CreateForOrphans	@old_concept_group_key
		IF @@ERROR <> 0 GOTO fail

		DELETE		Concept_Lineage
		WHERE		Concept_Key			=	@concept_key

		IF @@ERROR <> 0 GOTO fail
	END

	IF  @new_list_preferred = 1
		AND (@old_list_preferred = 0
			OR @old_concept_group_key <> @new_concept_group_key)
	BEGIN
		/* create lineage (if required) in new concept group */
		EXECUTE		usp_ConceptLineage_CreateSubTree	@concept_key,
														NULL
		IF @@ERROR <> 0 GOTO fail
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_ConceptUpdated failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_ConceptUpdated') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_ConceptUpdated'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [Dev - JNCC SQL]
END
GO

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
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

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


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_CreateSubtree]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
GO

/*===========================================================================*\
  Description:	Create lineage records beneath the given parent lineage for
				the specified concept and its descendants.

				If the concept is not list preferred or the group has no
				hierarchical relation then no lineage records are created. 

  Parameters:	@concept_key			Concept key
				@parent_lineage			Parent lineage
				@job_id					[optional] Import job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
	@concept_key		CHAR(16),
	@parent_lineage		VARCHAR(900),
	@job_id				INT				=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @list_preferred			BIT,
				@concept_group_key		CHAR(16),
				@relation_type_key		CHAR(16),
				@lineage				VARCHAR(900),
				@child_concept_key		CHAR(16)

	SELECT      @list_preferred			=	c.List_Preferred,
				@concept_group_key		=	c.Concept_Group_Key,
				@relation_type_key		=	g.Hierarchy_Relation_Type_Key
	FROM		Concept					AS	c
	INNER JOIN	Concept_Group			AS	g
	ON			g.Concept_Group_Key		=	c.Concept_Group_Key
	WHERE		c.Concept_Key			=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	IF @list_preferred = 1 AND @relation_type_key IS NOT NULL
	BEGIN
		IF @job_id IS NULL BEGIN TRANSACTION

		DECLARE @pending_lineage TABLE	(
						Concept_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
						Parent_Lineage  VARCHAR(900) COLLATE SQL_Latin1_General_CP1_CI_AS NULL)

		INSERT		@pending_lineage (
					Concept_Key,
					Parent_Lineage)
		VALUES		(@concept_key,
					@parent_lineage)

		WHILE @concept_key IS NOT NULL
		BEGIN
			IF @job_id IS NOT NULL BEGIN TRANSACTION

			/* create lineage for current concept */
			SET			@lineage		=	dbo.ufn_NextChildLineage(
													@parent_lineage,
													@concept_group_key)

			INSERT		Concept_Lineage (
						Concept_Key,
						Lineage_ID,
						Lineage)
			SELECT		@concept_key,
						ISNULL(MAX(Lineage_ID), 0) + 1,
						@lineage
			FROM		Concept_Lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* record lineage sequence number */
			IF @parent_lineage IS NULL
			BEGIN
				UPDATE		Concept_Group
				SET			Last_Sequence_Number	=	@lineage
				WHERE		Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END
			ELSE
			BEGIN
				UPDATE		Concept_Lineage
				SET			Last_Sequence_Number	=	dbo.ufn_LineageSequenceNumber(@lineage)
				FROM		Concept_Lineage			AS	l
				INNER JOIN	Concept					AS	c
				ON			c.Concept_Key			=	l.Concept_Key
				WHERE		l.Lineage				=	@parent_lineage
				AND			c.Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END

			/* remove current concept from pending list */
			DELETE		@pending_lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* add offspring of current concept to pending list */
			INSERT		@pending_lineage (
						Concept_Key,
						Parent_Lineage)
			SELECT		r.To_Concept_Key,
						@lineage
			FROM		Concept_Relation				AS	r
			INNER JOIN Concept AS c	ON c.Concept_Key	=	r.To_Concept_Key
			LEFT JOIN @pending_lineage pl ON pl.Concept_Key=C.Concept_Key
			WHERE       r.From_Concept_Key				=	@concept_key
			AND			r.Thesaurus_Relation_Type_Key	=	@relation_type_key
			AND			c.Concept_Group_Key				=	@concept_group_key
			AND			c.List_Preferred				=	1
			AND 		pl.Concept_Key IS NULL

			IF @@ERROR <> 0 GOTO fail

			/* select the next concept (if any) for processing */
			SET ROWCOUNT 1

			SELECT		@concept_key		=	Concept_Key,
						@parent_lineage		=	Parent_Lineage
			FROM		@pending_lineage

			IF @@ROWCOUNT = 0
				SET			@concept_key		=	NULL

			SET ROWCOUNT 0

			IF @job_id IS NOT NULL
			BEGIN
				EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
				IF @@ERROR <> 0 GOTO fail

				COMMIT TRANSACTION
			END
		END  /* WHILE @concept_key IS NOT NULL */

		IF @job_id IS NULL COMMIT TRANSACTION
	END /* IF @list_preferred = 1 AND ... */
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_CreateSubtree failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_CreateSubtree') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_CreateSubtree'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_DeleteConcept]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_DeleteConcept]
GO

/*===========================================================================*\
  Description:	Remove lineage corresponding to a specified concept.

  Parameters:	@concept_key			Concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_DeleteConcept]
	@concept_key				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16)

	SELECT		@concept_group_key	=	Concept_Group_Key
	FROM		Concept
	WHERE		Concept_Key			=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	BEGIN TRANSACTION

	/* delete lineage for descendants */
	DELETE		dl
	FROM		Concept_Lineage		AS	cl
	INNER JOIN	Concept_Lineage		AS	dl
	ON			dl.Lineage			LIKE cl.Lineage + '\%'
	INNER JOIN	Concept				AS	d
	ON			d.Concept_Key		=	dl.Concept_Key
	WHERE		cl.Concept_Key		=	@concept_key
	AND			d.Concept_Group_Key	=	@concept_group_key

	IF @@ERROR <> 0 GOTO fail

	/* create top-level lineage for newly orphaned concepts */
	EXECUTE		usp_ConceptLineage_CreateForOrphans		@concept_group_key
	IF @@ERROR <> 0 GOTO fail

	/* delete lineage for specified concept */
	DELETE		Concept_Lineage
	WHERE		Concept_Key			=	@concept_key

	IF @@ERROR <> 0 GOTO fail

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_DeleteConcept failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_DeleteConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_DeleteConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_DeleteSubtree]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_DeleteSubtree]
GO

/*===========================================================================*\
  Description:	Delete specified concept lineage and its descendants.

  Parameters:	@concept_group_key		Concept group key
				@lineage				Parent lineage

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_DeleteSubtree]
	@concept_group_key		CHAR(16),
	@lineage				VARCHAR(900)
AS
	SET NOCOUNT ON

	BEGIN TRANSACTION

	IF LEN(ISNULL(@lineage, '')) = 0
	BEGIN
		SET			@lineage				=	'%'

		UPDATE		Concept_Group
		SET			Last_Sequence_Number	=	NULL
		WHERE		Concept_Group_Key		=	@concept_group_key

		IF @@ERROR <> 0 GOTO fail
	END
	ELSE
	BEGIN
		/* delete specified lineage record */
		DELETE		Concept_Lineage
		FROM		Concept					AS	c
		INNER JOIN	Concept_Lineage			AS	l
		ON			l.Concept_Key			=	c.Concept_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			l.Lineage				=	@lineage

		IF @@ERROR <> 0 GOTO fail

		SET			@lineage				=	@lineage + '\%'
	END

	/* delete descendants */
	DELETE		Concept_Lineage
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key
	AND			l.Lineage				LIKE @lineage

	IF @@ERROR <> 0 GOTO fail	

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_DeleteSubtree failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_DeleteSubtree') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_DeleteSubtree'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_EmptyAndRebuild]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptLineage_EmptyAndRebuild]
GO

/*===========================================================================*\
  Description:	The Concept_Lineage table is emptied and rebuilt for all
		concepts in the concept group.

  Parameters:	@ConceptGroupKey	

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 16/12/05 9:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_EmptyAndRebuild]
	@Key char(16)
AS
	-- NOT YET IMPLEMENTED
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_EmptyAndRebuild') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_EmptyAndRebuild'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_EmptyAndRebuild TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_EmptyAndRebuild TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_EmptyAndRebuild TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_EmptyAndRebuild TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_EmptyAndRebuild TO [Dev - JNCC SQL]
END

GO

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
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptLineage_Get]
GO

/*===========================================================================*\
  Description: Returns a list of concept lineages for the supplied concept

  Parameters:	@ConceptKey
							@IncludeSynonyms - if 1 then lineages for all synonyms are returned
							@UserDomainMask

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 16/12/05 9:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_Get]
	@ConceptKey VARCHAR(100),
  @IncludeSynonyms BIT,
  @UserDomainMask INT
AS

IF @IncludeSynonyms=1
	SELECT DISTINCT CSyn.Concept_Key, CL.Lineage_ID, CL.Lineage, CSyn.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept CSyn on CSyn.Meaning_Key=C.Meaning_Key
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=CSyn.Concept_Key
  INNER JOIN Concept_Group CG on CG.Concept_Group_Key=CSyn.Concept_Group_Key
  INNER JOIN Local_Domain LD on LD.Local_Domain_Key=CG.Local_Domain_Key
  INNER JOIN Domain D on D.Domain_Key=LD.Domain_Key
      AND ((D.Domain_Mask & @UserDomainMask > 0) OR D.Has_Occurrences=0)
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

ELSE
	SELECT C.Concept_Key, CL.Lineage_ID, Lineage, C.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_NewConcept]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_NewConcept]
GO

/*===========================================================================*\
  Description:	Create concept lineage record for a new concept, if required.

  Parameters:	@concept_key			Concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_NewConcept]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	EXECUTE		usp_ConceptLineage_CreateSubTree	@concept_key,
													NULL
	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	RAISERROR ('usp_ConceptLineage_NewConcept failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_NewConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_NewConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [Dev - JNCC SQL]
END
GO

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
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

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
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

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
	$Revision: 1 $
	$Date: 16/12/05 9:00 $
	$Author: Johnvanbreda $

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

