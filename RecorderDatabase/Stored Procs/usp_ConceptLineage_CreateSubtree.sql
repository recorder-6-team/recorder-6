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
	$Revision: 8 $
	$Date: 15/11/06 11:36 $
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
			SELECT DISTINCT	r.To_Concept_Key,
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