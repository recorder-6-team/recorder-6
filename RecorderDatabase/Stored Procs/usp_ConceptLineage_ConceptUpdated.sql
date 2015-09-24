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
	$Revision: 4 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

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