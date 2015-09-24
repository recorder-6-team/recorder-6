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
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

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