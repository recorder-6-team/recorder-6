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
	$Revision: 3 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

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
