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
	$Revision: 3 $
	$Date: 12/05/04 14:04 $
	$Author: Anthonysimpson $

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