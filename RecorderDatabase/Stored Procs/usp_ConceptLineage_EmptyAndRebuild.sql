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
    $Date: 28/11/03 11:04 $
    $Author: Anthonysimpson $

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
