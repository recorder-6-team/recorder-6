/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select_ForApplication]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select_ForApplication]
GO

/*===========================================================================*\
  Description:	Retrieve the concept group responsible for a particular
		task (e.g. the one containing keywords)

  Parameters:	@Key		Application_Key

  Created:	December 2005

  Last revision information:
    $Revision: 1 $
    $Date: 15/12/05 13:18 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select_ForApplication]
	@Key CHAR(16)
AS

	SELECT CG.Concept_Group_Key, CG.Hierarchy_Relation_Type_Key
	FROM Concept_Group CG
	INNER JOIN Application_Concept_Group ACG ON ACG.Concept_Group_Key=CG.Concept_Group_Key
		AND ACG.Application_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_ForApplication') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_ForApplication'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [Dev - JNCC SQL]
END
GO
