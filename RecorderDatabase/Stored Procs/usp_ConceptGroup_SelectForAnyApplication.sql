/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_SelectForAnyApplication]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_SelectForAnyApplication]
GO

/*===========================================================================*\
  Description:	Retrieve the concept groups linked to the Application table

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 10/08/06 13:47 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_SelectForAnyApplication]
AS

	SELECT CG.Concept_Group_Key, CG.Item_Name, CG.Hierarchy_Relation_Type_Key
	FROM Concept_Group CG
	INNER JOIN Application_Concept_Group ACG ON ACG.Concept_Group_Key=CG.Concept_Group_Key
	ORDER BY CG.Item_Name

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_SelectForAnyApplication') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_SelectForAnyApplication'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [Dev - JNCC SQL]
END
GO
