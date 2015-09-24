/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent	

  Parameters:	@ParentConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 11 $
    $Date: 28/04/04 10:38 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForParent]
	@ParentConceptKey varchar(100),
  	@HierarchyRelationTypeKey char(16)
AS

SELECT distinct C.Concept_Key, 
		T.Item_Name, 
		C.Sort_Code, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 
							   ELSE 1 
		END AS HasChildren,
  		C.Concept_Rank_Key
FROM 		Concept_Relation CR1
INNER JOIN 	Concept C ON C.Concept_Key = CR1.To_Concept_Key
INNER JOIN 	Term T ON T.Term_Key = C.Term_Key
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1)
		ON CR2.From_Concept_Key = C.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
WHERE 		CR1.From_Concept_Key = @ParentConceptKey
AND 		CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
AND 		C.List_Preferred = 1
AND 		C.Is_Current = 1
ORDER BY 	C.Sort_Code, 
		T.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [Dev - JNCC SQL]
END

GO
