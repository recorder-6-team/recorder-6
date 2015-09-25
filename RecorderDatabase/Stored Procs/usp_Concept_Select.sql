/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select]
GO

/*===========================================================================*\
  Description:	Returns fields from the Concept table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 23/09/04 18:21 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	
			C.Term_Key AS Item_Key,
			TL.Item_Name AS Item_Name,
			Lower(TL.Language_Key) AS Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			C.Concept_Group_Key,
			CG.Hierarchy_Relation_Type_Key,
			C.Term_Version_Key,	
			C.List_Preferred,
			C.Is_Current,
			C.Preferred,
			C.Concept_Rank_Key,
			CR.Item_Name AS Concept_Rank_Name,
			C.Name_Type_Concept_Key,
			CT.Item_Name AS Name_Type_Concept_Name,
			C.Meaning_Key,
			C.Author_Copy,
			C.Sort_Code,
			C.List_Code,
			CASE WHEN CRel.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
			C.System_Supplied_Data,
			C.Entered_Session_ID,
			C.[Timestamp]
	FROM 		Concept AS C
	INNER JOIN	Term AS TL ON TL.Term_Key = C.Term_Key
	INNER JOIN	Language AS L ON L.Language_Key = TL.Language_Key
	LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = C.Name_Type_Concept_Key
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Concept_Relation CRel ON CRel.From_Concept_Key = C.Concept_Key
       				     	     AND CRel.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	WHERE		C.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [Dev - JNCC SQL]
END

GO
