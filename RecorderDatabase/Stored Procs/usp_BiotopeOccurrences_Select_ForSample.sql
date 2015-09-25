/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_BiotopeOccurrences_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_BiotopeOccurrences_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the biotope occurrences (with preferred determinations) for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 14/01/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_BiotopeOccurrences_Select_ForSample 	
	@SampleKey	CHAR(16)
AS
	SELECT	DISTINCT
			BO.Biotope_Occurrence_Key,
			CASE 
				WHEN B.Original_Code IS NULL THEN B.Short_Term 
				ELSE B.Original_Code + ', ' + B.Short_Term 
			END AS Item_Name,
			BD.Biotope_List_Item_Key,
			BO.Checked
	FROM 	Biotope_Occurrence 		BO 
	JOIN	Biotope_Determination	BD	ON	BD.Biotope_Occurrence_Key 	= BO.Biotope_Occurrence_Key
	JOIN	Biotope_List_Item 		BLI ON	BLI.Biotope_List_Item_Key 	= BD.Biotope_List_Item_Key
	JOIN	Biotope 				B 	ON	B.Biotope_Key 				= BLI.Biotope_Key
	WHERE 	BO.Sample_Key 	= @SampleKey
	AND 	BD.Preferred 	= 1
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_BiotopeOccurrences_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_RecordCardsOnly
GO
