/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrences_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrences_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the taxon occurrences (with preferred determinations) for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 14/01/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_TaxonOccurrences_Select_ForSample 	
	@SampleKey	CHAR(16)
AS
	SELECT 	TXO.Sample_Key, 
			TXO.Taxon_Occurrence_Key,
			TXO.Checked,
			TXO.Confidential,
			TXO.Provenance,
			TXO.Substrate_Key,
			TXO.Record_Type_Key,
			TXO.Comment,
			TXO.Custodian,
			ITN.Taxon_List_Item_Key,
			ITN.Preferred_Name,
			ITN.Preferred_Name_Italic,
			ITN.Common_Name,
			ITN.Common_Name_Italic,
			TD.Taxon_Determination_Key,
			TD.Determiner,
			TD.Vague_Date_Start,
			TD.Vague_Date_End,
			TD.Vague_Date_Type
	FROM 	Taxon_Occurrence 	TXO
	JOIN 	Taxon_Determination TD 	ON 	TD.Taxon_Occurrence_Key = TXO.Taxon_Occurrence_Key
	JOIN 	Index_Taxon_Name 	ITN ON 	ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	WHERE 	TXO.Sample_Key 				= @SampleKey
	AND   	TD.Preferred 				= 1
	AND 	ITN.System_Supplied_Data	= 1
	ORDER BY ITN.Preferred_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_TaxonOccurrences_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_RecordCardsOnly
GO
