IF Object_ID('dbo.usp_TaxonList_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonList_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a taxon list.

  Parameters:
	@Key	Key of the taxon list.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/07/09 14:25 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_TaxonList_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	Item_Name,
			Long_Name,
			TL.Description,
			Update_Mechanism,
			Local_Disk,
			Version,
			TLV.Authority,
			TLV.Taxon_List_Version_Key,
			Quality,
			Vague_Date_Start,
			Vague_Date_End,
			Vague_Date_Type
	FROM	Taxon_List			TL
	JOIN	Taxon_List_Type		TLT ON TL.Taxon_List_Type_Key	= TLT.Taxon_List_Type_Key
	JOIN	Taxon_List_Version	TLV ON TL.Taxon_List_Key		= TLV.Taxon_List_Key
	WHERE TL.Taxon_List_Key = @Key
	ORDER BY TLV.Version DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO "Dev - JNCC SQL"
GO