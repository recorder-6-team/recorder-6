IF Object_ID('dbo.usp_Taxon_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Taxon_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a taxon.

  Parameters:
	@Key	Key of the taxon list item.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/07/09 14:25 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Taxon_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	T.Taxon_Key,
			T.Item_Name,
			T.Entered_By,
			T.Entry_Date,
			T.Changed_By,
			T.Changed_Date,
			Tv.Taxon_Version_Key,
			TLI.Taxon_List_Item_Key
	FROM	Taxon_List_Item		TLI
	JOIN	Taxon_Version		TV	ON TV.Taxon_Version_Key	= TLI.Taxon_Version_Key
	JOIN	Taxon				T	ON T.Taxon_Key			= TV.Taxon_Key
	WHERE	TLI.Taxon_List_Item_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO "Dev - JNCC SQL"
GO