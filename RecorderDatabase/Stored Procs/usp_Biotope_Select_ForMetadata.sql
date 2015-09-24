IF Object_ID('dbo.usp_Biotope_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Biotope_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a biotope.

  Parameters:
	@Key	Key of the biotope list item.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/07/09 14:25 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Biotope_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	B.Biotope_Key,
			ISNULL(B.Original_Code + ' - ', '') + B.Short_Term AS DisplayField,
			B.Original_Code,
			B.Short_Term,
			B.Entered_By,
			B.Entry_Date,
			B.Changed_By,
			B.Changed_Date,
			BLI.Biotope_List_Item_Key
	FROM	Biotope_List_Item	BLI
	JOIN	Biotope				B	ON B.Biotope_Key = BLI.Biotope_Key
	WHERE	BLI.Biotope_List_Item_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO "Dev - JNCC SQL"
GO