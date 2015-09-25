IF Object_ID('dbo.usp_BiotopeClassification_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_BiotopeClassification_Select_ForMetadata
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

CREATE PROCEDURE dbo.usp_BiotopeClassification_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

    SELECT	BC.Long_Name, 
			BC.Objectives,
			BC.Created_By,
			BC.Created_Vague_Date_Start,
			BC.Created_Vague_Date_End,
			BC.Created_Vague_Date_Type,
			BCV.Revision_Number,
			BCV.Revision_Date
	FROM	Biotope_Classification			BC 
	JOIN	Biotope_Classification_Version	BCV ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	BC.Biotope_Classification_Key	= @Key
	ORDER BY BCV.Revision_Number DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO "Dev - JNCC SQL"
GO