/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ValidationLevel_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ValidationLevel_Get]
GO

/*===========================================================================*\
  Description:	Retrieves the validation competency level for a taxon list item

  Parameters:	Taxon list item key

  Created:	Sept 2006

  Last revision information:
    $Revision: 1 $
    $Date: 6/09/06 10:18 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ValidationLevel_Get]
@Key CHAR(16),
@Output TINYINT OUTPUT
AS
	SELECT @Output = Validation_Level
	FROM Taxon_List_Item TLI 
	INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key 
	WHERE TLI.Taxon_List_Item_Key=@Key
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ValidationLevel_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ValidationLevel_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ValidationLevel_Get TO [Dev - JNCC SQL]
END
GO