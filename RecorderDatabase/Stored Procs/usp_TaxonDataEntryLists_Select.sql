/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDataEntryLists_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
GO

/*===========================================================================*\
  Description:	Returns list of taxon lists.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/11/04 15:51 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
AS
	SELECT	TL.Taxon_List_Key AS Item_Key, TL.Item_Name
	FROM	Taxon_List TL
	INNER JOIN Taxon_List_Type TLT 
		ON TLT.Taxon_List_Type_Key=TL.Taxon_List_Type_Key
		AND TLT.Allow_Data_Entry=1
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDataEntryLists_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDataEntryLists_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [Dev - JNCC SQL]
END
GO