/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PostImport_TaxonListItem]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PostImport_TaxonListItem]
GO

/*===========================================================================*\
  Description:	Recreate constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/01/06 15:15 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PostImport_TaxonListItem]
AS

/* Remove any invalid links in Taxon_List_Item table entries before reapplying constraints */
UPDATE	TLI1
SET	Preferred_Name = TLI1.Taxon_List_Item_Key
FROM 	Taxon_List_Item TLI1
LEFT JOIN Taxon_List_Item TLI2 ON TLI1.Preferred_Name = TLI2.Taxon_List_Item_Key
WHERE	TLI2.Taxon_List_Item_Key IS NULL

UPDATE	TLI1
SET	Parent = Null
FROM 	Taxon_List_Item TLI1
LEFT JOIN Taxon_List_Item TLI2 ON TLI1.Parent = TLI2.Taxon_List_Item_Key
WHERE	TLI2.Taxon_List_Item_Key IS NULL AND TLI1.Parent IS NOT NULL

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM_Preferred]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE dbo.TAXON_LIST_ITEM ADD CONSTRAINT
	FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM_Preferred FOREIGN KEY
	(
	PREFERRED_NAME
	) REFERENCES dbo.TAXON_LIST_ITEM
	(
	TAXON_LIST_ITEM_KEY
	)

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE dbo.TAXON_LIST_ITEM ADD CONSTRAINT
	FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM FOREIGN KEY
	(
	PARENT
	) REFERENCES dbo.TAXON_LIST_ITEM
	(
	TAXON_LIST_ITEM_KEY
	)
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PostImport_TaxonListItem') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PostImport_TaxonListItem'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PostImport_TaxonListItem TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PostImport_TaxonListItem TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PostImport_TaxonListItem TO [Dev - JNCC SQL]
END
GO