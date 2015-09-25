/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PreImport_TaxonListItem]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PreImport_TaxonListItem]
GO

/*===========================================================================*\
  Description:	Drop constraints that impede import of taxon_list_item data

  Created:	Jan 2006

  Last revision information:
    $Revision: 1 $
    $Date: 9/01/06 15:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreImport_TaxonListItem]
AS

IF EXISTS (
		select 1 from dbo.sysobjects 
		where id = object_id(N'[dbo].[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM]') 
		and OBJECTPROPERTY(id, N'IsForeignKey') = 1 
		and parent_obj=object_id('taxon_list_item'))
ALTER TABLE [dbo].[Taxon_List_Item] DROP CONSTRAINT FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM

IF EXISTS (
		select 1 from dbo.sysobjects 
		where id = object_id(N'[dbo].[FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM_Preferred]') 
		and OBJECTPROPERTY(id, N'IsForeignKey') = 1 
		and parent_obj=object_id('taxon_list_item'))
ALTER TABLE [dbo].[Taxon_List_Item] DROP CONSTRAINT FK_TAXON_LIST_ITEM_TAXON_LIST_ITEM_Preferred

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreImport_TaxonListItem') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PreImport_TaxonListItem'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PreImport_TaxonListItem TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PreImport_TaxonListItem TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PreImport_TaxonListItem TO [Dev - JNCC SQL]
END
GO