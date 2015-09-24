/****** Object:  StoredProcedure [dbo].[usp_Taxa_GetChildren]    Script Date: 02/03/2014 11:30:20 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of sites and subsites recursively.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Taxa_GetChildren]

AS
	SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
		Raiserror('usp_Taxa_GetChildren requires #TempList temp table to be created first.', 16, 1)

	SELECT Contained_List_Item_Key
	FROM Taxon_List_Item TLI -- parent item
	INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY=TLI.TAXON_LIST_VERSION_KEY -- to get parent item's list
	INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY=TLI.TAXON_LIST_ITEM_KEY -- to find the recommended tli key
	INNER JOIN INDEX_TAXON_GROUP ITG ON ITG.TAXON_LIST_ITEM_KEY=ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY -- find child recommended items
	INNER JOIN INDEX_TAXON_NAME ITNC ON ITNC.TAXON_LIST_ITEM_KEY=ITG.CONTAINED_LIST_ITEM_KEY -- child recommended item	
	INNER JOIN INDEX_TAXON_NAME ITNCS ON ITNCS.RECOMMENDED_TAXON_LIST_ITEM_KEY=ITNC.RECOMMENDED_TAXON_LIST_ITEM_KEY -- child any (inc synonyms) items	
		AND ITNCS.Allow_Data_Entry=1
	INNER JOIN TAXON_LIST_VERSION TLVC ON TLVC.TAXON_LIST_VERSION_KEY=ITNCS.TAXON_LIST_VERSION_KEY -- to get child item's list
	INNER JOIN #TempList T ON T.RecordKey=TLI.Taxon_List_Item_Key -- parent item is in out search list
