/****** Object:  StoredProcedure [dbo].[usp_Rucksack_LoadTaxa]    Script Date: 09/26/2013 10:47:17 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of Taxa name recursively.

  Parameters:	@ItemKey
		@Output

  Created:	May 2008

  Last revision information:
    $Revision: 1 $
    $Date: 16/05/08 10:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Rucksack_LoadTaxa]
AS
  SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
	Raiserror('[usp_Rucksack_LoadTaxa]requires #TempList temp table to be created first.', 16, 1)

	SELECT Taxon_List_Item_Key as ItemKey, Actual_Name, Common_Name, Preferred_Name, 
      Authority, Preferred_Name_Authority,
      Actual_Name_Italic, Preferred_Name_Italic, Common_Name_Italic,
      Actual_Name_Attribute, Preferred_Name_Attribute, Common_Name_Attribute
	FROM Index_Taxon_Name ITN
	INNER JOIN #TempList T ON T.RecordKey=ITN.Taxon_List_Item_Key
	WHERE ITN.SYSTEM_SUPPLIED_DATA=1
    ORDER BY T.SortOrder



GO


