/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Rucksack_LoadTaxa]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Rucksack_LoadTaxa]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of Taxa name recursively.

  Parameters:	@ItemKey
		@Output

  Created:	May 2008

  Last revision information:
    $Revision: 3 $
    $Date: 15/05/08 15:33 $
    $Author: Qingsun $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Rucksack_LoadTaxa]
AS
  SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
	Raiserror('[usp_Rucksack_LoadTaxa]requires #TempList temp table to be created first.', 16, 1)

	SELECT Taxon_List_Item_Key as ItemKey, Actual_Name,Common_Name, Preferred_Name,Preferred_Name_Italic,Common_Name_Italic
	FROM Index_Taxon_Name ITG
	INNER JOIN #TempList T ON T.RecordKey=ITG.Taxon_List_Item_Key
    ORDER BY T.SortOrder

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Rucksack_LoadTaxa') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Rucksack_LoadTaxa'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Rucksack_LoadTaxa TO [Dev - JNCC SQL]
END
GO