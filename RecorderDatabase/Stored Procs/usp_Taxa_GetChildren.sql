/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxa_GetChildren]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Taxa_GetChildren]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of sites and subsites recursively.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/07/04 11:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxa_GetChildren]

AS
	SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
		Raiserror('usp_Taxa_GetChildren requires #TempList temp table to be created first.', 16, 1)

	SELECT Contained_List_Item_Key
	FROM Index_Taxon_Group ITG
	INNER JOIN #TempList T ON T.RecordKey=ITG.Taxon_List_Item_Key
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxa_GetChildren') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxa_GetChildren'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [Dev - JNCC SQL]
END
GO