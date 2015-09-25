/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationsSubSites_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocationsSubSites_Select]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of sites and subsites recursively.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 5/07/04 16:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationsSubSites_Select]

AS
	SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
		Raiserror('usp_LocationsSubSites_Select requires #TempList temp table to be created first.', 16, 1)

	WHILE 1=1
	BEGIN
	
		INSERT INTO #TempList 
		SELECT L.Location_Key AS RecordKey
		FROM Location L
		INNER JOIN #TempList T1 ON T1.RecordKey=L.Parent_Key
		LEFT JOIN #TempList T2 ON T2.RecordKey=L.Location_Key
		WHERE T2.RecordKey IS NULL
	
		IF @@RowCount=0 
			BREAK
	
	END

	-- Return dataset
	SELECT * FROM #TempList
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationsSubSites_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationsSubSites_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [Dev - JNCC SQL]
END
GO