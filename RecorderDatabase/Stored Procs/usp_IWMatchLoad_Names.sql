/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchLoad_Names]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchLoad_Names]
GO

/*===========================================================================*\
  Description:	Fills in the Recorder details for all matched names in the 
		temp match table

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 11/06/04 14:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchLoad_Names]
AS

UPDATE #Names
SET Match_Value=dbo.ufn_GetFormattedName(Match_Key)
WHERE Match_Count=1
AND Match_Key IS NOT NULL

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchLoad_Names') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchLoad_Names'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchLoad_Names TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchLoad_Names TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchLoad_Names TO [Dev - JNCC SQL]
END

GO