/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_Locations_Select_AllViceCounties') IS NOT NULL
	DROP PROCEDURE dbo.usp_Locations_Select_AllViceCounties
GO

/*===========================================================================*\
  Description:
	Returns all locations of type Vice-County. The results are formatted to be
	easily added as key/value pairs without any additional processing in the 
	calling application.

  Parameters:
	<none>

  Created:
	March 2009

  Last revision information:
    $Revision: 2 $
    $Date: 11/03/09 8:43 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Locations_Select_AllViceCounties
AS
	SET NOCOUNT OFF
	
	SELECT	File_Code + '=' + Location_Key AS Data
	FROM	Location
	WHERE	Location_Type_Key = 'JNCCIMPW00000001'
	AND		File_Code IS NOT NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_Locations_Select_AllViceCounties'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_Locations_Select_AllViceCounties TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Locations_Select_AllViceCounties TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Locations_Select_AllViceCounties TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Locations_Select_AllViceCounties TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_Locations_Select_AllViceCounties TO [Dev - JNCC SQL]
GO