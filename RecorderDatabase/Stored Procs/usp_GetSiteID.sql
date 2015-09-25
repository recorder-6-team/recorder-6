IF EXISTS (SELECT Name FROM   SysObjects 
	   WHERE  Id = Object_ID('[dbo].[usp_GetSiteID]') AND Type = 'P')
    DROP PROCEDURE [dbo].[usp_GetSiteID]
GO

/*
  DESCRIPTION
  This procedure returns the current Site ID 

  PARAMETERS
  @SiteID	Output parameter holding the Site ID.

  Last Revision Details:
    $Revision: 2 $
    $Date: 17/03/03 9:57 $
    $Author: Ericsalmon $

*/

CREATE PROCEDURE [dbo].[usp_GetSiteID] 
	@SiteID char(8) = NULL OUTPUT
AS
	SELECT	@SiteID = Data
	FROM	SETTING
	WHERE	Name = 'SiteID'
GO

-- Grant access permissions
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_GetSiteID]') AND type = 'P')
BEGIN
	PRINT 'Setting up security on procedure [dbo].[usp_GetSiteID]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON [dbo].[usp_GetSiteID] TO [R2k_AddOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON [dbo].[usp_GetSiteID] TO [R2k_Administrator]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON [dbo].[usp_GetSiteID] TO [R2k_FullEdit]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON [dbo].[usp_GetSiteID] TO [R2k_ReadOnly]

	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON [dbo].[usp_GetSiteID] TO [R2k_RecordCardsOnly]
END
GO

