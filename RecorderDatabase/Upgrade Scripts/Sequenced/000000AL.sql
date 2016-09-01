/****** Object:  StoredProcedure [dbo].[usp_Users_Select_Admin]    Script Date: 09/01/2016 08:34:10 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns all users who are Admin but not default user 

  Parameters:	@Default_User_Key 

  Created:	August 2016

  

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Users_Select_Admin]
	@Default_User_Key varChar(16) 
AS
SET NOCOUNT ON

	SELECT 	* 
	FROM	[User]
	WHERE	Security_Level = 5 and Name_Key <> @Default_User_Key 

SET NOCOUNT OFF


GRANT EXECUTE ON dbo].[usp_Users_Select_Admin] to Public	
	  

