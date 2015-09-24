/*===========================================================================*\
  A join table for determining which Surveys a User does not have access to.
\*===========================================================================*/
IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[User_Survey_Restriction]') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.User_Survey_Restriction (
		User_Survey_Restriction_Key	CHAR(16) CONSTRAINT PK_USER_SURVEY_RESTRICTION PRIMARY KEY,
		Name_Key	CHAR(16)
		CONSTRAINT FK_USER_SURVEY_RESTRICTION_USER FOREIGN KEY (Name_Key) REFERENCES "User" (Name_Key),
		Survey_Key	CHAR(16)
		CONSTRAINT FK_USER_SURVEY_RESTRICTION_SURVEY FOREIGN KEY (Survey_Key) REFERENCES Survey (Survey_Key)
	) ON [PRIMARY]
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT  SELECT  ON [dbo].[User_Survey_Restriction]  TO [R2k_ReadOnly]
GO
GRANT  SELECT  ON [dbo].[User_Survey_Restriction]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT  ON [dbo].[User_Survey_Restriction]  TO [R2k_AddOnly]
GO
GRANT  SELECT, UPDATE, INSERT ON [dbo].[User_Survey_Restriction]  TO [R2k_FullEdit]
GO
GRANT  SELECT, UPDATE, INSERT ON [dbo].[User_Survey_Restriction]  TO [R2k_Administrator]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForUserAccess') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForUserAccess]
GO

/*===========================================================================*\
  Description:		
	Returns all Surveys on the system using the standard caption for a survey 
	in alphabetical order. (Saved in JNCC folder). Also returns a column 
	stating whether or not the specified user can view each survey.

  Parameters: 
	@User_Name_Key CHAR(16) The Name_Key of the User whose surveys are
		being viewed.

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 5/02/09 10:14 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForUserAccess]
	@User_Name_Key CHAR(16)
AS
	SET NOCOUNT ON

	SELECT		SU.Survey_Key,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name,
				CASE
					WHEN USR.Name_Key IS NULL THEN 1 
					ELSE 0
				END AS Allow_View
	FROM		Survey			SU	
	JOIN		Name			N		ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I		ON	I.Name_Key		=	N.Name_Key
										AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 		ON	O.Name_Key		=	N.Name_Key
										AND	N.Organisation	=	1
	LEFT JOIN	User_Survey_Restriction USR	ON	USR.Survey_Key	=	SU.Survey_Key
										AND	USR.Name_Key	=	@User_Name_Key
	ORDER BY	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForUserAccess') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForUserAccess'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForUserAccess TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForUserAccess TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Allow_User_Access') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Survey_Allow_User_Access]
GO

/*===========================================================================*\
  Description:		
	Allows access to the specified survey for the specified user.

  Parameters:	
	@User_Name_Key CHAR(16) The Name_Key of the User.
	@SurveyKey CHAR(16) The Survey_Key of the Survey to allow access to.

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 5/02/09 10:14 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Survey_Allow_User_Access]
	@User_Name_Key	CHAR(16),
	@Survey_Key		CHAR(16)
AS
	SET NOCOUNT ON

	DELETE FROM User_Survey_Restriction
	WHERE	Name_Key	=	@User_Name_Key
		AND	Survey_Key	=	@Survey_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Allow_User_Access') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Survey_Allow_User_Access'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Allow_User_Access TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Survey_Allow_User_Access TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Deny_User_Access') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Survey_Deny_User_Access]
GO

/*===========================================================================*\
  Description:		
	Denies access to the specified survey for the specified user.

  Parameters:	
	@User_Name_Key CHAR(16) The Name_Key of the User.
	@SurveyKey CHAR(16) The Survey_Key of the Survey to deny access to.

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 5/02/09 10:14 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Survey_Deny_User_Access]
	@User_Name_Key	CHAR(16),
	@Survey_Key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Key	CHAR(16)

	EXECUTE spNextKey 'User_Survey_Restriction', @Key OUTPUT

	INSERT INTO User_Survey_Restriction (
		User_Survey_Restriction_Key,
		Name_Key, 
		Survey_Key
	)
    VALUES (
		@Key,
		@User_Name_Key, 
		@Survey_Key
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Deny_User_Access') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Survey_Deny_User_Access'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Deny_User_Access TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Survey_Deny_User_Access TO [Dev - JNCC SQL]
END
GO


