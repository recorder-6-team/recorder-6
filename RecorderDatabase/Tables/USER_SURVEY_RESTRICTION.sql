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
