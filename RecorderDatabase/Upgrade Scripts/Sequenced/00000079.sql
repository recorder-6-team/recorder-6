IF Object_ID('dbo.usp_DefaultUser_Get_Key') IS NOT NULL
	DROP PROCEDURE dbo.usp_DefaultUser_Get_Key
GO

/*===========================================================================*\
  Description:
	Retrieves the key for Default User.

  Parameters:

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_DefaultUser_Get_Key
AS
	SET NOCOUNT ON

	SELECT	U.Name_Key 
	FROM	"User"		U
	JOIN	"Name"		N ON N.Name_Key = U.Name_Key
	JOIN	Individual	I ON I.Name_Key = N.Name_Key
	WHERE	I.Forename = 'Default' 
	AND		I.Surname  = 'User'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_DefaultUser_Get_Key TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_Individual_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_Individual_Insert
GO

/*===========================================================================*\
  Description:
	Inserts a new individual record.

  Parameters:
	@Key			New key, output.
	@Title
	@Forename
	@Initials
	@Honorifics
	@Surname
	@Comment
	@DOBStart
	@DOBEnd
	@DOBType
	@DODStart
	@DODEnd
	@DODType
	@Floreat
	@EnteredBy
	@DepartmentKey

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Individual_Insert
	@Key			CHAR(16)	OUTPUT,
	@Title			VARCHAR(4)	= NULL,
	@Forename		VARCHAR(20)	= NULL,
	@Initials		VARCHAR(8)	= NULL,
	@Honorifics		VARCHAR(20)	= NULL,
	@Surname		VARCHAR(30),
	@Comment		TEXT		= NULL,
	@DOBStart		INT			= NULL,
	@DOBEnd			INT			= NULL,
	@DOBType		VARCHAR(2)	= NULL,
	@DODStart		INT			= NULL,
	@DODEnd			INT			= NULL,
	@DODType		VARCHAR(2)	= NULL,
	@Floreat		VARCHAR(12)	= NULL,
	@EnteredBy		CHAR(16),
	@DepartmentKey	CHAR(16)	= NULL
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Name', @Key OUTPUT

	-- Handles the First Run issue. The user is going to be the new one being created.
	IF ISNULL(@EnteredBy, '') = ''
		SET @EnteredBy = @Key

	INSERT INTO "Name" (
		Name_Key,
		Organisation,
		Entered_By
	) VALUES (
		@Key,
		0,
		@EnteredBy
	)

	INSERT INTO Individual (
		Name_Key,
		Title,
		Forename,
		Initials,
		Honorifics,
		Surname,
		Comment,
		Born_Vague_Date_Start,
		Born_Vague_Date_End,
		Born_Vague_Date_Type,
		Died_Vague_Date_Start,
		Died_Vague_Date_End,
		Died_Vague_Date_Type,
		Person_Floreat,
		Entered_By,
		Organisation_Department_Key
	) VALUES (
		@Key,
		@Title,
		@Forename,
		@Initials,
		@Honorifics,
		@Surname,
		@Comment,
		@DOBStart,
		@DOBEnd,
		@DOBType,
		@DODStart,
		@DODEnd,
		@DODType,
		@Floreat,
		@EnteredBy,
		@DepartmentKey
	)
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Individual_Insert TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_Survey_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_Survey_Insert
GO

/*===========================================================================*\
  Description:
	Inserts a new survey record.

  Parameters:
		@Key,
		@ItemName,
		@Description,
		@RunByKey,
		@SurveyStatusKey,
		@SurveyMediaKey,
		@SurveyTypeKey,
		@FromVagueDateStart,
		@FromVagueDateEnd,
		@FromVagueDateType,
		@ToVagueDateStart,
		@ToVagueDateEnd,
		@ToVagueDateType,
		@OPFromVagueDateStart,
		@OPFromVagueDateEnd,
		@OPFromVagueDateType,
		@OPToVagueDateStart,
		@OPToVagueDateEnd,
		@OPToVagueDateType,
		@SWSpatialRef,
		@SWLat,
		@SWLong,
		@SWSpatialRefQualifier,
		@NESpatialRef,
		@NELat,
		@NELong,
		@NESpatialRefQualifier,
		@SpatialRefSystem,
		@GeographicCoverage,
		@Periodicity,
		@EnteredBy

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Survey_Insert
	@Key					CHAR(16)	OUTPUT,
	@ItemName				VARCHAR(100),
	@Description			TEXT		= NULL,
	@RunByKey				CHAR(16),
	@SurveyStatusKey		CHAR(16)	= NULL,
	@SurveyMediaKey			CHAR(16)	= NULL,
	@SurveyTypeKey			CHAR(16),
	@FromVagueDateStart		INT			= NULL,
	@FromVagueDateEnd		INT			= NULL,
	@FromVagueDateType		VARCHAR(2),
	@ToVagueDateStart		INT			= NULL,
	@ToVagueDateEnd			INT			= NULL,
	@ToVagueDateType		VARCHAR(2)	= NULL,
	@OPFromVagueDateStart	INT			= NULL,
	@OPFromVagueDateEnd		INT			= NULL,
	@OPFromVagueDateType	VARCHAR(2)	= NULL,
	@OPToVagueDateStart		INT			= NULL,
	@OPToVagueDateEnd		INT			= NULL,
	@OPToVagueDateType		VARCHAR(2)	= NULL,
	@SWSpatialRef			VARCHAR(40) = NULL,
	@SWLat					FLOAT		= NULL,
	@SWLong					FLOAT		= NULL,
	@SWSpatialRefQualifier	VARCHAR(20) = NULL,
	@NESpatialRef			VARCHAR(40) = NULL,
	@NELat					FLOAT		= NULL,
	@NELong					FLOAT		= NULL,
	@NESpatialRefQualifier	VARCHAR(20) = NULL,
	@SpatialRefSystem		VARCHAR(4)	= NULL,
	@GeographicCoverage		TEXT		= NULL,
	@Periodicity			VARCHAR(16) = NULL,
	@EnteredBy				CHAR(16)
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Survey', @Key OUTPUT

	INSERT INTO Survey (
		Survey_Key,
		Item_Name,
		Description,
		Run_By,
		Survey_Status_Key,
		Survey_Media_Key,
		Survey_Type_Key,
		From_Vague_Date_Start,
		From_Vague_Date_End,
		From_Vague_Date_Type,
		To_Vague_Date_Start,
		To_Vague_Date_End,
		To_Vague_Date_Type,
		OP_From_Vague_Date_Start,
		OP_From_Vague_Date_End,
		OP_From_Vague_Date_Type,
		OP_To_Vague_Date_Start,
		OP_To_Vague_Date_End,
		OP_To_Vague_Date_Type,
		SW_Spatial_Ref,
		SW_Lat,
		SW_Long,
		SW_Spatial_Ref_Qualifier,
		NE_Spatial_Ref,
		NE_Lat,
		NE_Long,
		NE_Spatial_Ref_Qualifier,
		Spatial_Ref_System,
		Geographic_Coverage,
		Periodicity,
		Entered_By
	) VALUES (
		@Key,
		@ItemName,
		@Description,
		@RunByKey,
		@SurveyStatusKey,
		@SurveyMediaKey,
		@SurveyTypeKey,
		@FromVagueDateStart,
		@FromVagueDateEnd,
		@FromVagueDateType,
		@ToVagueDateStart,
		@ToVagueDateEnd,
		@ToVagueDateType,
		@OPFromVagueDateStart,
		@OPFromVagueDateEnd,
		@OPFromVagueDateType,
		@OPToVagueDateStart,
		@OPToVagueDateEnd,
		@OPToVagueDateType,
		@SWSpatialRef,
		@SWLat,
		@SWLong,
		@SWSpatialRefQualifier,
		@NESpatialRef,
		@NELat,
		@NELong,
		@NESpatialRefQualifier,
		@SpatialRefSystem,
		@GeographicCoverage,
		@Periodicity,
		@EnteredBy
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Survey_Insert TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_SurveyType_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_SurveyType_Select
GO

/*===========================================================================*\
  Description:
	Returns a Survey Type record.

  Parameters:
	@Key	Key of the survey type.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SurveyType_Select
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	Survey_Type_Key AS Item_Key,
			Short_Name		AS Item_Name
	FROM	Survey_Type
	WHERE	Survey_Type_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_User_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Insert
GO

/*===========================================================================*\
  Description:
	Inserts a new user record.

  Parameters:
	@NameKey			Key of user.
	@Password			User's password.
	@SecurityLevel		Level to grant to the user.
	@FullEditOwnData	Flag.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Insert
	@NameKey			CHAR(16),
	@Password			VARCHAR(20),
	@SecurityLevel		TINYINT,
	@FullEditOwnData	BIT
AS
	SET NOCOUNT ON

	INSERT INTO "User"(
		Name_Key,
		Password,
		Security_Level,
		Full_Edit_Own_Data
	) VALUES (
		@NameKey,
		@Password,
		@SecurityLevel,
		@FullEditOwnData
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Insert TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_User_Select_ForLogin') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Select_ForLogin
GO

/*===========================================================================*\
  Description:
	Check the combination of user's key and password.

  Parameters:
	@NameKey	Key of user to check.
	@Password	User's password.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Select_ForLogin
	@NameKey	CHAR(16),
	@Password	VARCHAR(20)
AS
	SET NOCOUNT ON

	SELECT	Name_Key, 
			Security_Level, 
			Full_Edit_Own_Data
	FROM	"User" 
	WHERE	Name_Key = @NameKey
	AND		Password = @Password
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Select_ForLogin TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_User_Update_FirstLogin') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Update_FirstLogin
GO

/*===========================================================================*\
  Description:
	Sets the First_Login flag to false for the given user.

  Parameters:
	@NameKey	Key of user.
	
  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Update_FirstLogin
	@NameKey	CHAR(16)
AS
	SET NOCOUNT ON

	UPDATE	"User"
	SET		First_Login = 0
	WHERE	Name_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_User_Update_Password') IS NOT NULL
	DROP PROCEDURE dbo.usp_User_Update_Password
GO

/*===========================================================================*\
  Description:
	Changes the password of the given user.

  Parameters:
	@NameKey	Key of user.
	@Pwd		User's password.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_User_Update_Password
	@NameKey	CHAR(16),
	@Password	VARCHAR(20)
AS
	SET NOCOUNT ON

	UPDATE	"User"
	SET		Password = @Password
	WHERE	Name_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_Password TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Update_Password TO "Dev - JNCC SQL"
GO
