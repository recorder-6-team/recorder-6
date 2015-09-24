IF Object_ID('dbo.usp_Biotope_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Biotope_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a biotope.

  Parameters:
	@Key	Key of the biotope list item.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Biotope_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	B.Biotope_Key,
			ISNULL(B.Original_Code + ' - ', '') + B.Short_Term AS DisplayField,
			B.Original_Code,
			B.Short_Term,
			B.Entered_By,
			B.Entry_Date,
			B.Changed_By,
			B.Changed_Date,
			BLI.Biotope_List_Item_Key
	FROM	Biotope_List_Item	BLI
	JOIN	Biotope				B	ON B.Biotope_Key = BLI.Biotope_Key
	WHERE	BLI.Biotope_List_Item_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForMetadata TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_BiotopeClassification_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_BiotopeClassification_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a taxon.

  Parameters:
	@Key	Key of the taxon list item.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_BiotopeClassification_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

    SELECT	BC.Long_Name, 
			BC.Objectives,
			BC.Created_By,
			BC.Created_Vague_Date_Start,
			BC.Created_Vague_Date_End,
			BC.Created_Vague_Date_Type,
			BCV.Revision_Number,
			BCV.Revision_Date
	FROM	Biotope_Classification			BC 
	JOIN	Biotope_Classification_Version	BCV ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	BC.Biotope_Classification_Key	= @Key
	ORDER BY BCV.Revision_Number DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_BiotopeClassification_Select_ForMetadata TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_Taxon_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Taxon_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a taxon.

  Parameters:
	@Key	Key of the taxon list item.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Taxon_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	T.Taxon_Key,
			T.Item_Name,
			T.Entered_By,
			T.Entry_Date,
			T.Changed_By,
			T.Changed_Date,
			Tv.Taxon_Version_Key,
			TLI.Taxon_List_Item_Key
	FROM	Taxon_List_Item		TLI
	JOIN	Taxon_Version		TV	ON TV.Taxon_Version_Key	= TLI.Taxon_Version_Key
	JOIN	Taxon				T	ON T.Taxon_Key			= TV.Taxon_Key
	WHERE	TLI.Taxon_List_Item_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Taxon_Select_ForMetadata TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_TaxonList_Select_ForMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonList_Select_ForMetadata
GO

/*===========================================================================*\
  Description:
	Returns information about a taxon list.

  Parameters:
	@Key	Key of the taxon list.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_TaxonList_Select_ForMetadata
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	Item_Name,
			Long_Name,
			TL.Description,
			Update_Mechanism,
			Local_Disk,
			Version,
			TLV.Authority,
			TLV.Taxon_List_Version_Key,
			Quality,
			Vague_Date_Start,
			Vague_Date_End,
			Vague_Date_Type
	FROM	Taxon_List			TL
	JOIN	Taxon_List_Type		TLT ON TL.Taxon_List_Type_Key	= TLT.Taxon_List_Type_Key
	JOIN	Taxon_List_Version	TLV ON TL.Taxon_List_Key		= TLV.Taxon_List_Key
	WHERE TL.Taxon_List_Key = @Key
	ORDER BY TLV.Version DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_TaxonList_Select_ForMetadata TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_User_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_User_Select]
GO

/*===========================================================================*\
  Description:	Returns a user, and the formatted user name.

  Parameters:	@NameKey

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_User_Select]
	@NameKey char(16)
AS
SET NOCOUNT ON

	SELECT 	*, dbo.ufn_GetFormattedName(@NameKey) AS [User_Name]
	FROM	[User]
	WHERE	Name_Key = @NameKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_User_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_User_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_User_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_User_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_User_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_User_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_User_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_User_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Users_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Users_Select]
GO

/*===========================================================================*\
  Description:	Returns all users, and the formatted user name.

  Parameters:	@MinimumSecurityLevel (optional)

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Users_Select]
	@MinimumSecurityLevel int = NULL
AS
SET NOCOUNT ON

	SELECT 	*, dbo.ufn_GetFormattedName(Name_Key) AS [User_Name]
	FROM	[User]
	WHERE	Security_Level >= IsNull(@MinimumSecurityLevel, 0)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Users_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Users_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Users_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Users_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Users_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Users_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Users_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Users_Select TO [Dev - JNCC SQL]
END
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
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
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
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_User_Update_FirstLogin TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_Setting_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_Setting_Get
GO

/*===========================================================================*\
  Description:
	Returns a setting entry.

  Parameters:
	@Name	The name of the setting to retrieve.
	@Value	The value of the entry, as an OUTPUT.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Setting_Get
	@Name	VARCHAR(20),
	@Value	VARCHAR(250) OUTPUT

AS
	SET NOCOUNT ON

	SELECT	@Value = Data
	FROM	Setting
	WHERE	Name = @Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Setting_Get TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Setting_Get TO "Dev - JNCC SQL"
GO

IF Object_ID('dbo.usp_Setting_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_Setting_Update
GO

/*===========================================================================*\
  Description:
	Updates a setting entry, or inserts a new entry if not found.

  Parameters:
	@Name	The name of the setting to update.
	@Value	The value of the entry.

  Created:	July 2009

  Last revision information:
    $Revision: 4 $
    $Date: 20/07/09 11:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Setting_Update
	@Name	VARCHAR(20),
	@Value	VARCHAR(250)
AS
	SET NOCOUNT ON

	IF EXISTS(SELECT * FROM Setting WHERE Name = @Name) 
		UPDATE	Setting
		SET		Data = @Value
		WHERE	Name = @Name
	ELSE
		INSERT INTO Setting (Name, Data)
		VALUES (@Name, @Value)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Setting_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Setting_Update TO "Dev - JNCC SQL"
GO