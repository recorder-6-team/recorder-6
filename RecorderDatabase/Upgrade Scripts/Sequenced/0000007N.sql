SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	VI 21086 / CCN364: Create Match Rule SP's
\*============================================================================*/


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationTypes_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Determination Types

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationTypes_Select]
AS
	SELECT		Determination_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Determination_Type
	ORDER BY 	Short_Name 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationTypes_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminerRoles_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminerRoles_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Determiner Roles

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminerRoles_Select]
AS
	SELECT		Determiner_Role_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Determiner_Role
	ORDER BY 	Short_Name 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminerRoles_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminerRoles_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminerRoles_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminationType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminationType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Determination_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Determination_Type(
			Determination_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#DeterminationTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_DeterminationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminerRole') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminerRole]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminerRole]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Determiner_Role', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Determiner_Role(
			Determiner_Role_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#DeterminerRoles
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminerRole') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_DeterminerRole'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminerRole TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminerRole TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminerRole TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_DeterminationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_DeterminationTypes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_DeterminationTypes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Determination_Types
	SET	Matched_Key = Match_Key
	FROM	#DeterminationTypes
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Determination_Types
	SELECT		Import_Value, Match_Key
	FROM		#DeterminationTypes 
	LEFT JOIN	IW_Matched_Determination_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_DeterminationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_DeterminationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_DeterminerRoles') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_DeterminerRoles]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_DeterminerRoles]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Determiner_Roles
	SET	Matched_Key = Match_Key
	FROM	#DeterminerRoles
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Determiner_Roles
	SELECT		Import_Value, Match_Key
	FROM		#DeterminerRoles 
	LEFT JOIN	IW_Matched_Determiner_Roles M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_DeterminerRoles') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_DeterminerRoles'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminerRoles TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminerRoles TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_DeterminerRoles TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminationTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminationTypes]
AS
	-- Update temp table with relevant data.
	UPDATE 	#DeterminationTypes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Determination_Types 
	JOIN	Determination_Type ON Determination_Type_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_DeterminationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminerRoles') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminerRoles]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminerRoles]
AS
	-- Update temp table with relevant data.
	UPDATE 	#DeterminerRoles
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Determiner_Roles 
	JOIN	Determiner_Role ON Determiner_Role_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminerRoles') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_DeterminerRoles'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminerRoles TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminerRoles TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminerRoles TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_DeterminationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_DeterminationType]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_DeterminationType]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#DeterminationTypes
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Determination_Type
		WHERE	Determination_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#DeterminationTypes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_DeterminationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_DeterminationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_DeterminerRole') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_DeterminerRole]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_DeterminerRole]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#DeterminerRoles
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Determiner_Role
		WHERE	Determiner_Role_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#DeterminerRoles
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_DeterminerRole') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_DeterminerRole'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminerRole TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminerRole TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_DeterminerRole TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_DeterminationTypes') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_DeterminationTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_DeterminationTypes]
AS
    UPDATE  #DeterminationTypes
    SET Match_Count =  (SELECT  Count(*)
                FROM    Determination_Type
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #DeterminationTypes
    SET Match_Value = Short_Name,
        Match_Key = Determination_Type_Key
    FROM    Determination_Type
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_DeterminationTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_DeterminationTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminationTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminationTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_DeterminerRoles') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_DeterminerRoles]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 17/03/10 17:20 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_DeterminerRoles]
AS
    UPDATE  #DeterminerRoles
    SET Match_Count =  (SELECT  Count(*)
                FROM    Determiner_Role
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #DeterminerRoles
    SET Match_Value = Short_Name,
        Match_Key = Determiner_Role_Key
    FROM    Determiner_Role
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_DeterminerRoles') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_DeterminerRoles'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminerRoles TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminerRoles TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_DeterminerRoles TO [Dev - JNCC SQL]
END
GO

