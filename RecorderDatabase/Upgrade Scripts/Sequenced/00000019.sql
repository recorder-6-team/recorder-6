SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AdminArea_TopLevelAdminType_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AdminArea_TopLevelAdminType_Get]
GO

/*===========================================================================*\
  Description:	Gets the top level admin type key for an admin area

  Parameters:	None

  Created:	July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AdminArea_TopLevelAdminType_Get]
	@Key CHAR(16),
	@AdminTypeKey CHAR(16) OUTPUT	
AS

DECLARE @AdminAreaKey CHAR(16)
DECLARE @ParentKey CHAR(16)

SET @AdminAreaKey=@Key

WHILE 1=1
BEGIN
	SELECT @ParentKey=Parent
	FROM Admin_Area
	WHERE Admin_Area_Key=@AdminAreaKey

	IF @ParentKey IS NULL
		BREAK
	ELSE
		SET @AdminAreaKey=@ParentKey	
END

SELECT @AdminTypeKey=Admin_Type_Key 
FROM Admin_Area 
WHERE Admin_Area_Key=@AdminAreaKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AdminArea_TopLevelAdminType_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_AdminArea_TopLevelAdminType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AdminArea_TopLevelAdminType_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Biotopes_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of biotopes matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
	@SearchKey char(16),
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
	ELSE
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				AND BCV.Biotope_Classification_Key = @SearchKey
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotopes_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotopes_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetLocationName]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
GO

/*===========================================================================*\
  Description:  Obtain the location name from an import record.

  Parameters:   @record_no              Identifies the import record.
                @location_name          [on exit] Location name.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
    @record_no      INT,
    @location_name  VARCHAR(100) OUTPUT
AS
    SELECT      @location_name      =   SYSTEM0100000000_data
    FROM        #master
    WHERE       Record_No           =   @record_no    

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the
                supplied site and record identifiers.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @site_id                Site identifier.
                @record_id              Record identifier.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey]
    @table_name             VARCHAR(50),
    @field_name             VARCHAR(256),
    @site_id                CHAR(8),
    @record_id              VARCHAR(6),
    @key                    CHAR(16)    OUTPUT
AS
    SET         @record_id  =   REPLICATE('0', 6 - LEN(@record_id))
                                + @record_id

    DECLARE     @previous_query     NVARCHAR(500),
                @unique_id          CHAR(2)

    SET         @previous_query =   N'SELECT @result = '
                                    + ' MAX(SUBSTRING('
                                            + @field_name + ', 9, 2))'
                                    + ' FROM #' + @table_name
                                    + ' WHERE ' + @field_name + ' LIKE '''
                                    + @site_id + '__' + @record_id + ''''

    EXECUTE     sp_executesql   @previous_query,
                                N'@result CHAR(2) OUTPUT',
                                @result = @unique_id OUTPUT
    IF @@ERROR <> 0 RETURN

    IF @unique_id IS NULL
        SET         @unique_id  =   '00'
    ELSE IF SUBSTRING(@unique_id, 2, 1) = 'Z'
        SET         @unique_id  =   dbo.IncrementChar(LEFT(@unique_id, 1)) + '0'
    ELSE
        SET         @unique_id  =   LEFT(@unique_id, 1)
                                    + dbo.IncrementChar(
                                        SUBSTRING(@unique_id, 2, 1))

    SET         @key    =   @site_id + @unique_id + @record_id
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_MapMate]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_MapMate]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the
                MapMate key specified in the import data.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @record_no              Identifies the import row.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_MapMate]
    @table_name             VARCHAR(50),
    @field_name             VARCHAR(256),
    @record_no              INT,
    @key                    CHAR(16)    OUTPUT
AS
    IF OBJECT_ID('tempdb..#master') IS NULL
    BEGIN
        RAISERROR('Missing import data (#master table does not exist)', 16, 1)
        RETURN
    END

    DECLARE     @site_id        CHAR(8),
                @record_id      VARCHAR(6)

    SELECT      @site_id    =   SYSTEM010000000E_Site_ID,
                @record_id  =   SYSTEM010000000E_Record_ID
    FROM        #master
    WHERE       Record_No       =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    EXECUTE     usp_ImportWizard_NextKey    @table_name,
                                            @field_name,
                                            @site_id,
                                            @record_id,
                                            @key        OUTPUT
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_MapMate') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_MapMate'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
            GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_MapMate TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_RecordID]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
GO

/*===========================================================================*\
  Description:  Generate a key value for an imported record using the site
                and record identifiers specified in the import data.

  Parameters:   @table_name             Name of table into which record is
                                        imported.
                @field_name             Name of field in which value must be
                                        unique.
                @record_no              Identifies the import row.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
    @table_name             VARCHAR(50),
    @field_name             VARCHAR(256),
    @record_no              INT,
    @key                    CHAR(16)    OUTPUT
AS
    IF OBJECT_ID('tempdb..#master') IS NULL
    BEGIN
        RAISERROR('Missing import data (#master table does not exist)', 16, 1)
        RETURN
    END

    DECLARE     @site_id    CHAR(8),
                @record_id  VARCHAR(6)

    SELECT      @site_id    =   SYSTEM010000000K_Site_ID,
                @record_id  =   SYSTEM010000000H_Record_ID
    FROM        #master
    WHERE       Record_No   =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    EXECUTE     usp_ImportWizard_NextKey    @table_name,
                                            @field_name,
                                            @site_id,
                                            @record_id,
                                            @key        OUTPUT
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_RecordID') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_RecordID'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_RecordID TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Individual_Select_ForNameSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Individual_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of individual names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Individual_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Individual_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Individual_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypeMatchRules_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypeMatchRules_Select]
GO

/*===========================================================================*\
  Description:  List of match rules for fields of a column type.

  Parameters:   @column_type_key        Identifies the column type.

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypeMatchRules_Select]
    @column_type_key     CHAR(16)
AS
    SELECT      Field_Index,
                IW_Match_Rule_Key
    FROM        IW_Column_Type_Match_Rule
    WHERE       IW_Column_Type_Key          =   @column_type_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypeMatchRules_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWColumnTypeMatchRules_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWColumnTypeMatchRules_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Measurement_Qualifier', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Measurement_Qualifier(
			Measurement_Qualifier_Key, Short_Name, Long_Name, Measurement_Type_Key, Entered_By
		) VALUES (
			@Key, @ImportValue, @ImportValue, 'NBNSYS0000000004', @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AbundanceQualifiers
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AbundanceQualifier'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Relationship_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Relationship_Type(
			Relationship_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, @ImportValue, @ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AssociationTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AssociationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Location') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
GO

/*===========================================================================*\
  Description:	Create a new location from an import value.

  Parameters:	
	@ImportValue	The name of the location.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS

	DECLARE	@Key char(16),
		@LNKey char(16)

	/*===================================================*\
	  Now create new Location and Location Name records
	\*===================================================*/
	BEGIN TRANSACTION
		EXECUTE spNextKey 'Location', @Key OUTPUT

		INSERT INTO Location (
			Location_Key, Location_Type_Key, Entered_By,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier
		)
		SELECT	@Key, 'NBNSYS0000000001', @EnteredBy,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier			
		FROM	#Locations
		WHERE	Import_Value = @ImportValue

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE spNextKey 'Location_Name', @LNKey OUTPUT
		INSERT INTO Location_Name (
			Location_Name_Key, Item_Name, Location_Key, Preferred, Entered_By
		) VALUES (
			@LNKey, @ImportValue, @Key, 1, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Locations
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Location') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Location'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchNewEntry_Name]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
GO

/*===========================================================================*\
  Description:	Create a new individual from an import value.

  Parameters:	
	@ImportValue	The raw name to parse and insert in database.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16),
		@Surname varchar(30),
		@Forename varchar(20),
		@Initials varchar(8),
		@Title varchar(4),
		@Working varchar(100),
		@Part varchar(50),
		@CurrentPos int,
		@NextPos int

	/*===================================================*\
	  Break down the ImportValue into name constituents.
	\*===================================================*/
	SET	@Surname = ''
	SET	@Working = @ImportValue
	SET 	@CurrentPos = 1
	SET	@NextPos = 1

	-- Rule 1. Check for 'xxx' or 'van xxx'
	IF CharIndex('.', @Working) = 0 BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		IF @NextPos = 0
			SET @Surname = @Working
		ELSE BEGIN
			SET @Surname = SubString(@Working, @CurrentPos, @NextPos - 1)
			IF @Surname IN ('van', 'von') BEGIN
				SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
				IF @NextPos = 0
					SET @Surname = @Working
				ELSE
					SET @Surname = ''
			END ELSE
				SET @Surname = ''
		END
		IF @Surname <> ''
			SET @Working = ''
	END

	-- Rule 2. Check for 'xxx, ###' or 'van xxx, ###'
	IF (@Surname = '') BEGIN
		SET @NextPos = CharIndex(',', @Working)
		IF @NextPos <> 0 BEGIN
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
			SET @Working = LTrim(SubString(@Working, @NextPos + 1, Len(@Working)))
		END
	END

	-- Rule 3. Check for '### xxx'/'###.xxx' or '### van xxx'/'###.van xxx'
	IF (@Surname = '') AND (Right(@Working, 1) <> '.') BEGIN
		SET @Working = Reverse(@Working)
		SET @NextPos = CharIndex(' ', @Working)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, 1, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working)
		-- Surname is last word.
		SET @Surname = Reverse(SubString(@Working, 1, @NextPos - 1))
		SET @CurrentPos = @NextPos + 1
		-- Check for 'van/von' too
		SET @NextPos = CharIndex(' ', @Working, @CurrentPos)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, @CurrentPos, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working, @CurrentPos)
		SET @Part = RTrim(SubString(@Working, @CurrentPos, Abs(@CurrentPos - @NextPos)))
		IF Reverse(@Part) IN ('van', 'von') BEGIN
			SET @Surname = LTrim(Reverse(SubString(@Working, 1, @NextPos - 1)))
			SET @CurrentPos = @NextPos + 1
		END

		SET @Working = Reverse(LTrim(SubString(@Working, @CurrentPos, Len(@Working))))
	END

	-- Rule 4. Check for 'xxx #.'/'xxx ###.' or 'van xxx #.'/'van xxx ###.'
	IF @Surname = '' BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		SET @Surname = SubString(@Working, 1, @NextPos - 1)
		IF @Surname IN ('van', 'von') BEGIN
			SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
		END

		SET @Working = SubString(@Working, @NextPos + 1, Len(@Working))
	END

	-- Parse remainder into Forename, Initials and Title
	SET @Part     = '';
	SET @Forename = '';
	SET @Initials = '';
	SET @Title    = '';
	SET @CurrentPos = 1

	WHILE @CurrentPos <= Len(@Working) BEGIN
		IF SubString(@Working, @CurrentPos, 1) IN (' ', '.') BEGIN
			IF Len(@Part) = 1
				SET @Initials = @Initials + @Part + '.'
			ELSE
			IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
				SET @Title = @Part
			ELSE
				SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
			SET @Part = ''
		END ELSE
			SET @Part = @Part + SubString(@Working, @CurrentPos, 1)
	
		SET @CurrentPos = @CurrentPos + 1
	END

	IF @Part <> ''  
		IF Len(@Part) = 1
			SET @Initials = @Initials + @Part + '.'
		ELSE
		IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
			SET @Title = @Part
		ELSE
			SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))

	IF @Forename = '' SET @Forename = NULL
	IF @Initials = '' SET @Initials = NULL
	IF @Title = '' SET @Title = NULL

	/*===================================================*\
	  Now create new Name and Individual records
	\*===================================================*/
	EXECUTE spNextKey 'Name', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO [Name] (	
			Name_Key, Organisation, Entered_By
		) VALUES (
			@Key, 0, @EnteredBy
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO Individual (
			Name_Key, Surname, Forename, Initials, Title, Entered_By
		) VALUES (
			@Key, @Surname, @Forename, @Initials, @Title, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Name') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Name'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Record_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Record_Type(
			Record_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, @ImportValue, @ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#RecordTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_RecordType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchNewEntry_SampleType')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
GO

/*===========================================================================*\
  Description:  Create a new item in a term list from an import value.

  Parameters:   @ImportValue            Name of new item
                @EnteredBy              Identifies current user

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
    @ImportValue    VARCHAR(100),
    @EnteredBy      CHAR(16)
AS
    DECLARE     @sample_type_key    CHAR(16)

    EXECUTE     spNextKey   'Sample_Type',
                            @sample_type_key    OUTPUT
    IF @@ERROR <> 0 RETURN

    BEGIN TRANSACTION

    INSERT INTO Sample_Type (
                Sample_Type_Key,
                Short_Name,
                Long_Name,
                Entered_By)
    VALUES      (@sample_type_key,
                @ImportValue,
                @ImportValue,
                @EnteredBy)

    IF @@ERROR <> 0 GOTO RollbackAndExit

    /* update import table with new data */
    UPDATE      #SampleTypes
    SET         Match_Value     =   Import_Value,
                Match_Key       =   @sample_type_key,
                Match_Count     =   1,
                Manual_Match    =   1,
                Remembered      =   0
    WHERE       Import_Value    =   @ImportValue

    IF @@ERROR <> 0 GOTO RollbackAndExit
    
    COMMIT TRANSACTION
    RETURN 0

RollBackAndExit:
    ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SampleType') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SampleType'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Specimen_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Specimen_Type(
			Specimen_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, @ImportValue, @ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#SpecimenTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SpecimenType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Substrate', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Substrate(
			Substrate_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, @ImportValue, @ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Substrates
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Substrate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AbundanceQualifiers') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_AbundanceQualifiers]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_AbundanceQualifiers]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Abundance_Qualifiers
	SET	Matched_Key = Match_Key
	FROM	#AbundanceQualifiers
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Abundance_Qualifiers
	SELECT		Import_Value, Match_Key
	FROM		#AbundanceQualifiers 
	LEFT JOIN	IW_Matched_Abundance_Qualifiers M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AbundanceQualifiers') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_AbundanceQualifiers'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AbundanceQualifiers TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AbundanceQualifiers TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_AbundanceQualifiers TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_AssociatedSpecies]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Associated_Species
	SET	Matched_Key = Match_Key
	FROM	#AssociatedSpecies
	WHERE	Matched_Value = Import_Value
	AND	Match_Checklist_Key = Checklist_Key
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Associated_Species
	SELECT		Import_Value, Match_Key, Checklist_Key
	FROM		#AssociatedSpecies
	LEFT JOIN	IW_Matched_Associated_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key = Checklist_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_AssociationTypes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_AssociationTypes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Association_Types
	SET	Matched_Key = Match_Key
	FROM	#AssociationTypes
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Association_Types
	SELECT		Import_Value, Match_Key
	FROM		#AssociationTypes 
	LEFT JOIN	IW_Matched_Association_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_AssociationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Biotopes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Biotopes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $ 	

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Biotopes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Biotopes
	SET	Matched_Key = Match_Key
	FROM	#Biotopes
	WHERE	Matched_Value = Import_Value
	AND	Match_Classification_Key = Classification_Key
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Biotopes
	SELECT		Import_Value, Match_Key, Classification_Key
	FROM		#Biotopes
	LEFT JOIN	IW_Matched_Biotopes M ON M.Matched_Value = Import_Value AND M.Match_Classification_Key = Classification_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Biotopes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Biotopes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Biotopes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Biotopes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Biotopes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Locations') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Locations]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Locations]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Locations
	SET	Matched_Key = Match_Key
	FROM	#Locations
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Locations
	SELECT		Import_Value, Match_Key
	FROM		#Locations 
	LEFT JOIN	IW_Matched_Locations M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Locations') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Locations'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Locations TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Locations TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Locations TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Names') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Names]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Names]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Names
	SET	Matched_Key = Match_Key
	FROM	#Names
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Names
	SELECT		Import_Value, Match_Key
	FROM		#Names
	LEFT JOIN	IW_Matched_Names M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Names') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Names'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Names TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Names TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Names TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_RecordTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_RecordTypes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_RecordTypes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Record_Types
	SET	Matched_Key = Match_Key
	FROM	#RecordTypes
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Record_Types
	SELECT		Import_Value, Match_Key
	FROM		#RecordTypes 
	LEFT JOIN	IW_Matched_Record_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_RecordTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_RecordTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_RecordTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_RecordTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_RecordTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_References') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_References]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_References]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_References
	SET	Matched_Key = Match_Key
	FROM	#References
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_References
	SELECT		Import_Value, Match_Key
	FROM		#References 
	LEFT JOIN	IW_Matched_References M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_References') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_References'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_References TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_References TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_References TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchRecord_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRecord_SampleTypes]
GO

/*===========================================================================*\
  Description:  Record matches for future imports.

  Parameters:   <none>

  Created:  July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_SampleTypes]
AS
	/* Update existing items, if they were "rematched" */
	UPDATE     m
	SET        m.Matched_Key           =   i.Match_Key
	FROM       #SampleTypes            AS  i
	INNER JOIN IW_Matched_Sample_Types AS  m
	ON         m.Matched_Value         =   i.Import_Value
	WHERE      i.Match_Key             IS NOT NULL
	AND        i.Manual_Match          = 1

	/* Add the new ones now. */
	INSERT INTO IW_Matched_Sample_Types (
	            Matched_Value,
	            Matched_Key)
	SELECT      i.Import_Value,
	            i.Match_Key
	FROM        #SampleTypes            AS  i
	LEFT JOIN   IW_Matched_Sample_Types AS  m
	ON          m.Matched_Value         =   i.Import_Value
	WHERE       m.Matched_Value         IS NULL
	AND         i.Match_Key             IS NOT NULL
	AND         i.Manual_Match          = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchRecord_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Species]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Species]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Species
	SET	Matched_Key = Match_Key
	FROM	#Species
	WHERE	Matched_Value = Import_Value
	AND	Match_Checklist_Key = Checklist_Key
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Species
	SELECT		Import_Value, Match_Key, Checklist_Key
	FROM		#Species
	LEFT JOIN	IW_Matched_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key = Checklist_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_SpecimenTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_SpecimenTypes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_SpecimenTypes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Specimen_Types
	SET	Matched_Key = Match_Key
	FROM	#SpecimenTypes
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Specimen_Types
	SELECT		Import_Value, Match_Key
	FROM		#SpecimenTypes 
	LEFT JOIN	IW_Matched_Specimen_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_SpecimenTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_SpecimenTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_SpecimenTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_SpecimenTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_SpecimenTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Substrates') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Substrates]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Substrates]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Substrates
	SET	Matched_Key = Match_Key
	FROM	#Substrates
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Substrates
	SELECT		Import_Value, Match_Key
	FROM		#Substrates 
	LEFT JOIN	IW_Matched_Substrates M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Substrates') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Substrates'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Substrates TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Substrates TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Substrates TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AbundanceQualifier') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_AbundanceQualifier]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_AbundanceQualifier]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#AbundanceQualifiers
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Measurement_Qualifier
		WHERE	Measurement_Qualifier_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#AbundanceQualifiers
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AbundanceQualifier') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_AbundanceQualifier'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AbundanceQualifier TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AbundanceQualifier TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_AbundanceQualifier TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_AssociatedSpecies]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@ChecklistKey char(16)

		-- Key value for rank 'Order' is 'NBNSYS..12'. 
		SELECT	@Order = ITN.Actual_Name
		FROM	Index_Taxon_Group ITG
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
		WHERE	ITG.Contained_List_Item_Key = @MatchKey
		AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 
	
		-- 'Not available' value handled and regionalised in app.
		IF @Order IS NULL SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@ChecklistKey = TL.Taxon_List_Key, @Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		-- And update match table.
		UPDATE	#AssociatedSpecies
		SET	Match_Value = dbo.ufn_GetFormattedSpeciesName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#AssociatedSpecies
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_AssociationType]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_AssociationType]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#AssociationTypes
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Relationship_Type
		WHERE	Relationship_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#AssociationTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_AssociationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_AssociationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_AssociationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchSet_Biotope]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchSet_Biotope]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Biotope]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Classification varchar(100),
			@ClassificationKey char(16)

		-- Get the associated checklist.
		SELECT	@ClassificationKey = BC.Biotope_Classification_Key, @Classification = BC.Short_Name
		FROM	Biotope_List_Item BLI
		JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
		JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
		WHERE	BLI.Biotope_List_Item_Key = @MatchKey

		-- And update match table.
		UPDATE	#Biotopes
		SET	Match_Value = dbo.ufn_GetFormattedBiotopeName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Classification = @Classification,
			Classification_Key = @ClassificationKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#Biotopes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			Classification = NULL,
			Classification_Key = NULL
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Biotope') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Biotope'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Location') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Location]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Location]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Locations
		SET	Match_Value = LN.Item_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Spatial_Ref = L.Spatial_Ref,
			Spatial_Ref_System = L.Spatial_Ref_System,
			Lat = L.Lat,
			Long = L.Long
		FROM	Location L
		JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
		WHERE	L.Location_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Locations
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			Spatial_Ref = NULL,
			Spatial_Ref_System = NULL,
			Lat = NULL,
			Long = NULL
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Location') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Location'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Location TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Location TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Location TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Name') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Name]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Name]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#Names
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Name') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Name'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Name TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Name TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Name TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_RecordType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_RecordType]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_RecordType]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#RecordTypes
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Record_Type
		WHERE	Record_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#RecordTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_RecordType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_RecordType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_RecordType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_RecordType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_RecordType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Reference') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Reference]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Reference]
	@ImportValue varchar(500),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#References
		SET	Match_Value = dbo.ufn_GetFormattedReferenceName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#References
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Reference') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Reference'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Reference TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Reference TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Reference TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchSet_SampleType')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchSet_SampleType]
GO

/*===========================================================================*\
  Description:  Set the match for the specified import value in the match
                table.

  Parameters:   @ImportValue            Imported value
                @MatchKey               Identifies matching record

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_SampleType]
    @ImportValue    VARCHAR(100),
    @MatchKey       CHAR(16)
AS
    IF @MatchKey IS NOT NULL
        UPDATE      i
        SET         i.Match_Value       =   t.Short_Name,
                    i.Match_Key         =   @MatchKey,
                    i.Match_Count       =   1,
                    i.Manual_Match      =   1,
                    i.Remembered        =   0
        FROM        #SampleTypes        AS  i,
                    Sample_Type         AS  t
        WHERE       i.Import_Value      =   @ImportValue
        AND         t.Sample_Type_Key   =   @MatchKey
    ELSE
        UPDATE      #SampleTypes
        SET         Match_Value         =   NULL,
                    Match_Key           =   NULL,
                    Match_Count         =   NULL,
                    Manual_Match        =   0,
                    Remembered          =   0
        WHERE       Import_Value        =   @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_SampleType') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchSet_SampleType'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@ChecklistKey char(16)

		-- Key value for rank 'Order' is 'NBNSYS..12'. 
		SELECT	@Order = ITN.Actual_Name
		FROM	Index_Taxon_Synonym ITS 
		JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
		WHERE	ITS.Taxon_List_Item_Key = @MatchKey
		AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 
	
		-- 'Not available' value handled and regionalised in app.
		IF @Order IS NULL SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@ChecklistKey = TL.Taxon_List_Key, @Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = dbo.ufn_GetFormattedSpeciesName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_SpecimenType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_SpecimenType]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import Specimen in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_SpecimenType]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#SpecimenTypes
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Specimen_Type
		WHERE	Specimen_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#SpecimenTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_SpecimenType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_SpecimenType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_SpecimenType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_SpecimenType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_SpecimenType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_SpecimenCardsOnly')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_SpecimenType TO [R2k_SpecimenCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_SpecimenType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Substrate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Substrate]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Substrate]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Substrates
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Substrate
		WHERE	Substrate_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Substrates
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Substrate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Substrate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AbundanceQualifiers') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_AbundanceQualifiers]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AbundanceQualifiers]
AS
    UPDATE  #AbundanceQualifiers
    SET Match_Count =  (SELECT  Count(*)
                FROM    Measurement_Qualifier
                WHERE   Measurement_Type_Key = 'NBNSYS0000000004'
                AND     (Import_Value = Short_Name
                OR      Import_Value = Long_Name))
    WHERE   Match_Key IS NULL
    
    UPDATE  #AbundanceQualifiers
    SET Match_Value = Short_Name,
        Match_Key = Measurement_Qualifier_Key
    FROM    Measurement_Qualifier
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND Measurement_Type_Key = 'NBNSYS0000000004'
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AbundanceQualifiers') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_AbundanceQualifiers'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_AbundanceQualifiers TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_AbundanceQualifiers TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_AbundanceQualifiers TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociationTypes') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
AS
    UPDATE  #AssociationTypes
    SET Match_Count =  (SELECT  Count(*)
                FROM    Relationship_Type
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #AssociationTypes
    SET Match_Value = Short_Name,
        Match_Key = Relationship_Type_Key
    FROM    Relationship_Type
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociationTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_AssociationTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Biotopes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Biotopes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Biotopes]
	@ChecklistKey char(16)
AS
	-- Set Match_Count first.
	UPDATE	#Biotopes
	SET	Match_Count =  (SELECT Count(*) 
				FROM Biotope B
				JOIN Biotope_List_Item BLI ON BLI.Biotope_Key = B.Biotope_Key
				JOIN Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				WHERE	BLI.BT_CL_Version_To IS NULL
				AND 	BCV.Biotope_Classification_Key = @ChecklistKey
				AND	(Import_Value = B.Short_Term
				OR	 Import_Value = B.Full_Term
				OR	 Import_Value = B.Original_Code
				OR	 Import_Value = B.Original_Code + ', ' + B.Short_Term
				OR	 Import_Value = B.Original_Code + '-' + B.Short_Term
				OR	 Import_Value = B.Original_Code + '_' + B.Short_Term)
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Biotopes
	SET	Match_Key = Biotope_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key),
		Classification = BC.Short_Name,
		Classification_Key = BC.Biotope_Classification_Key
	FROM	Biotope B
	JOIN	Biotope_List_Item BLI ON B.Biotope_Key = BLI.Biotope_Key
	JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
	JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	BLI.BT_CL_Version_To IS NULL
	AND	BC.Biotope_Classification_Key = @ChecklistKey
	AND 	(Import_Value = B.Short_Term
	OR	 Import_Value = B.Full_Term
	OR	 Import_Value = B.Original_Code
	OR	 Import_Value = B.Original_Code + ', ' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '-' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '_' + B.Short_Term)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Biotopes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Biotopes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Names') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Names]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Names]
AS
	-- Set Match_Count first.
	UPDATE	#Names
	SET	Match_Count =  (SELECT Count(*) FROM Individual
				WHERE	Import_Value = Forename + ' ' + Surname
				OR	Import_Value = Surname + ', ' + Forename
				OR	Import_Value = Surname + ' ' + Forename
				OR	Import_Value = Initials + ' ' + Surname
				OR	Import_Value = Initials + Surname
				OR	Import_Value = Surname + ', ' + Initials
				OR	Import_Value = Surname + ' ' + Initials
				OR	Import_Value = Surname
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Names
	SET	Match_Key = Name_Key,
		Match_Value = dbo.ufn_GetFormattedName(Name_Key)
	FROM	Individual
	WHERE	Match_Count = 1
	AND 	Match_Key IS NULL
	AND 	(Import_Value = Forename + ' ' + Surname
	OR	 Import_Value = Surname + ', ' + Forename
	OR	 Import_Value = Surname + ' ' + Forename
	OR	 Import_Value = Initials + ' ' + Surname
	OR	 Import_Value = Initials + Surname
	OR	 Import_Value = Surname + ', ' + Initials
	OR	 Import_Value = Surname + ' ' + Initials
	OR	 Import_Value = Surname)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Names') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Names'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_RecordTypes') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_RecordTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_RecordTypes]
AS
    UPDATE  #RecordTypes
    SET Match_Count =  (SELECT  Count(*)
                FROM    Record_Type
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #RecordTypes
    SET Match_Value = Short_Name,
        Match_Key = Record_Type_Key
    FROM    Record_Type
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_RecordTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_RecordTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_RecordTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_RecordTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_RecordTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatch_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatch_SampleTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched
                only.

  Parameters:   <none>

  Created:  July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_SampleTypes]
AS
    UPDATE      #SampleTypes
    SET         Match_Count     =  (SELECT  Count(*)
                                    FROM    Sample_Type     AS  t
                                    WHERE   t.Short_Name    =   Import_Value
                                    OR      t.Long_Name     =   Import_Value)
    WHERE       Match_Key       IS NULL

    UPDATE      i
    SET         i.Match_Value   =   t.Short_Name,
                i.Match_Key     =   t.Sample_Type_Key
    FROM        #SampleTypes    AS  i
    INNER JOIN  Sample_Type     AS  t
    ON          (t.Short_Name   =   i.Import_Value
    OR          t.Long_Name     =   i.Import_Value)
    WHERE       i.Match_Count   =   1
    AND         i.Match_Key     IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#Species
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	-- Set Match_Count first. Broken down in two separate updates for speed.
	UPDATE	UpdatedSpecies
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name 
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				)
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL

	UPDATE	UpdatedSpecies
	SET	Match_Count =  Match_Count + (
				SELECT	Count(*)
				FROM	Index_Taxon_Name ITN 
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				JOIN	#Species S ON S.Species_Name = ITN.Actual_Name + ' ' + ITN.Authority
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	S.Import_Value = UpdatedSpecies.Import_Value
				)
	FROM	#Species UpdatedSpecies
	WHERE	Match_Key IS NULL


	-- Now get values and keys for unique matches only. Broken down in tow separate updates for speed.
	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	Species_Name = Actual_Name

	UPDATE	#Species
	SET	Match_Key = ITN.Taxon_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Taxon_List_Item_Key),
		Checklist = TL.Item_Name,
		Checklist_Key = TL.Taxon_List_Key
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	TL.Taxon_List_Key = @ChecklistKey
	AND	Species_Name = Actual_Name + ' ' + ITN.Authority

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	#Species
	SET	[Order] = ITN.Actual_Name
	FROM	Index_Taxon_Synonym ITS
	JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
	JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
	AND	ITS.Taxon_List_Item_Key = Match_Key
	AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 

	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	UPDATE	#Species
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_SpecimenTypes') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_SpecimenTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_SpecimenTypes]
AS
    UPDATE  #SpecimenTypes
    SET Match_Count =  (SELECT  Count(*)
                FROM    Specimen_Type
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #SpecimenTypes
    SET Match_Value = Short_Name,
        Match_Key = Specimen_Type_Key
    FROM    Specimen_Type
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_SpecimenTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_SpecimenTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_SpecimenTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_SpecimenTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_SpecimenTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Substrates') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_Substrates]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Substrates]
AS
    UPDATE  #Substrates
    SET Match_Count =  (SELECT  Count(*)
                FROM    Substrate
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL
    
    UPDATE  #Substrates
    SET Match_Value = Short_Name,
        Match_Key = Substrate_Key
    FROM    Substrate
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Substrates') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_Substrates'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectRelatedTables]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectRelatedTables]
GO

/*===========================================================================*\
  Description:  List of related tables for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectRelatedTables]
    @table_rule_key     CHAR(16)
AS
    SELECT      Table_Name,
                Relationship
    FROM        IW_Table_Rule_Related_Table
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectRelatedTables') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectRelatedTables'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRelatedTables TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchKey
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey char(16),
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key

		WHERE 	Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%'
		ORDER BY SearchTerm
	ELSE
		SELECT	Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				AND TLV.Taxon_List_Key = @SearchKey
	
		WHERE 	Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%'
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

