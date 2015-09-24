If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptPopulateListItemKeysANDSampleKeys]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spRptPopulateListItemKeysANDSampleKeys'
        DROP PROCEDURE [dbo].[spRptPopulateListItemKeysANDSampleKeys]
    END
GO

    PRINT 'Creating procedure spRptPopulateListItemKeysANDSampleKeys'
GO

    /*
    $History: 00000012.sql $
 * 
 * *****************  Version 7  *****************
 * User: Johnvanbreda Date: 13/07/04   Time: 15:52
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Stored proc update before release
 * 
 * *****************  Version 6  *****************
 * User: Johnvanbreda Date: 5/07/04    Time: 16:49
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Import Wizard bug fixes
 * 
 * *****************  Version 1  *****************
 * User: Pollyshaw    Date: 12/02/03   Time: 14:07
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs/STORED PROCS
 * stored proc which moves the backup file from one place to another.
 * 
 * *****************  Version 5  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 4  *****************
 * User: Pollyshaw    Date: 6/02/03    Time: 12:51
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Created indexes within the procedures.
 * 
 * *****************  Version 3  *****************
 * User: Bencollier   Date: 3/12/02    Time: 13:46
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Stored Procedure now discriminates between BIOTOPE and TAXON
 * OCCURRENCE_KEYs
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 13/11/02   Time: 14:55
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Changed report table to a temporary table.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 6/11/02    Time: 14:27
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE [dbo].[spRptPopulateListItemKeysANDSampleKeys]

--
--	DESCRIPTION
--	This procedure populates the LIST_ITEM_KEY and SAMPLE_KEY fields based upon OCCURRENCE_KEY values
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 30/10/2002  
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

@Report_type as Char(1) = ' '

AS
SET NOCOUNT ON

    DECLARE	@Procedure_Name    SYSNAME	--    Holds the name of the currently executing procedure
    DECLARE	@ErrorFlag    INT         		--    Flags if there has been an error during the procedure
    DECLARE	@ErrorNo    INT         		--    Holds the @@ERROR value
    DECLARE	@ErrorMsg    VARCHAR(255)     	--    User defined error message

    --    Set the procedure name in the variable
    SELECT @Procedure_Name = OBJECT_NAME (@@PROCID)

    --    Set the @ErrorFlag variable to -1 as the default return value. 
    SELECT @ErrorFlag = -1

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
if @Report_Type <> 'B'

	UPDATE 
	#REPORT_OUTPUT
	SET #REPORT_OUTPUT.LIST_ITEM_KEY = TD.TAXON_LIST_ITEM_KEY, 
		#REPORT_OUTPUT.SAMPLE_KEY = XO.SAMPLE_KEY
	FROM
	#REPORT_OUTPUT 
	INNER JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=#Report_Output.Occurrence_Key
	INNER JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=XO.Taxon_Occurrence_Key
			AND TD.Preferred=1
	WHERE TYPE = 'T'

if @Report_Type <> 'T'
	
	UPDATE 
	#REPORT_OUTPUT
	SET #REPORT_OUTPUT.LIST_ITEM_KEY = BD.BIOTOPE_LIST_ITEM_KEY, 
		#REPORT_OUTPUT.SAMPLE_KEY = XO.SAMPLE_KEY
	FROM
	#REPORT_OUTPUT 
	INNER JOIN Biotope_Occurrence XO ON XO.Biotope_Occurrence_Key=#Report_Output.Occurrence_Key
	INNER JOIN Biotope_Determination BD ON BD.Biotope_Occurrence_Key=XO.Biotope_Occurrence_Key
			AND BD.Preferred=1
	WHERE TYPE = 'B'

--Create indices
CREATE INDEX IX_LIST_ITEM_KEY ON #REPORT_OUTPUT (LIST_ITEM_KEY)
CREATE INDEX IX_SAMPLE_KEY ON #REPORT_OUTPUT (SAMPLE_KEY)


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    SELECT @ErrorNo = @@ERROR

    IF @ErrorNo <> 0 
    BEGIN
        SELECT @ErrorMsg = 'Error number ' + CONVERT ( VARCHAR(6), @ErrorNo ) + ' in procedure ' + @Procedure_Name
        RAISERROR (@ErrorMsg, 16, -1) --WITH LOG
        GOTO PROC_RETURN
    END

    SELECT @ErrorFlag = 0

PROC_RETURN:

    RETURN @ErrorFlag
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spRptPopulateListItemKeysANDSampleKeys') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spRptPopulateListItemKeysANDSampleKeys'
         	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [Dev- JNCC SQL]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [R2k_AddOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [R2k_Administrator]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [R2k_FullEdit]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [R2k_ReadOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateListItemKeysANDSampleKeys TO [R2k_RecordCardsOnly]
    END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AbundanceQualifiers_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_AbundanceQualifiers_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of measurement qualifiers for the Abundance 
		measurement type.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AbundanceQualifiers_Select]
AS
	SELECT 		Measurement_Qualifier_Key AS Item_Key, Short_Name AS Item_Name
	FROM 		Measurement_Qualifier
	WHERE 		Measurement_Type_Key = 'NBNSYS0000000004'
	ORDER BY	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AbundanceQualifiers_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AbundanceQualifiers_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_BiotopeClassifications_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_BiotopeClassifications_Select]
GO

/*===========================================================================*\
  Description:	Returns list of biotope classifications.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BiotopeClassifications_Select]
AS
	SELECT	Biotope_Classification_Key AS Item_Key,
		IsNull(Long_Name, Short_Name) AS Item_Name
	FROM	Biotope_Classification
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeClassifications_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeClassifications_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeClassifications_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_BiotopeFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_BiotopeFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a Biotope name with checklist name.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BiotopeFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key) + ' - ' + BC.Short_Name
	FROM	Biotope_List_Item BLI
	JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
	JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	BLI.Biotope_List_Item_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_BiotopeName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_BiotopeName_Get]
GO

/*===========================================================================*\
  Description:	Returns the Short_Term field of a Biotope record.

  Parameters:	@Key		Key of record to be returned
		@Caption	Output caption

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_BiotopeName_Get] 
	@Key CHAR(16),
	@Caption VARCHAR(100) OUTPUT
AS
SET NOCOUNT ON

	SELECT 		@Caption = IsNull(Original_Code + ', ', '') + Short_Term
	FROM		Biotope AS B
	INNER JOIN	Biotope_List_item AS BLI ON BLI.Biotope_key = B.biotope_key 
	WHERE		BLI.Biotope_List_item_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [Dev - JNCC SQL]
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
	@SearchText varchar(100)
AS
	SELECT	Biotope_List_Item_Key AS Item_Key,
		dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key) AS SearchTerm
	FROM	Biotope_List_Item BLI
	JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
	WHERE 	Short_Term LIKE @SearchText + '%'
	OR	Original_Code LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key) LIKE @SearchText + '%'
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Biotope_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Biotope_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Biotope_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc. for searching on the Biotope names

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

SET NOCOUNT ON

	SELECT 		Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS SearchTerm
	FROM 		Biotope AS B
	INNER JOIN	Biotope_List_Item AS BLI ON BLI.Biotope_Key = B.Biotope_Key
	WHERE 		(Original_Code + ' - ' + Full_Term LIKE @SearchText + '%')
	OR		(Full_Term LIKE @SearchText + '%')
	ORDER BY 	IsNull(Original_Code + ' - ' + Full_Term, Full_Term)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotope_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotope_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ColumnTitleMatches_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ColumnTitleMatches_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of possible column type matches for a column title

  Parameters:	@ColumnTitle

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ColumnTitleMatches_Select]
	@ColumnTitle VARCHAR(50)
AS

DECLARE @Count INT

-- Try getting an exact match against the user-specified matches first
SELECT @Count=Count(*) 
FROM IW_Column_Type_Pattern WHERE @ColumnTitle=Pattern AND Exclude_Match=0

IF @Count=1
	SELECT CT.IW_Column_Type_Key, CT.Item_Name
	FROM IW_Column_Type CT
	INNER JOIN IW_Column_Type_Pattern CTP ON CTP.IW_Column_Type_Key=CT.IW_Column_Type_Key
	WHERE @ColumnTitle=CTP.Pattern AND CTP.Exclude_Match=0
ELSE
	-- No match to user specified title, so use like to get pattern match
	SELECT IW_Column_Type_Key, Item_Name
	FROM IW_Column_Type
	WHERE IW_Column_Type_Key IN 
			(SELECT IW_Column_Type_Key FROM IW_Column_Type_Pattern WHERE @ColumnTitle LIKE Pattern AND Exclude_Match=0)
	AND IW_Column_Type_Key NOT IN 
			(SELECT IW_Column_Type_Key FROM IW_Column_Type_Pattern WHERE @ColumnTitle LIKE Pattern AND Exclude_Match=1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ColumnTitleMatches_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_ColumnTitleMatches_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ColumnTitleMatches_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizardGroup_ColumnProcessed]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizardGroup_ColumnProcessed]
GO

/*===========================================================================*\
  Description:  Check whether the specified column type has been processed
                in #IW_Group.

  Parameters:   @column_type_key        Identifies the column type
                @column_processed       [On exit] Indicates whether the column
                                        type has been processed

  Created:      June 2004

  Last revision:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizardGroup_ColumnProcessed]
    @column_type_key    CHAR(16),
    @column_processed   BIT         OUTPUT
AS
    IF EXISTS ( SELECT      1
                FROM        #IW_Group
                WHERE       Column_Type_Key =   @column_type_key)
    BEGIN
        SET         @column_processed   =   1
    END
    ELSE
    BEGIN
        SET         @column_processed   =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizardGroup_ColumnProcessed') AND SysStat & 0xf = 4)
BEGIN
        PRINT 'Setting up security on procedure usp_ImportWizardGroup_ColumnProcessed'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
            GRANT EXECUTE ON dbo.usp_ImportWizardGroup_ColumnProcessed TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizardGroup_Insert]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizardGroup_Insert]
GO

/*===========================================================================*\
  Description:  Adds a record to #IW_Group.

  Parameters:   @column_type_key        Identifies the column type
                @qualification          Column type qualification
                @record_no              Identifies the imported record
                @group_id               Identifies the group of values
                @group_hash             Hash of grouped values

  Created:      June 2004

  Last revision:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizardGroup_Insert]
    @column_type_key    CHAR(16),
    @qualification      VARCHAR(50),
    @record_no          INT,
    @group_id           INT,
    @group_hash         INT
AS
    /* work around BaseADODataModule crapness that makes it impossible
       to pass an empty string as an argument */
    IF @qualification IS NULL
        SET         @qualification  =   ''

    INSERT INTO #IW_Group (
                Column_Type_Key,
                Qualification,
                Record_No,
                Group_Hash,
                Group_ID)
    VALUES      (@column_type_key,
                @qualification,
                @record_no,
                @group_hash,
                @group_id)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizardGroup_Insert') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizardGroup_Insert'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizardGroup_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]
GO

/*===========================================================================*\
  Description:  Set the spatial reference fields of any records in
                #Survey_Event whose samples all have the same spatial
                reference to the values from the sample.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]
AS
    UPDATE      #Survey_Event
    SET         Spatial_Ref             =   p.Spatial_Ref,
                Spatial_Ref_System      =   p.Spatial_Ref_System,
                Lat                     =   p.Lat,
                Long                    =   p.Long,
                Spatial_Ref_Qualifier   =   p.Spatial_Ref_Qualifier
    FROM        #Survey_Event           AS  e
    INNER JOIN  #Sample                 AS  p
    ON          p.Survey_Event_Key      =   e.Survey_Event_Key
    WHERE       NOT EXISTS (
                    SELECT      1
                    FROM        #Sample             AS  p2
                    WHERE       p2.Survey_Event_Key =   p.Survey_Event_Key
                    AND         p2.Spatial_Ref      <>  p.Spatial_Ref)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CalculateSurveyEventSpatialRef'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CalculateZeroAbundance]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CalculateZeroAbundance]
GO

/*===========================================================================*\
  Description:  Set the Zero_Abundance flag on #Taxon_Occurrence where there
                exist numeric abundance data and they are all zero.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CalculateZeroAbundance]
AS
    UPDATE      #Taxon_Occurrence
    SET         Zero_Abundance      =   1
    FROM        #Taxon_Occurrence   AS  o
    WHERE       (SELECT     SUM(CASE WHEN CAST(d.Data AS REAL) = 0.0
                                    THEN 0
                                    ELSE 1
                            END)
                FROM        #Taxon_Occurrence_Data      AS  d
                INNER JOIN  Measurement_Qualifier       AS  q
                ON          q.Measurement_Qualifier_Key =   d.Measurement_Qualifier_Key
                WHERE       d.Taxon_Occurrence_Key      =   o.Taxon_Occurrence_Key
                AND         q.Measurement_Type_Key      =   'NBNSYS0000000004' /* Abundance */
                AND         ISNUMERIC(d.Data)           =   1)
                =           0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CalculateZeroAbundance') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CalculateZeroAbundance'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CheckTableName]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
GO

/*===========================================================================*\
  Description:  Is the named table valid in an import/export database?

  Parameters:   @table_name             Table name
                @is_expected            [on exit] Is the table expected?

  Created:      July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
    @table_name     VARCHAR(30),
    @is_expected    BIT             OUTPUT
AS
    IF EXISTS ( SELECT      1
                FROM        DATABASE_RELATIONSHIP
                WHERE       MASTER_TABLE            =   @table_name
                OR          DETAIL_TABLE            =   @table_name)
        SET         @is_expected    =   1
    ELSE
        SET         @is_expected    =   0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CheckTableName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CheckTableName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetAssociatedOccurrence]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetAssociatedOccurrence]
GO

/*===========================================================================*\
  Description:  Identify the taxon occurrence record generated for the
                associated species of an import record.

  Parameters:   @record_no              Identifies the import record.
                @taxon_occurrency_key   [on exit] Taxon occurrence identifier.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetAssociatedOccurrence]
    @record_no              INT,
    @taxon_occurrence_key   CHAR(16)    OUTPUT
AS
    DECLARE     @sample_key             CHAR(16),
                @taxon_list_item_key    CHAR(16)

    DECLARE     @dummy                  INT,
                @message                VARCHAR(1000)

    SELECT      @dummy              =   1
    FROM        #master
    WHERE       Record_NO         =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing master record: ' + LTRIM(STR(@record_no))
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @dummy              =   1
    FROM        #RN_Sample
    WHERE       Record_No               =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing sample record: ' + LTRIM(STR(@record_no))
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @dummy                  =   1
    FROM        #master                 AS  i
    INNER JOIN  #AssociatedSpecies                AS  m
    ON          m.Import_Value          =   i.SYSTEM0100000008_data
    WHERE       i.Record_No             =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing species record: ' + LTRIM(STR(@record_no))
                + ':' + SYSTEM0100000008_data + ':'
        FROM    #master
        WHERE   Record_No = @record_no
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @sample_key             =   s.Sample_Key,
                @taxon_list_item_key    =   m.Match_Key
    FROM        #master                 AS  i
    INNER JOIN  #RN_Sample              AS  s
    ON          s.Record_No             =   i.Record_No
    INNER JOIN  #AssociatedSpecies      AS  m
    ON          m.Import_Value          =   i.SYSTEM0100000008_data
    WHERE       i.Record_No             =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    SELECT      @taxon_occurrence_key   =   o.Taxon_Occurrence_Key
    FROM        #Taxon_Occurrence       AS  o
    INNER JOIN  #Taxon_Determination    AS  d
    ON          d.Taxon_Occurrence_Key  =   o.Taxon_Occurrence_Key
    WHERE       o.Sample_Key            =   @sample_key
    AND         d.Taxon_List_Item_Key   =   @taxon_list_item_key

    IF @@ROWCOUNT = 0 GOTO MissingAssociatedSpecies
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN

MissingAssociatedSpecies:
    RAISERROR ('Missing associated species', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetAssociatedOccurrence') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetAssociatedOccurrence'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetDate]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_GetDate]
GO

/*===========================================================================*\
  Description:	Obtain the date components from an imported record.

  Parameters:	@record_no				Identifies the import row.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetDate]
	@record_no			INT
AS
	SELECT		SYSTEM0100000002_Vague_Date_Start	AS	Start,
				SYSTEM0100000002_Vague_Date_End		AS	"End",
				SYSTEM0100000002_Vague_Date_Type	AS	Type
	FROM		#master
	WHERE		Record_No							=	@record_no

	IF @@ROWCOUNT = 0 GOTO NoSuchRecord
	RETURN

NoSuchRecord:
	RAISERROR ('Unknown import record', 16, 1)
	RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetDate') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_ImportWizard_GetDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [Dev - JNCC SQL]
END

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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
    @record_no      INT,
    @location_name  VARCHAR(100) OUTPUT
AS
    SELECT      @location_name      =   'SYSTEM0100000000_data'
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
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetLocationSpatialRef]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetLocationSpatialRef]
GO

/*===========================================================================*\
  Description:  Obtain the spatial reference details for a location.

  Parameters:   @location_key           Identifies the location.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationSpatialRef]
    @location_key       CHAR(16)
AS
    SELECT      SPATIAL_REF,
                SPATIAL_REF_SYSTEM,
                LAT,
                LONG,
                SPATIAL_REF_QUALIFIER
    FROM        Location
    WHERE       Location_Key        =   @location_key    

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationSpatialRef') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationSpatialRef'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetViceCountyLocation]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetViceCountyLocation]
GO

/*===========================================================================*\
  Description:  Obtain a location by vice county number.

  Parameters:   @vice_county_no         Vice county number
                @location_key           [on exit] Identifies the corresponding
                                        location from the import row.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetViceCountyLocation]
    @vice_county_no     VARCHAR(20),
    @location_key       CHAR(16)    OUTPUT
AS
    SELECT      @location_key       =   Location_Key
    FROM        Location
    WHERE       File_Code           =   @vice_county_no
    AND         Location_Type_Key   =   'JNCCIMPW00000001'

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetViceCountyLocation') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetViceCountyLocation'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetViceCountyLocation TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetViceCountyLocation TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetViceCountyLocation TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey]
GO

/*===========================================================================*\
  Description:	Generate a key value for an imported record using the
				supplied site and record identifiers.

  Parameters:	@table_name				Name of table into which record is
										imported.
				@site_id				Site identifier.
				@record_id				Record identifier.
				@key					[on exit] New key value.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey]
	@table_name				VARCHAR(50),
	@site_id				CHAR(8),
	@record_id				VARCHAR(6),
	@key					CHAR(16)	OUTPUT
AS
	SET			@record_id	=	REPLICATE('0', 6 - LEN(@record_id))
								+ @record_id

	DECLARE		@previous_query		NVARCHAR(100),
				@unique_id			CHAR(2)

	SET			@previous_query	=	N'SELECT @result = '
									+ ' MAX(SUBSTRING(IW_Key, 9, 2))'
									+ ' FROM #' + @table_name
									+ ' WHERE IW_Key LIKE '''
									+ @site_id + '__' + @record_id + ''''

	EXECUTE		sp_executesql	@previous_query,
								N'@result CHAR(2) OUTPUT',
								@result = @unique_id OUTPUT
	IF @@ERROR <> 0 RETURN

	IF @unique_id IS NULL
		SET			@unique_id	=	'00'
	ELSE IF SUBSTRING(@unique_id, 2, 1) = 'Z'
		SET			@unique_id	=	dbo.IncrementChar(LEFT(@unique_id, 1)) + '0'
	ELSE
		SET			@unique_id	=	LEFT(@unique_id, 1)
									+ dbo.IncrementChar(
										SUBSTRING(@unique_id, 2, 1))

	SET			@key	=	@site_id + @unique_id + @record_id
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
                @record_no              Identifies the import row.
                @key                    [on exit] New key value.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_MapMate]
    @table_name             VARCHAR(50),
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
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
GO

/*===========================================================================*\
  Description:	Generate a key value for an imported record using the site
				and record identifiers specified in the import data.

  Parameters:	@table_name				Name of table into which record is
										imported.
				@record_no				Identifies the import row.
				@key					[on exit] New key value.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_RecordID]
	@table_name				VARCHAR(50),
	@record_no				INT,
	@key					CHAR(16)	OUTPUT
AS
	IF OBJECT_ID('tempdb..#master') IS NULL
	BEGIN
		RAISERROR('Missing import data (#master table does not exist)', 16, 1)
		RETURN
	END

	DECLARE		@site_id	CHAR(8),
				@record_id	VARCHAR(6)

	SELECT		@site_id	=	SYSTEM010000000K_Site_ID,
				@record_id	=	SYSTEM010000000H_Record_ID
	FROM		#master
	WHERE		Record_No	=	@record_no

	IF @@ROWCOUNT = 0 GOTO NoSuchRecord

	EXECUTE		usp_ImportWizard_NextKey	@table_name,
											@site_id,
											@record_id,
											@key		OUTPUT
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
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_SiteID]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_SiteID]
GO

/*===========================================================================*\
  Description:	Generate a key value for an imported record using the site
				identifier specified in the import data.

  Parameters:	@table_name				Name of table into which record is
										imported.
				@record_no				Identifies the import row.
				@key					[on exit] New key value.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_SiteID]
	@table_name				VARCHAR(50),
	@record_no				INT,
	@key					CHAR(16)	OUTPUT
AS
	IF OBJECT_ID('tempdb..#master') IS NULL
	BEGIN
		RAISERROR('Missing import data (#master table does not exist)', 16, 1)
		RETURN
	END

	DECLARE		@site_id	CHAR(8)

	SELECT		@site_id	=	SYSTEM010000000K_Site_ID
	FROM		#master
	WHERE		Record_No	=	@record_no

	IF @@ROWCOUNT = 0 GOTO NoSuchRecord

	EXECUTE		spNextKey	@table_name,
							@key		OUTPUT,
							@site_id
	RETURN

NoSuchRecord:
	RAISERROR ('Unknown import record', 16, 1)
	RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_SiteID') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_SiteID'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]
GO

/*===========================================================================*\
  Description:  List stored procedures that must be run after the table rules
                have been applied.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]
AS
    SELECT      Procedure_Name
    FROM        IW_Post_Processing_Procedure
    WHERE       Required_Table_Name                     IS NULL
    OR          (SELECT     object_id(
                                'tempdb..#'
                                + Required_Table_Name)) IS NOT NULL
    ORDER BY    Sequence
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_PostTableRuleProcedures_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_PostTableRuleProcedures_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypeRelationships_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypeRelationships_Select]
GO

/*===========================================================================*\
  Description:	Returns details of all column types that are related to a
		column type.  For example, dependencies, conflicts.

  Parameters:	@Key - IW_Column_Type_Key

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypeRelationships_Select]
	@Key CHAR(16)
AS

SELECT Related_IW_Column_Type_Key, Relationship_Type
FROM IW_Column_Type_Relationship
WHERE IW_Column_Type_Key=@Key
UNION
SELECT IW_Column_Type_Key, 4
FROM IW_Column_Type_Relationship
WHERE Related_IW_Column_Type_Key=@Key
AND Relationship_Type=3

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypeRelationships_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWColumnTypeRelationships_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of all column types available for the import 
			wizard, just the key value and classname

  Parameters:	None

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypes_Select]
AS

SELECT IW_Column_Type_Key, Class_Name
FROM IW_Column_Type
ORDER BY [Sequence]

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypes_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWColumnTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnType_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnType_Select]
GO

/*===========================================================================*\
  Description:  Returns details of a single import wizard column type

  Parameters:   @Key - IW_Column_Type_Key

  Created:  May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnType_Select]
    @Key CHAR(16)
AS

    SELECT
        Item_Name,
        Required,
        Commonly_Used,
        Parser_Class_Name,
        Term_List_Table,
        Field_Type,
        Maximum_Length
    FROM    IW_Column_Type
    WHERE   IW_Column_Type_Key=@Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnType_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWColumnType_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Abundance_Qualifiers
	SELECT		Import_Value, Match_Key
	FROM		#AbundanceQualifiers 
	LEFT JOIN	IW_Matched_Abundance_Qualifiers M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Associated_Species
	SELECT		Import_Value, Match_Key, Checklist_Key
	FROM		#AssociatedSpecies
	LEFT JOIN	IW_Matched_Associated_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key = Checklist_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Association_Types
	SELECT		Import_Value, Match_Key
	FROM		#AssociationTypes 
	LEFT JOIN	IW_Matched_Association_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Biotopes
	SELECT		Import_Value, Match_Key, Classification_Key
	FROM		#Biotopes
	LEFT JOIN	IW_Matched_Biotopes M ON M.Matched_Value = Import_Value AND M.Match_Classification_Key = Classification_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Locations
	SELECT		Import_Value, Match_Key
	FROM		#Locations 
	LEFT JOIN	IW_Matched_Locations M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Names
	SELECT		Import_Value, Match_Key
	FROM		#Names
	LEFT JOIN	IW_Matched_Names M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Record_Types
	SELECT		Import_Value, Match_Key
	FROM		#RecordTypes 
	LEFT JOIN	IW_Matched_Record_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_References
	SELECT		Import_Value, Match_Key
	FROM		#References 
	LEFT JOIN	IW_Matched_References M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_SampleTypes]
AS
    /* Update existing items, if they were "rematched" */
    UPDATE      m
    SET         m.Matched_Key           =   i.Match_Key
    FROM        #SampleTypes            AS  i
    INNER JOIN  IW_Matched_Sample_Types AS  m
    ON          m.Matched_Value         =   i.Import_Value
    WHERE       i.Match_Key             IS NOT NULL

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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Species
	SELECT		Import_Value, Match_Key, Checklist_Key
	FROM		#Species
	LEFT JOIN	IW_Matched_Species M ON M.Matched_Value = Import_Value AND M.Match_Checklist_Key = Checklist_Key
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Specimen_Types
	SELECT		Import_Value, Match_Key
	FROM		#SpecimenTypes 
	LEFT JOIN	IW_Matched_Specimen_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Substrates
	SELECT		Import_Value, Match_Key
	FROM		#Substrates 
	LEFT JOIN	IW_Matched_Substrates M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AbundanceQualifiers') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_AbundanceQualifiers]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_AbundanceQualifiers]
AS
	-- Update temp table with relevant data.
	UPDATE 	#AbundanceQualifiers
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Abundance_Qualifiers 
	JOIN	Measurement_Qualifier ON Measurement_Qualifier_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AbundanceQualifiers') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_AbundanceQualifiers'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_AssociatedSpecies]
	@ChecklistKey char(16)
AS
	-- Grab checklist name frist. Avoid joins on update query.
	DECLARE	@Checklist varchar(100)

	SELECT	@Checklist = Item_Name
	FROM	Taxon_List
	WHERE	Taxon_List_Key = @ChecklistKey

	-- Update temp table with relevant data.
	UPDATE 	#AssociatedSpecies
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = @Checklist,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Associated_Species
	JOIN	Taxon_List_Item ON Taxon_List_Item_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
	AND	Match_Checklist_Key = @ChecklistKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_AssociationTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_AssociationTypes]
AS
	-- Update temp table with relevant data.
	UPDATE 	#AssociationTypes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Association_Types 
	JOIN	Relationship_Type ON Relationship_Type_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_AssociationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Biotopes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Biotopes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Biotopes]
	@ChecklistKey char(16)
AS
	-- Grab Classification name frist. Avoid joins on update query.
	DECLARE	@Classification varchar(100)

	SELECT	@Classification = Short_Name
	FROM	Biotope_Classification
	WHERE	Biotope_Classification_Key = @ChecklistKey

	-- Update temp table with relevant data.
	UPDATE 	#Biotopes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedBiotopeName(Matched_Key),
		Remembered = 1,
		Classification = @Classification,
		Classification_Key = @ChecklistKey
	FROM 	IW_Matched_Biotopes
	JOIN	Biotope_List_Item ON Biotope_List_Item_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
	AND	Match_Classification_Key = @ChecklistKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Biotopes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Biotopes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Locations') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = LN.Item_Name,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	IW_Matched_Locations 
	JOIN	Location L ON L.Location_Key = Matched_Key
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Locations') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Locations'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Locations TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Locations TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Locations TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Names') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Names]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Names]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Names
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedName(Matched_Key), 
		Remembered = 1
	FROM 	IW_Matched_Names 
	JOIN	[Name] ON Name_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Names') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Names'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Names TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Names TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Names TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_RecordTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_RecordTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_RecordTypes]
AS
	-- Update temp table with relevant data.
	UPDATE 	#RecordTypes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Record_Types 
	JOIN	Record_Type ON Record_Type_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_RecordTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_RecordTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_RecordTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_RecordTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_RecordTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_References') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_References]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_References]
AS
	-- Update temp table with relevant data.
	UPDATE 	#References
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedReferenceName(Matched_Key), 
		Remembered = 1
	FROM 	IW_Matched_References 
	JOIN	Reference ON Source_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_References') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_References'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_References TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_References TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_References TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchRemembered_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRemembered_SampleTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values from previous
                imports.

  Parameters:   <none>

  Created:      July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_SampleTypes]
AS
    UPDATE      i
    SET         i.Match_Count           =   1,
                i.Match_Key             =   m.Matched_Key,
                i.Match_Value           =   t.Short_Name,
                i.Remembered            =   1
    FROM        #SampleTypes            AS  i
    INNER JOIN  IW_Matched_Sample_Types AS  m
    ON          m.Matched_Value         =   i.Import_Value
    INNER JOIN  Sample_Type             AS  t
    ON          t.Sample_Type_Key       =   m.Matched_Key
    WHERE       i.Match_Key             IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchRemembered_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
	@ChecklistKey char(16)
AS
	-- Grab checklist name frist. Avoid joins on update query.
	DECLARE	@Checklist varchar(100)

	SELECT	@Checklist = Item_Name
	FROM	Taxon_List
	WHERE	Taxon_List_Key = @ChecklistKey

	-- Update temp table with relevant data.
	UPDATE 	#Species
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = @Checklist,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Species
	JOIN	Taxon_List_Item ON Taxon_List_Item_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
	AND	Match_Checklist_Key = @ChecklistKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_SpecimenTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_SpecimenTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_SpecimenTypes]
AS
	-- Update temp table with relevant data.
	UPDATE 	#SpecimenTypes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Specimen_Types 
	JOIN	Specimen_Type ON Specimen_Type_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_SpecimenTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_SpecimenTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SpecimenTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SpecimenTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SpecimenTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Substrates') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Substrates]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Substrates]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Substrates
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Substrates 
	JOIN	Substrate ON Substrate_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Substrates') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Substrates'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Substrates TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Substrates TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Substrates TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRuleKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRuleKey_Get]
GO

/*===========================================================================*\
  Description:	returns the match rule key for a supplied column type key
		 and field index

  Parameters:	@Key - IW_ColumnTypeKey_Key
		@Index - index of field produced by parser

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRuleKey_Get]
	@Key CHAR(16),
	@Index INT,
	@MatchRuleKey CHAR(16) OUTPUT
AS

SELECT @MatchRuleKey = IW_Match_Rule_Key
FROM IW_Column_Type_Match_Rule
WHERE IW_Column_Type_Key=@Key
AND Field_Index = @Index

IF @MatchRuleKey IS NULL
	SET @MatchRuleKey=''
	

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRuleKey_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchRuleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRule_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRule_Select]
GO

/*===========================================================================*\
  Description:	returns details of a match rule

  Parameters:	@Key - IW_Match_Rule_Key

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRule_Select]
	@Key CHAR(16)
AS
	SELECT 	[Sequence], 
		Item_Name, 
		Control_Type,
		Imported_Data_Insert_Sql,
		Remembered_Matches_Procedure,
		Match_Procedure,
		Record_Matches_Procedure,
		New_Entry_Procedure,
		Requires_Checklist,
		Set_Match_Procedure,
		Table_Create_Sql,
		Key_To_Caption_Procedure,
		Search_Type,
		Checklists_Select_Procedure,
		Termlist_Select_Procedure
	FROM 	IW_Match_Rule
	WHERE 	IW_Match_Rule_Key = @Key
	ORDER BY [Sequence]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRule_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchRule_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [Dev - JNCC SQL]
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		FROM	Measurement_Qualifier
		WHERE	Measurement_Qualifier_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#AbundanceQualifiers
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		FROM	Relationship_Type
		WHERE	Relationship_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#AssociationTypes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0,
			Classification = @Classification,
			Classification_Key = @ClassificationKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#Biotopes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_LocationCentroid') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_LocationCentroid]
GO

/*===========================================================================*\
  Description:	Set the centroid of a location that hasn't been matched to one
		already existing in Recorder. These values will be used when 
		making a new entry in Location table

  Parameters:	@ImportValue
		@SpatialRef
		@SpatialRefSystem
		@Lat
		@Long

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_LocationCentroid]
	@ImportValue varchar(100),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20)
AS
	-- And update match table.
	UPDATE	#Locations
	SET	Spatial_Ref = @SpatialRef,
		Spatial_Ref_System = @SpatialRefSystem,
		Lat = @Lat,
		Long = @Long,
		Spatial_Ref_Qualifier = @SpatialRefQualifier
	WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_LocationCentroid') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_LocationCentroid'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [Dev - JNCC SQL]
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#Names
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		FROM	Record_Type
		WHERE	Record_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#RecordTypes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#References
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		FROM	Specimen_Type
		WHERE	Specimen_Type_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#SpecimenTypes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
			Remembered = 0
		FROM	Substrate
		WHERE	Substrate_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Substrates
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
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
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AbundanceQualifiers]
AS
	UPDATE 	#AbundanceQualifiers
	SET	Match_Count =  (SELECT	Count(*)
				FROM 	Measurement_Qualifier
				WHERE 	Measurement_Type_Key = 'NBNSYS0000000004'
				AND	Import_Value = Short_Name
				)
	WHERE	Match_Key IS NULL
	
	UPDATE 	#AbundanceQualifiers
	SET	Match_Value = Short_Name,
		Match_Key = Measurement_Qualifier_Key
	FROM	Measurement_Qualifier
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Measurement_Type_Key = 'NBNSYS0000000004'
	AND	Import_Value = Short_Name
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#AssociatedSpecies
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	-- Set Match_Count first.
	UPDATE	#AssociatedSpecies
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	(Species_Name = ITN.Actual_Name
				OR	 Species_Name = ITN.Actual_Name + ' ' + ITN.Authority))
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#AssociatedSpecies
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
	AND	(Species_Name = Actual_Name
	OR	 Species_Name = Actual_Name + ' ' + ITN.Authority)

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	#AssociatedSpecies
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
	UPDATE	#AssociatedSpecies
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
AS
	UPDATE 	#AssociationTypes
	SET	Match_Count =  (SELECT	Count(*)
				FROM 	Relationship_Type
				WHERE 	Import_Value = Short_Name
				)
	WHERE	Match_Key IS NULL
	
	UPDATE 	#AssociationTypes
	SET	Match_Value = Short_Name,
		Match_Key = Relationship_Type_Key
	FROM	Relationship_Type
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value = Short_Name
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
				WHERE	BCV.Biotope_Classification_Key = @ChecklistKey
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Locations') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_Locations]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Locations]
AS
	DECLARE @EarthRadiusKM float
	SET 	@EarthRadiusKM = 6378
				
	-- Set Match_Count first.
	UPDATE	#Locations
	SET	Match_Count =  (SELECT	Count(DISTINCT L.Location_Key)
				FROM	#Master M
				JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
				JOIN	Location L ON L.Location_Key = LN.Location_Key
				JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
				WHERE	M.SYSTEM0100000000_Data = Import_Value
				AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
						COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
						SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Locations
	SET	Match_Key = L.Location_Key,
		Match_Value = LN2.Item_Name,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM	#Master M
	JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
	JOIN	Location L ON L.Location_Key = LN.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	M.SYSTEM0100000000_Data = Import_Value
	AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Locations') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Locations'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [Dev - JNCC SQL]
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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
				OR	Import_Value = Initials
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
	OR	 Import_Value = Initials)
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
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_RecordTypes]
AS
	UPDATE 	#RecordTypes
	SET	Match_Count =  (SELECT	Count(*)
				FROM 	Record_Type
				WHERE 	Import_Value = Short_Name
				)
	WHERE	Match_Key IS NULL
	
	UPDATE 	#RecordTypes
	SET	Match_Value = Short_Name,
		Match_Key = Record_Type_Key
	FROM	Record_Type
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value = Short_Name
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
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_References') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_References]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_References]
AS
	UPDATE	#References
	SET	Match_Count =  (SELECT Count(*)
				FROM	Reference R
				JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
				WHERE 	Import_Value LIKE '%' + Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) + '%'
				AND 	Import_Value LIKE '%' + Replace(Cast(Title AS varchar(1000)), ' ', '%') + '%'
				AND	Import_Value LIKE '%' + Author + '%'
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#References
	SET	Match_Key = R.Source_Key,
		Match_Value = dbo.ufn_GetFormattedReferenceName(R.Source_Key)
	FROM	Reference R
	JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
	WHERE 	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value  LIKE '%' + Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) + '%'
	AND 	Import_Value  LIKE '%' + Replace(Cast(Title AS varchar(1000)), ' ', '%') + '%'
	AND	Import_Value  LIKE '%' + Author + '%'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_References') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_References'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_References TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_References TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_References TO [Dev - JNCC SQL]
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_SampleTypes]
AS
    UPDATE      #SampleTypes
    SET         Match_Count     =  (SELECT  Count(*)
                                    FROM    Sample_Type     AS  t
                                    WHERE   t.Short_Name    =   Import_Value)
    WHERE       Match_Key       IS NULL

    UPDATE      i
    SET         i.Match_Value   =   t.Short_Name,
                i.Match_Key     =   t.Record_Type_Key
    FROM        #SampleTypes    AS  i
    INNER JOIN  Record_Type     AS  t
    ON          t.Short_Name    =   i.Import_Value
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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

	-- Set Match_Count first.
	UPDATE	#Species
	SET	Match_Count =  (SELECT	Count(*)
				FROM	Index_Taxon_Name ITN
				JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
				WHERE	TLV.Taxon_List_Key = @ChecklistKey
				AND	(Species_Name = ITN.Actual_Name
				OR	 Species_Name = ITN.Actual_Name + ' ' + ITN.Authority))
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
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
	AND	(Species_Name = Actual_Name
	OR	 Species_Name = Actual_Name + ' ' + ITN.Authority)

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
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_SpecimenTypes]
AS
	UPDATE 	#SpecimenTypes
	SET	Match_Count =  (SELECT	Count(*)
				FROM 	Specimen_Type
				WHERE 	Import_Value = Short_Name
				)
	WHERE	Match_Key IS NULL
	
	UPDATE 	#SpecimenTypes
	SET	Match_Value = Short_Name,
		Match_Key = Specimen_Type_Key
	FROM	Specimen_Type
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value = Short_Name
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
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Substrates]
AS
	UPDATE 	#Substrates
	SET	Match_Count =  (SELECT	Count(*)
				FROM 	Substrate
				WHERE 	Import_Value = Short_Name
				)
	WHERE	Match_Key IS NULL
	
	UPDATE 	#Substrates
	SET	Match_Value = Short_Name,
		Match_Key = Substrate_Key
	FROM	Substrate
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value = Short_Name
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRules_Select]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWTableRules_Select]
GO

/*===========================================================================*\
  Description:	List of table rules, in the order they must be applied.

  Parameters:	None

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRules_Select]
AS
	SELECT		IW_Table_Rule_Key,
				Table_Name,
				Filter_Expression
	FROM		IW_Table_Rule
	ORDER BY	Sequence
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRules_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWTableRules_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [Dev - JNCC SQL]
END


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_Get]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWTableRule_Get]
GO

/*===========================================================================*\
  Description:	A table rule.

  Parameters:	@table_rule_key			Identifies the table rule.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_Get]
	@table_rule_key		CHAR(16)
AS
	SELECT		Table_Name,
				Filter_Expression
	FROM		IW_Table_Rule
	WHERE		IW_Table_Rule_Key	=	@table_rule_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_Get') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_IWTableRule_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [Dev - JNCC SQL]
END


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectGeneratingFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingFields]
GO

/*===========================================================================*\
  Description:  List of generating column types for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      IW_Column_Type_Key
    FROM        IW_Table_Rule_Related_Field
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
    AND         Relationship                    =   1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectGeneratingFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectGeneratingFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingFields TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectGeneratingTables]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingTables]
GO

/*===========================================================================*\
  Description:	List of generating tables for a table rule.

  Parameters:	@table_rule_key			Identifies the table rule.

  Created:		June 2004

  Last revision information:
	$Revision: 7 $
	$Date: 13/07/04 15:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectGeneratingTables]
	@table_rule_key		CHAR(16)
AS
	SELECT		Table_Name
	FROM		IW_Table_Rule_Generating_Table
	WHERE		IW_Table_Rule_Key				=	@table_rule_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectGeneratingTables') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWTableRule_SelectGeneratingTables'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingTables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingTables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_IWTableRule_SelectGeneratingTables TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectOutputFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectOutputFields]
GO

/*===========================================================================*\
  Description:  List of output fields for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectOutputFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      f.IW_Output_Field_Key,
                f.Name,
                f.Data_Type,
                f.IW_Column_Type_Key,
                f.Source_Field_Name,
                f.Generating_Class_Name,
                f.Generator_Field_Index,
                f.Literal_Value
    FROM        IW_Table_Rule_Output_Field  AS  rf
    INNER JOIN  IW_Output_Field             AS  f
    ON          f.IW_Output_Field_Key       =   rf.IW_Output_Field_Key
    WHERE       rf.IW_Table_Rule_Key        =   @table_rule_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectOutputFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectOutputFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectOutputFields TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_SelectRequiredFields]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWTableRule_SelectRequiredFields]
GO

/*===========================================================================*\
  Description:  List of Required column types for a table rule.

  Parameters:   @table_rule_key         Identifies the table rule.

  Created:      June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_SelectRequiredFields]
    @table_rule_key     CHAR(16)
AS
    SELECT      IW_Column_Type_Key
    FROM        IW_Table_Rule_Related_Field
    WHERE       IW_Table_Rule_Key               =   @table_rule_key
    AND         Relationship                    =   2
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_SelectRequiredFields') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWTableRule_SelectRequiredFields'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWTableRule_SelectRequiredFields TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFullySpecified_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a Location name with preferred name and spatial ref.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = LN1.Item_Name + ' (' + LN2.Item_Name + ' - ' + L.Spatial_Ref + ')'
	FROM	Location L
	JOIN	Location_Name LN1 ON LN1.Location_Key = L.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	L.Location_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationKeyFromVCNumber_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocationKeyFromVCNumber_Get]
GO

/*===========================================================================*\
  Description:	Tests if a vice county number is recognised and returns the 
		location_Key.  Used by import wizard.

  Parameters:	@Value - vice county number to test
		@Location_Key - output.  Null if not found.

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationKeyFromVCNumber_Get]
  @Value VARCHAR(20),
	@Location_Key CHAR(16) OUTPUT
AS

SELECT @Location_Key=Location_Key FROM Location WHERE File_Code=@Value AND Location_Type_Key='JNCCIMPW00000001'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationKeyFromVCNumber_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_LocationKeyFromVCNumber_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationKeyFromVCNumber_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_LocationName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_LocationName_Get] 
GO

/*===========================================================================*\
  Description:	Gets the Location Name given a location key

  Parameters:	@Key
		@Caption OUTPUT	

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationName_Get] 
@Key CHAR(16),
@Caption VARCHAR(100) OUTPUT

AS
	SET NOCOUNT ON

	SELECT		@Caption = Item_Name
	FROM		Location_Name AS LN
	INNER JOIN	Location AS L ON L.Location_Key = LN.Location_Key
	WHERE		L.Location_Key = @Key
	AND		LN.Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationName_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationsSubSites_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocationsSubSites_Select]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of sites and subsites recursively.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationsSubSites_Select]

AS
	SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
		Raiserror('usp_LocationsSubSites_Select requires #TempList temp table to be created first.', 16, 1)

	WHILE 1=1
	BEGIN
	
		INSERT INTO #TempList 
		SELECT L.Location_Key AS RecordKey
		FROM Location L
		INNER JOIN #TempList T1 ON T1.RecordKey=L.Parent_Key
		LEFT JOIN #TempList T2 ON T2.RecordKey=L.Location_Key
		WHERE T2.RecordKey IS NULL
	
		IF @@RowCount=0 
			BREAK
	
	END

	-- Return dataset
	SELECT * FROM #TempList
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationsSubSites_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationsSubSites_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationsSubSites_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Locations_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Locations_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of individual names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003
 
  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Locations_Select_ForSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT		LN.Location_Key AS Item_Key, 
			LN.Item_Name AS DisplayTerm, 
			LN.Item_Name AS SearchTerm
	FROM		Location_Name AS LN
	WHERE 		LN.Item_Name LIKE @SearchText + '%'
	ORDER BY 	DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Locations_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Locations_Select_InBoundingBox]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Locations_Select_InBoundingBox]
GO

/*===========================================================================*\
  Description:	Obtain a list of sites where the centroid or ANY grid square
		overlaps into a bounding box

  Parameters:	
			@swlat, @swlong - south west coordinates of box
			@nelat, @nelong - north east coordinates of box

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Locations_Select_InBoundingBox]
	@swlat FLOAT, 
	@swlong FLOAT, 
	@nelat FLOAT, 
	@nelong FLOAT
AS

SET NOCOUNT ON 

-- some constants to simplify the calculations
DECLARE	@EARTH_RADIUS_KM FLOAT
DECLARE @swLongRad FLOAT
DECLARE @swLatRad FLOAT
DECLARE @SinSwLatRad FLOAT 
DECLARE @CosSwLatRad FLOAT 

SET @EARTH_RADIUS_KM = 6378
SET @swLongRad = RADIANS(@swlong)
SET @swLatRad = RADIANS(@swlat)
SET @SinSwLatRad = SIN(@swLatRad)
SET @CosSwLatRad = COS(@swLatRad)

SELECT DISTINCT l.Location_key
FROM Location l
LEFT JOIN grid_square g on g.location_Key=l.location_key
	AND (
	-- Find grid squares where recorded point inside box
	(g.lat>=@swlat and g.lat<=@nelat and g.long>=@swlong and g.long<=@nelong)
	-- find grid squares sw of box which overlap
	OR (g.Lat<@swlat and g.long<@swlong  
		and @EARTH_RADIUS_KM * ACOS(@CosSwLatRad * @CosSwLatRad *
		COS(RADIANS(g.Long) - @swLongRad) + @SinSwLatRad * @SinSwLatRad) <= (g.[size]/1000)
		and @EARTH_RADIUS_KM * ACOS(COS(RADIANS(g.Lat)) * @CosSwLatRad *
		COS(@swLongRad - @swLongRad) + SIN(RADIANS(g.Lat)) * @SinSwLatRad) <= (g.[size]/1000))
	-- find grid squares w of box which overlap
	OR (g.Lat>=@swlat and g.Lat<= @nelat and g.long<@swlong and 
		@EARTH_RADIUS_KM * ACOS(@CosSwLatRad * @CosSwLatRad *
		COS(RADIANS(g.Long) - @swLongRad) + @SinSwLatRad * @SinSwLatRad) <= (g.[size]/1000))
	-- find grid squares s of box which overlap
	OR (g.Lat<@swlat and g.long>=@swlong and g.long<=@nelong and
		@EARTH_RADIUS_KM * ACOS(COS(RADIANS(g.Lat)) * @CosSwLatRad *
		COS(@swLongRad - @swLongRad) + SIN(RADIANS(g.Lat)) * @SinSwLatRad) <= (g.[size]/1000))
	)
LEFT JOIN Grid_Square g2 on g2.Location_Key=l.Location_Key
WHERE 
	g.Grid_Square_Key IS NOT NULL
	OR (
		g2.Grid_Square_Key IS NULL AND 
		l.lat>=@swlat and 
		l.lat<=@nelat and 
		l.long>=@swlong and 
		l.long<=@nelong
	)

	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_InBoundingBox') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Locations_Select_InBoundingBox'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Name_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Name_Get] 
GO

CREATE PROCEDURE [dbo].[usp_Name_Get] 
@Key CHAR(16),
@Caption VARCHAR(100) OUTPUT

AS

--  DESCRIPTION
--  Returns a Name caption given the NAME_KEY
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Key of record to be returned
--	@Caption			Output caption
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-11-03
--
SET NOCOUNT ON

SELECT @Caption = dbo.ufn_GetFormattedName(@Key)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Name_IsIndividual_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Name_IsIndividual_Get]
GO 

CREATE PROCEDURE [dbo].[usp_Name_IsIndividual_Get] 
@NameKey CHAR(16),
@IsIndividual BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether or not a Name_Key is an Individual Name_Key
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@NameKey				Name_Key
--	@IsIndividual		Output
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			16/02/2004
--

IF EXISTS(SELECT * FROM Individual WHERE Name_Key = @NameKey)
	SET @IsIndividual = 1
ELSE
	SET @IsIndividual = 0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_IsIndividual_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_IsIndividual_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Organisation_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Organisation_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of organisation names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Organisation_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Organisation_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Organisation_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypes_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_RecordTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Record Types

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RecordTypes_Select]
AS
	SELECT		Record_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Record_Type
	ORDER BY 	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RecordTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RecordTypes_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceName_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ReferenceName_Get]
GO

/*===========================================================================*\
  Description:	Returns formatted reference name.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceName_Get]
	@Key char(16),
	@Caption varchar(300) OUTPUT
AS
	SELECT @Caption = dbo.ufn_GetFormattedReferenceName(@Key)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceName_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_References_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of references matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForSearch]
	@SearchText varchar(500)
AS
	SELECT	R.Source_Key AS Item_Key,
		dbo.ufn_GetFormattedReferenceName(R.Source_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedReferenceName(R.Source_Key) AS SearchTerm
	FROM	Reference R
	JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
	WHERE 	Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) LIKE @SearchText + '%'
	OR	Cast(Title AS varchar(1000)) LIKE Replace(@SearchText, ' ', '%') + '%'
	OR 	Author LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RelationshipTypes_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_RelationshipTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Relationship Types

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RelationshipTypes_Select]
AS
	SELECT		Relationship_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Relationship_Type
	ORDER BY 	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RelationshipTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RelationshipTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RelationshipTypes_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorders_Select_ForSurveyEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorders_Select_ForSurveyEvent]
GO

/*===========================================================================*\
  Description:	Returns a list of Sample Recorders for a given 
		Survey_Event_Key. (Saved in JNCC folder)

  Parameters:	@Key	Survey_Event_Key

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorders_Select_ForSurveyEvent]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		SE_Recorder_Key AS [Key],
			Name_Key,
			dbo.ufn_GetFormattedName(Name_Key) AS [Name],
			Custodian,
			0x00000000005C275F AS [Timestamp] -- Bodge timestamp required by TAddinCachedDataItem.CreateFromRecord
	FROM		Survey_Event_Recorder
	WHERE		Survey_Event_Key = @Key
	ORDER BY	[Name]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorders_Select_ForSurveyEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorders_Select_ForSurveyEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select_ForSurveyEvent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_SampleTypes_Select')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleTypes_Select]
GO

/*===========================================================================*\
  Description:  Returns a list of Sample types.

  Parameters:   <none>

  Created:      July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleTypes_Select]
AS
    SELECT      Sample_Type_Key AS  Item_Key,
                Short_Name      AS  Item_Name
    FROM        Sample_Type
    ORDER BY    Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_SampleTypes_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpatialRefsForReport_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpatialRefsForReport_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of spatial references that apply to the 
		current report output.

  Parameters:	None

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpatialRefsForReport_Select]
AS

DECLARE @SQL AS NVARCHAR(1000)
DECLARE @Select AS NVARCHAR(1000)
DECLARE @Order AS NVARCHAR(100)

SET @Select = 'SELECT DISTINCT '

-- Find the occurrence key and return it if available
IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Type' AND ID=Object_Id('TempDB..#REPORT_OUTPUT')) AND
		EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Occurrence_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 'Occurrence_Key, Type, '
	SET @Order = ' ORDER BY Type'
END

IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Lat' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		AND EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Long' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	-- Spatial reference is in the table.  If it is complete, return all the fields, otherwise fill in as a lat-long
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Spatial_Ref' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
			AND EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Spatial_Ref_System' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + 'Spatial_Ref, Spatial_Ref_System, Lat, Long '
	ELSE
		SET @Select = @Select + 
				'	CASE 
						WHEN Lat>=0 THEN CAST(Lat AS VARCHAR(20)) + ''N'' 
						ELSE CAST((0-Lat) AS VARCHAR(20)) + ''S'' 
					END + '', '' +  
					CASE 
						WHEN Long>=0 THEN CAST(Long AS VARCHAR(20)) + ''E'' 
						ELSE CAST((0-Long) AS VARCHAR(20)) + ''W'' 
					END AS Spatial_Ref, 
					''LTLN'' AS Spatial_Ref_System, 
					Lat, 
					Long '
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Vague_Date_Start' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + ', Vague_Date_Start'
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT'
END
ELSE IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Sample_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 
			'	S.Sample_Key,
				S.Spatial_Ref,
				S.Spatial_Ref_System,
				S.Lat,
				S.Long,
				S.Vague_Date_Start '
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT R INNER JOIN [Sample] S ON S.Sample_Key=R.Sample_Key'
END
ELSE IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Location_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 
			'	L.Location_Key,
				L.Spatial_Ref,
				L.Spatial_Ref_System,
				L.Lat,
				L.Long '
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Vague_Date_Start' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + ', R.Vague_Date_Start'
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT R
			INNER JOIN [Location] L ON L.Location_Key=R.Location_Key'
END

IF @Order IS NOT NULL
	SET @SQL = @SQL + @Order

IF @SQL IS NOT NULL 
	EXEC sp_executesql @stmt = @SQL

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpatialRefsForReport_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_SpatialRefsForReport_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpeciesFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a species name with checklist name.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = dbo.ufn_GetFormattedSpeciesName(Taxon_List_Item_Key) + ' - ' + TL.Item_Name
	FROM	Index_Taxon_Name ITN
	JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	WHERE	ITN.Taxon_List_Item_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpeciesFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpeciesFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpeciesName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpeciesName_Get]
GO

/*===========================================================================*\
  Description:	Returns formatted Species name.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpeciesName_Get]
	@Key char(16),
	@Caption varchar(200) OUTPUT
AS
	SELECT @Caption = dbo.ufn_GetFormattedSpeciesName(@Key)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpeciesName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpeciesName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpeciesName_Get TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Species_Select_ForSearchByList]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
	@SearchKey char(16),
	@SearchText varchar(100)

AS
	SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
		CASE Actual_Name_Italic
			WHEN 1 THEN '<i>' + Actual_Name + '</i>'
			ELSE Actual_Name
		END + ' ' + ISNULL(ITN.Authority, '') AS DisplayTerm, 
		Actual_Name + ' ' + ISNULL(ITN.Authority, '') AS SearchTerm
	FROM	Index_Taxon_Name ITN
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key
			AND TLV.Taxon_List_Key=@SearchKey
	WHERE 	(Actual_Name LIKE @SearchText + '%'
	OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
	OR 	ITN.Authority LIKE @SearchText + '%')
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypes_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SpecimenTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of Specimen Types

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenTypes_Select]
AS
	SELECT		Specimen_Type_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Specimen_Type
	ORDER BY 	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Substrates_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Substrates_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of substrates

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Substrates_Select]
AS
	SELECT		Substrate_Key AS Item_Key, Short_Name AS Item_Name
	FROM		Substrate
	ORDER BY 	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Substrates_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Substrates_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Substrates_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Substrates_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Substrates_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Substrates_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Substrates_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Substrates_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_Get_ForSample]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForSample]
GO

/*===========================================================================*\
  Description: 	This proc. returns a Survey Event key when given a Sample key.

  Parameters:	@Key 		Sample Key
		@SurveyEventKey	(output)

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForSample] 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@SurveyEventKey = Survey_Event_Key
	FROM	[Sample]
	WHERE	Sample_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Surveys_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Surveys_Select]
GO

/*===========================================================================*\
  Description:	Returns all Surveys on the system using the standard caption
		for a survey in alphabetical order. (Saved in JNCC folder).

  Parameters:	

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select]
AS

SET NOCOUNT ON
	
	SELECT		Survey_Key, Item_Name,
			Item_Name + ' - ' + dbo.ufn_GetFormattedName(Run_By) AS Display_Name
	FROM		Survey
	ORDER BY	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Surveys_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Survey_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
GO

CREATE PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc for the search dialog window that searched for survey 
		names and their authors. 

  Parameters:	@SearchText

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

SET NOCOUNT ON

SELECT 	SV.Survey_Key AS Item_Key, 
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS DisplayTerm,
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS SearchTerm
FROM 	Survey AS SV
WHERE 	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Survey_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxa_GetChildren]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Taxa_GetChildren]
GO

/*===========================================================================*\
  Description:	Uses a pre-existing temp table (#TempList) to obtain a list
		of sites and subsites recursively.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxa_GetChildren]

AS
	SET NOCOUNT ON

  IF object_id('tempdb..#TempList') IS NULL
		Raiserror('usp_Taxa_GetChildren requires #TempList temp table to be created first.', 16, 1)

	SELECT Contained_List_Item_Key
	FROM Index_Taxon_Group ITG
	INNER JOIN #TempList T ON T.RecordKey=ITG.Taxon_List_Item_Key
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxa_GetChildren') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxa_GetChildren'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxa_GetChildren TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonLists_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonLists_Select]
GO

/*===========================================================================*\
  Description:	Returns list of taxon lists.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonLists_Select]
AS
	SELECT	Taxon_List_Key AS Item_Key, Item_Name
	FROM	Taxon_List
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonLists_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonLists_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonLists_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrenceDataTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrenceDataTypes_Select]
GO

/*===========================================================================*\
  Description:	Retrieves the list of taxon occurrence data types available.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrenceDataTypes_Select]
AS

SELECT 
		MT.Measurement_Type_Key, 
		MQ.Measurement_Qualifier_Key, 
		MU.Measurement_Unit_Key,
		MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name+ ')' AS Item_Name
FROM Measurement_Type MT
INNER JOIN Measurement_Unit MU ON MU.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Qualifier MQ ON MQ.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Type_Context MTC ON MTC.Measurement_Type_Key=Mt.Measurement_Type_Key
		AND MTC.Measurement_Context_Key='NBNSYS0000000003'
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceDataTypes_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_TaxonOccurrenceDataTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserAccessLevel_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserAccessLevel_Get]
GO

/*===========================================================================*\
  Description:	Takes a User key and returns the Access Level.
			1 = read only, 
			2 = record cards only, 
			3 = read/write, 
			4 = full user, 
			5 = system manager.  	
 
  Parameters:	@Key  
		@AccessLevel OUTPUT

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserAccessLevel_Get] 
	@Key CHAR(16),
	@AccessLevel Int OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@AccessLevel = Security_Level
	FROM	[User]
	WHERE	Name_key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserAccessLevel_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserAccessLevel_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserAccessLevel_Get TO [Dev - JNCC SQL]
END

GO

