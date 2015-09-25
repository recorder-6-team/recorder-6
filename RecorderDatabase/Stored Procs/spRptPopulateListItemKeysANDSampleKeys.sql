If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptPopulateListItemKeysANDSampleKeys]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spRptPopulateListItemKeysANDSampleKeys'
        DROP PROCEDURE [dbo].[spRptPopulateListItemKeysANDSampleKeys]
    END
GO

    PRINT 'Creating procedure spRptPopulateListItemKeysANDSampleKeys'
GO

    /*
    $History: spRptPopulateListItemKeysANDSampleKeys.sql $
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
