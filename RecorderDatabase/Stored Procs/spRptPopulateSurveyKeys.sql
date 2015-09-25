If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptPopulateSurveyKeys]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spRptPopulateSurveyKeys'
        DROP PROCEDURE [dbo].[spRptPopulateSurveyKeys]
    END
GO

    PRINT 'Creating procedure spRptPopulateSurveyKeys'
GO

    /*
    $History: spRptPopulateSurveyKeys.sql $
 * 
 * *****************  Version 4  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 3  *****************
 * User: Pollyshaw    Date: 6/02/03    Time: 12:51
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Created indexes within the procedures.
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 13/11/02   Time: 14:56
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Changed report table to a temporary table.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 6/11/02    Time: 14:27
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE [dbo].[spRptPopulateSurveyKeys]

--
--	DESCRIPTION
--	This procedure populates the SURVEY_KEY based upon SURVEY_EVENT_KEY values
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
UPDATE 
#REPORT_OUTPUT
SET #REPORT_OUTPUT.SURVEY_KEY = SURVEY_EVENT.SURVEY_KEY
FROM
#REPORT_OUTPUT
INNER JOIN 
	SURVEY_EVENT 
ON #REPORT_OUTPUT.SURVEY_EVENT_KEY=SURVEY_EVENT.SURVEY_EVENT_KEY 

CREATE INDEX IX_SURVEY_KEY ON #REPORT_OUTPUT (SURVEY_KEY)

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

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spRptPopulateSurveyKeys') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spRptPopulateSurveyKeys'
        	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [Dev- JNCC SQL]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [R2k_AddOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [R2k_Administrator]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [R2k_FullEdit]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [R2k_ReadOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        		GRANT EXECUTE ON dbo.spRptPopulateSurveyKeys TO [R2k_RecordCardsOnly]
    END
GO
