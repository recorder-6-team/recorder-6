/****** SQL FOR CHANGES IN WORKING OF TEMPORARY IMPORT ******/


ALTER TABLE Survey
ADD  Temporary_Survey bit

GO

ALTER TABLE INDIVIDUAL
ADD  ACTIVE_VAGUE_DATE_START int

GO

ALTER TABLE INDIVIDUAL
ADD  ACTIVE_VAGUE_DATE_END int


ALTER TABLE INDIVIDUAL
ADD  ACTIVE_VAGUE_DATE_TYPE varchar(2)


GO
DELETE FROM LICENCE WHERE LICENCE_KEY = 'NBNSYS0000000007'

GO

INSERT INTO LICENCE (LICENCE_KEY,LONG_NAME,SHORT_NAME,DESCRIPTION,VERSION,ENTERED_BY,ENTRY_DATE,
SYSTEM_SUPPLIED_DATA,CUSTODIAN)
VALUES ('NBNSYS0000000007','No rights to distribute','NO rights',
'There are no rights to distribute this data in any form. 
It is held within the system for use in summarised form. 
Even when made available summarised it may only be used for the
 specific purpose for which it is provided and it must not be 
 modified or treated as original data.','1','TESTDATA00000001',GETDATE(),1,'NBNSYS00')


GO

INSERT INTO Custodian_Relationship VALUES ('SYSTEM0000000407',
'Licence','Licence_key','Licence_key','Survey','Survey_key','P',74,
'TESTDATA00000001',GETDATE())


GO


DELETE FROM SETTING WHERE [NAME] = 'TempLic'

Go

INSERT INTO SETTING VALUES('TempLic','NBNSYS0000000007','NBNSYS0000000007')


GO


UPDATE SURVEY SET Temporary_Survey = 0

GO

UPDATE SURVEY SET LICENCE_KEY = 'NBNSYS0000000001' WHERE LICENCE_KEY IS NULL 

GO

UPDATE Survey set Temporary_Survey = 1,
LICENCE_KEY = 'NBNSYS0000000007'
FROM SURVEY INNER JOIN SURVEY_EVENT ON
SURVEY_EVENT.SURVEY_KEY = SURVEY.SURVEY_KEY
INNER JOIN SAMPLE  ON SAMPLE.SURVEY_EVENT_KEY =
SURVEY_EVENT.SURVEY_EVENT_KEY 
WHERE ISNULL(SAMPLE.RECORDERS,'') <> ''

GO

UPDATE Survey SET Temporary_Survey = 1,
LICENCE_KEY = 'NBNSYS0000000007'
FROM SURVEY 
WHERE SURVEY.SURVEY_MEDIA_KEY IN
(SELECT DATA FROM SETTING WHERE 
[NAME] = 'TempMedia')   

GO

/****** Object:  StoredProcedure [dbo].[usp_Survey_TempSurvey]    Script Date: 11/04/2018 12:22:20 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Returns the Temporary_Survey indicator

  Parameters:
	@Key	The key of the survey to retrieve.
	@Value	The value of the Temporary_Survey as an OUTPUT.

  Created:	November 2018

  

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Survey_TempSurvey]
	@Key	VARCHAR(20),
	@Value	bit OUTPUT

AS
	SET NOCOUNT ON

	SELECT	@Value = Temporary_Survey
	FROM	Survey
	WHERE	Survey_Key = @Key


GO

GRANT EXECUTE ON  [dbo].[usp_Survey_TempSurvey] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_Survey_Insert]    Script Date: 12/23/2018 17:50:10 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
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
		@LicenceKey,
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

ALTER PROCEDURE [dbo].[usp_Survey_Insert]
	@Key					CHAR(16)	OUTPUT,
	@ItemName				VARCHAR(100),
	@Description			TEXT		= NULL,
	@RunByKey				CHAR(16),
	@SurveyStatusKey		CHAR(16)	= NULL,
	@SurveyMediaKey			CHAR(16)	= NULL,
	@SurveyTypeKey			CHAR(16),
	@LicenceKey		        CHAR(16),
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
		Licence_key,
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
		@LicenceKey,
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

CREATE PROCEDURE [dbo].[usp_Setting_Temp_Survey]

AS
	DECLARE @TempLicenceKey varchar(16)

	UPDATE SURVEY SET Temporary_Survey = 1 WHERE
	EXISTS (SELECT * FROM SAMPLE S INNER JOIN SURVEY_EVENT SE ON
	SE.SURVEY_EVENT_KEY = S.SURVEY_EVENT_KEY INNER JOIN SURVEY SV 
	ON SV.SURVEY_KEY = SE.SURVEY_KEY WHERE ISNULL(S.RECORDERS,'') <> ''
	AND SV.SURVEY_KEY = SURVEY.SURVEY_KEY) 
		
	SELECT @TempLicenceKey = DATA FROM SETTING WHERE[NAME] = 'TempLic'
	UPDATE SURVEY SET LICENCE_KEY = @TempLicenceKey 
	WHERE  Temporary_Survey = 1
	
GO

GRANT EXECUTE ON [dbo].[usp_Setting_Temp_Survey] TO PUBLIC	