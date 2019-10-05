/****** Object:  StoredProcedure [dbo].[usp_Setting_Temp_Survey]    Script Date: 08/20/2019 10:40:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [dbo].[usp_Setting_Temp_Survey]

AS
    DECLARE @TemplicenceKey varchar(16) 
   
    UPDATE SURVEY SET Temporary_Survey = 1 WHERE
    EXISTS (SELECT * FROM SAMPLE S INNER JOIN SURVEY_EVENT SE ON
    SE.SURVEY_EVENT_KEY = S.SURVEY_EVENT_KEY INNER JOIN SURVEY SV 
    ON SV.SURVEY_KEY = SE.SURVEY_KEY WHERE ISNULL(S.RECORDERS,'') <> ''
    AND SV.SURVEY_KEY = SURVEY.SURVEY_KEY) 
    
    SELECT @TempLicenceKey = DATA FROM SETTING WHERE[NAME] = 'TempLic'
   
   
    UPDATE SURVEY SET LICENCE_KEY = @TempLicenceKey 
    FROM SURVEY INNER JOIN LICENCE ON LICENCE.LICENCE_KEY
    = SURVEY.LICENCE_KEY
    WHERE  SURVEY.Temporary_Survey = 1
    AND LICENCE.SYSTEM_SUPPLIED_DATA = 1