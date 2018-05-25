/****** Part of improvement to method of handling changes to Survey Event Recorders******/

Update Recorder_Role Set Long_Name = 'A Surveyor' WHERE RECORDER_ROLE_KEY = 'NBNSYS0000000001'
Update Recorder_Role Set Long_Name = 'A Recorder' WHERE RECORDER_ROLE_KEY = 'NBNSYS0000000002'
Update Recorder_Role Set Long_Name = Short_Name  WHERE RECORDER_ROLE_KEY NOT IN ('NBNSYS0000000001','NBNSYS0000000002')

GO

/*===========================================================================
  Drop stored proc before re-creating.
===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Cascade_Sample_Recorders]]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Cascade_Sample_Recorders]
GO


/****** Object:  StoredProcedure [dbo].[usp_Cascade_Sample_Recorders]    Script Date: 02/17/2018 20:59:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Cascades a new event recorder down to all sample in the event 
  Parameters:	@Key = Survey_Event_Key

  Created:	February 2018

  
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Cascade_Sample_Recorders]
	@Key char(16)
	
AS
	INSERT INTO SAMPLE_RECORDER(SAMPLE_KEY,SE_Recorder_Key,ENTERED_BY,ENTRY_DATE) 
    (SELECT DISTINCT SAMPLE.SAMPLE_KEY,SE_RECORDER_KEY,SER.ENTERED_BY,SER.Entry_Date  FROM SAMPLE 
    INNER JOIN SURVEY_EVENT SE ON SE.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY 
    INNER JOIN SURVEY_EVENT_RECORDER SER ON SER.SURVEY_EVENT_KEY =
    SE.SURVEY_EVENT_KEY WHERE SE.SURVEY_EVENT_KEY = @Key AND NOT EXISTS (
    SELECT * FROM SAMPLE_RECORDER WHERE SAMPLE_RECORDER.SE_RECORDER_KEY = SER.SE_RECORDER_KEY))
    
GO

GRANT EXECUTE ON [dbo].[usp_Cascade_Sample_Recorders] TO PUBLIC

GO
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Cascade_To_Determination]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Cascade_To_Determination]
GO

GO
/****** Object:  StoredProcedure [dbo].[usp_Cascade_To_Determination]    Script Date: 02/17/2018 21:00:31 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Cascades changed Sample Recorder down to Determination
  Parameters:	@Key = Survey_Event_Key

  Created:	February 2018

  
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Cascade_To_Determination]
	@SERKey char(16),  @NameKey char(16)
	
AS
	UPDATE Taxon_Determination Set DETERMINER = SER.Name_Key, changed_By = SER.ENTERED_BY, changed_date = getdate()
	FROM  SURVEY_EVENT_RECORDER SER 
	INNER JOIN SAMPLE_RECORDER SR ON SR.SE_RECORDER_KEY = SER.SE_RECORDER_KEY
	INNER JOIN TAXON_OCCURRENCE TOCC ON TOCC.SAMPLE_KEY = SR.SAMPLE_KEY
	INNER JOIN TAXON_DETERMINATION TDET ON TDET.TAXON_OCCURRENCE_KEY= TOCC.TAXON_OCCURRENCE_KEY
	WHERE SER.SE_RECORDER_KEY = @SERKey AND TDET.DETERMINER =  @NameKey  


        UPDATE Biotope_Determination Set DETERMINER = SER.Name_Key, changed_By = SER.ENTERED_BY, changed_date = getdate()
	FROM  SURVEY_EVENT_RECORDER SER 
	INNER JOIN SAMPLE_RECORDER SR ON SR.SE_RECORDER_KEY = SER.SE_RECORDER_KEY
	INNER JOIN BIOTOPE_OCCURRENCE BOCC ON BOCC.SAMPLE_KEY = SR.SAMPLE_KEY
	INNER JOIN BIOTOPE_DETERMINATION BDET ON BDET.BIOTOPE_OCCURRENCE_KEY= BOCC.BIOTOPE_OCCURRENCE_KEY
	WHERE SER.SE_RECORDER_KEY = @SERKey AND BDET.DETERMINER =  @NameKey  



GO

GRANT EXECUTE ON [dbo].[usp_Cascade_To_Determination] TO PUBLIC

GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SER_Sample_Recorder_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SER_Sample_Recorder_Insert]

GO
/****** Object:  StoredProcedure [dbo].[usp_SER_Sample_Recorder_Insert]    Script Date: 02/18/2018 18:52:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Inserts survey event recorders from merge

  Parameters:	
	@SampleKey,
	@NameKey,
	@SEKey,
	

  Created:	Feb 2018
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SER_Sample_Recorder_Insert] 
	@SampleKey CHAR (16),
	@NameKey CHAR(16),
    @SEKey CHAR(16)
AS
DECLARE	@key VARCHAR(16)
	
SET NOCOUNT OFF

EXECUTE spNextKey 'Survey_Event_Recorder', @Key OUTPUT

INSERT INTO Survey_Event_Recorder(
	SE_Recorder_Key,
	Name_Key,
	Survey_Event_Key,
	Recorder_Role_Key,
	Entered_By,
	Entry_Date,
	CHANGED_BY,
	CHANGED_DATE,
	Custodian
) 
SELECT @Key,
  SER.NAME_KEY,
  @SEKey, 
  SER.Recorder_Role_Key,
  SER.Entered_By,
  SER.Entry_Date,
  SER.CHANGED_BY,
  SER.CHANGED_DATE,
  SER.Custodian
FROM Survey_EVENT_RECORDER SER
INNER JOIN SAMPLE_RECORDER SR ON SR.SE_RECORDER_KEY
= SER.SE_RECORDER_KEY 
WHERE SR.Sample_Key = @SampleKey AND 
SER.NAME_KEY = @NameKey
AND NOT EXISTS (SELECT * FROM Survey_Event_Recorder 
WHERE Survey_Event_Recorder.Survey_Event_Key = @SEKey       
AND Survey_Event_Recorder.Name_Key =@NameKey)  


SET NOCOUNT ON

GO

GRANT EXECUTE ON [dbo].[usp_SER_Sample_Recorder_Insert] TO PUBLIC

GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrenceSource_Insert] ')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrenceSource_Insert] 




GO
/****** Object:  StoredProcedure [dbo].[usp_TaxonOccurrenceSource_Insert]    Script Date: 02/19/2018 14:26:13 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Insert a record in Taxon_Occurrence_Source based on source already held in Sample Sources
    Will always bring Occurrences in line with Sample if this is run
  Parameters:
	@TaxonOccurrenceKey 
	
  Created:
	February  2018

 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrenceSource_Insert]
	@TaxonOccurrenceKey 	CHAR(16)
	
AS
DECLARE @Key CHAR(16),@TOCCKey CHAR(16),@SourceKey Char(16)

DECLARE	TEMP_Cursor CURSOR FOR 
		(SELECT DISTINCT TAXON_OCCURRENCE_KEY,  SOURCE_KEY
		FROM TAXON_OCCURRENCE TOCC INNER JOIN SAMPLE_SOURCES SS
		ON SS.SAMPLE_KEY = TOCC.SAMPLE_KEY   
		WHERE SS.SAMPLE_KEY = (SELECT
		SAMPLE_KEY FROM TAXON_OCCURRENCE WHERE TAXON_OCCURRENCE_KEY = @TaxonOccurrenceKey 
		))
		
		OPEN	TEMP_Cursor
		
		FETCH NEXT FROM Temp_Cursor
		INTO	@TOCCKey,@SourceKey
		
		-- Loops through to get next key 
		WHILE @@FETCH_STATUS = 0
		BEGIN
		    IF NOT EXISTS(SELECT * FROM TAXON_OCCURRENCE_SOURCES 
		      WHERE TAXON_OCCURRENCE_KEY = @TOCCKey AND
		      SOURCE_KEY = @SourceKey)
		      BEGIN 
	       	    EXECUTE spNextKey 'Taxon_Occurrence_Sources', @Key  OUTPUT
			    INSERT INTO TAXON_OCCURRENCE_SOURCES 
				VALUES (@Key,@TOCCKey,@SourceKey,1,Left(@SourceKey,8)) 
						
			
			  END
	  	 FETCH NEXT FROM Temp_Cursor
	     INTO	@TOCCKey,@SourceKey
		END
		
		CLOSE		Temp_Cursor
		DEALLOCATE	Temp_Cursor
GO

GRANT EXECUTE ON [dbo].[usp_TaxonOccurrenceSource_Insert] TO PUBLIC 