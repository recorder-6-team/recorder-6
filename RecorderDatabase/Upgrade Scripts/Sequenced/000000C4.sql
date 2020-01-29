/****** Object:  StoredProcedure [dbo].[usp_BiotopeOccurrenceSource_Insert]    Script Date: 01/20/2020 14:26:13 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Insert a record in Biotope_Occurrence_Source based on source already held in Sample Sources
    Will always bring biotope Occurrences in line with Sample if this is run
  Parameters:
	@BiotopeOccurrenceKey 
	
  Created:
	February  2018

 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BiotopeOccurrenceSource_Insert]
	@BiotopeOccurrenceKey 	CHAR(16)
	
AS
DECLARE @Key CHAR(16),@BOCCKey CHAR(16),@SourceKey Char(16)

DECLARE	TEMP_Cursor CURSOR FOR 
		(SELECT DISTINCT BIOTOPE_OCCURRENCE_KEY,  SOURCE_KEY
		FROM BIOTOPE_OCCURRENCE BOCC INNER JOIN SAMPLE_SOURCES SS
		ON SS.SAMPLE_KEY = BOCC.SAMPLE_KEY   
		WHERE SS.SAMPLE_KEY = (SELECT
		SAMPLE_KEY FROM BIOTOPE_OCCURRENCE WHERE BIOTOPE_OCCURRENCE_KEY = @BiotopeOccurrenceKey 
		))
		
		OPEN	TEMP_Cursor
		
		FETCH NEXT FROM Temp_Cursor
		INTO	@BOCCKey,@SourceKey
		
		-- Loops through to get next key 
		WHILE @@FETCH_STATUS = 0
		BEGIN
		    IF NOT EXISTS(SELECT * FROM BIOTOPE_OCCURRENCE_SOURCES 
		      WHERE BIOTOPE_OCCURRENCE_KEY = @BOCCKey AND
		      SOURCE_KEY = @SourceKey)
		      BEGIN 
	       	    EXECUTE spNextKey 'Biotope_Occurrence_Sources', @Key  OUTPUT
			    INSERT INTO BIOTOPE_OCCURRENCE_SOURCES 
				VALUES (@Key,@BOCCKey,@SourceKey,1,Left(@SourceKey,8)) 
						
			
			  END
	  	 FETCH NEXT FROM Temp_Cursor
	     INTO	@BOCCKey,@SourceKey
		END
		
		CLOSE		Temp_Cursor
		DEALLOCATE	Temp_Cursor
GO

GRANT EXECUTE ON [dbo].[usp_BiotopeOccurrenceSource_Insert] TO PUBLIC 