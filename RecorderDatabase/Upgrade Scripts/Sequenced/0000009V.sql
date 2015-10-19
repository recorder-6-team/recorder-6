
/****** Object:  StoredProcedure [dbo].[usp_LastSurveyEvent_ForLocation_Select]    Script Date: 10/08/2015 13:58:11 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns the last known survey event date for a location.
  Updated so that Unknown dates are used only if there are no other dates available
  Parameters:	@Key	Location  Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 5 $
    $Date: 10/08/15 $
    $Author: mikeweidei $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_LastSurveyEvent_ForLocation_Select]
	@Key char(16)
AS
	DECLARE @EventVDS int,
		@EventVDE int,
		@EventVDT varchar(2),
		@SampleVDS int,
		@SampleVDE int,
		@SampleVDT varchar(2),	
	 	@UnknownEventVDT varchar(2),
		@UnknownSampleVDT varchar(2)
	
	SELECT TOP 1
		@EventVDS = Vague_Date_Start, 
		@EventVDE = Vague_Date_End,
		@EventVDT = Vague_Date_Type
	FROM 	Survey_Event
	WHERE 	Location_Key = @Key AND Vague_Date_Type <> 'U'
	ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC
	
	SELECT TOP 1 
		@SampleVDS = Vague_Date_Start, 
		@SampleVDE = Vague_Date_End, 
		@SampleVDT = Vague_Date_Type
	FROM 	Sample
	WHERE 	Location_Key = @Key AND Vague_Date_TYpe <> 'U'
	ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC
	
	SELECT @UnknownEventVDT  = 'U'
	FROM survey_Event
	WHERE 	Location_Key = @Key AND Vague_Date_Type = 'U'

	SELECT @UnknownSampleVDT  = 'U'
	FROM survey_Event
	WHERE 	Location_Key = @Key AND Vague_Date_Type = 'U'
	
	
	IF @EventVDT IS NULL AND @SampleVDT IS NULL AND @UnknownEventVDT IS NOT NULL
	BEGIN
	   Set    @EventVDT = 'U'
	   Set    @EventVDE =  0
	   Set    @EventVDS =  0 
	END
		
	-- Find the latest of the 2 dates
	 
	  IF (@SampleVDE > @EventVDE) OR (@EventVDT IS NULL)  
		 SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
	  ELSE 
      IF (@SampleVDE < @EventVDE) OR (@SampleVDT IS NULL)
		  SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
	     
	  ELSE
	--    end date same, match on start date
	  IF (@SampleVDS > @EventVDS) OR (@EventVDT IS NULL)
	      SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
	   ELSE
		   SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
        
     
	          
 
GO