/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LastSurveyEvent_ForLocation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LastSurveyEvent_ForLocation_Select]
GO

/*===========================================================================*\
  Description:	Returns the last known survey event date for a location.

  Parameters:	@Key	Location  Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/02/04 9:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LastSurveyEvent_ForLocation_Select]
	@Key char(16)
	
AS

DECLARE @EventVDS AS INT
DECLARE @EventVDE AS INT
DECLARE @EventVDT AS VARCHAR(2)

DECLARE @SampleVDS AS INT
DECLARE @SampleVDE AS INT
DECLARE @SampleVDT AS VARCHAR(2)

	
SELECT TOP 1 
	@EventVDS = Vague_Date_Start, 
	@EventVDE = Vague_Date_End, 
	@EventVDT = Vague_Date_Type
FROM Survey_Event
WHERE Location_Key = @Key
ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC

SELECT TOP 1 
	@SampleVDS = Vague_Date_Start, 
	@SampleVDE = Vague_Date_End, 
	@SampleVDT = Vague_Date_Type
FROM Sample
WHERE Location_Key = @Key
ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC

-- Find the latest of the 2 dates
IF @SampleVDE>@EventVDE 
  SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
ELSE IF @SampleVDE<@EventVDE 
  SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
ELSE
BEGIN
  -- end date same, match on start date
	IF @SampleVDS>@EventVDS
	  SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
	ELSE
	  SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LastSurveyEvent_ForLocation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LastSurveyEvent_ForLocation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [Dev - JNCC SQL]
END

GO