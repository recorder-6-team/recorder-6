/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorder_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorder_Insert]
GO
/*===========================================================================*\
  Description:	Inserting records into the Sample Recorder join table.
  Parameters:	@SampleKey
		@SERecorderKey
		@EnteredBy

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 26/02/09 9:50 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorder_Insert]
	@SampleKey char(16),
	@SERecorderKey char(16),
	@EnteredBy char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
		
	IF NOT EXISTS	(SELECT * 
			FROM 	Sample_Recorder 
			WHERE 	Sample_Key = @SampleKey 
			AND 	SE_Recorder_Key = @SERecorderKey) 
	BEGIN
		INSERT INTO Sample_Recorder
			(Sample_Key,
			SE_Recorder_Key,
			Entered_By,
			Entry_Date)
		VALUES
			(@SampleKey,
			@SERecorderKey,
			@EnteredBy,
			GetDate())
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorder_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorder_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorder_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleRecorder_Insert TO [Dev - JNCC SQL]
END

GO