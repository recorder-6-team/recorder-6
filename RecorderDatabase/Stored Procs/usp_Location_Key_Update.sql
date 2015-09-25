/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Location_Key_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Location_Key_Update]
GO

/*===========================================================================*\
  Description:	Updates all the samples under the parameter Event

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Location_Key_Update] (
	@EventKey char(16),
	@Value char(16)
)
AS
	UPDATE dbo.SAMPLE
	SET
		LOCATION_KEY = @Value
	WHERE
		SURVEY_EVENT_KEY = @EventKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Key_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ImportInitialise'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Location_Key_Update TO [Dev - JNCC SQL]
END
GO