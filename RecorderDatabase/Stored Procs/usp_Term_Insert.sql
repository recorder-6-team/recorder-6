/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a Term record

  Parameters:	@Key
		@LanguageKey 
		@ItemName 
		@Plaintext 
		@SessionID
		@SystemSuppliedData 

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8-03-06 14:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Insert]
	@Key char(16) OUTPUT,
	@LanguageKey varchar(4),
	@ItemName nvarchar(150),
	@Plaintext nvarchar(150) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Term', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Item_Name,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@LanguageKey,
			LTRIM(RTRIM(@ItemName)),
			LTRIM(RTRIM(@Plaintext)),
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [Dev - JNCC SQL]
END
GO