/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term table

  Parameters:	@Key 
		@LanguageKey 
		@ItemName 
		@Plaintext 
		@SessionID 

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 8/01/04 11:31 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Update]
	@Key char(16),
	@LanguageKey varchar(4),
	@ItemName nvarchar(150),
	@Plaintext nvarchar(150),
	@RecordsAffected int OUTPUT,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Term
		SET 	Language_Key = @LanguageKey,
			Item_Name = @ItemName,
			Plaintext = @Plaintext,
			Changed_Session_ID = @SessionID
		WHERE	Term_Key = @Key

		SET @RecordsAffected = @@Rowcount

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [Dev - JNCC SQL]
END
GO