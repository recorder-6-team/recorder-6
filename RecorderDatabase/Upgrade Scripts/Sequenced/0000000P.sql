/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[spNextKey]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[spNextKey]
GO

/*===========================================================================*\
  Description:	Generate a NBN key for the given table.
		The Site ID is obtained from the SETTING table and the last 
		used key from the LAST_KEY table.
		If the target table doesn't exist, a record is added to the 
		Last_Key table.

  Parameters:	@TableName	Name of table for which the new key is required
		@Key		OUTPUT. The new key.
		@SiteID		Optional.

  Created:	January 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/03/04 16:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[spNextKey]
	@TableName varchar(50),
	@Key char(16) OUTPUT,
	@SiteID char(8) = NULL
AS
BEGIN
	DECLARE @OldKey char(8),
		@NewKey char(8),
		@RowCount integer

	-- Get the current SiteID, if not already provided
	IF @SiteID IS NULL
		SELECT	@SiteID = Data FROM Setting WHERE [Name] = 'SiteID'

	-- Get the current last key value for given table
	SELECT	@OldKey = Last_Key_Text FROM Last_Key WHERE Table_Name = @TableName

	/*-------------------------------------------------------------*\
	  If no key in table, insert new record.
	\*-------------------------------------------------------------*/
	IF @OldKey IS NULL
	BEGIN
		-- Table name not in Last_Key table, add record with default for key value
		INSERT INTO Last_Key (Table_Name, Last_Key_Text) VALUES (@TableName, '00000000')
		-- Make sure it's the highest
		EXECUTE spRepairLastKey @TableName, NULL, @SiteID  -- SiteId already retrieved, so pass it on
		-- And select it
		SELECT	@OldKey = Last_Key_Text FROM Last_Key WHERE Table_Name = @TableName
	END

	/*-------------------------------------------------------------*\
	  Now generate a brand new key.
	\*-------------------------------------------------------------*/
	SET @RowCount=0
	-- Loop until we're sure it's a new key (multi-users and all that...)
	WHILE @RowCount = 0 
	BEGIN
		SET @NewKey = dbo.IncrementKey(@OldKey)

		UPDATE	Last_Key 
		SET	Last_Key_Text = @NewKey
		WHERE	Table_Name = @TableName AND Last_Key_Text = @OldKey

		SET @RowCount = @@RowCount  -- Should be 1 if record was successfully updated.
		SET @Oldkey = @NewKey
	END
	/*-------------------------------------------------------------*\
	  Concatenate the SiteID and return.
	\*-------------------------------------------------------------*/
	SET @Key = @SiteID + @NewKey
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE  Id = Object_Id(N'[dbo].[spNextKey]') AND Type = 'P')
BEGIN
	PRINT 'Setting up security on procedure spNextKey'
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'Dev- JNCC SQL')
		GRANT EXECUTE ON [dbo].[spNextKey] TO [Dev- JNCC SQL]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[spNextKey] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[spNextKey] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[spNextKey] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[spNextKey] TO [R2k_RecordCardsOnly]
END
GO