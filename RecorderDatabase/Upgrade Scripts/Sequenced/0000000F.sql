/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[spRepairLastKey]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[spRepairLastKey]
GO

/*===========================================================================*\
  Description:	Function used in creation of keys for tables

  Parameters:	@TableName
		@KeyField	Optional.
		@SiteId		Optional.

  Created:	January 2003

  Last revision information:
    $Revision: 3 $
    $Date: 25/02/04 10:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[spRepairLastKey]
	@TableName varchar(50),
	@KeyField varchar(50) = NULL,
	@SiteID varchar(8) = NULL
AS
BEGIN
	-- Get the key field name for given table, if not already provided
	IF @KeyField IS NULL
		SELECT	@KeyField  = Column_Name
		FROM	INFORMATION_SCHEMA.KEY_COLUMN_USAGE
		WHERE	Table_Name = @TableName
		AND	ObjectProperty(Object_ID(Constraint_Name), 'IsPrimaryKey') = 1

	-- Get the current SiteID, if not already provided
	IF @SiteID IS NULL
		SELECT	@SiteID = Data FROM Setting WHERE [Name] = 'SiteID'
		
	/*-------------------------------------------------------------*\
	  Make sure the highest key is in Last_Key, as it should be.
	\*-------------------------------------------------------------*/
	EXECUTE('IF EXISTS(SELECT TOP 1 * FROM ' + @TableName +
		' WHERE Left( ' + @KeyField + ', 8) = '''+ @SiteID + ''')' +
		'UPDATE Last_Key SET Last_Key_Text = ' +
				 '(SELECT Max(Right(' + @KeyField + ', 8)) FROM ' + @TableName + 
				 ' WHERE Left( ' + @KeyField + ', 8) = '''+ @SiteID + ''' ) ' +
			 'WHERE Table_Name = ''' + @TableName + ''' ')
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[spRepairLastKey]') AND Type = 'P')
BEGIN
	PRINT 'Setting up security on procedure spRepairLastKey'
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'Dev- JNCC SQL')
		GRANT EXECUTE ON [dbo].[spRepairLastKey] TO [Dev- JNCC SQL]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[spRepairLastKey] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[spRepairLastKey] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[spRepairLastKey] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SysUsers WHERE [Name] = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[spRepairLastKey] TO [R2k_RecordCardsOnly]
END
GO
