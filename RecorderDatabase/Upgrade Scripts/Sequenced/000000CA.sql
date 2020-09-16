/****** Add Update last key as a stored procedure ******/
GO
/****** Object:  StoredProcedure [dbo].[spRepairLastKey]    Script Date: 01/08/2020 20:23:09 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Function used in creation of keys for tables

  Parameters:	@TableName
		@KeyField	Optional.
		@SiteId		Optional.

  Created:	January 2003

  Last revision information:
    Michael Weideli -Jan 2020 to cope with table names which are reserved words 
    and those where there is no table 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[spRepairLastKey]
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
	IF @Keyfield IS NOT NULL
	
	EXECUTE('IF EXISTS(SELECT TOP 1 * FROM [' + @TableName + ']' +
		' WHERE Left( ' + @KeyField + ', 8) = '''+ @SiteID + ''')' +
		'UPDATE Last_Key SET Last_Key_Text = ' +
	    '(SELECT Max(Right(' + @KeyField + ', 8)) FROM [' + @TableName + ']' +
		' WHERE Left( ' + @KeyField + ', 8) = '''+ @SiteID + ''' ) ' +
		' WHERE Table_Name = ''' + @TableName + ''' ')
END



GO
/****** Object:  StoredProcedure [dbo].[spUpdateLastKey]    Script Date: 01/08/2020 20:21:35 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Procedure to bring last key table into line 

  Created:	January 2020
  
\*===========================================================================*/
CREATE PROCEDURE [dbo].[spUpdateLastKey]

AS
BEGIN
  DECLARE @TableName varchar(50)
  DECLARE Table_Cursor CURSOR FOR   
  SELECT Table_Name  
  FROM Last_Key ORDER BY TABLE_NAME  
  OPEN Table_cursor  
  FETCH NEXT FROM Table_Cursor   
    INTO @TableName  
    EXECUTE spRepairLastKey @TableName, NULL, NULL
  WHILE @@FETCH_STATUS = 0  
  BEGIN  
    FETCH NEXT FROM Table_Cursor   
      INTO @TableName 
    EXECUTE spRepairLastKey @TableName, NULL, NULL
  END   
CLOSE Table_Cursor;  
DEALLOCATE Table_Cursor
END

GO


GRANT EXECUTE ON  [dbo].[spRepairLastKey] TO PUBLIC 

GO
GRANT EXECUTE ON  [dbo].[spUpdateLastKey] TO PUBLIC 