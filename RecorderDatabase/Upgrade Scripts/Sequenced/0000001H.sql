SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Biotopes_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of biotopes matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 26/10/04 11:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Biotopes_Select_ForSearch]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
	ELSE
		SELECT	Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ', ' + Full_Term, Full_Term) AS SearchTerm
		FROM	Biotope_List_Item BLI
		JOIN	Biotope B ON B.Biotope_Key = BLI.Biotope_Key
		JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				AND BCV.Biotope_Classification_Key = @SearchKey
		WHERE 	BLI.BT_CL_Version_To IS NULL
		AND	(Short_Term LIKE @SearchText + '%'
		OR	 Original_Code LIKE @SearchText + '%'
		OR	 IsNull(Original_Code + ', ' + Full_Term, Full_Term) LIKE @SearchText + '%')
		ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotopes_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotopes_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotopes_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportFinalise]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportFinalise]
GO

/*===========================================================================*\
  Description:	Runs any stored proc on the database named 'usp_PostImport*'
				This allows addins to add constraints that impede the import process

  Created:	Oct 2004

  Last revision information:
    $Revision: 2 $
    $Date: 26/10/04 11:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportFinalise]
AS

DECLARE @spname VARCHAR(100)

DECLARE csr CURSOR FOR
  SELECT [name] FROM sysobjects where name like 'usp_PostImport%'

OPEN csr

WHILE 1=1
BEGIN
	FETCH NEXT FROM csr INTO @spname

	IF @@FETCH_STATUS<>0
		BREAK
	
	EXEC('EXEC ' + @spname)
END

CLOSE csr

DEALLOCATE csr

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportFinalise') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ImportFinalise'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportFinalise TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportFinalise TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ImportFinalise TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportInitialise]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportInitialise]
GO

/*===========================================================================*\
  Description:	Runs any stored proc on the database named 'usp_PreImport*'
				This allows addins to drop constraints that impede the import process

  Created:	Oct 2004

  Last revision information:
    $Revision: 2 $
    $Date: 26/10/04 11:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportInitialise]
AS

DECLARE @spname VARCHAR(100)

DECLARE csr CURSOR FOR
  SELECT [name] FROM sysobjects where name like 'usp_PreImport%'

OPEN csr

WHILE 1=1
BEGIN
	FETCH NEXT FROM csr INTO @spname

	IF @@FETCH_STATUS<>0
		BREAK
	
	EXEC('EXEC ' + @spname)
END

CLOSE csr

DEALLOCATE csr

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportInitialise') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ImportInitialise'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportInitialise TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportInitialise TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ImportInitialise TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CheckTableName]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
GO

/*===========================================================================*\
  Description:  Is the named table valid in an import/export database?

  Parameters:   @table_name             Table name
                @is_expected            [on exit] Is the table expected?

  Created:      July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 26/10/04 11:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CheckTableName]
    @table_name     VARCHAR(30),
    @is_expected    BIT             OUTPUT
AS
    IF EXISTS ( SELECT      1
                FROM        DATABASE_RELATIONSHIP
                WHERE       MASTER_TABLE            =   @table_name
                OR          DETAIL_TABLE            =   @table_name)
								OR @table_name in ('metadata', 'metadata_type', 'source_join')
        SET         @is_expected    =   1
    ELSE
        SET         @is_expected    =   0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CheckTableName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CheckTableName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CheckTableName TO [Dev - JNCC SQL]
END

