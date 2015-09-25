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