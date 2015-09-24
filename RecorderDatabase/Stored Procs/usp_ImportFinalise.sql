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
    $Revision: 1 $
    $Date: 14/10/04 12:30 $
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