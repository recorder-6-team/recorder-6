/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of all column types available for the import 
			wizard, just the key value and classname

  Parameters:	None

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 28/05/04 10:27 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypes_Select]
AS

SELECT IW_Column_Type_Key, Class_Name
FROM IW_Column_Type
ORDER BY [Sequence]

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypes_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWColumnTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWColumnTypes_Select TO [Dev - JNCC SQL]
END

GO