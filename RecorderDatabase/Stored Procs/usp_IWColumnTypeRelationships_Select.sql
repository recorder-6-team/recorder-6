/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnTypeRelationships_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnTypeRelationships_Select]
GO

/*===========================================================================*\
  Description:	Returns details of all column types that are related to a
		column type.  For example, dependencies, conflicts.

  Parameters:	@Key - IW_Column_Type_Key

  Created:	May 2004

  Last revision information:
    $Revision: 2 $
    $Date: 30/06/04 13:44 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnTypeRelationships_Select]
	@Key CHAR(16)
AS

SELECT Related_IW_Column_Type_Key, Relationship_Type
FROM IW_Column_Type_Relationship
WHERE IW_Column_Type_Key=@Key
UNION
SELECT IW_Column_Type_Key, 4
FROM IW_Column_Type_Relationship
WHERE Related_IW_Column_Type_Key=@Key
AND Relationship_Type=3

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnTypeRelationships_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWColumnTypeRelationships_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWColumnTypeRelationships_Select TO [Dev - JNCC SQL]
END

GO