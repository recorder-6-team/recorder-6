/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ComputerMap_DeleteInvalid]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
GO

/*===========================================================================*\
  Description:	Delete invalid records from Copmuter_Map, in case something
		wasn't cleaned up properly.

  Parameters:	<none>

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 19/02/04 14:29 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
AS
	DELETE Computer_Map
	WHERE Base_Map_Key NOT IN (SELECT Base_Map_Key FROM Base_Map)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ComputerMap_DeleteInvalid') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ComputerMap_DeleteInvalid'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [Dev - JNCC SQL]
END
GO