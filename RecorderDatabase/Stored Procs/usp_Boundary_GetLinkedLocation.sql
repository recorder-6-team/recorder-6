/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Boundary_GetLinkedLocation') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Boundary_GetLinkedLocation]
GO

/*===========================================================================*\
  Description:	Returns the location linked to a map boundary.  Returns an
			empty dataset if not linked

  Parameters:	Map_Sheet_Key, Static_Object_ID

  Created:	July 2006

  Last revision information:
    $Revision: 1 $
    $Date: 28/07/06 12:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Boundary_GetLinkedLocation]
@Map_Sheet_Key CHAR(16),
@Static_Object_ID INT
AS
	SELECT LN.Location_Key, LN.Item_Name 
	FROM Location_Boundary LB
	INNER JOIN Location_Name LN ON 
		LN.Location_Key=LB.Location_Key AND LN.Preferred=1
	WHERE LB.Map_Sheet_Key = @Map_Sheet_Key AND LB.[Object_Id] = @Static_Object_ID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Boundary_GetLinkedLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Boundary_GetLinkedLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [Dev - JNCC SQL]
END
GO
