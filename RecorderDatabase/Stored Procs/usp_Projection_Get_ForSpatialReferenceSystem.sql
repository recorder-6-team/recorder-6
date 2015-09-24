/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Projection_Get_ForSpatialReferenceSystem') AND SysStat & 0xf = 4)
	DROP PROCEDURE dbo.usp_Projection_Get_ForSpatialReferenceSystem
GO

/*===========================================================================*\
  Description:
	Gets the projection data for a specified spacial reference system.

  Parameters:
	@SpatialRefSystem	The spatial reference system.
	@Projection			The projection of this system (OUTPUT).

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 4/03/09 9:03 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Projection_Get_ForSpatialReferenceSystem
	@SpatialRefSystem	VARCHAR(4),
	@Projection			VARCHAR(255) OUTPUT
AS
	SET		@Projection			=	''

	SELECT	@Projection			=	Projection
	FROM	Spatial_Reference_System
	WHERE	Spatial_Ref_System	=	@SpatialRefSystem

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Projection_Get_ForSpatialReferenceSystem') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Projection_Get_ForSpatialReferenceSystem'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Projection_Get_ForSpatialReferenceSystem TO [Dev - JNCC SQL]
END
GO