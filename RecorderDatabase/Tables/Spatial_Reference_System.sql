/*===========================================================================*\
  Description:
	Table for storing the projections for each spatial reference system.

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 19/02/09 13:52 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.Spatial_Reference_System') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Spatial_Reference_System (
		Spatial_Ref_System	VARCHAR(4) NOT NULL
		CONSTRAINT	PK_Spatial_Reference_System PRIMARY KEY NONCLUSTERED,
		Projection VARCHAR(1000) NOT NULL
	)

-- Populate the spatial reference system table with the hardcoded spatial references.
-- TODO: Change these to the correct Well Known Text queries for the actual coordinate systems.
INSERT INTO dbo.Spatial_Reference_System VALUES ('')


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetLocationInfoForPolygon') AND SysStat & 0xf = 4)
	DROP PROCEDURE dbo.usp_GetLocationInfoForPolygon
GO

/*===========================================================================*\
  Description:
	Gets information on the location

  Parameters:
	@ObjectID		The checklist which the species are being recovered from.
	@MapSheetKey	The Name_Key of the current user.

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 19/02/09 13:52 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_GetLocationInfoForPolygon
	@ObjectID		SMALLINT,
	@MapSheetKey	CHAR(16)
AS

	SELECT		LN.Item_Name,
				L.Location_Key,
				L.File_Code
	FROM		Location_Boundary	LB
	INNER JOIN	Location			L
			ON	L.Location_Key		=	LB.Location_Key
	INNER JOIN	Location_Name		LN
			ON	LN.Location_Key		=	L.Location_Key
			AND	LN.Preferred		=	1
	WHERE		LB."Object_ID"		=	@ObjectID
			AND	LB.Map_Sheet_Key	=	@MapSheetKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetLocationInfoForPolygon') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_GetLocationInfoForPolygon'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_GetLocationInfoForPolygon TO [Dev - JNCC SQL]
END
GO