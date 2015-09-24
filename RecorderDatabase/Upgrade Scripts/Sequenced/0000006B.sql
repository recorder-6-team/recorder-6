/*===========================================================================*\
  Description:
	Table for storing the projections for each spatial reference system.

  Created:	February 2009

  Last revision information:
    $Revision: 4 $
    $Date: 4/03/09 9:03 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.Spatial_Reference_System') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Spatial_Reference_System (
		Spatial_Ref_System	VARCHAR(4) NOT NULL
		CONSTRAINT	PK_Spatial_Reference_System PRIMARY KEY NONCLUSTERED,
		Projection VARCHAR(1000) NOT NULL
	)

-- Fills the table with the hardcoded spatial reference systems
-- TODO: Create the correct Projections- these are just test values.
INSERT INTO dbo.Spatial_Reference_System VALUES ('OSGB', 'COMPD_CS["OSGB36 / British National Grid + ODN",PROJCS["OSGB 1936 / British National Grid",GEOGCS["OSGB 1936",DATUM["OSGB_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],TOWGS84[375,-111,431,0,0,0,0],AUTHORITY[["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["DMSH",0.0174532925199433,AUTHORITY["EPSG","9108"]],AXIS["Lat",NORTH],AXIS["Long",EAST],AUTHORITY[["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.999601272],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["E",EAST],AXIS["N",NORTH],AUTHORITY[["EPSG","27700"]],VERT_CS["Newlyn",VERT_DATUM["Ordnance Datum Newlyn",2005,AUTHORITY["EPSG","5101"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Up",UP],AUTHORITY[["EPSG","5701"]],AUTHORITY[["EPSG","7405"]]')
INSERT INTO dbo.Spatial_Reference_System VALUES ('OSIG', 'COMPD_CS["OSGB36 / British National Grid + ODN",PROJCS["OSGB 1936 / British National Grid",GEOGCS["OSGB 1936",DATUM["OSGB_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],TOWGS84[375,-111,431,0,0,0,0],AUTHORITY[["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["DMSH",0.0174532925199433,AUTHORITY["EPSG","9108"]],AXIS["Lat",NORTH],AXIS["Long",EAST],AUTHORITY[["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.999601272],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["E",EAST],AXIS["N",NORTH],AUTHORITY[["EPSG","27700"]],VERT_CS["Newlyn",VERT_DATUM["Ordnance Datum Newlyn",2005,AUTHORITY["EPSG","5101"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Up",UP],AUTHORITY[["EPSG","5701"]],AUTHORITY[["EPSG","7405"]]')
INSERT INTO dbo.Spatial_Reference_System VALUES ('LTLN', 'COMPD_CS["OSGB36 / British National Grid + ODN",PROJCS["OSGB 1936 / British National Grid",GEOGCS["OSGB 1936",DATUM["OSGB_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],TOWGS84[375,-111,431,0,0,0,0],AUTHORITY[["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["DMSH",0.0174532925199433,AUTHORITY["EPSG","9108"]],AXIS["Lat",NORTH],AXIS["Long",EAST],AUTHORITY[["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.999601272],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["E",EAST],AXIS["N",NORTH],AUTHORITY[["EPSG","27700"]],VERT_CS["Newlyn",VERT_DATUM["Ordnance Datum Newlyn",2005,AUTHORITY["EPSG","5101"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Up",UP],AUTHORITY[["EPSG","5701"]],AUTHORITY[["EPSG","7405"]]')
INSERT INTO dbo.Spatial_Reference_System VALUES ('UTM', 'COMPD_CS["OSGB36 / British National Grid + ODN",PROJCS["OSGB 1936 / British National Grid",GEOGCS["OSGB 1936",DATUM["OSGB_1936",SPHEROID["Airy 1830",6377563.396,299.3249646,AUTHORITY["EPSG","7001"]],TOWGS84[375,-111,431,0,0,0,0],AUTHORITY[["EPSG","6277"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["DMSH",0.0174532925199433,AUTHORITY["EPSG","9108"]],AXIS["Lat",NORTH],AXIS["Long",EAST],AUTHORITY[["EPSG","4277"]],PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",49],PARAMETER["central_meridian",-2],PARAMETER["scale_factor",0.999601272],PARAMETER["false_easting",400000],PARAMETER["false_northing",-100000],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["E",EAST],AXIS["N",NORTH],AUTHORITY[["EPSG","27700"]],VERT_CS["Newlyn",VERT_DATUM["Ordnance Datum Newlyn",2005,AUTHORITY["EPSG","5101"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Up",UP],AUTHORITY[["EPSG","5701"]],AUTHORITY[["EPSG","7405"]]')

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetProjectionForSpatialReferenceSystem') AND SysStat & 0xf = 4)
	DROP PROCEDURE dbo.usp_GetProjectionForSpatialReferenceSystem
GO
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
    $Revision: 4 $
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Get_ForPolygon') AND SysStat & 0xf = 4)
	DROP PROCEDURE dbo.usp_Location_Get_ForPolygon
GO

/*===========================================================================*\
  Description:
	Gets information on the location

  Parameters:
	@ObjectID		The checklist which the species are being recovered from.
	@MapSheetKey	The Name_Key of the current user.

  Created:	February 2009

  Last revision information:
    $Revision: 4 $
    $Date: 4/03/09 9:03 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Location_Get_ForPolygon
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
	WHERE		LB.Object_ID		=	@ObjectID
			AND	LB.Map_Sheet_Key	=	@MapSheetKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Get_ForPolygon') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Location_Get_ForPolygon'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Location_Get_ForPolygon TO [Dev - JNCC SQL]
END
GO