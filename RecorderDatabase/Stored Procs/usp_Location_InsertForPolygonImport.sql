/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Location_InsertForPolygonImport') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_Location_InsertForPolygonImport
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Inserts a new location into the database for linking to a polygon which
			is being imported.

  Created:	April 2009

  Last revision information:
    $Revision: 1 $
    $Date: 2/04/09 15:13 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Location_InsertForPolygonImport
	@LocationName			VARCHAR(100),
	@SpatialRef				VARCHAR(40) = NULL,
	@SpatialRefSystem		VARCHAR(4),
	@Lat					FLOAT,
	@Long					FLOAT,
	@SpatialRefQualifier	VARCHAR(20),
	@LocationType			CHAR(16),
	@UserID					CHAR(16),
	@PolygonIndex			INT
AS
	DECLARE	@LocationKey		CHAR(16)
	DECLARE	@LocationNameKey	CHAR(16)
	DECLARE	@Custodian			CHAR(8)

	SET @Custodian = (SELECT Data FROM Setting WHERE Name = 'SiteID')

	EXECUTE	spNextKey	'Location',			@LocationKey		OUTPUT
	EXECUTE spNextKey	'Location_Name',	@LocationNameKey	OUTPUT

	INSERT INTO	Location	(
			Location_Key,
			Spatial_Ref,
			Spatial_Ref_System,
			Lat,
			Long,
			Location_Type_Key,
			Spatial_Ref_Qualifier,
			Entered_By,
			Entry_Date,
			System_Supplied_Data,
			Custodian
	)
	VALUES					(
			@LocationKey,
			@SpatialRef,
			@SpatialRefSystem,
			@Lat,
			@Long,
			@LocationType,
			@SpatialRefQualifier,
			@UserID,
			GetDate(),
			0,
			@Custodian
	)

	INSERT INTO	Location_Name	(
			Location_Name_Key,
			Item_Name,
			Preferred,
			Location_Key,
			Entered_By,
			Entry_Date,
			Custodian
	)
	VALUES						(
			@LocationNameKey,
			@LocationName,
			1,
			@LocationKey,
			@UserID,
			GetDate(),
			@Custodian
	)

	UPDATE	#Polygons
	SET		LocationKey		=	@LocationKey,
			LocationName	=	@LocationName
	WHERE	PolygonIndex	=	@PolygonIndex

GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_InsertForPolygonImport') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Location_InsertForPolygonImport'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_InsertForPolygonImport TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_InsertForPolygonImport TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Location_InsertForPolygonImport TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Location_InsertForPolygonImport TO [Dev - JNCC SQL]
END
GO