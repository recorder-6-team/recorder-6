/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_LocationCentroid') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_LocationCentroid]
GO

/*===========================================================================*\
  Description:	Set the centroid of a location that hasn't been matched to one
		already existing in Recorder. These values will be used when 
		making a new entry in Location table

  Parameters:	@ImportValue
		@SpatialRef
		@SpatialRefSystem
		@Lat
		@Long

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 6/07/04 11:10 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_LocationCentroid]
	@ImportValue varchar(100),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20)
AS
	-- And update match table.
	UPDATE	#Locations
	SET	Spatial_Ref = @SpatialRef,
		Spatial_Ref_System = @SpatialRefSystem,
		Lat = @Lat,
		Long = @Long,
		Spatial_Ref_Qualifier = @SpatialRefQualifier
	WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_LocationCentroid') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_LocationCentroid'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_LocationCentroid TO [Dev - JNCC SQL]
END
GO