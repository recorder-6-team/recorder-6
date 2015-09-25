/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Locations_Select_InBoundingBox]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Locations_Select_InBoundingBox]
GO

/*===========================================================================*\
  Description:	Obtain a list of sites where the centroid or ANY grid square
		overlaps into a bounding box

  Parameters:	
			@swlat, @swlong - south west coordinates of box
			@nelat, @nelong - north east coordinates of box

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/04 16:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Locations_Select_InBoundingBox]
	@swlat FLOAT, 
	@swlong FLOAT, 
	@nelat FLOAT, 
	@nelong FLOAT
AS

SET NOCOUNT ON 

-- some constants to simplify the calculations
DECLARE	@EARTH_RADIUS_KM FLOAT
DECLARE @swLongRad FLOAT
DECLARE @swLatRad FLOAT
DECLARE @SinSwLatRad FLOAT 
DECLARE @CosSwLatRad FLOAT 

SET @EARTH_RADIUS_KM = 6378
SET @swLongRad = RADIANS(@swlong)
SET @swLatRad = RADIANS(@swlat)
SET @SinSwLatRad = SIN(@swLatRad)
SET @CosSwLatRad = COS(@swLatRad)

SELECT DISTINCT l.Location_key
FROM Location l
LEFT JOIN grid_square g on g.location_Key=l.location_key
	AND (
	-- Find grid squares where recorded point inside box
	(g.lat>=@swlat and g.lat<=@nelat and g.long>=@swlong and g.long<=@nelong)
	-- find grid squares sw of box which overlap
	OR (g.Lat<@swlat and g.long<@swlong  
		and @EARTH_RADIUS_KM * ACOS(@CosSwLatRad * @CosSwLatRad *
		COS(RADIANS(g.Long) - @swLongRad) + @SinSwLatRad * @SinSwLatRad) <= (g.[size]/1000)
		and @EARTH_RADIUS_KM * ACOS(COS(RADIANS(g.Lat)) * @CosSwLatRad *
		COS(@swLongRad - @swLongRad) + SIN(RADIANS(g.Lat)) * @SinSwLatRad) <= (g.[size]/1000))
	-- find grid squares w of box which overlap
	OR (g.Lat>=@swlat and g.Lat<= @nelat and g.long<@swlong and 
		@EARTH_RADIUS_KM * ACOS(@CosSwLatRad * @CosSwLatRad *
		COS(RADIANS(g.Long) - @swLongRad) + @SinSwLatRad * @SinSwLatRad) <= (g.[size]/1000))
	-- find grid squares s of box which overlap
	OR (g.Lat<@swlat and g.long>=@swlong and g.long<=@nelong and
		@EARTH_RADIUS_KM * ACOS(COS(RADIANS(g.Lat)) * @CosSwLatRad *
		COS(@swLongRad - @swLongRad) + SIN(RADIANS(g.Lat)) * @SinSwLatRad) <= (g.[size]/1000))
	)
LEFT JOIN Grid_Square g2 on g2.Location_Key=l.Location_Key
WHERE 
	g.Grid_Square_Key IS NOT NULL
	OR (
		g2.Grid_Square_Key IS NULL AND 
		l.lat>=@swlat and 
		l.lat<=@nelat and 
		l.long>=@swlong and 
		l.long<=@nelong
	)

	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_InBoundingBox') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Locations_Select_InBoundingBox'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_InBoundingBox TO [Dev - JNCC SQL]
END
GO