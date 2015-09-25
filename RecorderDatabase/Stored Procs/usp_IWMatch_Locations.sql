/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Locations') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_Locations]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/07/04 11:10 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Locations]
AS
	DECLARE @EarthRadiusKM float
	SET 	@EarthRadiusKM = 6378
				
	-- Set Match_Count first.
	UPDATE	#Locations
	SET	Match_Count =  (SELECT	Count(DISTINCT L.Location_Key)
				FROM	#Master M
				JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
				JOIN	Location L ON L.Location_Key = LN.Location_Key
				JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
				WHERE	M.SYSTEM0100000000_Data = Import_Value
				AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
						COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
						SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Locations
	SET	Match_Key = L.Location_Key,
		Match_Value = LN2.Item_Name,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM	#Master M
	JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
	JOIN	Location L ON L.Location_Key = LN.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	M.SYSTEM0100000000_Data = Import_Value
	AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Locations') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Locations'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO [Dev - JNCC SQL]
END
GO