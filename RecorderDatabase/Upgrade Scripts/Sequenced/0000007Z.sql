
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations]    Script Date: 09/09/2011 08:15:35 ******/
IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[usp_IWMatch_Locations]') AND type in (N'P', N'PC'))
DROP PROCEDURE [dbo].[usp_IWMatch_Locations]
GO


/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

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
				-- next is a safety check to avoid domain errors on ACOS
				AND COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
						COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
						SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat)) BETWEEN -1 AND 1
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
	-- next is a safety check to avoid domain errors on ACOS
	AND COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat)) BETWEEN -1 AND 1
	AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_IWMatch_Locations TO "Dev - JNCC SQL"
GO