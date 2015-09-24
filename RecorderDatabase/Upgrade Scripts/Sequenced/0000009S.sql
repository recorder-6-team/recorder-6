SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 506: Matching of Location shows key instead of name 
\*============================================================================*/




GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Locations]    Script Date: 10/01/2015 21:33:43 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 10 jan 2015 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = LN.Item_Name,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	IW_Matched_Locations 
	JOIN	Location L ON L.Location_Key = Matched_Key
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL AND Import_Value <> L.LOCATION_KEY
	     
    UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = L.Location_key, 
		Match_Value = L.Location_key,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	Location L 
	JOIN #Locations ON L.Location_Key = Import_value
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Match_Key IS NULL 