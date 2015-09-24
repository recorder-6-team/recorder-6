
/*===========================================================================*\
  Update all SpatiaL_Ref fields to english format (dots and commas rather than
  commas and semicolons)
\*===========================================================================*/

UPDATE Survey
SET		SW_Spatial_Ref = Replace(Replace(SW_Spatial_Ref, ',', '.'), ';', ',')
WHERE	CharIndex(';', SW_Spatial_Ref) > 0

UPDATE Survey
SET		NE_Spatial_Ref = Replace(Replace(NE_Spatial_Ref, ',', '.'), ';', ',')
WHERE	CharIndex(';', NE_Spatial_Ref) > 0

UPDATE Survey_Event
SET		Spatial_Ref = Replace(Replace(Spatial_Ref, ',', '.'), ';', ',')
WHERE	CharIndex(';', Spatial_Ref) > 0

UPDATE Sample
SET		Spatial_Ref = Replace(Replace(Spatial_Ref, ',', '.'), ';', ',')
WHERE	CharIndex(';', Spatial_Ref) > 0

UPDATE Location
SET		Spatial_Ref = Replace(Replace(Spatial_Ref, ',', '.'), ';', ',')
WHERE	CharIndex(';', Spatial_Ref) > 0

