-- correct wrong Changed_Date
UPDATE  Grid_Square
SET		Changed_Date = dateadd(mi, 1, Entry_Date)
WHERE	Changed_Date < Entry_Date


-- Update empty grid square reference systems/ changed_by
UPDATE GRID_SQUARE
SET Spatial_Ref_System = Temp.Spatial_Ref_System,
	Changed_By		   = Temp.Changed_By  
FROM GRID_SQUARE
INNER JOIN	(
	SELECT DISTINCT		gs.Location_Key,
						gs.Spatial_Ref_System,
						gs.Changed_By
	FROM	GRID_SQUARE gs
	INNER JOIN (
		SELECT	Location_Key,
				MAX(Changed_Date) as Last_Updated
		FROM GRID_SQUARE
		WHERE Spatial_Ref_System <>''
		GROUP BY Location_Key 
	) AS  LatestUpdated
	ON	LatestUpdated.Location_Key = gs.Location_Key 
	AND LatestUpdated.Last_Updated = gs.Changed_Date 
	WHERE gs.Spatial_Ref_System <> '' AND Changed_By <> ''
) AS Temp
ON Temp.Location_Key = GRID_SQUARE.Location_Key
WHERE GRID_SQUARE.Spatial_Ref_System ='' 

GO


	




