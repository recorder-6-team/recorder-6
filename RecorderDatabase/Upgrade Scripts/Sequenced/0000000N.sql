/*============================================================================*\
  Update File_Name and Dataset_Sheet_FileName fields to remove unnecessary 
  paths. No where clause, process the whole table.
\*============================================================================*/
UPDATE	Map_Sheet
SET 	[File_Name] = 
	CASE 
		-- Need to keep Source path for Raster/Vector layers. And polygon layers don't have path.
		WHEN Sheet_Type IN (1, 2) OR CharIndex('\', Reverse([File_Name])) = 0 THEN 
			[File_Name]
		-- Strip path off base maps, we know where to get them from.
		WHEN CharIndex('\', Reverse([File_Name])) > 0 THEN
			Reverse(Left(Reverse([File_Name]), CharIndex('\', Reverse([File_Name]))-1))
	END,
	Dataset_Sheet_FileName = 
	CASE 
		-- Strip path off ALL Dataset Sheet FileNames. We know where they are located.
		WHEN CharIndex('\', Reverse([Dataset_Sheet_FileName])) > 0 THEN
			Reverse(Left(Reverse([Dataset_Sheet_FileName]), CharIndex('\', Reverse([Dataset_Sheet_FileName]))-1))
		ELSE
			Dataset_Sheet_FileName
	END

/*============================================================================*\
  Some variables.
\*============================================================================*/
DECLARE @Key char(16), 
	@FileName varchar(100), 
	@SpatialSystem varchar(4), 
	@EnteredBy char(16),
	@CompID varchar(16), 
	@BaseMapKey char(16),
	@MapSheetKey char(16)

/*============================================================================*\
  Find and process all "Base maps".
\*============================================================================*/
DECLARE curBaseMaps CURSOR FOR
	SELECT 	DISTINCT [File_Name], Spatial_Ref_System, Entered_By
	FROM 	Map_Sheet
	WHERE 	Sheet_Type = 0

OPEN curBaseMaps
FETCH NEXT FROM curBaseMaps INTO @FileName, @SpatialSystem, @EnteredBy
WHILE @@Fetch_Status = 0 BEGIN
	-- Do not allow duplicates.
	IF NOT Exists(SELECT * FROM Base_Map WHERE Original_FileName = @FileName)
	BEGIN
		EXECUTE spNextKey 'Base_Map', @Key OUTPUT

		INSERT INTO Base_Map(
			Base_Map_Key, Original_FileName, Spatial_System, Display_Name, 
			Original_FileName_Before_Reset, Spatial_System_Before_Reset, Entered_By
		) VALUES (
			@Key, @FileName, @SpatialSystem, 'Base Map Sheet', @FileName, @SpatialSystem, @EnteredBy
		)

		UPDATE  Map_Sheet
		SET	Base_Map_Key = @Key
		WHERE	[File_Name] = @FileName
	END
	FETCH NEXT FROM curBaseMaps INTO @FileName, @SpatialSystem, @EnteredBy
END
CLOSE curBaseMaps
DEALLOCATE curBaseMaps

/*============================================================================*\
  Remove the Spatial_Ref_System on BaseMaps, it's irrelevant now.
\*============================================================================*/
UPDATE	Map_Sheet SET Spatial_Ref_System = NULL WHERE Sheet_Type = 0

/*============================================================================*\
  Find and process all Computer Maps.
\*============================================================================*/
DECLARE curComputerMaps CURSOR FOR
	SELECT	Computer_ID, Base_Map_Key, Entered_By
	FROM 	Map_Sheet
	WHERE	Computer_ID IS NOT NULL AND Sheet_Type = 0

OPEN curComputerMaps
FETCH NEXT FROM curComputerMaps INTO @CompID, @BaseMapKey, @EnteredBy
WHILE @@Fetch_Status = 0 BEGIN
	IF NOT Exists(SELECT * FROM Computer_Map WHERE Computer_ID = @CompID AND Base_Map_Key = @BaseMapKey)
	BEGIN
		EXECUTE spNextKey 'Computer_Map', @Key OUTPUT

		INSERT INTO Computer_Map(
			Computer_Map_Key, Computer_ID, Base_Map_Key, Default_Map, Entered_By			
		) VALUES (
			@Key, @CompID, @BaseMapKey, 1, @EnteredBy
		)
	END

	FETCH NEXT FROM curComputerMaps INTO @CompID, @BaseMapKey, @EnteredBy
END
CLOSE curComputerMaps
DEALLOCATE curComputerMaps

/*============================================================================*\
  Remove "duplicate" base maps, if MAP_SHEET table wasn't completely "clean".
\*============================================================================*/
SELECT 	TOP 1 @Key = MS1.Map_Sheet_Key
FROM 	Map_Sheet MS1
JOIN	Map_Sheet MS2 ON MS1.[File_Name] = MS2.[File_Name] 
	AND MS1.Computer_ID = MS2.Computer_ID AND MS1.Base_Map_Key = MS2.Base_Map_Key AND MS1.Map_Sheet_Key <> MS2.Map_Sheet_Key

WHILE @Key IS NOT NULL BEGIN
	DELETE	Map_Sheet
	WHERE 	Map_Sheet_Key = @Key

	-- Reset to NULL, or it keeps previous value!!!
	SET 	@Key = NULL

	-- Find out if there is more to do.	
	SELECT 	TOP 1 @Key = MS1.Map_Sheet_Key
	FROM 	Map_Sheet MS1
	JOIN	Map_Sheet MS2 ON MS1.[File_Name] = MS2.[File_Name] 
		AND MS1.Computer_ID = MS2.Computer_ID AND MS1.Base_Map_Key = MS2.Base_Map_Key AND MS1.Map_Sheet_Key <> MS2.Map_Sheet_Key
END

/*============================================================================*\
  Duplicate all other sheets for each different CompID/BaseMap combination.  
\*============================================================================*/
-- Use cartesian product to get all combinations.
DECLARE curSheets CURSOR FOR
	SELECT	MS.Map_Sheet_Key, CM.Computer_ID, CM.Base_Map_Key
	FROM	Map_Sheet MS, Computer_Map CM
	WHERE 	MS.Base_Map_Key IS NULL

OPEN curSheets
FETCH NEXT FROM curSheets INTO @MapSheetKey, @CompID, @BaseMapKey
WHILE @@Fetch_Status = 0 BEGIN
	EXECUTE spNextKey 'Map_Sheet', @Key OUTPUT

	-- Keep File_Name and Dataset_Sheet_FileName, or MapServer is not going to like it.
	INSERT INTO Map_Sheet(
		Map_Sheet_Key, Computer_ID, Base_Map_Key, [File_Name],
		Sheet_Name, Sheet_Type, SW_Spatial_Ref, NE_Spatial_Ref, Spatial_Ref_System,
		SW_Lat, SW_Long, NE_Lat, NE_Long, NE_Spatial_Ref_Qualifier, SW_Spatial_Ref_Qualifier,
		Cut_In_Scale, Cut_Out_Scale, Sheet_Displayed, Entered_By, Entry_Date, Changed_By, Changed_Date,
		New_Data, Modified_Data, Remove_Sheet, Dataset_Sheet_Name, Dataset_Sheet_FileName, Dataset_Sheet_Order,
		Selected_Colour, Unselected_Colour, Pattern_Index
	)
	SELECT	@Key, @CompID, @BaseMapKey, [File_Name],
		Sheet_Name, Sheet_Type, SW_Spatial_Ref, NE_Spatial_Ref, Spatial_Ref_System,
		SW_Lat, SW_Long, NE_Lat, NE_Long, NE_Spatial_Ref_Qualifier, SW_Spatial_Ref_Qualifier,
		Cut_In_Scale, Cut_Out_Scale, Sheet_Displayed, Entered_By, Entry_Date, Changed_By, Changed_Date,
		New_Data, 1, Remove_Sheet, Dataset_Sheet_Name, Dataset_Sheet_FileName, Dataset_Sheet_Order,
		Selected_Colour, Unselected_Colour, Pattern_Index
	FROM Map_Sheet WHERE Map_Sheet_Key = @MapSheetKey

	FETCH NEXT FROM curSheets INTO @MapSheetKey, @CompID, @BaseMapKey
END
CLOSE curSheets
DEALLOCATE curSheets

/*============================================================================*\
  Remove original records, they have been duplicated and are now superfluous.
\*============================================================================*/
DELETE 	Map_Sheet
WHERE	Base_Map_Key IS NULL
