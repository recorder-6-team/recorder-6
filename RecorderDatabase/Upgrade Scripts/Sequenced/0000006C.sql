/*===========================================================================*\
  Column Type for Spatial Reference System.
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM IW_Column_Type WHERE IW_Column_Type_Key = 'SYSTEM010000000U')
	INSERT INTO IW_Column_Type(
		IW_Column_Type_Key, 
		Class_Name, 
		Item_Name, 
		Required, 
		Commonly_Used, 
		Parser_Class_Name, 
		Maximum_Length, 
		Entered_By, 
		System_Supplied_Data) 
	VALUES (
		'SYSTEM010000000U', 
		'TColumnType', 
		'Spatial Reference System', 
		0, 
		0, 
		'TTextParser', 
		10, 
		'SYSTEM0000000000', 
		1)

/*===========================================================================*\
  Relationship to link Spatial Reference System to Spatial Reference.
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM IW_Column_Type_Relationship WHERE IW_Column_Type_Key = 'SYSTEM010000000U')
	INSERT INTO IW_Column_Type_Relationship(
		IW_Column_Type_Key,
		Related_IW_Column_Type_Key,
		Relationship_Type,
		Entered_By,
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000U',
		'SYSTEM0100000001',
		0,
		'SYSTEM0000000000',
		1)

/*===========================================================================*\
  Pattern records for Spatial Reference System.
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'SYSTEM010000000U') BEGIN
	INSERT INTO IW_Column_Type_Pattern(
		IW_Column_Type_Key,
		Pattern, 
		Exclude_Match,
		Entered_By, 
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000U',
		'ref%sys%',
		0,
		'SYSTEM0000000000',
		1)
	
	INSERT INTO IW_Column_Type_Pattern(
		IW_Column_Type_Key,
		Pattern, 
		Exclude_Match,
		Entered_By, 
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000U',
		'spatial%ref%sys%',
		0,
		'SYSTEM0000000000',
		1)
	
	INSERT INTO IW_Column_Type_Pattern(
		IW_Column_Type_Key,
		Pattern, 
		Exclude_Match,
		Entered_By, 
		System_Supplied_Data)
	VALUES (
		'SYSTEM010000000U',
		'spatial%sys%',
		0,
		'SYSTEM0000000000',
		1)
END

/*===========================================================================*\
  Update match rule to concatenate Spatial Ref System to Spatial Ref.
\*===========================================================================*/
UPDATE IW_Match_Rule
SET Imported_Data_Insert_SQL =
	'DECLARE @GridRefs varchar(100), @LocationName varchar(100)
	DECLARE @CRLF char(2)
	SET @CRLF=CHAR(13)+CHAR(10)
	DECLARE curLocations CURSOR FOR
		SELECT Import_Value FROM #Locations    
	OPEN curLocations
	FETCH NEXT FROM curLocations INTO @LocationName  
	WHILE @@Fetch_Status = 0 BEGIN
		SET @GridRefs = ''''
		SELECT @GridRefs = CASE 
			WHEN CharIndex(SYSTEM0100000001_Spatial_Ref, @GridRefs) = 0 
				THEN @GridRefs + @CRLF + SYSTEM0100000001_Spatial_Ref + ''|'' + SYSTEM0100000001_system
			ELSE @GridRefs END
		FROM #Master
		WHERE SYSTEM0100000000_Data = @LocationName     -- Remove superfluous leading carriage return.
		
		IF SubString(@GridRefs, 1, 2) = @CRLF
			SET @GridRefs = SubString(@GridRefs, 3, Len(@GridRefs))     -- Update field in match table.
		
		UPDATE #Locations
			SET Import_Grid_Reference = @GridRefs
		WHERE Import_Value = @LocationName
		
		FETCH NEXT FROM curLocations INTO @LocationName
	END
	CLOSE curLocations
	DEALLOCATE curLocations '
WHERE IW_Match_Rule_Key = 'SYSTEM0100000003'

GO
