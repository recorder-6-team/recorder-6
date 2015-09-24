IF NOT EXISTS (SELECT * FROM IW_Column_Type WHERE IW_Column_Type_Key = 'SYSTEM010000000T')
	INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence,
		Parser_Class_Name, Maximum_Length, Entered_By, Entry_Date, System_Supplied_Data)
	VALUES ('SYSTEM010000000T', 'TLocationInfoColumnType', 'Location Name', 0, 1, 8, 'TTextParser', 100,
		'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'SYSTEM010000000T' AND Pattern = 'location n%')
	INSERT INTO IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data)
	VALUES ('SYSTEM010000000T', 'location n%', 0, 'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'SYSTEM0100000000' AND Pattern = 'location n%')
	INSERT INTO IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data)
	VALUES ('SYSTEM0100000000', 'location n%', 1, 'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

UPDATE IW_Column_Type
SET Class_Name = 'TLocationInfoColumnType'
WHERE IW_Column_Type_Key IN ('SYSTEM0100000000', 'SYSTEM0100000001')
GO

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Relationship WHERE IW_Column_Type_Key = 'SYSTEM0100000000' AND Related_IW_Column_Type_Key = 'SYSTEM010000000T')
	INSERT INTO IW_Column_Type_Relationship (IW_Column_Type_Key, Related_IW_Column_Type_Key, Relationship_Type,
		Entered_By, Entry_Date, System_Supplied_Data)
	VALUES('SYSTEM0100000000', 'SYSTEM010000000T', 5, 'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Relationship WHERE IW_Column_Type_Key = 'SYSTEM010000000T' AND Related_IW_Column_Type_Key = 'SYSTEM0100000000')
	INSERT INTO IW_Column_Type_Relationship (IW_Column_Type_Key, Related_IW_Column_Type_Key, Relationship_Type,
		Entered_By, Entry_Date, System_Supplied_Data)
	VALUES('SYSTEM010000000T', 'SYSTEM0100000000', 5, 'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Relationship WHERE IW_Column_Type_Key = 'SYSTEM010000000T' AND Related_IW_Column_Type_Key = 'SYSTEM0100000001')
	INSERT INTO IW_Column_Type_Relationship (IW_Column_Type_Key, Related_IW_Column_Type_Key, Relationship_Type,
		Entered_By, Entry_Date, System_Supplied_Data)
	VALUES('SYSTEM010000000T', 'SYSTEM0100000001', 5, 'NBNSYS0000000004', 'Jan 31 2005  5:0', 1)
GO

UPDATE IW_Column_Type
SET Parser_Class_Name = 'TRequiredSpatialRefParser'
WHERE IW_Column_Type_Key = 'SYSTEM0100000001'
GO