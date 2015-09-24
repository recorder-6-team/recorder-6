UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #Names(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000000'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #Species(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 [Order] VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Checklist_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Species_Name VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000001'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #Biotopes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 Classification VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Classification_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000002'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #Locations(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Import_Grid_Reference VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Count INT,
 Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 Spatial_Ref VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Spatial_Ref_System VARCHAR(4) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Lat FLOAT,
 Long FLOAT,
 Spatial_Ref_Qualifier VARCHAR(20) COLLATE SQL_Latin1_General_CP1_CI_AS
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000003'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #References(
 Import_Value VARCHAR(500) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(500) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000004'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #AbundanceQualifiers(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000005'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #Substrates(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000006'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #RecordTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000007'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #AssociationTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000008'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #SpecimenTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM0100000009'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #AssociatedSpecies(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 [Order] VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Species_Name VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS
)'
WHERE	IW_Match_Rule_Key ='SYSTEM010000000A'

UPDATE	IW_Match_Rule
SET 	Table_Create_SQL =
'CREATE TABLE #SampleTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)'
WHERE	IW_Match_Rule_Key ='SYSTEM010000000B'

UPDATE      IW_Column_Type
SET         Field_Type          =   'text'
WHERE       IW_Column_Type_Key  =   'SYSTEM010000000L'

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM010000001A', 'NBNSYS0000000004', '20040610', 1)
