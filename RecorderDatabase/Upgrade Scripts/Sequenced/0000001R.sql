UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #AbundanceQualifiers(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM0100000005'

UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #Substrates(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM0100000006'

UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #RecordTypes(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM0100000007'

UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #AssociationTypes(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM0100000008'

UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #SpecimenTypes(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM0100000009'

UPDATE IW_Match_Rule
SET Table_Create_SQL='CREATE TABLE #SampleTypes(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,   Remembered BIT DEFAULT 0  )
'
WHERE IW_Match_Rule_Key='SYSTEM010000000B'