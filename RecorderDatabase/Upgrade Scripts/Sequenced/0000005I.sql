IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE Name='ENTERED_BY' and ID=Object_Id('Measurement_Unit_Value'))
	ALTER TABLE Measurement_Unit_Value 
	ADD	[ENTERED_BY] [char](16) NULL,
		[ENTRY_DATE] [smalldatetime] NOT NULL DEFAULT (getdate()),
		[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL  DEFAULT (0)
GO

UPDATE Measurement_Unit_Value
SET [ENTERED_BY]='TESTDATA00000001'
WHERE [ENTERED_BY] IS NULL

ALTER TABLE Measurement_Unit_Value
ALTER COLUMN [ENTERED_BY] [char](16) NOT NULL


