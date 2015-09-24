IF NOT EXISTS(SELECT * FROM SYSCOLUMNS WHERE Name='Verified' AND ID=OBJECT_ID('Determination_Type'))
	ALTER TABLE Determination_Type
	ADD Verified tinyint not null default 0
GO

IF NOT EXISTS(SELECT * FROM SYSCOLUMNS WHERE Name='Hide' AND ID=OBJECT_ID('Determination_Type'))
	ALTER TABLE Determination_Type
	ADD Hide bit not null default 0
GO

IF EXISTS(SELECT * FROM SYSCOLUMNS WHERE Name='Short_Name' AND ID=OBJECT_ID('Determination_Type'))
	ALTER TABLE Determination_Type
	ALTER COLUMN Short_Name VARCHAR(21) not null
GO

IF NOT EXISTS(SELECT * FROM SYSCOLUMNS WHERE Name='Hide' AND ID=OBJECT_ID('Determiner_Role'))
	ALTER TABLE Determiner_Role
	ADD Hide bit not null default 0
GO

UPDATE Determination_Type
SET Verified = 0
WHERE Determination_Type_Key IN ('NBNSYS0000000003', 'NBNSYS0000000004', 'NBNSYS0000000006')
GO

UPDATE Determination_Type
SET Verified = 1
WHERE Determination_Type_Key = 'NBNSYS0000000001'
GO

UPDATE Determination_Type
SET Verified = 2
WHERE Determination_Type_Key IN ('NBNSYS0000000002', 'NBNSYS0000000005')
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000012')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000012', 'Correct', 'Correct', 'NBNSYS0000000001', 1, 'NBNSYS00', 2, 0)
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000007')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000007', 'Considered Correct', 'Considered Correct', 'NBNSYS0000000001', 1, 'NBNSYS00', 2, 0)
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000008')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000008', 'Considered Incorrect', 'Considered Incorrect', 'NBNSYS0000000001', 1, 'NBNSYS00', 1, 0)
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000009')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000009', 'Incorrect', 'Incorrect', 'NBNSYS0000000001', 1, 'NBNSYS00', 1, 0)
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000010')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000010', 'Requires Confirmation', 'Requires Confirmation', 'NBNSYS0000000001', 1, 'NBNSYS00', 1, 0)
GO

IF NOT EXISTS (SELECT * FROM Determination_Type WHERE Determination_Type_Key = 'NBNSYS0000000011')
	INSERT INTO Determination_Type (Determination_Type_Key, Short_Name, Long_Name, Entered_By, System_Supplied_Data,
		Custodian, Verified, Hide)
	VALUES('NBNSYS0000000011', 'Unconfirmed', 'Unconfirmed', 'NBNSYS0000000001', 1, 'NBNSYS00', 0, 0)
GO

IF NOT EXISTS (SELECT TOP 1 * FROM Taxon_Determination)
BEGIN
	UPDATE Determination_Type
	SET Hide = 1
	WHERE Determination_Type_Key IN ('NBNSYS0000000001', 'NBNSYS0000000002', 'NBNSYS0000000003',
									'NBNSYS0000000004', 'NBNSYS0000000005', 'NBNSYS0000000006')
	
	UPDATE Determiner_Role
	SET Hide = 1
	WHERE Determiner_Role_Key = 'NBNSYS0000000002'
END
GO

UPDATE IW_Output_Field
SET Literal_Value = '''NBNSYS0000000011'''
WHERE IW_Output_Field_Key = 'SYSTEM010000001D'