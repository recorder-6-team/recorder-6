IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE Name='ONE_TO_ONE' AND ID=OBJECT_ID('Database_Relationship'))
ALTER TABLE Database_Relationship 
ADD ONE_TO_ONE BIT NOT NULL DEFAULT 0
GO

UPDATE Database_Relationship
SET ONE_TO_ONE=1
WHERE (Master_Table='Individual' AND Detail_Table='Name')
OR (Master_Table='Organisation' AND Detail_Table='Name')
OR (Master_Table='Source' AND Detail_Table='Source_File')
OR (Master_Table='Source' AND Detail_Table='Reference')