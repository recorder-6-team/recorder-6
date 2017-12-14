/****** adds a new field to Computer_Map to keep track of the Object_Sheet_file path******/

ALTER TABLE Computer_Map
ADD  Object_Sheet_Folder varchar(200) null

Go

ALTER TABLE Computer_Map
ADD  Master bit null 

GO

UPDATE COMPUTER_MAP SET MASTER = 0

GO

UPDATE COMPUTER_MAP SET MASTER = 1 WHERE Computer_ID = Host_Name()

GO

IF NOT EXISTS (SELECT * FROM COMPUTER_MAP WHERE MASTER = 1)
AND EXISTS (SELECT * FROM COMPUTER_MAP)
UPDATE COMPUTER_MAP SET MASTER = 1 WHERE COMPUTER_MAP_KEY =
(SELECT MIn(COMPUTER_MAP_KEY) FROM COMPUTER_MAP)
