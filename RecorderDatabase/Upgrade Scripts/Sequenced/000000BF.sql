/********* Adds External Source Report Entries *****************/

UPDATE REPORT_ATTRIBUTE SET ENTERED_BY = 'TESTDATA00000001' WHERE ENTERED_BY IS NULL


GO

UPDATE REPORT_FIELD SET ENTERED_BY = 'TESTDATA00000001' WHERE ENTERED_BY IS NULL

GO

DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002200000010'
GO
DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002200000010'
GO
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002200000100'
GO
DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002200000100'
GO
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002200000200'
GO
DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002200000200'
GO

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,
ATTRIBUTE_SQL,REPORT_JOIN_KEy,REPORT_WHERE_KEY, ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002200000010','Taxon\Observations','OBSERVATION','External Key',
'#REPORT_OUTPUT.[External Key] =[dbo].[GetExternalRef](TAXON_OCCURRENCE_KEY)',
'NBNSYS0000000016','NBNSYS0000000000','TESTDATA00000001',GETDATE(),1)

GO


INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,ATTRIBUTE_SQL,
REPORT_JOIN_KEy,REPORT_WHERE_KEY, ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002200000100','Taxon\Observations','OBSERVATION','External Source',
'#REPORT_OUTPUT.[External Source] =[dbo].[GetExternalSource](TAXON_OCCURRENCE_KEY)',
'NBNSYS0000000016','NBNSYS0000000000','TESTDATA00000001',GETDATE(),1)

GO

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,ATTRIBUTE_SQL,
REPORT_JOIN_KEy,REPORT_WHERE_KEY, ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002200000200','Taxon\Observations','OBSERVATION','External Source/Ref',
'#REPORT_OUTPUT.[External Source/Key] =[dbo].[GetExternalSourceRef](TAXON_OCCURRENCE_KEY)',
'NBNSYS0000000016','NBNSYS0000000000','TESTDATA00000001',GETDATE(),1)

GO

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('LCA0002200000010','LCA0002200000010','External Key','varchar',505,'TESTDATA00000001',GETDATE(),1)

GO
INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('LCA0002200000100','LCA0002200000100','External Source','varchar',155,'TESTDATA00000001',GETDATE(),1)

GO

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('LCA0002200000200','LCA0002200000200','External Source/Key','varchar',655,'TESTDATA00000001',GETDATE(),1)

GO

UPDATE IW_COLUMN_TYPE SET MAXIMUM_LENGTH = 100 WHERE IW_COLUMN_TYPE_KEY = 'LCA00023000000R8'

GO

UPDATE IW_Output_Field SET DATA_TYPE = 'VARCHAR(100)' WHERE IW_OUTPUT_FIELD_KEY = 'LCA00023000000R8'

GO

INSERT INTO IW_COLUMN_TYPE (IW_COLUMN_TYPE_KEY,CLASS_NAME,ITEM_NAME,REQUIRED,COMMONLY_USED,SEQUENCE,
FIELD_TYPE,PARSER_CLASS_NAME,MAXIMUM_LENGTH,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA00023000000R9','TColumnType','External Source',0,0,10,null,
'TTextParser',30,'TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_Post_Processing_Procedure (IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,
Procedure_Name,Entered_By,Entry_Date,System_Supplied_Data) VALUES ('LCA00023000000RA',10,
'Taxon_Private_Data','usp_IW_Taxon_Private_Item_Name','TESTDATA00000001',GETDATE(),1)


GO

INSERT INTO IW_Column_Type_Relationship (IW_Column_Type_Key,Related_IW_Column_Type_Key, Relationship_Type,
Entered_By,Entry_Date,System_Supplied_Data) VALUES(
'LCA00023000000R9','LCA00023000000R8',2,'TESTDATA00000001',getdate(),1)
