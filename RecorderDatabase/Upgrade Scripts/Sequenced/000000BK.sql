/****** Adds Taxon Code and Specimen Location  to Report Wizard  ******/
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002200000700'
GO
DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002200000700'
GO
INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_NAME,ITEM_GROUP,SOURCE_TABLE,
ATTRIBUTE_SQL,REPORT_JOIN_KEY,REPORT_WHERE_KEY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('LCA0002200000700','Taxon LIst Code','Taxon','Taxon_List',
'#REPORT_OUTPUT.[Taxon List Code] = TAXON_LIST_ITEM.LST_ITM_CODE', 'NBNSYS0000000030',
'NBNSYS0000000000','TESTDATA00000001',GetDate(),1)
GO
INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,ENTERED_BY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,
FIELD_TYPE,FIELD_SIZE,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('LCA0002200000700','TESTDATA00000001','LCA0002200000700','Taxon List Code',
'varchar',35,GetDate(),1)
GO
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'NBNSYS0000000167'
GO

DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'NBNSYS0000000167'

GO

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,    ITEM_NAME,    ITEM_GROUP,    SOURCE_TABLE,
ATTRIBUTE_SQL,    REPORT_JOIN_KEY,REPORT_WHERE_KEY,   
ENTERED_BY,    ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000167','Specimen Location',    'Taxon', 'TAXON',
'#REPORT_OUTPUT.[Specimen Location] = SPECIMEN.LOCATION',
'NBNSYS0000000024',    'NBNSYS0000000000',    'TESTDATA00000001',getdate(),1)

GO

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,    ENTERED_BY,    REPORT_ATTRIBUTE_KEY,
FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000167','TESTDATA00000001',    'NBNSYS0000000167','Specimen Location',
'varchar', 100,Getdate(),1)



