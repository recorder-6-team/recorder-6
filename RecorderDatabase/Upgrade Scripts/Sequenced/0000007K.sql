
/*==================================================================================================*\
	Script created by Lynn Heeley 02 October 2009 to update the where_SQL for RedList_GB_post94:EX 
	for the report wizard attrbiutes
 \*================================================================================================*/

--Report_Where table:
UPDATE Report_Where SET Where_SQL = 'TD.Taxon_Designation_Type_Key=''NBNSYS0000000104'' AND TD.source_key=''JNCCDEV600000001''', Changed_By = 'NBNSYS0000000011', Changed_Date = GetDate() where Report_Where_Key = 'NBNSYS0000000047'

INSERT INTO dbo.REPORT_WHERE (REPORT_WHERE_KEY, WHERE_SQL, ENTERED_BY, ENTRY_DATE,CHANGED_BY,CHANGED_DATE,SYSTEM_SUPPLIED_DATA) VALUES ('NBNSYS0000000081','TD.Taxon_Designation_Type_Key=''NBNSYS0000000100''','NBNSYS0000000011',GetDate(),NULL,NULL,-1)

--REPORT ATTRBIUTE TABLE
INSERT INTO dbo.REPORT_attribute (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,ATTRIBUTE_SQL,REPORT_JOIN_KEY,REPORT_WHERE_KEY,ENTERED_BY,ENTRY_DATE,CHANGED_BY,CHANGED_DATE,SYSTEM_SUPPLIED_DATA) VALUES ('NBNSYS0000000146','Taxon\Statuses','TAXON','Nationally scarce marine species','#REPORT_OUTPUT.[Nationally scarce marine species]=''Yes','NBNSYS000000003A','NBNSYS0000000081','NBNSYS0000000011',GetDate(),NULL,NULL,-1)

--REPORT_FIELD TABLE 
INSERT INTO dbo.REPORT_field (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,ENTERED_BY,ENTRY_DATE,CHANGED_BY,CHANGED_DATE,SYSTEM_SUPPLIED_DATA) VALUES ('NBNSYS0000000142','NBNSYS0000000146','Nationally scarce marine species','text','3','NBNSYS0000000011',GetDate(),NULL,NULL,-1)
