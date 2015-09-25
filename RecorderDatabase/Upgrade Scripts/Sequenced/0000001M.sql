--	This script is designed to add necessary rows to the Report_ tables
--	It uses the highest key values available from the test database

--	KJG 9/12/2004

IF NOT EXISTS(SELECT * FROM Report_Join WHERE Report_Join_Key='NBNSYS0000000039')
insert into report_join values(
'NBNSYS0000000039',
'FROM #REPORT_OUTPUT LEFT JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY=#REPORT_OUTPUT.LIST_ITEM_KEY AND ITN.SYSTEM_SUPPLIED_DATA=1 LEFT JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY=ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY AND INDEX_TAXON_NAME.SYSTEM_SUPPLIED_DATA=1',
NULL,
Getdate(),
NULL,
NULL,
1
)

IF NOT EXISTS(SELECT * FROM report_attribute WHERE report_attribute_Key='NBNSYS0000000086')
insert into report_attribute values(
'NBNSYS0000000086',
'Taxon',
'TAXON_LIST',
'Recommended Taxon Name',
'#REPORT_OUTPUT.[Recommended Taxon Name] = Index_Taxon_Name.Preferred_name',
'NBNSYS0000000039',
NULL,
NULL,
Getdate(),
NULL,
NULL,
1
)

IF NOT EXISTS(SELECT * FROM report_attribute WHERE report_attribute_Key='NBNSYS0000000087')
insert into report_attribute values(
'NBNSYS0000000087',
'Taxon',
'TAXON_LIST',
'Recommended Taxon Sort Order',
'#REPORT_OUTPUT.[Recommended Taxon Sort Order] = Index_Taxon_Name.Sort_Order',
'NBNSYS0000000039',
NULL,
NULL,
Getdate(),
NULL,
NULL,
1
)

IF NOT EXISTS(SELECT * FROM Report_Field WHERE Report_Field_Key='NBNSYS0000000094')
insert into report_field values(
'NBNSYS0000000094',
'NBNSYS0000000086',
'Recommended Taxon Name',
'varchar',
60,
NULL,
Getdate(),
NULL,
NULL,
1
)

IF NOT EXISTS(SELECT * FROM report_field WHERE Report_Field_Key='NBNSYS0000000095')
insert into report_field values(
'NBNSYS0000000095',
'NBNSYS0000000087',
'Recommended Taxon Sort Order',
'varchar',
30,
NULL,
Getdate(),
NULL,
NULL,
1
)

