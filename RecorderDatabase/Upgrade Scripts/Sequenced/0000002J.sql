update Database_relationship set Detail_Table = 'Index_Taxon_Group' where RELATIONSHIP_KEY = 'NBNSYS000000003V'

IF NOT EXISTS(SELECT 1 FROM Report_Join WHERE Report_Join_Key='NBNSYS0000000040')
Insert into Report_Join (REPORT_JOIN_KEY, JOIN_SQL, ENTERED_BY, ENTRY_DATE, SYSTEM_SUPPLIED_Data)
values ('NBNSYS0000000040', 'FROM #REPORT_OUTPUT left join Taxon_list_item on Taxon_list_item.Taxon_list_item_key = #REPORT_OUTPUT.LIST_ITEM_KEY left join Taxon_version on Taxon_version.Taxon_version_key = Taxon_list_item.Taxon_version_key left join Taxon_group on Taxon_group.Taxon_group_key =Taxon_version.Output_group_key','NBNSYS0000000001','2005-07-28', -1)

IF NOT EXISTS(SELECT 1 FROM Report_Attribute WHERE Report_Attribute_Key='NBNSYS0000000088')
Insert into Report_Attribute (REPORT_ATTRIBUTE_KEY, ITEM_GROUP, SOURCE_TABLE, ITEM_NAME, ATTRIBUTE_SQL, REPORT_JOIN_KEY, ENTERED_BY, ENTRY_DATE, SYSTEM_SUPPLIED_Data)
values ('NBNSYS0000000088','Taxon','TAXON_LIST','Taxon group','#REPORT_OUTPUT.[Taxon Group] = Taxon_group.Taxon_group_Name','NBNSYS0000000040','NBNSYS0000000001','2005-07-28', -1)

IF NOT EXISTS(SELECT 1 FROM Report_Field WHERE Report_Field_Key='NBNSYS0000000096')
Insert into Report_Field (REPORT_Field_KEY, REPORT_ATTRIBUTE_KEY, FIELD_ITEM_NAME, FIELD_TYPE, FIELD_SIZE, ENTERED_BY, ENTRY_DATE, SYSTEM_SUPPLIED_Data)
values ('NBNSYS0000000096','NBNSYS0000000088','Taxon Group','varchar', 30,'NBNSYS0000000001','2005-07-28', -1)
