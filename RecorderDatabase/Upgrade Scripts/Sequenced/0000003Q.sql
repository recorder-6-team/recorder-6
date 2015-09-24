UPDATE Report_Join SET Join_SQL='FROM #REPORT_OUTPUT 
INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=#REPORT_OUTPUT.List_Item_Key
INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
INNER JOIN Taxon_Designation TD ON TD.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
INNER JOIN Taxon_Designation_Type ON Taxon_Designation_Type.Taxon_Designation_Type_Key=TD.Taxon_Designation_Type_Key'
WHERE Report_Join_Key='NBNSYS0000000032'

UPDATE Report_Attribute SET Report_Join_Key='NBNSYS0000000032' WHERE Report_Join_Key='NBNSYS0000000033'

DELETE FROM Report_Join WHERE Report_Join_Key='NBNSYS0000000033'

