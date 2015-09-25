IF NOT EXISTS(SELECT * FROM Usable_Table WHERE Usable_Table_Key='ABABABAB00000062')
	INSERT INTO Usable_Table
	VALUES ('ABABABAB00000062', 'Index_Taxon_Name ITN2', 'Index_Taxon_Name ITN', 
	    'ITN.Recommended_Taxon_List_Item_Key=ITN2.Recommended_Taxon_List_Item_Key',
			NULL, 'T', 4)

UPDATE Usable_Table SET Join_Order=6 WHERE Usable_Table_Key='ABABABAB00000051'

UPDATE Usable_Table SET Join_Order=5, 
	Link_Table='Index_Taxon_Name ITN2',
	Link='Taxon_List_Version.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key'
WHERE Usable_Table_Key='ABABABAB00000050'