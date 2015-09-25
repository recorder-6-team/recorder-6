DELETE FROM Usable_Table WHERE Usable_Table_Key IN ('ABABABAB00000063', 'ABABABAB00000064', 'ABABABAB00000065',
		'ABABABAB00000066', 'ABABABAB00000067', 'ABABABAB00000068', 'ABABABAB00000069', 'ABABABAB0000006A', 'ABABABAB0000006B')

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000063', 
		'Index_Taxon_Synonym ITS', 
		'Taxon_Determination', 
		'ITS.Synonym_List_Item_Key = Taxon_Determination.Taxon_List_Item_Key',
		'T',
		2)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000064', 
		'Index_Taxon_Name ITN3', 
		'Index_Taxon_Synonym ITS', 
		'ITS.Taxon_List_Item_Key=ITN3.Taxon_List_Item_Key',
		'T',
		 3)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000065', 
		'Taxon_List_Version TLV2', 
		'Index_Taxon_Name ITN3', 
		'TLV2.Taxon_List_Version_Key = ITN3.Taxon_List_Version_Key',
		'T',
		 4)

UPDATE Usable_Table 
SET Table_Name = 'Taxon_List TL', 
		Link_Table='Taxon_List_Version TLV', 
		Link='TL.Taxon_List_Key = TLV.Taxon_List_Key',
		Join_Order=5
WHERE Usable_Table_Key='ABABABAB00000051'

UPDATE Usable_Table 
SET Table_Name = 'Taxon_List_Version TLV', 
		Link='TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key',
		Join_Order=4
WHERE Usable_Table_Key='ABABABAB00000050'

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000066', 
		'Taxon_List TL2', 
		'Taxon_List_Version TLV2', 
		'TL2.Taxon_List_Key = TLV2.Taxon_List_Key',
		'T',
		 5)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000067', 
		'Index_Taxon_Group ITG', 
		'Index_Taxon_Synonym ITS', 
		'ITG.Contained_List_Item_Key = ITS.Taxon_List_Item_Key',
		'T',
		 3)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000068', 
		'Index_Taxon_Group ITG2', 
		'Index_Taxon_Name ITN2', 
		'ITG2.Contained_List_Item_Key = ITN2.Taxon_List_Item_Key',
		'T',
		 4)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB00000069', 
		'Index_Taxon_Synonym ITS2', 
		'Index_Taxon_Group ITG', 
		'ITS2.Synonym_List_Item_Key=ITG.Taxon_List_Item_Key',
		'T',
		 4)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB0000006A', 
		'Index_Taxon_Name ITN4', 
		'Index_Taxon_Group ITG2', 
		'ITN4.Taxon_List_Item_Key=ITG2.Taxon_List_Item_Key',
		'T',
		 5)

INSERT INTO Usable_Table (Usable_Table_Key, Table_Name, Link_Table, Link, Apply_To, Join_Order)
VALUES ('ABABABAB0000006B', 
		'Index_Taxon_Name ITN5', 
		'Index_Taxon_Name ITN4', 
		'ITN5.Recommended_Taxon_List_Item_Key = ITN4.Recommended_Taxon_List_Item_Key',
		'T',
		 6)
