
/*
	Fix for broken taxa. the Preferred_Name must exist as a Taxon_List_Item_Key. 
*/

UPDATE	TLI1
SET	Preferred_Name = TLI1.Taxon_List_Item_Key
FROM 	Taxon_List_Item TLI1
LEFT JOIN Taxon_List_Item TLI2 ON TLI1.Preferred_Name = TLI2.Taxon_List_Item_Key
WHERE	TLI2.Taxon_List_Item_Key IS NULL
