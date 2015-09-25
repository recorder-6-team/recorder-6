IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE Name='Has_Children' AND ID=OBJECT_ID('Index_Taxon_Name'))
	ALTER TABLE Index_Taxon_Name
	ADD Has_Children BIT NOT NULL DEFAULT 0
GO

UPDATE ITN
SET Has_Children=1 
FROM Index_Taxon_Name ITN
INNER JOIN Taxon_List_Item TLIChild 
	ON TLIChild.Parent=ITN.Taxon_List_Item_Key

