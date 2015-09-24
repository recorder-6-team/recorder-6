-- Script to add Preferred information for taxon lists, CCN117 and 118
IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE NAME='Preferred_List' AND ID=Object_ID('Index_Taxon_Name'))
	ALTER TABLE Index_Taxon_Name
		ADD PREFERRED_LIST BIT NOT NULL DEFAULT 0
GO

IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE NAME='Preferred' AND ID=Object_ID('Taxon_List'))
	ALTER TABLE Taxon_List
		ADD PREFERRED BIT NOT NULL DEFAULT 0
GO

UPDATE Taxon_List SET PREFERRED=1 WHERE Taxon_List_Key IN ('NBNSYS0000000074')

UPDATE ITN
SET ITN.Preferred_List=1
FROM Index_Taxon_Name ITN
INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key
WHERE TL.Preferred=1



