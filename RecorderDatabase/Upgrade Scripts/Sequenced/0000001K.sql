IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE NAME='Allow_Data_Entry' AND ID=Object_ID('Taxon_List_Type')) 
	ALTER TABLE Taxon_List_Type
		ADD Allow_Data_Entry BIT NOT NULL DEFAULT 1
GO

IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE NAME='Allow_Data_Entry' AND ID=Object_ID('Index_Taxon_Name')) 
	ALTER TABLE Index_Taxon_Name
		ADD Allow_Data_Entry BIT NOT NULL DEFAULT 1
GO

UPDATE Taxon_List_Type 
SET Allow_Data_Entry=0 
WHERE Taxon_List_Type_Key IN ('NBNSYS0000000001', 'NBNSYS0000000002', 'NBNSYS0000000003')

-- Ensure Index Taxon Name is updated
UPDATE ITN 
SET ITN.Preferred_List=TL.Preferred, ITN.Allow_Data_Entry=TLT.Allow_Data_Entry 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key 
INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key 
INNER JOIN Taxon_List_Type TLT ON TL.Taxon_List_Type_Key=TLT.Taxon_List_Type_Key 