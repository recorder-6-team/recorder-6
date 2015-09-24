IF NOT EXISTS(SELECT 1 FROM SYSINDEXES WHERE NAME='IX_Recommended' AND OBJECT_NAME(ID)='Index_Taxon_Name')
	CREATE INDEX IX_Recommended ON Index_Taxon_Name(Recommended_Taxon_List_Item_Key)
