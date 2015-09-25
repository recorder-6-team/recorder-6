UPDATE dbo.USABLE_TABLE
SET LINK = 'Taxon_Determination.Taxon_Occurrence_Key = Taxon_Occurrence.Taxon_Occurrence_Key and Taxon_Determination.Preferred = 1'
WHERE USABLE_TABLE_KEY = 'ABABABAB00000027'