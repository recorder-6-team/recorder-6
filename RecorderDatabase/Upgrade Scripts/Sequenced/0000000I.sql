-- Increase Taxon_Occurrence_Data.Data field size to 20
ALTER TABLE Taxon_Occurrence_Data
	ALTER COLUMN Data VARCHAR(20)