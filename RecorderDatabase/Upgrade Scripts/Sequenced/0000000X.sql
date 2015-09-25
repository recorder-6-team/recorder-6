
-- Ensure the Spatial_Ref_System column in Map_Sheet is VarChar(4)
ALTER TABLE Map_Sheet
	ALTER COLUMN Spatial_Ref_System varchar(4)

-- Ensure the Spatial_Ref_System column in Survey is VarChar(4)
ALTER TABLE Survey
	ALTER COLUMN Spatial_Ref_System varchar(4)
