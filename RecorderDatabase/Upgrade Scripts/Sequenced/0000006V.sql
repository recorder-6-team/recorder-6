/*============================================================================*\
  Set size of Accuracy field in all measurement tables to 20.
\*============================================================================*/
ALTER TABLE Biotope_Occurrence_Data ALTER COLUMN Accuracy VARCHAR(20)
ALTER TABLE Location_Data ALTER COLUMN Accuracy VARCHAR(20)
ALTER TABLE Sample_Data ALTER COLUMN Accuracy VARCHAR(20)
ALTER TABLE Taxon_Occurrence_Data ALTER COLUMN Accuracy VARCHAR(20)

/*============================================================================*\
  Set size of all Data and Accuracy fields to 20 for consistency.
\*============================================================================*/
UPDATE IW_Output_Field
SET    Data_Type = 'VARCHAR(20)'
WHERE  IW_Output_Field_Key IN (
	-- Accuracy
	'SYSTEM010000000T',
	'SYSTEM010000001I',
	'SYSTEM010000001N',
	-- Data
	'SYSTEM010000000S',
	'SYSTEM010000001H',
	'SYSTEM010000001M'
)

