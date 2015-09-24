
/* Update Calculation_SQL for filter on Determiner for Taxon Occurrences to allow for Confidential filter. */
UPDATE	Usable_Field
SET 	Calculation_SQL = 
'SELECT TXO.Taxon_Occurrence_Key AS ItemKey FROM Taxon_Occurrence TXO
JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key = TXO.Taxon_Occurrence_Key
JOIN Individual I ON TD.Determiner = I.Name_Key 
WHERE Surname'
WHERE	Usable_Field_Key = 'ABABABAB00000087'

