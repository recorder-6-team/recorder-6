UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Source_Key AS ItemKey FROM Reference WHERE dbo.ufn_RtfToPlainText(Full_Reference)' 
WHERE Usable_Field_Key = 'ABABABAB00000070'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Location_Key AS ItemKey FROM Location WHERE dbo.ufn_RtfToPlainText(DESCRIPTION)' 
WHERE Usable_Field_Key = 'JNCCDEV100000049'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Survey_Key AS ItemKey FROM Survey WHERE dbo.ufn_RtfToPlainText(DESCRIPTION)' 
WHERE Usable_Field_Key = 'JNCCDEV100000050'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Survey_Event_Key AS ItemKey FROM Survey_Event WHERE dbo.ufn_RtfToPlainText(Comment)' 
WHERE Usable_Field_Key = 'JNCCDEV100000051'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Sample_Key AS ItemKey FROM Sample WHERE dbo.ufn_RtfToPlainText(Comment)' 
WHERE Usable_Field_Key = 'JNCCDEV100000054'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Taxon_Occurrence_Key AS ItemKey FROM Taxon_Occurrence WHERE dbo.ufn_RtfToPlainText(Comment)' 
WHERE Usable_Field_Key = 'JNCCDEV100000056'

UPDATE Usable_Field 
SET Calculation_SQL = 'SELECT Biotope_Occurrence_Key AS ItemKey FROM Biotope_Occurrence WHERE dbo.ufn_RtfToPlainText(Comment)' 
WHERE Usable_Field_Key = 'JNCCDEV100000058'


