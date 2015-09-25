/* Fix filters so that if you filter for a survey where run by organisation is not x, then surveys run by an 
   individual are returned, and vice versa*/
UPDATE Usable_Field
SET Calculation_SQL = 'SELECT Survey_Key AS ItemKey FROM Survey S LEFT JOIN Individual I ON S.Run_By = I.Name_Key WHERE Surname IS NULL OR Surname'
WHERE Usable_Field_Key='ABABABAB00000077'

UPDATE Usable_Field
SET Calculation_SQL = 'SELECT Survey_Key AS ItemKey FROM Survey S LEFT JOIN Organisation O ON S.Run_By = O.Name_Key WHERE Full_Name IS NULL OR Full_Name'
WHERE Usable_Field_Key='ABABABAB00000078'


