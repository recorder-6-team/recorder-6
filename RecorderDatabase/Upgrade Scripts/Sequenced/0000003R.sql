UPDATE Usable_Field 
SET Calculation_SQL='SELECT Survey_Key AS ItemKey FROM Survey S LEFT JOIN Individual I ON S.Run_By = I.Name_Key WHERE Surname'
WHERE Usable_Field_Key='ABABABAB00000077'

UPDATE Usable_Field 
SET Calculation_SQL='SELECT Survey_Key AS ItemKey FROM Survey S LEFT JOIN Organisation O ON S.Run_By = O.Name_Key WHERE Full_Name'
WHERE Usable_Field_Key='ABABABAB00000078'