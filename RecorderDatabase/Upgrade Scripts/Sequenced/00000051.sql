-- Fix field length of specimen number field
UPDATE Report_Field
SET Field_Size=30 
WHERE Report_Field_Key='NBNSYS0000000067'