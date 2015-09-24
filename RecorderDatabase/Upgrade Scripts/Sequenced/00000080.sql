-- Fix for Mantis 293

-- Report field output should not be text if the field has a field size defined.
UPDATE Report_Field 
SET Field_Type='varchar'
WHERE Field_Type='text'
AND Field_Size IS NOT NULL

-- Some corrections to the field sizes
-- Determination Type should be 21 not 20
UPDATE Report_Field SET Field_Size = 21 WHERE Report_Field_Key='JNCCDEV100000012'
-- Association Type (Relationship Type.Short_Name) should be 40 not 20
UPDATE Report_Field SET Field_Size = 40 WHERE Report_Field_Key='JNCCDEV100000014'