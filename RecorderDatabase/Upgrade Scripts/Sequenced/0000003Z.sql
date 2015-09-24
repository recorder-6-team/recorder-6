/*Script by S.Shaw 25-02-07. Corrects the misspelling of term 'Exuvae' to 'exuvia' in MEASUREMENT_QUALIFIER table.*/ 

UPDATE Measurement_Qualifier
SET Short_name = 'Exuvia', long_name = 'Exuvia'
WHERE Measurement_Qualifier_Key = 'NBNSYS0000000047'