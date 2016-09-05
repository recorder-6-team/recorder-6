/****** Changes associated with preventing an address from exporting  ******/

ALTER TABLE Database_Relationship
ADD Exclude_Type varchar(1) 

GO

UPDATE Database_Relationship set Exclude_Type = '0'

GO

UPDATE Database_Relationship set Exclude_Type = 'A' WHERE RElationship_Key IN ('NBNSYS0000000000',
'NBNSYS000000000Y','NBNSYS000000000Z','NBNSYS0000000010')


