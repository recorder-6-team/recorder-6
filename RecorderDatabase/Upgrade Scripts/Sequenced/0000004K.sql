/*===========================================================================*\
  IW_Column_Type_Relationship.
\*===========================================================================*/
DELETE FROM IW_Column_Type_Relationship
WHERE	IW_Column_Type_Key 			= 'SYSTEM010000000T' 
AND 	Related_IW_Column_Type_Key 	= 'SYSTEM0100000000'

DELETE FROM IW_Column_Type_Relationship
WHERE	IW_Column_Type_Key 			= 'SYSTEM0100000000' 
AND 	Related_IW_Column_Type_Key 	= 'SYSTEM010000000T'

IF NOT EXISTS (SELECT * FROM IW_Column_Type_Relationship WHERE IW_Column_Type_Key = 'SYSTEM0100000001' AND Related_IW_Column_Type_Key = 'SYSTEM010000000T')
	INSERT INTO IW_Column_Type_Relationship (IW_Column_Type_Key, Related_IW_Column_Type_Key, Relationship_Type, Entered_By, System_Supplied_Data)
	VALUES('SYSTEM0100000001', 'SYSTEM010000000T', 5, 'NBNSYS0000000004', 1)

