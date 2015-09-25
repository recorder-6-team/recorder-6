/*===========================================================================*\
  Additional generating field for survey events.
\*===========================================================================*/
IF NOT EXISTS (SELECT * FROM IW_Table_Rule_Related_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000001' AND IW_Column_Type_Key = 'SYSTEM010000000T')
	INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
	VALUES ('SYSTEM0100000001', 'SYSTEM010000000T', 1, 'NBNSYS0000000004', GetDate(), 1)
