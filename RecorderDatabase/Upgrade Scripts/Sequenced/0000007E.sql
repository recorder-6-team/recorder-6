/*===========================================================================*\
  Script created by Steve Wilkinson 8 July 2009 to rectify bug from earlier script.
  Additional generating field (comment) for survey events.
\*===========================================================================*/
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entry_date, Entered_By, System_Supplied_Data) 
VALUES ('SYSTEM0100000001', 'JNCCDEV500000005',	1, GetDate(), 'NBNSYS0000000005', 1)

INSERT INTO IW_COLUMN_TYPE_PATTERN (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data)
VALUES('SYSTEM0100000004', '%Event%Com%', 1, 'NBNSYS0000000005', GetDate(), 1)

/*===========================================================================*\
  Additional generating field ( comment) for samples.
\*===========================================================================*/

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entry_date, Entered_By, System_Supplied_Data) 
VALUES ('SYSTEM0100000002', 'JNCCDEV500000004',	1, GetDate(), 'NBNSYS0000000005', 1)