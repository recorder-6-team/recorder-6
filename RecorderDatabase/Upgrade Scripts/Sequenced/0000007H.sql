/*==================================================================================================*\
	Script created by Lynn Heeley 4 August 2009 to match three column headings in the import wizard:
	Survey Event Weather, Taxon Determination Comment and Taxon Occurrence Comment
 \*================================================================================================*/

--Survey event weather:
INSERT into dbo.IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data) VALUES ('SYSTEM0100000004','%weath%',1,'NBNSYS0000000005', GetDate(), 1)

					
--Taxon occurrence comment:
UPDATE IW_Column_Type_Pattern SET Pattern = '%comm%', Changed_By = 'NBNSYS0000000011', Changed_Date = GetDate() where IW_Column_Type_Key = 'SYSTEM0100000006' and Pattern = 'comm%'

INSERT into dbo.IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data) 
VALUES ('SYSTEM0100000003', '%comm%', 1, 'NBNSYS0000000011', GetDate(), 1)							
							

--Taxon determination comment:
UPDATE IW_Column_Type_Pattern SET Pattern = 'Tax%Det%Com%', Changed_by = 'NBNSYS0000000011', Changed_Date = GetDate() where IW_Column_Type_Key = 'JNCCDEV500000006'						

INSERT into dbo.IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date,  System_Supplied_Data) 
VALUES ('SYSTEM0100000003', 'Tax%Det%Com%', 1, 'NBNSYS0000000011', Getdate(), 1)							

INSERT into dbo.IW_Column_Type_Pattern (IW_Column_Type_Key, Pattern, Exclude_Match, Entered_By, Entry_Date, System_Supplied_Data) 
VALUES  ('SYSTEM0100000006', 'Tax%Det%Com%', 1, 'NBNSYS0000000011', GetDate(), 1)							






							
