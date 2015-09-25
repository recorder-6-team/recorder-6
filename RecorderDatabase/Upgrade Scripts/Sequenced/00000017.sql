if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Table_Rule_Generating_Table]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Table_Rule_Generating_Table]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Table_Rule_Related_Table]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Table_Rule_Related_Table]
GO

CREATE TABLE [dbo].[IW_Table_Rule_Related_Table] (
	[IW_Table_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Table_Name] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relationship] [tinyint] NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[IW_Table_Rule_Related_Table] WITH NOCHECK ADD
	CONSTRAINT [PK_IW_Table_Rule_Related_Table] PRIMARY KEY  CLUSTERED
	(
		[IW_Table_Rule_Key],
		[Table_Name]
	)  ON [PRIMARY]
GO

DECLARE @user_id CHAR(16),
        @date    DATETIME

SELECT  @user_id    =   'NBNSYS0000000004',
        @date       =   '20040610'

INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'Survey_Event', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'Survey_Event', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'CT_SYSTEM0100000004', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'Sample', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'Survey_Event_Recorder', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'Sample', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'Sample', 2, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'Sample', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'CT_SYSTEM010000000Q', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'CT_SYSTEM010000000S', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'Sample', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'Biotope_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'Taxon_Occurrence', 1, @user_id, @date, 1)
GO

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM0100000004', 0, 'NBNSYS0000000004', GetDate(), 1)
GO

