/****** Changes associated with preventing unwanted reltionships exporting  ******/



/****** Object:  Table [dbo].[Database_Relationship_Tables]    Script Date: 09/07/2016 20:51:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Database_Relationship_Tables](
	[Relationship_Key] [char](16) NOT NULL,
	[Table_Name] [varchar](100) NOT NULL,
	[Related_Field_1] [varchar](100) NOT NULL,
	[Related_Field_2] [varchar](100) NOT NULL,
	[Main_Table] [varchar](100) NOT NULL,
	[Main_Key_Field] [varchar](100) NOT NULL,
	[Has_System_Supplied] [bit] NOT NULL)
        ON [PRIMARY] 

SET ANSI_PADDING OFF
GO

  
ALTER TABLE [dbo].[Database_Relationship_Tables] ADD 
	CONSTRAINT [PK_RELATIONSHIP_KEY] PRIMARY KEY CLUSTERED 
	( [RELATIONSHIP_KEY]
	)  ON [PRIMARY] 

GO

GRANT SELECT ON [dbo].[Database_Relationship_Tables] TO R2k_AddOnly 
GRANT INSERT ON [dbo].[Database_Relationship_Tables] TO R2k_AddOnly 
GRANT SELECT ON [dbo].[Database_Relationship_Tables] TO R2k_ReadOnly 
GRANT SELECT ON [dbo].[Database_Relationship_Tables] TO R2k_RecordCardsOnly
GRANT SELECT ON [dbo].[Database_Relationship_Tables] TO R2k_Administrator 
GRANT INSERT ON [dbo].[Database_Relationship_Tables] TO R2k_Administrator 
GRANT DELETE ON [dbo].[Database_Relationship_Tables] TO R2k_Administrator 
GRANT UPDATE ON [dbo].[Database_Relationship_Tables] TO R2k_Administrator 
GRANT SELECT ON [dbo].[Database_Relationship_Tables] TO R2k_FullEdit
GRANT INSERT ON [dbo].[Database_Relationship_Tables] TO R2k_FullEdit 
GRANT DELETE ON [dbo].[Database_Relationship_Tables] TO R2k_FullEdit
GRANT UPDATE ON [dbo].[Database_Relationship_Tables] TO R2k_FullEdit
GO

INSERT INTO [dbo].[Database_Relationship_Tables] VALUES('NBNSYS0000000001','LOCATION_RELATION','LOCATION_KEY_1',
'LOCATION_KEY_2','LOCATION','LOCATION_KEY',1)

INSERT INTO [dbo].[Database_Relationship_Tables] VALUES('NBNSYS0000000002','SAMPLE_RELATION','SAMPLE_KEY_1',
'SAMPLE_KEY_1','SAMPLE','SAMPLE_KEY',0)


INSERT INTO [dbo].[Database_Relationship_Tables] VALUES('NBNSYS0000000003','COMMUNICATION','NAME_KEY_1',
'NAME_KEY_2','NAME','NAME_KEY',0)

INSERT INTO [dbo].[Database_Relationship_Tables] VALUES('NBNSYS0000000004','NAME_RELATION','NAME_KEY_1',
'NAME_KEY_2','NAME','NAME_KEY',0)




 