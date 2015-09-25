CREATE TABLE [dbo].[Application] (
	[Application_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [char] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]

CREATE TABLE [dbo].[Application_Concept_Group] (
	[Application_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]

ALTER TABLE [dbo].[Application] WITH NOCHECK ADD 
	CONSTRAINT [PK_Application] PRIMARY KEY  CLUSTERED 
	(
		[Application_Key]
	)  ON [PRIMARY] 

ALTER TABLE [dbo].[Application_Concept_Group] WITH NOCHECK ADD 
	CONSTRAINT [PK_Application_Concept_Group] PRIMARY KEY  CLUSTERED 
	(
		[Application_Key],
		[Concept_Group_Key]
	)  ON [PRIMARY] 

ALTER TABLE [dbo].[Application] ADD 
	CONSTRAINT [DF_Application_System_Supplied_Data] DEFAULT (1) FOR [System_Supplied_Data]

ALTER TABLE [dbo].[Application_Concept_Group] ADD 
	CONSTRAINT [FK_Application_Concept_Group_Application] FOREIGN KEY 
	(
		[Application_Key]
	) REFERENCES [dbo].[Application] (
		[Application_Key]
	),
	CONSTRAINT [FK_Application_Concept_Group_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [dbo].[Concept_Group] (
		[Concept_Group_Key]
	)
