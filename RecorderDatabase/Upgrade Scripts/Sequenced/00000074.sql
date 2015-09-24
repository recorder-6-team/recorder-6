	IF NOT EXISTS(SELECT * FROM SysObjects WHERE Name='Reference_Location' AND type='U')
BEGIN

	CREATE TABLE [dbo].[Reference_Location] (
		[Reference_Location_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Source_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Location_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Entry_Date] [datetime] NOT NULL ,
		[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Changed_Date] [datetime] NULL 
	) ON [PRIMARY]
	
	ALTER TABLE [dbo].[Reference_Location] WITH NOCHECK ADD 
		CONSTRAINT [PK_Reference_Location] PRIMARY KEY  CLUSTERED 
		(
			[Reference_Location_Key]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[Reference_Location] ADD 
		CONSTRAINT [DF_Reference_Location_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
	
	CREATE  INDEX [IX_Source_Key] ON [dbo].[Reference_Location]([Source_Key]) ON [PRIMARY]
	
	ALTER TABLE [dbo].[Reference_Location] ADD 
		CONSTRAINT [FK_Reference_Location_LOCATION] FOREIGN KEY 
		(
			[Location_Key]
		) REFERENCES [dbo].[LOCATION] (
			[LOCATION_KEY]
		) ON DELETE CASCADE ,
		CONSTRAINT [FK_Reference_Location_REFERENCE] FOREIGN KEY 
		(
			[Source_Key]
		) REFERENCES [dbo].[REFERENCE] (
			[SOURCE_KEY]
		) ON DELETE CASCADE 
END
GO

IF NOT EXISTS(SELECT * FROM SysObjects WHERE Name='Reference_Supplier' AND type='U')
BEGIN
	CREATE TABLE [dbo].[Reference_Supplier] (
		[Reference_Supplier_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Supplier_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Source_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Recommended] [bit] NOT NULL ,
		[Location] [varchar] (500) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Media_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Format_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Availability_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Entry_Date] [datetime] NOT NULL ,
		[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Changed_Date] [datetime] NULL,
		[Custodian] [char] (8) NOT NULL DEFAULT 'TESTDATA'
	) ON [PRIMARY]
	
	ALTER TABLE [dbo].[Reference_Supplier] WITH NOCHECK ADD 
		CONSTRAINT [PK_Reference_Supplier] PRIMARY KEY  CLUSTERED 
		(
			[Reference_Supplier_Key]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[Reference_Supplier] ADD 
		CONSTRAINT [DF_Reference_Supplier_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
	
	CREATE  INDEX [IX_Source_Key] ON [dbo].[Reference_Supplier]([Source_Key]) ON [PRIMARY]
	
	ALTER TABLE [dbo].[Reference_Supplier] ADD 
		CONSTRAINT [FK_Reference_Supplier_Availability] FOREIGN KEY 
		(
			[Availability_Concept_Key]
		) REFERENCES [dbo].[Concept] (
			[Concept_Key]
		),
		CONSTRAINT [FK_Reference_Supplier_Format] FOREIGN KEY 
		(
			[Format_Concept_Key]
		) REFERENCES [dbo].[Concept] (
			[Concept_Key]
		),
		CONSTRAINT [FK_Reference_Supplier_Media] FOREIGN KEY 
		(
			[Media_Concept_Key]
		) REFERENCES [dbo].[Concept] (
			[Concept_Key]
		),
		CONSTRAINT [FK_Reference_Supplier_ORGANISATION] FOREIGN KEY 
		(
			[Supplier_Name_Key]
		) REFERENCES [dbo].[ORGANISATION] (
			[NAME_KEY]
		) ON DELETE CASCADE ,
		CONSTRAINT [FK_Reference_Supplier_REFERENCE] FOREIGN KEY 
		(
			[Source_Key]
		) REFERENCES [dbo].[REFERENCE] (
			[SOURCE_KEY]
		) ON DELETE CASCADE 
END
GO

EXEC spCreateCustodianTrigger 'Reference_Supplier', 'Reference_Supplier_Key'
GO

IF NOT EXISTS(SELECT * FROM SysObjects WHERE Name='Reference_Survey_Metadata' AND type='U')
BEGIN
	CREATE TABLE [dbo].[Reference_Survey_Metadata] (
		[Source_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Commissioned_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Digitised] [bit] NOT NULL ,
		[Digitisation_Quality] [tinyint] NULL ,
		[Gis_Link_Quality] [tinyint] NULL ,
		[Comments] [varchar] (2000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Entry_Date] [datetime] NOT NULL ,
		[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[Changed_Date] [datetime] NULL 
	) ON [PRIMARY]
	
	ALTER TABLE [dbo].[Reference_Survey_Metadata] WITH NOCHECK ADD 
		CONSTRAINT [PK_Reference_Survey_Metadata] PRIMARY KEY  CLUSTERED 
		(
			[Source_Key]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[Reference_Survey_Metadata] ADD 
		CONSTRAINT [CK_Digitisation_Quality] CHECK ([Digitisation_Quality] >= 1 and [Digitisation_Quality] <= 5),
		CONSTRAINT [CK_Gis_Link_Quality] CHECK ([Gis_Link_Quality] >= 1 and [Gis_Link_Quality] <= 5)
	
	ALTER TABLE [dbo].[Reference_Survey_Metadata] ADD 
		CONSTRAINT [FK_Reference_Survey_Metadata_ORGANISATION] FOREIGN KEY 
		(
			[Commissioned_By_Name_Key]
		) REFERENCES [dbo].[ORGANISATION] (
			[NAME_KEY]
		),
		CONSTRAINT [FK_Reference_Survey_Metadata_REFERENCE] FOREIGN KEY 
		(
			[Source_Key]
		) REFERENCES [dbo].[REFERENCE] (
			[SOURCE_KEY]
		) ON DELETE CASCADE 
END
GO

GRANT  SELECT  ON [dbo].[Reference_Location]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Reference_Location]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT ,  DELETE  ON [dbo].[Reference_Location]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Location]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Location]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Reference_Supplier]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Reference_Supplier]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT ,  DELETE  ON [dbo].[Reference_Supplier]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Supplier]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Supplier]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Reference_Survey_Metadata]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Reference_Survey_Metadata]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT ,  DELETE  ON [dbo].[Reference_Survey_Metadata]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Survey_Metadata]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Reference_Survey_Metadata]  TO [R2k_Administrator]
GO