if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Column_Type_Match_Rule_IW_Column_Type]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Column_Type_Match_Rule] DROP CONSTRAINT FK_IW_Column_Type_Match_Rule_IW_Column_Type
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Column_Type_Pattern_IW_Column_Type]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Column_Type_Pattern] DROP CONSTRAINT FK_IW_Column_Type_Pattern_IW_Column_Type
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Column_Type_Relationship_IW_Column_Type]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Column_Type_Relationship] DROP CONSTRAINT FK_IW_Column_Type_Relationship_IW_Column_Type
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Column_Type_Relationship_IW_Column_Type1]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Column_Type_Relationship] DROP CONSTRAINT FK_IW_Column_Type_Relationship_IW_Column_Type1
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Output_Field_IW_Column_Type]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Output_Field] DROP CONSTRAINT FK_IW_Output_Field_IW_Column_Type
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Table_Rule_Generating_Field_IW_Column_Type]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Table_Rule_Related_Field] DROP CONSTRAINT FK_IW_Table_Rule_Generating_Field_IW_Column_Type
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Column_Type_Match_Rule_IW_Match_Rule]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Column_Type_Match_Rule] DROP CONSTRAINT FK_IW_Column_Type_Match_Rule_IW_Match_Rule
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Table_Rule_Output_Field_IW_Output_Field]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Table_Rule_Output_Field] DROP CONSTRAINT FK_IW_Table_Rule_Output_Field_IW_Output_Field
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Table_Rule_Generating_Rule]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Table_Rule] DROP CONSTRAINT FK_IW_Table_Rule_Generating_Rule
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Table_Rule_Output_Field_IW_Table_Rule]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Table_Rule_Output_Field] DROP CONSTRAINT FK_IW_Table_Rule_Output_Field_IW_Table_Rule
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_IW_Table_Rule_Generating_Field_IW_Table_Rule]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[IW_Table_Rule_Related_Field] DROP CONSTRAINT FK_IW_Table_Rule_Generating_Field_IW_Table_Rule
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Column_Type]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Column_Type]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Column_Type_Match_Rule]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Column_Type_Match_Rule]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Column_Type_Pattern]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Column_Type_Pattern]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Column_Type_Relationship]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Column_Type_Relationship]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Match_Rule]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Match_Rule]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Abundance_Qualifiers]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Abundance_Qualifiers]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Associated_Species]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Associated_Species]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Association_Types]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Association_Types]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Biotopes]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Biotopes]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Locations]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Locations]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Names]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Names]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Record_Types]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Record_Types]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_References]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_References]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Species]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Species]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Specimen_Types]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Specimen_Types]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Substrates]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Substrates]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Matched_Sample_Types]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Matched_Sample_Types]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Output_Field]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Output_Field]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Post_Processing_Procedure]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Post_Processing_Procedure]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Table_Rule]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Table_Rule]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Table_Rule_Output_Field]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Table_Rule_Output_Field]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[IW_Table_Rule_Related_Field]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[IW_Table_Rule_Related_Field]
GO

CREATE TABLE [dbo].[IW_Column_Type] (
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Class_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Required] [bit] NOT NULL ,
	[Commonly_Used] [bit] NOT NULL ,
	[Sequence] [tinyint] NULL ,
	[Field_Type] [varchar] (20) COLLATE Latin1_General_CI_AS NULL ,
	[Parser_Class_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
    [Maximum_Length] [integer] NULL ,
	[Term_List_Table] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Column_Type_Match_Rule] (
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[IW_Match_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Field_Index] [int] NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Column_Type_Pattern] (
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Pattern] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Exclude_Match] [bit] NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Column_Type_Relationship] (
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Related_IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relationship_Type] [tinyint] NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Match_Rule] (
	[IW_Match_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [int] NULL ,
	[Item_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Control_Type] [tinyint] NOT NULL ,
	[Imported_Data_Insert_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Remembered_Matches_Procedure] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[Match_Procedure] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Record_Matches_Procedure] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[New_Entry_Procedure] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Requires_Checklist] [bit] NOT NULL ,
	[Set_Match_Procedure] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Table_Create_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Key_To_Caption_Procedure] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[Search_Type] [int] NULL ,
	[Checklists_Select_Procedure] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[Termlist_Select_Procedure] [varchar] (50) COLLATE Latin1_General_CI_AS NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [datetime] NULL ,
	[System_Supplied_Data] [bit] NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Abundance_Qualifiers] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Associated_Species] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Match_Checklist_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Association_Types] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Biotopes] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Match_Classification_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Locations] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Names] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Record_Types] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_References] (
	[Matched_Value] [varchar] (500) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Species] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Match_Checklist_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Specimen_Types] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Substrates] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Matched_Sample_Types] (
	[Matched_Value] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Matched_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Output_Field] (
	[IW_Output_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Name] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Data_Type] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Source_Field_Name] [varchar] (30) COLLATE Latin1_General_CI_AS NULL ,
	[Generating_Class_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Generator_Field_Index] [int] NULL ,
	[Literal_Value] [varchar] (20) COLLATE Latin1_General_CI_AS NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [datetime] NULL ,
	[System_Supplied_Data] [bit] NOT NULL
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Post_Processing_Procedure] (
	[IW_Post_Processing_Procedure_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [int] NOT NULL ,
	[Required_Table_Name] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Procedure_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Table_Rule] (
	[IW_Table_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [tinyint] NOT NULL ,
	[Table_Name] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Filter_Expression] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [datetime] NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Table_Rule_Output_Field] (
	[IW_Table_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[IW_Output_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

CREATE TABLE [dbo].[IW_Table_Rule_Related_Field] (
	[IW_Table_Rule_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[IW_Column_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relationship] [tinyint] NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [datetime] NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[IW_Column_Type] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Column_Type] PRIMARY KEY  CLUSTERED 
	(
		[IW_Column_Type_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Column_Type_Match_Rule] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Column_Type_Match_Rule] PRIMARY KEY  CLUSTERED 
	(
		[IW_Column_Type_Key],
		[IW_Match_Rule_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Column_Type_Pattern] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Column_Type_Pattern] PRIMARY KEY  CLUSTERED 
	(
		[IW_Column_Type_Key],
		[Pattern]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Column_Type_Relationship] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Column_Type_Relationship] PRIMARY KEY  CLUSTERED 
	(
		[IW_Column_Type_Key],
		[Related_IW_Column_Type_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Match_Rule] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Match_Rule] PRIMARY KEY  CLUSTERED 
	(
		[IW_Match_Rule_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Output_Field] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Output_Field] PRIMARY KEY  CLUSTERED 
	(
		[IW_Output_Field_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Post_Processing_Procedure] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Table_Rule_Post_Processing_Procedure] PRIMARY KEY  CLUSTERED 
	(
		[IW_Post_Processing_Procedure_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Table_Rule] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Table_Rule] PRIMARY KEY  CLUSTERED 
	(
		[IW_Table_Rule_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Table_Rule_Output_Field] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Table_Rule_Output_Field] PRIMARY KEY  CLUSTERED 
	(
		[IW_Table_Rule_Key],
		[IW_Output_Field_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Table_Rule_Related_Field] WITH NOCHECK ADD 
	CONSTRAINT [PK_IW_Table_Rule_Generating_Field] PRIMARY KEY  CLUSTERED 
	(
		[IW_Table_Rule_Key],
		[IW_Column_Type_Key],
		[Relationship]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[IW_Column_Type] ADD 
	CONSTRAINT [DF_IW_Column_Type_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[IW_Column_Type_Pattern] ADD 
	CONSTRAINT [DF_IW_Column_Type_Pattern_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[IW_Column_Type_Relationship] ADD 
	CONSTRAINT [DF_IW_Column_Type_Relationship_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[IW_Output_Field] ADD 
	CONSTRAINT [DF_IW_Output_Field_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[IW_Table_Rule] ADD 
	CONSTRAINT [DF_IW_Table_Rule_Entry_Date] DEFAULT (getdate()) FOR [Entry_Date]
GO

GRANT  SELECT  ON [dbo].[IW_Matched_Names]  TO [R2k_FullEdit]
GO

GRANT  SELECT  ON [dbo].[IW_Matched_Names]  TO [R2k_Administrator]
GO

ALTER TABLE [dbo].[IW_Column_Type_Match_Rule] ADD 
	CONSTRAINT [FK_IW_Column_Type_Match_Rule_IW_Column_Type] FOREIGN KEY 
	(
		[IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	),
	CONSTRAINT [FK_IW_Column_Type_Match_Rule_IW_Match_Rule] FOREIGN KEY 
	(
		[IW_Match_Rule_Key]
	) REFERENCES [dbo].[IW_Match_Rule] (
		[IW_Match_Rule_Key]
	)
GO

ALTER TABLE [dbo].[IW_Column_Type_Pattern] ADD 
	CONSTRAINT [FK_IW_Column_Type_Pattern_IW_Column_Type] FOREIGN KEY 
	(
		[IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	)
GO

ALTER TABLE [dbo].[IW_Column_Type_Relationship] ADD 
	CONSTRAINT [FK_IW_Column_Type_Relationship_IW_Column_Type] FOREIGN KEY 
	(
		[IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	),
	CONSTRAINT [FK_IW_Column_Type_Relationship_IW_Column_Type1] FOREIGN KEY 
	(
		[Related_IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	)
GO

ALTER TABLE [dbo].[IW_Output_Field] ADD 
	CONSTRAINT [FK_IW_Output_Field_IW_Column_Type] FOREIGN KEY 
	(
		[IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	)
GO

ALTER TABLE [dbo].[IW_Table_Rule] ADD 
	CONSTRAINT [FK_IW_Table_Rule_Generating_Rule] FOREIGN KEY 
	(
		[IW_Table_Rule_Key]
	) REFERENCES [dbo].[IW_Table_Rule] (
		[IW_Table_Rule_Key]
	)
GO

ALTER TABLE [dbo].[IW_Table_Rule_Output_Field] ADD 
	CONSTRAINT [FK_IW_Table_Rule_Output_Field_IW_Output_Field] FOREIGN KEY 
	(
		[IW_Output_Field_Key]
	) REFERENCES [dbo].[IW_Output_Field] (
		[IW_Output_Field_Key]
	),
	CONSTRAINT [FK_IW_Table_Rule_Output_Field_IW_Table_Rule] FOREIGN KEY 
	(
		[IW_Table_Rule_Key]
	) REFERENCES [dbo].[IW_Table_Rule] (
		[IW_Table_Rule_Key]
	)
GO

ALTER TABLE [dbo].[IW_Table_Rule_Related_Field] ADD 
	CONSTRAINT [FK_IW_Table_Rule_Generating_Field_IW_Column_Type] FOREIGN KEY 
	(
		[IW_Column_Type_Key]
	) REFERENCES [dbo].[IW_Column_Type] (
		[IW_Column_Type_Key]
	) ON DELETE CASCADE ,
	CONSTRAINT [FK_IW_Table_Rule_Generating_Field_IW_Table_Rule] FOREIGN KEY 
	(
		[IW_Table_Rule_Key]
	) REFERENCES [dbo].[IW_Table_Rule] (
		[IW_Table_Rule_Key]
	) ON DELETE CASCADE 
GO

