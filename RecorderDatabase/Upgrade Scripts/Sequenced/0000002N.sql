--Create the Thesaurus Tables.

--This whole script is aborted if the Thesaurus already exists
IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN

/****** Object:  Table [dbo].[Concept]    Script Date: 12/12/2005 16:33:43 ******/
CREATE TABLE [dbo].[Concept] (
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[List_Preferred] [bit] NOT NULL ,
	[Is_Current] [bit] NOT NULL ,
	[Preferred] [bit] NOT NULL ,
	[Concept_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Name_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Author_Copy] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Sort_Code] [int] NULL ,
	[List_Code] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]

/****** Object:  Table [dbo].[Concept_Designation]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_Designation] (
	[Concept_Designation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Designation_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_Group]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_Group] (
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Authority] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Url] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Hierarchy_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Last_Sequence_Number] [varchar] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_Group_Version]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_Group_Version] (
	[Concept_Group_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Version] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [int] NOT NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Acq_Vague_Date_Start] [int] NULL ,
	[Acq_Vague_Date_End] [int] NULL ,
	[Acq_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Url] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_History]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_History] (
	[Concept_History_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Version_From] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Concept_Group_Version_To] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_Lineage]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_Lineage] (
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Lineage_ID] [int] NOT NULL ,
	[Lineage] [varchar] (900) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Last_Sequence_Number] [varchar] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_Rank]    Script Date: 12/12/2005 16:33:45 ******/
CREATE TABLE [dbo].[Concept_Rank] (
	[Concept_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sort_Order] [int] NULL ,
	[Abbreviation] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Color_R] [tinyint] NOT NULL ,
	[Color_G] [tinyint] NOT NULL ,
	[Color_B] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Concept_Relation]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Concept_Relation] (
	[Concept_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Inherited] [bit] NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Domain]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Domain] (
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Subject_Area_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Has_Occurrences] [bit] NOT NULL ,
	[Default_Hierarchy_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Domain_Mask] [int] NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Domain_Hyperlink]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Domain_Hyperlink] (
	[Domain_Hyperlink_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Image_File] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[URL] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Use_Concept_Key] [bit] NULL ,
	[Word_Separator] [varchar] (5) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Language]    Script Date: 15/12/2005 15:55:16 ******/
CREATE TABLE [dbo].[Language] (
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Priority] [smallint] NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Local_Domain]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Local_Domain] (
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Label] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Meaning]    Script Date: 14/12/2005 08:44:30 ******/
CREATE TABLE [dbo].[Meaning] (
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]

/****** Object:  Table [dbo].[Meaning_Relation]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Meaning_Relation] (
	[Meaning_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Inherited] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Semantic_Relation]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Semantic_Relation] (
	[Semantic_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Unidirectional] [bit] NOT NULL ,
	[Forward_Equivalence_Possible] [bit] NOT NULL ,
	[Forward_Equivalence_Definite] [bit] NOT NULL ,
	[Reverse_Equivalence_Possible] [bit] NOT NULL ,
	[Reverse_Equivalence_Definite] [bit] NOT NULL ,
	[Proportional_Relationship] [bit] NOT NULL ,
	[Adjacent] [bit] NULL ,
	[Chronological_Overlap] [tinyint] NULL ,
	[Description] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Subject_Area]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Subject_Area] (
	[Subject_Area_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Term]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Term] (
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Plaintext] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AI NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Term_Version]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Term_Version] (
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Version_Label] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Author_And_Date] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Term_Version_Relation]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Term_Version_Relation] (
	[Term_Version_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Thesaurus_Fact]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Thesaurus_Fact] (
	[Thesaurus_Fact_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Data] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Related_Term_Versions] [bit] NOT NULL ,
	[Inherited] [bit] NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Fact_Vague_Date_Start] [int] NULL ,
	[Fact_Vague_Date_End] [int] NULL ,
	[Fact_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Fact_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


/****** Object:  Table [dbo].[Thesaurus_Relation_Type]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Thesaurus_Relation_Type] (
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Semantic_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Forward_Term] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reverse_Term] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]


/****** Object:  Table [dbo].[Thesaurus_Relation_Type_Usage]    Script Date: 12/12/2005 16:33:46 ******/
CREATE TABLE [dbo].[Thesaurus_Relation_Type_Usage] (
	[Thesaurus_Relation_Type_Usage_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relation_Usage] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]



ALTER TABLE [dbo].[Concept] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_Designation] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_Status] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Designation_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_Group] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_Group] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Group_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_Group_Version] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_List_Version] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Group_Version_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_History] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_History] PRIMARY KEY  CLUSTERED 
	(
		[Concept_History_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_Rank] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_Rank] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Rank_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Concept_Relation] WITH NOCHECK ADD 
	CONSTRAINT [PK_Concept_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Relation_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Domain] WITH NOCHECK ADD 
	CONSTRAINT [PK_Domain] PRIMARY KEY  CLUSTERED 
	(
		[Domain_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Domain_Hyperlink] WITH NOCHECK ADD 
	CONSTRAINT [PK_Domain_Hyperlink] PRIMARY KEY  CLUSTERED 
	(
		[Domain_Hyperlink_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Language] WITH NOCHECK ADD 
	CONSTRAINT [PK_Language] PRIMARY KEY  CLUSTERED 
	(
		[Language_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Local_Domain] WITH NOCHECK ADD 
	CONSTRAINT [PK_Local_Domain] PRIMARY KEY  CLUSTERED 
	(
		[Local_Domain_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Meaning_Relation] WITH NOCHECK ADD 
	CONSTRAINT [PK_Meaning_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Meaning_Relation_Key]
	)  ON [PRIMARY] 
	

ALTER TABLE [dbo].[Meaning] WITH NOCHECK ADD 
	CONSTRAINT [PK_Meaning] PRIMARY KEY  CLUSTERED 
	(
		[Meaning_Key]
	)  ON [PRIMARY] 

ALTER TABLE [dbo].[Semantic_Relation] WITH NOCHECK ADD 
	CONSTRAINT [PK_Relationship_Type_Semantic] PRIMARY KEY  CLUSTERED 
	(
		[Semantic_Relation_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Subject_Area] WITH NOCHECK ADD 
	CONSTRAINT [PK_Subject_Area] PRIMARY KEY  CLUSTERED 
	(
		[Subject_Area_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Term] WITH NOCHECK ADD 
	CONSTRAINT [PK_Term] PRIMARY KEY  CLUSTERED 
	(
		[Term_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Term_Version] WITH NOCHECK ADD 
	CONSTRAINT [PK_Author_And_Version] PRIMARY KEY  CLUSTERED 
	(
		[Term_Version_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Term_Version_Relation] WITH NOCHECK ADD 
	CONSTRAINT [PK_Term_Version_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Term_Version_Relation_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Thesaurus_Fact] WITH NOCHECK ADD 
	CONSTRAINT [PK_Thesaurus_Fact] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Fact_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Thesaurus_Relation_Type] WITH NOCHECK ADD 
	CONSTRAINT [PK_Symantic_Relation_Type] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Relation_Type_Key]
	)  ON [PRIMARY] 


ALTER TABLE [dbo].[Thesaurus_Relation_Type_Usage] WITH NOCHECK ADD 
	CONSTRAINT [PK_Thesaurus_Relation_Type_Usage] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Relation_Type_Usage_Key]
	)  ON [PRIMARY] 


 CREATE  CLUSTERED  INDEX [IX_Lineage] ON [dbo].[Concept_Lineage]([Lineage]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept] ADD 
	CONSTRAINT [DF_Concept_Is_Current] DEFAULT (1) FOR [Is_Current],
	CONSTRAINT [DF_Concept_Preferred] DEFAULT (0) FOR [Preferred],
	CONSTRAINT [DF_Concept_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Term_Key] ON [dbo].[Concept]([Term_Key]) ON [PRIMARY]


 CREATE  INDEX [IX_Meaning_Key] ON [dbo].[Concept]([Meaning_Key]) ON [PRIMARY]


 CREATE  INDEX [IX_Concept_Group_Key] ON [dbo].[Concept]([Concept_Group_Key]) ON [PRIMARY]


 CREATE  INDEX [IX_Term_Version_Key] ON [dbo].[Concept]([Term_Version_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_Designation] ADD 
	CONSTRAINT [DF_Concept_Status_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_Designation]([Concept_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_Group] ADD 
	CONSTRAINT [DF_Concept_Group_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Local_Domain_Key] ON [dbo].[Concept_Group]([Local_Domain_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_Group_Version] ADD 
	CONSTRAINT [DF_Concept_List_Version_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Concept_Group_Key] ON [dbo].[Concept_Group_Version]([Concept_Group_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_History] ADD 
	CONSTRAINT [DF_Concept_History_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_History]([Concept_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_Lineage] ADD 
	CONSTRAINT [PK_Concept_Lineage] PRIMARY KEY  NONCLUSTERED 
	(
		[Concept_Key],
		[Lineage_ID]
	)  ON [PRIMARY] 


 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_Lineage]([Concept_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Concept_Rank] ADD 
	CONSTRAINT [DF_Concept_Rank_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


ALTER TABLE [dbo].[Concept_Relation] ADD 
	CONSTRAINT [DF_Concept_Relation_Inherited] DEFAULT (0) FOR [Inherited],
	CONSTRAINT [DF_Concept_Relation_Comment] DEFAULT ('0') FOR [Comment]


 CREATE  INDEX [IX_From_Concept_Key] ON [dbo].[Concept_Relation]([From_Concept_Key]) ON [PRIMARY]


 CREATE  INDEX [IX_To_Concept_Key] ON [dbo].[Concept_Relation]([To_Concept_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Domain] ADD 
	CONSTRAINT [DF_Domain_Has_Occurrences] DEFAULT (0) FOR [Has_Occurrences]


ALTER TABLE [dbo].[Domain_Hyperlink] ADD 
	CONSTRAINT [DF_Domain_Hyperlink_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


ALTER TABLE [dbo].[Local_Domain] ADD 
	CONSTRAINT [DF_Local_Domain_Concept_Group_Label] DEFAULT ('Concept Group') FOR [Concept_Group_Label],
	CONSTRAINT [DF_Local_Domain_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Local_Domain_Domain_Key] ON [dbo].[Local_Domain]([Domain_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Meaning_Relation] ADD 
	CONSTRAINT [DF_Meaning_Relation_Inherited] DEFAULT (0) FOR [Inherited],
	CONSTRAINT [DF_Meaning_Relation_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


ALTER TABLE [dbo].[Semantic_Relation] ADD 
	CONSTRAINT [DF_Semantic_Relation_Unidirectional] DEFAULT (0) FOR [Unidirectional],
	CONSTRAINT [CK_Semantic_Relation_Chronological_Overlap] CHECK ([Chronological_Overlap] = 9 or ([Chronological_Overlap] = 8 or ([Chronological_Overlap] = 7 or ([Chronological_Overlap] = 6 or ([Chronological_Overlap] = 5 or ([Chronological_Overlap] = 4 or ([Chronological_Overlap] = 3 or ([Chronological_Overlap] = 2 or [Chronological_Overlap] = 1))))))) or [Chronological_Overlap] = 0 or [Chronological_Overlap] is null)


ALTER TABLE [dbo].[Term] ADD 
	CONSTRAINT [DF_Term_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data],
	CONSTRAINT [IX_Term_Unique] UNIQUE  NONCLUSTERED 
	(
		[Language_Key],
		[Item_Name]
	)  ON [PRIMARY] 


 CREATE  INDEX [IX_PlainText] ON [dbo].[Term]([Plaintext]) ON [PRIMARY]


ALTER TABLE [dbo].[Term_Version] ADD 
	CONSTRAINT [DF_Author_And_Version_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]


 CREATE  INDEX [IX_Term_Version_Relation_From] ON [dbo].[Term_Version_Relation]([From_Term_Version_Key]) ON [PRIMARY]


 CREATE  INDEX [IX_Term_Version_Relation_To] ON [dbo].[Term_Version_Relation]([Term_Version_Relation_Key], [To_Term_Version_Key]) ON [PRIMARY]


ALTER TABLE [dbo].[Thesaurus_Fact] ADD 
	CONSTRAINT [DF_Thesaurus_Fact_Related_Term_Versions] DEFAULT (0) FOR [Related_Term_Versions],
	CONSTRAINT [DF_Thesaurus_Fact_Inherited] DEFAULT (0) FOR [Inherited]


ALTER TABLE [dbo].[Concept] ADD 
	CONSTRAINT [FK_Concept_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [dbo].[Concept_Group] (
		[Concept_Group_Key]
	),
	CONSTRAINT [FK_Concept_Concept_Name_Type] FOREIGN KEY 
	(
		[Name_Type_Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	),
	CONSTRAINT [FK_Concept_Concept_Rank] FOREIGN KEY 
	(
		[Concept_Rank_Key]
	) REFERENCES [dbo].[Concept_Rank] (
		[Concept_Rank_Key]
	),
	CONSTRAINT [FK_Concept_Meaning] FOREIGN KEY 
	(
		[Meaning_Key]
	) REFERENCES [dbo].[Meaning] (
		[Meaning_Key]
	),
	CONSTRAINT [FK_Concept_Term] FOREIGN KEY 
	(
		[Term_Key]
	) REFERENCES [dbo].[Term] (
		[Term_Key]
	),
	CONSTRAINT [FK_Concept_Term_Version] FOREIGN KEY 
	(
		[Term_Version_Key]
	) REFERENCES [dbo].[Term_Version] (
		[Term_Version_Key]
	)


ALTER TABLE [dbo].[Concept_Designation] ADD 
	CONSTRAINT [FK_Concept_Designation_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	),
	CONSTRAINT [FK_Concept_Designation_Type] FOREIGN KEY 
	(
		[Designation_Type_Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	)


ALTER TABLE [dbo].[Concept_Group] ADD 
	CONSTRAINT [FK_Concept_Group_Local_Domain] FOREIGN KEY 
	(
		[Local_Domain_Key]
	) REFERENCES [dbo].[Local_Domain] (
		[Local_Domain_Key]
	),
	CONSTRAINT [FK_Concept_Group_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Hierarchy_Relation_Type_Key]
	) REFERENCES [dbo].[Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)


ALTER TABLE [dbo].[Concept_Group_Version] ADD 
	CONSTRAINT [FK_Concept_Group_Version_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [dbo].[Concept_Group] (
		[Concept_Group_Key]
	)


ALTER TABLE [dbo].[Concept_History] ADD 
	CONSTRAINT [FK_Concept_History_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	),
	CONSTRAINT [FK_Concept_History_Concept_Group_Version_From] FOREIGN KEY 
	(
		[Concept_Group_Version_From]
	) REFERENCES [dbo].[Concept_Group_Version] (
		[Concept_Group_Version_Key]
	),
	CONSTRAINT [FK_Concept_History_Concept_Group_Version_To] FOREIGN KEY 
	(
		[Concept_Group_Version_To]
	) REFERENCES [dbo].[Concept_Group_Version] (
		[Concept_Group_Version_Key]
	)


ALTER TABLE [dbo].[Concept_Lineage] ADD 
	CONSTRAINT [FK_Concept_Lineage_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	)


ALTER TABLE [dbo].[Concept_Rank] ADD 
	CONSTRAINT [FK_Concept_Rank_Domain] FOREIGN KEY 
	(
		[Domain_Key]
	) REFERENCES [dbo].[Domain] (
		[Domain_Key]
	)


ALTER TABLE [dbo].[Concept_Relation] ADD 
	CONSTRAINT [FK_Concept_Relation_Concept] FOREIGN KEY 
	(
		[From_Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	),
	CONSTRAINT [FK_Concept_Relation_Concept1] FOREIGN KEY 
	(
		[To_Concept_Key]
	) REFERENCES [dbo].[Concept] (
		[Concept_Key]
	),
	CONSTRAINT [FK_Concept_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [dbo].[Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)


ALTER TABLE [dbo].[Domain] ADD 
	CONSTRAINT [FK_Domain_Subject_Area] FOREIGN KEY 
	(
		[Subject_Area_Key]
	) REFERENCES [dbo].[Subject_Area] (
		[Subject_Area_Key]
	),
	CONSTRAINT [FK_Domain_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Default_Hierarchy_Relation_Type_Key]
	) REFERENCES [dbo].[Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)


ALTER TABLE [dbo].[Domain_Hyperlink] ADD 
	CONSTRAINT [FK_Domain_Hyperlink_Local_Domain] FOREIGN KEY 
	(
		[Local_Domain_Key]
	) REFERENCES [dbo].[Local_Domain] (
		[Local_Domain_Key]
	)


ALTER TABLE [dbo].[Local_Domain] ADD 
	CONSTRAINT [FK_Local_Domain_Domain] FOREIGN KEY 
	(
		[Domain_Key]
	) REFERENCES [dbo].[Domain] (
		[Domain_Key]
	),
	CONSTRAINT [FK_Local_Domain_Language] FOREIGN KEY 
	(
		[Language_Key]
	) REFERENCES [dbo].[Language] (
		[Language_Key]
	)


ALTER TABLE [dbo].[Term_Version] ADD 
	CONSTRAINT [FK_Term_Version_Term] FOREIGN KEY 
	(
		[Term_Key]
	) REFERENCES [dbo].[Term] (
		[Term_Key]
	)


ALTER TABLE [dbo].[Term_Version_Relation] ADD 
	CONSTRAINT [FK_Term_Version_Relation_Term_Version_From] FOREIGN KEY 
	(
		[From_Term_Version_Key]
	) REFERENCES [dbo].[Term_Version] (
		[Term_Version_Key]
	),
	CONSTRAINT [FK_Term_Version_Relation_Term_Version_To] FOREIGN KEY 
	(
		[To_Term_Version_Key]
	) REFERENCES [dbo].[Term_Version] (
		[Term_Version_Key]
	),
	CONSTRAINT [FK_Term_Version_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [dbo].[Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)


ALTER TABLE [dbo].[Thesaurus_Relation_Type] ADD 
	CONSTRAINT [FK_Thesaurus_Relation_Type_Semantic_Relation] FOREIGN KEY 
	(
		[Semantic_Relation_Key]
	) REFERENCES [dbo].[Semantic_Relation] (
		[Semantic_Relation_Key]
	)


ALTER TABLE [dbo].[Thesaurus_Relation_Type_Usage] ADD 
	CONSTRAINT [FK_Thesaurus_Relation_Type_Usage_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [dbo].[Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)


SET QUOTED_IDENTIFIER ON 

SET ANSI_NULLS ON 

END

--Add referential integrity between meaning and meaning_relation
--which was missing in the original Thesaurus
DELETE Concept
FROM Concept
LEFT JOIN Meaning m ON m.Meaning_Key=Concept.Meaning_Key
WHERE m.Meaning_Key IS NULL

ALTER TABLE [dbo].[Meaning_Relation] ADD 
	CONSTRAINT [FK_Meaning_Relation_Meaning_From] FOREIGN KEY 
	(
		[From_Meaning_Key]
	) REFERENCES [dbo].[Meaning] (
		[Meaning_Key]
	),
	CONSTRAINT [FK_Meaning_Relation_Meaning_To] FOREIGN KEY 
	(
		[To_Meaning_Key]
	) REFERENCES [dbo].[Meaning] (
		[Meaning_Key]
	)