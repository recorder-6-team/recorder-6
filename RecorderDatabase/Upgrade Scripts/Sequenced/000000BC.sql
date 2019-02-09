/****** makes changes and additions for new Private tab for taxon Occurrence - new table  *****/
      
/****** Object:  Table [dbo].[Taxon_Private_Type]    Script Date: 15/03/2018 20:02:56 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Taxon_Private_Type](
	[Taxon_Private_Type_Key] [char](16) NOT NULL,
	[Short_Name] [varchar](30) NOT NULL,
	[Long_Name] [varchar](100) NULL,
	[Entered_By] [char](16) NOT NULL,
	[Entry_Date] [smalldatetime] NOT NULL,
	[Changed_By] [char](16) NULL,
	[Changed_Date] [smalldatetime] NULL,
	[Custodian] [char](8)  NULL,
	[System_Supplied_Data] [bit] NULL,
	[Description] [text] NULL,
 CONSTRAINT [PK_Taxon_Private_Type] PRIMARY KEY CLUSTERED 
(
	[Taxon_Private_Type_Key] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[Taxon_Private_Type] 
ADD  CONSTRAINT [DF_Taxon_Private_Type_Entry_Date]  DEFAULT (getdate()) FOR [Entry_Date]

GO

CREATE TRIGGER [dbo].[TAXON_PRIVATE_TYPECustodianInsert] ON [dbo].[TAXON_PRIVATE_TYPE]
AFTER INSERT AS UPDATE TAXON_PRIVATE_TYPE 
SET TAXON_PRIVATE_TYPE.CUSTODIAN = SUBSTRING(TAXON_PRIVATE_TYPE.TAXON_PRIVATE_TYPE_KEY, 1, 8)
FROM TAXON_PRIVATE_TYPE
INNER JOIN INSERTED ON TAXON_PRIVATE_TYPE.TAXON_PRIVATE_TYPE_KEY = INSERTED.TAXON_PRIVATE_TYPE_KEY
WHERE TAXON_PRIVATE_TYPE.CUSTODIAN IS NULL

GO


/****** Object:  Table [dbo].[Taxon_Private_Data]    Script Date: 15/03/2018 20:02:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[Taxon_Private_Data](
	[Taxon_Private_Data_Key] [char](16) NOT NULL,
	[Taxon_Occurrence_Key] [char](16) NOT NULL,
	[Taxon_Private_Type_Key] [char](16) NOT NULL,
	[Item_Name] [varchar](30)  NULL,
	[Detail] [varchar](100) NULL,
	[Item_Date] [smalldatetime] NULL,
        [Item_Value] [varchar](16)  NULL,
        [Comment] [text] NULL,
	[Entered_by] [char](16) NOT NULL,
	[Entry_Date] [smalldatetime] NOT NULL,
	[Changed_By] [char](16) NULL,
	[Changed_Date] [smalldatetime] NULL,
	[Custodian] [char](8) NULL,
 CONSTRAINT [PK_Taxon_Private_Data] PRIMARY KEY CLUSTERED 
(
	[Taxon_Private_Data_Key] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[Taxon_Private_Data] ADD  CONSTRAINT [DF_Taxon_Private_Data_Entry_Date]  DEFAULT (getdate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[Taxon_Private_Data] ADD  CONSTRAINT [DF_Taxon_Private_Data_Item_Name]  DEFAULT ('None') FOR [Item_Name]

GO

CREATE TRIGGER [dbo].[TAXON_PRIVATE_DATACustodianInsert] ON [dbo].[TAXON_PRIVATE_DATA] AFTER INSERT AS UPDATE TAXON_PRIVATE_DATA SET TAXON_PRIVATE_DATA.CUSTODIAN = SUBSTRING(TAXON_PRIVATE_DATA.TAXON_PRIVATE_DATA_KEY, 1, 8) FROM TAXON_PRIVATE_DATA INNER JOIN INSERTED ON TAXON_PRIVATE_DATA.TAXON_PRIVATE_DATA_KEY = INSERTED.TAXON_PRIVATE_DATA_KEY WHERE TAXON_PRIVATE_DATA.CUSTODIAN IS NULL

GO

ALTER TABLE [dbo].[TAXON_PRIVATE_DATA]  WITH NOCHECK ADD  CONSTRAINT [FK_TAXON_PRIVATE_DATA_TYPE] FOREIGN KEY([TAXON_PRIVATE_TYPE_KEY])
REFERENCES [dbo].[TAXON_PRIVATE_TYPE] ([TAXON_PRIVATE_TYPE_KEY])

GO

ALTER TABLE [dbo].[TAXON_PRIVATE_DATA] CHECK CONSTRAINT [FK_TAXON_PRIVATE_DATA_TYPE]
GO

ALTER TABLE [dbo].[TAXON_PRIVATE_DATA]  WITH NOCHECK ADD  CONSTRAINT [FK_TAXON_PRIVATE_DATA_OCCURRENCE] FOREIGN KEY([TAXON_OCCURRENCE_KEY])
REFERENCES [dbo].[TAXON_OCCURRENCE] ([TAXON_OCCURRENCE_KEY])
GO

ALTER TABLE [dbo].[TAXON_PRIVATE_DATA] CHECK CONSTRAINT [FK_TAXON_PRIVATE_DATA_OCCURRENCE]

GO

/****** permissions **************/


GRANT SELECT ON TAXON_PRIVATE_TYPE TO PUBLIC

GRANT DELETE ON TAXON_PRIVATE_TYPE TO PUBLIC

GRANT UPDATE ON TAXON_PRIVATE_TYPE TO PUBLIC

GRANT INSERT ON TAXON_PRIVATE_TYPE TO PUBLIC

GO

GRANT SELECT ON TAXON_PRIVATE_DATA TO PUBLIC

GRANT DELETE ON TAXON_PRIVATE_DATA TO PUBLIC

GRANT UPDATE ON TAXON_PRIVATE_DATA TO PUBLIC

GRANT INSERT ON TAXON_PRIVATE_DATA TO PUBLIC

GO

Update DataBase_Relationship SET
RELATIONSHIP_NAME = 'TAXONOCCURRENCETAXONPRIVATEDATA',
DETAIL_TABLE = 'TAXON_PRIVATE_DATA'
WHERE Relationship_Key = 'LCA00023000000R8'

GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Taxon_Private_Item_Name]    Script Date: 12/29/2018 15:12:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Make the field in Taxon_Private_Data 'None' if null
  Parameters:   None

  Created:      Dec 2018 

  Last revision information:
  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Taxon_Private_Item_Name]
AS
    UPDATE     #Taxon_Private_Data 
    SET         Item_Name = 'None' 
                WHERE ISNULL(Item_Name,'') = ''   
                
GO

GRANT EXECUTE ON [dbo].[usp_IW_Taxon_Private_Item_Name] TO PUBLIC

