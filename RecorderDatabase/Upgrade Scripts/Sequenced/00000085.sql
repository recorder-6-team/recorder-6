/*===========================================================================*\
  Description:  Creates the new table required for Organism 
  Created:       2012
  Last revision information:
  $Date: 10/10/2012 $
  $Author: Mike Weideli$

\*=========================================================================== */

IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='ORGANISM')
	DROP TABLE ORGANISM

GO

CREATE TABLE ORGANISM (
	
	ORGANISM_KEY char(16) CONSTRAINT PK_ORGANISM_KEY PRIMARY KEY NOT NULL,
	[PARENT_KEY] [char](16) NULL,
	[TAXON_VERSION_KEY] [char](16) NOT NULL,
	[UK_STATUS_KEY] [char](16) NULL,
	[MARINE_FLAG] [char] (1) NULL,
    [TERRESTRIAL_FRESHWATER_FLAG] [char] (1) NULL,
    [REDUNDANT_FLAG] [char] (1) NULL,
	[NON_NATIVE_FLAG] [char] (1) NULL,
	[ONLY_IN_NOT_FIT_FOR_WEB] [CHAR] (1) NULL,
    [ORPHAN] [char] (1) NULL,
	[VERNACULAR] [char] (1) NULL,
	[WEIGHT] [int] NULL,
	[LINEAGE] [varchar](30) NULL,
	[SORT_LEVEL] [int] NULL,
	[SORT_ORDER] [char](30) NULL,
	[ORGANISM_RANK_KEY] [char](16) NOT NULL,
	[ENTERED_BY] [char](16) NOT NULL,
	[ENTRY_DATE] [datetime] NOT NULL,
	[CHANGED_BY] [char](16) NULL,
	[CHANGED_DATE] [datetime] NULL,
	[DELETED_DATE] [datetime] NULL,
	[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL)
GO

ALTER TABLE dbo.ORGANISM
    ADD CONSTRAINT FK_TAXON_VERSION 
    FOREIGN KEY (TAXON_VERSION_KEY)
    REFERENCES TAXON_VERSION(TAXON_VERSION_KEY)
GO  

ALTER TABLE dbo.ORGANISM
    ADD CONSTRAINT FK_TAXON_RANK
    FOREIGN KEY (ORGANISM_RANK_KEY)
    REFERENCES TAXON_RANK(TAXON_RANK_KEY)
GO    

 ALTER TABLE dbo.ORGANISM
    ADD CONSTRAINT IX_ORGANISM_KEY
    FOREIGN KEY (PARENT_KEY)
    REFERENCES ORGANISM(ORGANISM_KEY)
GO

 CREATE NONCLUSTERED INDEX [IDX_TAXON_VERSION] ON [dbo].[ORGANISM] ([TAXON_VERSION_KEY])

GO
 
CREATE NONCLUSTERED INDEX [IDX_LINEAGE] ON [dbo].[ORGANISM] ([LINEAGE])

GO


GRANT SELECT ON ORGANISM TO PUBLIC
GRANT DELETE ON ORGANISM TO PUBLIC
GRANT UPDATE ON ORGANISM TO PUBLIC
GRANT INSERT ON ORGANISM TO PUBLIC

GO

/*===========================================================================*\
  Description:  Creates the new table required for Index_Taxon_Hierarchy
  Created:       Nov 2012
  Last revision information:
  $Date: 10/10/2012 $
  $Author: Mike Weideli$

\*=========================================================================== */


IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='Index_Taxon_Hierarchy')
	DROP TABLE Index_Taxon_Hierarchy
GO


       CREATE TABLE [dbo].[Index_Taxon_Hierarchy](
	[Recommended_Taxon_Version_Key] [char](16) NOT NULL,
	[Hierarchy_Taxon_Version_Key] [char](16) NOT NULL,
	[Hierarchy_Type] [char](1) NOT NULL
) ON [PRIMARY]



GO
GRANT SELECT ON Index_Taxon_Hierarchy TO PUBLIC
GRANT DELETE ON Index_Taxon_Hierarchy TO PUBLIC
GRANT UPDATE ON Index_Taxon_Hierarchy TO PUBLIC
GRANT INSERT ON Index_Taxon_Hierarchy TO PUBLIC
