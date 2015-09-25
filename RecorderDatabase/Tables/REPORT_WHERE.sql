if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_REPORT_ATTRIBUTE_REPORT_WHERE]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[REPORT_ATTRIBUTE] DROP CONSTRAINT FK_REPORT_ATTRIBUTE_REPORT_WHERE
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[REPORT_WHERE]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[REPORT_WHERE]
GO

CREATE TABLE [dbo].[REPORT_WHERE] (
	[REPORT_WHERE_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[WHERE_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[ENTERED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[ENTRY_DATE] [smalldatetime] NULL ,
	[CHANGED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[CHANGED_DATE] [smalldatetime] NULL ,
	[SYSTEM_SUPPLIED_DATA] [bit] NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[REPORT_WHERE] WITH NOCHECK ADD 
	CONSTRAINT [PK_WHERE] PRIMARY KEY  CLUSTERED 
	(
		[REPORT_WHERE_KEY]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[REPORT_WHERE] ADD 
	CONSTRAINT [DF_REPORT_WHERE_ENTRY_DATE] DEFAULT (getdate()) FOR [ENTRY_DATE],
	CONSTRAINT [DF_REPORT_WHERE_SYSTEM_SUPPLIED_DATA] DEFAULT (0) FOR [SYSTEM_SUPPLIED_DATA]
GO

GRANT  SELECT  ON [dbo].[REPORT_WHERE]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[REPORT_WHERE]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[REPORT_WHERE]  TO [R2k_AddOnly]
GO

GRANT  SELECT  ON [dbo].[REPORT_WHERE]  TO [R2k_FullEdit]
GO

GRANT  SELECT  ON [dbo].[REPORT_WHERE]  TO [R2k_Administrator]
GO

