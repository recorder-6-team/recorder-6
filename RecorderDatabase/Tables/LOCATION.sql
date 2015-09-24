if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_GRID_SQUARE_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[GRID_SQUARE] DROP CONSTRAINT FK_GRID_SQUARE_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LAND_PARCEL_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LAND_PARCEL] DROP CONSTRAINT FK_LAND_PARCEL_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION] DROP CONSTRAINT FK_LOCATION_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_ADMIN_AREAS_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_ADMIN_AREAS] DROP CONSTRAINT FK_LOCATION_ADMIN_AREAS_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_BOUNDARY_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_BOUNDARY] DROP CONSTRAINT FK_LOCATION_BOUNDARY_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_DATA_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_DATA] DROP CONSTRAINT FK_LOCATION_DATA_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_DESIGNATION_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_DESIGNATION] DROP CONSTRAINT FK_LOCATION_DESIGNATION_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_FEATURE_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_FEATURE] DROP CONSTRAINT FK_LOCATION_FEATURE_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_NAME_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_NAME] DROP CONSTRAINT FK_LOCATION_NAME_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_RELATION_LOCATION1]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_RELATION] DROP CONSTRAINT FK_LOCATION_RELATION_LOCATION1
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_RELATION_LOCATION2]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_RELATION] DROP CONSTRAINT FK_LOCATION_RELATION_LOCATION2
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_SOURCES_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_SOURCES] DROP CONSTRAINT FK_LOCATION_SOURCES_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_LOCATION_USE_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[LOCATION_USE] DROP CONSTRAINT FK_LOCATION_USE_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_SAMPLE_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[SAMPLE] DROP CONSTRAINT FK_SAMPLE_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_SURVEY_EVENT_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[SURVEY_EVENT] DROP CONSTRAINT FK_SURVEY_EVENT_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_TENURE_LOCATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[TENURE] DROP CONSTRAINT FK_TENURE_LOCATION
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[LOCATIONCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[LOCATIONCustodianInsert]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[LOCATION]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[LOCATION]
GO

CREATE TABLE [dbo].[LOCATION] (
	[LOCATION_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[DESCRIPTION] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[PARENT_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[SPATIAL_REF] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[SPATIAL_REF_SYSTEM] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[LAT] [float] NOT NULL ,
	[LONG] [float] NOT NULL ,
	[LOCATION_TYPE_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[FILE_CODE] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[SPATIAL_REF_QUALIFIER] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[APPROACH] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[RESTRICTION] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[ENTERED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[ENTRY_DATE] [smalldatetime] NOT NULL ,
	[CHANGED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[CHANGED_DATE] [smalldatetime] NULL ,
	[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL ,
	[CUSTODIAN] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[LOCATION] ADD 
	CONSTRAINT [DF__Temporary__ENTRY__787EE5A0] DEFAULT (getdate()) FOR [ENTRY_DATE],
	CONSTRAINT [DF__Temporary__SYSTE__797309D9] DEFAULT (0) FOR [SYSTEM_SUPPLIED_DATA],
	CONSTRAINT [PK_LOCATION] PRIMARY KEY  NONCLUSTERED 
	(
		[LOCATION_KEY]
	)  ON [PRIMARY] 
GO

 CREATE  INDEX [IX_PARENT_KEY] ON [dbo].[LOCATION]([PARENT_KEY]) ON [PRIMARY]
GO

GRANT  SELECT  ON [dbo].[LOCATION]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[LOCATION]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[LOCATION]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[LOCATION]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[LOCATION]  TO [R2k_Administrator]
GO

ALTER TABLE [dbo].[LOCATION] ADD 
	CONSTRAINT [FK_LOCATION_LOCATION] FOREIGN KEY 
	(
		[PARENT_KEY]
	) REFERENCES [dbo].[LOCATION] (
		[LOCATION_KEY]
	),
	CONSTRAINT [FK_LOCATION_LOCATION_TYPE] FOREIGN KEY 
	(
		[LOCATION_TYPE_KEY]
	) REFERENCES [dbo].[LOCATION_TYPE] (
		[LOCATION_TYPE_KEY]
	)
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

CREATE TRIGGER LOCATIONCustodianInsert ON dbo.LOCATION AFTER INSERT AS UPDATE LOCATION SET LOCATION.CUSTODIAN = SUBSTRING(LOCATION.LOCATION_KEY, 1, 8) FROM LOCATION INNER JOIN INSERTED ON LOCATION.LOCATION_KEY = INSERTED.LOCATION_KEY WHERE LOCATION.CUSTODIAN IS NULL

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

