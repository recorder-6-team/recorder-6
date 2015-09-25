if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[LOCATION_NAMECustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[LOCATION_NAMECustodianInsert]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[LOCATION_NAME]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[LOCATION_NAME]
GO

CREATE TABLE [dbo].[LOCATION_NAME] (
	[LOCATION_NAME_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[ITEM_NAME] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[PREFERRED] [bit] NOT NULL ,
	[LOCATION_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[ENTERED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[ENTRY_DATE] [smalldatetime] NOT NULL ,
	[CHANGED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[CHANGED_DATE] [smalldatetime] NULL ,
	[CUSTODIAN] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

 CREATE  CLUSTERED  INDEX [IX_LOCATION_KEY] ON [dbo].[LOCATION_NAME]([LOCATION_KEY]) ON [PRIMARY]
GO

ALTER TABLE [dbo].[LOCATION_NAME] ADD 
	CONSTRAINT [DF__Temporary__PREFE__1DB06A4F] DEFAULT (0) FOR [PREFERRED],
	CONSTRAINT [DF__Temporary__ENTRY__1EA48E88] DEFAULT (getdate()) FOR [ENTRY_DATE],
	CONSTRAINT [PK_LOCATION_NAME] PRIMARY KEY  NONCLUSTERED 
	(
		[LOCATION_NAME_KEY]
	)  ON [PRIMARY] 
GO

GRANT  SELECT  ON [dbo].[LOCATION_NAME]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[LOCATION_NAME]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[LOCATION_NAME]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[LOCATION_NAME]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[LOCATION_NAME]  TO [R2k_Administrator]
GO

ALTER TABLE [dbo].[LOCATION_NAME] ADD 
	CONSTRAINT [FK_LOCATION_NAME_LOCATION] FOREIGN KEY 
	(
		[LOCATION_KEY]
	) REFERENCES [dbo].[LOCATION] (
		[LOCATION_KEY]
	)
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

CREATE TRIGGER LOCATION_NAMECustodianInsert ON dbo.LOCATION_NAME AFTER INSERT AS UPDATE LOCATION_NAME SET LOCATION_NAME.CUSTODIAN = SUBSTRING(LOCATION_NAME.LOCATION_NAME_KEY, 1, 8) FROM LOCATION_NAME INNER JOIN INSERTED ON LOCATION_NAME.LOCATION_NAME_KEY = INSERTED.LOCATION_NAME_KEY WHERE LOCATION_NAME.CUSTODIAN IS NULL

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

