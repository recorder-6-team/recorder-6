/*===========================================================================*\
  Description:	Upgrade script for multiple maps support.

  Created:	January 2004

  Last revision information:
    $Revision: 4 $
    $Date: 22/06/04 10:28 $
    $Author: Johnvanbreda $
\*===========================================================================*/

/*======================================*\
  Drop constraints and tables if needed.
\*======================================*/
IF EXISTS (SELECT * FROM dbo.SysObjects WHERE ID = Object_ID(N'[dbo].[FK_Computer_Map_Base_Map]') AND ObjectProperty(ID, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Computer_Map] DROP CONSTRAINT FK_Computer_Map_Base_Map
GO

IF EXISTS (SELECT * FROM dbo.SysObjects WHERE ID = Object_ID(N'[dbo].[FK_MAP_SHEET_Base_Map]') AND ObjectProperty(ID, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[MAP_SHEET] DROP CONSTRAINT FK_MAP_SHEET_Base_Map
GO

IF EXISTS (SELECT * FROM dbo.SysObjects WHERE ID = Object_ID(N'[dbo].[Base_Map]') AND ObjectProperty(ID, N'IsUserTable') = 1)
DROP TABLE [dbo].[Base_Map]
GO

IF EXISTS (SELECT * FROM dbo.SysObjects WHERE ID = Object_ID(N'[dbo].[Computer_Map]') AND ObjectProperty(ID, N'IsUserTable') = 1)
DROP TABLE [dbo].[Computer_Map]
GO

/*=====================*\
  New table: Baser_Map.
\*=====================*/
CREATE TABLE [dbo].[Base_Map] (
	[Base_Map_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Original_Filename] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Spatial_System] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Display_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reset_Index] [int] NOT NULL ,
	[Original_FileName_Before_Reset] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Spatial_System_Before_Reset] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [smalldatetime] NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [smalldatetime] NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Base_Map] WITH NOCHECK ADD 
	CONSTRAINT [PK_Base_Map] PRIMARY KEY  CLUSTERED (
		[Base_Map_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[Base_Map] WITH NOCHECK ADD 
	CONSTRAINT [DF_Base_Map_Reset_Index] DEFAULT (0) FOR [Reset_Index],
	CONSTRAINT [DF_Base_Map_Entry_Date] DEFAULT (GetDate()) FOR [Entry_Date]
GO

/*=======================*\
  New table Computer_Map.
\*=======================*/
CREATE TABLE [dbo].[Computer_Map] (
	[Computer_Map_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Computer_ID] [varchar] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Base_Map_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reset_Index] [int] NOT NULL ,
	[Default_Map] [bit] NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] [smalldatetime] NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Changed_Date] [smalldatetime] NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Computer_Map] WITH NOCHECK ADD 
	CONSTRAINT [PK_User_Map] PRIMARY KEY  CLUSTERED (
		[Computer_Map_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[Computer_Map] WITH NOCHECK ADD 
	CONSTRAINT [DF_Computer_Map_Reset_Index] DEFAULT (0) FOR [Reset_Index],
	CONSTRAINT [DF_Computer_Map_Default] DEFAULT (0) FOR [Default_Map],
	CONSTRAINT [DF_Computer_Map_Entry_Date] DEFAULT (GetDate()) FOR [Entry_Date]
GO

ALTER TABLE [dbo].[Computer_Map] ADD 
	CONSTRAINT [FK_Computer_Map_Base_Map] FOREIGN KEY (
		[Base_Map_Key]
	) REFERENCES [dbo].[Base_Map] (
		[Base_Map_Key]
	) NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[Computer_Map] NOCHECK CONSTRAINT [FK_Computer_Map_Base_Map]
GO

/*=================================================*\
  Map_Sheet table needs a new field, and contraint.
\*=================================================*/
IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE [Name] = 'Base_Map_Key' AND [ID]=object_id('Map_Sheet'))
	ALTER TABLE Map_Sheet
		ADD [Base_Map_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
GO

IF NOT EXISTS(SELECT 1 FROM SysObjects WHERE [Name]='FK_MAP_SHEET_Base_Map') 
	ALTER TABLE [dbo].[MAP_SHEET] ADD 
		CONSTRAINT [FK_MAP_SHEET_Base_Map] FOREIGN KEY (
			[Base_Map_Key]
		) REFERENCES [dbo].[Base_Map] (
			[Base_Map_Key]
		)
GO

/*==================================*\
  Grant access rights to new tables.
\*==================================*/
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Base_Map]  TO [R2k_ReadOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Base_Map]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Base_Map]  TO [R2k_AddOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Base_Map]  TO [R2k_FullEdit]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Base_Map]  TO [R2k_Administrator]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Computer_Map]  TO [R2k_ReadOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Computer_Map]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Computer_Map]  TO [R2k_AddOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Computer_Map]  TO [R2k_FullEdit]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Computer_Map]  TO [R2k_Administrator]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ComputerMap_DeleteInvalid]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
GO

/*===========================================================================*\
  Description:	Delete invalid records from Copmuter_Map, in case something
		wasn't cleaned up properly.

  Parameters:	<none>

  Created:	February 2004

  Last revision information:
    $Revision: 4 $
    $Date: 22/06/04 10:28 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
AS
	DELETE Computer_Map
	WHERE Base_Map_Key NOT IN (SELECT Base_Map_Key FROM Base_Map)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ComputerMap_DeleteInvalid') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ComputerMap_DeleteInvalid'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [Dev - JNCC SQL]
END
GO