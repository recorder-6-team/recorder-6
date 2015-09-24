IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[Reference_Keyword]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
	CREATE TABLE Reference_Keyword (
		Reference_Keyword_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Source_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Entered_Session_ID CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Changed_Session_ID CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Custodian CHAR(8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		System_Supplied_Data BIT NOT NULL DEFAULT 0
		CONSTRAINT [PK_Reference_Keyword] PRIMARY KEY  CLUSTERED 
		(
			[Reference_Keyword_Key]
		)  ON [PRIMARY] ,
		CONSTRAINT [FK_Reference_Keyword_Reference] FOREIGN KEY 
		(
			[Source_Key]
		) REFERENCES [Reference] (
			[Source_Key]
		) ,
		CONSTRAINT [FK_Reference_Keyword_Concept] FOREIGN KEY 
		(
			[Concept_Key]
		) REFERENCES [Concept] (
			[Concept_Key]
		) ON DELETE CASCADE
	)
END

IF EXISTS(SELECT 1 FROM SysObjects WHERE [Name]='REFERENCE_KEYWORDCustodianInsert')
  DROP TRIGGER REFERENCE_KEYWORDCustodianInsert
GO

CREATE TRIGGER REFERENCE_KEYWORDCustodianInsert 
ON dbo.REFERENCE_KEYWORD 
AFTER INSERT AS 
	UPDATE REFERENCE_KEYWORD 
	SET REFERENCE_KEYWORD.CUSTODIAN = SUBSTRING(REFERENCE_KEYWORD.REFERENCE_KEYWORD_KEY, 1, 8) 
	FROM REFERENCE_KEYWORD 
	INNER JOIN INSERTED ON REFERENCE_KEYWORD.REFERENCE_KEYWORD_KEY = INSERTED.REFERENCE_KEYWORD_KEY 
	WHERE REFERENCE_KEYWORD.CUSTODIAN IS NULL


IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[Session]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN

	/****** Object:  Table [dbo].[Session]    Script Date: 12/12/2005 16:38:38 ******/
	CREATE TABLE [dbo].[Session] (
		[Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[User_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Date_Time_Start] [datetime] NOT NULL ,
		[Date_Time_End] [datetime] NULL 
	) 
	
	ALTER TABLE [dbo].[Session] WITH NOCHECK ADD 
		CONSTRAINT [PK_Session] PRIMARY KEY  CLUSTERED 
		(
			[Session_ID]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[Session] ADD 
		CONSTRAINT [DF_Session_Date_Time_Start] DEFAULT (getdate()) FOR [Date_Time_Start]

END
GO

GRANT  SELECT  ON [dbo].[Reference_Keyword]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Reference_Keyword]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Reference_Keyword]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Reference_Keyword]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Reference_Keyword]  TO [R2k_Administrator]
GO

