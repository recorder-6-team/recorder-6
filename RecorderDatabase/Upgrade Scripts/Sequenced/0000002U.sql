IF NOT EXISTS(SELECT * FROM SysObjects WHERE Name='Session' AND Type='U')
BEGIN
	
	/****** Object:  Table [dbo].[Session]    Script Date: 16/12/2005 08:29:55 ******/
	CREATE TABLE [dbo].[Session] (
		[Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[User_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[Date_Time_Start] [datetime] NOT NULL ,
		[Date_Time_End] [datetime] NULL 
	) ON [PRIMARY]
	
	
	ALTER TABLE [dbo].[Session] WITH NOCHECK ADD 
		CONSTRAINT [PK_Session] PRIMARY KEY  CLUSTERED 
		(
			[Session_ID]
		) WITH  FILLFACTOR = 90  ON [PRIMARY] 
	
	
	ALTER TABLE [dbo].[Session] ADD 
		CONSTRAINT [DF_Session_Date_Time_Start] DEFAULT (getdate()) FOR [Date_Time_Start]

END

