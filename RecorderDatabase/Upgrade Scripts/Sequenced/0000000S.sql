/*===========================================================================*\
  Script for Survey event owner tab
\*===========================================================================*/
/*===========================================================================*\
  Create Survey_Event_Owner table
\*===========================================================================*/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SURVEY_EVENT_OWNER]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
	CREATE TABLE [dbo].[SURVEY_EVENT_OWNER] (
		[SURVEY_EVENT_OWNER_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[SURVEY_EVENT_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[NAME_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[SURVEY_EVENT_OWNER_TYPE_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[ENTERED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[ENTRY_DATE] [smalldatetime] NOT NULL ,
		[CHANGED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[CHANGED_DATE] [smalldatetime] NULL ,
		[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL ,
		[CUSTODIAN] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	) ON [PRIMARY]
	
	ALTER TABLE [dbo].[SURVEY_EVENT_OWNER] WITH NOCHECK ADD 
		CONSTRAINT [PK_SURVEYE_EVENT_OWNERSHIP] PRIMARY KEY  CLUSTERED 
		(
			[SURVEY_EVENT_OWNER_KEY]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[SURVEY_EVENT_OWNER] ADD 
		CONSTRAINT [DF_SURVEYE_EVENT_OWNERSHIP_ENTRY_DATE] DEFAULT (getdate()) FOR [ENTRY_DATE],
		CONSTRAINT [DF_SURVEYE_EVENT_OWNERSHIP_SYSTEM_SUPPLIED_DATA] DEFAULT (1) FOR [SYSTEM_SUPPLIED_DATA]
	
	 CREATE  INDEX [IX_SURVEY_EVENT_OWNERSHIP] ON [dbo].[SURVEY_EVENT_OWNER]([SURVEY_EVENT_KEY]) ON [PRIMARY]
END
GO

/*===========================================================================*\
  Create Survey_Event_Owner_Type table
\*===========================================================================*/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[SURVEY_EVENT_OWNER_TYPE]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
BEGIN
	CREATE TABLE [dbo].[SURVEY_EVENT_OWNER_TYPE] (
		[SURVEY_EVENT_OWNER_TYPE_KEY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[SHORT_NAME] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[LONG_NAME] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[DESCRIPTION] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[ENTERED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
		[ENTRY_DATE] [smalldatetime] NOT NULL ,
		[CHANGED_BY] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
		[CHANGED_DATE] [smalldatetime] NULL ,
		[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL ,
		[CUSTODIAN] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
	
	ALTER TABLE [dbo].[SURVEY_EVENT_OWNER_TYPE] WITH NOCHECK ADD 
		CONSTRAINT [PK_SURVEY_EVENT_OWNERSHIP_TYPE] PRIMARY KEY  CLUSTERED 
		(
			[SURVEY_EVENT_OWNER_TYPE_KEY]
		)  ON [PRIMARY] 
	
	ALTER TABLE [dbo].[SURVEY_EVENT_OWNER_TYPE] ADD 
		CONSTRAINT [DF_SURVEY_EVENT_OWNERSHIP_TYPE_ENTRY_DATE] DEFAULT (getdate()) FOR [ENTRY_DATE],
		CONSTRAINT [DF_SURVEY_EVENT_OWNERSHIP_TYPE_SYSTEM_SUPPLIED_DATA] DEFAULT (1) FOR [SYSTEM_SUPPLIED_DATA]

	/*===========================================================================*\
	  New table relationships
	\*===========================================================================*/
	ALTER TABLE [dbo].[SURVEY_EVENT_OWNER] ADD 
		CONSTRAINT [FK_SURVEY_EVENT_OWNER_SURVEY_EVENT] FOREIGN KEY 
		(
			[SURVEY_EVENT_KEY]
		) REFERENCES [dbo].[SURVEY_EVENT] (
			[SURVEY_EVENT_KEY]
		),
		CONSTRAINT [FK_SURVEY_EVENT_OWNERSHIP_SURVEY_EVENT_OWNERSHIP_TYPE] FOREIGN KEY 
		(
			[SURVEY_EVENT_OWNER_TYPE_KEY]
		) REFERENCES [dbo].[SURVEY_EVENT_OWNER_TYPE] (
			[SURVEY_EVENT_OWNER_TYPE_KEY]
		)

END
GO

/*===========================================================================*\
  New table security
\*===========================================================================*/
GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[SURVEY_EVENT_OWNER_TYPE]  TO [NBNUser]
GO

/*===========================================================================*\
  New data required
\*===========================================================================*/

--Provide system supplied data
IF NOT EXISTS(SELECT * FROM SURVEY_EVENT_OWNER_TYPE WHERE SURVEY_EVENT_OWNER_TYPE_KEY='NBNSYS0000000000')
	INSERT INTO SURVEY_EVENT_OWNER_TYPE (
		SURVEY_EVENT_OWNER_TYPE_KEY,
		SHORT_NAME,
		LONG_NAME,
		ENTERED_BY,
		ENTRY_DATE,
		SYSTEM_SUPPLIED_DATA,
		CUSTODIAN
	)
	VALUES (
		'NBNSYS0000000000',
		'Owner',
		'Owner of survey event data',
		'TESTDATA00000001',
		GetDate(),
		1,
		'NBNSYS00'
	)

IF NOT EXISTS(SELECT * FROM SURVEY_EVENT_OWNER_TYPE WHERE SURVEY_EVENT_OWNER_TYPE_KEY='NBNSYS0000000001')
	INSERT INTO SURVEY_EVENT_OWNER_TYPE (
		SURVEY_EVENT_OWNER_TYPE_KEY,
		SHORT_NAME,
		LONG_NAME,
		ENTERED_BY,
		ENTRY_DATE,
		SYSTEM_SUPPLIED_DATA,
		CUSTODIAN
	)
	VALUES (
		'NBNSYS0000000001',
		'Funder',
		'Funder of survey event',
		'TESTDATA00000001',
		GetDate(),
		1,
		'NBNSYS00'
	)

IF NOT EXISTS(SELECT * FROM SURVEY_EVENT_OWNER_TYPE WHERE SURVEY_EVENT_OWNER_TYPE_KEY='NBNSYS0000000002')
	INSERT INTO SURVEY_EVENT_OWNER_TYPE (
		SURVEY_EVENT_OWNER_TYPE_KEY,
		SHORT_NAME,
		LONG_NAME,
		ENTERED_BY,
		ENTRY_DATE,
		SYSTEM_SUPPLIED_DATA,
		CUSTODIAN
	)
	VALUES (
		'NBNSYS0000000002',
		'Copyright holder',
		'Copyright holder of survey event data',
		'TESTDATA00000001',
		GetDate(),
		1,
		'NBNSYS00'
	)
GO

--Create TERMLIST record
IF NOT EXISTS(SELECT * FROM TERM_LIST WHERE [TABLE]='SURVEY_EVENT_OWNER_TYPE')
	INSERT INTO TERM_LIST (
		[TABLE],
		KEY_FIELD,
		[DESCRIPTION],
		SYSTEM_SUPPLIED_DATA,
		ADDITIONAL_FIELDS
	)
	VALUES (
		'SURVEY_EVENT_OWNER_TYPE',
		'SURVEY_EVENT_OWNER_TYPE_KEY',
		'Survey Event Owner Type',
		1,
		0
	)
GO

--Records to ensure these export OK
IF NOT EXISTS(SELECT * FROM DATABASE_RELATIONSHIP WHERE RELATIONSHIP_KEY='NBNSYS0000000048') 
  INSERT INTO DATABASE_RELATIONSHIP (
		RELATIONSHIP_KEY,
		RELATIONSHIP_NAME,
		MASTER_TABLE,
		MASTER_FIELD,
		DETAIL_TABLE,
		DETAIL_FIELD,
		FOLLOW_UP,
		FOLLOW_DOWN
	)
	VALUES (
		'NBNSYS0000000048',
		'SURVEY_EVENTSURVEY_EVENT_OWNER',
		'SURVEY_EVENT',
		'SURVEY_EVENT_KEY',
		'SURVEY_EVENT_OWNER',
		'SURVEY_EVENT_KEY',
		0,	
		1
	)

IF NOT EXISTS(SELECT * FROM DATABASE_RELATIONSHIP WHERE RELATIONSHIP_KEY='NBNSYS0000000049') 
  INSERT INTO DATABASE_RELATIONSHIP (
		RELATIONSHIP_KEY,
		RELATIONSHIP_NAME,
		MASTER_TABLE,
		MASTER_FIELD,
		DETAIL_TABLE,
		DETAIL_FIELD,
		FOLLOW_UP,
		FOLLOW_DOWN
	)
	VALUES (
		'NBNSYS0000000049',
		'SURVEY_EVENT_OWNER_TYPESURVEY_EVENT_OWNER',
		'SURVEY_EVENT_OWNER_TYPE',
		'SURVEY_EVENT_OWNER_TYPE_KEY',
		'SURVEY_EVENT_OWNERSHIP',
		'SURVEY_EVENT_OWNERSHIP_TYPE_KEY',
		1,	
		0
	)
GO

--New report attribute
IF NOT EXISTS(SELECT * FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY='NBNSYS0000000085') 
	INSERT INTO REPORT_ATTRIBUTE (
	  REPORT_ATTRIBUTE_KEY,
		ITEM_GROUP,
		SOURCE_TABLE,
		ITEM_NAME,
		ATTRIBUTE_SQL,
		REPORT_JOIN_KEY,
		ENTERED_BY,
		ENTRY_DATE,
		SYSTEM_SUPPLIED_DATA
	)
	VALUES (
		'NBNSYS0000000085',
		'Event',
		'SURVEY_EVENT',
		'Event Owners',
		'#REPORT_OUTPUT.[Event Owners] = dbo.ufn_FormatEventOwners(SURVEY_EVENT.SURVEY_EVENT_KEY)',
		'NBNSYS0000000004',
		'TESTDATA00000001',
		GetDate(),
		1
	)

IF NOT EXISTS(SELECT * FROM REPORT_FIELD WHERE REPORT_FIELD_KEY='NBNSYS0000000093') 
	INSERT INTO REPORT_FIELD (
	  REPORT_FIELD_KEY,
		REPORT_ATTRIBUTE_KEY,
		FIELD_ITEM_NAME,
		FIELD_TYPE,
		FIELD_SIZE,
		ENTERED_BY,
		ENTRY_DATE,
		SYSTEM_SUPPLIED_DATA
	)
	VALUES (
		'NBNSYS0000000093',
		'NBNSYS0000000085',
		'Event Owners',
		'varchar',
		8000,
		'TESTDATA00000001',
		GetDate(),
		1
	)
GO

-- Correction for null entered by values
UPDATE REPORT_ATTRIBUTE
	SET ENTERED_BY='TESTDATA00000001'
	WHERE ENTERED_BY IS NULL


/*===========================================================================*\
  STORED PROCEDURES
\*===========================================================================*/

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
GO

/*===========================================================================*\
  Description:	Returns all names for an event's ownership

  Parameters:	@Key	Survey_Event_key

  Created:	Feb 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT DISTINCT
  SO.Survey_Event_Owner_Key,
	SO.Name_Key,
	OT.Short_Name,
	SO.Survey_Event_Owner_Type_Key,
	SO.Custodian,
	dbo.ufn_GetFormattedName(Name_Key) as DisplayName
FROM Survey_Event_Owner SO
INNER JOIN Survey_Event_Owner_Type OT ON OT.Survey_Event_Owner_Type_Key = SO.Survey_Event_Owner_Type_Key
WHERE Survey_Event_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwners_Select_ForSurveyEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwners_Select_ForSurveyEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwner_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwner_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Survey_Event_Owner table.

  Parameters:	@Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwner_Delete]
	@Key char(16)
AS

	DELETE	Survey_Event_Owner
	WHERE	Survey_Event_Owner_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwner_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwner_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Delete TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwnerTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwnerTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns the content of the survey event owner type termlist

  Parameters:	@Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwnerTypes_Select]
AS

	SELECT Survey_Event_Owner_Type_Key, Short_Name 
	FROM Survey_Event_Owner_Type

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwnerTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwnerTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwnerTypes_Select TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwner_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwner_Update]
GO

/*===========================================================================*\
  Description:	Updates a survey event owner

  Parameters:	
	@SurveyEventOwnerKey CHAR(16),
	@NameKey CHAR(16),
	@SurveyEventOwnerTypeKey CHAR(16),
	@ChangedBy CHAR(16),
	@ChangedDate DATETIME=NULL

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwner_Update]
	@SurveyEventOwnerKey CHAR(16),
	@NameKey CHAR(16),
	@SurveyEventOwnerTypeKey CHAR(16),
	@ChangedBy CHAR(16),
	@ChangedDate DATETIME=NULL

AS

SET NOCOUNT OFF

IF @ChangedDate IS NULL
  SET @ChangedDate = GETDATE()

UPDATE Survey_Event_Owner 
SET Name_Key=@NameKey,
		Survey_Event_Owner_Type_Key=@SurveyEventOwnerTypeKey,
		Changed_By=@ChangedBy,
		Changed_Date=@ChangedDate
WHERE Survey_Event_Owner_Key=@SurveyEventOwnerKey

SET NOCOUNT ON

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwner_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwner_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwner_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwner_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new survey event owner

  Parameters:	
	@Key - OUTPUT
	@NameKey,
	@Acronym,
	@ItemName,
	@EnteredBy,
	@EntryDate,
	@ChangedBy,
	@ChangedDate,
	@SystemSuppliedData,  
  @Custodian

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwner_Insert]
	@Key CHAR(16) OUTPUT,
	@SurveyEventKey CHAR(16),
	@NameKey CHAR(16),
	@SurveyEventOwnerTypeKey CHAR(16),
	@EnteredBy CHAR(16), 
	@EntryDate DATETIME=NULL,
	@ChangedBy CHAR(16)=NULL,
	@ChangedDate DATETIME=NULL,
	@Custodian CHAR(8)=NULL,
	@SystemSuppliedData BIT=NULL
AS

SET NOCOUNT OFF

EXECUTE spNextKey 'Survey_Event_Owner', @Key OUTPUT

IF @Custodian IS NULL  
	SET @Custodian = LEFT(@Key, 8)

IF @EntryDate IS NULL
  SET @EntryDate = GETDATE()

IF @SystemSuppliedData IS NULL
  SET @SystemSuppliedData=0

INSERT INTO Survey_Event_Owner (
	Survey_Event_Owner_Key,
	Survey_Event_Key,
	Name_Key,
	Survey_Event_Owner_Type_Key,
	Entered_By,
	Entry_Date,
	Changed_By,
	Changed_Date,
	System_Supplied_Data,
  Custodian
) 
VALUES (
	@Key,
	@SurveyEventKey,
	@NameKey,
	@SurveyEventOwnerTypeKey,
	@EnteredBy,
	@EntryDate,
	@ChangedBy,
	@ChangedDate,
	@SystemSuppliedData,  
  @Custodian
)

SET NOCOUNT ON


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwner_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwner_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SurveyEventOwner_Insert TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE Id = OBJECT_ID(N'[dbo].[ufn_FormatEventOwners]')
	   AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
  DROP FUNCTION [dbo].[ufn_FormatEventOwners]
GO

/*===========================================================================*\
  Description:	Returns the list of survey event owners as a semi-colon 
		separated string

  Parameters:	@Key	survey event key

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/02/04 17:19 $
    $Author: Johnvanbreda $

\*===========================================================================*/   
CREATE FUNCTION dbo.ufn_FormatEventOwners(@Key char(16))
RETURNS varchar(8000)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(70)
DECLARE @NameKey char(16)
DECLARE @OwnerType varchar(20)

DECLARE csrEventOwner CURSOR
FOR
  SELECT EO.NAME_KEY, OT.SHORT_NAME
	FROM SURVEY_EVENT_OWNER EO
	INNER JOIN SURVEY_EVENT_OWNER_TYPE OT ON OT.SURVEY_EVENT_OWNER_TYPE_KEY = EO.SURVEY_EVENT_OWNER_TYPE_KEY
	WHERE EO.SURVEY_EVENT_KEY=@Key

OPEN csrEventOwner

FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType

IF @@FETCH_STATUS = 0 
  SELECT @ReturnString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'

WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType
	SELECT @ItemString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'
	IF @@FETCH_STATUS = 0 
		SELECT @ReturnString = @ReturnString + ';' + @ItemString
END

CLOSE csrEventOwner
DEALLOCATE csrEventOwner

RETURN @ReturnString
--****************************************************************************************************

END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.ufn_FormatEventOwners'))
BEGIN
	PRINT 'Setting up security on function ufn_FormatEventOwners'

	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_ReadOnly]
	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_RecordCardsOnly]
	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_AddOnly]
	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_FullEdit]
	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [Dev - JNCC SQL]
END
GO
