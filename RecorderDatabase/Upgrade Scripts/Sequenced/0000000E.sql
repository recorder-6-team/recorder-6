if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Individual_Organisation_Department]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[INDIVIDUAL] DROP CONSTRAINT FK_Individual_Organisation_Department
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Movement_Of_Material_Organisation_Department]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Movement_Of_Material] DROP CONSTRAINT FK_Movement_Of_Material_Organisation_Department
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Organisation_Department]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[Organisation_Department]
GO

CREATE TABLE [dbo].[Organisation_Department] (
	[Organisation_Department_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Acronym] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entry_Date] datetime NOT NULL ,
	[Changed_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS ,
	[Changed_Date] datetime ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Organisation_Department] WITH NOCHECK ADD 
	CONSTRAINT [PK_Organisation_Department] PRIMARY KEY  CLUSTERED 
	(
		[Organisation_Department_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[Organisation_Department] ADD 
	CONSTRAINT [DF_Organisation_Department_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
GO

ALTER TABLE [dbo].[Organisation_Department] ADD 
	CONSTRAINT [FK_Organisation_Department_ORGANISATION] FOREIGN KEY 
	(
		[Name_Key]
	) REFERENCES [dbo].[ORGANISATION] (
		[NAME_KEY]
	)
GO

IF NOT EXISTS(SELECT *
          FROM INFORMATION_SCHEMA.COLUMNS
          WHERE TABLE_SCHEMA = 'dbo'
            AND TABLE_NAME = 'INDIVIDUAL'
            AND COLUMN_NAME = 'ORGANISATION_DEPARTMENT_KEY')
	ALTER TABLE [dbo].[INDIVIDUAL] ADD 
		ORGANISATION_DEPARTMENT_KEY CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
GO

ALTER TABLE [dbo].[INDIVIDUAL] ADD 
	CONSTRAINT [FK_Individual_Organisation_Department] FOREIGN KEY 
	(
		[Organisation_Department_Key]
	) REFERENCES [dbo].[Organisation_Department] (
		[Organisation_Department_Key]
	)
GO

-- Add permissions.
GRANT  SELECT  ON [dbo].[Organisation_Department]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Organisation_Department]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[Organisation_Department]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Organisation_Department]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Organisation_Department]  TO [R2k_Administrator]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartments_Select_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartments_Select_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns the list of departments that an individual could
      					possibly belong to

  Parameters:	@Key	Name key

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OrganisationDepartments_Select_ForIndividual]
	@Key char(16)
AS

SET NOCOUNT ON

--Select data.  Note relationships are selected which ever way round they are
SELECT DISTINCT
  OD.Organisation_Department_Key, 
	OD.Item_Name
FROM Organisation_Department OD
INNER JOIN Organisation O ON O.Name_Key=OD.Name_Key
INNER JOIN Name_Relation NR ON (NR.Name_Key_1=OD.Name_Key OR NR.Name_Key_2=OD.Name_Key)
WHERE NR.Name_Key_1=@Key
OR NR.Name_Key_2=@Key	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartments_Select_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartments_Select_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartment_Select_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartment_Select_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns the single department that an individual does belong to

  Parameters:	@Key	Name key

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OrganisationDepartment_Select_ForIndividual]
	@Key char(16)
AS

SET NOCOUNT ON

--Select data.  Note relationships are selected which ever way round they are
SELECT DISTINCT
  I.Organisation_Department_Key, 
	OD.Item_Name
FROM Individual I
INNER JOIN Organisation_Department OD ON OD.Organisation_Department_Key=I.Organisation_Department_Key
WHERE I.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartment_Select_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartment_Select_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Select_ForIndividual TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Departments_Select_ForOrganisation]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Departments_Select_ForOrganisation]
GO

/*===========================================================================*\
  Description:	Returns all departments within an organisation

  Parameters:	@Key	Name key

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Departments_Select_ForOrganisation]
	@Key char(16)
AS

SET NOCOUNT ON

--Select data.  Note relationships are selected which ever way round they are
SELECT DISTINCT
  OD.Organisation_Department_Key, 
	OD.Acronym, 
	OD.Item_Name,
	OD.Custodian
FROM Organisation_Department OD
WHERE OD.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Departments_Select_ForOrganisation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Departments_Select_ForOrganisation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartment_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartment_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new organisation department

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
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OrganisationDepartment_Insert]
	@Key CHAR(16) OUTPUT,
	@NameKey CHAR(16),
	@Acronym VARCHAR(20),
	@ItemName VARCHAR(100),
	@EnteredBy CHAR(16), 
	@EntryDate DATETIME=NULL,
	@ChangedBy CHAR(16)=NULL,
	@ChangedDate DATETIME=NULL,
	@Custodian CHAR(8)=NULL,
	@SystemSuppliedData BIT=NULL
AS

SET NOCOUNT OFF

EXECUTE spNextKey 'Organisation_Department', @Key OUTPUT

IF @Custodian IS NULL  
	SET @Custodian = LEFT(@Key, 8)

IF @EntryDate IS NULL
  SET @EntryDate = GETDATE()

IF @SystemSuppliedData IS NULL
  SET @SystemSuppliedData=0

INSERT INTO Organisation_Department (
	Organisation_Department_Key,
	Name_Key,
	Acronym,
	Item_Name,	
	Entered_By,
	Entry_Date,
	Changed_By,
	Changed_Date,
	System_Supplied_Data,
  Custodian
) 
VALUES (
	@Key,
	@NameKey,
	@Acronym,
	@ItemName,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartment_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartment_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Insert TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartment_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartment_Update]
GO


/*===========================================================================*\
  Description: 	Updates a record in Organisation_Department.

  Parameters:	@Key 
		@ItemName 
		@Description
		@Changed_By

  Created:     	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OrganisationDepartment_Update]
	@Key char(16),
  @Acronym varchar(20),
	@ItemName varchar(100),
  @ChangedBy char(16)
AS
		UPDATE 	Organisation_Department
		SET	Item_Name = @ItemName,
			Acronym = @Acronym,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate()
		WHERE	Organisation_Department_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartment_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartment_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [Dev - JNCC SQL]
END
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartment_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartment_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Organisation_Department table.

  Parameters:	@Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 11/03/04 10:20 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OrganisationDepartment_Delete]
	@Key char(16)
AS

	DELETE	Organisation_Department
	WHERE	Organisation_Department_Key = @Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartment_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartment_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Delete TO [Dev - JNCC SQL]
END
GO


EXECUTE spCreateCustodianTrigger 'Organisation_Department', 'Organisation_Department_Key'