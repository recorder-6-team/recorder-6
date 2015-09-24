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
    $Revision: 1 $
    $Date: 27/02/04 10:28 $
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