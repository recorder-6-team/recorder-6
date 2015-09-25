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
    $Revision: 1 $
    $Date: 27/02/04 10:28 $
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