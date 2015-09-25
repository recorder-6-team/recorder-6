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
    $Revision: 1 $
    $Date: 27/02/04 10:28 $
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