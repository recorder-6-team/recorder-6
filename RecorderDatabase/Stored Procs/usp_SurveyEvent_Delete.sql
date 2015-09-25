/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Survey_Event table with related tables.

  Parameters:	@Key

  Created:	May 2008

  Last revision information:
    $Revision: 3 $
    $Date: 13/05/08 9:50 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Delete]
	@Key char(16)
AS
	SET NOCOUNT ON

    DELETE  Survey_Event_Recorder Where Survey_Event_Key = @Key
	DELETE	Survey_Event_Owner WHERE	Survey_Event_Key = @Key
    Delete  Survey_Event where Survey_Event_Key = @Key
GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_SurveyEvent_Delete]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete]TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.[usp_SurveyEvent_Delete] TO [Dev - JNCC SQL]
END
GO