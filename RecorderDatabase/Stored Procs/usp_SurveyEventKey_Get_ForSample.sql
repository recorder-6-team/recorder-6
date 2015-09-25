If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_Get_ForSample]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForSample]
GO

/*===========================================================================*\
  Description: 	This proc. returns a Survey Event key when given a Sample key.

  Parameters:	@Key 		Sample Key
		@SurveyEventKey	(output)

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 8/06/04 10:44 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForSample] 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 	@SurveyEventKey = Survey_Event_Key
	FROM	[Sample]
	WHERE	Sample_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForSample TO [Dev - JNCC SQL]
END

GO
