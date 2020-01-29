/******Change to add delete USR when Survey is deleted  ******/
/****** Object:  StoredProcedure [dbo].[usp_SurveyTag_Delete_ForSurvey]    Script Date: 01/03/2020 15:07:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Remove a survey from all its associated tags and User_Survey_Restriction .

  Parameters:
	@SurveyKey

  Created:	February 2008

  Last revision information:
  January 2020 to add USR  

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_SurveyTag_Delete_ForSurvey]
	@SurveyKey CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Survey_Tag
	WHERE  Survey_Key = @SurveyKey
	
	DELETE FROM User_Survey_Restriction 
	WHERE Survey_Key = @SurveyKey 


