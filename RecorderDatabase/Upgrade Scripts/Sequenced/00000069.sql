/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSurveyTagString]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSurveyTagString
GO

/*===========================================================================*\
  Description:	
	Outputs a list of all the tags in a particular survey.

  Parameters:	
	@Survey_Key	The primary key of the survey to get the tags for.

  Created:	January	2009

  Last revision information:
    $Revision: 1 $
    $Date: 13/02/09 17:21 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSurveyTagString]	(@Survey_Key	VARCHAR(1000))
RETURNS	VARCHAR(1000)
BEGIN
	DECLARE @Survey_Tags AS VARCHAR(1000)

	SET @Survey_Tags = ''

	SELECT		@Survey_Tags	=	@Survey_Tags + T.Item_Name + '; '
	FROM		Survey_Tag		ST
	INNER JOIN	Concept			C
			ON	C.Concept_Key	=	ST.Concept_Key
	INNER JOIN	Term			T
			ON	T.Term_Key		=	C.Term_Key
	WHERE		ST.Survey_Key	=	@Survey_Key

	SET			@Survey_Tags	=	SUBSTRING(@Survey_Tags, 0, LEN(@Survey_Tags))

	RETURN		@Survey_Tags
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSurveyTagString]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedTerm'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetSurveyTagString TO [Dev - JNCC SQL]
	END
GO