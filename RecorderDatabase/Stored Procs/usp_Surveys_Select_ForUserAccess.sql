/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForUserAccess') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForUserAccess]
GO

/*===========================================================================*\
  Description:		
	Returns all Surveys on the system using the standard caption for a survey 
	in alphabetical order. (Saved in JNCC folder). Also returns a column 
	stating whether or not the specified user can view each survey.

  Parameters: 
	@User_Name_Key CHAR(16) The Name_Key of the User whose surveys are
		being viewed.

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 7/01/09 15:31 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForUserAccess]
	@User_Name_Key CHAR(16)
AS
	SET NOCOUNT ON

	SELECT		SU.Survey_Key,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name,
				CASE
					WHEN USR.Name_Key IS NULL THEN 1 
					ELSE 0
				END AS Allow_View
	FROM		Survey			SU	
	JOIN		Name			N		ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I		ON	I.Name_Key		=	N.Name_Key
										AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 		ON	O.Name_Key		=	N.Name_Key
										AND	N.Organisation	=	1
	LEFT JOIN	User_Survey_Restriction USR	ON	USR.Survey_Key	=	SU.Survey_Key
										AND	USR.Name_Key	=	@User_Name_Key
	ORDER BY	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForUserAccess') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForUserAccess'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForUserAccess TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForUserAccess TO [Dev - JNCC SQL]
END
GO
