/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select]
GO

/*===========================================================================*\
  Description:		Returns all Surveys on the system using the standard caption
		for a survey in alphabetical order. (Saved in JNCC folder).

  Parameters:

  Created:	May 2004

  Last revision information:
    $Revision: 4 $
    $Date: 7/02/08 14:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select]
AS
	SET NOCOUNT ON

	SELECT		SU.Survey_Key,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name
	FROM		Survey			SU	
	JOIN		Name			N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I	ON	I.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 	ON	O.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	1
	ORDER BY	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [Dev - JNCC SQL]
END
GO
