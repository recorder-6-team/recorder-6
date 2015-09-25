IF Object_ID('dbo.usp_SurveyType_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_SurveyType_Select
GO

/*===========================================================================*\
  Description:
	Returns a Survey Type record.

  Parameters:
	@Key	Key of the survey type.

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SurveyType_Select
	@Key	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	Survey_Type_Key AS Item_Key,
			Short_Name		AS Item_Name
	FROM	Survey_Type
	WHERE	Survey_Type_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_SurveyType_Select TO "Dev - JNCC SQL"
GO