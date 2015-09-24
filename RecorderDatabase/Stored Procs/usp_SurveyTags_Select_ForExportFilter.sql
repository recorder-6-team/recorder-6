/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForExportFilter') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select_ForExportFilter]
GO

/*===========================================================================*\
  Description:	Retrieve the tags for an export filter

  Parameters:
	@Key		Export Filter Key

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/08 14:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select_ForExportFilter]
	@Key CHAR(16)
AS
	SELECT 	DISTINCT 
			CT.Concept_Key, 
			CT.Plaintext
	FROM 	Survey_Tag			ST
	JOIN 	VW_ConceptTerm 		CT 	ON 	CT.Concept_Key	=	ST.Concept_Key
	JOIN	Export_Filter_Tag	EFT	ON	EFT.Concept_Key	=	ST.Concept_Key
	WHERE	EFT.Export_Filter_Key	=	@Key
	AND		CT.List_Preferred 	= 	1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForExportFilter') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select_ForExportFilter'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForExportFilter TO [Dev - JNCC SQL]
END
GO
